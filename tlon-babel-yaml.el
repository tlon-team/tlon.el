;;; tlon-babel-yaml.el --- Parse, get, set & edit YAML -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon-babel
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Parse, get, set & edit YAML.

;;; Code:

(require 'citar)
(require 'files-extras)
(require 'tlon-core)
(require 'tlon-babel-core)
(require 'tlon-babel-refs)

;;;; Variables

(defconst tlon-babel-yaml-delimiter "---\n"
  "Delimiter for YAML metadata.")

(defconst tlon-babel-yaml-core-keys
  '("type" "original_path")
  "List of YAML keys necessary to initialize the translation metadata.")

(defconst tlon-babel-yaml-translation-only-keys
  '("original_path" "publication_status")
  "List of YAML keys included in translation metadata only.
I.e., these keys are not included in the metadata of originals.")

(defconst tlon-babel-yaml-article-keys
  '("type" "title" "authors" "translators" "tags" "date" "original_path" "bibtex_key" "publication_status" "description")
  "List of YAML keys of fields to include in articles.
The order of the keys determines the sort order by
`tlon-babel-yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-tag-keys
  '("type" "title" "brief_title" "original_path" "publication_status")
  "List of YAML keys of fields to include in tags.
The order of the keys determines the sort order by
`tlon-babel-yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-author-keys
  '("type" "title" "original_path" "publication_status")
  "List of YAML keys of fields to include in authors.
The order of the keys determines the sort order by
`tlon-babel-yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-collection-keys
  '("title" "original_path" "publication_status")
  "List of YAML keys of fields to include in collections.
The order of the keys determines the sort order by
`tlon-babel-yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-tag-or-author-keys
  '("titulo" "estado_de_publicacion")
  "List of YAML keys of fields to include in BAE tags or authors.
The order of the keys determines the sort order by
`tlon-babel--yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-original-author-keys
  '("title")
  "List of YAML keys of fields to include in `uqbar-es' authors.
The order of the keys determines the sort order by
`tlon-babel-yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-publication-statuses
  '("unpublished" "test" "production")
  "List of publication statuses.")

(defconst tlon-babel-yaml-field-setters
  '((:key "title"
	  :original tlon-babel-yaml-set-title-in-original
	  :translation tlon-babel-yaml-set-title-in-translation)
    (:key "authors"
	  :original tlon-babel-yaml-set-authors-in-original
	  :translation tlon-babel-yaml-set-authors-in-translation)
    (:key "translators"
	  :original tlon-babel-yaml-set-translators-in-original
	  :translation tlon-babel-yaml-set-translators-in-translation)
    (:key "tags"
	  :original tlon-babel-yaml-set-tags-in-original
	  :translation tlon-babel-yaml-set-tags-in-translation)
    (:key "date"
	  :original tlon-babel-yaml-set-date-in-original
	  :translation tlon-babel-yaml-set-date-in-translation)
    (:key "bibtex_key"
	  :original tlon-babel-yaml-set-bibtex-key-in-original
	  :translation tlon-babel-yaml-set-bibtex-key-in-translation)
    (:key "description"
	  :original tlon-babel-yaml-set-description-in-original
	  :translation tlon-babel-yaml-set-description-in-translation)
    ;; translation-only
    (:key "publication_status"
	  :translation tlon-babel-yaml-set-publication-status-in-translation)
    (:key "original_path"
	  :translation tlon-babel-yaml-set-original-path-in-translation))
  "Property list of field setters for YAML metadata.")

;;;; Functions

;;;;; Parse

;; I tried an Elisp implementation of YAML parsing
;; (https://github.com/zkry/yaml.el), but it was too slow.
;; But I haven’t tried this C-based parser:
;; https://github.com/syohex/emacs-libyaml

(defun tlon-babel-yaml-to-alist (strings)
  "Convert YAML STRINGS to an alist."
  (let ((metadata '()))
    (dolist (line strings)
      (when (string-match "^\\(.*?\\):\\s-+\\(.*\\)$" line)
	(let* ((key (match-string 1 line))
	       (value (match-string 2 line))
	       (trimmed-value (string-trim value)))
	  (push (cons (string-trim key) trimmed-value) metadata))))
    (nreverse metadata)))

(defun tlon-babel-yaml-format-values-of-alist (alist)
  "Format the values of ALIST, converting from YAML format to Elisp format."
  (mapcar (lambda (pair)
	    (cons (car pair)
		  (tlon-babel-yaml-format-value (cdr pair))))
	  alist))

(defun tlon-babel-yaml-format-value (value)
  "Format VALUE by converting from the YAML format to an Elisp format."
  (cond
   ((and (string-prefix-p "[" value) (string-suffix-p "]" value)) ;; list
    (mapcar #'string-trim
	    (mapcar (lambda (s)
		      (if (and (string-prefix-p "\"" s) (string-suffix-p "\"" s))
			  (substring s 1 -1)
			s))
		    (split-string (substring value 1 -1) "\\s *,\\s *"))))
   ((and (string-prefix-p "\"" value) (string-suffix-p "\"" value)) ;; string
    (substring value 1 -1))
   (t value)))

(defun tlon-babel-yaml-read-until-match (delimiter)
  "Return a list of lines until DELIMITER is matched.
The delimiter is not included in the result. If DELIMITER is not found, signal
an error."
  (let ((result '()))
    (while (not (or (looking-at-p delimiter) (eobp)))
      (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) result)
      (forward-line))
    (if (eobp)
	(error "Delimiter not found")
      (nreverse result))))

(defun tlon-babel-yaml-convert-list (list)
  "Convert an Elisp LIST to a YAML list."
  (concat "[\"" (mapconcat 'identity list "\", \"") "\"]"))

;;;;; Get

(defun tlon-babel-metadata-in-repos (&rest pairs)
  "Return metadata of all repos matching all PAIRS.
PAIRS is an even-sized list of <key value> tuples."
  (let (metadata)
    (dolist (repo (tlon-babel-repo-lookup-all :dir pairs))
      (setq metadata (append (tlon-babel-metadata-in-repo repo) metadata)))
    metadata))

(defun tlon-babel-metadata-in-repo (&optional repo)
  "Return metadata of REPO.
If REPO is nil, return metadata of the current repository."
  (let* ((repo (or repo (tlon-babel-get-repo))))
    (when (eq (tlon-babel-repo-lookup :type :dir repo) 'content)
      (tlon-babel-metadata-in-dir repo))))

(defun tlon-babel-metadata-in-dir (dir)
  "Return the metadata in DIR and all its subdirectories as an association list."
  (let (metadata)
    (dolist (file (directory-files-recursively dir "\\.md$"))
      (when-let ((meta-in-file (tlon-babel-metadata-in-file file)))
	(push meta-in-file metadata)))
    metadata))

(defun tlon-babel-metadata-in-file (file-or-buffer)
  "Return the metadata in FILE-OR-BUFFER as an association list.
This includes the YAML metadata at the beginning of the file, as well as
additional metadata such as the file name and the file type."
  (let* ((metadata (tlon-babel-yaml-format-values-of-alist
		    (tlon-babel-yaml-get-metadata file-or-buffer)))
	 (extras `(("file" . ,file-or-buffer)
		   ("type" . "online")
		   ("database" . "Tlön")
		   ("landid" . "es"))))
    (append metadata extras)))

(defun tlon-babel-yaml-get-metadata (&optional file-or-buffer raw)
  "Return the YAML metadata from FILE-OR-BUFFER as strings in a list.
If FILE-OR-BUFFER is nil, use the current buffer. Return the metadata as an
alist, unless RAW is non-nil."
  (let ((file-or-buffer (or file-or-buffer
			    (buffer-file-name)
			    (current-buffer))))
    (with-temp-buffer
      (cond
       ;; If `file-or-buffer' is a buffer object
       ((bufferp file-or-buffer)
	(insert (with-current-buffer file-or-buffer (buffer-string))))
       ;; If `file-or-buffer' is a string
       ((stringp file-or-buffer)
	(insert-file-contents file-or-buffer)))
      (goto-char (point-min))
      (when (looking-at-p tlon-babel-yaml-delimiter)
	(forward-line)
	(let ((metadata (tlon-babel-yaml-read-until-match tlon-babel-yaml-delimiter)))
	  (if raw
	      metadata
	    (tlon-babel-yaml-to-alist metadata)))))))

(defun tlon-babel-yaml-sort-fields (fields &optional keys no-error)
  "Sort alist of YAML FIELDS by order of KEYS.
If one of FIELDS is not found, throw an error unless NO-ERROR is non-nil."
  (mapcar (lambda (key)
	    (if-let ((match (assoc key fields)))
		match
	      (unless no-error
		(user-error "Key `%s' not found in file `%s'" key (buffer-file-name)))))
	  keys))

(defun tlon-babel-yaml-get-valid-keys (&optional file)
  "Return the admissible keys for YAML metadata in FILE.
If FILE is nil, return the work type of the file visited by the current buffer."
  (let* ((file (or file (buffer-file-name))))
    (pcase (file-name-nondirectory (directory-file-name (file-name-directory file)))
      ("articulos" tlon-babel-yaml-article-keys)
      ("temas" tlon-babel-yaml-tag-keys)
      ("authors" tlon-babel-yaml-author-keys))))

(defun tlon-babel-get-repo-in-subproject-language (language &optional repo)
  "Return the path of the subproject in REPO corresopnding to LANGUAGE.
If REPO is nil, use the current repository."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (subproject (tlon-babel-repo-lookup :subproject :dir repo)))
    (tlon-babel-repo-lookup :dir :subproject subproject :language language)))

(defun tlon-babel-yaml-get-filenames-in-dir (&optional dir extension)
  "Return a list of all filenames in DIR.
If DIR is nil, use the current directory. EXTENSION defaults to \"md\". If you
want to search all files, use the empty string."
  (let* ((dir (or dir default-directory))
	 (extension (or extension "md"))
	 (extension-regex (format "\\.%s$" extension))
	 (files (directory-files-recursively dir extension-regex)))
    (mapcar #'file-name-nondirectory files)))

;;;;; Set

(defun tlon-babel-yaml-set-metadata-fields (fields &optional title)
  "Set the field values for the given FIELDS in the current buffer.
If TITLE is non-nil, use it instead of prompting for one."
  (let* ((var-generators
	  `(("date" . ,(lambda () (format-time-string "%FT%T%z")))
	    ("title" . ,(lambda () (or title (read-string "Title: "))))
	    ("authors-list" . ,(lambda () (tlon-babel-yaml-set-multi-value-field "title" "authors")))
	    ("translators" . ,#'tlon-babel-yaml-set-translators)
	    ("tags" . ,#'tlon-babel-yaml-set-tags)
	    ("original_path" . ,#'tlon-babel-yaml-set-original-path)))
	 (processed-fields (if (member "authors" fields)
			       (cons "authors-list" fields)
			     fields))
	 (field-values (cl-loop for field in processed-fields
				for generator = (cdr (assoc field var-generators))
				if generator collect `(,field . ,(funcall generator)))))
    ;; calculate first-author and adjust field-values
    (let* ((first-author (cdr (or (assoc "authors" field-values) (assoc "authors-list" field-values))))
	   (authors (when first-author
		      (or
		       (cdr (assoc "authors" field-values))
		       (tlon-babel-yaml-convert-list first-author))))
	   (cmpl-generators
	    `(("first-author" . ,first-author)
	      ("authors" . ,authors)
	      ("publication_status" . "no publicado")
	      ("original_key" . ,(when first-author (tlon-babel-yaml-set-original-key (car first-author))))
	      ("translation_key" . ,(when first-author
				      (tlon-babel-refs-generate-autokey
				       (car first-author)
				       (substring (cdr (assoc "date" field-values)) 0 4)
				       (cdr (assoc "title" field-values))))))))
      ;; revise field-values
      (setq field-values (assoc-delete-all "authors-list" field-values))
      (dolist (field fields)
	(when (cdr (assoc field cmpl-generators))
	  (push `(,field . ,(cdr (assoc field cmpl-generators))) field-values))))
    field-values))

(defun tlon-babel-yaml-set-metadata (keys &optional title)
  "Insert YAML fields for KEYS for `uqbar-en' article in the current buffer.
If TITLE is non-nil, use it instead of prompting for one. The fields will be
inserted in the order in which KEYS are listed."
  (let* ((fields (tlon-babel-yaml-set-metadata-fields keys title))
	 (sorted-fields (tlon-babel-yaml-sort-fields fields keys)))
    (tlon-babel-yaml-insert-fields sorted-fields)))

;; TODO: revise `translations' now that we use separate repos
(defun tlon-babel-yaml-set-multi-value-field (field &optional dir repo)
  "Set the value of multivalue FIELD in metadata of REPO.
If DIR is non-nil, only search in directory within the repo. Note DIR does not
include the `translations' directory. That is, it is the directory component of
the repo's locator. For example, to search only in `translations/autores', use
`autores' as DIR."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (metadata (tlon-babel-metadata-in-repo repo))
	 (full-dir (when dir (file-name-concat repo "translations" dir))))
    (completing-read-multiple (format "%s: " (capitalize dir))
			      (tlon-babel-metadata-lookup-all
			       metadata field (when dir "file") (when dir full-dir)))))

(defun tlon-babel-yaml-set-authors ()
  "Set the value of `authors' YAML field."
  (tlon-babel-yaml-convert-list
   (tlon-babel-yaml-set-multi-value-field "title" "authors")))

(defun tlon-babel-yaml-set-translators ()
  "Set the value of `translators' YAML field."
  (tlon-babel-yaml-convert-list
   (completing-read-multiple
    "Translators: "
    (tlon-babel-metadata-lookup-all (tlon-babel-metadata-in-repos :subtype 'translations) "translators"))))

(defun tlon-babel-yaml-set-tags ()
  "Set the value of `tags' YAML field."
  (tlon-babel-yaml-convert-list
   (tlon-babel-yaml-set-multi-value-field "title" "tags")))

(defun tlon-babel-yaml-set-original-path ()
  "Set the value of `original_path' YAML field."
  (let ((dir (tlon-babel-get-counterpart-dir (buffer-file-name))))
    (completing-read "Original filename: "
		     (tlon-babel-yaml-get-filenames-in-dir dir))))

(defun tlon-babel-yaml-set-original-key (author)
  "Set the value of `original_key' YAML field.
AUTHOR is the first author of the original work."
  (let ((first-author (car (last (split-string author)))))
    (car (split-string
	  (completing-read
	   "English original: "
	   (citar--completion-table (citar--format-candidates) nil)
	   nil
	   nil
	   (format "%s " first-author)
	   'citar-history citar-presets nil)))))

;;;;; Edit

;; TODO: throw error if any of fields already present
(defun tlon-babel-yaml-insert-fields (fields)
  "Insert YAML FIELDS in the buffer at point.
FIELDS is an alist, typically generated via `tlon-babel-yaml-to-alist'."
  (when (looking-at-p tlon-babel-yaml-delimiter)
    (user-error "File appears to already contain a metadata section"))
  (save-excursion
    (goto-char (point-min))
    ;; calculate the max key length
    (let ((max-key-len (cl-reduce 'max (mapcar (lambda (cons) (length (car cons))) fields)))
	  format-str)
      ;; determine the format for string
      (setq format-str (format "%%-%ds %%s\n" (+ max-key-len 2)))
      ;; insert the yaml delimiter & fields
      (insert tlon-babel-yaml-delimiter)
      (dolist (cons fields)
	(insert (format format-str (concat (car cons) ":") (cdr cons))))
      (insert tlon-babel-yaml-delimiter))))

(defun tlon-babel-yaml-delete-metadata ()
  "Delete YAML metadata section."
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p tlon-babel-yaml-delimiter)
      (user-error "File does not appear to contain a metadata section"))
    (forward-line)
    (re-search-forward tlon-babel-yaml-delimiter)
    (delete-region (point-min) (point))))

(defun tlon-babel-yaml-reorder-metadata ()
  "Reorder the YAML metadata in the buffer at point."
  (save-excursion
    (let* ((unsorted (tlon-babel-yaml-get-metadata))
	   (sorted (tlon-babel-yaml-sort-fields
		    unsorted (tlon-babel-yaml-get-valid-keys) 'no-error)))
      (tlon-babel-yaml-delete-metadata)
      (tlon-babel-yaml-insert-fields sorted))))

;;;;; Interactive editing

(defun tlon-babel-yaml-edit-field ()
  "Edit the YAML field at point."
  (interactive)
  (cl-destructuring-bind (key value) (tlon-babel-yaml-get-field-at-point)
    (tlon-babel-yaml-get-completions key value)))

(defun tlon-babel-yaml-get-completions (key value)
  "Get completions based on KEY.
If KEY already has VALUE, use it as the initial input."
  (if-let ((fun (tlon-babel-yaml-get-completion-functions key))
	   (val (tlon-babel-yaml-get-completion-values key)))
      (funcall fun val)
    (tlon-babel-yaml-insert-string (list value))))

(defun tlon-babel-yaml-get-completion-values (key)
  "Get completion values for a YAML field with KEY."
  (pcase key
    ("translators" (tlon-babel-metadata-get-translators))
    ("tags" (tlon-babel-metadata-get-uqbar-fields "tags"))
    ("authors" (tlon-babel-metadata-get-uqbar-fields "authors"))
    ("original_path" (tlon-babel-yaml-get-filenames-in-dir))
    ("original_key" (citar--completion-table (citar--format-candidates) nil))
    ("translation_key" (citar--completion-table (citar--format-candidates) nil))
    ("publication_status" tlon-babel-yaml-publication-statuses)
    (_ nil)))

(defun tlon-babel-yaml-get-completion-functions (key)
  "Get completion functions for a YAML field with KEY."
  (pcase key
    ((or "authors" "translators" "tags") #'tlon-babel-yaml-insert-list)
    ((or "original_path" "original_key" "translation_key" "publication_status") #'tlon-babel-yaml-insert-string)
    (_ nil)))

;; TODO: integrate `tlon-babel-yaml-get-completion-values'
(defun tlon-babel-yaml-insert-field (&optional key value file field-exists)
  "Insert a new field in the YAML metadata of FILE.
If FILE is nil, use the file visited by the current buffer. If KEY or VALUE are
nil, prompt for one. If field exists, throw an error if FIELD-EXISTS is
`throw-error', overwrite if it is `overwrite', and do nothing otherwise."
  (interactive)
  (let ((key (or key (completing-read "Key: " (tlon-babel-yaml-get-valid-keys))))
	(value (or value (read-string "Value: ")))
	(file (or file (buffer-file-name))))
    (if-let ((metadata (tlon-babel-yaml-get-metadata file)))
	(if-let ((key-exists-p (assoc key metadata)))
	    (cond ((eq field-exists 'overwrite)
		   (tlon-babel-yaml-delete-field key file)
		   (tlon-babel-yaml-write-field key value file))
		  ((eq field-exists 'throw-error)
		   (user-error "Field `%s' already exists in `%s'" key file)))
	  (tlon-babel-yaml-write-field key value file))
      (user-error "File `%s' does not appear to contain a metadata section" file))))

(defun tlon-babel-yaml-write-field (key value file)
  "Set KEY to VALUE in FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (forward-line)
    (insert (format "%s:  %s\n" key value))
    (save-buffer)
    (tlon-babel-yaml-reorder-metadata)))

;; TODO: refactor with above
(defun tlon-babel-yaml-delete-field (&optional key file)
  "Delete the YAML field with KEY in FILE."
  (let ((key (or key (completing-read "Field: " tlon-babel-yaml-article-keys)))
	(file (or file (buffer-file-name))))
    (if-let ((metadata (tlon-babel-yaml-get-metadata file)))
	(if (assoc key metadata)
	    (with-current-buffer (find-file-noselect file)
	      (goto-char (point-min))
	      (re-search-forward (format "%s:.*\n" key))
	      (delete-region (match-beginning 0) (match-end 0))
	      (save-buffer))
	  (user-error "Key `%s' not found in file `%s'" key file))
      (user-error "File does not appear to contain a metadata section"))))

;; TODO: Handle multiline fields, specifically `description’
(defun tlon-babel-yaml-get-field-at-point ()
  "Return a list with the YAML key and value at point, or nil if there is none."
  (when-let* ((bounds (bounds-of-thing-at-point 'line))
	      (line (buffer-substring-no-properties (car bounds) (cdr bounds)))
	      (elts (split-string line ":" nil "\\s-+")))
    elts))

(defun tlon-babel-yaml-get-key (key)
  "Get value of KEY in YAML metadata."
  (alist-get key (tlon-babel-yaml-get-metadata) nil nil #'string=))

(defun tlon-babel-yaml-insert-list (candidates)
  "Insert a list in YAML field at point.
Prompt the user to select one or more elements in CANDIDATES. If point is on a
list, use them pre-populate the selection."
  (let* ((bounds (bounds-of-thing-at-point 'line))
	 ;; retrieve the line
	 (line (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when (string-match "\\[\\(.*?\\)\\]" line)
      ;; retrieve and parse the elements in the list at point, removing quotes
      (let ((elems-at-point (mapcar (lambda (s)
				      (replace-regexp-in-string "\\`\"\\|\"\\'" "" s))
				    (split-string (match-string 1 line) ", "))))
	;; prompt the user to select multiple elements from the list,
	;; prefilling with previously selected items
	(let ((choices (completing-read-multiple "Value (comma-separated): "
						 candidates
						 nil nil
						 (mapconcat 'identity elems-at-point ", "))))
	  ;; delete the old line
	  (delete-region (car bounds) (cdr bounds))
	  ;; insert the new line into the current buffer
	  (insert (replace-regexp-in-string "\\[.*?\\]"
					    (concat "["
						    (mapconcat (lambda (item)
								 (format "\"%s\"" item))
							       choices ", ")
						    "]")
					    line)))))))

(defun tlon-babel-yaml-insert-string (candidates)
  "Insert a string in the YAML field at point.
Prompt the user for a choice in CANDIDATES. If point is on a string, use it to
pre-populate the selection."
  (cl-destructuring-bind (key _) (tlon-babel-yaml-get-field-at-point)
    (let* ((choice (completing-read (format "Value of `%s': " key)
				    candidates))
	   (bounds (bounds-of-thing-at-point 'line)))
      (delete-region (car bounds) (cdr bounds))
      (insert (format "%s:  %s\n" key choice)))))


;;;;; Get metadata

;;;;;; Get repo-specific entities

(defun tlon-babel-metadata-get-uqbar-fields (type &optional field language)
  "Return FIELDS in files of TYPE in `uqbar' repo of LANGUAGE.
If FIELD is nil, default to \"title\". If LANG is nil, default to
`tlon-babel-translation-language'."
  (let* ((field (or field "title"))
	 (language (or language tlon-babel-translation-language))
	 (type-in-language (tlon-babel-get-bare-dir-translation language "en" type))
	 (repo (tlon-babel-repo-lookup :dir :subproject "uqbar" :language language)))
    (tlon-babel-metadata-lookup-all
     (tlon-babel-metadata-in-repo repo) field "file" (file-name-concat repo type-in-language))))

(defun tlon-babel-metadata-get-all-uqbar-entities ()
  "Get a list of all `uqbar-en' entities."
  (append
   (tlon-babel-metadata-get-uqbar-fields "articles")
   (tlon-babel-metadata-get-uqbar-fields "authors")
   (tlon-babel-metadata-get-uqbar-fields "tags")))

;;;;;; Create repo-specific entities

(defun tlon-babel-create-uqbar-entity (dir)
  "Create a new file for `uqbar-es' entity in DIR."
  (let ((default-directory (file-name-concat
			    (tlon-babel-repo-lookup :dir :name "uqbar-es")
			    (file-name-as-directory dir))))
    (files-extras-new-empty-buffer)
    (tlon-babel-yaml-set-metadata tlon-babel-yaml-tag-or-author-keys)
    (goto-char (point-max))
    (tlon-babel-name-file-from-title)
    (insert (format "**%s** es " (tlon-babel-metadata-get-field-value-in-file "title")))
    (save-buffer)))

(defun tlon-babel-name-file-from-title (&optional title)
  "Save the current buffer to a file named after TITLE.
Set the name to the slugified version of TITLE with the extension `.md'. If
TITLE is nil, get it from the file metadata. If the file doesn't have metadata,
prompt the user for a title.

When buffer is already visiting a file, prompt the user for confirmation before
renaming it."
  (interactive)
  (let* ((title (or title
		    (tlon-babel-metadata-get-field-value-in-file "title")
		    (read-string "Title: ")))
	 (target (tlon-babel-set-file-from-title title default-directory)))
    (if-let ((buf (buffer-file-name)))
	(when (yes-or-no-p (format "Rename `%s` to `%s`? "
				   (file-name-nondirectory buf)
				   (file-name-nondirectory target)))
	  (rename-file buf target)
	  (set-visited-file-name target)
	  (save-buffer))
      (write-file target))))

(defun tlon-babel-set-file-from-title (&optional title dir)
  "Set the file path based on its title.
The file name is the slugified version of TITLE with the extension `.md'. This
is appended to DIR to generate the file path. If DIR is not provided, use the
current repository followed by `originals/'."
  (let* ((title (or title
		    (read-string "Title: ")))
	 (filename (file-name-with-extension (tlon-core-slugify title) "md"))
	 (dirname (file-name-as-directory
		   (or dir
		       (file-name-concat (tlon-babel-get-repo) "originals")))))
    (file-name-concat dirname filename)))

;;;;;; Get repo-agnostic elements

(defun tlon-babel-metadata-get-field-value-in-file (field &optional file-or-buffer)
  "Return the value of FIELD in metadata of FILE-OR-BUFFER.
If FILE is nil, use the file visited by the current buffer."
  (when-let* ((file-or-buffer (or file-or-buffer
				  (buffer-file-name)
				  (current-buffer)))
	      (metadata (tlon-babel-metadata-in-file file-or-buffer)))
    (alist-get field metadata nil nil #'string=)))

(defun tlon-babel-metadata-get-translators ()
  "Get a list of translators in all `translations' repos."
  (tlon-babel-metadata-lookup-all
   (tlon-babel-metadata-in-repos :subtype 'translations)
   "translators"))

(provide 'tlon-babel-yaml)
;;; tlon-babel-yaml.el ends here
