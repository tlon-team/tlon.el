;;; tlon-yaml.el --- Parse, get, set & edit YAML -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini

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

;;;; Variables

(defconst tlon-yaml-delimiter "---\n"
  "Delimiter for YAML metadata.")

(defconst tlon-yaml-core-keys
  '("type" "original_path")
  "List of YAML keys necessary to initialize the translation metadata.")

(defconst tlon-yaml-translation-only-keys
  '("original_path" "publication_status")
  "List of YAML keys included in translation metadata only.
I.e., these keys are not included in the metadata of originals.")

(defconst tlon-yaml-article-keys
  '("type" "title" "authors" "translators" "tags" "date" "original_path" "original_key" "translation_key" "publication_status" "description")
  "List of YAML keys of fields to include in articles.
The order of the keys determines the sort order by
`tlon-yaml-sort-fields', unless overridden.")

(defconst tlon-yaml-tag-keys
  '("type" "title" "brief_title" "original_path" "publication_status")
  "List of YAML keys of fields to include in tags.
The order of the keys determines the sort order by
`tlon-yaml-sort-fields', unless overridden.")

(defconst tlon-yaml-author-keys
  '("type" "title" "original_path" "publication_status")
  "List of YAML keys of fields to include in authors.
The order of the keys determines the sort order by
`tlon-yaml-sort-fields', unless overridden.")

(defconst tlon-yaml-collection-keys
  '("title" "original_path" "publication_status")
  "List of YAML keys of fields to include in collections.
The order of the keys determines the sort order by
`tlon-yaml-sort-fields', unless overridden.")

(defconst tlon-yaml-tag-or-author-keys
  '("titulo" "estado_de_publicacion")
  "List of YAML keys of fields to include in BAE tags or authors.
The order of the keys determines the sort order by
`tlon--yaml-sort-fields', unless overridden.")

(defconst tlon-yaml-original-author-keys
  '("title")
  "List of YAML keys of fields to include in `uqbar-es' authors.
The order of the keys determines the sort order by
`tlon-yaml-sort-fields', unless overridden.")

(defconst tlon-yaml-publication-statuses
  '("unpublished" "test" "production")
  "List of publication statuses.")

;;;; Functions

;;;;; Parse

;; I tried an Elisp implementation of YAML parsing
;; (https://github.com/zkry/yaml.el), but it was too slow. The unused function
;; is `tlon-yaml-get-metadata2'.
;;
;; There is also a C-based parser, but it doesn’t support association lists and
;; installing it is a hassle: https://github.com/syohex/emacs-libyaml

(defun tlon-yaml-to-alist (strings)
  "Convert YAML STRINGS to an alist."
  (let ((metadata '()))
    (dolist (line strings)
      (when (string-match "^\\(.*?\\):\\s-+\\(.*\\)$" line)
	(let* ((key (match-string 1 line))
	       (value (match-string 2 line))
	       (trimmed-value (string-trim value)))
	  (push (cons (string-trim key) trimmed-value) metadata))))
    (nreverse metadata)))

(defun tlon-yaml-format-values-of-alist (alist)
  "Format the values of ALIST, converting from YAML format to Elisp format."
  (mapcar (lambda (pair)
	    (cons (car pair)
		  (tlon-yaml-format-value (cdr pair))))
	  alist))

(defun tlon-yaml-format-value (value)
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

(defun tlon-yaml-read-until-match (delimiter)
  "Return a list of lines until DELIMITER is matched.
The delimiter is not included in the result. If DELIMITER is not found, signal
an error."
  (let ((result '()))
    (while (not (or (looking-at-p delimiter) (eobp)))
      (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) result)
      (forward-line))
    (if (eobp)
	(user-error (format "YAML delimiter not found in `%s'" (buffer-string)))
      (nreverse result))))

(defun tlon-yaml-convert-list (list)
  "Convert an Elisp LIST to a YAML list."
  (concat "[\"" (mapconcat 'identity list "\", \"") "\"]"))

;;;;; Get

;;;###autoload
(defun tlon-metadata-in-repos (&rest pairs)
  "Return metadata of all repos matching all PAIRS.
PAIRS is an even-sized list of <key value> tuples."
  (let (metadata)
    (dolist (repo (tlon-repo-lookup-all :dir pairs))
      (setq metadata (append (tlon-metadata-in-repo repo) metadata)))
    metadata))

;;;###autoload
(defun tlon-metadata-in-repo (&optional repo)
  "Return metadata of REPO.
If REPO is nil, return metadata of the current repository."
  (let* ((repo (or repo (tlon-get-repo))))
    (when (eq (tlon-repo-lookup :type :dir repo) 'content)
      (tlon-metadata-in-dir repo))))

;;;###autoload
(defun tlon-metadata-in-dir (dir)
  "Return the metadata in DIR and all its subdirectories as an association list."
  (let (metadata)
    (dolist (file (directory-files-recursively dir "\\.md$"))
      (when-let ((meta-in-file (tlon-metadata-in-file file)))
	(push meta-in-file metadata)))
    metadata))

;;;###autoload
(defun tlon-metadata-in-file (&optional file-or-buffer)
  "Return the metadata in FILE-OR-BUFFER as an association list.
This includes the YAML metadata at the beginning of the file, as well as
additional metadata such as the file name and the file type."
  (let* ((file-or-buffer (or file-or-buffer (buffer-file-name)))
	 (metadata (tlon-yaml-get-metadata file-or-buffer))
	 (repo (tlon-get-repo-from-dir (or
					(when-let ((file (buffer-file-name)))
					  (file-name-directory file))
					default-directory)))
	 (language (tlon-repo-lookup :language :dir repo))
	 (extras `(("file" . ,file-or-buffer)
		   ("database" . "Tlön")
		   ("landid" . ,language))))
    (append metadata extras)))

(defun tlon-yaml-get-metadata (&optional file-or-buffer raw)
  "Return the YAML metadata from FILE-OR-BUFFER as strings in a list.
If FILE-OR-BUFFER is nil, use the current buffer. Return the metadata as an
alist, unless RAW is non-nil."
  (let ((file-or-buffer (or file-or-buffer
			    (buffer-file-name)
			    (current-buffer))))
    (with-temp-buffer
      (cond
       ((bufferp file-or-buffer)
	(insert (with-current-buffer file-or-buffer (buffer-string))))
       ((stringp file-or-buffer)
	(insert-file-contents file-or-buffer)))
      (goto-char (point-min))
      (when (looking-at-p tlon-yaml-delimiter)
	(forward-line)
	(let ((metadata (tlon-yaml-read-until-match tlon-yaml-delimiter)))
	  (if raw
	      metadata
	    (tlon-yaml-to-alist metadata)))))))

(declare-function yaml-parse-string "yaml")
(defun tlon-yaml-get-metadata2 (&optional file-or-buffer raw)
  "Return the YAML metadata from FILE-OR-BUFFER as strings in a list.
If FILE-OR-BUFFER is nil, use the current buffer. Return the metadata as an
alist, unless RAW is non-nil."
  (let ((file-or-buffer (or file-or-buffer
			    (buffer-file-name)
			    (current-buffer))))
    (with-temp-buffer
      (cond
       ((bufferp file-or-buffer)
	(insert (with-current-buffer file-or-buffer (buffer-string))))
       ((stringp file-or-buffer)
	(insert-file-contents file-or-buffer)))
      (when-let ((metadata-pos (tlon-get-delimited-region-pos tlon-yaml-delimiter nil t)))
	(cl-destructuring-bind (begin . end) metadata-pos
	  (let ((metadata (buffer-substring-no-properties begin end)))
	    (if raw
		metadata
	      (yaml-parse-string metadata :object-type 'alist))))))))

(defun tlon-yaml-sort-fields (fields &optional keys no-error)
  "Sort alist of YAML FIELDS by order of KEYS.
If one of FIELDS is not found, throw an error unless NO-ERROR is non-nil."
  (cl-remove-if #'null
		(mapcar (lambda (key)
			  (if-let ((match (assoc key fields)))
			      match
			    (unless no-error
			      (user-error "Key `%s' not found in file `%s'" key (buffer-file-name)))))
			keys)))

(defun tlon-yaml-get-valid-keys (&optional file type no-core)
  "Return the admissible keys for YAML metadata in FILE.
If FILE is nil, return the work type of the file visited by the current buffer.
If TYPE is nil, use the value of the `type' field in FILE. If NO-CORE is
non-nil, exclude core keys, as defined in `tlon-yaml-core-keys'."
  (let* ((file (or file (buffer-file-name)))
	 (type (or type (tlon-yaml-get-key "type" file)))
	 (keys (pcase type
		 ("article" tlon-yaml-article-keys)
		 ("tag" tlon-yaml-tag-keys)
		 ("author" tlon-yaml-author-keys)
		 ("collection" tlon-yaml-collection-keys))))
    (if no-core
	(cl-remove-if (lambda (key)
			(member key tlon-yaml-core-keys))
		      keys)
      keys)))

(defun tlon-yaml-get-filenames-in-dir (&optional dir extension)
  "Return a list of all filenames in DIR.
If DIR is nil, use the current directory. EXTENSION defaults to \"md\". If you
want to search all files, use the empty string."
  (let* ((dir (or dir default-directory))
	 (extension (or extension "md"))
	 (extension-regex (format "\\.%s$" extension))
	 (files (directory-files-recursively dir extension-regex)))
    (mapcar #'file-name-nondirectory files)))

;;;;; Set fields
;; fun2: set `translation_key' metadata field
;; fun3: get bibtex key from `translation_key' metadata field
;; fun4: compare existing bibtex key with bibtex key from `translation_key' metadata field

;; document process for creating a new translation:
;; 1. Import original article bibliographic details as bibtex entry
;; 2. Import original article content. This function should add a `bibtex_key'
;;    field to the metadata.
;; 3. Populate original metadata from bibtex fields.
;; 4. Create translation file via `tlon-create-translation-file'.
;; 5. Populate translation metadata from original metadata.
;; 6. Create translation bibtex entry from translation metadata.

;; originals and translations should have have a metadata section with the same
;; structure. in both cases, some fields will overlap with the bibtex fields. we
;; deal with this situation in the same way in both cases.

(declare-function tlon-get-bare-dir "tlon-counterpart")
(defun tlon-create-translation-file (&optional file language)
  "Create a new translation file for original FILE.
If FILE is nil, use the file visited by the current buffer. If LANGUAGE is nil,
use the value of `tlon-translation-language'."
  (interactive)
  ;; check that FILE has a bibtex key
  (when-let* ((file (or file (buffer-file-name)))
	      (language (or language (tlon-select-language 'code 'babel)))
	      (subproject (tlon-repo-lookup :subproject :dir (tlon-get-repo-from-file file)))
	      (repo (tlon-repo-lookup :dir :subproject subproject :language language))
	      (bare-dir (tlon-get-bare-dir-translation language "en" (tlon-get-bare-dir file)))
	      (title (read-string "Translated title: "))
	      (dir (file-name-concat repo bare-dir))
	      (path (tlon-set-file-from-title title dir)))
    (with-current-buffer (find-file-noselect path)
      (save-buffer)
      (tlon-initialize-translation-metadata path file))))

(defun tlon-initialize-translation-metadata (file original)
  "Set the initial metadata section for a FILE that translates ORIGINAL.
This function creates a new metadata section in FILE, and sets the value of
`type' and `original_path'. These values are needed for the remaining metadata
to be set via `tlon-populate-translation-metadata'."
  (with-current-buffer (find-file-noselect file)
    (let ((type (tlon-yaml-get-key "type" original)))
      ;; consider using a fun that sets all fields at once
      ;; consider setting it by reference to `tlon-yaml-core-keys'
      (tlon-yaml-insert-field "type" type)
      (tlon-yaml-insert-field "original_path" (file-name-nondirectory original)))))

(defun tlon-yaml-set-key (key)
  "Set the value of the YAML field with KEY."
  (tlon-yaml-convert-list
   (completing-read-multiple
    (format "%s: " key)
    (tlon-yaml-get-key-values key))))

;;;;; Edit

(defun tlon-yaml-insert-metadata-section (&optional file)
  "Insert a YAML metadata section in FILE, when it does not already contain one.
If FILE is nil, use the file visited by the current buffer."
  (let ((file (or file (buffer-file-name))))
    (when (tlon-yaml-get-metadata file)
      (user-error "File `%s' already contains a metadata section" file))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (insert (format "%1$s\n%1$s" tlon-yaml-delimiter))
      (save-buffer))))

;; TODO: throw error if any of fields already present
(defun tlon-yaml-insert-fields (fields)
  "Insert YAML FIELDS in the buffer at point.
FIELDS is an alist, typically generated via `tlon-yaml-to-alist'."
  (when (looking-at-p tlon-yaml-delimiter)
    (user-error "File appears to already contain a metadata section"))
  (save-excursion
    (goto-char (point-min))
    ;; calculate the max key length
    (let ((max-key-len (cl-reduce 'max (mapcar (lambda (cons)
						 (length (car cons)))
					       fields)))
	  format-str)
      ;; determine the format for string
      (setq format-str (format "%%-%ds %%s\n" (+ max-key-len 2)))
      ;; insert the yaml delimiter & fields
      (insert tlon-yaml-delimiter)
      (dolist (cons fields)
	(insert (format format-str (concat (car cons) ":") (cdr cons))))
      (insert tlon-yaml-delimiter))))

(defun tlon-yaml-delete-metadata ()
  "Delete YAML metadata section."
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p tlon-yaml-delimiter)
      (user-error "File does not appear to contain a metadata section"))
    (forward-line)
    (re-search-forward tlon-yaml-delimiter)
    (delete-region (point-min) (point))))

(defun tlon-yaml-reorder-metadata ()
  "Reorder the YAML metadata in the buffer at point."
  (save-excursion
    (let* ((unsorted (tlon-yaml-get-metadata))
	   (sorted (tlon-yaml-sort-fields
		    unsorted (tlon-yaml-get-valid-keys) 'no-error)))
      (tlon-yaml-delete-metadata)
      (tlon-yaml-insert-fields sorted))))

;;;;; Interactive editing

(defun tlon-yaml-insert-field (&optional key value)
  "Insert YAML field with KEY and VALUE in the metadata section.
If KEY or VALUE are nil, prompt user to select from list of suitable candidates."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p tlon-yaml-delimiter)
      (tlon-yaml-insert-metadata-section))
    (let* ((candidates (tlon-yaml-get-valid-keys))
	   (key (or key (completing-read "key:" candidates))))
      (when (tlon-yaml-get-key key)
	(if (y-or-n-p (format "Field '%s' already exists; replace?" key))
	    (tlon-yaml-delete-field key)
	  (user-error "Aborted")))
      (let* ((value (or value
			(completing-read (format "%s: " key)
					 (tlon-metadata-lookup-all
					  (tlon-metadata-in-repo) key))))
	     (formatted-value (cond
			       ((or (vectorp value) (listp value))
				(format "[%s]"
					(mapconcat (lambda (item)
						     (format "\"%s\"" item))
						   (if (vectorp value)
						       (append value nil)
						     value)
						   ", ")))
			       ((stringp value) value)
			       (t (format "%s" value))))
	     (new-field (format "%-20s %s" (concat key ":") formatted-value))
	     (target-pos (cl-position key candidates :test #'string=))
	     (next-key (nth (1+ target-pos) candidates)))
	(goto-char (point-min))
	(forward-line)
	(when next-key
	  (search-forward-regexp (format "^%s:" next-key) nil t)
	  (beginning-of-line))
	(insert new-field "\n")))))

;;;###autoload
(defun tlon-yaml-insert-original-path ()
  "Insert the value of `original_path' YAML field."
  (interactive)
  (tlon-yaml-insert-field "original_path"))

;; create an edit field command
;; then an insert or edit command

(defun tlon-yaml-set-key-value (key)
  "Set the value of the YAML field with KEY."
  (let ((values (tlon-yaml-get-key-values key)))
    (tlon-yaml-select-key-values key values)))

;;;###autoload
(defun tlon-yaml-get-key-values (key)
  "Return the admissible values for a YAML field with KEY."
  (pcase key
    ;; ("type" )
    ("authors" (tlon-get-metadata-values-of-type "author"))
    ("translators" (tlon-metadata-get-translators))
    ("tags" (tlon-get-metadata-values-of-type "tag")) ; make it language-specific
    ("date" (format-time-string "%FT%T%z"))
    ("original_path" (tlon-yaml-get-original-path))
    ("publication_status" tlon-yaml-publication-statuses)))

(defun tlon-yaml-select-key-values (key values)
  "Select a value for a YAML field with KEY from a list of VALUES."
  (let ((prompt (format "%s: " key)))
    (pcase key
      ((or "type" "authors" "translators" "tags" "original_path" "publication_status")
       (completing-read prompt values))
      ((or "title" "date" "description")
       (read-string prompt values))
      ((or "original_key" "translation_key")
       (tlon-yaml-get-bibtex-key-values key)))))

(defun tlon-yaml-insert-key-value (key value)
  "Insert a YAML field with KEY and VALUE."
  (pcase key
    ((or "authors" "translators" "tags")
     (tlon-yaml-insert-list value))
    ((or "type" "type" "date" "original_path" "original_key" "translation_key" "publication_status" "description")
     (tlon-yaml-insert-string value))))

(defun tlon-get-bibtex-key (&optional initial-input)
  "Get the BibTeX key of the selected work.
INITIAL-INPUT is used as the initial input for the completion."
  (car (split-string
	(completing-read
	 "English original: "
	 (citar--completion-table (citar--format-candidates) nil)
	 nil
	 nil
	 (when initial-input (concat initial-input " "))
	 'citar-history citar-presets nil))))

(autoload 'tlon-get-content-subtype "tlon-counterpart")
(defun tlon-yaml-get-bibtex-key-values (field)
  "Return the BibTeX key of the work at point apprporiate for FIELD.
If FIELD is `original_key', return the BibTeX key of the original work. If FIELD
is `translation_key', return the BibTeX key of the translation"
  (let* ((first-author (car (tlon-yaml-get-key "authors")))
	 (initial-input (car (last (split-string first-author))))
	 (original-key (tlon-get-bibtex-key initial-input))
	 (subtype (tlon-get-content-subtype)))
    (cond ((or (eq subtype 'originals) (string= field "translation_key"))
	   original-key)
	  ((and (eq subtype 'translations) (string= field "original_key"))
	   (citar-get-value "translation" original-key)))))

(defun tlon-yaml-write-field (key value file)
  "Set KEY to VALUE in FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (when (looking-at-p tlon-yaml-delimiter)
      (forward-line)
      (insert (format "%s:  %s\n" key value))
      (save-buffer)
      (tlon-yaml-reorder-metadata))))

;; TODO: refactor with above
(defun tlon-yaml-delete-field (&optional key file)
  "Delete the YAML field with KEY in FILE."
  (let ((key (or key (completing-read "Field: " (tlon-yaml-get-valid-keys))))
	(file (or file (buffer-file-name))))
    (if-let ((metadata (tlon-yaml-get-metadata file)))
	(if (assoc key metadata)
	    (with-current-buffer (find-file-noselect file)
	      (goto-char (point-min))
	      (re-search-forward (format "%s:.*\n" key))
	      (delete-region (match-beginning 0) (match-end 0))
	      (save-buffer))
	  (user-error "Key `%s' not found in file `%s'" key file))
      (user-error "File does not appear to contain a metadata section"))))

;; TODO: Handle multiline fields, specifically `description’
;; TODO: make it throw an error unless looking at metadata
(defun tlon-yaml-get-field-at-point ()
  "Return a list with the YAML key and value at point, or nil if there is none."
  (when-let* ((bounds (bounds-of-thing-at-point 'line))
	      (line (buffer-substring-no-properties (car bounds) (cdr bounds)))
	      (elts (split-string line ":" nil "\\s-+")))
    elts))

(defun tlon-yaml-get-key (key &optional file-or-buffer)
  "Get value of KEY in YAML metadata of FILE-OR-BUFFER.
If FILE is nil, use the file visited by the current buffer."
  (when-let* ((file-or-buffer (or file-or-buffer
				  (buffer-file-name)
				  (current-buffer)))
	      (metadata (tlon-metadata-in-file file-or-buffer)))
    (alist-get key metadata nil nil #'string=)))

(defun tlon-yaml-insert-list (candidates)
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

(defun tlon-yaml-insert-string (candidates)
  "Insert a string in the YAML field at point.
Prompt the user for a choice in CANDIDATES. If point is on a string, use it to
pre-populate the selection."
  (cl-destructuring-bind (key _) (tlon-yaml-get-field-at-point)
    (let* ((choice (completing-read (format "Value of `%s': " key)
				    candidates))
	   (bounds (bounds-of-thing-at-point 'line)))
      (delete-region (car bounds) (cdr bounds))
      (insert (format "%s:  %s\n" key choice)))))

;;;;; Get metadata

;;;;;; Get repo-specific entities

(defun tlon-get-metadata-values-of-type (type &optional language current-repo)
  "Return all metadata values of TYPE.
Search all repos of `translations' subtype in LANGUAGE. If LANGUAGE is nil,
default to `tlon-translation-language'. If CURRENT-REPO is non-nil,
restrict search to the current repository."
  (let ((repos (if current-repo
		   (list (tlon-get-repo))
		 (tlon-repo-lookup-all
		  :dir
		  :subtype 'translations
		  :language (or language tlon-translation-language))))
	metadata)
    (dolist (repo repos)
      (setq metadata
	    (append metadata
		    (tlon-metadata-lookup-all
		     (tlon-metadata-in-repo repo)
		     ;; TODO: add `type' field to metadata in utilitarianism
		     "title" "type" type))))
    metadata))

(defun tlon-metadata-get-values-of-all-types (&optional language current-repo)
  "Get a list of all `uqbar-en' entities.
Search all repos of `translations' subtype in LANGUAGE. If LANGUAGE is nil,
default to `tlon-translation-language'. If CURRENT-REPO is non-nil,
restrict search to the current repository."
  (append
   (tlon-get-metadata-values-of-type "article" language current-repo)
   (tlon-get-metadata-values-of-type "author" language current-repo)
   (tlon-get-metadata-values-of-type "tag" language current-repo)))

;;;;;; Create repo-specific entities

(defun tlon-name-file-from-title (&optional title)
  "Save the current buffer to a file named after TITLE.
Set the name to the slugified version of TITLE with the extension `.md'. If
TITLE is nil, get it from the file metadata. If the file doesn't have metadata,
prompt the user for a title.

When buffer is already visiting a file, prompt the user for confirmation before
renaming it."
  (interactive)
  (let* ((title (or title
		    (tlon-yaml-get-key "title")
		    (read-string "Title: ")))
	 (target (tlon-set-file-from-title title default-directory)))
    (if-let ((buf (buffer-file-name)))
	(when (yes-or-no-p (format "Rename `%s` to `%s`? "
				   (file-name-nondirectory buf)
				   (file-name-nondirectory target)))
	  (rename-file buf target)
	  (set-visited-file-name target)
	  (save-buffer))
      (write-file target))))

(defun tlon-yaml-get-original-path ()
  "Set the value of `original_path' YAML field."
  (let* ((subproject (tlon-repo-lookup :subproject :dir (tlon-get-repo)))
	 (dir (tlon-repo-lookup :dir :subproject subproject :language "en")))
    (tlon-yaml-get-filenames-in-dir dir)))

(declare-function simple-extras-slugify "simple-extras")
(defun tlon-set-file-from-title (&optional title dir)
  "Set the file path based on its title.
The file name is the slugified version of TITLE with the extension `.md'. This
is appended to DIR to generate the file path. If DIR is not provided, prompt the
user for one."
  (let* ((title (or title (read-string "Title: ")))
	 (filename (file-name-with-extension (simple-extras-slugify title) "md"))
	 (dirname (file-name-as-directory (or dir (tlon-get-repo)))))
    (file-name-concat dirname filename)))

;;;;;; Get repo-agnostic elements

(defun tlon-metadata-get-translators ()
  "Get a list of translators in all `translations' repos."
  (tlon-metadata-lookup-all
   (tlon-metadata-in-repos :subtype 'translations)
   "translators"))

(provide 'tlon-yaml)
;;; tlon-yaml.el ends here
