;;; tlon-yaml.el --- Parse, get, set & edit YAML -*- lexical-binding: t -*-

;; Copyright (C) 2025

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
(require 'tlon-ai)
(require 'simple-extras)
(require 'seq)
(require 'json)

;;;; Variables

(defconst tlon-yaml-core-keys
  '("original_path")
  "List of YAML keys necessary to initialize the translation metadata.
The type is derived from the directory structure and is not stored in YAML.")

(defconst tlon-yaml-translation-only-keys
  '("original_path" "publication_status")
  "List of YAML keys included in translation metadata only.
I.e., these keys are not included in the metadata of originals.")

(defconst tlon-yaml-article-keys
  '("title" "authors" "translators" "tags" "date" "original_path" "original_key" "translation_key" "publication_status" "description")
  "List of YAML keys of fields to include in articles.
The order of the keys determines the sort order by
`tlon-yaml-sort-fields', unless overridden.")

(defconst tlon-yaml-tag-keys
  '("title" "brief_title" "original_path" "publication_status")
  "List of YAML keys of fields to include in tags.
The order of the keys determines the sort order by
`tlon-yaml-sort-fields', unless overridden.")

(defconst tlon-yaml-author-keys
  '("title" "original_path" "publication_status")
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

;;;;; AI

(defconst tlon-yaml-suggest-tags-prompt
  "You are an assistant that assigns encyclopedia tags to articles. Below you will find, first, the full Markdown of one article delimited by <ARTICLE> … </ARTICLE>.  After that you will see a catalogue of candidate tags.  Each candidate starts with the tag *title* on its own line and is immediately followed by its first descriptive paragraph; candidates are separated by one blank line.\n Return ONLY a JSON array whose elements are the titles of the tags in the list below that best apply to the article, ordered from most to least relevant. Suggest only the most relevant tags; you should not suggest more than six tags. Do not output any other prose, comments, or code blocks.\n <ARTICLE>\n%s\n</ARTICLE>\n <CANDIDATE TAGS>\n%s\n</CANDIDATE TAGS>"
  "Prompt used by `tlon-yaml-suggest-tags'.")

(defconst tlon-yaml-guess-counterpart-prompt
  "You are an assistant that maps a tag/author title to its English filename.\nTitle: \"%s\"\n\nBelow is a list of candidate Markdown filenames, one per line.\nReturn ONLY the single exact filename from the list that corresponds to the\nEnglish counterpart of the title. Do not include any extra text, quotes, code\nfences, or explanations.\n\n%s"
  "Prompt used by `tlon-yaml-guess-english-counterpart'.")

;;;; User options

(defgroup tlon-yaml ()
  "`tlon-yaml' group."
  :group 'tlon)

(defcustom tlon-yaml-suggest-tags-model nil
  "AI model to use for `tlon-yaml-suggest-tags'.
The value is a cons cell (BACKEND . MODEL) like the other
`tlon-yaml-*model' custom variables.  When nil, use the current
`gptel' default model."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-yaml)

(defcustom tlon-yaml-guess-counterpart-model nil
  "AI model to use for `tlon-yaml-guess-english-counterpart'.
The value is a cons cell (BACKEND . MODEL). When nil, use the current
`gptel' default model."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-yaml)

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
		   ("landid" . ,language)
                   ("type" . ,(tlon-yaml-get-type file-or-buffer)))))
    (append metadata extras)))

;;;###autoload
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
(declare-function tlon-get-bare-dir-translation "tlon-core")
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
If FILE is nil, use the current buffer’s file. If TYPE is nil, derive it from
the directory name (articles, tags, authors, collections). If NO-CORE is
non-nil, exclude core keys as defined in `tlon-yaml-core-keys'."
  (let* ((file (or file (buffer-file-name)))
	 (type (or type (tlon-yaml-get-type file)))
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

(defun tlon-yaml-get-type (&optional file)
  "Return canonical type for FILE based on its directory.
The result is one of \"article\", \"tag\", \"author\" or \"collection\".
When FILE is nil, use the current buffer's file."
  (let* ((file (or file (buffer-file-name)))
         (repo (and file (tlon-get-repo-from-file file)))
         (lang (and repo (tlon-repo-lookup :language :dir repo))))
    (unless (and file repo lang)
      (user-error "Cannot determine type for file %s" file))
    (let* ((subproject (tlon-repo-lookup :subproject :dir repo)))
      (if (not (string= subproject "uqbar"))
          "article"
        (let* ((rel (file-relative-name file repo))
               (first (downcase (car (split-string rel "/" t))))
               (articles (tlon-get-bare-dir-translation lang "en" "articles"))
               (tags (tlon-get-bare-dir-translation lang "en" "tags"))
               (authors (tlon-get-bare-dir-translation lang "en" "authors"))
               (collections (tlon-get-bare-dir-translation lang "en" "collections"))
               (articles-d (and (stringp articles) (downcase articles)))
               (tags-d (and (stringp tags) (downcase tags)))
               (authors-d (and (stringp authors) (downcase authors)))
               (collections-d (and (stringp collections) (downcase collections))))
          (cond
           ((and articles-d (string= first articles-d)) "article")
           ((and tags-d (string= first tags-d)) "tag")
           ((and authors-d (string= first authors-d)) "author")
           ((and collections-d (string= first collections-d)) "collection")
           (t (user-error "Cannot determine YAML type for %s" file))))))))

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

(defun tlon-initialize-translation-metadata (file original)
  "Set the initial metadata section for a FILE that translates ORIGINAL.
This function creates a new metadata section in FILE, and sets the value of
`original_path'. The type is derived from the directory structure."
  (with-current-buffer (find-file-noselect file)
    ;; consider using a fun that sets all fields at once
    ;; consider setting it by reference to `tlon-yaml-core-keys'
    (tlon-yaml-insert-field "original_path" (file-name-nondirectory original))))

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
      (insert (format "%1$s%1$s" tlon-yaml-delimiter))
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

;;;###autoload
(defun tlon-yaml-insert-field (&optional key value overwrite)
  "Insert YAML field with KEY and VALUE in the metadata section.
If KEY or VALUE are nil, prompt user to select from list of suitable candidates.
If OVERWRITE is non-nil, overwrite existing field without asking."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p tlon-yaml-delimiter)
      (tlon-yaml-insert-metadata-section))
    (let* ((candidates (tlon-yaml-get-valid-keys))
	   (key (or key (completing-read "key:" candidates))))
      (when (tlon-yaml-get-key key)
	(if (or overwrite (y-or-n-p (format "Field '%s' already exists; replace?" key)))
	    (tlon-yaml-delete-field key)
	  (user-error "Aborted")))
      (let* ((value (or value ;; This `value` is the raw string from AI or user input.
			(completing-read (format "%s: " key)
					 (tlon-yaml-get-key-values key))))
             (new-field
              (if (string= key "meta") ;; Check if the key is "meta"
                  ;; Format for "meta" using literal block scalar style.
                  ;; (string-trim value) removes potential leading/trailing newlines from AI.
                  ;; (replace-regexp-in-string "\n" "\n  " ...) ensures subsequent lines are indented.
                  (format "%s: >\n%s" key
                          (concat "  " (replace-regexp-in-string "\n" "\n  " (string-trim value))))
                ;; Original formatting logic for other keys
                (let ((formatted-value (cond
                                        ((or (vectorp value) (listp value))
                                         (format "[%s]"
                                                 (mapconcat (lambda (item)
                                                              (format "\"%s\"" item))
                                                            (if (vectorp value)
                                                                (append value nil)
                                                              value)
                                                            ", ")))
                                        ((stringp value) value)
                                        (t (format "%s" value)))))
                  (format "%-20s %s" (concat key ":") formatted-value))))
             (target-pos (cl-position key candidates :test #'string=))) ; target-pos is nil if key not in candidates
        (goto-char (point-min)) ; Go to start of buffer
        (unless (looking-at-p tlon-yaml-delimiter)
          (user-error "YAML opening delimiter not found before insertion logic"))
        (forward-line) ; Point is now after the opening "---"

        (if (string= key "meta")
            ;; For "meta" key, insert just before the closing delimiter
            (progn
              (unless (re-search-forward (regexp-quote tlon-yaml-delimiter) nil t)
                (user-error "Could not find closing YAML delimiter for meta insertion"))
              (goto-char (match-beginning 0)) ; Go to the start of the closing "---"
              (insert new-field "\n"))
          ;; For other keys, use refined existing logic for ordered insertion
          (progn
            (when target-pos ; Key is in the ordered list of candidates
              (when-let ((next-key (nth (1+ target-pos) candidates)))
                ;; Try to find the line where next-key starts, to insert before it.
                ;; Search from current point up to the closing delimiter.
                (let ((boundary-for-search
                       (save-excursion ; Calculate boundary without moving main point
                         (unless (re-search-forward (regexp-quote tlon-yaml-delimiter) nil t)
                           (user-error "Closing YAML delimiter not found for search boundary"))
                         (match-beginning 0))))
                  (if (search-forward-regexp (format "^%s:" next-key) boundary-for-search t)
                      (goto-char (match-beginning 0)) ; Found next-key, point is at its start
                    ;; Else: next-key not found in the YAML block. Point remains where it was.
                    nil))))
            ;; Point is now:
            ;; 1. After opening "---" (if key not in candidates, or key is last in candidates,
            ;;    or its next-key from candidates was not found in the file).
            ;; 2. At the start of next-key's line (if key in candidates and next-key was found).
            (insert new-field "\n"))))
      ;; (tlon-yaml-reorder-metadata) ; This would re-sort; not called here directly.
      (save-buffer))))

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
    ("authors" (tlon-metadata-lookup-all (tlon-metadata-in-repo) "title" "type" "author"))
    ("translators" (tlon-metadata-get-translators))
    ("tags" (tlon-metadata-lookup-all (tlon-metadata-in-repo) "title" "type" "tag"))
    ("date" (format-time-string "%FT%T%z"))
    ("original_path" (tlon-yaml-get-original-path))
    ("publication_status" tlon-yaml-publication-statuses)))

(defun tlon-yaml-select-key-values (key values)
  "Select a value for a YAML field with KEY from a list of VALUES."
  (let ((prompt (format "%s: " key)))
    (pcase key
      ((or "authors" "translators" "tags" "original_path" "publication_status")
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
    ((or "date" "original_path" "original_key" "translation_key" "publication_status" "description")
     (tlon-yaml-insert-string value))))

(autoload 'dired-get-file-for-visit "dired")
(defvar tlon-db-publication-statuses)

;;;###autoload
(defun tlon-yaml-set-publication-status (&optional status)
  "Set the YAML publication_status of the Markdown file at point to STATUS.
If STATUS is nil, prompt for a status from `tlon-db-publication-statuses',
defaulting to the existing value. Signal an error for non-Markdown files. When
the file is an article, signal an error suggesting
`tlon-db-set-publication-status' instead."
  (interactive)
  (let* ((file (or (buffer-file-name)
                   (when (derived-mode-p 'dired-mode) (dired-get-file-for-visit))
                   (user-error "No file at point")))
         (ext (file-name-extension file)))
    (unless (and ext (string= ext "md"))
      (user-error "This command only works on Markdown files"))
    (when (string= (tlon-yaml-get-type file) "article")
      (user-error "To set the publication status of an article, please use `tlon-db-set-publication-status` instead"))
    (let* ((current (tlon-yaml-get-key "publication_status" file))
           (choices tlon-db-publication-statuses)
           (final-status (or status
                             (completing-read "Publication status: " choices nil t current nil current))))
      (with-current-buffer (find-file-noselect file)
        (tlon-yaml-insert-field "publication_status" final-status t)
        (save-buffer))
      final-status)))

;;;;; tags format normalization

;;;###autoload
(defun tlon-yaml-normalize-tags-field ()
  "Convert multiline `tags' YAML field (Format 1) to single-line Format 2.
When a conversion occurs, return non-nil."
  (interactive)
  (save-excursion
    ;; move inside the YAML block
    (goto-char (point-min))
    (when (looking-at-p tlon-yaml-delimiter)
      (forward-line))
    (let ((delimiter-pos (save-excursion
                           (re-search-forward tlon-yaml-delimiter nil t))))
      (when (and delimiter-pos
                 (re-search-forward "^tags:\\s-*$" delimiter-pos t))
        (let* ((field-start (match-beginning 0))
               (list-beg (progn (forward-line 1) (point)))
               (list-end (when (re-search-forward "]" delimiter-pos t)
                           (point))))
          (when list-end
            (let* ((raw (buffer-substring-no-properties list-beg list-end))
                   ;; collapse whitespace, then drop any trailing comma before the closing bracket
                   (collapsed (replace-regexp-in-string
                               ",\\s-*\\]$" "]"
                               (replace-regexp-in-string "[ \t\n]+" " "
                                                          (string-trim raw))))
                   (formatted (format "%-20s %s" "tags:" collapsed)))
              (delete-region field-start list-end)
              (insert formatted "\n")
              t)))))))

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
    (if-let ((metadata (tlon-yaml-get-metadata file))) ; Check if metadata section exists
	(if (assoc key metadata) ; Check if key exists in parsed metadata
	    (with-current-buffer (find-file-noselect file)
	      (save-excursion ; Preserve point if called interactively
                (goto-char (point-min))
                (if (re-search-forward (format "^%s:[ \t]*" key) nil t) ; Find "key:" possibly followed by spaces/tabs
                    (let ((field-start (match-beginning 0))
                          field-end)
                      ;; Determine the end of the field
                      (goto-char (line-end-position)) ; Go to end of the "key: ..." line
                      (forward-line 1) ; Move to the beginning of the next line

                      ;; Loop to consume indented lines (for block scalars like 'meta: >')
                      ;; The loop continues as long as the current line is indented
                      ;; AND it's not the closing YAML delimiter.
                      (while (and (not (eobp))
                                  (looking-at "[ \t]") ; Current line starts with space/tab (is indented)
                                  (not (looking-at-p (regexp-quote tlon-yaml-delimiter)))) ; And not the YAML delimiter
                        (forward-line 1))
                      ;; After the loop, point is at the beginning of the first line
                      ;; that is NOT part of the multi-line value (or at eobp, or at '---').
                      (setq field-end (point))
                      (delete-region field-start field-end)
                      (save-buffer))
                  ;; This case should ideally not be reached if `(assoc key metadata)` was true,
                  ;; indicating an inconsistency between parsed metadata and file content.
                  (user-error "Key `%s' was in parsed metadata but regex search failed in file `%s'" key file))))
	  (user-error "Key `%s' not found in metadata of file `%s'" key file))
      (user-error "File `%s' does not appear to contain a metadata section" file))))

;; TODO: Handle multiline fields, specifically `description’
;; TODO: make it throw an error unless looking at metadata
(defun tlon-yaml-get-field-at-point ()
  "Return a list with the YAML key and value at point, or nil if there is none."
  (when-let* ((bounds (bounds-of-thing-at-point 'line))
	      (line (buffer-substring-no-properties (car bounds) (cdr bounds)))
	      (elts (split-string line ":" nil "\\s-+")))
    elts))

;;;###autoload
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

;; unused
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

(defun tlon-metadata-get-values-of-all-types ()
  "Get a list of all `uqbar-en' entities."
  (append
   (tlon-metadata-lookup-all (tlon-metadata-in-repo) "title" "type" "article")
   (tlon-metadata-lookup-all (tlon-metadata-in-repo) "title" "type" "author")
   (tlon-metadata-lookup-all (tlon-metadata-in-repo) "title" "type" "tag")))

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

(autoload 'tlon-get-counterpart-dir "tlon-counterpart")
(defun tlon-yaml-get-original-path ()
  "Get the value of `original_path' YAML field.
Accumulate the list of all files of current type in the English version of the
current project. For example, if the current project is \"uqbar\" and the
current type is \"article\", get all filenames in \"uqbar-en/articles\"."
  (let* ((lang (tlon-get-language-in-file))
	 (dir (if (string= lang "en")
		  (let ((subproject (tlon-repo-lookup :subproject :dir (tlon-get-repo))))
		    (tlon-repo-lookup :dir :subproject subproject :language "en"))
		(tlon-get-counterpart-dir))))
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

;;;;;; Tag suggestion

(declare-function tlon-make-gptel-request "tlon-ai")
;;;###autoload
(defun tlon-yaml-suggest-tags (&optional article-file)
  "Suggest tags for ARTICLE-FILE using AI.
When called interactively, default to the current article or prompt for
one inside the uqbar-en/articles directory.  When ARTICLE-FILE is
non-nil, run non-interactively on that file without prompting."
  (interactive (list nil))
  (let* ((repo-dir (tlon-repo-lookup :dir :name "uqbar-en"))
         (articles-dir (file-name-concat repo-dir "articles/"))
         (default-filename (when (and (buffer-file-name)
				      (string-prefix-p (expand-file-name articles-dir)
						       (expand-file-name (buffer-file-name))))
			     (file-name-nondirectory (buffer-file-name))))
	 (article-file (or article-file
			   (expand-file-name
			    (read-file-name "Suggest tags in this article: " articles-dir default-filename t nil
					    (lambda (f) (string-suffix-p ".md" f)))))))
    ;; Collect candidate tags
    (let* ((tags-dir (file-name-concat repo-dir "tags"))
           ;; Exclude any tag files inside the “excluded” subdirectory.
           (tag-files (cl-remove-if (lambda (f)
                                      (string-match-p "/excluded/" f))
                                    (directory-files-recursively tags-dir "\\.md$")))
           (candidates
            (mapconcat
             (lambda (tag-file)
               (let ((title (tlon-yaml-get-key "title" tag-file))
                     (para  (tlon-yaml--first-paragraph tag-file)))
                 (format "%s\n%s" title para)))
             tag-files "\n\n"))
           ;; Build a mapping from downcased title -> canonical title
           (title-map (let ((table (make-hash-table :test #'equal)))
                        (dolist (tag-file tag-files)
                          (let ((title (tlon-yaml-get-key "title" tag-file)))
                            (puthash (downcase title) title table)))
                        table))
           (article-content (tlon-md-read-content article-file))
           (prompt (format tlon-yaml-suggest-tags-prompt article-content candidates)))
      (message "Requesting tag suggestions for %s …"
               (file-name-nondirectory article-file))
      (tlon-make-gptel-request prompt nil
                               (tlon-yaml-suggest-tags-callback article-file title-map)
                               tlon-yaml-suggest-tags-model t))))

(declare-function tlon-md-read-content "tlon-md")
(declare-function tlon-md-beginning-of-content "tlon-md")
(defun tlon-yaml--first-paragraph (file)
  "Return the first non-empty paragraph of FILE as a trimmed string.
Falls back to a simpler parser when `tlon-md-read-content' errors (for instance
when the Markdown file lacks a local-variables section)."
  (condition-case nil
      (tlon-md-read-content file)
    (error
     (with-temp-buffer
       ;; Insert raw file contents
       (insert-file-contents file)
       ;; Skip YAML front-matter if present
       (let ((beg (goto-char (tlon-md-beginning-of-content))))
	 (forward-paragraph)
	 (buffer-substring-no-properties beg (point)))))))

(defun tlon-yaml--parse-tags-from-response (response)
  "Convert RESPONSE into a clean list of tag titles.
RESPONSE may be a fenced JSON array, a raw JSON array, or a
comma-/newline-separated list."
  (let* ((s (string-trim (or response ""))))
    ;; Strip fenced code blocks: ```lang\n ... \n```
    (when (string-match "\\`\\s-*```[[:alnum:]-_]*[ \t]*\n" s)
      (let ((start (match-end 0))
            (end (or (string-match "\n```\\s-*\\'" s (match-end 0))
                     (string-match "\n```" s (match-end 0)))))
        (when end
          (setq s (substring s start end)))))
    ;; If there's an array inside the text, keep only [ ... ] span.
    (let ((o (string-match "\\[" s)))
      (when o
        (let ((last -1)
              (len (length s)))
          (dotimes (i len)
            (when (eq (aref s i) ?\]) (setq last i)))
          (when (>= last 0)
            (setq s (substring s o (1+ last)))))))
    (or
     ;; Try strict JSON first.
     (condition-case nil
         (let ((json-array-type 'list))
           (json-read-from-string s))
       (error nil))
     ;; Fallback: split and clean tokens.
     (let* ((raw (split-string s "[,\n]+" t))
            (clean (mapcar (lambda (tok)
                             (let* ((x (string-trim tok))
                                    ;; Drop bracket/fence artifacts.
                                    (x (replace-regexp-in-string "\\`\\[\\|\\]\\'\\|^```.*\\'" "" x))
                                    ;; Remove repeated leading/trailing quotes.
                                    (x (replace-regexp-in-string "\\`[\"']+\\|[\"']+\\'" "" x)))
                               (string-trim x)))
                           raw)))
       (cl-remove-if
        (lambda (x)
          (or (string-empty-p x)
              (member (downcase x) '("[" "]" "```" "```json"))))
        clean)))))

(declare-function tlon-ai-callback-fail "tlon-ai")
(defun tlon-yaml-suggest-tags-callback (article-file title-map)
  "Insert suggested tags into ARTICLE-FILE.
TITLE-MAP is a hash table mapping down-cased tag titles to their
canonical form taken from the tag metadata."
  (lambda (response info)
    (if (not response)
        (tlon-ai-callback-fail info)
      (let* ((tags (tlon-yaml--parse-tags-from-response response))
             ;; normalise casing to match canonical titles
             (tags (mapcar (lambda (tag)
                             (or (gethash (downcase tag) title-map) tag))
                           tags))
             (tags (delete-dups (cl-remove-if #'string-empty-p tags))))
        (if (null tags)
            (message "AI returned no parsable tags.")
          (with-current-buffer (find-file-noselect article-file)
            (tlon-yaml-insert-field "tags" tags)
	    (save-buffer))
          (message "Inserted %d tag%s in %s"
                   (length tags) (if (= (length tags) 1) "" "s")
                   (file-name-nondirectory article-file)))))))

;;;###autoload
(defun tlon-yaml-suggest-tags-in-dir (&optional n dir)
  "Suggest tags via AI for article files in DIR that lack `tags'.
When N is non-nil (prefix arg), only process the first N articles
missing Tags. When DIR is nil, use `default-directory'. Each file
is processed by calling `tlon-yaml-suggest-tags'."
  (interactive "P")
  (let* ((dir (file-name-as-directory (or dir default-directory))))
    (unless (file-directory-p dir)
      (user-error "Directory not found: %s" dir))
    (let* ((files (directory-files dir t "\\.md\\'"))
           (processed 0)
           (limit (and n (prefix-numeric-value n))))
      (dolist (file files)
        (condition-case nil
            (when (and (file-regular-p file)
                       (string= (tlon-yaml-get-type file) "article"))
              (unless (tlon-yaml-get-key "tags" file)
                (if limit
                    (when (< processed limit)
                      (tlon-yaml-suggest-tags file)
                      (setq processed (1+ processed)))
                  (tlon-yaml-suggest-tags file)
                  (setq processed (1+ processed)))))
          (error nil)))
      (message "Queued AI tag suggestions for %d article%s without tags in %s"
               processed (if (= processed 1) "" "s") dir))))

;;;;;; Guess English counterpart

;;;###autoload
(defun tlon-yaml-guess-english-counterpart (&optional file)
  "Guess the English counterpart filename for the tag/author in FILE.
When FILE is nil, use the file visited by the current buffer.

If `original_path' already exists:
- When called interactively, ask for confirmation before proceeding.
- When called programmatically, do nothing."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
         (type (file-name-nondirectory (directory-file-name (file-name-directory file))))
         (lang (tlon-get-language-in-file file))
	 (en-type (tlon-get-bare-dir-translation "en" lang type))
         (title (tlon-yaml-get-key "title" file)))
    (unless (member en-type '("tags" "authors"))
      (user-error "This command only works in tag or author files"))
    (unless title
      (user-error "YAML field `title' not found for %s" file))
    (let* ((existing (tlon-yaml-get-key "original_path" file))
           (proceed (if existing
                        (if (called-interactively-p 'interactive)
                            (y-or-n-p
                             (format "Field `original_path' already exists with value `%s'. Proceed? "
                                     existing))
                          nil)
                      t)))
      (if (not proceed)
          (message "Skipping %s (original_path already present)"
                   (file-name-nondirectory file))
        (let* ((en-root (tlon-repo-lookup :dir :name "uqbar-en"))
               (en-dir  (file-name-concat en-root en-type))
               (candidates (mapcar #'file-name-nondirectory
                                   (directory-files-recursively en-dir "\\.md\\'")))
               (prompt (tlon-yaml--make-guess-counterpart-prompt title candidates)))
          (message "Requesting English counterpart for “%s” …" title)
          (tlon-make-gptel-request
           prompt nil
           (tlon-yaml-guess-english-counterpart-callback candidates en-dir file)
           tlon-yaml-guess-counterpart-model t))))))

(defun tlon-yaml-guess-english-counterpart-callback (candidates en-dir file)
  "Return a closure to handle AI response using CANDIDATES, EN-DIR and FILE.
When a valid filename is returned, insert it into FILE as
`original_path' in the YAML metadata."
  (lambda (response info)
    (if (not response)
        (tlon-ai-callback-fail info)
      (let* ((ans (string-trim response))
             (ans (car (split-string ans "[\r\n]+" t)))
             (match (seq-find (lambda (f) (string= f ans)) candidates)))
        (if (not match)
            (message "Model returned an unknown filename: %s" ans)
          (let ((full (expand-file-name match en-dir)))
            (with-current-buffer (find-file-noselect file)
              (tlon-yaml-insert-field "original_path" match)
              (save-buffer))
            (message "Inserted original_path: %s%s"
                     full
                     (if (file-exists-p full) "" " [file not found]"))))))))

(defun tlon-yaml--make-guess-counterpart-prompt (title candidates)
  "Build the prompt from TITLE and CANDIDATES."
  (format tlon-yaml-guess-counterpart-prompt
          title
          (mapconcat #'identity candidates "\n")))

;;;###autoload
(defun tlon-yaml-guess-english-counterpart-in-dir (&optional n dir)
  "Guess English counterparts for all tag/author files in DIR, non-recursively.

When N is non-nil, only process the first N files that do not
already contain an `original_path' field.  When DIR is nil, use
`default-directory'.

Each file is processed by calling `tlon-yaml-guess-english-counterpart'
programmatically (i.e., without interactive prompts)."
  (interactive "P")
  (let* ((dir (file-name-as-directory (or dir default-directory))))
    (unless (file-directory-p dir)
      (user-error "Directory not found: %s" dir))
    (let* ((files (directory-files dir t "\\.md\\'"))
           (missing-count 0)
           (limit (and n (prefix-numeric-value n))))
      (dolist (file files)
        (let ((has (tlon-yaml-get-key "original_path" file)))
          (cond
           (limit
            (unless has
              (when (< missing-count limit)
                (setq missing-count (1+ missing-count))
                (tlon-yaml-guess-english-counterpart file))))
           (t
            ;; No limit: call for all files (files with original_path will be skipped)
            (tlon-yaml-guess-english-counterpart file)))))
      (when limit
        (message "Queued AI requests for %d file%s without original_path in %s"
                 missing-count (if (= missing-count 1) "" "s") dir)))))

;;;;;; Translated tags

(defun tlon--yaml--tags-dir (repo-dir language)
  "Return absolute path of the tags directory inside REPO-DIR for LANGUAGE.
It uses `tlon-get-bare-dir-translation' to translate the canonical
\"tags\" directory name from English into LANGUAGE."
  (let* ((bare-dir (tlon-get-bare-dir-translation language "en" "tags")))
    (file-name-concat repo-dir bare-dir)))

(defun tlon--yaml--find-tag-file-by-title (title repo-dir)
  "Return absolute path of the tag file in REPO-DIR whose YAML `title' is TITLE.
Searches under REPO-DIR/tags recursively.  Returns nil when not found."
  (let* ((language (tlon-repo-lookup :language :dir repo-dir))
         (tags-dir (tlon--yaml--tags-dir repo-dir language)))
    (seq-find (lambda (file)
                (string= title (tlon-yaml-get-key "title" file)))
              (directory-files-recursively tags-dir "\\.md\\'"))))

(declare-function tlon-get-counterpart "tlon-counterpart")
(declare-function tlon-get-counterpart-in-originals "tlon-counterpart")
;;;###autoload
(defun tlon-yaml-insert-translated-tags (&optional file)
  "Insert translated tag titles into FILE’s YAML metadata.
When called interactively prompt for FILE, defaulting to the Markdown
file visited by the current buffer.

The function looks at the English counterpart of FILE, reads its `tags`
field, maps every tag slug to its tag file in English, locates the tag
translation into the language of FILE, retrieves the translated tag’s
`title`, and finally writes the list of titles back to FILE as the value
of the `tags` field."
  (interactive)
  (let* ((file (or file
                   (when (and (buffer-file-name)
                              (string= (file-name-extension (buffer-file-name)) "md"))
                     (buffer-file-name))
                   (read-file-name "Markdown file: " nil nil t nil
                                   (lambda (f) (string-suffix-p ".md" f t)))))
         (target-lang (tlon-get-language-in-file file))
         (en-file (if (string= target-lang "en")
                      file
                    (tlon-get-counterpart file)))
         (tags (tlon-yaml-get-key "tags" en-file))
         (titles '())
         (missing '()))
    (when (stringp tags)
      (setq tags (tlon-yaml-format-value tags)))
    (unless tags
      (user-error "English counterpart has no `tags' field"))
    (dolist (slug tags)
      (let* ((en-repo (tlon-get-repo-from-file en-file))
             (tags-dir (file-name-concat en-repo "tags"))
             (orig-tag-file (file-name-concat tags-dir
                                              (format "%s.md" (simple-extras-slugify slug))))
             (got-title nil))
        (when (file-exists-p orig-tag-file)
          (when-let ((tr-tag-file (tlon-get-counterpart-in-originals
                                   orig-tag-file target-lang)))
            (when-let ((title (tlon-yaml-get-key "title" tr-tag-file)))
              (push title titles)
              (setq got-title t))))
        (unless got-title
          (push slug missing))))
    (setq titles (nreverse (delete-dups titles)))
    (unless titles
      (user-error "No translated tag titles found"))
    (with-current-buffer (find-file-noselect file)
      (tlon-yaml-insert-field "tags" titles))
    (when missing
      (message "No translation found for %d tag%s: %s"
               (length missing)
               (if (= (length missing) 1) "" "s")
               (mapconcat #'identity (nreverse (delete-dups missing)) ", ")))))

;;;###autoload
(defun tlon-yaml-insert-translated-tags-in-dir (&optional n dir)
  "Insert translated tags in article files in DIR that lack `tags'.
When N is non-nil (prefix arg), only process the first N articles
missing Tags. When DIR is nil, use `default-directory'. Each file
is processed by calling `tlon-yaml-insert-translated-tags'."
  (interactive "P")
  (let* ((dir (file-name-as-directory (or dir default-directory))))
    (unless (file-directory-p dir)
      (user-error "Directory not found: %s" dir))
    (let* ((files (directory-files dir t "\\.md\\'"))
           (processed 0)
           (limit (and n (prefix-numeric-value n))))
      (dolist (file files)
        (condition-case nil
            (when (and (file-regular-p file)
                       (string= (tlon-yaml-get-type file) "article"))
              (unless (tlon-yaml-get-key "tags" file)
                (if limit
                    (when (< processed limit)
                      (tlon-yaml-insert-translated-tags file)
                      (setq processed (1+ processed)))
                  (tlon-yaml-insert-translated-tags file)
                  (setq processed (1+ processed)))))
          (error nil)))
      (message "Inserted translated tags in %d article%s without tags in %s"
               processed (if (= processed 1) "" "s") dir))))

;;;###autoload
(defun tlon-yaml-list-files-missing-field-in-dir (&optional dir field)
  "List Markdown files in DIR missing YAML FIELD.

When DIR is nil, use `default-directory'.  Prompt for FIELD from the
union of valid keys found among file types present in DIR.  Search only
the top level of DIR (non-recursive).  Display the results in a
temporary buffer and return the list of file paths."
  (interactive)
  (let* ((dir (file-name-as-directory (or dir default-directory)))
         (files (directory-files dir t "\\.md\\'"))
         (keys (let ((table (make-hash-table :test #'equal)))
                 (dolist (f files)
                   (ignore-errors
                     (dolist (k (or (tlon-yaml-get-valid-keys f nil) '()))
                       (puthash k t table))))
                 (let (acc)
                   (maphash (lambda (k _v) (push k acc)) table)
                   (nreverse acc))))
         (field (or field
                    (if keys
                        (completing-read "Field: " keys nil nil nil nil (car keys))
                      (read-string "Field: "))))
         (missing
          (cl-loop for f in files
                   for val = (ignore-errors (tlon-yaml-get-key field f))
                   for miss = (or (null val)
                                  (and (stringp val)
                                       (let ((s (string-trim val)))
                                         (or (string= s "")
                                             (string= s "[]")))))
                   if miss collect f)))
    (let ((buf (get-buffer-create "*tlon-missing-field*")))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (format "Directory: %s\nField missing: %s\nCount: %d\n\n"
                        dir field (length missing)))
        (dolist (f missing)
          (insert f "\n"))
        (goto-char (point-min))
        (view-mode 1))
      (display-buffer buf))
    missing))

;;;###autoload
(defun tlon-yaml-delete-field-in-dir (&optional dir field)
  "Delete YAML FIELD from all Markdown files in DIR, non-recursively.
When DIR is nil, use `default-directory'. Prompt for FIELD from the union of
valid keys present among file types in DIR. Display a summary and return the
list of files modified."
  (interactive)
  (let* ((dir (file-name-as-directory (or dir default-directory)))
         (files (directory-files dir t "\\.md\\'"))
         (keys (let ((table (make-hash-table :test #'equal)))
                 (dolist (f files)
                   (ignore-errors
                     (dolist (k (or (tlon-yaml-get-valid-keys f nil) '()))
                       (puthash k t table))))
                 (let (acc)
                   (maphash (lambda (k _v) (push k acc)) table)
                   (nreverse acc))))
         (field (or field
                    (if keys
                        (completing-read "Field to delete: " keys nil t nil nil (car keys))
                      (read-string "Field to delete: "))))
         (deleted '()))
    (dolist (f files)
      (ignore-errors
        (let ((meta (tlon-yaml-get-metadata f)))
          (when (and meta (assoc field meta))
            (tlon-yaml-delete-field field f)
            (push f deleted)))))
    (setq deleted (nreverse deleted))
    (message "Deleted field `%s' from %d file%s in %s"
             field (length deleted) (if (= (length deleted) 1) "" "s") dir)
    deleted))

;;;;; menu

(transient-define-infix tlon-yaml-infix-suggest-tags-model ()
  "AI model to use for suggesting tags.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-yaml-suggest-tags-model)

(transient-define-infix tlon-yaml-infix-guess-counterpart-model ()
  "AI model to use for guessing English counterparts.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-yaml-guess-counterpart-model)

(autoload 'gptel--infix-provider "gptel-transient")
;;;###autoload (autoload 'tlon-yaml-menu "tlon-yaml" nil t)
(transient-define-prefix tlon-yaml-menu ()
  "Menu for `tlon-yaml'."
  :info-manual "(tlon) yaml"
  [["General"
    ("i" "insert or edit field" tlon-yaml-insert-field)
    ("m" "list files missing field in dir" tlon-yaml-list-files-missing-field-in-dir)
    ("s" "set publication status" tlon-yaml-set-publication-status)
    ""
    "Tags"
    ("t s" "suggest tags"   tlon-yaml-suggest-tags)
    ("t S" "suggest tags in dir" tlon-yaml-suggest-tags-in-dir)
    ("t t" "insert translated tags" tlon-yaml-insert-translated-tags)
    ("t T" "insert translated tags in dir" tlon-yaml-insert-translated-tags-in-dir)
    ("t g" "guess English counterpart" tlon-yaml-guess-english-counterpart)
    ("t G" "guess counterparts in dir" tlon-yaml-guess-english-counterpart-in-dir)
    ""
    "AI models"
    ("-t" "suggest tags" tlon-yaml-infix-suggest-tags-model)
    ("-g" "guess counterpart" tlon-yaml-infix-guess-counterpart-model)]])

;;;;; temp

(declare-function tlon-db-get-translation-key "tlon-db")
(defun tlon-yaml-add-translation-keys (lang)
  "Populate `key' metadata in all article translations inlang.
This walks every markdown file under the articles folder of the repo for LANG.
For each file it:

1. Reads the `original_path' field, which is the filename of the
   English original.
2. Opens this file.
3. Retrieves the value of its `key' field.
4. Uses `tlon-db-get-translation-key' to obtain the BibTeX key of
   the translation into LANG.
5. Writes that key back into the LANG file as a `key' field,
   unless the field already exists."
  (interactive (list (tlon-select-language 'code 'babel)))
  (let* ((en-dir (file-name-concat (tlon-repo-lookup :dir :name "uqbar-en")
				   "articles"))
	 (trans-dir (file-name-concat (tlon-repo-lookup :dir :subproject "uqbar" :language lang)
				      (tlon-lookup tlon-core-bare-dirs lang "en" "articles")))
	 (files  (directory-files-recursively trans-dir "\\.md$")))
    (dolist (trans-file files)
      (let* ((original-filename (tlon-yaml-get-key "original_path" trans-file))
             (en-file (and original-filename
                           (expand-file-name original-filename en-dir))))
	(cond
	 ((not original-filename)
          (tlon-message-debug "No `original_path' in %s" trans-file))
	 ((not (file-exists-p en-file))
          (tlon-message-debug "Original file %s not found" en-file))
	 (t
          (let* ((en-key (tlon-yaml-get-key "key" en-file))
		 (trans-key (and en-key
				 (tlon-db-get-translation-key en-key lang)))
		 (existing (tlon-yaml-get-key "key" trans-file)))
            (cond
             ((not en-key)
              (tlon-message-debug "`key' missing in original %s" en-file))
             ((not trans-key)
              (tlon-message-debug "Translation key not found for %s" en-key))
             (existing
              (tlon-message-debug "`key' already present in %s" trans-file))
             (t
              (with-current-buffer (find-file-noselect trans-file)
		(tlon-yaml-insert-field "key" trans-key))
              (tlon-message-debug "Inserted key %s in %s" trans-key trans-file))))))))))

;;;###autoload
(defun tlon-yaml-sync-article-tags-to-es (&optional dir)
  "Insert Spanish tag translations for all English articles in DIR.

The function iterates over every Markdown file under DIR (defaulting
to the canonical uqbar-en articles directory) and calls
`tlon-yaml-insert-translated-tags' on each with target language code
\"es\".

With a prefix argument, prompt for DIR."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name
            "English articles directory: "
            "/Users/pablostafforini/Library/CloudStorage/Dropbox/repos/uqbar-en/articles/"
            nil t))))
  (let* ((dir   (or dir "/Users/pablostafforini/Library/CloudStorage/Dropbox/repos/uqbar-en/articles/"))
         ;; walk the directory tree recursively
         (files (directory-files-recursively dir "\\.md\\'"))
         (processed 0))
    (dolist (file files)
      ;; only act when a Spanish counterpart exists
      (when-let ((es-file (tlon-get-counterpart file "es")))
        (when (file-exists-p es-file)
          (tlon-yaml-insert-translated-tags file)
          (setq processed (1+ processed)))))
    (message "Processed %d of %d article%s in %s"
             processed
             (length files)
             (if (= processed 1) "" "s")
             dir)))

;;;###autoload
(defun tlon-yaml-lowercase-it-tag-titles (&optional dir)
  "Lowercase only the first word of the YAML title field for al files in DIR.
When DIR is nil, default to
\"/Users/pablostafforini/Library/CloudStorage/Dropbox/repos/uqbar-it/soggetti\".
Operate recursively and only update files whose title changes. This is
a temporary clean-up helper."
  (interactive)
  (let* ((dir (file-name-as-directory
               (or dir "/Users/pablostafforini/Library/CloudStorage/Dropbox/repos/uqbar-it/soggetti")))
         (files (directory-files-recursively dir "\\.md\\'"))
         (updated 0)
         (skipped 0))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (if (not (looking-at-p tlon-yaml-delimiter))
              (setq skipped (1+ skipped))
            (forward-line)
            (let ((end (save-excursion (re-search-forward tlon-yaml-delimiter nil t))))
              (if (not end)
                  (setq skipped (1+ skipped))
                (when (re-search-forward "^title:\\s-*\\(.*\\)$" end t)
                  (let* ((raw (match-string 1))
                         (trim (string-trim raw))
                         (q (and (>= (length trim) 2)
                                 (eq (aref trim 0) ?\")
                                 (eq (aref trim (1- (length trim))) ?\")))
                         (core (if q (substring trim 1 -1) trim))
                         (pos (save-match-data (string-match "[ \t]" core)))
                         (first (if pos (substring core 0 pos) core))
                         (rest (if pos (substring core pos) ""))
                         (new-core (concat (downcase first) rest)))
                    (unless (string= core new-core)
                      (replace-match (if q (concat "\"" new-core "\"") new-core) t t nil 1)
                      (save-buffer)
                      (setq updated (1+ updated))))))))))
      (message "Lowercased first word of titles in %d file%s under %s"
               updated (if (= updated 1) "" "s") dir))))

;;;###autoload
(defun tlon-yaml-uncapitalize-fr-article-tags (&optional dir)
  "Lowercase the first word of each tag in the tags field for all files in DIR.
When DIR is nil, default to
\"/Users/pablostafforini/Library/CloudStorage/Dropbox/repos/uqbar-fr/articles\".
Operate recursively and only update files whose tags change. This is a
temporary clean-up helper."
  (interactive)
  (let* ((dir (file-name-as-directory
               (or dir "/Users/pablostafforini/Library/CloudStorage/Dropbox/repos/uqbar-fr/articles")))
         (files (directory-files-recursively dir "\\.md\\'"))
         (updated 0)
         (skipped 0))
    (dolist (file files)
      (let ((tags (tlon-yaml-get-key "tags" file)))
        (when (stringp tags)
          (setq tags (tlon-yaml-format-value tags)))
        (if (not (listp tags))
            (setq skipped (1+ skipped))
          (let* ((new-tags
                  (mapcar (lambda (s)
                            (let* ((s (or s ""))
                                   (s (string-trim s))
                                   (pos (string-match "[ \t]" s))
                                   (first (if pos (substring s 0 pos) s))
                                   (rest (if pos (substring s pos) "")))
                              (concat (downcase first) rest)))
                          tags)))
            (unless (equal tags new-tags)
              (with-current-buffer (find-file-noselect file)
                (tlon-yaml-insert-field "tags" new-tags)
                (save-buffer))
              (setq updated (1+ updated)))))))
    (message "Lowercased first word of tags in %d file%s under %s"
             updated (if (= updated 1) "" "s") dir)))

(provide 'tlon-yaml)
;;; tlon-yaml.el ends here
