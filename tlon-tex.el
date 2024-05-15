;;; tlon-tex.el --- BibTeX related functionality -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon
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

;; BibTeX related functionality.

;;; Code:

(require 'bib)
(require 'bibtex-extras)
(require 'doi-utils)
(require 'paths)
(require 'shut-up)

;;;; User options

(defgroup tlon-tex ()
  "BibTeX related functionality."
  :group 'tlon)

(defcustom tlon-abstract-overwrite 'prompt
  "Whether to overwrite the abstract if already present."
  :type '(choice
	  (const :tag "Always overwrite" always)
	  (const :tag "Never overwrite" never)
	  (const :tag "Prompt" prompt)))

;;;; Variables

(defconst tlon-locators
  '(("book" . "bk.")
    ("chapter ". "chap.")
    ("column" . "col.")
    ("figure" . "fig.")
    ("folio" . "fol.")
    ("number" . "no.")
    ("line" . "l.")
    ("note" . "n.")
    ("opus" . "op.")
    ("page" . "p.")
    ("paragraph" . "para.")
    ("part" . "pt.")
    ("section" . "sec.")
    ("sub verbo" . "s.v")
    ("verse" . "v.")
    ("volumes" . "vol.")
    ("books" . "bks.")
    ("chapter ". "chaps.")
    ("columns" . "cols.")
    ("figures" . "figs.")
    ("folios" . "fols.")
    ("numbers" . "nos.")
    ("lines" . "ll.")
    ("notes" . "nn.")
    ("opera" . "opp.")
    ("pages" . "pp.")
    ("paragraphs" . "paras.")
    ("parts" . "pts.")
    ("sections" . "secs.")
    ("sub  verbis" . "s.vv.")
    ("verses" . "vv.")
    ("volumes" . "vols."))
  "Alist of locators and their abbreviations.")

(defvar tlon-refs-dir
  paths-dir-babel-refs
  "Directory of the `babel-refs' repo.")

(defvar tlon-bibtex-dir
  (file-name-concat tlon-refs-dir "bib/")
  "Directory where BibTeX files are stored.")

(defvar tlon-file-fluid
  (file-name-concat tlon-bibtex-dir "fluid.bib")
  "File containing the fluid bibliography.")

(defvar tlon-file-stable
  (file-name-concat tlon-bibtex-dir "stable.bib")
  "File containing the stable bibliography.")

(defvar tlon-bibliography-files
  `(,tlon-file-fluid
    ,tlon-file-stable)
  "List of bibliography files.")

;;;; Functions

;;;;; Fetch fields

(declare-function ebib-extras-get-field "ebib-extras")
(declare-function tlon-ai-batch-continue "tlon-ai")
(defun tlon-fetch-and-set-abstract ()
  "Fetch the abstract of the entry at point and set it as the new value.
We use CrossRef for DOIs, Google Books for ISBN and Zotero for URLs.

When the entry already contains an abstract, prompt the user for confirmation.
Bypass this prompt if OVERWRITE is either `always' or `never'; if so, the new
abstract will, or will not, replace the existing one, respectively."
  (interactive)
  (cl-destructuring-bind (get-field set-field)
      (pcase major-mode
	('ebib-entry-mode '(ebib-extras-get-field ebib-extras-set-field))
	('bibtex-mode '(bibtex-extras-get-field bibtex-set-field))
	(_ (error "Not in `ebib-entry-mode' or `bibtex-mode'")))
    (when (tlon-abstract-may-proceed-p)
      (cl-destructuring-bind (doi isbn url)
	  (mapcar (lambda
		    (field)
		    (funcall get-field field))
		  '("doi" "isbn" "url"))
	(let ((key (pcase major-mode
		     ('ebib-entry-mode (ebib-extras-get-field "=key="))
		     ('bibtex-mode (bibtex-extras-get-key))))
	      found)
	  (if-let ((value (or
			   (tlon-fetch-abstract-from-crossref doi)
			   (tlon-fetch-abstract-from-google-books isbn)
			   (tlon-fetch-abstract-with-zotra url url))))
	      (progn
		(shut-up
		  (funcall set-field "abstract" (tlon-abstract-cleanup value)))
		(message "Set abstract of `%s'." key)
		(setq found t))
	    (message "Could not find abstract for `%s' using non-AI methods." key)
	    (setq found nil))
	  (tlon-ai-batch-continue)
	  found)))))

(defvar tlon-ai-batch-fun "tlon-ai")
(declare-function zotra-extras-fetch-field "zotra-extras")
(defun tlon-fetch-abstract-with-zotra (url doi)
  "Return the abstract of the work with URL or DOI."
  (when-let ((id (or url doi)))
    (message "Trying to find abstract for %s with zotra..." id)
    (let* ((doi (when doi (tlon-fetch-url-from-doi doi))))
      (if-let ((abstract
		(catch 'found
		  (dolist (field (list url (unless (string= url doi) doi)))
		    (when (and field
			       (not (string-match-p "\\.pdf$" field)))
		      (when-let ((abstract
				  (shut-up (zotra-extras-fetch-field
					    "abstract" field (when tlon-ai-batch-fun 'no-error)))))
			(throw 'found abstract)))))))
	  abstract
	(progn (message "No abstract found.") nil)))))

;; TODO: submit as pull request to `doi-utils'?
;; `doi-utils-get-redirect' doesn't work
;; note that my function doesn't always return the final target of the redirect
;; because they sometimes use JavaScript; see id:1ED71E19-1CE4-4221-8880-AFFD799E34F0
(defun tlon-fetch-url-from-doi (doi)
  "Fetch the URL from a DOI."
  (with-temp-buffer
    (call-process "curl" nil t nil
		  "-ILs" (concat doi-utils-dx-doi-org-url doi))
    (goto-char (point-max))
    (when-let ((final-url
		;; with multiple redirects, we want to get the final URL
		(when (search-backward-regexp "Location: \\(.*\\)" nil t)
		  (match-string 1))))
      (substring final-url 0 -1))))

;; TODO: refactor two functions below
(defun tlon-fetch-abstract-from-crossref (doi)
  "Return the abstract of the work with DOI."
  (when doi
    (let ((url (format "https://api.crossref.org/works/%s" doi)))
      (message "Trying to find abstract for %s with Crossref..." doi)
      (with-current-buffer (shut-up (url-retrieve-synchronously url))
	(goto-char (point-min))
	(if (search-forward-regexp "HTTP/.* 404" nil t) ; check for 404 not found
	    (progn
	      (kill-buffer)
	      nil)
	  (re-search-forward "^$")
	  (delete-region (point) (point-min))
	  (let* ((json-object-type 'plist)
		 (json-array-type 'list)
		 (json (json-read))
		 (message-plist (plist-get json :message)))
	    (kill-buffer)
	    (if-let ((abstract (plist-get message-plist :abstract)))
		abstract
	      (progn (message "No abstract found.") nil))))))))

(defun tlon-fetch-abstract-from-google-books (isbn)
  "Return the abstract of the book with ISBN."
  (when isbn
    (let ((url (format "https://www.googleapis.com/books/v1/volumes?q=isbn:%s" isbn))
	  (description nil))
      (message "Trying to find abstract for %s with Google Books..." isbn)
      (with-current-buffer (url-retrieve-synchronously url)
        (set-buffer-multibyte t) ;; Ensure buffer is treated as multibyte
        (set-buffer-file-coding-system 'utf-8) ;; Set coding system to UTF-8
        (goto-char (point-min))
        (re-search-forward "^$")
        (delete-region (point) (point-min))
        (let* ((json-object-type 'plist)
               (json-array-type 'list)
               (json (json-read))
               (items (plist-get json :items))
               (volume-info (and items (plist-get (car items) :volumeInfo))))
          (setq description (and volume-info (plist-get volume-info :description)))))
      (when (get-buffer url)
	(kill-buffer url))
      (if description description (progn (message "No abstract found.") nil)))))

(defun tlon-abstract-may-proceed-p ()
  "Return t iff it’s okay to proceed with abstract processing."
  (if (derived-mode-p 'bibtex-mode 'ebib-entry-mode)
      (let* ((get-field (pcase major-mode
			  ('ebib-entry-mode #'ebib-extras-get-field)
			  ('bibtex-mode #'bibtex-extras-get-field)))
	     (abstract (funcall get-field  "abstract")))
	(if (or
	     (eq tlon-abstract-overwrite 'always)
	     (not abstract)
	     (unless (eq tlon-abstract-overwrite 'never)
	       (y-or-n-p "Abstract already exists. Overwrite?")))
	    t
	  (message "Skipping: `%s' already contains an abstract."
		   (pcase major-mode
		     ('bibtex-mode (bibtex-extras-get-key))
		     ('ebib-entry-mode (ebib-extras-get-field "=key="))))
	  nil))
    (derived-mode-p 'text-mode 'pdf-view-mode)))


(declare-function ebib-extras-get-or-fetch-id-or-url "ebib-extras")
(defun tlon-fetch-field-with-zotra (field &optional id-or-url no-error)
  "Fetch the value of FIELD from the ID-OR-URL of the entry at point.
IF ID-OR-URL is nil, try to get it or fetch it. If NO-ERROR is non-nil, handle
errors gracefully."
  (let* ((id-or-url (or id-or-url (ebib-extras-get-or-fetch-id-or-url))))
    (zotra-extras-fetch-field field id-or-url no-error)))

(defun tlon-abstract-cleanup (string)
  "Clean up raw abstract consisting of STRING."
  ;; remove a bunch of stuff
  (dolist (regexp '("<[^>]+>" ; XML tags
		    "{\\\\textless}.?p{\\\\textgreater}" ; LaTeX tag
		    "^summary\\|^abstract\\(:? ?\\)" ; extraneous leading words
		    )
		  string)
    (setq string (replace-regexp-in-string regexp "" string)))
  ;; add a period at the end of the abstract if missing
  (replace-regexp-in-string "\\([^\\.]\\)$" "\\1." string))

;;;;; Move entries

(defun tlon-move-entry (&optional key file)
  "Move entry with KEY to FILE.
Save citekey to \"kill-ring\". If KEY is nil, use the key of the entry at point.
If FILE is non-nil, use `tlon-file-fluid'."
  (interactive)
  (let ((key (or key (bibtex-extras-get-key)))
	(file (or file tlon-file-fluid)))
    (bibtex-extras-move-entry key file)
    (with-current-buffer (find-file-noselect file)
      (widen)
      (bibtex-search-entry key)
      (tlon-add-or-update-tlon-field)
      (save-buffer))
    (kill-new key)))

(defun tlon-move-entry-without-abstract ()
  "Move entry to `tlon-file-fluid' if it doesn't have an abstract."
  (message "Moving `%s'..." (bibtex-extras-get-key))
  (unless (bibtex-extras-get-field "abstract")
    (tlon-move-entry))
  (bibtex-next-entry)
  (tlon-move-entry-without-abstract))

(defun tlon-move-entry-without-file ()
  "Move entry to `tlon-file-fluid' if it doesn't have an abstract."
  (let ((key (bibtex-extras-get-key)))
    (unless (or (bibtex-extras-get-field "file")
		(bibtex-extras-get-field "crossref"))
      (message "Moving `%s'..." key)
      (tlon-move-entry key "temp.bib"))
    (bibtex-next-entry)
    (tlon-move-entry-without-file)))

(defun tlon-move-lesswrong-entries ()
  "Move LessWrong entry to \"temp.bib\"."
  (message "Moving `%s'..." (bibtex-extras-get-key))
  (when (string= (bibtex-extras-get-field "journaltitle") "{LessWrong}")
    (tlon-move-entry nil (file-name-concat tlon-bibtex-dir "temp.bib")))
  (bibtex-next-entry)
  (tlon-move-lesswrong-entries))

;;;;; Add fields

(defun tlon-add-or-update-tlon-field ()
  "Add or update \"database\" field with \"Tlön\" value in the current BibTeX entry."
  (bibtex-extras-add-or-update-field "database" "Tlön"))

(defun tlon-add-database-field (file)
  "Iterate over each entry in FILE and add/update the `database' field.
Adds the field `database' to every entry if it doesn't have it
and sets the value of the field for all entries to `Tlön'."
  (interactive "fBibTeX file: ")
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (bibtex-map-entries
       (lambda (_key start _end)
	 (save-excursion
	   (goto-char start)
	   (tlon-add-or-update-tlon-field)))))
    ;; Save the updated entries to the file
    (save-buffer)))

(defun tlon-auto-add-database-field ()
  "Run `bibtex-extras-add-database-field' every time `new.bib' is saved."
  (let ((file tlon-file-fluid))
    (when (string= (buffer-file-name) file)
      (tlon-add-database-field file))))

;;;;; Cleanup

(defun tlon-auto-clean-entry ()
  "Clean up bibtex entry at point upon saving."
  (let ((after-save-hook nil))
    (tlon-add-lang-id-to-entry)
    (tlon-remove-empty-spaces)
    (bibtex-extras-escape-special-characters)
    (bibtex-clean-entry)
    (save-buffer)))

;; TODO: support arbitrary langs
(defun tlon-add-lang-id-to-entry (&optional _ _ _)
  "Add `langid' field to entry at point, if appropriate.
If the field `landig' is present, the function does nothing; else, it sets the
`langid' field to `spanish' if the entry has either a `translation' or a
`translator' field, and to `english' otherwise."
  (unless (bibtex-text-in-field "langid")
    (if (or (bibtex-text-in-field "translation")
	    (bibtex-text-in-field "translator"))
	(bibtex-set-field "langid" "spanish")
      (bibtex-set-field "langid" "english"))))

(defun tlon-remove-empty-spaces ()
  "Remove empty spaces at the end of field."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward " \\}" nil t)
      (replace-match "}" t t))))

(defun bibtex-extras-escape-special-characters ()
  "Escape special characters in the current BibTeX file."
  (interactive)
  (save-excursion
    (dolist (char '("$" "%" "#" "&"))
      (goto-char (point-min))
      (while (re-search-forward (format "\\(\\(?:[^\\]\\|^\\)\\)\\(\\%s\\)" char) nil t)
	(unless (member (bibtex-extras-get-field-name) '("url" "file"))
	  (replace-match (format "\\1\\\\%s" char) nil nil))))))


;;;;; Autokey

(defun tlon-generate-autokey (author year title)
  "Generate a BibTeX key based on AUTHOR, YEAR, and TITLE."
  ;; TODO: check that they key doesn't already exist in all metadata
  (let* ((author (tlon-autokey-get-names author))
	 (year (tlon-autokey-get-year year))
	 (title (tlon-autokey-get-title title))
	 (autokey (concat bibtex-autokey-prefix-string
			  author
			  (unless (or (equal author "")
				      (equal year ""))
			    bibtex-autokey-name-year-separator)
			  year
			  (unless (or (and (equal author "")
					   (equal year ""))
				      (equal title ""))
			    bibtex-autokey-year-title-separator)
			  title)))
    (if bibtex-autokey-before-presentation-function
	(funcall bibtex-autokey-before-presentation-function autokey)
      autokey)))

(defun tlon-autokey-get-names (name)
  "Return formatted contents of NAME field."
  (if (string= "" name)
      name
    (let* ((case-fold-search t)
	   (name-list (mapcar #'bibtex-autokey-demangle-name
			      (split-string name "[ \t\n]+and[ \t\n]+")))
	   additional-name)
      (unless (or (not (numberp bibtex-autokey-names))
		  (<= (length name-list)
		      (+ bibtex-autokey-names
			 bibtex-autokey-names-stretch)))
	(setq name-list (nreverse (nthcdr (- (length name-list)
					     bibtex-autokey-names)
					  (nreverse name-list)))
	      additional-name bibtex-autokey-additional-names))
      (concat (mapconcat #'identity name-list
			 bibtex-autokey-name-separator)
	      additional-name))))

(defun tlon-autokey-get-year (year)
  "Get formatted contents of YEAR field."
  (substring year (max 0 (- (length year) bibtex-autokey-year-length))))

(defun tlon-autokey-get-title (title)
  "Get formatted contents of TITLE field."
  (let ((case-fold-search t))
    (if (string-match bibtex-autokey-title-terminators title)
	(setq title (substring title 0 (match-beginning 0))))
    (let ((counter 0)
	  (ignore-re (concat "\\`\\(?:"
			     (mapconcat #'identity
					bibtex-autokey-titleword-ignore "\\|")
			     "\\)\\'"))
	  titlewords titlewords-extra word)
      (while (and (or (not (numberp bibtex-autokey-titlewords))
		      (< counter (+ bibtex-autokey-titlewords
				    bibtex-autokey-titlewords-stretch)))
		  (string-match "\\b\\w+" title))
	(setq word (match-string 0 title)
	      title (substring title (match-end 0)))
	;; `bibtex-autokey-titleword-ignore'.
	(unless (let (case-fold-search)
		  (string-match ignore-re word))
	  (setq counter (1+ counter))
	  (if (or (not (numberp bibtex-autokey-titlewords))
		  (<= counter bibtex-autokey-titlewords))
	      (push word titlewords)
	    (push word titlewords-extra))))
      (unless (string-match "\\b\\w+" title)
	(setq titlewords (append titlewords-extra titlewords)))
      (mapconcat #'bibtex-autokey-demangle-title (nreverse titlewords)
		 bibtex-autokey-titleword-separator))))

;;;;; Add missing URLs

(defvar zotra-extras-add-multiple-urls-filename)
(declare-function files-extras-list-to-lines "files-extras")
;;;###autoload
(defun tlon-prompt-to-add-missing-urls ()
  "Prompt to add missing URLs in the current buffer to the Tlön bibliography."
  (interactive)
  (save-excursion
    (let ((urls (tlon-get-missing-urls))
	  urls-to-add)
      (dolist (url urls urls-to-add)
	(goto-char (point-min))
	(re-search-forward url nil t)
	(when (y-or-n-p (format "Add `%s' to bibliography?" url))
	  (push url urls-to-add)))
      (if (null urls-to-add)
	  (message "No URLs to add.")
	(files-extras-list-to-lines urls-to-add zotra-extras-add-multiple-urls-filename)))))

;; TODO: maybe generalize to other fields, e.g. isbn, doi
(declare-function tlon-get-md-links-in-file "tlon-babl-md")
(declare-function simple-extras-simplify-url "simple-extras")
(defun tlon-get-missing-urls (&optional file)
  "Return all URLs present in FILE but missing in the Tlön bibliography.
If FILE is nil, use the file visited by the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (urls-in-biblio (tlon-get-field-in-bibliography "url"))
	 (urls-in-file (tlon-get-md-links-in-file file))
	 (urls-in-biblio-simple (mapcar #'simple-extras-simplify-url urls-in-biblio))
	 (urls-in-file-simple (mapcar #'simple-extras-simplify-url urls-in-file))
	 (missing-urls-simple (cl-set-difference urls-in-file-simple urls-in-biblio-simple :test #'string=)))
    (mapcar (lambda (url)
	      (concat "https://" url))
	    missing-urls-simple)))

;;;;; Translation

(declare-function ebib-extras-set-field "ebib-extras")
(defun tlon-tex-create-translation-entry ()
  "Create a translation entry from the entry at point."
  (interactive)
  (unless (derived-mode-p 'ebib-entry-mode)
    (user-error "Not in `ebib-entry-mode'"))
  (let* ((language (tlon-select-language 'code 'babel))
	 (fields `(("translation" . ,(ebib-extras-get-field "=key="))
		   ("=type=" . ,(ebib-extras-get-field "=type="))
		   ("author" . ,(ebib-extras-get-field "author"))
		   ("database" . ,(ebib-extras-get-field "database"))
		   ("langid" . ,(tlon-lookup tlon-languages-properties :name :code language))
		   ("title" . ,(read-string "Title: "))
		   ("translator" . ,(when-let ((field "translator"))
				      (ebib--edit-list-field field (list field) (ebib-unbrace nil))))
		   ("date" .  ,(format-time-string "%Y")))))
    (ebib-add-entry)
    (sleep-for 0.1)
    (dolist (field fields)
      (cl-destructuring-bind (key . value) field
	(ebib-extras-set-field key value)))
    (ebib-generate-autokey)))

;;;;; Misc

(defvar citar-bibliography)
;; TODO: Maybe I should use `query-replace-regexp', to deal with the `save-match-data' issue?
(defun tlon-replace-titles (&optional file)
  "Replace titles in FILE with proper citations."
  (interactive)
  (let ((file (or file (buffer-file-name)))
	(titles (tlon-get-field-in-bibliography "title"))
	(citar-bibliography tlon-bibliography-files)
	(open-delim "[\"'“‘\\[\\*")
	(close-delim "[\"'”’\\]\\*]")
	always-long always-short)
    (find-file file)
    (save-excursion
      (dolist (title titles)
	(catch 'skip
	  (let (replacement)
	    (goto-char (point-min))
	    (when (re-search-forward (concat open-delim title close-delim) nil t)
	      (save-match-data
		(let ((choice (cond (always-long ?l)
				    (always-short ?s)
				    ((read-key "long (l) | short (s) | skip (n) | all long (L) | all short (S)")))))
		  (if (eq choice ?n)
		      (throw 'skip (message (format "Skipping `%s'." title)))
		    (when-let* ((key (tlon-bibliography-lookup "=key=" "title" title))
				;; TODO: replace with constants
				(cite-long (format "<Cite bibKey={\"%s\"} />" key))
				(cite-short (format "<Cite bibKey={\"%s\"} short />" key)))
		      (setq replacement (pcase choice
					  ((or ?l ?L)
					   (when (eq choice ?L)
					     (setq always-long t))
					   cite-long)
					  ((or ?s ?S)
					   (when (eq choice ?S)
					     (setq always-short t))
					   cite-short)))))))
	      (replace-match replacement t t))))))))

(defvar citar-cache--bibliographies)
(defun tlon-bibliography-lookup (assoc-field field value)
  "Return the value for ASSOC-FIELD in the entry where FIELD matches VALUE."
  (catch 'found
    (maphash (lambda (_key bibliography)
               (let ((entries (citar-cache--bibliography-entries bibliography)))
                 (maphash (lambda (_ entry)
                            (when (string= (cdr (assoc field entry)) value)
                              (throw 'found (cdr (assoc assoc-field entry)))))
                          entries)))
             citar-cache--bibliographies)
    nil))

(defvar citar-cache--bibliographies)
(declare-function citar-cache--bibliography-entries "citar-cache")
(defun tlon-get-field-in-bibliography (field)
  "Return all FIELD values in BibTeX entries in the Tlön bibliography."
  (let (fields)
    (maphash (lambda (_key bibliography)
	       (when-let ((entries (citar-cache--bibliography-entries bibliography)))
		 (maphash (lambda (_ entry)
			    (when-let ((value (cdr (assoc field entry))))
			      (push value fields)))
			  entries)))
	     citar-cache--bibliographies)
    fields))

(provide 'tlon-tex)
;;; tlon-tex.el ends here
