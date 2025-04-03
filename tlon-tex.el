;;; tlon-tex.el --- BibTeX related functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon

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

(require 'bibtex-extras)
(require 'paths)
(require 'shut-up)
(require 'tlon-core)
(require 'transient)

;;;; User options

(defgroup tlon-tex ()
  "BibTeX related functionality."
  :group 'tlon)

(defcustom tlon-abstract-overwrite 'ask
  "Whether to overwrite the abstract if already present."
  :type '(choice
	  (const :tag "Always overwrite" always)
	  (const :tag "Never overwrite" never)
	  (const :tag "Ask" ask)))

;;;; Variables

;;;;; Files

(defconst tlon-file-abstract-translations
  (file-name-concat (tlon-repo-lookup :dir :name "babel-refs")
		    "bib" "abstract-translations.json")
  "The JSON file containing the abstract translations.")

(defconst tlon-tex-excluded-keys-file
  (file-name-concat tlon-package-dir "tlon-excluded-keys.el")
  "File where the excluded keys are persisted.")

;;;;; Locators

(defconst tlon-locators
  '(("book" . "bk.")
    ("chapter ". "chap.")
    ("column" . "col.")
    ("figure" . "fig.")
    ("folio" . "fol.")
    ("line" . "l.")
    ("note" . "n.")
    ("number" . "no.")
    ("opus" . "op.")
    ("page" . "p.")
    ("paragraph" . "para.")
    ("part" . "pt.")
    ("section" . "sec.")
    ("sub verbo" . "s.v.")
    ("verse" . "v.")
    ("volume" . "vol.")
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
    ("sub verbis" . "s.vv.")
    ("verses" . "vv.")
    ("volumes" . "vols."))
  "Alist of locators and their abbreviations.")

;;;;; Citation styles

(defconst tlon-tex-pandoc-cite-pattern-long
  "\\[-?@\\(?:{\\(?1:.*?\\)}\\|\\(?1:[[:alnum:]_][[:alnum:]]*\\(?:[:.#$%&+?<>~/-][[:alnum:]]+\\)*\\)\\(?:, \\(?2:.*?\\)\\)?\\)\\]"
  "Regular expression for a \"long\" Pandoc citation key.

Group 1 captures the key. Group 2 captures the locator(s), if present. Based on
`citar-markdown-citation-key-regexp'.")

(defconst tlon-tex-pandoc-cite-pattern-short
  (concat "<cite>" tlon-tex-pandoc-cite-pattern-long "</cite>")
  "Regular expression for a \"short\" Pandoc citation key.

Group 1 captures the key. Group 2 captures the locator(s), if present. Based on
`citar-markdown-citation-key-regexp'.")

(defconst tlon-tex-pandoc-cite-pattern
  (concat "\\(?:<cite>\\)?" tlon-tex-pandoc-cite-pattern-long "\\(?:</cite>\\)?")
  "Regular expression for a Pandoc citation key.

Group 1 captures the key. Group 2 captures the locator(s), if present. Based on
`citar-markdown-citation-key-regexp'.")

;;;;; Regexp patterns

(defconst tlon-regexp-locator-in-citation
  (format "\\(?2:, \\(?:%s\\).*?\\)"
	  (mapconcat (lambda (locator)
		       (regexp-quote (cdr locator)))
		     tlon-locators "\\|"))
  "Regexp to match one or more locators in a citation.")

(defconst tlon-regexp-expanded-citation-formatter-with-locators
  (format "^[[:alnum:]]*?, [[:alnum:]& ]*? ([[:digit:]]\\{4\\}) %%s\\(?:%s\\|\\(, .*?\\)\\)*$"
	  tlon-regexp-locator-in-citation)
  "Formatter for a regexp pattern to match expanded citations, handling locators.
NOTE: This is not working correctly.")

(defconst tlon-regexp-expanded-citation-formatter
  "^[[:alnum:]]*?, [[:alnum:]& ]*? ([[:digit:]]\\{4\\}) %%s.*$"
  "Formatter for a regexp pattern to match expanded citations, handling locators.
NOTE: This is not working correctly.")

(defvar tlon-md-regexp-link-formatter "tlon-md")
(defconst tlon-regexp-expanded-citation-with-link
  (format tlon-regexp-expanded-citation-formatter
	  (format tlon-md-regexp-link-formatter "" "" "1" "" "" "" "" ""))
  "Regexp to match a citation whose title has a link in our \"long\" style.
The capture group 3 contains the title of the work.")

(defconst tlon-regexp-expanded-citation-with-no-link
  (format tlon-regexp-expanded-citation-formatter "[\"“'‘\\*]?\\(?1:.*?\\)[\"”'’\\*]")
  "Regexp to match a citation whose title has no link in our \"long\" style.
The capture group 3 contains the title of the work.")

;;;;; Abstracts

(defconst tlon-tex-max-abstract-length 300
  "Maximum length of an abstract, in words.")

(defconst tlon-tex-min-abstract-length 45
  "Minimum length of an abstract, in words.")

(defconst tlon-tex-synopsis-length 2000
  "Maximum length of a synopsis, in words.")

;;;;;; Reports

(defvar tlon-tex-excluded-keys nil
  "List of keys of entries that do not need an abstract.
Keys in this list are excluded from the list of entries missing an abstract
generated by `tlon-tex-entries-report'.")

;;;; Functions

;;;;; Fetch fields

(declare-function ebib-extras-get-field "ebib-extras")
(declare-function tlon-ai-batch-continue "tlon-ai")
;;;###autoload
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
		     ('ebib-entry-mode (ebib--get-key-at-point))
		     ('bibtex-mode (bibtex-extras-get-key))))
	      found)
	  (if-let ((value (or
			   (tlon-fetch-abstract-from-crossref doi)
			   (tlon-fetch-abstract-from-google-books isbn)
			   (tlon-fetch-abstract-with-zotra url url))))
	      (progn
		(funcall set-field "abstract" (tlon-abstract-cleanup value))
		(message "Set abstract of `%s'." key)
		(setq found t))
	    (message "Could not find abstract for `%s' using non-AI methods." key)
	    (setq found nil))
	  (tlon-ai-batch-continue)
	  found)))))

(defvar tlon-ai-batch-fun)
(autoload 'zotra-extras-fetch-field "zotra-extras")
(defun tlon-fetch-abstract-with-zotra (url doi)
  "Return the abstract of the work with URL or DOI.
Give up after five seconds."
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
					    "abstract" field (when tlon-ai-batch-fun 'no-error) 5))))
			(throw 'found abstract)))))))
	  abstract
	(progn (message "No abstract found.") nil)))))

;; TODO: submit as pull request to `doi-utils'?
;; `doi-utils-get-redirect' doesn't work
;; note that my function doesn't always return the final target of the redirect
;; because they sometimes use JavaScript; see id:1ED71E19-1CE4-4221-8880-AFFD799E34F0
(defvar doi-utils-dx-doi-org-url)
(defun tlon-fetch-url-from-doi (doi)
  "Fetch the URL from a DOI."
  (require 'doi-utils)
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
  "Return the abstract of the book with ISBN, timing out after 5 seconds."
  (when isbn
    (with-timeout (5 (message "Timeout while fetching abstract") nil)
      (let ((url (format "https://www.googleapis.com/books/v1/volumes?q=isbn:%s" isbn))
            (description nil))
        (message "Trying to find abstract for %s with Google Books..." isbn)
        (with-current-buffer (url-retrieve-synchronously url)
          (set-buffer-multibyte t)
          (set-buffer-file-coding-system 'utf-8)
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
        (if description description (progn (message "No abstract found.") nil))))))

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
		     ('ebib-entry-mode (ebib--get-key-at-point))))
	  nil))
    (derived-mode-p 'text-mode 'pdf-view-mode 'eww-mode)))

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

;;;###autoload
(defun tlon-move-entry-to-fluid (&optional key)
  "Move entry with KEY to FILE.
Save citekey to \"kill-ring\". If KEY is nil, use the key of the entry at point."
  (interactive)
  ;; TODO: add ensure in tex modes check
  (let ((key (or key (pcase major-mode
		       ('bibtex-mode (bibtex-extras-get-key))
		       ((or 'ebib-entry-mode 'ebib-index-mode) (ebib--get-key-at-point))))))
    (bibtex-extras-move-entry key tlon-file-fluid)
    (tlon-add-or-update-tlon-field-in-file key tlon-file-fluid)
    (kill-new key)
    (message "Moved entry `%1$s' to `%s' and copied `%1$s' to kill ring." key tlon-file-fluid)))

;;;###autoload
(defun tlon-move-all-fluid-entries-to-stable ()
  "Move all entries in `fluid.bib' to `stable.bib'."
  (interactive)
  (when (or (buffer-modified-p (find-file-noselect tlon-file-fluid))
	    (buffer-modified-p (find-file-noselect tlon-file-stable)))
    (user-error "Save `fluid.bib' and `stable.bib' before proceeding"))
  (let (entries)
    (with-current-buffer (find-file-noselect tlon-file-fluid)
      (widen)
      (setq entries (buffer-string))
      (erase-buffer)
      (save-buffer))
    (with-current-buffer (find-file-noselect tlon-file-stable)
      (widen)
      (goto-char (point-max))
      (insert entries)
      (save-buffer))
    (message "Moved all entries from `fluid.bib' to `stable.bib'. You may want to commit these changes.")))

;;;;; Add fields

(defun tlon-add-or-update-tlon-field ()
  "Add or update \"database\" field with \"Tlön\" value in the current BibTeX entry."
  (bibtex-extras-add-or-update-field "database" "Tlön"))

(defun tlon-add-or-update-tlon-field-in-file (key file)
  "Add or update \"database\" field with \"Tlön\" value in KEY of FILE."
  (with-current-buffer (find-file-noselect file)
    (widen)
    (bibtex-search-entry key)
    (tlon-add-or-update-tlon-field)
    (save-buffer)))

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
  (when (or (derived-mode-p 'bibtex-mode)
	    (< (point-min) (point-max)))
    (let ((after-save-hook nil))
      (tlon-remove-empty-spaces)
      (bibtex-extras-escape-special-characters)
      (when (looking-at bibtex-any-entry-maybe-empty-head)
	(bibtex-clean-entry))
      (save-buffer))))

(defun tlon-add-lang-id-to-entries ()
  "Add `langid' field to entries in the current buffer, if missing."
  (interactive)
  (unless (derived-mode-p 'bibtex-mode)
    (user-error "Not in `bibtex-mode'"))
  (widen)
  (while (bibtex-next-entry)
    (bibtex-narrow-to-entry)
    (unless (bibtex-extras-get-field "langid")
      (let ((lang (tlon-select-language)))
	(bibtex-set-field "langid" lang)))
    (widen)))

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
	  urls-to-add urls-to-exclude)
      (dolist (url urls urls-to-add)
	(goto-char (point-min))
	(re-search-forward url nil t)
	(hi-lock-face-buffer (regexp-quote url) 'hi-yellow)
	(if (and (not (member url urls-to-exclude))
		 (y-or-n-p (format "%s is missing from bibliography. Add? " url)))
	    (push url urls-to-add)
	  (push url urls-to-exclude))
	(hi-lock-unface-buffer (regexp-quote url)))
      (if (null urls-to-add)
	  (message "No URLs to add.")
	(files-extras-list-to-lines urls-to-add zotra-extras-add-multiple-urls-filename)
	(message "Saved URLs to `%s'. You can now add them with Zotra via `zotra-extras-add-multiple-urls'."
		 zotra-extras-add-multiple-urls-filename)))))

;; TODO: maybe generalize to other fields, e.g. isbn, doi
(declare-function simple-extras-simplify-url "simple-extras")
(declare-function tlon-import-eaf-get-id-from-identifier "tlon-import")
(defun tlon-get-missing-urls (&optional file)
  "Return all URLs present in FILE but missing in the Tlön bibliography.
If FILE is nil, use the file visited by the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (urls-in-biblio (tlon-get-field-in-bibliography "url"))
	 (urls-in-file (tlon-get-urls-in-file file))
	 (urls-in-biblio-simple (mapcar #'simple-extras-simplify-url urls-in-biblio))
	 (urls-in-file-simple (mapcar #'simple-extras-simplify-url urls-in-file))
	 (missing-urls-simple (cl-set-difference urls-in-file-simple urls-in-biblio-simple :test #'string=)))
    (mapcar (lambda (url)
	      (concat "https://" url))
	    missing-urls-simple)))

(defvar markdown-regex-link-inline)
(declare-function ffap-url-p "ffap")
(defun tlon-get-urls-in-file (&optional file)
  "Return a list of all the URLs in the Markdown links present in FILE.
If FILE is nil, use the file visited by the current buffer."
  (let ((file (or file (buffer-file-name))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((links))
	(while (re-search-forward markdown-regex-link-inline nil t)
	  (when-let ((url (ffap-url-p (match-string-no-properties 6))))
	    (unless (member url links)
	      (push url links))))
	(reverse links)))))

;;;;; Translation

(declare-function ebib-extras-set-field "ebib-extras")
;;;###autoload
(defun tlon-tex-create-translation-entry ()
  "Create a translation entry from the entry at point."
  (interactive)
  (unless (derived-mode-p 'ebib-entry-mode)
    (user-error "Not in `ebib-entry-mode'"))
  (let* ((language (tlon-select-language 'code 'babel))
	 (fields `(("translation" . ,(ebib--get-key-at-point))
		   ("=type=" . ,(ebib-extras-get-field "=type="))
		   ("author" . ,(ebib-extras-get-field "author"))
		   ("database" . ,(ebib-extras-get-field "database"))
		   ("langid" . ,(tlon-lookup tlon-languages-properties :name :code language))
		   ("title" . ,(read-string "Title: "))
		   ("translator" . ,(when-let ((field "translator"))
				      (ebib--edit-list-field field (list field) (ebib-unbrace nil))))
		   ("date" .  ,(format-time-string "%Y")))))
    (ebib-switch-to-database-nth 3)
    (ebib-add-entry)
    (sleep-for 0.1)
    (dolist (field fields)
      (cl-destructuring-bind (key . value) field
	(ebib-extras-set-field key value)))
    (ebib-generate-autokey)))

;;;;; Convert to `Cite'

(defvar markdown-regex-link-inline)
;;;###autoload
(defun tlon-convert-links-to-cite ()
  "Prompt the user to convert all links in the current buffer to citations."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward markdown-regex-link-inline nil t)
      (let* ((url (match-string-no-properties 6))
	     (key (tlon-bibliography-lookup "url" url "=key="))
	     (start (match-beginning 0))
	     (end (match-end 0)))
	(when key
	  (save-match-data
	    (when (y-or-n-p (format "Convert %s to citation with key %s? " url key))
	      (goto-char start)
	      (when (re-search-forward markdown-regex-link-inline end t)
		(replace-match (concat (format (tlon-md-get-tag-to-fill "Cite") key) ".") t t)))))))))

(declare-function tlon-md-get-tag-to-fill "tlon-md")
;;;###autoload
(defun tlon-convert-bibliography-to-cite ()
  "Convert all references in a bibliography section to citations using `Cite'.
NB: This command should be run with the buffer narrowed to the section
containing the bibliography (such as the \"Further reading\" section of a tag),
since it can only handle works cited one work per line and does not handle
locators (which are not relevant in a bibliography)."
  (interactive)
  (save-excursion
    (dolist (pattern (list tlon-regexp-expanded-citation-with-link
			   tlon-regexp-expanded-citation-with-no-link))
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
	(let* ((title (match-string 1))
	       (key (tlon-bibliography-lookup "title" title "=key=")))
	  (when key
	    (replace-match (concat (format (tlon-md-get-tag-to-fill "Cite") key) ".")
			   t t)))))))

(defvar citar-cache--bibliographies)
(defun tlon-bibliography-lookup (field value &optional assoc-field substring)
  "Return the ASSOC-FIELD value in the entry whose FIELD value matches VALUE.
If ASSOC-FIELD is nil, return VALUE if the entry is found, else return nil. If
SUBSTRING is non-nil, return the ASSOC-FIELD value in the entry whose FIELD
value contains VALUE as a substring."
  (let ((assoc-field (or assoc-field field)))
    (catch 'found
      (maphash (lambda (_key bibliography)
		 (let ((entries (citar-cache--bibliography-entries bibliography)))
		   (maphash (lambda (_ entry)
			      (when-let ((field-value (cdr (assoc field entry))))
				(when (if substring
					  (string-match-p (regexp-quote value) field-value)
					(string= field-value value))
				  (throw 'found (cdr (assoc assoc-field entry))))))
			    entries)))
	       citar-cache--bibliographies)
      nil)))

(defvar citar-cache--bibliographies)
(declare-function citar-cache--bibliography-entries "citar-cache")
(defun tlon-get-field-in-bibliography (field)
  "Return all FIELD values in BibTeX entries in the Tlön bibliography."
  (when (eq (hash-table-count citar-cache--bibliographies) 0)
    (user-error "No bibliographies cached; please run `M-x citar-insert-citation' (`H-/')"))
  (let (fields)
    (maphash (lambda (_key bibliography)
	       (when-let ((entries (citar-cache--bibliography-entries bibliography)))
		 (maphash (lambda (_ entry)
			    (when-let ((value (cdr (assoc field entry))))
			      (push value fields)))
			  entries)))
	     citar-cache--bibliographies)
    fields))

(defun tlon-tex-replace-keys-with-citations (&optional file audio)
  "Replace all BibTeX keys in FILE with CSL-defined citations.
If FILE is nil, use the current buffer.

By default, export citations in \"short\" or \"long\" format depending on what
the tag specified. If AUDIO is non-nil, export all citations in \"audio\" format
instead."
  (interactive)
  (if-let ((file (or file (buffer-file-name))))
      (with-current-buffer (find-file-noselect file)
	(tlon-tex-do-replace-keys-with-citations audio))
    (tlon-tex-do-replace-keys-with-citations audio)))

(declare-function tlon-md-get-tag-pattern "tlon-md")
(declare-function tlon-api-get-citation "tlon-api")
(defun tlon-tex-do-replace-keys-with-citations (&optional audio)
  "Perform the actual replacement of BibTeX keys with CSL-defined citations.
LIST is a list of cons cells, where the car is the citation style and the cdr is
the regular expression pattern to match the key.

By default, export citations in \"short\" or \"long\" format depending on what
the tag specified. If AUDIO is non-nil, export all citations in \"audio\" format
instead."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (tlon-md-get-tag-pattern "Cite") nil t)
      (let* ((key (match-string-no-properties 3))
	     short-p csl)
	(message "Processing `%s'..." key)
	(save-match-data
	  (setq short-p (string-match "short" (match-string-no-properties 6)))
	  (setq csl (if audio
			(if short-p 'short-audio 'long-audio)
		      (if short-p 'short 'long))))
	(if-let ((citation (tlon-api-get-citation key csl)))
	    (replace-match citation t)
	  (user-error "Could not find `%s'" key))))))

(defun tlon-tex-replace-keys-with-citations-in-dir (&optional audio)
  "Recursively replace all BibTeX keys with CSL-defined citations in current dir.

By default, export citations in \"short\" or \"long\" format depending on what
the tag specified. If AUDIO is non-nil, export all citations in \"audio\" format
instead."
  (interactive)
  (let ((files (directory-files-recursively default-directory "\\.md$")))
    (dolist (file files)
      (tlon-tex-replace-keys-with-citations file audio))))

(defun tlon-tex-find-next-entry-with-missing-field (field)
  "For each entry in the current BibTeX buffer, check if FIELD’s value is missing."
  (interactive (list (completing-read "Field: " bibtex-extras-biblatex-fields nil t)))
  (bibtex-next-entry)
  (while (or (bibtex-extras-get-field field)
	     (bibtex-extras-get-field "crossref"))
    (bibtex-next-entry)))

;;;;; Abstracts

;;;;;; Translations

(defun tlon-read-abstract-translations ()
  "Read the JSON file with the abstract translations."
  (tlon-read-json tlon-file-abstract-translations))

(defun tlon-write-abstract-translations (data)
  "Write DATA to the JSON file with the abstract translations."
  (tlon-write-data tlon-file-abstract-translations data))

(defun tlon-add-abstract-translation (key target-lang translation &optional overwrite var)
  "Add a TRANSLATION of KEY in TARGET-LANG.
If a translation already exists, do nothing unless OVERWRITE is non-nil. If KEY
is not present, add a new entry for this KEY."
  (let* ((data (or (symbol-value var) (tlon-read-abstract-translations)))
	 (entry (assoc key data)))
    (if entry
	(let ((lang-entry (assoc target-lang (cdr entry))))
	  (if lang-entry
	      (when overwrite
		(setcdr lang-entry translation))
	    (setcdr entry (cons (cons target-lang translation) (cdr entry)))))
      (setq data (cons (cons key (list (cons target-lang translation))) data)))
    (if var
	(set var data)
      (tlon-write-abstract-translations data))))

(declare-function tlon-deepl-translate "tlon-deepl")
(declare-function tlon-deepl-print-translation "tlon-deepl")
;; TODO: include BibTeX entry as context so that DeepL can use correct genders, plurals, etc.
(defun tlon-translate-abstract (&optional key target-lang source-lang var)
  "Translate the abstract of KEY from SOURCE-LANG to TARGET-LANG.
If KEY is nil, use the key of the entry at point. If TARGET-LANG is nil, prompt
the user to select a language. If VAR is non-nil, save the translation in VAR;
otherwise, save it to `tlon-file-abstract-translations'."
  (interactive)
  (when-let* ((get-field (pcase major-mode
			   ('ebib-entry-mode #'ebib-extras-get-field)
			   ('bibtex-mode #'bibtex-extras-get-field)))
	      (key (or key (pcase major-mode
			     ('ebib-entry-mode (ebib--get-key-at-point))
			     ('bibtex-mode (bibtex-extras-get-key)))))
	      (abstract (funcall get-field "abstract"))
	      (source-lang (or source-lang
			       (tlon-lookup tlon-languages-properties :code :name (funcall get-field "langid"))))
	      (target-lang (or target-lang (tlon-select-language 'code 'babel))))
    (tlon-deepl-translate (tlon-tex-remove-braces abstract) target-lang source-lang
			  (tlon-translate-abstract-callback key target-lang nil var))))

(defun tlon-translate-abstracts (&optional target-lang source-lang overwrite var)
  "Translate multiple abstracts in the current buffer.
TARGET-LANG is the target language of the translation. If SOURCE-LANG is nil,
use the language of the `langid' field. If OVERWRITE is non-nil, overwrite the
existing translation. If VAR is non-nil, save the translations in VAR;
otherwise, save them in `tlon-file-abstract-translations'.

Set the value of `dotimes' below to a few hundreds and repeat the command until
all abstracts are translated. Use VAR to save the translations in a variable,
since otherwise the file will be overwritten each time, causing repeated errors.

Make sure the relevant glossaries are loaded before running this function."
  (unless (derived-mode-p 'bibtex-mode)
    (user-error "Not in `bibtex-mode'"))
  (widen)
  (dotimes (_ 2000)
    (bibtex-next-entry)
    (bibtex-narrow-to-entry)
    (let ((key (bibtex-extras-get-key)))
      (when (or overwrite (not (alist-get key (symbol-value var) nil nil #'string=)))
	(tlon-translate-abstract key target-lang source-lang var))
      (widen))))

(defun tlon-translate-abstract-callback (key target-lang &optional overwrite var)
  "Callback for `tlon-translate-abstract'.
KEY is the key of the entry and TARGET-LANG is the target language of the
translation. If OVERWRITE is non-nil, overwrite the existing translation. If VAR
is non-nil, save the translations in VAR; otherwise, save them in
`tlon-file-abstract-translations'."
  (let ((translation (tlon-deepl-print-translation)))
    (tlon-add-abstract-translation key target-lang translation overwrite var)))

;;;;;; Report

;;;###autoload
(defun tlon-tex-entries-report ()
  "Return a report about the BibTeX buffer at point."
  (interactive)
  (tlon-tex-load-excluded-keys)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (let (no-abstract short-abstracts long-abstracts minimal-abstracts)
      (while (bibtex-next-entry)
	(bibtex-narrow-to-entry)
	(let ((key (bibtex-extras-get-key)))
	  (unless (or (member key tlon-tex-excluded-keys)
		      (bibtex-extras-get-field "crossref"))
	    (if (re-search-forward "^[[:blank:]]+abstract = {" nil t)
		(let* ((begin (point))
		       (end (progn (bibtex-next-field t) (beginning-of-line) (left-char 3) (point))))
		  (cond ((> (count-words begin end) tlon-tex-max-abstract-length)
			 (push key long-abstracts))
			((< (count-words begin end) 10)
			 (push key minimal-abstracts))
			((< (count-words begin end) tlon-tex-min-abstract-length)
			 (push key short-abstracts))))
	      (push key no-abstract)))
	  (widen)))
      (setq long-abstracts (tlon-text-remove-translated-entries long-abstracts)
	    short-abstracts (tlon-text-remove-translated-entries short-abstracts)
	    minimal-abstracts (tlon-text-remove-translated-entries minimal-abstracts)
	    no-abstract (tlon-text-remove-translated-entries no-abstract))
      (let ((buffer-name "*tlon-tex-entries-report*"))
	(with-current-buffer (get-buffer-create buffer-name)
	  (erase-buffer)
	  (insert (format "Abstracts above maximum length (%s words): %s

Abstracts below minimum length (%s words): %s

Abstracts with fewer than 10 words: %s

No abstract: %s"
			  tlon-tex-max-abstract-length long-abstracts
			  tlon-tex-min-abstract-length short-abstracts
			  minimal-abstracts
			  no-abstract))
	  (pop-to-buffer buffer-name))))))

(declare-function citar-extras-open-in-ebib "citar-extras")
(defun tlon-text-remove-translated-entries (list)
  "Remove entries in LIST if they are translated entries."
  (let (new-list)
    (dolist (key list new-list)
      (save-window-excursion
	(citar-extras-open-in-ebib key)
	(unless (ebib-extras-get-field "translation")
	  (push key new-list))))))

(defun tlon-tex-save-excluded-keys ()
  "Save the excluded citation keys to the file."
  (with-temp-file tlon-tex-excluded-keys-file
    (insert ";;; Tlon excluded keys -*- lexical-binding: t -*-\n\n")
    (insert ";; This file contains the keys to be excluded from `tlon-tex-entries-report'.\n\n")
    (insert "(setq tlon-tex-excluded-keys '")
    (prin1 tlon-tex-excluded-keys (current-buffer))
    (insert ")\n\n(provide 'tlon-tex-excluded-keys)")))

(defun tlon-tex-load-excluded-keys ()
  "Load the excluded keys from the file."
  (let ((file tlon-tex-excluded-keys-file))
    (when (file-exists-p file)
      (load file))))

(defun tlon-tex-add-to-excluded-keys (string)
  "Add STRING of key symbols to the list of keys no needing an abstract."
  (interactive (list (read-string "Key(s): " (thing-at-point 'word))))
  (tlon-tex-load-excluded-keys)
  (dolist (key (split-string string))
    (push key tlon-tex-excluded-keys))
  (tlon-tex-save-excluded-keys))

;;;;; Monitor field values

(declare-function tlon-deepl-translate-abstract "tlon-deepl")
(defun tlon-tex-field-modified (field value key)
  "Act when FIELD is set to VALUE in a BibTeX entry with KEY."
  (cond ((string= field "abstract")
	 (tlon-deepl-translate-abstract value key))))

(defun tlon-tex-remove-braces (string)
  "Remove braces from STRING."
  (when string
    (replace-regexp-in-string "[{}]" "" string)))

(defun tlon-tex-bibtex-set-field-advice (orig-fun &rest args)
  "Advice to catch modifications to BibTeX fields in `bibtex-mode'.
ORIG-FUN is the original function, ARGS are the arguments passed to it."
  (let ((field (nth 0 args))
	(value (nth 1 args)))
    (apply orig-fun args)
    (tlon-tex-field-modified field value (bibtex-extras-get-key))))

(advice-add 'bibtex-set-field :around #'tlon-tex-bibtex-set-field-advice)

(defun tlon-tex-ebib-set-field-advice (orig-fun &rest args)
  "Advice to catch modifications to BibTeX fields in `ebib-entry-mode'.
ORIG-FUN is the original function, ARGS are the arguments passed to it."
  (let ((field (nth 0 args))
	(value (nth 1 args)))
    (apply orig-fun args)
    (tlon-tex-field-modified field value (ebib--get-key-at-point))))

(advice-add 'ebib-set-field-value :around #'tlon-tex-ebib-set-field-advice)

;;;;; Menu

;;;###autoload (autoload 'tlon-tex-menu "tlon-tex" nil t)
(transient-define-prefix tlon-tex-menu ()
  "Menu for `tex' functions."
  [["Markdown"
    "URLs missing from database"
    ("f" "Find in file"                        tlon-prompt-to-add-missing-urls)
    ("z" "Add with Zotra"                      zotra-extras-add-multiple-urls)
    ""
    "Convert to `Cite'"
    ("b" "Convert bibliography"                tlon-convert-bibliography-to-cite)
    ("l" "Convert links"                       tlon-convert-links-to-cite)]
   ["Ebib"
    ("a" "Fetch abstract"                      tlon-fetch-and-set-abstract)
    ("c" "Create translation entry"            tlon-tex-create-translation-entry)]
   ["BibTeX"
    "Report"
    ("r r" "Generate"                          tlon-tex-entries-report)
    ("r x" "Add to not needing abstract"       tlon-tex-add-to-excluded-keys)
    ""
    "Move"
    ("t" "Move this entry to Tlön database"    tlon-move-entry-to-fluid)
    ("s" "Move all entries to stable"          tlon-move-all-fluid-entries-to-stable)]])

(provide 'tlon-tex)
;;; tlon-tex.el ends here
