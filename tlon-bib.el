;;; tlon-bib.el --- BibTeX related functionality -*- lexical-binding: t -*-

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
(require 'citar-cache)
(require 'gptel)
(require 'gptel-extras)
(require 'seq)
(require 'cl-lib)
(require 'shut-up)
(require 'tlon)
(require 'tlon-core)
(require 'tlon-ai)
(require 'transient)
(require 'subr-x)
(require 'json)

;;;; User options

(defgroup tlon-bib ()
  "BibTeX related functionality."
  :group 'tlon)

(defcustom tlon-abstract-overwrite 'ask
  "Whether to overwrite the abstract if already present."
  :type '(choice
	  (const :tag "Always overwrite" always)
	  (const :tag "Never overwrite" never)
	  (const :tag "Ask" ask)))

(defcustom tlon-bib-replace-citations-model
  '("Gemini" . gemini-pro-latest)
  "Model to use for replacing citations (`tlon-bib-replace-citations-in-file').
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, use the
default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-bib)

(defcustom tlon-bib-add-missing-citations-model
  tlon-bib-replace-citations-model
  "Model to use for adding missing citations (`tlon-bib-add-missing-citations').
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, use the
default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-bib)

(defcustom tlon-bib-replace-citations-max-concurrent 5
  "Number of org files to process simultaneously when replacing citations.
This option controls the batch size for
`tlon-bib-replace-citations-in-org-files-in-directory'."
  :type 'natnum
  :group 'tlon-bib)

;;;; Variables

;;;;; Files

(defconst tlon-bib-excluded-keys-file
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

(defconst tlon-bib-pandoc-cite-pattern-long
  "\\[-?@\\(?:{\\(?1:.*?\\)}\\|\\(?1:[[:alnum:]_][[:alnum:]]*\\(?:[:.#$%&+?<>~/-][[:alnum:]]+\\)*\\)\\(?:, \\(?2:.*?\\)\\)?\\)\\]"
  "Regular expression for a \"long\" Pandoc citation key.

Group 1 captures the key. Group 2 captures the locator(s), if present. Based on
`citar-markdown-citation-key-regexp'.")

(defconst tlon-bib-pandoc-cite-pattern-short
  (concat "<cite>" tlon-bib-pandoc-cite-pattern-long "</cite>")
  "Regular expression for a \"short\" Pandoc citation key.

Group 1 captures the key. Group 2 captures the locator(s), if present. Based on
`citar-markdown-citation-key-regexp'.")

(defconst tlon-bib-pandoc-cite-pattern
  (concat "\\(?:<cite>\\)?" tlon-bib-pandoc-cite-pattern-long "\\(?:</cite>\\)?")
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

;;;;;; Reports

(defvar tlon-bib-excluded-keys nil
  "List of keys of entries that do not need an abstract.
Keys in this list are excluded from the list of entries missing an abstract
generated by `tlon-bib-entries-report'.")

;;;; Functions

;;;;; General

;;;###autoload
(defun tlon-ensure-bib ()
  "Ensure that the current buffer is in a BibTeX-related mode."
  (unless (derived-mode-p 'bibtex-mode 'ebib-entry-mode 'ebib-index-mode)
    (user-error "Not in a BibTeX-related mode")))

;;;###autoload
(defun tlon-ensure-bibtex ()
  "Ensure that the current buffer is in `bibtex-mode'."
  (unless (derived-mode-p 'bibtex-mode)
    (user-error "Not in `bibtex-mode'")))

(defun tlon-get-key-at-point ()
  "If there is a bibtex key at point, return it."
  (pcase major-mode
    ((or 'ebib-index-mode 'ebib-entry-mode) (ebib--get-key-at-point))
    ('bibtex-mode (bibtex-extras-get-key))))

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
	(let ((key (tlon-get-key-at-point))
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
	  (message "Skipping: `%s' already contains an abstract." (tlon-get-key-at-point))
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
  (let ((key (or key (tlon-get-key-at-point))))
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

;;;;; Modify fields

(declare-function citar-extras-goto-bibtex-entry "citar-extras")
;;;###autoload
(defun tlon-bib-swap-colon-in-title (&optional key)
  "Swap colon and modified letter colon in the title of a BibTeX entry.
Prompt for KEY, jump to the entry using `citar-extras-goto-bibtex-entry',
replace all occurrences of \":\" with \"꞉\" or vice versa in the title field,
save the buffer, and display an appropriate message."
  (interactive)
  (let ((key (citar-extras-goto-bibtex-entry key)))
    (bibtex-narrow-to-entry)
    (let ((bounds (bibtex-search-forward-field "title" t))
	  direction)
      (unless bounds
	(user-error "Entry %s has no title field" key))
      (let* ((title (bibtex-text-in-field-bounds bounds t))
	     (new-title
	      (cond
	       ((string-match-p "꞉" title)
		(setq direction 'modified-to-colon)
		(replace-regexp-in-string "꞉" ":" title nil t))
	       ((string-match-p ":" title)
		(setq direction 'colon-to-modified)
		(replace-regexp-in-string ":" "꞉" title nil t))
	       (t
		(user-error "Title for %s contains neither \":\" nor \"꞉\"" key))))
	     (message (format (if (eq direction 'colon-to-modified)
				  "Replaced colon with modified letter colon in title of %s"
				"Replaced modified letter colon with colon in title of %s")
			      key)))
	(bibtex-set-field "title" new-title)
	(save-buffer)
	(kill-buffer)
	(run-with-timer 0.5 nil (lambda () (message message)))))))

;;;;; Cleanup

(declare-function bibtex-extras-escape-special-characters "bibtex-extras")
;;;###autoload
(defun tlon-auto-clean-entry ()
  "Clean up bibtex entry at point upon saving."
  (when (or (derived-mode-p 'bibtex-mode)
	    (< (point-min) (point-max)))
    (let ((after-save-hook nil))
      (tlon-remove-empty-spaces)
      (bibtex-extras-escape-special-characters)
      (when (and
	     (not (file-equal-p (buffer-file-name) tlon-file-db-upstream))
	     (looking-at bibtex-any-entry-maybe-empty-head))
	(bibtex-clean-entry))
      (save-buffer))))

(defun tlon-add-lang-id-to-entries ()
  "Add `langid' field to entries in the current buffer, if missing."
  (interactive)
  (tlon-ensure-bibtex)
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

(defvar bibtex-extras-escaped-characters)
;;;###autoload
(defun tlon-bib-unescape-escaped-characters (&optional file)
  "Unescape characters escaped by `bibtex-extras-escaped-characters' in FILE.
If FILE is nil, use the file visited by the current buffer, if any. Return the
total number of replacements made."
  (interactive)
  (let* ((buf (if file (find-file-noselect file) (current-buffer)))
	 (total 0))
    (unless (buffer-live-p buf)
      (user-error "No file specified and current buffer does not visit a file"))
    (with-current-buffer buf
      (save-excursion
	(save-restriction
	  (widen)
	  (let ((case-fold-search nil))
	    (dolist (ch bibtex-extras-escaped-characters)
	      (let* ((s (if (integerp ch) (char-to-string ch) ch))
		     (from (concat "\\\\" s)))
		(goto-char (point-min))
		(while (search-forward from nil t)
		  (replace-match s nil t)
		  (cl-incf total)))))
	  (save-buffer))))
    (when (called-interactively-p 'interactive)
      (message "Unescaped %d occurrence%s%s"
	       total
	       (if (= total 1) "" "s")
	       (if (and (buffer-live-p buf) (buffer-file-name buf))
		   (format " in %s" (file-name-nondirectory (buffer-file-name buf)))
		 "")))
    total))

;;;###autoload
(defun tlon-bib-remove-url-fields-with-doi (&optional save)
  "Remove the `url' field from entries with a `doi' field in the current buffer.
If SAVE is non-nil (interactively with a prefix argument), save the buffer after
modification. Display a message reporting how many entries were updated."
  (interactive "P")
  (tlon-ensure-bibtex)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (let ((removed 0))
      (bibtex-map-entries
       (lambda (_key start _end)
	 (save-excursion
	   (goto-char start)
	   (save-restriction
	     (bibtex-narrow-to-entry)
	     (when (and (bibtex-extras-get-field "doi")
			(bibtex-extras-get-field "url"))
	       ;; completely remove the url field instead of only clearing its value
	       (when-let ((url-bounds (bibtex-search-forward-field "url" t)))
		 (goto-char (bibtex-start-of-name-in-field url-bounds))
		 ;; kill the whole field (including trailing comma, if any)
		 (bibtex-kill-field nil t))
	       (cl-incf removed))))))
      (when save (save-buffer))
      (message "Removed url field from %d entr%s."
	       removed (if (= removed 1) "y" "ies")))))

;;;###autoload
(defun tlon-bib-remove-url-fields-with-isbn (&optional save)
  "Remove the `url' field from entries with an `isbn' field in the current buffer.
If SAVE is non-nil (interactively with a prefix argument), save the buffer after
modification. Display a message reporting how many entries were updated."
  (interactive "P")
  (tlon-ensure-bibtex)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (let ((removed 0))
      (bibtex-map-entries
       (lambda (_key start _end)
	 (save-excursion
	   (goto-char start)
	   (save-restriction
	     (bibtex-narrow-to-entry)
	     (when (and (bibtex-extras-get-field "isbn")
			(bibtex-extras-get-field "url"))
	       (when-let ((url-bounds (bibtex-search-forward-field "url" t)))
		 (goto-char (bibtex-start-of-name-in-field url-bounds))
		 (bibtex-kill-field nil t))
	       (cl-incf removed))))))
      (when save (save-buffer))
      (message "Removed url field from %d entr%s."
	       removed (if (= removed 1) "y" "ies")))))

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

(defvar zotra-extras-add-multiple-urls-from-file)
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
	(files-extras-list-to-lines urls-to-add zotra-extras-add-multiple-urls-from-file)
	(message "Saved URLs to `%s'. You can now add them with Zotra via `zotra-extras-add-multiple-urls'."
		 zotra-extras-add-multiple-urls-from-file)))))

;; TODO: maybe generalize to other fields, e.g. isbn, doi
(declare-function simple-extras-simplify-url "simple-extras")
(declare-function tlon-import-eaf-get-id-from-identifier "tlon-import")
(declare-function tlon-get-urls-in-file "tlon-url")
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

(declare-function simple-extras-slugify "simple-extras")

;;;###autoload
(defun tlon-bib-populate-url-field ()
  "Populate the `url' field of the BibTeX entry at point.
Build the URL as BASE/PATH/SLUG where:
- BASE is (tlon-repo-lookup :url :subproject \"uqbar\" :language LANG)
- PATH is (tlon-lookup tlon-core-bare-dirs LANG \"en\" \"articles\")
- SLUG is (simple-extras-slugify (TITLE))

LANG is the two-letter language code derived from the entry's `langid' field."
  (interactive)
  (tlon-ensure-bib)
  (cl-destructuring-bind (get-field set-field)
      (pcase major-mode
	('ebib-entry-mode '(ebib-extras-get-field ebib-extras-set-field))
	('bibtex-mode '(bibtex-extras-get-field bibtex-set-field))
	(_ (user-error "Not in a BibTeX-related mode")))
    (save-restriction
      (when (derived-mode-p 'bibtex-mode)
        (bibtex-narrow-to-entry)
        (bibtex-beginning-of-entry))
      (let* ((lang-name (funcall get-field "langid"))
	     (_ (unless lang-name (user-error "Entry has no langid field")))
	     (lang (tlon-lookup tlon-languages-properties :code :name lang-name))
	     (_ (unless lang (user-error "Could not determine language code for %s" lang-name)))
	     (title (funcall get-field "title"))
	     (_ (unless title (user-error "Entry has no title field")))
	     (slug (simple-extras-slugify (tlon-bib-remove-braces title)))
	     (base (tlon-repo-lookup :url :subproject "uqbar" :language lang))
	     (_ (unless base (user-error "Could not determine base URL for language %s" lang)))
	     (path (tlon-lookup tlon-core-bare-dirs lang "en" "articles"))
	     (_ (unless path (user-error "Could not determine path for language %s" lang)))
	     (url (mapconcat #'identity
			     (list (string-remove-suffix "/" base)
				   (string-trim path "/")
				   slug)
			     "/")))
	(when (funcall get-field "url")
	  (unless (y-or-n-p "Entry already has a url. Overwrite? ")
	    (user-error "Aborted")))
	(funcall set-field "url" url)
	(message "Set url of `%s' to %s." (tlon-get-key-at-point) url)))))

(declare-function tlon-yaml-get-filenames-in-dir "tlon-yaml")
(declare-function tlon-yaml-get-key "tlon-yaml")

;;;###autoload
(defun tlon-bib-populate-url-fields-in-language ()
  "Populate the `url' field for all entries in the language at point.
Determine LANG from the current entry's `langid', collect all article files in
the corresponding site directory, read their `key' from YAML front matter, and
for each matching BibTeX entry, populate the `url` field if missing or empty.
If the field is present, prompt to confirm overwriting and regenerate it via
`tlon-bib-populate-url-field'."
  (interactive)
  (tlon-ensure-bib)
  (cl-destructuring-bind (get-field _set-field)
      (pcase major-mode
	('ebib-entry-mode '(ebib-extras-get-field nil))
	('bibtex-mode '(bibtex-extras-get-field nil))
	(_ (user-error "Not in a BibTeX-related mode")))
    (let* ((lang-name (funcall get-field "langid"))
	   (_ (unless lang-name (user-error "Entry has no langid field")))
	   (lang (tlon-lookup tlon-languages-properties :code :name lang-name))
	   (_ (unless lang (user-error "Could not determine language code for %s" lang-name)))
	   (repo-dir (tlon-repo-lookup :dir :subproject "uqbar" :language lang))
	   (_ (unless repo-dir (user-error "Could not determine repository directory for %s" lang)))
	   (path (tlon-lookup tlon-core-bare-dirs lang "en" "articles"))
	   (_ (unless path (user-error "Could not determine path for language %s" lang)))
	   (articles-dir (file-name-concat repo-dir path))
	   (files (let* ((names (tlon-yaml-get-filenames-in-dir articles-dir "md")))
		    (mapcar (lambda (name)
			      (if (file-name-absolute-p name)
				  name
				(file-name-concat articles-dir name)))
			    names)))
	   (keys (delete-dups (delq nil (mapcar (lambda (f) (tlon-yaml-get-key "key" f)) files))))
	   (_ (when (null keys)
		(user-error "No article keys found in %s" articles-dir)))
	   (updated 0)
	   (skipped 0)
	   (missing 0))
      (dolist (key keys)
	(condition-case err
	    (progn
	      (citar-extras-goto-bibtex-entry key)
	      (bibtex-narrow-to-entry)
	      (let ((url (bibtex-extras-get-field "url")))
		(if (and url (not (string-empty-p (string-trim url))))
		    (if (y-or-n-p (format "Entry %s already has a url. Overwrite? " key))
			(progn
			  ;; remove existing url field to avoid double prompt downstream
			  (when-let ((url-bounds (bibtex-search-forward-field "url" t)))
			    (goto-char (bibtex-start-of-name-in-field url-bounds))
			    (bibtex-kill-field nil t))
			  (tlon-bib-populate-url-field)
			  (cl-incf updated))
		      (cl-incf skipped))
		  (tlon-bib-populate-url-field)
		  (cl-incf updated)))
	      (widen)
	      (when (buffer-modified-p) (save-buffer)))
	  (error
	   (cl-incf missing)
	   (message "Could not process key %s: %s" key (error-message-string err)))))
      (message "Populate URLs (%s): %d updated, %d skipped (had url), %d not found"
	       lang updated skipped missing))))

(declare-function ebib--cur-db "ebib-utils")
;;;###autoload
(defun tlon-bib-populate-journaltitle-fields-in-language ()
  "Populate the `journaltitle' field for all entries in the language at point.
Determine LANG code from current entry's `langid'. For each BibTeX key found
in the corresponding site articles directory, set `journaltitle' to the value
returned by (tlon-uqbar-front-get-message LANG \"HomePage\" \"Title\").
If the field is already set, prompt before overwriting."
  (interactive)
  (tlon-ensure-bib)
  (cl-destructuring-bind (get-field _set-field)
      (pcase major-mode
	('ebib-entry-mode '(ebib-extras-get-field nil))
	('bibtex-mode '(bibtex-extras-get-field nil))
	(_ (user-error "Not in a BibTeX-related mode")))
    (let* ((lang-name (funcall get-field "langid"))
	   (_ (unless lang-name (user-error "Entry has no langid field")))
	   (lang (tlon-lookup tlon-languages-properties :code :name lang-name))
	   (_ (unless lang (user-error "Could not determine language code for %s" lang-name)))
	   (jt (tlon-uqbar-front-get-message lang "HomePage" "title"))
	   (_ (unless (and jt (not (string-empty-p jt)))
		(user-error "Could not determine journaltitle for language %s" lang)))
	   (repo-dir (tlon-repo-lookup :dir :subproject "uqbar" :language lang))
	   (_ (unless repo-dir (user-error "Could not determine repository directory for %s" lang)))
	   (path (tlon-lookup tlon-core-bare-dirs lang "en" "articles"))
	   (_ (unless path (user-error "Could not determine path for language %s" lang)))
	   (articles-dir (file-name-concat repo-dir path))
	   (files (let* ((names (tlon-yaml-get-filenames-in-dir articles-dir "md")))
		    (mapcar (lambda (name)
			      (if (file-name-absolute-p name)
				  name
				(file-name-concat articles-dir name)))
			    names)))
	   (keys (delete-dups (delq nil (mapcar (lambda (f) (tlon-yaml-get-key "key" f)) files))))
	   (_ (when (null keys)
		(user-error "No article keys found in %s" articles-dir)))
	   (updated 0)
	   (skipped 0)
	   (missing 0))
      (dolist (key keys)
	(condition-case err
	    (progn
	      (citar-extras-goto-bibtex-entry key)
	      (when (derived-mode-p 'ebib-index-mode)
		(citar-extras-open-in-ebib key))
	      (cond
	       ((derived-mode-p 'bibtex-mode)
		(bibtex-narrow-to-entry)
		(let ((existing (bibtex-extras-get-field "journaltitle")))
		  (if (and existing (not (string-empty-p (string-trim existing))))
		      (if (y-or-n-p (format "Entry %s already has a journaltitle. Overwrite? " key))
			  (progn
			    (bibtex-set-field "journaltitle" jt)
			    (cl-incf updated))
			(cl-incf skipped))
		    (bibtex-set-field "journaltitle" jt)
		    (cl-incf updated)))
		(widen)
		(when (buffer-modified-p) (save-buffer)))
	       ((derived-mode-p 'ebib-entry-mode 'ebib-index-mode)
		(let ((existing (ebib-extras-get-field "journaltitle")))
		  (if (and existing (not (string-empty-p (string-trim existing))))
		      (if (y-or-n-p (format "Entry %s already has a journaltitle. Overwrite? " key))
			  (progn
			    (ebib-extras-set-field "journaltitle" jt)
			    (cl-incf updated))
			(cl-incf skipped))
		    (ebib-extras-set-field "journaltitle" jt)
		    (cl-incf updated)))
		(when (fboundp 'ebib-save-current-database)
		  (ignore-errors (ebib-save-current-database (ebib--cur-db)))))
	       (t
		(user-error "Unsupported mode: %s" major-mode))))
	  (error
	   (cl-incf missing)
	   (message "Could not process key %s: %s" key (error-message-string err)))))
      (message "Populate journaltitle (%s): %d updated, %d skipped (had value), %d not found"
	       lang updated skipped missing))))

;;;;; Translation

(declare-function ebib-extras-set-field "ebib-extras")
;;;###autoload
(defun tlon-create-bibtex-translation (&optional target-code no-glossary)
  "Create a BibTeX entry representing a translation of the entry at point.
The command works in `bibtex-mode', `ebib-entry-mode' and
`ebib-index-mode'.

TARGET-CODE is the two-letter language code for the target language. If nil,
prompt the user to select a language. NO-GLOSSARY, when non-nil, disables
glossary usage during translation.

Fields set in the new entry:

- entry type: taken from the original entry (defaults to \"online\").
- key: original key followed by capitalized two-letter language code (e.g., Tr).
- langid: target language (standard name, not the code).
- title / abstract: translated with DeepL.
- author: copied from the original entry.
- journaltitle: looked-up in `tlon-site-data'.
- translation: key of the original entry.
- date: current date (YYYY-MM-DD).
- timestamp: not inserted (let tools add it later).
- url: populated with `tlon-bib-populate-url-field'."
  (interactive)
  (tlon-ensure-bib)
  (let* ((target-code (or target-code (tlon-select-language 'code 'babel)))
	 (target-lang (tlon-lookup tlon-languages-properties :standard :code target-code))
	 (mode-ebib   (derived-mode-p 'ebib-entry-mode 'ebib-index-mode))
	 (orig-key    (tlon-get-key-at-point))
	 ;; Site information remains language–specific
	 (site-name   (tlon-lookup tlon-site-data :name :language target-code))
	 (get-field   (if mode-ebib #'ebib-extras-get-field #'bibtex-extras-get-field))
	 (entry-type
	  (or (funcall get-field "=type=")
	      (when (derived-mode-p 'bibtex-mode)
		(downcase (or (bibtex-type-in-head) "online")))
	      "online"))
	 (author      (funcall get-field "author"))
	 (orig-title  (funcall get-field "title"))
	 (orig-abs    (funcall get-field "abstract"))
	 (source-lang (tlon-get-iso-code
		       (downcase (or (funcall get-field "langid") "english"))))
	 ;; DeepL translations (synchronous wrapper defined below)
	 (trans-title (tlon-bib--translate-string orig-title source-lang target-code no-glossary))
	 (trans-abs   (tlon-bib--translate-string orig-abs   source-lang target-code no-glossary))
	 (new-key     (concat orig-key (capitalize target-code)))
	 (date-now    (format-time-string "%Y-%m-%d")))
    (if mode-ebib
	;; -------- EBIB --------
	(progn
	  (ebib-switch-to-database-nth 3)      ; translation DB
	  (ebib-add-entry)                     ; create skeleton
	  (sleep-for 0.05)
	  ;; change key first
	  (ebib--update-keyname new-key)
	  ;; set fields
	  (mapc (lambda (kv) (ebib-extras-set-field (car kv) (cdr kv)))
		`(("langid"      . ,target-lang)
		  ("title"       . ,trans-title)
		  ("author"      . ,author)
		  ("journaltitle" . ,site-name)
		  ("translation" . ,orig-key)
		  ("abstract"    . ,trans-abs)
		  ("date"        . ,date-now)))
	  (ebib-extras-set-field "=type=" entry-type)
	  (message "Created translation entry %s in Ebib." new-key))
      ;; -------- BIBTeX --------
      (save-excursion
	(bibtex-narrow-to-entry)
	(bibtex-end-of-entry)
	(end-of-line)
	(open-line 2)
	(forward-line 1)
	(insert (format "@%s{%s,\n"
			entry-type new-key))
	(dolist (field
		 `(("langid"      . ,target-lang)
		   ("title"       . ,trans-title)
		   ("author"      . ,author)
		   ("journaltitle". ,site-name)
		   ("translation" . ,orig-key)
		   ("abstract"    . ,trans-abs)
		   ("date"        . ,date-now)))
	  (insert (format "\t%s = {%s},\n" (car field) (cdr field))))
	;; close entry
	(insert "}\n")
	(bibtex-clean-entry)))
    (unless mode-ebib
      (bibtex-search-entry new-key))
    (tlon-bib-populate-url-field)
    (prog1 new-key
      (message "Inserted translation entry %s." new-key))))

(defun tlon-bib--translate-string (string source-lang target-lang &optional no-glossary)
  "Synchronously translate STRING from SOURCE-LANG to TARGET-LANG with DeepL.
If NO-GLOSSARY is non-nil, do not use glossary for translation. Return STRING
unchanged if translation fails."
  (if (or (not string) (string-empty-p string))
      string
    (let (result)
      (tlon-deepl-translate string target-lang source-lang
			    (lambda ()
			      (setq result (tlon-deepl-print-translation)))
                            no-glossary)
      (or result string))))

;;;###autoload
(defun tlon-bib-create-translations-from-dir (&optional dir)
  "Create translation entries for Markdown files in DIR.
DIR defaults to `default-directory'. For each Markdown file in DIR:

1. Read its YAML fields `original_key' and `title'.

2. Jump to the BibTeX entry with that key.

3. Call `tlon-create-bibtex-translation'.

4. Set the new entry's `title' to the YAML `title' value (if present).

5. Regenerate the `url' based on the updated title via
`tlon-bib-populate-url-field'."
  (interactive)
  (let* ((dir (file-name-as-directory (or dir default-directory)))
         (files (directory-files dir t "\\.md\\'"))
         (processed 0))
    (dolist (file files)
      (let ((key (ignore-errors (tlon-yaml-get-key "original_key" file))))
        (if (not key)
            (message "Skipping %s: no original_key" (file-name-nondirectory file))
          (condition-case err
              (progn
                ;; Open original entry
                (citar-extras-goto-bibtex-entry key)
                ;; Create translation (Korean, no glossary), capture new key
                (let* ((new-key (tlon-create-bibtex-translation "ko" t))
                       (yaml-title (ignore-errors (tlon-yaml-get-key "title" file))))
                  (when new-key
                    (save-excursion
                      (save-restriction
                        (widen)
                        (bibtex-search-entry new-key)
                        (bibtex-narrow-to-entry)
                        ;; If YAML title present, set it
                        (when (and yaml-title (not (string-empty-p (string-trim yaml-title))))
                          (bibtex-set-field "title" yaml-title))
                        ;; Remove existing url field (to avoid overwrite prompt)
                        (when-let ((url-bounds (bibtex-search-forward-field "url" t)))
                          (goto-char (bibtex-start-of-name-in-field url-bounds))
                          (bibtex-kill-field nil t))
                        ;; Regenerate URL based on updated title
                        (tlon-bib-populate-url-field)
                        (widen)))
                    (when (buffer-modified-p) (save-buffer))))
                (setq processed (1+ processed)))
            (error
             (message "Error processing %s (key %s): %s"
                      (file-name-nondirectory file) key (error-message-string err)))))))
    (message "Finished. Processed %d file%s."
             processed (if (= processed 1) "" "s"))
    processed))

;;;;; Convert to `Cite'

(defvar markdown-regex-link-inline)
;;;###autoload
(defun tlon-convert-links-to-cite ()
  "Prompt the user to convert each URL in the current buffer to a citation.
The user will only be prompted to convert URLs for which a BibTeX entry is
found."
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
		(replace-match (concat (format (tlon-md-get-tag-to-fill "Cite") key "" "") ".") t t)))))))))

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
	    (replace-match (concat (format (tlon-md-get-tag-to-fill "Cite") key "" "") ".")
			   t t)))))))

(defvar citar-cache--bibliographies)
;;;###autoload
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

(declare-function citar-cache--get-bibliography "citar-cache")
(defun tlon-bib-get-keys-in-file (file)
  "Return a list of all BibTeX keys in FILE, using the citar cache.
FILE should be the absolute path to the BibTeX file.
Ensures the file is cached by citar if not already."
  (let* ((bib (citar-cache--get-bibliography (file-truename file))) ; Get/cache bibliography
	 (entries (citar-cache--bibliography-entries bib)))
    (map-keys entries)))

(defun tlon-bib-replace-keys-with-citations (&optional file audio)
  "Replace all BibTeX keys in FILE with CSL-defined citations.
If FILE is nil, use the current buffer.

By default, export citations in \"short\" or \"long\" format depending on what
the tag specified. If AUDIO is non-nil, export all citations in \"audio\" format
instead."
  (interactive)
  (if-let ((file (or file (buffer-file-name))))
      (with-current-buffer (find-file-noselect file)
	(tlon-bib-do-replace-keys-with-citations audio))
    (tlon-bib-do-replace-keys-with-citations audio)))

(declare-function tlon-md-get-tag-pattern "tlon-md")
(declare-function tlon-api-get-citation "tlon-api")
(defun tlon-bib-do-replace-keys-with-citations (&optional audio)
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

(defun tlon-bib-replace-keys-with-citations-in-dir (&optional audio)
  "Recursively replace all BibTeX keys with CSL-defined citations in current dir.

By default, export citations in \"short\" or \"long\" format depending on what
the tag specified. If AUDIO is non-nil, export all citations in \"audio\" format
instead."
  (interactive)
  (let ((files (directory-files-recursively default-directory "\\.md$")))
    (dolist (file files)
      (tlon-bib-replace-keys-with-citations file audio))))

(defun tlon-bib-find-next-entry-with-missing-field (field)
  "For each entry in the current BibTeX buffer, check if FIELD’s value is missing."
  (interactive (list (completing-read "Field: " bibtex-extras-biblatex-fields nil t)))
  (bibtex-next-entry)
  (while (or (bibtex-extras-get-field field)
	     (bibtex-extras-get-field "crossref"))
    (bibtex-next-entry)))

;;;;;; Citation replacement (batching)

(defun tlon-bib--replace-citations-in-files (files &optional label)
  "Replace citations in FILES using the AI agent, batching requests.

FILES is a list of file paths. LABEL, when non-nil, is used for progress
messages.

If the number of FILES exceeds `tlon-bib-replace-citations-max-concurrent',
process them in batches of that size, starting the next batch only after all
requests in the current batch have finished."
  (let* ((files (delq nil (mapcar #'expand-file-name files)))
	 (pending (copy-sequence files))
	 (batch-size (max 1 tlon-bib-replace-citations-max-concurrent))
	 (batch-num 0)
	 (label (or label "file")))
    (unless files
      (user-error "No files selected"))
    (cl-labels
	((start-next-batch ()
	   (when pending
	     (cl-incf batch-num)
	     (let* ((batch (cl-subseq pending 0 (min batch-size (length pending))))
		    (remaining (nthcdr (length batch) pending))
		    (done 0)
		    (total (length batch))
		    (failures 0))
	       (setq pending remaining)
	       (message "Starting batch %d (%d %s%s)..."
			batch-num total label (if (= total 1) "" "s"))
	       (dolist (file batch)
		 (tlon-bib--replace-citations-in-file
		  file
		  (lambda (_file ok)
		    (cl-incf done)
		    (unless ok (cl-incf failures))
		    (when (= done total)
		      (message "Finished batch %d (%d ok, %d failed)."
			       batch-num (- total failures) failures)
		      (start-next-batch)))))))))
      (message "Processing %d %s%s (batch size: %d)..."
	       (length files) label (if (= (length files) 1) "" "s") batch-size)
      (start-next-batch))))

;;;###autoload
(defun tlon-bib-check-bibkeys (&optional file)
  "Check if all BibTeX keys in FILE are valid.
If FILE is not provided, use the file visited by the current buffer. If a
region is selected, check only the region."
  (interactive)
  (let* ((text (if (use-region-p)
		   (buffer-substring-no-properties (region-beginning) (region-end))
		 (let ((filename (or file (buffer-file-name))))
		   (if filename
		       (with-temp-buffer
			 (insert-file-contents filename)
			 (buffer-string))
		     (buffer-string)))))
	 (pattern (tlon-md-get-tag-pattern "Cite"))
	 invalid-keys)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
	(let ((key (match-string-no-properties 3)))
	  (when key
	    (unless (tlon-bibliography-lookup "=key=" key)
	      (push key invalid-keys))))))
    (if invalid-keys
	(message "Invalid BibTeX keys found: %s" (mapconcat #'identity (delete-dups invalid-keys) ", "))
      (message "All BibTeX keys are valid."))))

;;;;; Abstracts

;;;;;; Translations

(defun tlon-read-abstract-translations (&optional code-or-language)
  "Read abstract translations for CODE-OR-LANGUAGE as an alist (KEY . TEXT).
CODE-OR-LANGUAGE may be a two-letter code (e.g., \"es\") or a standard language
name (e.g., \"spanish\")."
  (tlon-read-json (tlon-bib--abstracts-file code-or-language) 'alist 'list 'string))

(defun tlon-write-abstract-translations (data &optional code-or-language)
  "Write DATA (alist of (KEY . TEXT)) to the store for CODE-OR-LANGUAGE.
CODE-OR-LANGUAGE may be a two-letter code (e.g., \"es\") or a standard language
name (e.g., \"spanish\")."
  (let* ((file (tlon-bib--abstracts-file code-or-language))
         (dir  (file-name-directory file)))
    (when (and (stringp dir) (not (file-directory-p dir)))
      (make-directory dir t))
    (tlon-write-data file data)))

(defun tlon-bib--abstracts-file (code-or-language)
  "Return the abstract-translations.json path for CODE-OR-LANGUAGE.
CODE-OR-LANGUAGE may be a two-letter code or a standard language name."
  (let* ((base (tlon-bib--abstracts-base-dir))
         (code (tlon-bib--ensure-lang-code code-or-language)))
    (file-name-concat base code "abstract-translations.json")))

(defun tlon-bib--abstracts-base-dir ()
  "Return the base directory for per-language abstract translations.

This resolves to the \"json\" directory inside the babel-refs repo."
  (let ((repo (tlon-repo-lookup :dir :name "babel-refs")))
    (unless repo
      (user-error "Could not locate babel-refs repo"))
    (file-name-concat repo "json")))

(defun tlon-bib--ensure-lang-code (code-or-language)
  "Return a two-letter code for CODE-OR-LANGUAGE.
Accepts either a language CODE (e.g., \"es\") or a standard language NAME
\\=(e.g., \"spanish\"). Signals a `user-error' if it cannot be resolved."
  (unless (and code-or-language
               (stringp code-or-language)
               (not (string-blank-p code-or-language)))
    (user-error "Language code or name is required"))
  (let* ((value (downcase code-or-language))
         (as-code (tlon-lookup tlon-languages-properties :code :code value))
         (code (or as-code
                   (tlon-lookup tlon-languages-properties :code :standard value))))
    (unless code
      (user-error "Unrecognized language: %s" code-or-language))
    code))

(defun tlon-add-abstract-translation (key target-lang translation &optional overwrite var)
  "Add TRANSLATION for KEY in per-language store TARGET-LANG.
If a translation already exists, do nothing unless OVERWRITE is non-nil. If KEY
is not present, add a new entry for this KEY. When VAR is non-nil, update that
variable (alist of (KEY . TEXT)) instead of writing to disk."
  (let* ((data (or (and var (symbol-value var))
                   (tlon-read-abstract-translations target-lang)))
         (cell (assoc key data)))
    (if cell
        (when overwrite
          (setcdr cell translation))
      (push (cons key translation) data))
    (if var
        (set var data)
      (tlon-write-abstract-translations data target-lang))))

;;;###autoload
(defun tlon-get-abstract-translation (bibkey code &optional translations)
  "Return the translated abstract TEXT for BIBKEY in language CODE.
If TRANSLATIONS is nil, read the per-language store for CODE. Return nil if not
found."
  (let* ((translations (or translations (tlon-read-abstract-translations code)))
         (cell (assoc bibkey translations)))
    (and cell (cdr cell))))

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
	      (key (or key (tlon-get-key-at-point)))
	      (abstract (funcall get-field "abstract"))
	      (source-lang (or source-lang
			       (tlon-lookup tlon-languages-properties :code :name (funcall get-field "langid"))))
	      (target-lang (or target-lang (tlon-select-language 'code 'babel))))
    (tlon-deepl-translate (tlon-bib-remove-braces abstract) target-lang source-lang
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
  (tlon-ensure-bibtex)
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
is non-nil, save the translations in VAR; otherwise, save them in the
per-language JSON file."
  (let ((translation (tlon-deepl-print-translation)))
    (tlon-add-abstract-translation key target-lang translation overwrite var)
    (when (null var)
      (tlon-bib-unescape-escaped-characters (tlon-bib--abstracts-file target-lang)))))

;;;;;; Report

;;;###autoload
(defun tlon-bib-entries-report ()
  "Return a report about the BibTeX buffer at point."
  (interactive)
  (tlon-bib-load-excluded-keys)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (let (no-abstract short-abstracts long-abstracts minimal-abstracts)
      (while (bibtex-next-entry)
	(bibtex-narrow-to-entry)
	(let ((key (bibtex-extras-get-key)))
	  (unless (or (member key tlon-bib-excluded-keys)
		      (bibtex-extras-get-field "crossref"))
	    (if (re-search-forward "^[[:blank:]]+abstract = {" nil t)
		(let* ((begin (point))
		       (end (progn (bibtex-next-field t) (beginning-of-line) (left-char 3) (point))))
		  (cond ((> (count-words begin end) tlon-max-abstract-length)
			 (push key long-abstracts))
			((< (count-words begin end) 10)
			 (push key minimal-abstracts))
			((< (count-words begin end) tlon-min-abstract-length)
			 (push key short-abstracts))))
	      (push key no-abstract)))
	  (widen)))
      (setq long-abstracts (tlon-bibt-remove-translated-entries long-abstracts)
	    short-abstracts (tlon-bibt-remove-translated-entries short-abstracts)
	    minimal-abstracts (tlon-bibt-remove-translated-entries minimal-abstracts)
	    no-abstract (tlon-bibt-remove-translated-entries no-abstract))
      (let ((buffer-name "*tlon-bib-entries-report*"))
	(with-current-buffer (get-buffer-create buffer-name)
	  (erase-buffer)
	  (insert (format "Abstracts above maximum length (%s words): %s

Abstracts below minimum length (%s words): %s

Abstracts with fewer than 10 words: %s

No abstract: %s"
			  tlon-max-abstract-length long-abstracts
			  tlon-min-abstract-length short-abstracts
			  minimal-abstracts
			  no-abstract))
	  (pop-to-buffer buffer-name))))))

(declare-function citar-extras-open-in-ebib "citar-extras")
(defun tlon-bibt-remove-translated-entries (list)
  "Remove entries in LIST if they are translated entries."
  (let (new-list)
    (dolist (key list new-list)
      (save-window-excursion
	(citar-extras-open-in-ebib key)
	(unless (ebib-extras-get-field "translation")
	  (push key new-list))))))

(defun tlon-bib-save-excluded-keys ()
  "Save the excluded citation keys to the file."
  (with-temp-file tlon-bib-excluded-keys-file
    (insert ";;; Tlon excluded keys -*- lexical-binding: t -*-\n\n")
    (insert ";; This file contains the keys to be excluded from `tlon-bib-entries-report'.\n\n")
    (insert "(setq tlon-bib-excluded-keys '")
    (prin1 tlon-bib-excluded-keys (current-buffer))
    (insert ")\n\n(provide 'tlon-bib-excluded-keys)")))

(defun tlon-bib-load-excluded-keys ()
  "Load the excluded keys from the file."
  (let ((file tlon-bib-excluded-keys-file))
    (when (file-exists-p file)
      (load file))))

(defun tlon-bib-add-to-excluded-keys (string)
  "Add STRING of key symbols to the list of keys no needing an abstract."
  (interactive (list (read-string "Key(s): " (thing-at-point 'word))))
  (tlon-bib-load-excluded-keys)
  (dolist (key (split-string string))
    (push key tlon-bib-excluded-keys))
  (tlon-bib-save-excluded-keys))

;;;;; Monitor field values

(declare-function tlon-translate-abstract "tlon-deepl")
(defun tlon-bib-field-modified (field value key)
  "Act when FIELD is set to VALUE in a BibTeX entry with KEY."
  (cond ((string= field "abstract")
	 (tlon-translate-abstract value key))))

(defun tlon-bib-remove-braces (string)
  "Remove braces from STRING."
  (when string
    (replace-regexp-in-string "[{}]" "" string)))

(defun tlon-bib-bibtex-set-field-advice (orig-fun &rest args)
  "Advice to catch modifications to BibTeX fields in `bibtex-mode'.
ORIG-FUN is the original function, ARGS are the arguments passed to it."
  (let ((field (nth 0 args))
	(value (nth 1 args)))
    (apply orig-fun args)
    (tlon-bib-field-modified field value (bibtex-extras-get-key))))

;; (advice-add 'bibtex-set-field :around #'tlon-bib-bibtex-set-field-advice)

(defun tlon-bib-ebib-set-field-advice (orig-fun &rest args)
  "Advice to catch modifications to BibTeX fields in `ebib-entry-mode'.
ORIG-FUN is the original function, ARGS are the arguments passed to it."
  (let ((field (nth 0 args))
	(value (nth 1 args)))
    (apply orig-fun args)
    (tlon-bib-field-modified field value (ebib--get-key-at-point))))

;; (advice-add 'ebib-set-field-value :around #'tlon-bib-ebib-set-field-advice)

;;;;; Misc

(declare-function files-extras-lines-to-list "file-extras")
(declare-function zotra-extras-add-multiple-urls "zotra-extras")
(defun tlon-bib-add-multiple-urls-from-file (file)
  "Prompt user for FILE with a list of URLs and add each to `tlon-file-fluid'."
  (let* ((urls (delete-dups (files-extras-lines-to-list file)))
	 (missing-urls (seq-filter (lambda (url)
				     (not (tlon-bibliography-lookup "url" url)))
				   urls)))
    (zotra-extras-add-multiple-urls missing-urls tlon-file-fluid)))

(declare-function zotra-extras-add-entry "zotra-extras")
;;;###autoload
(defun tlon-create-bibtex-original ()
  "Add a new original BibTeX entry."
  (interactive)
  (zotra-extras-add-entry nil nil tlon-file-fluid))

;;;;; Bibliographic Reference Extraction

(defconst tlon-bib-get-bibkeys-prompt
  "You are an expert bibliographic database lookup tool. You will be given an 'Input Reference' string and a 'Database' in JSON format containing bibliographic entries.\n\nYour task is to find the *single best matching entry* in the Database for the Input Reference. The match should be based on semantic similarity (author, title, year), even if the strings are not identical. \n\nOnce you find the best match, you must return *only* the value of the 'key' field for that matching entry. Do not return anything else - no explanations, no 'The key is:', just the key string itself.\n\nIf you cannot find a reasonably good match in the Database, return the exact string 'NOT_FOUND'.\n\nInput Reference:\n%s\n\nDatabase:\n```json\n%s\n```\n\nKey:"
  ;; %s will be the single input reference
  ;; %s will be the single input reference
  ;; %s will be the JSON database string
  "Prompt for finding a BibTeX key for a single reference against a JSON database.")

(defconst tlon-bib-extract-exact-references-prompt
  (format "You are an expert academic assistant. Please carefully scan the following text and extract all bibliographic references you can find.%s Return each distinct reference *exactly* as it appears in the text, including all original punctuation and spacing. Each reference should be on a new line. Do not include any commentary, numbering, or bullet points, just the exact reference strings themselves."
	  tlon-ai-string-wrapper)
  "Prompt for extracting bibliographic references exactly as found.")

(defconst tlon-bib-get-bibkeys-batch-prompt
  "You are an expert bibliographic database lookup tool. You will be given a list of 'Input References' (one per line) and a 'Database' in JSON format containing bibliographic entries.\n\nYour task is to find the *single best matching entry* in the Database for *each* Input Reference. The match should be based on semantic similarity (author, title, year), even if the strings are not identical.\n\nReturn a list of keys, one per line, corresponding *exactly* to the order of the Input References. For each Input Reference:\n- If you find a good match, return the value of the 'key' field for that entry.\n- If you cannot find a reasonably good match, return the exact string 'NOT_FOUND'.\n\nDo not return anything else - no explanations, no numbering, just the list of keys (or 'NOT_FOUND'), one per line.\n\nInput References:\n```\n%s\n```\n\nDatabase:\n```json\n%s\n```\n\nKeys:"
  ;; %s will be the newline-separated list of input references
  ;; %s will be the JSON database string
  "Prompt for finding BibTeX keys for multiple references against a JSON database.")

;;;;; Citation Replacement

(defconst tlon-bib-replace-citations-prompt
  "You are an expert academic editor. Your task is to process a text file, identify all bibliographic citations within it, and replace them with structured citation tags.

Here is the process you must follow:
1. Locate all the citations in the text provided at the end of these instructions. The citations may appear in a variety of different formats. Some examples: 'Hutchinson (2021)', 'Smith, 2019', 'see Jones et al., forthcoming', 'Nick Bostrom (2019) [The vulnerable world hypothesis](https://doi.org/10.1111/1758-5899.12718), *Global Policy*, vol. 10, pp. 455–476', '[Smith KF, Sax DF, Lafferty KD. Evidence for the role of infectious disease in species extinction and endangerment. Conserv Biol. 2006 Oct;20(5):1349-57.]', etc. You should use your common sense to determine whether a string is or isn't a citation.
2. Note: some citations may have already been processed. These citations have the format `%1$s`. You should completely IGNORE all of these citations: just leave them as they are.
3. For each citation you find, determine if the cited works exist using the `search_bibliography` tool. This tool will return the unique BibTeX key for the entry if a match is found.
4. If a citation includes a link to a web page, you may need to use the `fetch_content` and `search` tools to identify the work before looking it up in the bibliography. NB: you don't need to open every link you find in the text; you only need to open links that are part of a citation.
5. Once you have the BibTeX key (e.g., 'Hutchinson2021WhatGivesMe'), you must replace the original citation string in the text with the format `%1$s`. For example, replace 'Hutchinson (2021)' with %s If the original citation was enclosed in parentheses or square brackets, keep those delimiters and replace only the citation string inside them. For example, replace '(Finney 2008)' with '%s'. If the citation included a link, do not keep it. For example, replace 'Nick Bostrom (2019) [The vulnerable world hypothesis](https://doi.org/10.1111/1758-5899.12718), *Global Policy*, vol. 10, pp. 455–476' with '%s'.%s
7. Occasionally, when the citation intends to refer to a specific part of a work, it will contain a 'locator'. In these cases, you should include the locator within the citation. For example, replace 'Hutchinson (2021): pp. 822–824' with '%5$s'. Sometimes the abbreviation of the locator specifying the entity being referred to (‘pp.', 'ch.', etc) may be omitted. In these cases, you should use your judgment to figure out what abbreviation should be inserted as part of the locator. For example, replace 'Hutchinson (2021): 822–824' with '%5$s'. The main locators are: \"book\" (\"bk.\"), \"chapter \" (\"chap.\"), \"figure\" (\"fig.\"), \"note\" (\"n.\"), \"page\" (\"p.\"), \"section\" (\"sec.\"), and \"volume\" (\"vol.\"); and the corresponding plurals: \"books\" (\"bks.\"), \"chapter \" (\"chaps.\"), \"figures\" (\"figs.\"), \"notes\" (\"nn.\"), \"pages\" (\"pp.\"), \"sections\" (\"secs.\"), and \"volumes\" (\"vols.\").
8. Enclose those citation that you are unable to find in my bibliography between '{!' and '!}', so that they can be easily identified later. For example, replace 'Smith, 2019' with '{!Smith, 2019$}'. NOTE: if a citation you can't find includes a link, do NOT remove it. For example, if you can't find 'Ajay K. Kohli & Bernard J. Jaworski (1990) [Market orientation: the construct, research propositions, and managerial implications](https://doi.org/10.1177/002224299005400201), *Journal of Marketing*, vol. 54, pp. 1–18', repalce it with '{!Ajay K. Kohli & Bernard J. Jaworski (1990) [Market orientation: the construct, research propositions, and managerial implications](https://doi.org/10.1177/002224299005400201), *Journal of Marketing*, vol. 54, pp. 1–18!}'.
8. After identifying all citations and their keys, use the `edit_file` tool to apply all the replacements to `%%s', which is the file from which the text was taken. You should perform all edits in a single operation if possible.

  Here is the text:

%%s"
  "Prompt for replacing citations with BibTeX keys.")

(defconst tlon-bib-replace-citations-prompt-examples
  '(("<Cite bibKey=\"KEY\" />" . "[cite:@KEY]")
    ("'<Cite bibKey=\"Hutchinson2021WhatGivesMe\" />'. (Note that there is a space between the final quote and the forward slash.)" . "'[cite:@Hutchinson2021WhatGivesMe]'.")
    ("(<Cite bibKey=\"Finney2008NamingBeliefs\" />)" . "[cite:@Finney2008NamingBeliefs]")
    ("<Cite bibKey=\"Bostrom2019VulnerableWorldHypothesis\" />" . "[cite:@Bostrom2019VulnerableWorldHypothesis]")
    ("<Cite bibKey=\"Hutchinson2021WhatGivesMe\" locator=\"pp. 822–824\" />" . "[cite:@Hutchinson2021WhatGivesMe, pp. 822–824]")
    ("" . " Sometimes there are two or more consecutive citations, separated by ';' or some other delimiter. Do don’t need to write the entire citation format for each of them. Instead, write '[cite:@KEY1;@KEY2;etc]'. For example, '[cite:@AgainstMalariaFoundation2021NetDistributionsWorld]'."))
  "List of cons cells with examples for Mardown and Org-mode citation replacement.")

(defconst tlon-bib-add-missing-citations-prompt
  "You are an expert academic assistant. Your task is to process a text and add missing bibliographic entries to a BibTeX file.

Here is the process you must follow:
1. In the text provided at the end of these instructions, find all citations enclosed in `{!` and `!}`.
2. For each citation found, you must find a unique identifier for the work, such as a URL, DOI, or ISBN. The citation string itself may contain it. If not, you must use the `search` tool to find one online.
3. When you succeed in finding an identifier, use the `add_bib_entry` tool to add a new entry to the bibliography file. The `bibfile` parameter for this tool MUST be '%s'. The `identifier` parameter should be the URL, DOI, or ISBN you found. Rarely, `add_bib_ebtry` may fail to add the work. In these cases, just proceed to the next missing citation.
4. After adding the entry, replace the original citation string in the text with the format `<Cite bibKey=\"KEY\" />`, where `KEY` is the BibTeX key generated by the `add_bib_entry` tool. This key is returned by 'add_bib_entry' after adding the entry. For example, if they key corresponding to '{!Smith, 2019$}' is 'Liu2021Covid19And', you should replace '{!Smith, 2019$}' with '<Cite bibKey=\"Smith2019EarlyCovidVaccines\" />'.
5. If you do not find a unique identifier for a citation, you must leave it as is. Do NOT try to find an alternative work or entry to replace it with. Just leave the citation as '{!CITATION$}'.
6. After processing all missing citations, use the `edit_file` tool to apply all the replacements to `%s', which is the file from which the text was taken. You should perform all edits in a single operation if possible.

Here is the text: %s"
  "Prompt for adding missing citations to the bibliography.")

;;;;;; Bibkey Lookup Command and Helpers

(defvar tlon-bib--bibkey-state nil
  "Internal state variable for asynchronous bibkey lookup.")

(defun tlon-bib--batch-bibkey-result-handler (response info)
  "Callback function to handle the result of a batch bibkey lookup.
Parses the newline-separated keys, associates them with original references, and
triggers replacements. RESPONSE is the AI's response, INFO is the response info."
  (cl-block tlon-bib--batch-bibkey-result-handler
    (let* ((state tlon-bib--bibkey-state)
	   (references-with-pos (plist-get state :references-with-pos))
	   (num-references (length references-with-pos))
	   (results '())) ; Build the results list here

      (unless response
	(message "AI batch request failed. Status: %s" (plist-get info :status))
	(setq tlon-bib--bibkey-state nil) ; Clean up state
	(cl-return-from tlon-bib--batch-bibkey-result-handler))

      (let ((returned-keys (split-string (string-trim response) "\n" t)))
	(unless (= (length returned-keys) num-references)
	  (message "Error: AI returned %d keys, but %d references were sent. Aborting replacements."
		   (length returned-keys) num-references)
	  (message "AI Response:\n%s" response) ; Log response for debugging
	  (setq tlon-bib--bibkey-state nil) ; Clean up state
	  (cl-return-from tlon-bib--batch-bibkey-result-handler))

	;; Associate keys with positions
	(dotimes (i num-references)
	  (let* ((entry (nth i references-with-pos))
		 (start-pos (nth 1 entry))
		 (end-pos (nth 2 entry))
		 (key (nth i returned-keys)))
	    (push (list start-pos end-pos key) results)))

	;; Store the final results (reversed to match original order implicitly)
	(setf (plist-get state :results) (nreverse results))
	(setq tlon-bib--bibkey-state state) ; Update state with results

	;; All references processed, apply replacements
	(tlon-bib--apply-bibkey-replacements)))))

(defun tlon-bib--apply-bibkey-replacements ()
  "Apply the BibTeX key replacements in the source buffer."
  (cl-block tlon-bib--apply-bibkey-replacements
    (let* ((state tlon-bib--bibkey-state)
	   (results (plist-get state :results)) ; List of (start end key)
	   (source-buffer (plist-get state :source-buffer))
	   (replacements-made 0)
	   (errors-occurred 0))

      (unless (buffer-live-p source-buffer)
	(message "Source buffer is no longer live. Aborting replacements.")
	(setq tlon-bib--bibkey-state nil)
	(cl-return-from tlon-bib--apply-bibkey-replacements))

      ;; Sort results by start position in REVERSE order to avoid messing up positions
      (setq results (sort results (lambda (a b) (> (car a) (car b)))))

      (with-current-buffer source-buffer
	(dolist (result results)
	  (let ((start (nth 0 result))
		(end (nth 1 result))
		(key (nth 2 result)))
	    ;; Check if key is valid (not an error marker)
	    (if (or (string= key "NOT_FOUND") (string= key "ERROR_AI"))
		(progn
		  (message "No valid key found for text at %d-%d (Result: %s). Skipping." start end key)
		  (cl-incf errors-occurred))
	      ;; Perform replacement
	      (let ((replacement-text (format "<Cite bibKey=\"%s\" />" key)))
		(goto-char start) ; Go to start before deleting
		(delete-region start end)
		(insert replacement-text)
		(cl-incf replacements-made))))))

      (message "BibTeX key replacement complete. Replaced %d reference(s). Skipped %d due to errors or no match."
	       replacements-made errors-occurred)
      ;; Clean up state variable
      (setq tlon-bib--bibkey-state nil))))

;;;;;; Extract and Replace Command

(defvar tlon-bib--extract-replace-state nil
  "Internal state variable for asynchronous reference extraction and replacement.")

(defun tlon-bib--extract-references-exact-callback (response info)
  "Callback for the initial reference extraction.
Finds positions and starts key lookup. RESPONSE is the AI's response, INFO is
the response info."
  (if (not response)
      (progn
	(setq tlon-bib--extract-replace-state nil) ; Clean up state
	(tlon-ai-callback-fail info))
    (let* ((state tlon-bib--extract-replace-state)
	   (extracted-refs (split-string (string-trim response) "\n" t)))
      (setf (plist-get state :extracted-references) extracted-refs)
      (message "AI extracted %d potential references. Finding positions..." (length extracted-refs))
      (setf (plist-get state :reference-positions)
	    (tlon-bib--find-reference-positions
	     extracted-refs
	     (plist-get state :source-buffer)
	     (plist-get state :region-start)
	     (plist-get state :region-end)))
      (let ((unique-refs (cl-delete-duplicates
			  (mapcar #'car (plist-get state :reference-positions)) :test #'string=)))
	(setf (plist-get state :unique-references) unique-refs)
	(setf (plist-get state :keys-to-fetch) (length unique-refs))
	(setf (plist-get state :key-map) (make-hash-table :test 'equal))
	(if (zerop (plist-get state :keys-to-fetch))
	    (progn
	      (message "No references found or positions located.")
	      (setq tlon-bib--extract-replace-state nil)) ; Clean up
	  (message "Found positions for %d unique references. Requesting BibTeX keys in batch..." (length unique-refs))
	  (tlon-bib--get-keys-for-extracted-references)))))) ; Start batch key lookup

(defun tlon-bib--find-reference-positions (references buffer beg end)
  "Search for occurrences of REFERENCES strings within BUFFER between BEG and END.
Returns an alist: (ref-string . list-of-(start . end))."
  (let ((positions-alist '()))
    (with-current-buffer buffer
      (dolist (ref references positions-alist)
	(let ((ref-positions '()))
	  (save-excursion
	    (goto-char end) ; Start searching backwards from the end
	    (while (search-backward ref beg t)
	      (let* ((match-start (match-beginning 0))
		     (match-end (match-end 0))
		     (line-start (line-beginning-position))
		     (adjusted-start match-start))
		;; Check if the match is preceded by a footnote marker at line start
		(save-excursion
		  (goto-char match-start)
		  (when (and (= (point) line-start) ; Ensure match starts exactly at line beginning
			     (looking-at "\\[\\^[0-9]+\\]: "))
		    ;; If marker found, adjust start position past the marker
		    (setq adjusted-start (match-end 0))))
		(push (cons adjusted-start match-end) ref-positions))))
	  (when ref-positions
	    (push (cons ref ref-positions) positions-alist)))))))

(defun tlon-bib--get-keys-for-extracted-references ()
  "Initiate a single batch request to get BibTeX keys for extracted references."
  (let* ((state tlon-bib--extract-replace-state)
	 (unique-refs (plist-get state :unique-references))
	 (db-string (plist-get state :db-string)))
    (if (null unique-refs)
	(progn
	  (message "No unique references to look up keys for.")
	  (setq tlon-bib--extract-replace-state nil)) ; Clean up state
      (let* ((references-block (mapconcat #'identity unique-refs "\n"))
	     (prompt (format tlon-bib-get-bibkeys-batch-prompt references-block db-string)))
	;; Make the single batch request
	(tlon-make-gptel-request prompt nil #'tlon-bib--extracted-batch-bibkey-result-handler nil t)))))

(defun tlon-bib--extracted-batch-bibkey-result-handler (response info)
  "Callback to handle the result of batch bibkey lookup for extracted references.
Parses the newline-separated keys, populates the key-map, and triggers
replacements. RESPONSE is the AI's response, INFO is the response info."
  (let* ((state tlon-bib--extract-replace-state)
	 (unique-refs (plist-get state :unique-references))
	 (key-map (plist-get state :key-map)) ; Hash table: ref-string -> key
	 (num-references (length unique-refs)))

    (if response
	;; Process valid response
	(let ((returned-keys (split-string (string-trim response) "\n" t)))
	  (if (= (length returned-keys) num-references)
	      ;; Correct number of keys returned
	      (progn
		;; Populate the key-map hash table
		(dotimes (i num-references)
		  (let ((ref-string (nth i unique-refs))
			(key (nth i returned-keys)))
		    (puthash ref-string key key-map)))
		(message "All keys fetched via batch request. Applying replacements...")
		(tlon-bib--apply-extracted-reference-replacements)) ; Proceed to replacements
	    ;; Incorrect number of keys returned
	    (message "Error: AI returned %d keys, but %d unique references were sent. Aborting replacements."
		     (length returned-keys) num-references)
	    (message "AI Response:\n%s" response) ; Log response for debugging
	    (setq tlon-bib--extract-replace-state nil))) ; Clean up state
      ;; Handle failed AI request
      (message "AI batch key lookup request failed. Status: %s" (plist-get info :status))
      (setq tlon-bib--extract-replace-state nil)))) ; Clean up state

(defun tlon-bib--apply-extracted-reference-replacements ()
  "Apply the BibTeX key replacements in the source buffer for extracted references."
  (cl-block tlon-bib--apply-extracted-reference-replacements
    (let* ((state tlon-bib--extract-replace-state)
	   ;; Check if state is nil (might have been cleaned up due to error)
	   (_ (unless state (user-error "State lost, likely due to previous error. Aborting")))
	   (source-buffer (plist-get state :source-buffer))
	   (ref-positions (plist-get state :reference-positions)) ; (ref-string . list-of-(start . end))
	   (key-map (plist-get state :key-map))
	   (replacements '()) ; List of (start end replacement-text)
	   (replacements-made 0)
	   (errors-occurred 0)
	   (not-found-count 0))
      (unless (buffer-live-p source-buffer)
	(message "Source buffer is no longer live. Aborting replacements.")
	(setq tlon-bib--extract-replace-state nil)
	(cl-return-from tlon-bib--apply-extracted-reference-replacements))
      ;; Build the list of replacements
      (dolist (pos-entry ref-positions)
	(let* ((ref-string (car pos-entry))
	       (positions (cdr pos-entry))
	       (key (gethash ref-string key-map "ERROR_AI"))) ; Default to error if somehow missing
	  (if (or (string= key "NOT_FOUND") (string= key "ERROR_AI"))
	      (progn
		(when (string= key "NOT_FOUND") (cl-incf not-found-count))
		(when (string= key "ERROR_AI") (cl-incf errors-occurred)))
	    ;; Valid key found, create replacement entries for all found positions
	    (let ((replacement-text (format "<Cite bibKey=\"%s\" />" key)))
	      (dolist (pos positions)
		(push (list (car pos) (cdr pos) replacement-text) replacements))))))
      ;; Sort replacements by start position in REVERSE order
      (setq replacements (sort replacements (lambda (a b) (> (car a) (car b)))))
      ;; Apply replacements
      (with-current-buffer source-buffer
	(dolist (replacement replacements)
	  (let ((start (nth 0 replacement))
		(end (nth 1 replacement))
		(text (nth 2 replacement)))
	    ;; Check if the region still contains the expected text? (Might be too complex/slow)
	    (goto-char start)
	    (delete-region start end)
	    (insert text)
	    (cl-incf replacements-made))))
      (message "Reference replacement complete. Replaced %d instance(s). Skipped %d (key not found), %d (AI error)."
	       replacements-made not-found-count errors-occurred)
      ;; Clean up state variable
      (setq tlon-bib--extract-replace-state nil))))

;;;;;; Citation Replacement (AI Agent)

(defun tlon-bib--replace-citations-in-file (file &optional done-callback)
  "Replace citations in FILE with <Cite> tags using the AI agent.
When DONE-CALLBACK is non-nil, call it with two arguments (FILE OK).
OK is non-nil when the request finishes without error (i.e., when we receive a
non-nil response)."
  (let* ((org-file-p (string-suffix-p ".org" file t))
	 (examples (tlon-bib-get-citation-replacement-prompt-examples org-file-p))
	 (prompt-template (apply #'format tlon-bib-replace-citations-prompt examples))
	 (prompt (format prompt-template file
			 ;; return file contents
			 (with-temp-buffer
			   (insert-file-contents file)
			   (buffer-string))))
	 (tools '("search_bibliography" "fetch_content" "search" "edit_file" "apply_diff" "replace_file_contents"))
	 (callback
	  (lambda (response info)
	    (tlon-bib-replace-citations-callback response info)
	    (when done-callback
	      (funcall done-callback file (not (null response)))))))
    (unless (file-exists-p file)
      (user-error "File does not exist: %s" file))
    (message "Requesting AI to process citations in %s..." (file-name-nondirectory file))
    (tlon-make-gptel-request prompt nil callback
			     tlon-bib-replace-citations-model t nil tools nil '("ddg-search"))))

(declare-function dired-get-marked-files "dired")
;;;###autoload
(defun tlon-bib-replace-citations-in-file (&optional file)
  "Use AI to find and replace bibliographic citations with citation tags.
When called from Dired with marked files, process all marked files.

When called from a buffer visiting a file with an active region, process the
current file (the agent will still see the full file contents).

Otherwise, prompt for a file.

FILE, when non-nil, is processed directly."
  (interactive)
  (let ((files
	 (cond
	  (file (list file))
	  ((and (derived-mode-p 'dired-mode) (fboundp 'dired-get-marked-files))
	   (dired-get-marked-files nil nil))
	  ((region-active-p)
	   (list (or (buffer-file-name) (user-error "Buffer is not visiting a file"))))
	  (t
	   (list (read-file-name "File to process: " nil nil nil
				 (file-relative-name (buffer-file-name) default-directory)))))))
    (tlon-bib--replace-citations-in-files files "file")))

(defun tlon-bib-get-citation-replacement-prompt-examples (org)
  "Return a list of examples for replacing citations.
If ORG is non-nil, return a list of Org-mode citation replacement examples."
  (mapcar (lambda (cons)
	    (funcall (if org #'cdr #'car) cons))
	  tlon-bib-replace-citations-prompt-examples))

(defun tlon-bib-replace-citations-callback (response info)
  "Callback for `tlon-bib-replace-citations-in-file'.
RESPONSE is the AI's response, INFO is the response info.
This function primarily exists to confirm that the AI agent has finished its
task, as the file modifications are expected to be done via tools."
  (unless response
    (tlon-ai-callback-fail info)))

;;;###autoload
(defun tlon-bib-replace-citations-in-org-files-in-directory (directory)
  "Run `tlon-bib-replace-citations-in-file' on each .org file in DIRECTORY.
Files are processed in non-recursive batches of size
`tlon-bib-replace-citations-max-concurrent'. The next batch is started only
after all requests in the current batch have finished."
  (interactive "DDirectory: ")
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
	 (files (directory-files directory t "\\.org\\'")))
    (unless files
      (user-error "No .org files found in %s" directory))
    (tlon-bib--replace-citations-in-files files "org file")))

;;;###autoload
(defun tlon-bib-add-missing-citations ()
  "Use AI to add missing citations in a file to the bibliography.
This command prompts for a file and then instructs an AI agent to find all
citations enclosed in `{!` and `!}`. For each one, it uses the `add_bib_entry`
tool to add an entry to `tlon-file-fluid`."
  (interactive)
  (let* ((file (if (region-active-p)
		   (buffer-file-name)
		 (read-file-name "File to process: " nil nil nil
				 (file-relative-name (buffer-file-name) default-directory))))
	 (prompt (format tlon-bib-add-missing-citations-prompt
			 tlon-file-fluid
			 file
			 (if (region-active-p)
			     (buffer-substring-no-properties (region-beginning) (region-end))
			   (with-temp-buffer
			     (insert-file-contents file)
			     (buffer-string)))))
	 (tools '("add_bib_entry" "search" "edit_file" "apply_diff" "replace_file_contents")))
    (unless (file-exists-p file)
      (user-error "File does not exist: %s" file))
    (message "Requesting AI to add missing citations from %s..." (file-name-nondirectory file))
    (tlon-make-gptel-request prompt nil #'tlon-bib-add-missing-citations-callback tlon-bib-add-missing-citations-model t nil tools)))

(defun tlon-bib-add-missing-citations-callback (response info)
  "Callback for `tlon-bib-add-missing-citations'.
RESPONSE is the AI's response, INFO is the response info."
  (if response
      (message "AI agent finished processing missing citations.")
    (tlon-ai-callback-fail info)))

;;;;;; Model selection

(transient-define-infix tlon-bib-infix-select-replace-citations-model ()
  "AI model to use for replacing citations.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-bib-replace-citations-model)

(transient-define-infix tlon-bib-infix-select-add-missing-citations-model ()
  "AI model to use for adding missing citations.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-bib-add-missing-citations-model)

;;;;; Menu

;;;###autoload (autoload 'tlon-bib-menu "tlon-bib" nil t)
(transient-define-prefix tlon-bib-menu ()
  "Menu for `tex' functions."
  [["Markdown"
    "URLs missing from database"
    ("m f" "Find in file"                            tlon-prompt-to-add-missing-urls)
    ""
    "Convert to `Cite'"
    ("m b" "Convert bibliography"                    tlon-convert-bibliography-to-cite)
    ("m l" "Convert links"                           tlon-convert-links-to-cite)
    ""
    "Check"
    ("m c" "Check BibTeX keys"                       tlon-bib-check-bibkeys)
    ""
    "AI"
    ("m r" "Replace citations in file(s)"            tlon-bib-replace-citations-in-file)
    ("m R" "Replace citations in dir"                tlon-bib-replace-citations-in-org-files-in-directory)
    ("m a" "Add missing citations"                   tlon-bib-add-missing-citations)
    ""
    "AI Models"
    ("m -r" "Replace citations"                      tlon-bib-infix-select-replace-citations-model)
    ("m -a" "Add missing citations"                  tlon-bib-infix-select-add-missing-citations-model)]
   ["Ebib"
    ("e a" "Fetch abstract"                          tlon-fetch-and-set-abstract)
    ("e c" "Create translation entry"                tlon-create-bibtex-translation)
    ("e u" "Populate URL"                            tlon-bib-populate-url-field)
    ("e U" "Populate URL in dir"                     tlon-bib-populate-url-fields-in-language)
    ("e J" "Populate journaltitle in dir"            tlon-bib-populate-journaltitle-fields-in-language)]
   ["BibTeX"
    "Report"
    ("b g" "Generate"                                tlon-bib-entries-report)
    ("b x" "Add to not needing abstract"             tlon-bib-add-to-excluded-keys)
    ""
    "Move"
    ("b t" "Move this entry to Tlön database"        tlon-move-entry-to-fluid)
    ("b s" "Move all entries to stable"              tlon-move-all-fluid-entries-to-stable)
    ""
    "Remove"
    ("b d" "Remove URLs when DOI present"            tlon-bib-remove-url-fields-with-doi)
    ("b i" "Remove URLs when ISBN present"           tlon-bib-remove-url-fields-with-isbn)
    ""
    "Modify"
    ("b :" "Swap colon character in title"           tlon-bib-swap-colon-in-title)]])

;;;###autoload
(defun tlon-bib-create-entry-from-markdown ()
  "Create a BibTeX entry for the Markdown file at point from YAML metadata.
The entry is written to `tlon-file-db'. The following fields are populated when
available: title, author (from authors), translator (from translators),
langid (from repo language), date, translation (from original_key). Then the URL
is populated using `tlon-bib-populate-url-field'."
  (interactive)
  (unless (derived-mode-p 'markdown-mode)
    (user-error "Not in a Markdown buffer"))
  (let* ((file (or (buffer-file-name) (user-error "Buffer is not visiting a file")))
         (title (tlon-yaml-get-key "title" file))
         (type (or (tlon-yaml-get-key "type" file) "online"))
         (authors (let ((lst (tlon-yaml-get-list "authors" file)))
                    (tlon-bib--join-names (mapcar #'tlon-bib--reverse-name lst))))
         (translators (let ((lst (tlon-yaml-get-list "translators" file)))
                        (tlon-bib--join-names (mapcar #'tlon-bib--reverse-name lst))))
         (date (tlon-yaml-get-key "date" file))
         (date-iso (tlon-bib--date-iso date))
         (original-key (tlon-yaml-get-key "original_key" file))
         (repo (tlon-get-repo-from-file file t))
         (lang-code (tlon-repo-lookup :language :dir repo))
         (langid (and lang-code
                      (tlon-lookup tlon-languages-properties :standard :code lang-code))))
    (unless title (user-error "YAML field `title' is required"))
    (unless original-key
      (user-error "YAML field `original_key' is required"))
    (let* ((suffix (capitalize lang-code))
           (key (concat original-key suffix))
           (entry-type (downcase (or type "online")))
           (target-file tlon-file-db))
      (with-current-buffer (find-file-noselect target-file)
        (bibtex-mode)
        (widen)
        (goto-char (point-min))
        (when (bibtex-search-entry key)
          (user-error "BibTeX key `%s' already exists in %s"
                      key (file-name-nondirectory target-file)))
        (goto-char (point-max))
        (delete-blank-lines)
        (unless (bolp) (insert "\n"))
        (insert "\n")
        (insert (format "@%s{%s,\n}\n" entry-type key))
        (bibtex-search-entry key)
        (bibtex-narrow-to-entry)
        (when title (bibtex-extras-add-or-update-field "title" (tlon-bib-remove-braces title)))
        (when authors (bibtex-extras-add-or-update-field "author" authors))
        (when translators (bibtex-extras-add-or-update-field "translator" translators))
        (when langid (bibtex-extras-add-or-update-field "langid" langid))
        (when date-iso (bibtex-extras-add-or-update-field "date" date-iso))
        (when original-key (bibtex-extras-add-or-update-field "translation" original-key))
        (tlon-add-or-update-tlon-field)
        (bibtex-clean-entry)
        (widen)
        (save-buffer)
        ;; Populate URL based on repo/language/slug rules
        (bibtex-search-entry key)
        (tlon-bib-populate-url-field)
        (save-buffer))
      (message "Created BibTeX entry `%s' in %s"
               key (file-name-nondirectory tlon-file-db))
      key)))

;;;###autoload
(defun tlon-bib-create-entries-in-dir (&optional dir)
  "Create BibTeX entries for all Markdown files in DIR from YAML metadata.
When DIR is nil, use `default-directory'. Process files in DIR non‑recursively
whose names end with .md or .markdown. For each file, open it and call
`tlon-bib-create-entry-from-markdown'. Continue on errors and report a summary."
  (interactive)
  (let* ((dir (file-name-as-directory (or dir default-directory)))
         (files (cl-remove-if-not
                 #'file-regular-p
                 (append (directory-files dir t "\\.md\\'")
                         (directory-files dir t "\\.markdown\\'"))))
         (created 0)
         (skipped 0)
         (failed 0)
         (failed-items '()))
    (dolist (file files)
      (message "Processing %s..." (file-name-nondirectory file))
      (condition-case err
          (with-current-buffer (find-file-noselect file)
            (if (derived-mode-p 'markdown-mode)
                (progn
                  (tlon-bib-create-entry-from-markdown)
                  (setq created (1+ created)))
              (setq skipped (1+ skipped))))
        (error
         (setq failed (1+ failed))
         (push (file-name-nondirectory file) failed-items)
         (message "Error processing %s: %s"
                  (file-name-nondirectory file)
                  (error-message-string err)))))
    (let ((summary (format "Processed %d files: %d created, %d skipped (non-markdown), %d errors"
                           (length files) created skipped failed)))
      (if (> failed 0)
          (message "%s. Errors in: %s" summary (mapconcat #'identity (nreverse failed-items) ", "))
        (message "%s" summary)))))

;; Helpers (intentionally placed after the caller, per conventions)

(declare-function tlon-yaml-get-key "tlon-yaml")
(declare-function tlon-yaml-get-key-values "tlon-yaml")
(declare-function tlon-yaml-get-list "tlon-yaml")
(declare-function tlon-get-repo-from-file "tlon-core")
(declare-function tlon-get-value-in-entry "tlon-core")
(declare-function bib-reverse-first-last-name "bibtex-extras")


(defun tlon-bib--join-names (names)
  "Join NAMES with \" and \" for BibLaTeX author-like fields."
  (when (and names
             (seq-some (lambda (s)
                         (and (stringp s)
                              (not (string-empty-p (string-trim s)))))
                       names))
    (mapconcat (lambda (s) (string-trim (or s ""))) names " and ")))

(defun tlon-bib--reverse-name (name)
  "Return NAME in \"Last, First …\" form when appropriate.
If NAME already contains a comma or has a single token, return it
unchanged. Otherwise, move the last whitespace-delimited token to
the front and insert a comma."
  (let* ((name (string-trim (or name ""))))
    (cond
     ((or (string-empty-p name)
          (string-match-p "," name))
      name)
     (t
      (let* ((parts (split-string name "[ \t]+" t))
             (last  (car (last parts)))
             (first (string-join (butlast parts) " ")))
        (if (and first (not (string-empty-p first)))
            (format "%s, %s" last first)
          last))))))

(defun tlon-bib--date-year (date)
  "Extract 4-digit YEAR from DATE string."
  (when (and (stringp date) (string-match "\\`\\([0-9]\\{4\\}\\)" date))
    (match-string 1 date)))

(defun tlon-bib--date-iso (date)
  "Normalize DATE to YYYY-MM-DD when possible.
If DATE has a time component (e.g., YYYY-MM-DDTHH:MM:SS), strip it."
  (cond
   ((and (stringp date)
         (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" date))
    (format "%s-%s-%s" (match-string 1 date) (match-string 2 date) (match-string 3 date)))
   ((tlon-bib--date-year date) (format "%s-01-01" (tlon-bib--date-year date)))
   (t nil)))

(provide 'tlon-bib)
;;; tlon-bib.el ends here

;; Local Variables:
;; End:
