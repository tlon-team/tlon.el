;;; tlon-babel-tex.el --- BibTeX related functionality -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon-babel
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
(require 'doi-utils)
(require 'bibtex-extras)
(require 'paths)

;;;; User options

(defgroup tlon-babel-tex ()
  "BibTeX related functionality."
  :group 'tlon-babel)

(defcustom tlon-babel-abstract-overwrite 'prompt
  "Whether to overwrite the abstract if already present."
  :type '(choice
	  (const :tag "Always overwrite" always)
	  (const :tag "Never overwrite" never)
	  (const :tag "Prompt" prompt)))

;;;; Variables

;; TODO: remove `refs' from var names. make sure nothing breaks in config files
(defvar tlon-babel-refs-dir
  paths-dir-babel-refs
  "Directory of the `babel-refs' repo.")

(defvar tlon-babel-refs-bibtex-dir
  (file-name-concat tlon-babel-refs-dir "bib/")
  "Directory where BibTeX files are stored.")

(defvar tlon-babel-refs-file-fluid
  (file-name-concat tlon-babel-refs-bibtex-dir "fluid.bib")
  "File containing the fluid bibliography.")

(defvar tlon-babel-refs-file-stable
  (file-name-concat tlon-babel-refs-bibtex-dir "stable.bib")
  "File containing the stable bibliography.")

(defvar tlon-babel-refs-bibliography-files
  `(,tlon-babel-refs-file-fluid
    ,tlon-babel-refs-file-stable)
  "List of bibliography files.")

(defconst bibtex-extras-valid-languages
  '(("albanian" . "sq")
    ("american" . "en-US")
    ("amharic" . "am")
    ("arabic" . "ar")
    ("argentinian" . "es-AR")
    ("armenian" . "hy")
    ("assamese" . "as")
    ("asturian" . "ast")
    ("austrian" . "de-AT")
    ("australian" . "en-AU")
    ("basque" . "eu")
    ("belarusian" . "be")
    ("bengali" . "bn")
    ("bosnian" . "bs")
    ("breton" . "br")
    ("british" . "en-GB")
    ("bulgarian" . "bg")
    ("canadian" . "en-CA")
    ("catalan" . "ca")
    ("chinese" . "zh")
    ("coptic" . "cop")
    ("croatian" . "hr")
    ("czech" . "cs")
    ("danish" . "da")
    ("dutch" . "nl")
    ("english" . "en")
    ("esperanto" . "eo")
    ("estonian" . "et")
    ("finnish" . "fi")
    ("french" . "fr")
    ("galician" . "gl")
    ("georgian" . "ka")
    ("german" . "de")
    ("greek" . "el")
    ("hebrew" . "he")
    ("hindi" . "hi")
    ("hungarian" . "hu")
    ("icelandic" . "is")
    ("interlingua" . "ia")
    ("irish" . "ga")
    ("italian" . "it")
    ("japanese" . "ja")
    ("kannada" . "kn")
    ("korean" . "ko")
    ("lao" . "lo")
    ("latin" . "la")
    ("latvian" . "lv")
    ("lithuanian" . "lt")
    ("macedonian" . "mk")
    ("malayalam" . "ml")
    ("marathi" . "mr")
    ("mexican" . "es-MX")
    ("mongolian" . "mn")
    ("naustrian" . "de-AT")
    ("newzealand" . "en-NZ")
    ("ngerman" . "de-DE")
    ("nko" . "nqo")
    ("norwegian" . "no")
    ("oriya" . "or")
    ("persian" . "fa")
    ("polish" . "pl")
    ("portuges" . "pt")
    ("romanian" . "ro")
    ("russian" . "ru")
    ("sanskrit" . "sa")
    ("serbian" . "sr")
    ("slovak" . "sk")
    ("slovenian" . "sl")
    ("spanish" . "es")
    ("spanish" . "es")
    ("swedish" . "sv")
    ("swissgerman" . "de-CH")
    ("tamil" . "ta")
    ("telugu" . "te")
    ("thai" . "th")
    ("turkish" . "tr")
    ("turkmen" . "tk")
    ("ukenglish" . "en-GB")
    ("ukrainian" . "uk")
    ("urdu" . "ur")
    ("vietnamese" . "vi")
    ("welsh" . "cy")
    ("afrikaans" . "af"))
  "Alist of languages and ISO 639-1 codes for the `landid' field in BibTeX entries.")

;;;; Functions

;;;;; language

(defun tlon-babel-validate-language (language)
  "If LANGUAGE is a valid language, return it.
The validation is case-insensitive, but the returned language is in lowercase."
  (let ((language (downcase language)))
    (when (member language (mapcar #'car bibtex-extras-valid-languages))
      language)))

(defun tlon-babel-get-two-letter-code (language)
  "Return the two-letter code for LANGUAGE."
  (when-let* ((downcased (downcase language))
	      (code-raw (alist-get downcased bibtex-extras-valid-languages nil nil #'string=)))
    (string-limit code-raw 2)))

;;;;; fetch

(defun tlon-babel-fetch-and-set-abstract ()
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
    (when (tlon-babel-abstract-may-proceed-p)
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
			   (tlon-babel-fetch-abstract-from-crossref doi)
			   (tlon-babel-fetch-abstract-from-google-books isbn)
			   (tlon-babel-fetch-abstract-with-zotra url doi))))
	      (progn
		(shut-up
		  (funcall set-field "abstract" (tlon-babel-abstract-cleanup value)))
		(message "Set abstract of `%s'." key)
		(setq found t))
	    (message "Could not find abstract for `%s' using non-AI methods." key)
	    (setq found nil))
	  (tlon-babel-ai-batch-continue)
	  found)))))

(defun tlon-babel-fetch-abstract-with-zotra (url doi)
  "Return the abstract of the work with URL or DOI."
  (when-let ((id (or url doi)))
    (message "Trying to find abstract for %s with zotra..." id)
    (let* ((doi (when doi (tlon-babel-fetch-url-from-doi doi))))
      (if-let ((abstract
		(catch 'found
		  (dolist (field (list url (unless (string= url doi) doi)))
		    (when (and field
			       (not (string-match-p "\\.pdf$" field)))
		      (when-let ((abstract
				  (shut-up (zotra-extras-fetch-field
					    "abstract" field (when tlon-babel-ai-batch-fun 'no-error)))))
			(throw 'found abstract)))))))
	  abstract
	(progn (message "No abstract found.") nil)))))

;; TODO: submit as pull request to `doi-utils'?
;; `doi-utils-get-redirect' doesn't work
;; note that my function doesn't always return the final target of the redirect
;; because they sometimes use JavaScript; see id:1ED71E19-1CE4-4221-8880-AFFD799E34F0
(defun tlon-babel-fetch-url-from-doi (doi)
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
(defun tlon-babel-fetch-abstract-from-crossref (doi)
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

(defun tlon-babel-fetch-abstract-from-google-books (isbn)
  "Return the abstract of the book with ISBN."
  (when isbn
    (let ((url (format "https://www.googleapis.com/books/v1/volumes?q=isbn:%s" isbn))
	  (description nil))
      (message "Trying to find abstract for %s with Google Books..." isbn)
      (with-current-buffer (shut-up (url-retrieve-synchronously url))
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

(defun tlon-babel-abstract-may-proceed-p ()
  "Return t iff it’s okay to proceed with abstract processing."
  (if (derived-mode-p 'bibtex-mode 'ebib-entry-mode)
      (let* ((get-field (pcase major-mode
			  ('ebib-entry-mode #'ebib-extras-get-field)
			  ('bibtex-mode #'bibtex-extras-get-field)))
	     (abstract (funcall get-field  "abstract")))
	(if (or
	     (eq tlon-babel-abstract-overwrite 'always)
	     (not abstract)
	     (unless (eq tlon-babel-abstract-overwrite 'never)
	       (y-or-n-p "Abstract already exists. Overwrite?")))
	    t
	  (message "Skipping: `%s' already contains an abstract."
		   (pcase major-mode
		     ('bibtex-mode (bibtex-extras-get-key))
		     ('ebib-entry-mode (bibtex-extras-get-key "=key="))))
	  nil))
    (derived-mode-p 'text-mode)))


(defun tlon-babel-fetch-field-with-zotra (field &optional id-or-url no-error)
  "Fetch the value of FIELD from the ID-OR-URL of the entry at point.
IF ID-OR-URL is nil, try to get it or fetch it. If NO-ERROR is non-nil, handle
errors gracefully."
  (let* ((id-or-url (or id-or-url (ebib-extras-get-or-fetch-id-or-url))))
    (zotra-extras-fetch-field field id-or-url no-error)))

(defun tlon-babel-abstract-cleanup (string)
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

;;;;; moving entries

(defun tlon-babel-move-entry-to-tlon (&optional key)
  "Move entry with KEY to `tlon-babel-refs-file-fluid'.
Save citekey to \"kill-ring\". If KEY is nil, use the key of the entry at point."
  (interactive)
  (let ((key (or key (bibtex-extras-get-key)))
	(target tlon-babel-refs-file-fluid))
    (bibtex-extras-move-entry key target)
    (with-current-buffer (find-file-noselect target)
      (widen)
      (bibtex-search-entry key)
      (tlon-babel-add-or-update-tlon-field)
      (save-buffer))
    (kill-new key)))

;;;;; adding fields

(defun tlon-babel-add-or-update-tlon-field ()
  "Add or update \"database\" field with \"Tlön\" value in the current BibTeX entry."
  (bibtex-extras-add-or-update-field "database" "Tlön"))

(defun tlon-babel-add-database-field (file)
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
	   (tlon-babel-add-or-update-tlon-field)))))
    ;; Save the updated entries to the file
    (save-buffer)))

(defun tlon-babel-auto-add-database-field ()
  "Run `bibtex-extras-add-database-field' every time `new.bib' is saved."
  (let ((file tlon-babel-refs-file-fluid))
    (when (string= (buffer-file-name) file)
      (tlon-babel-add-database-field file))))

;;;;; cleanup

(defun tlon-babel-auto-clean-entry ()
  "Clean up bibtex entry at point upon saving."
  (let ((after-save-hook nil))
    (tlon-babel-add-lang-id-to-entry)
    (tlon-babel-remove-empty-spaces)
    (bibtex-extras-escape-dollar-signs)
    (bibtex-clean-entry)
    (save-buffer)))

;; TODO: support arbitrary langs
(defun tlon-babel-add-lang-id-to-entry (&optional _ _ _)
  "Add `langid' field to entry at point, if appropriate.
If the field `landig' is present, the function does nothing; else, it sets the
`langid' field to `spanish' if the entry has either a `translation' or a
`translator' field, and to `english' otherwise."
  (unless (bibtex-text-in-field "langid")
    (if (or (bibtex-text-in-field "translation")
	    (bibtex-text-in-field "translator"))
	(bibtex-set-field "langid" "spanish")
      (bibtex-set-field "langid" "english"))))

(defun tlon-babel-remove-empty-spaces ()
  "Remove empty spaces at the end of field."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward " \\}" nil t)
      (replace-match "}" t t))))

(defun bibtex-extras-escape-dollar-signs ()
  "Escape all dollar signs in the current BibTeX file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\\(?:[^\\]\\|^\\)\\)\\(\\$\\)" nil t)
      (replace-match "\\1\\\\$" nil nil))))


;;;;; autokey

(defun tlon-babel-generate-autokey (author year title)
  "Generate a BibTeX key based on AUTHOR, YEAR, and TITLE."
  ;; TODO: check that they key doesn't already exist in all metadata
  (let* ((author (tlon-babel-autokey-get-names author))
	 (year (tlon-babel-autokey-get-year year))
	 (title (tlon-babel-autokey-get-title title))
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

(defun tlon-babel-autokey-get-names (name)
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

(defun tlon-babel-autokey-get-year (year)
  "Get formatted contents of YEAR field."
  (substring year (max 0 (- (length year) bibtex-autokey-year-length))))

(defun tlon-babel-autokey-get-title (title)
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

(provide 'tlon-babel-tex)
;;; tlon-babel-tex.el ends here
