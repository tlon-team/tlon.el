;;; tlon-bae.el --- A collection of convenience functions for the Tlön BAE project. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.11
;; Homepage: https://tlon.team
;; Keywords: convenience tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:
(require 'org)
(require 'org-clock)
(require 'bibtex)
(require 'cl-lib)
(require 'transient)
(require 'subr-x)

;;; Version
(setq tlon-bae-version "0.1.11")

(defun tlon-bae-version ()
  "Return the version of the Tlön BAE package."
  (interactive)
  (message "`tlon-bae' version %s" tlon-bae-version))

(defun tlon-bae-update ()
  "Update `tlon-bae' package."
  (interactive)
  (let* ((default-directory (file-name-concat user-emacs-directory "elpaca/repos/tlon-bae/"))
	 (builds-directory (file-name-concat user-emacs-directory "elpaca/builds/tlon-bae/"))
	 (tlon-bae-file (file-name-concat default-directory "tlon-bae.el")))
    (shell-command "git pull")
    (with-current-buffer (find-file-noselect tlon-bae-file)
      (eval-buffer))
    (dired-delete-file builds-directory 'always t)
    (message "Package updated. %s" tlon-bae-version)))

;;; File vars

(defvar tlon-bae-dir-original-posts
  (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "originals/posts/")
  "Directory containing original posts.")

(defvar tlon-bae-dir-translated-posts
  (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "translations/posts/")
  "Directory containing translated posts.")

(defvar tlon-bae-dir-original-tags
  (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "originals/tags/")
  "Directory containing original tags.")

(defvar tlon-bae-dir-translated-tags
  (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "translations/tags/")
  "Directory containing translated tags.")

;;;

(defun tlon-bae-forge ()
  "Launch the Forge dispatcher in the BAE directory."
  (interactive)
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (call-interactively 'forge-dispatch)))

(defun tlon-bae-orgit-capture ()
  "Capture a new org mode task for topic at point."
  (interactive)
  (let ((assignee (alist-get
		   (tlon-bae-forge-get-assignee-at-point)
		   tlon-bae-github-users nil nil 'string=))
	(label (tlon-bae-forge-get-label-at-point)))
    ;; when the topic has neither a label nor an assignee, we offer to
    ;; process it as a new job
    (if (not (or assignee label))
	(if (y-or-n-p "Process issue as a new job? This will assign the issue to you, add the label 'Awaiting processing', and create a new master TODO in your org mode file.")
	    (progn
	      (tlon-bae-start-job t)
	      (sleep-for 4)
	      (tlon-bae-orgit-capture))
	  (user-error "Aborted"))
      ;; else we prompt for an assignee...
      (unless (string= user-full-name assignee)
	(if (y-or-n-p
	     (format "The assignee of this topic is %s. Would you like to become the assignee?" assignee))
	    (progn
	      (tlon-bae-set-assignee (tlon-bae-find-key-in-alist user-full-name tlon-bae-github-users))
	      (sleep-for 2))
	  (user-error "Aborted")))
      ;; ...or for a label
      (unless label
	(if (y-or-n-p "The topic has no label. Would you like to add one?")
	    (tlon-bae-set-label (tlon-bae-select-label))
	  (tlon-bae-set-assignee (tlon-bae-find-key-in-alist user-full-name tlon-bae-github-users))
	  (user-error "Aborted")))
      (orgit-store-link nil)
      (magit-pull-from-upstream nil)
      (if-let* ((org-link (ps/org-nth-stored-link 0))
		(refile-position (org-find-exact-headline-in-buffer
				  (cadr (nth 0 org-stored-links))
				  (find-file-noselect ps/file-tlon-bae))))
	  (let ((action (alist-get label tlon-bae-label-actions nil nil #'string=))
		(binding (upcase (alist-get label tlon-bae-label-bindings nil nil #'string=))))
	    (kill-new (format "%s %s" action org-link))
	    (org-capture nil (concat "tb" binding))
	    ;; refile under job
	    (org-refile nil nil (list nil (buffer-file-name) nil refile-position))
	    (ps/org-refile-goto-latest))
	(when (y-or-n-p "No master TODO found for this topic. Create?")
	  (tlon-bae-start-job)
	  (tlon-bae-orgit-capture))))))

(defun tlon-bae-start-job (&optional set-topic)
  "Create new job.
If SET-TOPIC is non-nil, set topic label to 'Awaiting processing'
and assignee to the current user."
  (save-window-excursion
    (when set-topic
      (tlon-bae-set-initial-label-and-assignee))
    (orgit-store-link nil)
    (let ((job-name (cadr (nth 0 org-stored-links))))
      (kill-new (format "%s" job-name)))
    (org-capture nil "tbJ")))

;;; File processing
(defun tlon-bae-generate-file-path (&optional lastname title tag translation)
  "Return a file name based on user supplied information.
TITLE is the title of the work. If TAG is 'tag, use `tag' as
LASTNAME. If TRANSLATION is non-nil, use `translatons' in the
file path."
  (let* ((tag (or (eq tag 'tag)
		  (string= lastname "tag")))
	 (lastname (or lastname (if tag
				    "tag"
				  (read-string "Last name (only first author if multi-authored): "))))
	 (title (or title (tlon-bae-shorten-title (read-string "Title (in English): "))))
	 (slug-lastname (tlon-core-slugify lastname))
	 (slug-title (tlon-core-slugify title))
	 (file-name (file-name-with-extension (concat slug-lastname "--" slug-title) "md"))
	 (file-path (file-name-concat
		     ps/dir-tlon-biblioteca-altruismo-eficaz
		     (if translation "translations" "originals")
		     (if tag "tags" "posts")
		     file-name)))
    file-path))

(defun tlon-bae-copy-file-name ()
  "Copy the generated file name to the kill ring."
  (interactive)
  (let* ((file-path (tlon-bae-generate-file-path))
	 (filename (file-name-nondirectory file-path)))
    (kill-new filename)
    (message "Copied `%s'" filename)))

(defun tlon-bae-extract-lastname-from-author (author)
  "Try to extract a last name from AUTHOR."
  (let* ((case-fold-search nil)
	 (uncameled-author (replace-regexp-in-string "\\([A-Z]\\)" " \\1" author))
	 (lastname (car (last (split-string uncameled-author "[ _-]")))))
    lastname))

(defun tlon-bae-eaf-generate-post-file-path (response)
  "Generate EAF post file-path baed on author and title from RESPONSE."
  (let* ((title (tlon-bae-shorten-title (tlon-bae-eaf-get-post-title response)))
	 ;; sometimes the 'author' field is empty so we use the 'user' field instead
	 (author (or (tlon-bae-eaf-get-post-author response)
		     (tlon-bae-eaf-get-post-username response)))
	 (lastname (tlon-bae-extract-lastname-from-author author))
	 (file-path (tlon-bae-generate-file-path lastname title))
	 (filename (file-name-nondirectory file-path))
	 (filename-reviewed (tlon-bae-review-filename filename)))
    (unless (string= filename filename-reviewed)
      (setq file-path (file-name-concat (file-name-directory file-path)
					filename-reviewed)))
    file-path))

(defun tlon-bae-eaf-generate-tag-file-path (response)
  "Generate EAF tag file-path baed on author and title from RESPONSE."
  (let* ((title (tlon-bae-shorten-title (tlon-bae-eaf-get-tag-title response)))
	 (lastname "tag")
	 (file-path (tlon-bae-generate-file-path lastname title))
	 (filename (file-name-nondirectory file-path))
	 (filename-reviewed (tlon-bae-review-filename filename)))
    (unless (string= filename filename-reviewed)
      (setq file-path (file-name-concat (file-name-directory file-path)
					filename-reviewed)))
    file-path))

(defun tlon-bae-review-filename (filename)
  "Review FILENAME."
  (let* ((choices '((?a . "ccept") (?e . "dit") (?r . "egenerate")))
	 (keys (mapcar 'car choices))
	 (key-description (mapconcat (lambda (choice)
				       (format "[%c]%s" (car choice) (cdr choice)))
				     choices " "))
	 (prompt (format "%s %s" filename key-description))
	 (choice (read-char-choice prompt keys)))
    (pcase choice
      (?a filename)
      (?e (read-string "Job name: " filename))
      (?r (file-name-nondirectory (tlon-bae-generate-file-path))))))

(defun tlon-bae-rename-file ()
  "Rename file at point based on user-supplied information.
If EXTENSION is not provided, markdown is used."
  (interactive)
  (let* ((source-file-path (dired-get-filename))
	 (target-file-name (file-name-nondirectory (tlon-bae-generate-file-path)))
	 (target-file-path (file-name-concat
			    (file-name-directory source-file-path)
			    target-file-name)))
    (rename-file
     source-file-path
     target-file-path)
    (revert-buffer)))

(defun tlon-bae-create-file ()
  "Create a new file based on user-supplied information.
Prompt the user for bibliographic information and create a new
 file based on it in the current directory."
  (interactive)
  (let ((file (tlon-bae-generate-file-path)))
    (find-file file)))

(defun tlon-bae-latest-user-commit-in-file (&optional file)
  "Return latest commit by the current user in FILE.
If no FILE is provided, use the file visited by the current buffer."
  (let* ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz)
	 (file (or file (buffer-file-name)))
	 (user (tlon-bae-find-key-in-alist user-full-name tlon-bae-system-users))
	 ;; get most recent commit in FILE by USER
	 (output (shell-command-to-string (format "git log --pretty=format:'%%h %%an %%s' --follow -- '%s' | grep -m 1 '%s' | awk '{print $1}'" file user)))
	 (commit (car (split-string output "\n"))))
    commit))

(defun tlon-bae-log-buffer-latest-user-commit (&optional file)
  "Show FILE changes since the latest commit by the current user.
If no FILE is provided, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-bae-latest-user-commit-in-file file)))
    (magit-diff-range commit nil (list file))))

(defun tlon-bae-log-buffer-latest-user-commit-ediff (&optional file)
  "Docstring."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-bae-latest-user-commit-in-file file))
	 (commit-file (tlon-bae-create-file-from-commit file commit)))
    (ediff-files commit-file file)))

(defun tlon-bae-shorten-title (title)
  "Return a shortened version of TITLE."
  (string-match "\\([[:alnum:] ,'‘’“”@#$%*\\^`~&\"]*\\)" title)
  (match-string 1 title))

(defvar tlon-bae-markdown-eawiki-footnote-source
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\{1,2\\}\\)\\\\\\]\\](#.+?)\\^\\]{#.+? .footnote-reference role=\"doc-noteref\"}"
  "Regexp to match footnotes in the main body.")

(defvar tlon-bae-markdown-eawiki-footnote-source2
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\)\\\\\\]\\](#.+?)\\^\\]{#\\\\\\\\\\\\\".+?\\\\\\\\\\\\\" .\\\\\\\\\\\\\\\"footnote-reference\\\\\\\\\\\\\" role=\"\\\\\\\\\\\\\"doc-noteref\\\\\\\\\\\\\"\"}"
  "Regexp to match footnotes in the main body.")

(defvar tlon-bae-markdown-eawiki-footnote-target
  "\\([[:digit:]]\\{1,3\\}\\). +?\\[\\[^\\*\\*\\[\\\\^\\](#[[:alnum:]]\\{12,18\\})\\*\\*^\\]\\]{#[[:alnum:]]\\{10,15\\}}

    footnote-content.*?"
  "Regexp to match footnotes in the footnote section.")

(defvar tlon-markdown-eawiki-links
  "\\[\\(.+?\\)\\](\\\\%22\\(.+?\\)\\\\%22)"
  "Regexp to match links.")

(defun tlon-bae-markdown-eaf-cleanup (&optional buffer)
  "Cleanup the BUFFER visiting an EAF entry."
  (interactive)
  (when (not (eq major-mode 'markdown-mode))
    (user-error "Not in a Markdown buffer"))
  (save-excursion
    (unfill-region (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "{.underline}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\(.*?\\)\\]\\](" nil t)
      (replace-match (format "[%s]("
			     ;; remove extra backslashes in replacement text to avoid elisp errors
			     (replace-regexp-in-string "\\\\" "" (match-string-no-properties 1)))))
    (goto-char (point-min))
    ;; remove asterisks surrounding links
    (while (re-search-forward " ??\\* ??\\[\\*\\[\\(.*?\\)\\]\\*\\](\\(.*?\\)) ??\\* ??" nil t)
      (replace-match (format " [%s](%s)" (match-string-no-properties 1) (match-string-no-properties 2))))
    (goto-char (point-min))
    ;; move double asterisks outside of link
    (while (re-search-forward "\\[\\*\\*\\[\\(.*?\\)\\]\\*\\*\\](\\(.*?\\))" nil t)
      (replace-match (format "**[%s](%s)**" (match-string-no-properties 1) (match-string-no-properties 2))))
    (goto-char (point-min))
    (while (re-search-forward "{.footnote-back-link}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward " " nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward " :::" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "{.underline}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-source nil t)
      (replace-match (format "[^%s] " (match-string-no-properties 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-source2 nil t)
      (replace-match (format "[^%s]: " (match-string-no-properties 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-target nil t)
      (replace-match (format "[^%s]:" (match-string-no-properties 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-markdown-eawiki-links nil t)
      (replace-match (format "[%s](%s)" (match-string-no-properties 1) (match-string-no-properties 2)) nil t))
    (goto-char (point-min))
    ;; remove double asterisks surrounding headings
    (while (re-search-forward "# \\*\\*\\(.*\\)\\*\\* *?$" nil t)
      (replace-match (format "# %s" (match-string-no-properties 1))))
    (tlon-bae-non-eaf-cleanup)
    (fill-region (point-min) (point-max))
    (save-buffer)))

(defun tlon-bae-non-eaf-cleanup ()
  "Cleanup the buffer visiting a non-EAF entry."
  (interactive)
  ;; TODO: Move this to a common function called by both the EAF and non-EAF commands.
  (goto-char (point-min))
  ;; unescape double quotes
  (while (re-search-forward "\\\\\"" nil t)
    (replace-match "\""))
  (goto-char (point-min))
  ;; unescape single quotes
  (while (re-search-forward "\\\\'" nil t)
    (replace-match "'"))
  (goto-char (point-min))
  ;; unescape vertical bars
  (while (re-search-forward "\\\\|" nil t)
    (replace-match "|"))
  (goto-char (point-min))  ;; remove extra line breaks
  (while (re-search-forward "\n\n\n" nil t)
    (replace-match "\n\n"))
  (goto-char (point-min)))

(defun tlon-bae-convert-to-markdown ()
  "Convert a file from EA Wiki to Markdown."
  (interactive)
  (dolist (file (directory-files "." nil "\\.html$"))
    (let ((md-file (file-name-with-extension file "md")))
      (shell-command (format "pandoc -s '%s' -t markdown -o '%s'"
			     file
			     md-file)))))

(defun tlon-bae-cleanup-markdown-multiple ()
  "Clean up html files imported from EA Wiki."
  (interactive)
  (dolist (file (directory-files "." nil "\\.md$"))
    (with-current-buffer (find-file-noselect file)
      (message "Cleaning up %s" (buffer-name))
      (tlon-bae-markdown-eaf-cleanup))))

;;; csv and txt parsing

(defun tlon-bae-parse-csv-line (line &optional separator)
  "Split CSV LINE into fields, considering quotes, using SEPARATOR."
  (let ((index 0)
	(separator (or separator ?,))
	(quote-char ?\")
	(len (length line))
	(in-quote nil)
	start
	fields)
    (while (< index len)
      (setq start index)
      (while (and (< index len)
		  (not (and (not in-quote) (char-equal (elt line index) separator))))
	(when (char-equal (elt line index) quote-char)
	  (setq in-quote (not in-quote)))
	(setq index (1+ index)))
      (let ((field (substring-no-properties line start index)))
	(when (and (> (length field) 1)
		   (char-equal (elt field 0) ?\")
		   (char-equal (elt field (1- (length field))) ?\"))
	  (setq field (substring-no-properties field 1 -1)))
	(push field fields))
      (setq index (1+ index)))
    (nreverse fields)))

(defun tlon-bae-csv-file-to-alist (file-path)
  "Convert CSV file to an alist."
  (let ((file-content (with-temp-buffer
			(insert-file-contents file-path)
			(buffer-string))))
    (let* ((lines (split-string file-content "[\r\n]+" t))
	   (content lines)
	   alist)
      (dolist (row content)
	(let* ((fields (tlon-bae-parse-csv-line row))
	       (entry (cons (car fields) (cadr fields))))
	  (setq alist (push entry alist))))
      (nreverse alist))))

(defun tlon-bae-read-urls-from-file (file-path)
  "Read URLs from a plain text FILE-PATH into an Emacs Lisp list."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defvar tlon-bae-post-correspondence nil
  "Alist of English and Spanish posts.")

(defvar tlon-bae-tag-correspondence nil
  "Alist of English and Spanish tags.")

(defvar tlon-bae-work-correspondence nil
  "Alist of original and Spanish works.")

(defvar tlon-bae-tag-slugs nil
  "List of EA Wiki slugs.")

(defvar tlon-bae-wiki-urls nil
  "List of EA Wiki URLs.")

(defun tlon-bae-insert-tag-slug ()
  "Insert an EA Wiki slug at point."
  (interactive)
  (insert (completing-read "URL: " tlon-bae-tag-slugs)))

(defun tlon-bae-insert-tag-url ()
  "Insert an EA Wiki slug at point."
  (interactive)
  (insert (completing-read "URL: " tlon-bae-wiki-urls)))

(defun tlon-bae-get-original-translated (bib-file)
  "Parse BIB-FILE and return an alist of original-translation key pairs."
  (let* ((bib-buffer (find-buffer-visiting bib-file))
         (buffer-exists (buffer-live-p bib-buffer))
         (translations-alist '()))
    ;; Check if a buffer is visiting the file.
    ;; If not, open the file in a new buffer.
    (unless buffer-exists
      (setq bib-buffer (find-file-noselect bib-file)))
    (with-current-buffer bib-buffer
      (bibtex-map-entries
       (lambda (key _beg _end)
         (if (not key) (message "Found empty key at %s" (point)))
         (bibtex-narrow-to-entry)
         (when-let* ((translation (bibtex-autokey-get-field "translation")))
           (unless (string-empty-p translation)
             (setq translations-alist (cons (cons translation key) translations-alist))))
         (widen))))
    translations-alist))

(defun tlon-bae-convert-keys-to-files (input-alist)
  "Take INPUT-ALIST of keys and return an a list of corresponding files."
  (let ((output-alist '()))
    (dolist (cons-cell input-alist output-alist)
      (let* ((original-key (car cons-cell))
	     (translation-key (cdr cons-cell))
	     (original-file (file-name-with-extension original-key "md"))
	     (translation-file (file-name-with-extension translation-key "md")))
	(setq output-alist (cons (cons original-file translation-file) output-alist))))))


;;; correspondences
(defun tlon-bae-load-post-correspondence ()
  "Refresh alist of original-translation file pairs."
  (interactive)
  (let* ((key-alist-pending (tlon-bae-get-original-translated ps/file-tlon-bibliography-pending))
	 (key-alist-finished (tlon-bae-get-original-translated ps/file-tlon-bibliography-finished))
	 (input-alist (tlon-bae-convert-keys-to-files (append key-alist-pending key-alist-finished))))
    (setq tlon-bae-post-correspondence input-alist)))

(defun tlon-bae-get-translation-file (original-filename)
  "Return file that translates ORIGINAL-FILENAME."
  (or (alist-get original-filename tlon-bae-post-correspondence nil nil #'equal)
      (alist-get original-filename tlon-bae-tag-correspondence nil nil #'equal)))

(defun tlon-bae-get-original-file (translation-filename)
  "Return file that TRANSLATION-FILENAME translates."
  (cl-loop for (key . val) in (append
			       tlon-bae-post-correspondence
			       tlon-bae-tag-correspondence)
	   when (equal val translation-filename)
	   return key))

(defun tlon-bae-get-counterpart-filename (file-path)
  "Get the counterpart of file in FILE-PATH."
  (interactive)
  (let ((filename (file-name-nondirectory file-path)))
    (if (or (string-match tlon-bae-dir-original-posts file-path)
	    (string-match tlon-bae-dir-original-tags file-path))
	(tlon-bae-get-translation-file filename)
      (tlon-bae-get-original-file filename))))

(defun tlon-bae-get-counterpart-dirname (file-path)
  "Get the counterpart of file in FILE-PATH."
  (let* ((dirname (file-name-directory file-path))
	 (counterpart-dirname (if (string-match "/originals/" dirname)
				  (replace-regexp-in-string
				   "/originals/" "/translations/" dirname)
				(replace-regexp-in-string
				 "/translations/" "/originals/" dirname))))
    counterpart-dirname))

(defun tlon-bae-open-counterpart (&optional file-path)
  "Open the counterpart of the file visited by the current buffer."
  (interactive)
  (if-let* ((file-path (or file-path (buffer-file-name)))
	    (counterpart-filename (tlon-bae-get-counterpart-filename file-path))
	    (counterpart-dirname (tlon-bae-get-counterpart-dirname file-path))
	    (counterpart (concat counterpart-dirname counterpart-filename)))
      (find-file counterpart)
    (user-error "No corresponding file found. Consider running `tlon-bae-load-post-correspondence'")))

;;; EAF validation

(defvar tlon-bae-eaf-p
  "forum\\.effectivealtruism\\.org/"
  "Regular expression for validating EAF URLs.")

(defvar tlon-bae-eaf-post-id-regexp
  "\\([[:alnum:]]\\{17\\}\\)"
  "Regular expression for validating post IDs.")

(defvar tlon-bae-eaf-tag-slug-regexp
  "\\([[:alnum:]-]*\\)"
  "Regular expression for validating tag slugs.")

(defun tlon-bae-eaf-p (url)
  "Return t if URL is an EAF URL, nil otherwise."
  (not (not (string-match tlon-bae-eaf-p url))))

(defun tlon-bae-eaf-post-id-p (identifier)
  "Return t if IDENTIFIER is a post ID, nil otherwise."
  (not (not (string-match (format "^%s$" tlon-bae-eaf-post-id-regexp) identifier))))

(defun tlon-bae-eaf-tag-slug-p (identifier)
  "Return t if IDENTIFIER is a tag slug, nil otherwise."
  (not (not (string-match (format "^%s$" tlon-bae-eaf-tag-slug-regexp) identifier))))

(defun tlon-bae-eaf-get-id-or-slug-from-identifier (identifier)
  "Return the EAF post ID or tag slug from IDENTIFIER, if found.
IDENTIFIER can be an URL, a post ID or a tag slug."
  (interactive "sURL: ")
  (if (ps/string-is-url-p identifier)
      (or (tlon-bae-eaf-get-id-from-identifier identifier)
	  (tlon-bae-eaf-get-slug-from-identifier identifier))
    ;; return id or slug if identifier is an id or slug
    (pcase identifier
      ((pred tlon-bae-eaf-post-id-p) identifier)
      ((pred tlon-bae-eaf-tag-slug-p) identifier))))

(defun tlon-bae-eaf-get-id-from-identifier (identifier)
  "Return the EAF post ID from IDENTIFIER, if found."
  (when-let ((id (or (when (string-match (format "^.+?forum.effectivealtruism.org/posts/%s"
						 tlon-bae-eaf-post-id-regexp)
					 identifier)
		       (match-string-no-properties 1 identifier))
		     (when (string-match (format "^.+?forum.effectivealtruism.org/s/%s/p/%s"
						 tlon-bae-eaf-post-id-regexp tlon-bae-eaf-post-id-regexp)
					 identifier)
		       (match-string-no-properties 2 identifier)))))
    id))

(defun tlon-bae-eaf-get-slug-from-identifier (identifier)
  "Return the EAF tag slug from IDENTIFIER, if found."
  (when (string-match (format "^.+?forum.effectivealtruism.org/topics/%s"
			      tlon-bae-eaf-tag-slug-regexp)
		      identifier)
    (match-string-no-properties 1 identifier)))

(defun tlon-bae-eaf-get-id-or-slug-from-response (response)
  "Return the EAF post ID or tag slug from Json RESPONSE."
  (or (tlon-bae-eaf-get-post-id response)
      (tlon-bae-eaf-get-tag-slug response)))

(defun tlon-bae-eaf-get-object (id-or-slug)
  "Return the EAF object in ID-OR-SLUG."
  (let ((object (cond ((tlon-bae-eaf-post-id-p id-or-slug)
		       'post)
		      ((tlon-bae-eaf-tag-slug-p id-or-slug)
		       'tag)
		      (t (user-error "Not an ID or slug: %S" id-or-slug)))))
    object))

;;; Clocked heading

(defun tlon-bae-get-clock-file ()
  "Return file name in clocked heading.
Assumes file name is enclosed in backticks."
  (unless org-clock-current-task
    (user-error "No clock running"))
  (let ((clock (substring-no-properties org-clock-current-task)))
    (if (string-match "`\\(.+?\\)`" clock)
	(match-string 1 clock)
      (user-error "I wasn't able to find a file in clocked heading"))))

(defun tlon-bae-get-clock-topic ()
  "Get topic GID from `orgit-forge' link in heading at point."
  (unless org-clock-heading
    (user-error "No clock running"))
  (save-window-excursion
    (org-clock-goto)
    (org-narrow-to-subtree)
    (when (re-search-forward org-link-bracket-re)
      (let ((raw-link (org-link-unescape (match-string-no-properties 1))))
	(string-match "orgit-topic:\\(.+\\)" raw-link)
	(match-string 1 raw-link)))))

(defun tlon-bae-open-clock-topic ()
  "Open the topic from `orgit-forge' link in heading at point."
  (interactive)
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz)
	(topic (tlon-bae-get-clock-topic)))
    (forge-visit-issue topic)))

(defun tlon-bae-get-clock-action ()
  "Return action in heading at point.
Assumes action is first word of clocked task."
  ;; as rough validation, we check that the clocked heading contains a file
  (tlon-bae-get-clock-file)
  (let ((action (car (split-string (substring-no-properties org-clock-current-task))))
	(actions (mapcar #'cdr tlon-bae-label-actions)))
    (if (member action actions)
	action
      (user-error "I wasn't able to find a relevant action in clocked heading"))))

(defun tlon-bae-mark-clock-heading-as-done (&optional parent)
  "Mark heading associated with current clock heading as DONE.
If PARENT is non-nil, mark parent heading as DONE as well."
  (save-window-excursion
    (org-clock-goto)
    (org-todo "DONE")
    (when parent
      (widen)
      (org-up-heading-safe)
      (org-todo "DONE"))
    (save-buffer)))

(defun tlon-bae-get-forge-file-path ()
  "Get the file path of the topic at point or in current forge buffer."
  (unless (or (derived-mode-p 'magit-status-mode)
	      (derived-mode-p 'forge-topic-mode))
    (user-error "I'm not in a forge buffer"))
  (let* ((inhibit-message t)
	 (captured (cadr (call-interactively #'orgit-store-link))))
    (setq org-stored-links (cdr org-stored-links))
    (if (string-match "`\\(.+?\\)`" captured)
	(tlon-bae-set-original-path (match-string 1 captured))
      (user-error "I wasn't able to find a file at point or in the forge buffer"))))

(defun tlon-bae-open-forge-file ()
  "Open the file of the topic at point or in the current forge buffer."
  (interactive)
  (find-file (tlon-bae-get-forge-file-path)))

(defun tlon-bae-open-forge-counterpart ()
  "Open the file counterpart of the topic at point or in the current forge buffer."
  (interactive)
  (tlon-bae-open-counterpart (tlon-bae-get-forge-file-path)))

(defun tlon-bae-find-subdirectory-containing-file (filename)
  "Search for a FILENAME in BAE repo dir and all its subdirectories.
Return the subdirectory containing the FILENAME, or nil if not found."
  (catch 'found
    (dolist (file (directory-files-recursively ps/dir-tlon-biblioteca-altruismo-eficaz filename t))
      (when (and (file-exists-p file)
		 (not (file-directory-p file))
		 (string-equal (file-name-nondirectory file) filename))
	(throw 'found (file-name-directory file)))
      nil)))

(defun tlon-bae-get-issue-gid-by-file (repo file)
  "Return issue GID for FILE in REPO.
Assumes the issue title contains FILE, which is a unique file in
the `originals/tags' directory."
  (cl-loop for topic in (forge-ls-topics repo 'forge-issue)
	   when (string= file (oref topic title))
	   return (oref topic id)))

(defun tlon-bae-get-issue-gid-by-partial-title (repo search-str)
  "Return issue GID matching SEARCH-STR in REPO."
  (cl-loop for topic in (forge-ls-topics repo 'forge-issue)
	   for title = (oref topic title)
	   when (cl-loop for substr in (split-string search-str)
			 always (string-match-p (regexp-quote substr) title))
	   return (oref topic id)))

(defun tlon-bae-copy-file-contents (&optional file deepl)
  "Copy the unfilled contents of FILE to the kill ring.
Defaults to the current buffer if no FILE is specified. If DEEPL
is non-nil, open DeepL."
  (let ((file (or file (buffer-file-name))))
    (with-current-buffer (find-file-noselect file)
      (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
	(with-temp-buffer
	  (insert contents)
	  (unfill-region (point-min) (point-max))
	  (copy-region-as-kill (point-min) (point-max)))
	(message "Copied the contents of `%s' to kill ring" (file-name-nondirectory file))))
    (when deepl
      (shell-command "open '/Applications/DeepL.app/Contents/MacOS/DeepL'"))))

(defun tlon-bae-copy-region (beg end)
  "Copy the unfilled contents between BEG and END to the kill ring."
  (let ((contents (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert contents)
      (unfill-region (point-min) (point-max))
      (copy-region-as-kill (point-min) (point-max)))
    (message "Copied the contents of the region to kill ring")))

(defun tlon-bae-copy-dwim ()
  "Copy the unfilled contents of the region or buffer to the kill ring."
  (interactive)
  (if (region-active-p)
      (tlon-bae-copy-region (region-beginning) (region-end))
    (tlon-bae-copy-file-contents)))

(defun tlon-bae-set-original-path (filename)
  "Return full path of FILENAME."
  (let* ((type (if (string-match "[[:digit:]]" filename) "posts/" "tags/"))
	 (dir (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "originals/" type))
	 (file-path (file-name-concat dir filename)))
    file-path))

(defun tlon-bae-open-clock-file ()
  "Open file of clocked task."
  (interactive)
  (let ((file-path (tlon-bae-set-original-path (tlon-bae-get-clock-file))))
    (find-file file-path)))

(defun tlon-bae-set-paths ()
  "Return paths for original and translation files from ORIGINAL-FILE."
  (if-let* ((original-path (tlon-bae-set-original-path (tlon-bae-get-clock-file)))
	    (original-file (file-name-nondirectory original-path))
	    (translation-file (tlon-bae-get-translation-file (file-name-nondirectory original-path)))
	    (dir (tlon-bae-post-or-tag original-file))
	    (translation-dir (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "translations" dir))
	    (translation-path (file-name-concat translation-dir translation-file)))
      (cl-values original-path translation-path original-file translation-file)
    (user-error "I wasn't able to find `%s' in the correspondence file" original-file)))

(defun tlon-bae-post-or-tag (file)
  "Return `posts' or `tags' depending on FILE."
  (if (string-match "[[:digit:]]" file)
      "posts"
    "tags"))

(defun tlon-bae-set-windows (original-path translation-path)
  "Open ORIGINAL-PATH and TRANSLATION-PATH in windows 1 and 2."
  (ps/window-split-if-unsplit)
  (winum-select-window-1)
  (find-file original-path)
  (winum-select-window-2)
  (find-file translation-path))

;;; Main functions

(defun tlon-bae-magit-status ()
  "Show the status of the BAE repository in a buffer."
  (interactive)
  (magit-status ps/dir-tlon-biblioteca-altruismo-eficaz))

(defun tlon-bae-magit-get-filename ()
  "Get filename of file to commit."
  (save-window-excursion
    (let (found)
      (catch 'done
	(mapc (lambda (x)
		(when (with-current-buffer x (eq major-mode 'magit-diff-mode))
		  (switch-to-buffer x)
		  (setq found t)
		  (throw 'done nil)))
	      (buffer-list))
	(unless found
	  (user-error "Magit buffer not found"))))
    (goto-char (point-min))
    (let ((regex "modified.*/\\(.*\\.*\\)"))
      (when (eq (count-matches regex) 1)
	(save-excursion
	  (re-search-forward regex nil t)
	  (match-string-no-properties 1))))))

(defun tlon-bae-dwim ()
  "Initialize or finalize process based on clocked task."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-clock-in))
  (save-buffer)
  (let ((action (tlon-bae-get-clock-action)))
    (pcase major-mode
      ;; assumes user initializes in org-mode and finalizes in markdown-mode
      ('org-mode (pcase action
		   ("Process" (tlon-bae-initialize-processing))
		   ("Translate" (tlon-bae-initialize-translation))
		   ("Check" (tlon-bae-initialize-check))
		   ("Revise" (tlon-bae-initialize-revision))
		   ("Review" (tlon-bae-initialize-review))
		   (_ (user-error "I don't know what to do with `%s`" action))))
      ('markdown-mode (pcase action
			("Process" (tlon-bae-finalize-processing))
			("Translate" (tlon-bae-finalize-translation))
			("Check" (tlon-bae-finalize-check))
			("Revise" (tlon-bae-finalize-revision))
			("Review" (tlon-bae-finalize-review))
			(_ (user-error "I don't know what to do with `%s`" action))))
      (_ (user-error "I don't know what to do in `%s`" major-mode)))))

(defun tlon-bae-create-job (identifier)
  "Create a new job for IDENTIFIER.
Creating a new job means (1) importing a document and (2)
creating an associated issue.

IDENTIFIER can be a URL or a PDF file path."
  (interactive "sURL or path to PDF: ")
  (tlon-bae-import-document identifier t)
  (message "Next step: capture the new job (`,`) and run the usual command (`H-r r`)."))

(defun tlon-bae-import-document (identifier &optional issue)
  "Import a document from IDENTIFIER.
IDENTIFIER can be a URL or a PDF file path. If ISSUE is non-nil,
a new issue will be created."
  (interactive "sURL or path to PDF: ")
  (if (ps/string-is-url-p identifier)
      (tlon-bae-import-html identifier issue)
    (tlon-bae-import-pdf identifier issue)))

(defun tlon-bae-import-html (url &optional issue)
  "Import the HTML in URL and convert it to Markdown.
If ISSUE is non-nil, create a new issue."
  (if-let ((id-or-slug (tlon-bae-eaf-get-id-or-slug-from-identifier url)))
      (tlon-bae-import-html-eaf id-or-slug issue)
    (let* ((target (tlon-bae-generate-file-path)))
      (tlon-bae-html-to-markdown url target)
      (when issue
	(tlon-bae-create-issue-for-job (file-name-nondirectory target))))))

(defun tlon-bae-import-html-eaf (id-or-slug &optional issue)
  "Import the HTML of EAF entity with ID-OR-SLUG and convert it to Markdown.
If ISSUE is non-nil, create an issue for the new job."
  (let* ((response (tlon-bae-eaf-request id-or-slug))
	 (object (tlon-bae-eaf-get-object id-or-slug))
	 (file-path (pcase object
		      ('post (tlon-bae-eaf-generate-post-file-path response))
		      ('tag (tlon-bae-eaf-generate-tag-file-path response))))
	 (html (pcase object
		 ('post (tlon-bae-eaf-get-post-html response))
		 ('tag (tlon-bae-eaf-get-tag-html response))))
	 (html-file (tlon-bae-save-html-to-file html)))
    (tlon-bae-html-to-markdown html-file file-path)
    (with-current-buffer (find-file-noselect file-path)
      (tlon-bae-markdown-eaf-cleanup))
    (find-file file-path)
    (when issue
      (tlon-bae-create-issue-for-job (file-name-nondirectory file-path))
      (forge-pull))))

(defun tlon-bae-html-to-markdown (source target)
  "Convert HTML text in SOURCE to Markdown text in TARGET.
SOURCE can be a URL or, like TARGET, a file path."
  (let ((pandoc (if (ps/string-is-url-p source)
		    tlon-bae-pandoc-convert-from-url
		  tlon-bae-pandoc-convert-from-file)))
    (shell-command
     (format pandoc source target))
    (find-file target)))

(defvar tlon-bae-pdf2md
  (file-name-concat ps/dir-source "pdf2md/lib/pdf2md-cli.js")
  "Path to `pdf2md-cli.js' executable.")

(defun tlon-bae-import-pdf (path)
  "Import the PDF in PATH and convert it to Markdown."
  (let* ((path (or path (read-file-name "PDF: ")))
	 (final-target-file (tlon-bae-generate-file-path))
	 (temp-source-dir (make-temp-file "pdf-source" t))
	 (temp-source-file (file-name-concat temp-source-dir "source.pdf"))
	 (temp-target-dir (make-temp-file "pdf-target" t))
	 (temp-target-file (file-name-concat temp-target-dir "source.md")))
    (unwind-protect
	(progn
	  (copy-file path temp-source-file)
	  (shell-command (format "node %s --inputFolderPath='%s' --outputFolderPath='%s'"
				 tlon-bae-pdf2md temp-source-dir temp-target-dir))
	  (copy-file temp-target-file final-target-file)
	  (delete-directory temp-source-dir t)
	  (delete-directory temp-target-dir t)))))

(defvar tlon-bae-pdftotext
  (file-name-concat ps/dir-source "xpdf-tools-mac-4.04/bin64/pdftotext")
  "Path to `pdftotext' executable.")

;; This function is not currently used because we now use pdf2md, rather
;; than pdftotext, to convert PDFs to Markdown.
(defun tlon-bae-import-pdf-pdftotext (path &optional issue)
  "Import the PDF in PATH and convert it to Markdown.
If ISSUE is non-nil, create an issue for the new job.

The command requires the user to supply values for the header and
footer elements to be excluded from the conversion, which are
different for each PDF. To determine these values, measure the
distance between the top/bottom of the PDF (which will open in
the other window) and note the number of pixels until the end of
the header/footer. (You can measure the number of pixels between
two points by taking a screenshot: note the numbers next to the
pointer.) Then enter these values when prompted. If ISSUE is
non-nil, a new issue will be created."
  (let* ((path (read-file-name "PDF: "))
	 (target (tlon-bae-generate-file-path)))
    (find-file-other-window path)
    (let ((header (read-string "Header: "))
	  (footer (read-string "Footer: ")))
      (shell-command (format "'%s' -margint %s -marginb %s '%s' '%s'"
			     tlon-bae-pdftotext header footer path target))
      (find-file target))
    (when issue
      (tlon-bae-create-issue-for-job (file-name-nondirectory target)))))

(defun tlon-bae-create-issue-for-job (&optional filename)
  "Create an issue based on FILENAME.
Creates a new issue in the BAE repository with the format `Job:
FILENAME`."
  (interactive)
  (let ((filename (or filename (read-string "Filename: ")))
	(default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (call-interactively #'forge-create-issue)
    (insert (format "Job: `%s`" filename))
    (call-interactively #'forge-post-submit)
    (sleep-for 2)
    (forge-pull)
    (magit-status ps/dir-tlon-biblioteca-altruismo-eficaz)
    ;; needs to be done twice for some reason; FIXME
    (forge-pull)))

;; revise imported file
(defun tlon-bae-initialize-processing ()
  "Initialize processing."
  (interactive)
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-branch "main")
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz)
	(original (tlon-bae-set-original-path (tlon-bae-get-clock-file)))
	(docs ps/file-tlon-docs-bae))
    (magit-pull-from-upstream nil)
    (sleep-for 2)
    (tlon-bae-set-windows original docs)
    (org-id-goto "60251C8E-6A6F-430A-9DB3-15158CC82EAE")
    (org-narrow-to-subtree)
    (ps/org-show-subtree-hide-drawers)
    (winum-select-window-2)
    ;; TODO: get URL via call to the Airtable API and open it with eww
    (let ((topic (tlon-bae-get-clock-topic)))
      (orgit-topic-open topic))))

(defun tlon-bae-initialize-translation ()
  "Initialize translation."
  (interactive)
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-branch "main")
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (magit-pull-from-upstream nil)
    (sleep-for 2)
    (tlon-bae-load-variables)
    (cl-multiple-value-bind
	(original-path translation-path original-file)
	(tlon-bae-set-paths)
      (let ((topic (tlon-bae-get-clock-topic)))
	(tlon-bae-set-windows original-path translation-path)
	(write-file translation-path)
	(ispell-change-dictionary "espanol")
	(flyspell-buffer)
	(winum-select-window-2)
	(orgit-topic-open topic)
	(tlon-bae-copy-file-contents original-path)))))

(defun tlon-bae-initialize-revision ()
  "Initialize stylistic revision."
  (interactive)
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-branch "main")
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (magit-pull-from-upstream nil)
    (sleep-for 2)
    (cl-multiple-value-bind
	(original-path translation-path original-file translation-file)
	(tlon-bae-set-paths)
      (tlon-bae-check-staged-or-unstaged-other-than translation-path)
      (let ((topic (tlon-bae-get-clock-topic)))
	(tlon-bae-set-windows original-path translation-path)
	(ispell-change-dictionary "espanol")
	(flyspell-buffer)
	(orgit-topic-open topic)
	(winum-select-window-2)
	(tlon-bae-copy-file-contents original-path)))))

(defun tlon-bae-initialize-check ()
  "Initialize accuracy check."
  (interactive)
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-branch "main")
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (magit-pull-from-upstream nil)
    (sleep-for 2)
    (cl-multiple-value-bind
	(original-path translation-path original-file translation-file)
	(tlon-bae-set-paths)
      (tlon-bae-check-staged-or-unstaged-other-than translation-path)
      (let ((topic (tlon-bae-get-clock-topic)))
	(tlon-bae-set-windows original-path translation-path)
	(ispell-change-dictionary "espanol")
	(flyspell-buffer)
	(winum-select-window-1)
	(let* ((markdown-buffer (concat "preview of " original-file))
	       (eww-buffer (concat markdown-buffer " — eww")))
	  (markdown-preview markdown-buffer)
	  (switch-to-buffer eww-buffer)
	  (read-aloud-buf))))))

(defun tlon-bae-initialize-review ()
  "Initialize review."
  (interactive)
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-branch "main")
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (magit-pull-from-upstream nil)
    (sleep-for 2)
    (cl-multiple-value-bind
	(original-path translation-path original-file translation-file)
	(tlon-bae-set-paths)
      (tlon-bae-check-staged-or-unstaged-other-than translation-path)
      (let ((topic (tlon-bae-get-clock-topic)))
	(tlon-bae-set-windows original-path translation-path)
	(ispell-change-dictionary "espanol")
	(flyspell-buffer)
	(tlon-bae-log-buffer-latest-user-commit-ediff translation-path)
	;; opens in other window, so no need to switch to it first
	(orgit-topic-open topic)
	(tlon-bae-copy-file-contents original-path)))))

(defun tlon-bae-finalize-processing ()
  "Finalize processing."
  (interactive)
  (tlon-bae-check-branch "main")
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-file)
  (cl-multiple-value-bind
      (original-path translation-path original-file)
      (tlon-bae-set-paths)
    (fill-region (point-min) (point-max))
    (save-buffer)
    (let ((label "Awaiting translation")
	  (assignee "benthamite"))
      (tlon-bae-act-on-topic original-file label assignee)
      (tlon-bae-commit-and-push "Process " original-path)
      (org-clock-goto)
      (org-todo "DONE")
      (save-buffer)
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'" label assignee))
    (sit-for 5)))

(defun tlon-bae-finalize-translation ()
  "Finalize translation."
  (interactive)
  (save-buffer)
  (tlon-bae-check-branch "main")
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-file)
  (cl-multiple-value-bind
      (original-path translation-path original-file translation-file)
      (tlon-bae-set-paths)
    (fill-region (point-min) (point-max))
    (save-buffer)
    (write-file translation-path)
    (tlon-bae-commit-and-push "Translate " translation-path)
    (let ((label "Awaiting revision")
	  (assignee "worldsaround"))
      (tlon-bae-act-on-topic original-file label assignee)
      (org-clock-goto)
      (org-todo "DONE")
      (save-buffer)
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'" label assignee)
      (sit-for 5))))

(defun tlon-bae-finalize-revision ()
  "Finalize stylistic revision."
  (interactive)
  (save-buffer)
  (tlon-bae-check-branch "main")
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-file)
  (cl-multiple-value-bind
      (original-path translation-path original-file translation-file)
      (tlon-bae-set-paths)
    (fill-region (point-min) (point-max))
    (write-file translation-path)
    (tlon-bae-commit-and-push "Revise " translation-path)
    (let ((label "Awaiting check")
	  (assignee "worldsaround"))
      (tlon-bae-act-on-topic original-file label assignee)
      (org-clock-goto)
      (org-todo "DONE")
      (save-buffer)
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'" label assignee)
      (sit-for 5))))

(defun tlon-bae-finalize-check ()
  "Finalize accuracy check."
  (interactive)
  (save-buffer)
  (tlon-bae-check-branch "main")
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-file)
  (cl-multiple-value-bind
      (original-path translation-path original-file translation-file)
      (tlon-bae-set-paths)
    (fill-region (point-min) (point-max))
    (write-file translation-path)
    (tlon-bae-commit-and-push "Check " translation-path)
    (let ((label "Awaiting review")
	  (assignee "benthamite"))
      (tlon-bae-act-on-topic original-file label assignee)
      (org-clock-goto)
      (org-todo "DONE")
      (save-buffer)
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'" label assignee)
      (sit-for 5))))

(defun tlon-bae-finalize-review ()
  "Finalize review."
  (interactive)
  (save-buffer)
  (tlon-bae-check-branch "main")
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-file)
  (cl-multiple-value-bind
      (original-path translation-path original-file translation-file)
      (tlon-bae-set-paths)
    (fill-region (point-min) (point-max))
    (write-file translation-path)
    (tlon-bae-commit-and-push "Review " translation-path)
    (let ((label "Awaiting publication")
	  (assignee ""))
      (tlon-bae-act-on-topic original-file label assignee 'close)
      (tlon-bae-mark-clock-heading-as-done t)
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'" label assignee)
      (sit-for 5))))

;;; TTS

(defun tlon-bae-read-target-buffer ()
  "Return the buffer that `read-aloud' should read."
  (let ((buffer-list (cl-remove-if-not
		      (lambda (buffer)
			(string-match-p (regexp-quote "**markdown-output* # eww*") (buffer-name buffer)))
		      (buffer-list)))
	buffer)
    (cond ((= (length buffer-list) 1)
	   (setq buffer (car buffer-list)))
	  ((< (length buffer-list) 1)
	   (user-error "No buffer found"))
	  ((> (length buffer-list) 1)
	   (user-error "More than one buffer found")))
    buffer))

(defvar tlon-bae-read-aloud-next-action
  'read-aloud-buf)

(defun tlon-bae-read-start-or-stop ()
  "Start or stop reading the buffer, based on the most recent action."
  (funcall tlon-bae-read-aloud-next-action)
  (setq tlon-bae-read-aloud-next-action
	(if (eq tlon-bae-read-aloud-next-action 'read-aloud-buf)
	    'read-aloud-stop
	  'read-aloud-buf)))

(defun tlon-bae-read-target-start-or-stop ()
  "Start or stop reading the target buffer."
  (interactive)
  (let ((buffer (tlon-bae-read-target-buffer))
	(current-buffer (current-buffer)))
    (pop-to-buffer buffer)
    (when read-aloud--c-bufpos
      (goto-char read-aloud--c-bufpos))
    (read-aloud-buf)
    ;; we move point to the previous chunk, using the chunk divider
    ;; defined in `read-aloud--grab-text'
    (re-search-backward "[,.:!;]\\|\\(-\\|\n\\|\r\n\\)\\{2,\\}" nil t)
    (pop-to-buffer current-buffer)))

(defun tlon-bae--read-backward-or-forward (direction)
  "Move in DIRECTION in the target buffer."
  (interactive)
  (let ((buffer (tlon-bae-read-target-buffer))
	(current-buffer (current-buffer))
	(fun (if (eq direction 'backward)
		 're-search-backward
	       're-search-forward)))
    (when read-aloud--c-bufpos
      (read-aloud-buf))
    (pop-to-buffer buffer)
    (funcall fun "[,.:!;]\\|\\(-\\|\n\\|\r\n\\)\\{2,\\}" nil t 1)
    (pop-to-buffer current-buffer)))

(defun tlon-bae-read-backward ()
  "Move backward in the target buffer."
  (interactive)
  (tlon-bae--read-backward-or-forward 'backward))

(defun tlon-bae-read-forward ()
  "Move forward in the target buffer."
  (interactive)
  (tlon-bae--read-backward-or-forward 'forward))

;;; Sentence highlighting
;; TODO: (1) highlight sentence in target window; (2) diagnose why first
;; two characters in a sentence are matched to the previous sentence;
;; (3) diagnose performance issues, or else disable `post-command-hook'
;; and rely on other triggers; (4) use `lin-blue' as face for highlighting))))
(defvar tlon-bae-sentence-highlight-offset 0
  "Number of sentences to offset the sentence count in the source
window.")

(defun tlon-bae-sentence-highlight-offset-set ()
  "Set the sentence offset.
This command should be run from the source window."
  (interactive)
  (let ((source-window-sentences (count-sentences (point-min) (point)))
	target-window-sentences)
    (with-selected-window (cadr (window-list))
      (setq target-window-sentences (count-sentences (point-min) (point))))
    (setq tlon-bae-sentence-highlight-offset
	  (- source-window-sentences target-window-sentences))))

(defun tlon-bae-remove-source-overlays ()
  "Remove all existing overlays in the source window."
  (remove-overlays (point-min) (point-max)))

(defun tlon-bae-current-window-line ()
  "Get the current line number in the window."
  (save-excursion
    (let ((end (point)))
      (move-to-window-line 0)
      (count-screen-lines (point) end))))

(defun tlon-bae-highlight-corresponding-sentence ()
  "Highlight the corresponding sentence in the source text and unhighlight others."
  (interactive)
  (let* ((source-window (cadr (window-list)))
	 (target-window (car (window-list)))
	 (target-sentence-index)
	 (overlay (make-overlay (point) (point)))
	 (target-window-line (tlon-bae-current-window-line)))
    (with-selected-window target-window
      (save-excursion
	(backward-sentence)
	(setq target-sentence-index (count-sentences (point-min) (point)))))
    (with-selected-window source-window
      (tlon-bae-remove-source-overlays)
      (let ((beg)
	    (end))
	;; +1 because otherwise `count-sentences' throws an error
	(goto-char (1+ (point-min)))
	(while (< (count-sentences (point-min) (point))
		  (+ target-sentence-index tlon-bae-sentence-highlight-offset))
	  (forward-sentence))
	(setq beg (point))
	(forward-sentence)
	(setq end (point))
	(move-overlay overlay beg end (current-buffer))
	(overlay-put overlay 'face 'highlight)
	(backward-sentence)
	(recenter target-window-line)))))

(defvar tlon-bae-enable-automatic-highlighting nil
  "Whether to automatically highlight corresponding sentences.")

(defun tlon-bae-toggle-automatic-highlighting ()
  "Toggle automatic highlighting of corresponding sentences."
  (interactive)
  (if tlon-bae-enable-automatic-highlighting
      (progn
	(remove-hook 'post-command-hook 'tlon-bae-highlight-corresponding-sentence t)
	(setq tlon-bae-enable-automatic-highlighting nil)
	(with-selected-window (cadr (window-list))
	  (tlon-bae-remove-source-overlays))
	(message "Automatic sentence highlighting disabled."))
    (add-hook 'post-command-hook 'tlon-bae-highlight-corresponding-sentence nil t)
    (setq tlon-bae-enable-automatic-highlighting t)
    (message "Automatic sentence highlighting enabled.")))

;;; Checking

(defun tlon-bae-check-branch (branch)
  "Throw an error unless current buffer is in BAE branch BRANCH."
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (unless (string= (magit-get-current-branch) branch)
      (user-error "Please switch to the branch `%s' before proceeding" branch))
    t))

(defun tlon-bae-check-file ()
  "Throw an error unless current file matches file in clock."
  (unless (or (string= (file-name-nondirectory (buffer-file-name))
		       (tlon-bae-get-clock-file))
	      (string= (file-name-nondirectory (buffer-file-name))
		       (tlon-bae-get-translation-file (tlon-bae-get-clock-file))))
    (user-error "Current file does not match file in clock"))
  t)

(defun tlon-bae-check-label-and-assignee ()
  "Check that clocked action matches topic label and assignee matches user."
  (save-window-excursion
    (let* ((filename (tlon-bae-get-clock-file))
	   (topic (format "Job: `%s`" filename))
	   (clocked-label (car (rassoc (tlon-bae-get-clock-action) tlon-bae-label-actions))))
      (tlon-bae-magit-status)
      (magit-section-show-level-3-all)
      (goto-char (point-min))
      (if (search-forward topic nil t)
	  (let ((label (tlon-bae-forge-get-label-at-point))
		(assignee (alist-get
			   (tlon-bae-forge-get-assignee-at-point)
			   tlon-bae-github-users nil nil 'string=)))
	    (unless (string= clocked-label label)
	      (user-error "The `org-mode' TODO says the label is `%s', but the actual topic label is `%s'"
			  clocked-label label))
	    (unless (string= user-full-name assignee)
	      (user-error "The `org-mode' TODO says the assignee is `%s', but the actual topic assignee is `%s'"
			  user-full-name assignee))
	    t)
	(user-error "No topic found for %s" filename)))))

(defun tlon-bae-check-staged-or-unstaged (file-path)
  "Check if there are staged or unstaged changes in repo involving FILE-PATH."
  (catch 'found
    (dolist (flag '("staged" ""))
      (let ((git-command (format "git diff --%s --name-only %s" flag file-path)))
	(when (not (string-empty-p (shell-command-to-string git-command)))
	  (throw 'found t))))))

(defun tlon-bae-check-staged-or-unstaged-other-than (file-path)
  "Check if there are staged or unstaged changes in repo not involving FILE-PATH."
  (let* ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz)
	 (all-changes (magit-git-str "diff" "HEAD" "--" "."))
	 (filtered-changes (magit-git-str "diff" "HEAD" "--" file-path)))
    (unless (string= all-changes filtered-changes)
      (user-error "There are staged or unstaged changes in repo. Please commit or stash them before continuing"))))

(defun tlon-bae-act-on-topic (original-file label &optional assignee action)
  "Apply LABEL and ASSIGNEE to topic associated with ORIGINAL-FILE.
If ACTION is `convert', convert the existing issue into a pull
request. If ACTION is `close', close issue."
  (let ((topic (format "Job: `%s`" original-file))
	(default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (tlon-bae-magit-status)
    (magit-section-show-level-3-all)
    (goto-char (point-min))
    (if (search-forward topic nil t)
	(progn
	  (tlon-bae-set-parameters topic label)
	  (when assignee (tlon-bae-set-parameters topic assignee))
	  (search-forward topic nil t)
	  (pcase action
	    (`convert (call-interactively 'forge-create-pullreq-from-issue))
	    (`close (call-interactively 'forge-edit-topic-state))))
      (user-error "Could not find topic `%s' in Magit buffer" topic))))

(defun tlon-bae-set-parameters (topic &optional label-or-assignee)
  "Docstring."
  (let ((assignee-p (member label-or-assignee (mapcar 'car tlon-bae-github-users))))
    (search-forward topic nil t)
    (if assignee-p
	(tlon-bae-set-assignee label-or-assignee)
      (tlon-bae-set-label label-or-assignee))
    (goto-char (point-min))))

;;; Search

(defun tlon-bae-search-github (&optional search-string)
  "Search for SEARCH-STRING in BAE GitHub issues and pull requests."
  (interactive "sSearch string: ")
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (forge-search search-string)))

(defun tlon-bae-search-multi (search-string)
  "Search for SEARCH-STRING in BAE locally and issues and pull requests."
  (interactive "sSearch string: ")
  (tlon-bae-search-github search-string)
  (ps/window-split-if-unsplit)
  (other-window 1)
  (consult-ripgrep ps/dir-tlon-biblioteca-altruismo-eficaz search-string))

(defun tlon-bae-commit-and-push (prefix file)
  "Commit and push changes in BAE repo, with message 'PREFIX FILE'."
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (tlon-bae-check-staged-or-unstaged-other-than file)
    (when (string= (magit-get-current-branch) "main")
      (magit-pull-from-upstream nil)
      (sleep-for 2))
    (magit-stage-file file)
    ;; we check for staged or unstaged changes to FILE because
    ;; `magit-commit-create' interrupts the process if there aren't
    (when (tlon-bae-check-staged-or-unstaged file)
      (magit-commit-create (list "-m" (concat prefix (file-name-nondirectory file)))))
    (call-interactively #'magit-push-current-to-pushremote)))

(defun tlon-bae-commit-when-slug-at-point (&optional prefix)
  "Commit and push change when point is on a slug.
Unless PREFIX is specified, prompt user to select between
'Revise' and 'Translate'."
  (interactive)
  (unless (eq major-mode 'magit-status-mode)
    (user-error "Please run this command in the Magit status buffer"))
  (beginning-of-line)
  (when (looking-at ".+?\\(\\w+--.+?.md\\)")
    (let ((file (match-string 1)))
      (tlon-bae-commit-and-push prefix file))))

;;; Change topic properties

(defun tlon-bae-select-label ()
  "Prompt the user to select a LABEL."
  (let ((label (completing-read "What should be the label? "
				tlon-bae-label-actions)))
    label))

(defun tlon-bae-set-label (label)
  "Apply LABEL to topic at point.
Note that this only works for topics listed in the main buffer."
  (interactive
   (list (tlon-bae-select-label)))
  (let* ((topic (forge-get-topic (forge-topic-at-point)))
	 (repo  (forge-get-repository topic))
	 (crm-separator ","))
    (forge--set-topic-labels
     repo topic (list label))))

(defun tlon-bae-select-assignee ()
  "Prompt the user to select an ASSIGNEE.
The prompt defaults to the current user."
  (let ((assignee (completing-read "Who should be the assignee? "
				   tlon-bae-github-users nil nil
				   (tlon-bae-find-key-in-alist
				    user-full-name
				    tlon-bae-github-users))))
    assignee))

(defun tlon-bae-set-assignee (assignee)
  "Make ASSIGNEE the assignee of topic at point."
  (interactive
   (list (tlon-bae-select-assignee)))
  (let* ((topic (forge-get-topic (forge-topic-at-point)))
	 (repo  (forge-get-repository topic))
	 (value (closql--iref topic 'assignees))
	 (choices (mapcar #'cadr (oref repo assignees)))
	 (crm-separator ","))
    (forge--set-topic-assignees
     repo topic
     (list assignee))))

(defun tlon-bae-set-initial-label-and-assignee ()
  "Set label to `Awaiting processing' and assignee to current user."
  (tlon-bae-set-label "Awaiting processing")
  (tlon-bae-set-assignee (tlon-bae-find-key-in-alist user-full-name tlon-bae-github-users)))

;; this is just a slightly tweaked version of `forge-edit-topic-labels'.
;; It differs from that function only in that it returns the selection
;; rather than submitting it.
(defun tlon-bae-forge-get-label (topic)
  "Return the label of the current TOPIC.
If the topic has more than one label, return the first."
  (interactive (list (forge-read-topic "Edit labels of")))
  (let* ((topic (forge-get-topic topic))
	 (repo  (forge-get-repository topic))
	 (crm-separator ","))
    (car (magit-completing-read-multiple
	  "Labels: "
	  (mapcar #'cadr (oref repo labels))
	  nil t
	  (mapconcat #'car (closql--iref topic 'labels) ",")))))

(defun tlon-bae-forge-get-assignee (topic)
  "Return the assignee of the current TOPIC.
If the topic has more than one assignee, return the first."
  (interactive (list (forge-read-topic "Edit assignees of")))
  (let* ((topic (forge-get-topic topic))
	 (repo  (forge-get-repository topic))
	 (value (closql--iref topic 'assignees))
	 (choices (mapcar #'cadr (oref repo assignees)))
	 (crm-separator ","))
    (car (magit-completing-read-multiple
	  "Assignees: " choices nil
	  (if (forge--childp repo 'forge-gitlab-repository)
	      t ; Selecting something else would fail later on.
	    'confirm)
	  (mapconcat #'car value ",")))))

;; This function simply confirms the selection offered to the user by
;; `tlon-bae-forge-get-label'. I don't know how to do this
;; properly with `magit-completing-read-multiple', so I just simulate a
;; RET keypress.
(defun tlon-bae-forge-get-label-at-point ()
  "Return the label of the topic at point.
If the topic has more than one label, return the first."
  (let ((exit-minibuffer-func (lambda () (exit-minibuffer))))
    (minibuffer-with-setup-hook
	(lambda ()
	  (add-hook 'post-command-hook exit-minibuffer-func t t))
      (tlon-bae-forge-get-label (forge-current-topic)))))

(defun tlon-bae-forge-get-assignee-at-point ()
  "Return the assignee of the topic at point.
If the topic has more than one assignee, return the first."
  (let ((exit-minibuffer-func (lambda () (exit-minibuffer))))
    (minibuffer-with-setup-hook
	(lambda ()
	  (add-hook 'post-command-hook exit-minibuffer-func t t))
      (tlon-bae-forge-get-assignee (forge-current-topic)))))

(defun tlon-bae-open-original-or-translation ()
  "Open the translation if visiting the original, and vice versa."
  (interactive)
  (let* ((current-file (file-name-nondirectory (buffer-file-name))))
    (alist-get current-file tlon-bae-translation-alist
	       (lambda (key default) default))))

(defvar tlon-bae-label-actions
  '(("Awaiting processing" . "Process")
    ("Awaiting translation" . "Translate")
    ("Awaiting check" . "Check")
    ("Awaiting revision" . "Revise")
    ("Awaiting review" . "Review")
    ("Awaiting publication" . "Publish")
    ("Awaiting rewrite" . "Rewrite")
    ("Glossary" . "Respond")
    ("Misc" . "Misc"))
  "Alist of topic labels and corresponding actions.")

(defvar tlon-bae-label-bindings
  '(("Awaiting processing" . "p")
    ("Awaiting translation" . "t")
    ("Awaiting check" . "c")
    ("Awaiting revision" . "r")
    ("Awaiting review" . "v")
    ("Awaiting publication" . "u")
    ("Awaiting rewrite" . "w")
    ("Glossary" . "g")
    ("Misc" . "m"))
  "Alist of topic labels and corresponding key bindings.")

(defvar tlon-bae-github-users
  '(("fstafforini" . "Federico Stafforini")
    ("worldsaround" . "Leonardo Picón")
    ("benthamite" . "Pablo Stafforini"))
  "Alist of GitHub usernames and corresponding full names.")

(defvar tlon-bae-system-users
  '(("Federico Stafforini" . "Federico Stafforini")
    ("cartago" . "Leonardo Picón")
    ("Pablo Stafforini" . "Pablo Stafforini"))
  "Alist of system usernames and corresponding full names.")

(defun tlon-bae-find-key-in-alist (value alist)
  "Find the corresponding key for a VALUE in ALIST."
  (let ((pair (cl-find-if (lambda (x) (equal (cdr x) value)) alist)))
    (when pair
      (car pair))))

(defun tlon-bae-label-match (label)
  "Return a suitable action for the LABEL of topic at point.
The function relies on the alist `tlon-bae-label-actions' to
determine an appropriate action from the topic's label."
  (let* ((label (or label
		    (tlon-bae-forge-get-label-at-point)))
	 (action (alist-get label tlon-bae-label-actions nil nil 'string=)))
    action))

(defun tlon-bae-add-to-glossary (english spanish)
  "Add a new entry to the glossary for ENGLISH and SPANISH terms."
  (interactive "sEnglish: \nsSpanish: ")
  (tlon-bae-check-branch "main")
  (let ((glossary (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "etc/Glossary.csv"))
	(default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (with-current-buffer (find-file-noselect glossary)
      (goto-char (point-max))
      (insert (format "\n\"%s\",\"%s\",\"EN\",\"ES\"" english spanish))
      (goto-char (point-min))
      (flush-lines "^$")
      (save-buffer))
    (magit-pull-from-upstream nil)
    ;; if there are staged files, we do not commit or push the changes
    (unless (magit-staged-files)
      (magit-stage-file glossary)
      (magit-commit-create (list "-m" (format  "Glossary: add \"%s\"" english)))
      (call-interactively #'magit-push-current-to-pushremote))))

(defun tlon-bae-add-to-work-correspondece (original spanish)
  "Add a new entry to the correspondece file for ORIGINAL and SPANISH terms."
  (interactive "sOriginal: \nsSpanish: ")
  (tlon-bae-check-branch "main")
  (let ((glossary (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "etc/work-correspondence.csv"))
	(default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (with-current-buffer (find-file-noselect glossary)
      (goto-char (point-max))
      (insert (format "\n\"%s\",\"%s\"" original spanish))
      (goto-char (point-min))
      (flush-lines "^$")
      (save-buffer))
    (magit-stage-file glossary)
    (magit-commit-create (list "-m" (format  "Correspondence: add \"%s\""
					     (s-truncate (- vc-git-log-edit-summary-max-len 25) original))))
    (call-interactively #'magit-push-current-to-pushremote)))

(defmacro tlon-bae-create-file-opening-command (file-path)
  "Create a command to open file in FILE-PATH."
  (let* ((file-base (downcase (file-name-base file-path)))
	 (file-name (file-name-nondirectory file-path))
	 (command-name (intern (concat "tlon-bae-open-" file-base))))
    `(defun ,command-name ()
       ,(format "Open `%s'." file-name)
       (interactive)
       (find-file (file-name-concat
		   ps/dir-tlon-biblioteca-altruismo-eficaz
		   ,file-path)))))

(defmacro tlon-bae-create-dir-opening-command (&optional dir-path)
  "Create a command to open file in DIR-PATH.
If DIR-PATH is nil, create a command to open the BAE repository."
  (let* ((dir-path-hyphenated (when dir-path
				(s-replace "/" "-" dir-path)))
	 (command-name (if dir-path
			   (intern (concat "tlon-bae-open-" dir-path-hyphenated))
			 'tlon-bae-open-repo)))
    `(defun ,command-name ()
       ,(format "Open `%s'." dir-path)
       (interactive)
       (find-file (file-name-concat
		   ps/dir-tlon-biblioteca-altruismo-eficaz
		   ,dir-path)))))

(tlon-bae-create-file-opening-command "etc/Glossary.csv")
(tlon-bae-create-file-opening-command "etc/tag-correspondence.csv")
(tlon-bae-create-file-opening-command "etc/work-correspondence.csv")
(tlon-bae-create-file-opening-command "etc/new.bib")
(tlon-bae-create-file-opening-command "etc/old.bib")
(tlon-bae-create-file-opening-command "etc/finished.bib")
(tlon-bae-create-file-opening-command "etc/pending.bib")
(tlon-bae-create-file-opening-command "etc/tag-slugs.txt")
(tlon-bae-create-file-opening-command "../tlon-docs/bae.org")
(tlon-bae-create-file-opening-command "readme.md")

(tlon-bae-create-dir-opening-command)
(tlon-bae-create-dir-opening-command "originals/posts")
(tlon-bae-create-dir-opening-command "originals/tags")
(tlon-bae-create-dir-opening-command "translations/posts")
(tlon-bae-create-dir-opening-command "translations/tags")
(tlon-bae-create-dir-opening-command "etc")


(transient-define-prefix tlon-bae-dispatch ()
  "Dispatch a `tlon-bae' command."
  [["Main"
    ("j" "job"                          tlon-bae-create-job)
    ("r" "dwim"                         tlon-bae-dwim)
    ("m" "magit"                         tlon-bae-magit-status)
    ("n" "forge"                        tlon-bae-forge)
    """Package"
    ("p p" "update"                      tlon-bae-update)
    ("p l" "load variables"             tlon-bae-load-variables)
    ("p v" "version"                    tlon-bae-version)
    ]
   ["Add"
    ("a a" "to glossary"                tlon-bae-add-to-glossary)
    ("a w" "to work correspondence"     tlon-bae-add-to-work-correspondece)
    """Search"
    ("s s" "repo"                       tlon-bae-search-github)
    ("s m" "multi"                      tlon-bae-search-multi)
    ]
   ["Open file"
    ("f f" "counterpart"                tlon-bae-open-counterpart)
    ("f g" "Glossary.csv"               tlon-bae-open-glossary)
    ("f n" "new.bib"                    tlon-bae-open-new)
    ("f o" "old.bib"                    tlon-bae-open-old)
    ("f i" "finished.bib"               tlon-bae-open-finished)
    ("f p" "pending.bib"                tlon-bae-open-pending)
    ("f m" "manual.md"                  tlon-bae-open-bae)
    ("f r" "readme.md"                  tlon-bae-open-readme)
    ]
   ["Open directory"
    ("d d" "repo"                       tlon-bae-open-repo)
    ("d P" "originals > posts"          tlon-bae-open-originals-posts)
    ("d T" "originals > tags"           tlon-bae-open-originals-tags)
    ("d p" "translations > posts"       tlon-bae-open-translations-posts)
    ("d t" "translations > tags"        tlon-bae-open-translations-tags)
    ("d e" "etc"                        tlon-bae-open-etc)
    ]
   ["Browse"
    ("b b" "file"                tlon-bae-browse-file)
    ("b r" "repo"                tlon-bae-browse-repo)
    """File changes"
    ("h h" "Log"                        magit-log-buffer-file)
    ("h d" "Diffs since last user change"  tlon-bae-log-buffer-latest-user-commit)
    ("h e" "Ediff with last user change"  tlon-bae-log-buffer-latest-user-commit-ediff)]
   ["Clock"
    ("c c" "Issue"                        tlon-bae-open-clock-topic)
    ("c f" "File"                         tlon-bae-open-clock-file )
    ("c o" "Heading"                     org-clock-goto)
    """Issue"
    ("i i" "Open counterpart"                tlon-bae-open-forge-counterpart)
    ("i I" "Open file"                tlon-bae-open-forge-file)
    ]
   ]
  )

(defun tlon-bae-browse-file ()
  "Browse the current file in the BAE repository."
  (interactive)
  (let* ((url-suffix (file-relative-name (buffer-file-name) ps/dir-tlon-biblioteca-altruismo-eficaz))
	 (url-prefix "https://github.com/tlon-team/biblioteca-altruismo-eficaz/blob/main/"))
    (browse-url (concat url-prefix url-suffix))))

(defun tlon-bae-browse-repo ()
  "Browse the BAE repository."
  (interactive)
  (browse-url "https://github.com/tlon-team/biblioteca-altruismo-eficaz"))

;;;

(defconst tlon-bae-eaf-api-url
  "https://forum.effectivealtruism.org/graphql"
  "URL for the EAF GraphQL API endpoint.")

(defvar tlon-bae-eaf-objects
  '(post tag)
  "List of entities supported by the EAF GraphQL API.")

;;; request

(defun tlon-bae-eaf-post-query (id)
  "Return an EA Forum GraphQL query for post whose ID is ID."
  (concat "{\"query\":\"{\\n  post(\\n    input: {\\n      selector: {\\n        _id: \\\""
	  id
	  "\\\"\\n      }\\n    }\\n  ) {\\n    result {\\n      _id\\n      postedAt\\n      url\\n      canonicalSource\\n      title\\n      contents {\\n        markdown\\n        ckEditorMarkup\\n      }\\n      slug\\n      commentCount\\n      htmlBody\\n      baseScore\\n      voteCount\\n      pageUrl\\n      legacyId\\n      question\\n      tableOfContents\\n      author\\n      user {\\n        username\\n        displayName\\n        slug\\n        bio\\n      }\\n      coauthors {\\n        _id\\n        username\\n        displayName\\n        slug\\n      }\\n    }\\n  }\\n}\\n\"}"))

(defun tlon-bae-eaf-tag-query (slug)
  "Return an EA Forum GraphQL query for tag whose slug is SLUG."
  (concat "{\"query\":\"{\\n  tag(input: { selector: { slug: \\\""
	  slug
	  "\\\" } }) {\\n    result {\\n      name\\n      slug\\n      description {\\n        html\\n      }\\n      parentTag {\\n        name\\n      }\\n    }\\n  }\\n}\\n\"}"))

(defun tlon-bae-eaf-request (id-or-slug &optional async)
  "Run an EAF request for ID-OR-SLUG.
If ASYNC is t, run the request asynchronously."
  (let* ((object (tlon-bae-eaf-get-object id-or-slug))
	 (fun (pcase object
		('post 'tlon-bae-eaf-post-query)
		('tag 'tlon-bae-eaf-tag-query)
		(_ (error "Invalid object: %S" object))))
	 (query (funcall fun id-or-slug))
	 response)
    (request
      tlon-bae-eaf-api-url
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data query
      :parser 'json-read
      :sync (not async)
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq response (cdr (assoc 'data data)))))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error: %S" error-thrown))))
    response))

(defun tlon-bae--eaf-get-post-result (response)
  "docstring"
  (let* ((post (cdr (assoc 'post response)))
	 (result (cdr (assoc 'result post))))
    result))

(defun tlon-bae-eaf-get-post-id (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-get-post-result response))
	 (id (cdr (assoc '_id result))))
    id))

(defun tlon-bae-eaf-get-post-html (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-get-post-result response))
	 (html (cdr (assoc 'htmlBody result))))
    html))

(defun tlon-bae-eaf-get-post-title (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-get-post-result response))
	 (title (cdr (assoc 'title result))))
    title))

(defun tlon-bae-eaf-get-post-author (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-get-post-result response))
	 (author (cdr (assoc 'author result))))
    author))

(defun tlon-bae-eaf-get-post-username (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-get-post-result response))
	 (user (cdr (assoc 'user result)))
	 (username (cdr (assoc 'username user))))
    username))

(defun tlon-bae--eaf-get-tag-result (response)
  "docstring"
  (let* ((tag (cdr (assoc 'tag response)))
	 (result (cdr (assoc 'result tag))))
    result))

(defun tlon-bae-eaf-get-tag-slug (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-get-tag-result response))
	 (slug (cdr (assoc 'slug result))))
    slug))

(defun tlon-bae-eaf-get-tag-html (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-get-tag-result response))
	 (description (cdr (assoc 'description result)))
	 (html (cdr (assoc 'html description))))
    html))

(defun tlon-bae-eaf-get-tag-title (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-get-tag-result response))
	 (title (cdr (assoc 'name result))))
    (tlon-bae-shorten-title title)))

;;; html import

(defvar tlon-bae-pandoc-convert-from-file
  "pandoc -s '%s' -t markdown -o '%s'"
  "Command to convert from HTML file to markdown.")

(defvar tlon-bae-pandoc-convert-from-url
  "pandoc -s -r html '%s' -o '%s'"
  "Command to convert from URL to markdown.")

(defun tlon-bae-save-html-to-file (html)
  "Save the HTML string HTML to a temporary file."
  (let ((filename (make-temp-file "tlon-bae-request-" nil ".html")))
    (with-temp-file filename
      (insert html))
    filename))

;;; translation

(defun tlon-bae-file-to-string (file-path)
  "Read the contents of FILE-PATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (unfill-region (point-min) (point-max))
    (buffer-string)))

(defun tlon-bae-gpt-rewrite ()
  "Docstring."
  (interactive)
  (let* ((text (if (region-active-p)
		   (buffer-substring-no-properties (region-beginning) (region-end))
		 (read-string "Text to rewrite: "))))
    (gptel-request
     (format "Por favor, genera las mejores diez variantes del siguiente texto castellano: '%s'. Por favor, devuelve todas las variantes en una única linea, separadas por '|'. No insertes un espacio ni antes ni después de '|'. No agregues ningún comentario aclaratorio: solo necesito la lista de variantes. A modo de ejemplo, para la expresión 'búsqueda de poert' el texto a devolver sería: 'ansia de poder|ambición de poder|búsqueda de autoridad|sed de poder|afán de poder|aspiración de poder|anhelo de poder|deseo de control|búsqueda de dominio|búsqueda de control' (esta lista solo pretende ilustrar el formato en que debes presentar tu respuesta). Gracias!" text)
     :callback
     (lambda (response info)
       (if (not response)
	   (message "gptel-quick failed with message: %s" (plist-get info :status))
	 (let* ((variants (split-string response "|"))
		(variant (completing-read "Variant: " variants)))
	   (delete-region (region-beginning) (region-end))
	   (kill-new variant)))))))

(defun tlon-bae-gpt-translate (text)
  "Docstring."
  (interactive "sText to translate: ")
  (gptel-request
   (format "Generate the best ten Spanish translations of the following English text: '%s'. Please return each translation on the same line, separated by '|'. Do not add a space either before or after the '|'. Do not precede your answer by 'Here are ten Spanish translations' or any comments of that sort: just return the translations. An example return string for the word 'beautiful' would be: 'bello|bonito|hermoso|atractivo' (etc). Thanks!" text)
   :callback
   (lambda (response info)
     (if (not response)
	 (message "gptel-quick failed with message: %s" (plist-get info :status))
       (let ((translations (split-string response "|")))
	 (kill-new (completing-read "Translation: " translations)))))))

(defun tlon-bae-gpt-translate-file (file)
  "Docstring."
  (let* ((counterpart (tlon-bae-get-counterpart-filename file))
	 (target-path (concat
		       (file-name-sans-extension counterpart)
		       "--gpt-translated.md")))
    (gptel-request
     (concat "Translate the following text into Spanish:\n\n"
	     (tlon-bae-file-to-string file))
     :callback
     (lambda (response info)
       (if (not response)
	   (message "gptel-quick failed with message: %s" (plist-get info :status))
	 (with-temp-buffer
	   (insert response)
	   (write-region (point-min) (point-max) target-path)))))))

(defun tlon-bae-create-file-from-commit (file-path commit-hash)
  "Create a temporary file with the state of the FILE-PATH at the COMMIT-HASH and return this path."
  (let* ((file-name (file-name-nondirectory file-path))
	 (file-directory (file-name-directory file-path))
	 (repo-root (locate-dominating-file file-path ".git"))
	 (relative-file-path (file-relative-name file-path repo-root))
	 (new-file-name (format "%s_%s" commit-hash file-name))
	 (new-file-path (make-temp-file new-file-name nil ".md"))
	 (git-command
	  (format "git show %s:\"%s\""
		  commit-hash
		  (shell-quote-argument relative-file-path))))
    (let ((default-directory repo-root))
      (with-temp-buffer
	(insert (shell-command-to-string git-command))
	(write-file new-file-path)))
    (message "File created: %s" new-file-path)
    new-file-path))

(defun tlon-bae-split-footnotes-into-separate-paragraphs ()
  "Split footnotes into separate paragraphs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[\\^[[:digit:]]\\{1,3\\}\\]:\\)" nil t)
      (replace-match "\n\n\\1"))))

(defun tlon-bae-load-variables ()
  "Load the variables to reflect changes to the files in the `etc' directory."
  (interactive)
  (setq tlon-bae-tag-correspondence
	(tlon-bae-csv-file-to-alist (file-name-concat
				     ps/dir-tlon-biblioteca-altruismo-eficaz
				     "etc/tag-correspondence.csv"))
	tlon-bae-work-correspondence
	(tlon-bae-csv-file-to-alist (file-name-concat
				     ps/dir-tlon-biblioteca-altruismo-eficaz
				     "etc/work-correspondence.csv"))
	tlon-bae-tag-slugs
	(tlon-bae-read-urls-from-file (file-name-concat
				       ps/dir-tlon-biblioteca-altruismo-eficaz
				       "etc/tag-slugs.txt"))
	tlon-bae-wiki-urls (list ""))
  (dolist (slug tlon-bae-tag-slugs)
    (add-to-list 'tlon-bae-wiki-urls
		 (format "https://forum.effectivealtruism.org/topics/%s" slug)))
  (tlon-bae-load-post-correspondence)
  (message "Variables loaded."))

(tlon-bae-load-variables)

(provide 'tlon-bae)
;;; tlon-bae.el ends here
