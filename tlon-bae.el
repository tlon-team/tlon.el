;;; tlon-bae.el --- A collection of convenience functions for the Tlön BAE project. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.9
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
(require 'cl-lib)
(require 'transient)

;;; Version
(setq tlon-bae-version "0.1.7")

(defun tlon-bae-version ()
  "Return the version of the Tlön BAE package."
  (interactive)
  (message "`tlon-bae' version %s" tlon-bae-version))

(defun tlon-bae-update ()
  "Update `tlon-bae' package."
  (interactive)
  (let* ((default-directory (file-name-concat user-emacs-directory "elpaca/repos/tlon-bae/"))
	 (tlon-bae-file (file-name-concat default-directory "tlon-bae.el")))
    (shell-command "git pull")
    (with-current-buffer (find-file-noselect tlon-bae-file)
      (eval-buffer)))
  (message "Package updated. %s" tlon-bae-version))

(defun tlon-bae-forge ()
  "Launch the Forge dispatcher in the BAE directory."
  (interactive)
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (call-interactively 'forge-dispatch)))

(defun tlon-bae-orgit-capture ()
  "Capture a new Magit/Forge task."
  (interactive)
  (let ((assignee (alist-get
		   (tlon-bae-forge-get-assignee-at-point)
		   tlon-bae-github-users nil nil 'string=))
	(label (tlon-bae-forge-get-label-at-point)))
    ;; when the topic has neither a label nor an assignee, we offer to
    ;; process it as a new job
    (if (not (or assignee label))
	(if (y-or-n-p "Process as a new job? ")
	    (tlon-bae-create-new-job t)
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
				  (get-file-buffer ps/file-tlon-bae))))
	  (let ((action (alist-get label tlon-bae-label-actions nil nil #'string=))
		(binding (upcase (alist-get label tlon-bae-label-bindings nil nil #'string=))))
	    (kill-new (format "%s %s" action org-link))
	    (org-capture nil (concat "tb" binding))
	    ;; refile under job
	    (org-refile nil nil (list nil (buffer-file-name) nil refile-position))
	    (ps/org-refile-goto-latest))
	(when (y-or-n-p "No job found for this issue. Create new job?")
	  (tlon-bae-create-new-job))))))

(defun tlon-bae-create-new-job (&optional set-topic)
  "Create new job.
If SET-TOPIC is non-nil, set topic label to 'Awaiting processing'
and assignee to the current user."
  (save-window-excursion
    (when set-topic
      (tlon-bae-set-initial-label-and-assignee))
    (orgit-store-link nil)
    (let ((job-name (cadr (nth 0 org-stored-links))))
      (kill-new (format "%s" job-name)))
    (org-capture nil "tbJ"))
  ;;  we run `tlon-bae-orgit-capture' again to now properly capture the issue
  (sleep-for 3)
  (tlon-bae-orgit-capture))

;;; File processing
(defun tlon-bae-generate-file-path (&optional lastname title tag translation)
  "Return a file name based on user supplied information.
TITLE is the title of the work. If TAG is 'tag, use `tag' as
lastname. If TRANSLATION is non-nil, use `translatons' in the
file path."
  (let* ((tag (or (eq tag 'tag)
		  (string= lastname "tag")))
	 (lastname (or lastname (if tag
				    "tag"
				  (read-string "Last name [only first author if multi-authored]: "))))
	 (title (or title (tlon-bae-shorten-title (read-string "Title: "))))
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

(defun tlon-bae-eaf-generate-file-path (response)
  "Docstring."
  (let* ((title (tlon-bae-shorten-title (tlon-bae-eaf-post-get-title response)))
	 ;; sometimes the 'author' field is empty so we use the 'user' field instead
	 (identifier (tlon-bae-eaf-get-identifier-from-response response))
	 (author (or (tlon-bae-eaf-post-get-author response)
		     (tlon-bae-eaf-post-get-user response)))
	 ;; we past 'tag' as lastname if object is tag
	 (lastname (if author
		       (car (last (split-string author "[ _-]")))
		     "tag"))
	 (file-path (tlon-bae-generate-file-path lastname title))
	 (filename (file-name-nondirectory file-path))
	 (filename-reviewed (read-string "Job name: " filename)))
    (unless (string= filename filename-reviewed)
      (setq file-path (file-name-concat (file-name-directory file-path) filename-reviewed)))
    file-path))

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

(defun tlon-bae-log-buffer-diff (file)
  "Show changes in FILE since the latest commit by the current user."
  (let* ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz)
	 (user (tlon-bae-find-key-in-alist user-full-name tlon-bae-system-users))
	 ;; get most recent commit in FILE by USER
	 (output (shell-command-to-string (format "git log --pretty=format:'%%h %%an %%s' --follow -- '%s' | grep -m 1 '%s' | awk '{print $1}'" file user)))
	 (commit (car (split-string output "\n"))))
    (magit-diff-range commit nil (list file))))

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

(defvar tlon-markdown-eawiki-escaped-quotes
  "\\\\\\\\\\\\\""
  "Regexp to match escaped quotes.")

(defun tlon-bae-markdown-eaf-cleanup (&optional buffer)
  "Cleanup the BUFFER visiting an EA Wiki entry."
  (interactive)
  (when (not (eq major-mode 'markdown-mode))
    (user-error "Not in a Markdown buffer"))
  (save-excursion
    (unfill-region (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "{.underline}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    ;; remove extra pair of square brackets in links
    (while (re-search-forward "\\[\\[\\(.*?\\)\\]\\](" nil t)
      (replace-match (format "[%s](" (match-string 1))))
    (goto-char (point-min))
    ;; remove asterisks surrounding links
    (while (re-search-forward " ??\\* ??\\[\\*\\[\\(.*?\\)\\]\\*\\](\\(.*?\\)) ??\\* ??" nil t)
      (replace-match (format " [%s](%s)" (match-string 1) (match-string 2))))
    (goto-char (point-min))
    ;; move double asterisks outside of link
    (while (re-search-forward "\\[\\*\\*\\[\\(.*?\\)\\]\\*\\*\\](\\(.*?\\))" nil t)
      (replace-match (format "**[%s](%s)**" (match-string 1) (match-string 2))))
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
      (replace-match (format "[^%s] " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-source2 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-target nil t)
      (replace-match (format "[^%s]:" (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-markdown-eawiki-links nil t)
      (replace-match (format "[%s](%s)" (match-string 1) (match-string 2)) nil t))
    (goto-char (point-min))
    (while (re-search-forward tlon-markdown-eawiki-escaped-quotes nil t)
      (replace-match "\""))
    (goto-char (point-min))
    ;; remove double asterisks surrounding headings
    (while (re-search-forward "# \\*\\*\\(.*\\)\\*\\* *?$" nil t)
      (replace-match (format "# %s" (match-string 1))))
    (fill-region (point-min) (point-max))
    (save-buffer)))

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
  "Split CSV LINE into fields, considering quotes, using SEPARATOR (defaults to comma)."
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

(defun tlon-bae-load-variables ()
  "Load the variables to reflect changes to the files in the `etc' directory."
  (interactive)
  (setq tlon-bae-post-correspondence
	(tlon-bae-csv-file-to-alist (file-name-concat
				     ps/dir-tlon-biblioteca-altruismo-eficaz
				     "etc/post-correspondence.csv"))
	tlon-bae-tag-correspondence
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
  (message "Variables loaded."))

(tlon-bae-load-variables)

(defun tlon-bae-insert-tag-slug ()
  "Insert an EA Wiki slug at point."
  (interactive)
  (insert (completing-read "URL: " tlon-bae-tag-slugs)))

(defun tlon-bae-insert-tag-url ()
  "Insert an EA Wiki slug at point."
  (interactive)
  (insert (completing-read "URL: " tlon-bae-wiki-urls)))

(defun tlon-bae-get-counterpart ()
  "Get the counterpart of the current file."
  (let* ((current-file (buffer-file-name))
	 (current-dir (file-name-directory current-file))
	 (current-file-name (file-name-nondirectory current-file))
	 (correspondence-alist (cond
				((string-match "/originals/posts/" current-dir)
				 tlon-bae-post-correspondence)
				((string-match "/originals/tags/" current-dir)
				 tlon-bae-tag-correspondence)
				((string-match "/translations/posts/" current-dir)
				 (mapcar (lambda (pair) (cons (cdr pair) (car pair))) tlon-bae-post-correspondence))
				((string-match "/translations/tags/" current-dir)
				 (mapcar (lambda (pair) (cons (cdr pair) (car pair))) tlon-bae-tag-correspondence)))))
    (when-let ((new-file-name (cdr (assoc current-file-name correspondence-alist)))
	       (new-dir (if (string-match "/originals/" current-dir)
			    (replace-regexp-in-string "/originals/" "/translations/" current-dir)
			  (replace-regexp-in-string "/translations/" "/originals/" current-dir))))
      (expand-file-name new-file-name new-dir))))

(defun tlon-bae-open-counterpart ()
  "Open the counterpart of the current file."
  (interactive)
  (if-let ((new-file (tlon-bae-get-counterpart)))
      (progn
	(find-file new-file)
	(message "Switched to corresponding file: %s" new-file))
    (user-error "No corresponding file found")))

;;; EAF validation

(defvar tlon-bae-eaf-post-id-regexp
  "\\([[:alnum:]]\\{17\\}\\)"
  "Regular expression for validating post IDs.")

(defvar tlon-bae-eaf-tag-slug-regexp
  "\\([[:alnum:]-]*\\)"
  "Regular expression for validating tag slugs.")

(defun tlon-bae-eaf-post-id-p (identifier)
  "Return t if IDENTIFIER is a post ID, nil otherwise."
  (not (not (string-match (format "^%s$" tlon-bae-eaf-post-id-regexp) identifier))))

(defun tlon-bae-eaf-tag-slug-p (identifier)
  "Return t if IDENTIFIER is a tag slug, nil otherwise."
  (not (not (string-match (format "^%s$" tlon-bae-eaf-tag-slug-regexp) identifier))))

(defun tlon-bae-eaf-get-identifier-from-url (url)
  "Return the EAF identifier in URL, if found.
The identifier is either a post ID or a tag slug."
  (interactive "sURL: ")
  (let ((identifier (cond ((string-match (format "^.+?forum.effectivealtruism.org/posts/%s"
						 tlon-bae-eaf-post-id-regexp)
					 url)
			   (match-string-no-properties 1 url))
			  ((string-match (format "^.+?forum.effectivealtruism.org/topics/%s"
						 tlon-bae-eaf-tag-slug-regexp) url)
			   (match-string-no-properties 1 url)))))
    identifier))

(defun tlon-bae-eaf-get-identifier-from-response (response)
  "Return the EAF identifier from Json RESPONSE.
The identifier is either a post ID or a tag slug."
  (or (tlon-bae-eaf-post-get-id response)
      (tlon-bae-eaf-tag-get-slug response)))

(defun tlon-bae-eaf-get-object (identifier)
  "Return the EAF object in IDENTIFIER."
  (let ((object (cond ((tlon-bae-eaf-post-id-p identifier)
		       'post)
		      ((tlon-bae-eaf-tag-slug-p identifier)
		       'tag)
		      (t (user-error "Invalid identifier: %S" identifier)))))
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

(defun tlon-bae-get-translation-file (original-file)
  "Return file that translates ORIGINAL-FILE."
  (or (alist-get original-file tlon-bae-post-correspondence nil nil #'equal)
      (alist-get original-file tlon-bae-tag-correspondence nil nil #'equal)))

(defun tlon-bae-get-original-file (translation-file)
  "Return file that TRANSLATION-FILE translates."
  (cl-loop for (key . val) in tlon-bae-tag-correspondence
	   when (equal val translation-file)
	   return key))

(defun tlon-bae-get-forge-file ()
  "Get the file of the topic at point or in current forge buffer."
  (unless (or (derived-mode-p 'magit-status-mode)
	      (derived-mode-p 'forge-topic-mode))
    (user-error "I'm not in a forge buffer"))
  (let* ((inhibit-message t)
	 (captured (cadr (call-interactively #'orgit-store-link))))
    (setq org-stored-links (cdr org-stored-links))
    (if (string-match "`\\(.+?\\)`" captured)
	(match-string 1 captured)
      (user-error "I wasn't able to find a file at point or in the forge buffer"))))

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

(defun tlon-bae-open-forge-file ()
  "Open the file of the topic at point or in current forge buffer."
  (interactive)
  (if-let* ((filename (tlon-bae-get-forge-file))
	    (dir (tlon-bae-find-subdirectory-containing-file filename))
	    (file (file-name-concat dir filename)))
      (find-file file)
    (user-error "I couldn't find `%s' in `%s'"
		filename ps/dir-tlon-biblioteca-altruismo-eficaz)))

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

(defun tlon-bae-copy-file-contents (file)
  "Copy contents of FILE to kill ring."
  (with-current-buffer (find-file-noselect file)
    (unfill-region (point-min) (point-max))
    (copy-region-as-kill (point-min) (point-max))
    (fill-region (point-min) (point-max))
    (save-buffer)
    (message "The contents of `%s' have been copied to the to kill ring, in case you want to paste them into the DeepL editor."
	     (file-name-nondirectory file))))

(defun tlon-bae-set-original-path ()
  "Docstring."
  (let* ((filename (tlon-bae-get-clock-file))
	 (type (if (string-match "tag--" filename) "tags/" "posts/"))
	 (dir (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "originals/" type))
	 (file-path (file-name-concat dir filename)))
    file-path))

(defun tlon-bae-open-clocked-file ()
  "Open file of clocked task."
  (interactive)
  (let ((file-path (tlon-bae-set-original-path)))
    (find-file file-path)))

(defun tlon-bae-set-paths ()
  "Return paths for original and translation files from ORIGINAL-FILE."
  (if-let* ((original-path (tlon-bae-set-original-path))
	    (original-file (file-name-nondirectory original-path))
	    (translation-file (tlon-bae-get-translation-file (file-name-nondirectory original-path)))
	    (dir (tlon-bae-post-or-tag original-file))
	    (translation-dir (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "translations" dir))
	    (translation-path (file-name-concat translation-dir translation-file)))
      (cl-values original-path translation-path original-file translation-file)
    (user-error "I wasn't able to find `%s' in the correspondence file" original-file)))

(defun tlon-bae-post-or-tag (file)
  "Return `posts' or `tags' depending on FILE."
  (if (string-match "tag--" file)
      "tags"
    "posts"))

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
  "Get filename of file to be committed."
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
    (let ((regex "modified.*/\\(.*\\)\\.md"))
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

(defun tlon-bae-create-job (url)
  (interactive "sURL: ")
  (if (tlon-bae-eaf-get-identifier-from-url url)
      (tlon-bae-create-job-eaf url)
    (tlon-bae-create-job-non-eaf url))
  ;; (when (y-or-n-p "Add work to ebib?")
  ;; (zotra-add-entry-from-url url))
  (message "Next step: capture the new job (`,`) and run the usual command (`H-r r`)."))

(defun tlon-bae-create-job-eaf (url)
  "Docstring."
  (let* ((identifier (tlon-bae-eaf-get-identifier-from-url url))
	 (response (tlon-bae-eaf-request identifier))
	 (file-path (tlon-bae-eaf-generate-file-path response))
	 (filename (file-name-nondirectory file-path)))
    (tlon-bae-create-issue-for-job filename)
    (tlon-bae-eaf-import-html identifier file-path)))

(defun tlon-bae-create-job-non-eaf (url)
  "Docstring."
  (let* ((file-path (tlon-bae-generate-file-path))
	 (filename (file-name-nondirectory file-path)))
    (tlon-bae-create-issue-for-job filename)
    (tlon-bae-html-to-markdown url file-path)))

(defun tlon-bae-create-issue-for-job (filename)
  "Docstring."
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (call-interactively #'forge-create-issue)
    (insert (format "Job: `%s`" filename))
    (call-interactively #'forge-post-submit)
    (sleep-for 2)
    (forge-pull)
    (magit-status ps/dir-tlon-biblioteca-altruismo-eficaz)))

;; revise imported file
(defun tlon-bae-initialize-processing ()
  "Initialize processing."
  (interactive)
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-branch "main")
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz)
	(original (tlon-bae-set-original-path))
	(docs ps/file-tlon-docs-bae))
    (magit-pull-from-upstream nil)
    (sleep-for 2)
    (tlon-bae-set-windows original docs)
    (org-id-goto "60251C8E-6A6F-430A-9DB3-15158CC82EAE")
    (org-narrow-to-subtree)
    (ps/org-show-subtree-hide-drawers)
    (winum-select-window-2)
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
	(tlon-bae-log-buffer-diff original-path)
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
	(orgit-topic-open topic)
	;; opens in other window, so no need to switch to it first
	(tlon-bae-log-buffer-diff original-path)
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
    (tlon-bae-act-on-topic original-file "Awaiting translation" "benthamite")
    (tlon-bae-commit-and-push "Process " original-path)
    (tlon-bae-commit-and-push "Process " translation-path))
  (org-clock-goto)
  (org-todo "DONE")
  (save-buffer))

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
    (tlon-bae-act-on-topic original-file "Awaiting revision" "worldsaround")
    (org-clock-goto)
    (org-todo "DONE")
    (save-buffer)
    (message "Marked as DONE. Set label to `%s' and assignee to `%s'" label assignee)
    (sit-for 5)))

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
      (tlon-bae-act-on-topic original-file label assignee)
      (org-clock-goto)
      (org-todo "DONE")
      (save-buffer)
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'" label assignee)
      (sit-for 5))))

;;; TTS

(defun tlon-bae-read-target-buffer ()
  "Return the buffer that `read-aloud' should read."
  (let ((buffer-list (cl-remove-if-not
		      (lambda (buffer)
			(string-match-p (regexp-quote "*markdown-output* — eww") (buffer-name buffer)))
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
  "Check if there are staged or unstaged changes in repo involving FILENAME."
  (catch 'found
    (dolist (flag '("staged" ""))
      (let ((git-command (format "git diff --%s --name-only %s" flag file-path)))
	(when (not (string-empty-p (shell-command-to-string git-command)))
	  (throw 'found t))))))

(defun tlon-bae-check-staged-or-unstaged-other-than (file-path)
  "Check if there are staged or unstaged changes in repo not involving FILENAME."
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
	(default-directory ps/dir-tlon-biblioteca-altruismo-eficaz)
	(funlist `((tlon-bae-set-label ,label))))
    (when assignee (push `(tlon-bae-set-assignee ,assignee) funlist))
    (tlon-bae-magit-status)
    (magit-section-show-level-3-all)
    (goto-char (point-min))
    (if (search-forward topic nil t)
	(progn
	  (dolist (fun funlist)
	    (search-forward topic nil t)
	    (funcall (car fun) (cadr fun))
	    (goto-char (point-min)))
	  (search-forward topic nil t)
	  (pcase action
	    (`convert (call-interactively 'forge-create-pullreq-from-issue))
	    (`close (call-interactively 'forge-edit-topic-state))))
      (user-error "Could not find topic `%s' in Magit buffer" topic))))

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
  "Commit and push changes in BAE repo.
As commit message, use 'PREFIX FILE'."
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
  "Set label to 'Awaiting processing' and assignee to current user."
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
    (magit-stage-file glossary)
    (magit-commit-create (list "-m" (format  "Glossary: add \"%s\"" english)))
    (call-interactively #'magit-push-current-to-pushremote)))

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
(tlon-bae-create-file-opening-command "etc/post-correspondence.csv")
(tlon-bae-create-file-opening-command "etc/tag-slugs.txt")
(tlon-bae-create-file-opening-command "etc/work-correspondence.csv")
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
    ("r" "dwim"                         tlon-bae-dwim)
    ("m" "magit"                         tlon-bae-magit-status)
    ("n" "forge"                        tlon-bae-forge)
    """Package"
    ;; ("p p" "update"                      )
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
    ("f c" "current clock"              tlon-bae-open-clocked-file)
    ("f g" "Glossary.csv"               tlon-bae-open-glossary)
    ("f w" "work-correspondence.csv"    tlon-bae-open-work-correspondence)
    ("f t" "tag-correspondence.csv"     tlon-bae-open-tag-correspondence)
    ("f p" "post-correspondence.csv"     tlon-bae-open-post-correspondence)
    ("f s" "tag-slugs.txt"              tlon-bae-open-tag-slugs)
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
   ["Read"
    ("e e" "Read other"                 tlon-bae-read-other)
    ("e t" "Read this"              tlon-bae-read-this)
    ("e r" "Read stop"           tlon-bae-read-stop)
    """Browse"
    ("b b" "Browse file"                tlon-bae-browse-file)
    ]
   ]
  )

(defun tlon-bae-browse-file ()
  "Browse the current file in the BAE repository."
  (interactive)
  (let* ((url-suffix (file-relative-name (buffer-file-name) ps/dir-tlon-biblioteca-altruismo-eficaz))
	 (url-prefix "https://github.com/tlon-team/biblioteca-altruismo-eficaz/blob/main/"))
    (browse-url (concat url-prefix url-suffix))))

;;;

(defconst tlon-bae-eaf-api-url
  "https://forum.effectivealtruism.org/graphql"
  "URL for the EAF GraphQL API endpoint.")

(defvar tlon-bae-eaf-objects
  '(post tag)
  "List of entities supported by the EAF GraphQL API.")

;;; queries

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

;;; get json objects

(defun tlon-bae--eaf-post-get-result (response)
  "docstring"
  (let* ((post (cdr (assoc 'post response)))
	 (result (cdr (assoc 'result post))))
    result))

(defun tlon-bae-eaf-post-get-id (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-post-get-result response))
	 (id (cdr (assoc '_id result))))
    id))

(defun tlon-bae-eaf-post-get-html (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-post-get-result response))
	 (html (cdr (assoc 'htmlBody result))))
    html))

(defun tlon-bae-eaf-post-get-title (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-post-get-result response))
	 (title (cdr (assoc 'title result))))
    title))

(defun tlon-bae-eaf-post-get-author (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-post-get-result response))
	 (author (cdr (assoc 'author result))))
    author))

(defun tlon-bae-eaf-post-get-user (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-post-get-result response))
	 (user (cdr (assoc 'user result))))
    user))

(defun tlon-bae--eaf-tag-get-result (response)
  "docstring"
  (let* ((tag (cdr (assoc 'tag response)))
	 (result (cdr (assoc 'result tag))))
    result))

(defun tlon-bae-eaf-tag-get-slug (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-tag-get-result response))
	 (slug (cdr (assoc 'slug result))))
    slug))

(defun tlon-bae-eaf-tag-get-html (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-tag-get-result response))
	 (description (cdr (assoc 'description result)))
	 (html (cdr (assoc 'html description))))
    html))

(defun tlon-bae-eaf-tag-get-title (response)
  "docstring"
  (let* ((result (tlon-bae--eaf-tag-get-result response))
	 (title (cdr (assoc 'name result))))
    (tlon-bae-shorten-title title)))

;;; file handling

(defun tlon-bae-save-html-to-file (html)
  "Save the HTML string HTML to a temporary file."
  (let ((filename (make-temp-file "tlon-bae-request-" nil ".html")))
    (with-temp-file filename
      (insert html))
    filename))

;;; request functions

(defun tlon-bae-eaf-request (identifier &optional async)
  "Run an EAF request for IDENTIFIER.
IDENTIFIER should be a string returned by
`tlon-bae-validate-identifier'; see its docstring for details.
If ASYNC is t, run the request asynchronously."
  (let* ((object (tlon-bae-eaf-get-object identifier))
	 (fun (pcase object
		('post 'tlon-bae-eaf-post-query)
		('tag 'tlon-bae-eaf-tag-query)
		(_ (error "Invalid object: %S" object))))
	 (query (funcall fun identifier))
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

(defun tlon-bae-eaf-import-html (identifier file-path)
  "Import the html of EAF post/tag with IDENTIFIER to FILE-PATH."
  (let* ((response (tlon-bae-eaf-request identifier))
	 (object (tlon-bae-eaf-get-object identifier))
	 (html (pcase object
		 ('post (tlon-bae-eaf-post-get-html response))
		 ('tag (tlon-bae-eaf-get-tag-html response))))
	 (html-file (tlon-bae-save-html-to-file html)))
    (tlon-bae-html-to-markdown html-file file-path)
    (with-current-buffer (find-file-noselect file-path)
      (tlon-bae-markdown-eaf-cleanup)))
  (message "Exported to `%s'" file-path))

(defun tlon-bae-html-to-markdown (source target)
  "Docstring."
  (let ((pandoc (if (ps/string-is-url-p source)
		    tlon-bae-pandoc-convert-from-url
		  tlon-bae-pandoc-convert-from-file)))
    (shell-command
     (format pandoc source target))))

(defvar tlon-bae-pandoc-convert-from-file
  "pandoc -s '%s' -t markdown -o '%s'"
  "Command to convert from HTML file to markdown.")

(defvar tlon-bae-pandoc-convert-from-url
  "pandoc -s -r html '%s' -o '%s'"
  "Command to convert from URL to markdown.")

(provide 'tlon-bae)
;;; tlon-bae.el ends here
