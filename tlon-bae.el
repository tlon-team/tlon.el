;;; tlon-bae.el --- A collection of convenience functions for the Tlön BAE project. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.3
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
(require 'cl-lib)
(require 'github-review)

;;; Version
(defvar tlon-bae-version "0.1.3"
  "Version of the Tlön BAE package.")

(defun tlon-bae-version ()
  "Return the version of the Tlön BAE package."
  (interactive)
  (message "`tlon-bae' version %s" tlon-bae-version))

(defun tlon-bae-forge ()
  "Launch the Forge dispatcher in the BAE directory."
  (interactive)
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (call-interactively 'forge-dispatch)))

;;; File processing
(defun tlon-bae-format-file (&optional title extension tag)
  "Return a file name based on user supplied information.
TITLE is the title of the work. If EXTENSION is not provided, use
`md'. If TAG is non-nil, use `tag' as lastname."
  (let* ((lastname (if tag "tag" (read-string "Last name(s) [separated by spaces if more than one author]: ")))
	 (title (or title (read-string "Title: ")))
	 (slug-lastname (tlon-core-slugify lastname))
	 (slug-title (tlon-core-slugify title))
	 (extension (or extension "md")))
    (cl-values (file-name-with-extension (concat slug-lastname "--" slug-title) extension)
	       lastname
	       title)))

(defun tlon-bae-rename-file (&optional extension)
  "Rename file at point based on user-supplied information.
If EXTENSION is not provided, markdown is used."
  (interactive)
  (let* ((source-file-path (dired-get-filename))
	 (file (cl-nth-value 0 (tlon-bae-format-file extension))))
    (rename-file
     source-file-path
     (file-name-concat
      (file-name-directory source-file-path)
      file)))
  (revert-buffer))

(defun tlon-bae-create-file (&optional extension)
  "Create a new file based on user-supplied information.
Prompt the user for bibliographic information and create a new
 file based on it in the current directory. If EXTENSION is not
 provided, markdown is used."
  (interactive)
  (let ((file (cl-nth-value 0 (tlon-bae-format-file extension))))
    (find-file file)))

(defvar tlon-bae-markdown-eawiki-footnote-source
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\{1,2\\}\\)\\\\\\]\\](#.+?)\\^\\]{#.+? .footnote-reference role=\"doc-noteref\"}"
  "Regexp to match footnotes in the main body.")

(defvar tlon-bae-markdown-eawiki-footnote-source2
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\)\\\\\\]\\](#.+?)\\^\\]{#\\\\\\\\\\\\\".+?\\\\\\\\\\\\\" .\\\\\\\\\\\\\\\"footnote-reference\\\\\\\\\\\\\" role=\"\\\\\\\\\\\\\"doc-noteref\\\\\\\\\\\\\"\"}"
  "Regexp to match footnotes in the main body.")

(defvar tlon-bae-markdown-eawiki-footnote-target
  "\\([[:digit:]]\\{1,2\\}\\). *?\\[\\[^\\*\\*\\[\\\\^\\](#.+?)\\*\\*^\\]{\\.footnote-back-link}\\]{#.+?}

    ::: footnote-content"

  "Regexp to match footnotes in the footnote section.")

(defvar tlon-bae-markdown-eawiki-footnote-target2
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[^\\*\\*\\[\\\\^\\](#.+?)\\*\\*^\\]{\..+?}\\]{#.+?}

    ::: footnote-content "
  "Regexp to match footnotes in the footnote section.")

(defvar tlon-bae-markdown-eawiki-footnote-target3
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[\\^\\*\\*\\[\\\\\\^\\](\\\\%22#.+?\\\\%22)\\*\\*\\^\\]{\\.\\\\\\\\\\\\\"footnote-back-link\\\\\\\\\\\\\"}\\]{#\\\\\\\\\\\\\\\".+?\\\\\\\\\\\\\\\"}

    ::: \\\\\"footnote-content\\\\\" "
  "Regexp to match footnotes in the footnote section.")

(defvar tlon-bae-markdown-eawiki-footnote-target4
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[^\\*\\*\\[\\\\^\\](.+?)\\*\\*^\\]\\]{#.+?}

    footnote-content "
  "Regexp to match footnotes in the footnote section.")

(defvar tlon-bae-markdown-eawiki-footnote-target5
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[^\\*\\*\\[\\\\^\\](#.+?)\\*\\*^\\]\\]{#.+?}

    footnote-content "
  "Regexp to match footnotes in the footnote section.")

(defvar tlon-bae-markdown-eawiki-footnote-target6
  "\\([[:digit:]]\\)\\.  \\[\\[\\^\\*\\*\\[\\\\\\^\\](.+?)\\*\\*\\^\\]\\]{.+?}

 +?footnote-content "
  "Regexp to match footnotes in the footnote section.")

(defvar tlon-markdown-eawiki-links
  "\\[\\(.+?\\)\\](\\\\%22\\(.+?\\)\\\\%22)"
  "Regexp to match links.")

(defvar tlon-markdown-eawiki-escaped-quotes
  "\\\\\\\\\\\\\""
  "Regexp to match escaped quotes.")

(defun tlon-bae-markdown-eawiki-cleanup (&optional buffer)
  "Cleanup the buffer visiting an EA Wiki entry.
Assumes that the entry was imported using the GraphQL query below
and converted to Markdown with Pandoc using `pandoc -s
[source.html] -t markdown -o [destination.md]'.

`{
  tag(input:{selector:{slug:\"[slug]\"}}) {
    result {
      name
      description {
	html
      }
      parentTag {
	name
      }
    }
  }
}'"
  (interactive)
  (when (not (eq major-mode 'markdown-mode))
    (user-error "Not in a Markdown buffer"))
  (save-excursion
    (unfill-region (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "{.underline}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "{.footnote-back-link}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-source nil t)
      (replace-match (format "[^%s] " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-source2 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-target nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-target2 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-target3 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-target4 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-target5 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-bae-markdown-eawiki-footnote-target6 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-markdown-eawiki-links nil t)
      (replace-match (format "[%s](%s)" (match-string 1) (match-string 2)) nil t))
    (goto-char (point-min))
    (while (re-search-forward tlon-markdown-eawiki-escaped-quotes nil t)
      (replace-match "\""))
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
    (while (re-search-forward "{.footnote-back-link}" nil t)
      (replace-match ""))
    (fill-region (point-min) (point-max))
    (save-buffer)
    ))

(defun tlon-bae-convert-to-markdown ()
  "Convert a file from EA Wiki to Markdown."
  (interactive)
  (dolist (file (directory-files "." nil "\\.html$"))
    (let ((md-file (file-name-with-extension file "md")))
      (shell-command (format "pandoc -s '%s' -t markdown -o '%s'"
			     file
			     md-file)))))

(defun tlon-bae-cleanup-markdown ()
  "Clean up html files imported from EA Wiki."
  (interactive)
  (dolist (file (directory-files "." nil "\\.md$"))
    (with-current-buffer (find-file-noselect file)
      (message "Cleaning up %s" (buffer-name))
      (tlon-bae-markdown-eawiki-cleanup))))

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

(defun tlon-bae-insert-tag-slug ()
  "Insert an EA Wiki slug at point."
  (interactive)
  (insert (completing-read "URL: " tlon-bae-tag-slugs)))

(defun tlon-bae-insert-tag-url ()
  "Insert an EA Wiki slug at point."
  (interactive)
  (insert (completing-read "URL: " tlon-bae-wiki-urls)))

(defun tlon-bae-open-counterpart ()
  (interactive)
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
                                 (mapcar (lambda (pair) (cons (cdr pair) (car pair))) tlon-bae-tag-correspondence))))
         (new-file-name (cdr (assoc current-file-name correspondence-alist))))
    (if new-file-name
        (let* ((new-dir (if (string-match "/originals/" current-dir)
                            (replace-regexp-in-string "/originals/" "/translations/" current-dir)
                          (replace-regexp-in-string "/translations/" "/originals/" current-dir)))
               (new-file (expand-file-name new-file-name new-dir)))
          (find-file new-file)
          (message "Switched to corresponding file: %s" new-file))
      (message "No corresponding file found."))))

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
      (save-excursion
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
  (alist-get original-file tlon-bae-tag-correspondence nil nil #'equal))

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
    (message "The contents of `%s' have been copied to the to kill ring. You may now paste them into the DeepL editor."
	     (file-name-nondirectory file))))

(defun tlon-bae-set-paths ()
  "Return paths for original and translation files."
  (if-let* ((original-file (tlon-bae-get-clock-file))
	    (original-dir (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "originals/tags"))
	    (original-path (file-name-concat original-dir original-file))
	    (translation-file (tlon-bae-get-translation-file original-file))
	    (translation-dir (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "translations/tags"))
	    (translation-path (file-name-concat translation-dir translation-file)))
      (cl-values original-path translation-path original-file translation-file)
    (user-error "I wasn't able to find `%s' in `tlon-bae-tag-correspondence'" original-file)))

(defun tlon-bae-set-windows (original-path translation-path)
  "Open ORIGINAL-PATH and TRANSLATION-PATH in windows 1 and 2."
  (ps/window-split-if-unsplit)
  (winum-select-window-1)
  (find-file original-path)
  (winum-select-window-2)
  (find-file translation-path))

;;; Main functions

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
		   ("Revise" (tlon-bae-initialize-revision))
		   ("Translate" (tlon-bae-initialize-translation))
		   ("Review" (tlon-bae-initialize-review))
		   (_ (user-error "I don't know what to do with `%s`" action))))
      ('markdown-mode (pcase action
			("Process" (tlon-bae-finalize-processing))
			("Revise" (tlon-bae-finalize-revision))
			("Translate" (tlon-bae-finalize-translation))
			("Review" (tlon-bae-finalize-review))
			(_ (user-error "I don't know what to do with `%s`" action))))
      (_ (user-error "I don't know what to do in `%s`" major-mode)))))

(defun tlon-bae-initialize-processing ()
  "Initialize processing."
  (interactive)
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-branch "main")
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (magit-pull-from-upstream nil)
    (sleep-for 2)
    (cl-multiple-value-bind
	(original-path translation-path original-file)
	(tlon-bae-set-paths)
      (let* ((slug (replace-regexp-in-string ".+?--\\(.*\\)\\.md" "\\1" original-file))
	     (node (s-join " " (split-string slug "-"))))
	(winum-select-window-2)
	(find-file original-path)
	(when (string= user-full-name "Pablo Stafforini")
	  (winum-select-window-1)
	  (advice-remove 'org-roam-node-find #'widen)
	  (advice-remove 'org-roam-node-find #'ps/org-narrow-to-entry-and-children)
	  (org-roam-node-find nil node)
	  (advice-add 'org-roam-node-find :before #'widen)
	  (advice-add 'org-roam-node-find :after #'ps/org-narrow-to-entry-and-children)
	  (shell-command (format "open --background https://forum.effectivealtruism.org/topics/%s" slug)))))))

(defun tlon-bae-initialize-translation ()
  "Initialize translation."
  (interactive)
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-branch "main")
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (magit-pull-from-upstream nil)
    (sleep-for 2)
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
  "Initialize revision."
  (interactive)
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-branch "main")
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (magit-pull-from-upstream nil)
    (sleep-for 2)
    (cl-multiple-value-bind
	(original-path translation-path original-file translation-file)
	(tlon-bae-set-paths)
      (tlon-bae-check-staged-or-unstaged translation-path)
      (let ((topic (tlon-bae-get-clock-topic)))
	(tlon-bae-set-windows original-path translation-path)
	(when (magit-branch-p translation-file)
	  (user-error "Branch `%s' already exists" translation-file))
	(magit-branch-create translation-file "main")
	(magit-branch-checkout translation-file)
	(revert-buffer t t)
	(ispell-change-dictionary "espanol")
	(flyspell-buffer)
	(winum-select-window-2)
	(orgit-topic-open topic)
	(let ((message (tlon-bae-copy-file-contents original-path)))
	  (message (format "Now at branch `%s'. %s" translation-file message)))))))

(defun tlon-bae-initialize-review ()
  "Initialize review."
  (interactive)
  (tlon-bae-check-label-and-assignee)
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (magit-pull-from-upstream nil)
    (sleep-for 2)
    (cl-multiple-value-bind
	(original-path translation-path original-file translation-file)
	(tlon-bae-set-paths)
      (let ((topic (tlon-bae-get-clock-topic)))
	(magit-branch-checkout translation-file)
	(tlon-bae-set-windows original-path translation-path)
	(ispell-change-dictionary "espanol")
	(flyspell-buffer)
	(winum-select-window-2)
	(orgit-topic-open topic)))))

(defun tlon-bae-finalize-processing ()
  "Finalize processing."
  (interactive)
  (tlon-bae-check-branch "main")
  (tlon-bae-check-label-and-assignee)
  (tlon-bae-check-file)
  (let ((translator (completing-read "Who should translate this document? " '("benthamite" "worldsaround"))))
    (cl-multiple-value-bind
	(original-path translation-path original-file)
	(tlon-bae-set-paths)
      (fill-region (point-min) (point-max))
      (save-buffer)
      (tlon-bae-act-on-topic original-file "Awaiting translation" translator)
      (tlon-bae-commit-and-push "Revise " original-path)))
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
    (write-file translation-path)
    (tlon-bae-commit-and-push "Translate " translation-path)
    (let ((label "Awaiting revision")
	  (assignee (if (string= user-full-name "Pablo Stafforini")
			"worldsaround"
		      "benthamite")))
      (tlon-bae-act-on-topic original-file
			     label
			     assignee)
      (org-clock-goto)
      (org-todo "DONE")
      (save-buffer)
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'" label assignee)
      (sit-for 5))))

(defun tlon-bae-finalize-revision ()
  "Finalize revision."
  (interactive)
  (save-buffer)
  (tlon-bae-check-label-and-assignee)
  (cl-multiple-value-bind
      (original-path translation-path original-file translation-file)
      (tlon-bae-set-paths)
    (tlon-bae-check-branch translation-file)
    (let* ((target-branch "main")
	   (translation-relative-path (file-relative-name translation-path ps/dir-tlon-biblioteca-altruismo-eficaz))
	   message
	   delete-branch-p)
      (fill-region (point-min) (point-max))
      (save-buffer)
      (if (or (member translation-relative-path (magit-unstaged-files))
	      (member translation-relative-path (magit-staged-files)))
	  (let ((label "Awaiting review")
		(assignee (if (string= user-full-name "Pablo Stafforini")
			      "worldsaround"
			    "benthamite")))
	    (tlon-bae-commit-and-push "Revise " translation-path)
	    (tlon-bae-act-on-topic original-file label assignee 'convert)
	    (setq message (format "Converted issue into pull request. Set label to `%s' and assignee to `%s'. "
				  label assignee)))
	(let ((label "Awaiting publication")
	      (assignee ""))
	  (tlon-bae-act-on-topic original-file
				 label
				 assignee
				 'close)
	  (setq delete-branch-p t)
	  (setq message "Since no changes were made to the file, no pull request was created. ")
	  (sit-for 5)))
      (magit-branch-checkout target-branch)
      (setq message (concat message (format "Now at branch `%s'. " target-branch)))
      (when delete-branch-p
	(magit-branch-delete (list translation-file))
	(revert-buffer t t)
	(setq message (concat message (format "Branch `%s' deleted. " translation-file))))
      (org-clock-goto)
      (org-todo "DONE")
      (save-buffer)
      (message message))))

(defun tlon-bae-finalize-review ()
  "Finalize review."
  (interactive)
  (save-buffer)
  (tlon-bae-check-label-and-assignee)
  (cl-multiple-value-bind
      (original-path translation-path original-file translation-file)
      (tlon-bae-set-paths)
    (tlon-bae-check-branch translation-file)
    (tlon-bae-check-staged-or-unstaged translation-path)
    (let* ((target-branch "main")
	   (translation-relative-path (file-relative-name translation-path ps/dir-tlon-biblioteca-altruismo-eficaz)))
      (fill-region (point-min) (point-max))
      (save-buffer)
      (when (or (member translation-relative-path (magit-unstaged-files))
		(member translation-relative-path (magit-staged-files)))
	(magit-stage-file translation-path)
	(magit-commit-create (list "-m" (concat "Revise " translation-file))))
      (call-interactively (lambda! (magit-merge-into "main")))
      (sleep-for 3)
      (call-interactively #'magit-push-current-to-pushremote)
      (sleep-for 3)
      (let ((label "Awaiting publication")
	    (assignee ""))
	(tlon-bae-act-on-topic original-file label assignee 'close)
	(call-interactively (lambda! (magit-branch-delete (list (concat "origin/" translation-file)))))
	(revert-buffer t t)
	(org-clock-goto)
	(org-todo "DONE")
	(save-buffer)
	(message (format "Merged pull request and deleted branch `%s'. Set label to `%s' "
			 translation-file label))))))

(defun tlon-bae-submit-comment-revisions ()
  "Submit PR comments and check out `main' branch."
  (interactive)
  (unless (eq major-mode 'github-review-mode)
    (error "Not in `github-review-mode'"))
  (github-review-comment)
  (save-buffer)
  (kill-buffer)
  (magit-status ps/dir-tlon-biblioteca-altruismo-eficaz)
  (let ((branch "main"))
    (magit-branch-checkout branch)
    (message "Submitted PR comments and checked out `%s' branch." branch)))

(defun tlon-bae-update-and-reload ()
  "Update and rebuild `tlon'."
  (interactive)
  (elpaca-update 'tlon t)
  (load-library "tlon"))

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
    (cl-multiple-value-bind
	(original-path translation-path original-file)
	(tlon-bae-set-paths)
      (let ((topic (format "Job: `%s`" original-file))
	    (clocked-label (car (rassoc (tlon-bae-get-clock-action) tlon-bae-label-actions))))
	(magit-status ps/dir-tlon-biblioteca-altruismo-eficaz)
	(magit-section-show-level-3-all)
	(goto-char (point-min))
	(if (search-forward topic nil t)
	    (let ((topic-label (tlon-bae-forge-get-label-at-point))
		  (topic-assignee (alist-get
				   (tlon-bae-forge-get-assignee-at-point)
				   tlon-bae-users nil nil 'string=)))
	      (unless (string= clocked-label topic-label)
		(user-error "The `org-mode' TODO says the label is `%s', but the actual topic label is `%s'"
			    clocked-label topic-label))
	      (unless (string= user-full-name topic-assignee)
		(user-error "The `org-mode' TODO says the assignee is `%s', but the actual topic assignee is `%s'"
			    user-full-name topic-assignee))
	      t)
	  (user-error "No topic found for %s" original-file))))))

(defun tlon-bae-check-staged-or-unstaged (filename)
  "Check if there are staged or unstaged changes in repo not involving FILENAME."
  (let* ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz)
	 (all-changes (magit-git-str "diff" "HEAD" "--" "."))
	 (filtered-changes (magit-git-str "diff" "HEAD" "--" filename)))
    (unless (string= all-changes filtered-changes)
      (user-error "There are staged or unstaged changes in repo. Please commit or stash them before continuing"))))

(defun tlon-bae-act-on-topic (original-file label assignee &optional pullreq)
  "Apply LABEL and ASSIGNEE to topic associated with ORIGINAL-FILE.
If PULLREQ is `convert', convert the existing issue into a pull
request. If PULLREQ is `close', close pull request."
  (let ((topic (format "Job: `%s`" original-file)))
    (magit-status ps/dir-tlon-biblioteca-altruismo-eficaz)
    (magit-section-show-level-3-all)
    (goto-char (point-min))
    (if (search-forward topic nil t)
	(progn
	  (dolist (elt `((tlon-bae-apply-label ,label)
			 (tlon-bae-make-assignee ,assignee)))
	    (search-forward topic nil t)
	    (funcall (car elt) (cadr elt))
	    (goto-char (point-min)))
	  (search-forward topic nil t)
	  (pcase pullreq
	    (`convert (call-interactively 'forge-create-pullreq-from-issue))
	    ;; (`close (call-interactively 'forge-edit-topic-state))
	    ))
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
As commit message, use 'PREFIX FILE'. Unless PREFIX is specified,
prompt user to select between 'Translate', 'Revise' and 'Review'.
Unless FILE is specified, use the name of the current buffer."
  (interactive)
  (let ((default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (tlon-bae-check-staged-or-unstaged file)
    (when (string= (magit-get-current-branch) "main")
      (magit-pull-from-upstream nil)
      (sleep-for 2))
    (let ((prefix (or prefix
		      (completing-read "" '("Revise "
					    "Translate "
					    "Review "))))
	  (file (or file (buffer-name))))
      (magit-stage-file file)
      (magit-commit-create (list "-m" (concat prefix (file-name-nondirectory file))))
      (call-interactively #'magit-push-current-to-pushremote))))

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

(defun tlon-bae-apply-label (label)
  "Apply LABEL to topic at point.
Note that this only works for topics listed in the main buffer."
  (interactive)
  (let* ((topic (forge-get-topic (forge-topic-at-point)))
	 (repo  (forge-get-repository topic))
	 (crm-separator ","))
    (forge--set-topic-labels
     repo topic (list label))))

(defun tlon-bae-make-assignee (assignee)
  "Edit ASSIGNEE the assignee of topic at point."
  (interactive)
  (let* ((topic (forge-get-topic (forge-topic-at-point)))
	 (repo  (forge-get-repository topic))
	 (value (closql--iref topic 'assignees))
	 (choices (mapcar #'cadr (oref repo assignees)))
	 (crm-separator ","))
    (forge--set-topic-assignees
     repo topic
     (list assignee))))

;; 0th stage of process
(defun tlon-bae-label-awaiting-processing-and-assign-to-pablo ()
  "Label topic at point 'Awaiting processing' and assign it to Pablo."
  (interactive)
  (tlon-bae-apply-label "Awaiting processing")
  (tlon-bae-make-assignee "benthamite"))

;; 1st stage of process
(defun tlon-bae-label-awaiting-translation-and-assign-to-leo ()
  "Label topic at point 'Awaiting translation' and it assign to Leo."
  (interactive)
  (tlon-bae-apply-label "Awaiting translation")
  (tlon-bae-make-assignee "worldsaround"))

;; 2nd stage of process
(defun tlon-bae-label-awaiting-revision-and-assign-to-pablo ()
  "Label topic at point 'Awaiting revision' and assign it to Pablo."
  (interactive)
  (tlon-bae-apply-label "Awaiting revision")
  (tlon-bae-make-assignee "benthamite"))

;; 3rd stage of process
(defun tlon-bae-label-awaiting-review-and-assign-to-leo ()
  "Label topic at point 'Awaiting review' and it assign to Leo."
  (interactive)
  (tlon-bae-apply-label "Awaiting review")
  (tlon-bae-make-assignee "worldsaround"))

;; 4th stage of process
(defun tlon-bae-label-awaiting-publication ()
  "Label topic at point 'Awaiting publication'."
  (interactive)
  (tlon-bae-apply-label "Awaiting publication"))

;; obsolete
(defun tlon-bae-label-awaiting-publication-and-assign-to-fede ()
  "Label topic at point 'Awaiting publication'."
  (interactive)
  (tlon-bae-apply-label "Awaiting publication")
  (tlon-bae-make-assignee "fstafforini"))

(make-obsolete 'tlon-bae-label-awaiting-publication-and-assign-to-fede
	       'tlon-bae-label-awaiting-publication
	       "2023-04-26.")

(defun tlon-bae-label-awaiting-rewrite-and-assign-to-pablo ()
  "Label topic at point 'Awaiting rewrite' and it assign to Pablo."
  (interactive)
  (tlon-bae-apply-label "Awaiting rewrite")
  (tlon-bae-make-assignee "benthamite"))

(defun tlon-bae-label-awaiting-import-and-assign-to-pablo ()
  "Label topic at point 'Awaiting import' and assign it to Pablo."
  (interactive)
  (tlon-bae-apply-label "Awaiting import")
  (tlon-bae-make-assignee "benthamite"))

;; this is just a slightly tweaked version of `forge-edit-topic-labels'.
;; It differs from that function only in that it returns the selection
;; rather than submitting it.
(defun tlon-bae-forge-get-topic-label (topic)
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

(defun tlon-bae-forge-get-topic-assignee (topic)
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
;; `tlon-bae-forge-get-topic-label'. I don't know how to do this
;; properly with `magit-completing-read-multiple', so I just simulate a
;; RET keypress.
(defun tlon-bae-forge-get-label-at-point ()
  "Return the label of the topic at point.
If the topic has more than one label, return the first."
  (let ((exit-minibuffer-func (lambda () (exit-minibuffer))))
    (minibuffer-with-setup-hook
	(lambda ()
	  (add-hook 'post-command-hook exit-minibuffer-func t t))
      (tlon-bae-forge-get-topic-label (forge-current-topic)))))

(defun tlon-bae-forge-get-assignee-at-point ()
  "Return the assignee of the topic at point.
If the topic has more than one assignee, return the first."
  (let ((exit-minibuffer-func (lambda () (exit-minibuffer))))
    (minibuffer-with-setup-hook
	(lambda ()
	  (add-hook 'post-command-hook exit-minibuffer-func t t))
      (tlon-bae-forge-get-topic-assignee (forge-current-topic)))))

(defun tlon-bae-open-original-or-translation ()
  "Open the translation if visiting the original, and vice versa."
  (interactive)
  (let* ((current-file (file-name-nondirectory (buffer-file-name))))
    (alist-get current-file tlon-bae-translation-alist
	       (lambda (key default) default))))

(defvar tlon-bae-label-actions
  '(("Awaiting processing" . "Process")
    ("Awaiting importing" . "Import")
    ("Awaiting publication" . "Publish")
    ("Awaiting review" . "Review")
    ("Awaiting revision" . "Revise")
    ("Awaiting rewrite" . "Rewrite")
    ("Awaiting translation" . "Translate")
    ("Glossary" . "Respond")
    ("Misc" . "Misc"))
  "Alist of topic labels and corresponding actions.")

(defvar tlon-bae-label-bindings
  '(("Awaiting processing" . "p")
    ("Awaiting importing" . "i")
    ("Awaiting rewrite" . "w")
    ("Awaiting revision" . "r")
    ("Awaiting translation" . "t")
    ("Awaiting review" . "v")
    ("Awaiting publication" . "u")
    ("Glossary" . "g")
    ("Misc" . "m"))
  "Alist of topic labels and corresponding key bindings.")

(defvar tlon-bae-users
  '(("fstafforini" . "Federico Stafforini")
    ("worldsaround" . "Leonardo Picón")
    ("benthamite" . "Pablo Stafforini"))
  "Alist of GitHub usernames and corresponding full names.")

(defun tlon-bae-topic-label-match (label)
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
  (let ((glossary (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "etc/Glossary.csv"))
	(default-directory ps/dir-tlon-biblioteca-altruismo-eficaz))
    (with-current-buffer (find-file-noselect glossary)
      (goto-char (point-max))
      (insert (format "\n\"%s\",\"%s\",\"EN\",\"ES\"" english spanish))
      (goto-char (point-min))
      (flush-lines "^$")
      (save-buffer))
    (magit-stage-file glossary)
    (magit-commit-create (list "-m" (format  "Glossary: add \"%s\"" english)))
    (call-interactively #'magit-push-current-to-pushremote)))

(defun tlon-bae-add-to-work-correspondece (original spanish)
  "Add a new entry to the correspondece file for ORIGINAL and SPANISH terms."
  (interactive "sOriginal: \nsSpanish: ")
  (let ((glossary (file-name-concat ps/dir-tlon-biblioteca-altruismo-eficaz "etc/Correspondence.csv"))
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


(global-set-key (kbd "H-D") 'tlon-bae-dwim)
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
(define-key github-review-mode-map (kbd "s-c") 'tlon-bae-submit-comment-revisions)
(define-key markdown-mode-map (kbd "s-f") 'tlon-bae-finalize-revision)

(provide 'tlon-bae)
;;; tlon-bae.el ends here
