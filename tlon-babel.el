;;; tlon-babel.el --- A collection of convenience functions for the Tlön babel project. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.13
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
(require 'ebib)

;;; Version

(defvar tlon-babel-version "0.1.13")

(defun tlon-babel-version ()
  "Return the version of the `tlon-babel' package."
  (interactive)
  (message "`tlon-babel' version %s" tlon-babel-version))

(defun tlon-babel-update ()
  "Update `tlon-babel' package."
  (interactive)
  (let* ((default-directory (file-name-concat user-emacs-directory "elpaca/repos/tlon-babel/"))
	 (builds-directory (file-name-concat user-emacs-directory "elpaca/builds/tlon-babel/"))
	 (tlon-babel-file (file-name-concat default-directory "tlon-babel.el")))
    (shell-command "git pull")
    (with-current-buffer (find-file-noselect tlon-babel-file)
      (eval-buffer))
    (dired-delete-file builds-directory 'always t)
    (message "Package updated. %s" tlon-babel-version)))

;;; Vars

;;;; files

(defvar tlon-babel-dir-repos
  (expand-file-name "~/Library/CloudStorage/Dropbox/repos/")
  "Directory where the Tlön repos are stored.")

(defvar tlon-babel-project-names-and-abbrevs
  '(("biblioteca-altruismo-eficaz" . "bae")
    ("ensayos-sobre-largoplacismo" . "largoplacismo")
    ("utilitarismo.net" . "utilitarismo"))
  "List of Babel project names and associated abbreviations.")

(defvar tlon-babel-project-names
  (mapcar #'car tlon-babel-project-names-and-abbrevs)
  "List of Babel project names.")

(defvar tlon-babel-project-abbrevs
  (mapcar #'cdr tlon-babel-project-names-and-abbrevs)
  "List of Babel project names.")

(defvar tlon-babel-project-names-and-dirs
  (mapcar (lambda (project)
	    (cons project (file-name-as-directory (file-name-concat tlon-babel-dir-repos project))))
	  tlon-babel-project-names)
  "Association list of Babel projects and their file paths.")

(defvar tlon-babel-repo-names-and-abbrevs
  (append  tlon-babel-project-names-and-abbrevs `(("genus" . "genus")))
  "Association list of all repos and their file paths.")

(defvar tlon-babel-project-names-and-metadata-vars
  '(("biblioteca-altruismo-eficaz" . tlon-babel-bae-metadata)
    ("ensayos-sobre-largoplacismo" . tlon-babel-largoplacismo-metadata)
    ("utilitarismo.net" . tlon-babel-utilitarismo-metadata))
  "List of Babel project names and associated metadata variables.")

(defvar tlon-babel-repo-names
  (mapcar #'car tlon-babel-repo-names-and-abbrevs)
  "List of Babel repo names.")

(defvar tlon-babel-repo-abbrevs
  (mapcar #'cdr tlon-babel-repo-names-and-abbrevs)
  "List of Babel repo abbrevs.")

(defvar tlon-babel-repo-names-and-dirs
  (mapcar (lambda (repo)
	    (cons repo (file-name-as-directory (file-name-concat tlon-babel-dir-repos repo))))
	  tlon-babel-repo-names)
  "Association list of Babel repos and their file paths.")

(defun tlon-babel-generate-project-dir-vars ()
  "Generate variables for storing relevant paths in each project repo.
The stored paths, for each repo, are the path to the repo itself,
the path to the `originals' subdirectory, and the path to the
`translations' subdirectory. The names of the variables are
formed by the prefix `tlon-babel-dir-', the repo's canonical
abbreviation (as defined in `tlon-babel-project-names-and-abbrevs') and
either no suffix, the suffix `', `-originals', or the suffix
`-dir-translations', respectively. For example, the name of the
variable that stores the path of the `originals' subdirectory of
the `bae' repo is named `tlon-babel-dir-bae-originals'."
  (dolist (project tlon-babel-project-names)
    (let* ((abbrev (alist-get project tlon-babel-project-names-and-abbrevs nil nil 'string=))
	   (dir (alist-get project tlon-babel-project-names-and-dirs nil nil 'string=)))
      (dolist (suffix '("" "originals" "translations"))
	(let* ((separator (if (string= suffix "") "" "-"))
	       (var-name (concat "tlon-babel-dir-" abbrev separator suffix))
	       (var-value (file-name-as-directory (file-name-concat dir suffix))))
	  (set (intern var-name) var-value))))))

(tlon-babel-generate-project-dir-vars)

(defvar tlon-babel-dir-genus
  (file-name-concat tlon-babel-dir-repos "genus/")
  "Directory where the genus repo is stored.")

(defvar tlon-babel-dir-refs
  (file-name-concat tlon-babel-dir-genus "refs/")
  "Directory where BibTeX files are stored.")

(defvar tlon-babel-dir-dict
  (file-name-concat tlon-babel-dir-genus "dict/")
  "Directory where dictionary files are stored.")

(defvar tlon-babel-dir-locales
  (file-name-concat tlon-babel-dir-refs "locales/")
  "Directory where CSL locale files are stored.")

(defvar tlon-babel-dir-styles
  (file-name-concat tlon-babel-dir-refs "styles/")
  "Directory where CSL style files are stored.")

(defvar tlon-babel-file-manual
  (file-name-concat tlon-babel-dir-genus "manual.org")
  "File containing the manual.")

(defvar tlon-babel-file-readme
  (file-name-concat tlon-babel-dir-genus "readme.md")
  "File containing the readme.")

(defvar tlon-babel-file-jobs
  (file-name-concat tlon-babel-dir-genus "jobs.org")
  "File containing the glossary.")

(defvar tlon-babel-file-contacts
  (file-name-concat tlon-babel-dir-genus "contacts.json")
  "File containing the contacts.")

(defvar tlon-babel-file-glossary
  (file-name-concat tlon-babel-dir-dict "Glossary.csv")
  "File containing the glossary.")

(defvar tlon-babel-file-fluid
  (file-name-concat tlon-babel-dir-refs "fluid.bib")
  "File containing the fluid bibliography.")

(defvar tlon-babel-file-stable
  (file-name-concat tlon-babel-dir-refs "stable.bib")
  "File containing the stable bibliography.")

;;;; org ids

(defvar tlon-babel-manual-processing-id
  "60251C8E-6A6F-430A-9DB3-15158CC82EAE"
  "ID of the `processing' heading in `manual.org'.")

(defvar tlon-babel-jobs-id
  "820BEDE2-F982-466F-A391-100235D4C596"
  "ID of the `jobs' heading in `jobs.org'.")

;;;; forge

(defvar tlon-babel-label-actions
  '(("Awaiting processing" . "Process")
    ("Awaiting translation" . "Translate")
    ("Awaiting revision" . "Revise")
    ("Awaiting check" . "Check")
    ("Awaiting review" . "Review")
    ("Awaiting publication" . "Publish")
    ("Awaiting rewrite" . "Rewrite")
    ("Glossary" . "Respond")
    ("Misc" . "Misc"))
  "Alist of topic labels and corresponding actions.")

(defvar tlon-babel-label-bindings
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

(defvar tlon-babel-label-assignees
  '(("Awaiting processing" . "worldsaround")
    ("Awaiting translation" . "benthamite")
    ("Awaiting revision" . "worldsaround")
    ("Awaiting check" . "worldsaround")
    ("Awaiting review" . "benthamite")
    ("Awaiting publication" . ""))
  "Alist of topic labels and corresponding assignees.")

(defvar tlon-babel-github-users
  '(("fstafforini" . "Federico Stafforini")
    ("worldsaround" . "Leonardo Picón")
    ("benthamite" . "Pablo Stafforini"))
  "Alist of GitHub usernames and corresponding full names.")

(defvar tlon-babel-system-users
  '(("Federico Stafforini" . "Federico Stafforini")
    ("cartago" . "Leonardo Picón")
    ("Pablo Stafforini" . "Pablo Stafforini"))
  "Alist of system usernames and corresponding full names.")

;;;

(defun tlon-babel-alist-key (value alist)
  "Find the first key from ALIST that corresponds to VALUE."
  (cl-loop for (key . val) in alist
	   when (equal val value)
	   return key))

(defun tlon-babel-forge ()
  "Launch the Forge dispatcher.
If the current directory matches none of the directories in
 `tlon-babel-project-names', prompt the user to select a repo from that
 list."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo nil 'genus)))
    (call-interactively 'forge-dispatch)))

(defun tlon-babel-get-repo (&optional no-prompt genus)
  "Get Babel repository path.
If the current directory matches any of the directories in `tlon-
babel-project-names', return it. Else, prompt the user to select
a repo from that list, unless NO-PROMPT is non-nil. In that case,
signal an error if its value is `error', else return nil. If
GENUS is non-nil, include the `genus' repo. In that case, the
matching will be made against `tlon-babel-repo-names'."
  (if-let ((current-repo (tlon-babel-get-repo-from-file)))
      current-repo
    (if no-prompt
	(when (eq no-prompt 'error)
	  (user-error "Not in a recognized Babel repo"))
      (alist-get (completing-read "Select repo: "
				  (if genus
				      tlon-babel-repo-names
				    tlon-babel-project-names))
		 tlon-babel-repo-names-and-dirs nil nil 'string=))))

(defun tlon-babel-buffer-file-name ()
  "Return name of file BUFFER is visiting, handling `git-dirs' path."
  ;; check that current buffer is visiting a file
  (when-let ((file (buffer-file-name)))
    (replace-regexp-in-string
     "/git-dirs/"
     "/Library/CloudStorage/Dropbox/repos/"
     (buffer-file-name))))

(defun tlon-babel-get-repo-from-file (&optional file)
  "Return the repo to which FILE belongs.
If FILE is nil, use the current buffer's file name."
  (let* ((file (or file (tlon-babel-buffer-file-name) default-directory))
	 (directory-path (file-name-directory file)))
    (cl-some (lambda (cons-cell)
	       (let ((tlon-babel-project-names-and-dirs (cdr cons-cell)))
		 (when (string-prefix-p (file-name-as-directory tlon-babel-project-names-and-dirs)
					directory-path)
		   tlon-babel-project-names-and-dirs)))
	     tlon-babel-project-names-and-dirs)))

(defun tlon-babel-get-repo-from-key (key)
  "Return the repo corresponding to KEY."
  (catch 'found
    (dolist (metadata tlon-babel-all-metadata)
      (when-let (file (tlon-babel-metadata-lookup "key_original" key "file" metadata))
	(throw 'found (tlon-babel-get-repo-from-file file))))))

(defun tlon-babel-get-name-from-repo (repo)
  "Return the name of the repo REPO."
  (tlon-babel-alist-key repo tlon-babel-repo-names-and-dirs))

(defun tlon-babel-get-repo-from-name (repo-name)
  "Return the path of the repo named REPO-NAME."
  (alist-get repo-name tlon-babel-repo-names-and-dirs nil nil 'string=))

(defun tlon-babel-get-abbreviated-name-from-repo (&optional repo)
  "Return the abbreviated name of the repo REPO."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (repo-name (tlon-babel-get-name-from-repo repo)))
    (alist-get repo-name tlon-babel-repo-names-and-abbrevs nil nil 'string=)))

(defun tlon-babel-orgit-capture ()
  "Capture a new org mode task for topic at point."
  (interactive)
  (let ((assignee (alist-get
		   (tlon-babel-forge-get-assignee-at-point)
		   tlon-babel-github-users nil nil 'string=))
	(label (tlon-babel-forge-get-label-at-point)))
    ;; when the topic has neither a label nor an assignee, we offer to
    ;; process it as a new job
    (if (not (or assignee label))
	(if (y-or-n-p "Process issue as a new job (this will assign the issue to you, add the label 'Awaiting processing', and create a new master TODO in your org mode file)?")
	    (progn
	      (tlon-babel-start-job t)
	      (sleep-for 4)
	      (tlon-babel-orgit-capture))
	  (user-error "Aborted"))
      ;; else we prompt for an assignee...
      (unless (string= user-full-name assignee)
	(if (y-or-n-p
	     (format "The assignee of this topic is %s. Would you like to become the assignee?" assignee))
	    (progn
	      (tlon-babel-set-assignee (tlon-babel-find-key-in-alist user-full-name tlon-babel-github-users))
	      (sleep-for 2))
	  (user-error "Aborted")))
      ;; ...or for a label
      (unless label
	(if (y-or-n-p "The topic has no label. Would you like to add one?")
	    (tlon-babel-set-label (tlon-babel-select-label))
	  (tlon-babel-set-assignee (tlon-babel-find-key-in-alist user-full-name tlon-babel-github-users))
	  (user-error "Aborted")))
      (orgit-store-link nil)
      (magit-pull-from-upstream nil)
      (if-let* ((org-link (ps/org-nth-stored-link 0))
		(refile-position (org-find-exact-headline-in-buffer
				  (cadr (nth 0 org-stored-links))
				  (find-file-noselect ps/file-tlon-babel))))
	  (let ((action (alist-get label tlon-babel-label-actions nil nil #'string=))
		(binding (upcase (alist-get label tlon-babel-label-bindings nil nil #'string=))))
	    (kill-new (format "%s %s" action org-link))
	    ;; TODO: use different capture templates for the different project
	    (org-capture nil (concat "tb" binding))
	    ;; refile under job
	    (org-refile nil nil (list nil (buffer-file-name) nil refile-position))
	    (ps/org-refile-goto-latest))
	(when (y-or-n-p "No master TODO found for this topic. Create?")
	  (tlon-babel-start-job)
	  (tlon-babel-orgit-capture))))))

(defun tlon-babel-start-job (&optional set-topic)
  "Create new job.
If SET-TOPIC is non-nil, set topic label to `Awaiting processing'
and assignee to the current user."
  (save-window-excursion
    (when set-topic
      (tlon-babel-set-initial-label-and-assignee))
    (orgit-store-link nil)
    (let ((job-name (cadr (nth 0 org-stored-links))))
      (kill-new (format "%s" job-name)))
    ;; TODO: use different capture templates for the different projects
    (org-capture nil "tbJ")))

;;; user commits

(defun tlon-babel-latest-user-commit-in-file (&optional file)
  "Return latest commit by the current user in FILE.
If no FILE is provided, use the file visited by the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (default-directory (file-name-directory file))
	 (user (tlon-babel-find-key-in-alist user-full-name tlon-babel-system-users))
	 ;; get most recent commit in FILE by USER
	 (output (shell-command-to-string (format "git log --pretty=format:'%%h %%an %%s' --follow -- '%s' | grep -m 1 '%s' | awk '{print $1}'" file user)))
	 (commit (car (split-string output "\n"))))
    commit))

(defun tlon-babel-log-buffer-latest-user-commit (&optional file)
  "Show changes to FILE since the latest commit by the current user.
If no FILE is provided, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-babel-latest-user-commit-in-file file)))
    (magit-diff-range commit nil (list file))))

(defun tlon-babel-log-buffer-latest-user-commit-ediff (&optional file)
  "Run `ediff' session for FILE and its state when last committed by current user.
If FILE is not provided, use the file visited by the current
buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-babel-latest-user-commit-in-file file))
	 (commit-file (tlon-babel-create-file-from-commit file commit)))
    (ediff-files commit-file file)))

;;; Markdown
;;;; cleanup

(defvar tlon-babel-markdown-eawiki-footnote-source
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\{1,2\\}\\)\\\\\\]\\](#.+?)\\^\\]{#.+? .footnote-reference role=\"doc-noteref\"}"
  "Regexp to match footnotes in the main body.")

(defvar tlon-babel-markdown-eawiki-footnote-source2
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\)\\\\\\]\\](#.+?)\\^\\]{#\\\\\\\\\\\\\".+?\\\\\\\\\\\\\" .\\\\\\\\\\\\\\\"footnote-reference\\\\\\\\\\\\\" role=\"\\\\\\\\\\\\\"doc-noteref\\\\\\\\\\\\\"\"}"
  "Regexp to match footnotes in the main body.")

(defvar tlon-babel-markdown-eawiki-footnote-target
  "\\([[:digit:]]\\{1,3\\}\\). +?\\[\\[^\\*\\*\\[\\\\^\\](#[[:alnum:]]\\{12,18\\})\\*\\*^\\]\\]{#[[:alnum:]]\\{10,15\\}}

    footnote-content.*?"
  "Regexp to match footnotes in the footnote section.")

(defvar tlon-markdown-eawiki-links
  "\\[\\(.+?\\)\\](\\\\%22\\(.+?\\)\\\\%22)"
  "Regexp to match links.")

(defun tlon-babel-markdown-eaf-cleanup (&optional buffer)
  "Cleanup the BUFFER visiting an EAF entry."
  (interactive)
  (tlon-babel-check-in-markdown-mode)
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
    (while (re-search-forward tlon-babel-markdown-eawiki-footnote-source nil t)
      (replace-match (format "[^%s] " (match-string-no-properties 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-babel-markdown-eawiki-footnote-source2 nil t)
      (replace-match (format "[^%s]: " (match-string-no-properties 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-babel-markdown-eawiki-footnote-target nil t)
      (replace-match (format "[^%s]:" (match-string-no-properties 1))))
    (goto-char (point-min))
    (while (re-search-forward tlon-markdown-eawiki-links nil t)
      (replace-match (format "[%s](%s)" (match-string-no-properties 1) (match-string-no-properties 2)) nil t))
    (goto-char (point-min))
    ;; remove double asterisks surrounding headings
    (while (re-search-forward "# \\*\\*\\(.*\\)\\*\\* *?$" nil t)
      (replace-match (format "# %s" (match-string-no-properties 1))))
    (tlon-babel-non-eaf-cleanup)
    (save-buffer)))

(defun tlon-babel-non-eaf-cleanup ()
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

(defun tlon-babel-convert-to-markdown ()
  "Convert a file from EA Wiki to Markdown."
  (interactive)
  (dolist (file (directory-files "." nil "\\.html$"))
    (let ((md-file (file-name-with-extension file "md")))
      (shell-command (format "pandoc -s '%s' -t markdown -o '%s'"
			     file
			     md-file)))))

(defun tlon-babel-cleanup-markdown-multiple ()
  "Clean up html files imported from EA Wiki."
  (interactive)
  (dolist (file (directory-files "." nil "\\.md$"))
    (with-current-buffer (find-file-noselect file)
      (message "Cleaning up %s" (buffer-name))
      (tlon-babel-markdown-eaf-cleanup))))

;;; Markdown

(defun tlon-babel-check-in-markdown-mode ()
  "Check if the current buffer is in a Markdown-derived mode."
  (unless (derived-mode-p 'markdown-mode)
    (user-error "Not in a Markdown buffer")))

;;;; insertion commands
(defun tlon-babel-markdown-insert-tag ()
  "Insert a tag slug at point."
  (interactive)
  (tlon-babel-check-in-markdown-mode)
  (let* ((selection (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))
         (current-link (markdown-link-at-pos (point)))
	 (current-desc (nth 2 current-link))
	 (current-target (nth 3 current-link))
         current-tag-title)
    (when current-target
      (setq current-tag-title (tlon-babel-markdown-get-tag-title-in-link-target current-target)))
    (let* ((new-tag-title (completing-read "Tag: " (tlon-babel-get-bae-tags) nil t
					   (or current-tag-title
					       selection)))
           (new-target-file (file-name-nondirectory
                             (tlon-babel-metadata-lookup "titulo" new-tag-title "file" (tlon-babel-get-repo-metadata))))
           (tags-dir (file-name-concat tlon-babel-dir-bae-translations "temas"))
           (new-target-dir (file-relative-name tags-dir default-directory))
           (new-target (file-name-concat new-target-dir new-target-file))
           (new-desc (if (and current-desc (string= new-target current-target))
			 current-desc
                       (or selection new-tag-title)))
           (link (format "[%s](%s)" new-desc new-target)))
      (when current-target
	(ps/markdown--delete-link))
      (when selection
	(delete-region (region-beginning) (region-end)))
      (insert link))))

(defun tlon-babel-markdown-get-tag-title-in-link-target (target)
  "Return the title of the tag to which the TARGET of a Markdown link points."
  (let* ((slug (progn
		 (string-match "\\(.*/\\)?\\(.*?\\.md\\)" target)
		 (match-string 2 target)))
	 (file (concat tlon-babel-dir-bae-translations "temas/" slug))
	 (title (tlon-babel-metadata-lookup "file" file "titulo" (tlon-babel-get-repo-metadata))))
    title))

(defun tlon-babel-markdown-sort-elements-in-paragraph (separator)
  "Sort the elements separated by SEPARATOR in the current paragraph alphabetically."
  (save-excursion
    ;; Get paragraph boundaries
    (let* ((para-start (progn (backward-paragraph)
                              (skip-chars-forward "\n\t ") (point)))
           (para-end (progn (end-of-paragraph-text)
                            (point)))
           ;; Get paragraph text, separate the links
           (para-text (buffer-substring-no-properties para-start para-end))
           (link-list (split-string para-text separator))
           ;; Trim and sort the links
           (sorted-links (sort (mapcar 'string-trim link-list)
                               'string<)))
      ;; Clear the current paragraph
      (delete-region para-start para-end)
      ;; Replace it with sorted links
      (goto-char para-start)
      (insert (mapconcat 'identity sorted-links separator)))))

(defun tlon-babel-markdown-sort-related-entries ()
  "Sort the links in the `related entries' section in current buffer.
If no section is found, signal an error."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^## Entradas relacionadas" nil t)
      (error "No `related entries' section found"))
    (forward-paragraph)
    (tlon-babel-markdown-sort-elements-in-paragraph " • ")))

(defun tlon-babel-insert-element-pair (open close)
  "Insert an element pair at point or around the region if selected.
OPEN is the opening element and CLOSE is the closing element."
  (interactive)
  (tlon-babel-check-in-markdown-mode)
  (if (use-region-p)
      (let ((begin (region-beginning)))
	(goto-char (region-end))
	(insert close)
	(goto-char begin)
	(insert open))
    (insert (concat open close))
    (backward-char (length close))))

(defun tlon-babel-insert-cite-element ()
  "Insert an HTML `cite' element pair at point or around the region if selected.
When a Bibtex key is enclosed in a `cite' element pair, only its
title will be displayed in the exported web page."
  (interactive)
  (tlon-babel-insert-element-pair "<cite>" "</cite>"))

(defun tlon-babel-insert-abbr-element ()
  "Insert an HTML `abbr' element pair at point or around the region if selected.
Text enclosed by an `abbr' element pair will be displayed in small
caps."
  (interactive)
  (tlon-babel-insert-element-pair "<abbr>" "</abbr>"))

(defun tlon-babel-insert-inline-math-element ()
  "Insert an inline math element pair at point or around the region if selected."
  (interactive)
  (tlon-babel-insert-element-pair "$`" "`$"))

(defun tlon-babel-insert-display-math-element ()
  "Insert a display math element pair at point or around the region if selected."
  (interactive)
  (tlon-babel-insert-element-pair "$$\n" "\n$$"))

;;; metadata

;;;; get metadata



(defun tlon-babel-get-dir-metadata (dir)
  "Return the metadata in DIR and all its subdirectories as an association list."
  (let ((metadata '()))
    (dolist (file (directory-files-recursively dir "\\.md$"))
      (push (tlon-babel-get-file-metadata file) metadata))
    metadata))

(defun tlon-babel-get-file-metadata (file-path)
  "Return the metadata in FILE-PATH as an association list."
  (let* ((metadata (tlon-babel-get-yaml-front-matter file-path))
	 (extras `(("file" . ,file-path)
		   ("type" . "online")
		   ("database" . "Tlön")
		   ("landid" . "es"))))
    (append metadata extras)))

(defun tlon-babel-get-metadata-value-in-file (key &optional file)
  "Return the value of KEY in the YAML front matter of FILE.
If FILE is nil, use the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (metadata (tlon-babel-get-file-metadata file)))
    (alist-get key metadata nil nil #'string=)))

(defun tlon-babel-get-key-in-buffer ()
  "Get the key in the current Markdown buffer."
  (tlon-babel-check-in-markdown-mode)
  (let ((key (tlon-babel-get-metadata-value-in-file "key_original")))
    (unless key
      (user-error "No key found"))
    key))

(defun tlon-babel-get-locators-in-repo (&optional repo subdir)
  "Return a list of all locators in SUBDIR of REPO.
If REPO is nil, return files in current repository. SUBDIR should
be one of \"translations\" or \"originals\". If nil, return all
files."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (files (directory-files-recursively (file-name-concat repo subdir) "\\.md$")))
    (mapcar #'tlon-babel-get-locator-from-file files)))

;;; YAML front matter
(defvar tlon-babel-yaml-delimiter "---"
  "Delimiter for YAML front matter.")

(defun tlon-babel-get-yaml-front-matter (&optional file-path)
  "Return the YAML front matter from FILE-PATH as an association list.
If FILE-PATH is nil, use the current buffer."
  (let ((file-path (or file-path (buffer-file-name))))
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((metadata '()))
	(when (looking-at-p tlon-babel-yaml-delimiter)
	  (forward-line)
	  (dolist (line (tlon-babel-read-until-match tlon-babel-yaml-delimiter))
	    (when (string-match "^\\(.*?\\):\\s-+\\(.*\\)$" line)
	      (let* ((key (match-string 1 line))
		     (value (match-string 2 line))
		     (trimmed-value (string-trim value)))
		(push (cons (string-trim key)
			    (cond
			     ((and (string-prefix-p "[" trimmed-value)
				   (string-suffix-p "]" trimmed-value))
			      (mapcar #'string-trim
				      (mapcar (lambda (s) (if (and (string-prefix-p "\"" s)
							      (string-suffix-p "\"" s))
							 (substring s 1 -1)
						       s))
					      (split-string (substring trimmed-value 1 -1) "\\s *,\\s *")
					      )))
			     ((and (string-prefix-p "\"" trimmed-value)
				   (string-suffix-p "\"" trimmed-value))
			      (substring trimmed-value 1 -1))
			     (t trimmed-value)))
		      metadata)))))
	(nreverse metadata)))))

(defun tlon-babel-read-until-match (delimiter)
  "Return a list of lines until DELIMITER is matched.
The delimiter is not included in the result. If DELIMITER is not
found, signal an error."
  (let ((result '()))
    (while (not (or (looking-at-p delimiter) (eobp)))
      (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) result)
      (forward-line))
    (if (eobp)
	(error "Delimiter not found")
      (nreverse result))))

(defun tlon-babel--get-field-values (fields &optional file-path)
  "Get the field values for the given FIELDS in FILE-PATH."
  (let* ((file-path (or file-path (buffer-file-name)))
	 (var-generators
	  `(("fecha" . ,(lambda () (format-time-string "%FT%T%z")))
	    ("titulo" . ,(lambda () (read-string "Título: ")))
	    ("authors-list" . ,(lambda () (tlon-babel-set-multi-value-field "autores")))
	    ("traductores" . ,#'tlon-babel-set-yaml-traductores)
	    ("temas" . ,#'tlon-babel-set-yaml-temas)
	    ("path_original" . ,#'tlon-babel-set-yaml-path_original)))
	 (processed-fields (if (member "autores" fields)
			       (cons "authors-list" fields)
			     fields))
	 (field-values (cl-loop for field in processed-fields
				for generator = (cdr (assoc field var-generators))
				if generator collect `(,field . ,(funcall generator)))))
    ;; calculate first-author and adjust field-values
    (let* ((first-author (cdr (or (assoc "autores" field-values) (assoc "authors-list" field-values))))
	   (autores (when first-author
		      (or
		       (cdr (assoc "autores" field-values))
		       (tlon-babel-elisp-list-to-yaml first-author))))
	   (cmpl-generators
	    `(("first-author" . ,first-author)
	      ("autores" . ,autores)
	      ("key_original" . ,(when first-author (tlon-babel-set-yaml-key_original (car first-author))))
	      ("key_traduccion" . ,(when first-author
				     (tlon-babel-bibtex-generate-autokey
				      (car first-author)
				      (substring (cdr (assoc "fecha" field-values)) 0 4)
				      (cdr (assoc "titulo" field-values))))))))
      ;; revise field-values
      (setq field-values (assoc-delete-all "authors-list" field-values))
      (dolist (field fields)
	(when (cdr (assoc field cmpl-generators))
	  (push `(,field . ,(cdr (assoc field cmpl-generators))) field-values))))
    field-values))

(defun tlon-babel-insert-yaml-fields-in-bae-post (&optional file-path)
  "Insert YAML fields for BAE post in FILE-PATH.
If FILE-PATH is nil, use the current buffer."
  (interactive)
  (let* ((file-path (or file-path (buffer-file-name)))
	 (field-values
	  (tlon-babel--get-field-values
	   '("titulo" "autores" "traductores" "temas" "fecha" "path_original" "key_original" "key_traduccion")
	   file-path)))
    (tlon-babel-insert-yaml-fields field-values)))

(defun tlon-babel-insert-yaml-fields-in-bae-tag (&optional file-path)
  "Insert YAML fields for BAE tag in FILE-PATH.
If FILE-PATH is nil, use the current buffer."
  (interactive)
  (let* ((file-path (or file-path (buffer-file-name)))
	 (field-values
	  (tlon-babel--get-field-values
	   '("titulo")
	   file-path)))
    (tlon-babel-insert-yaml-fields field-values)))

;;;; get YAML values

(defun tlon-babel-set-multi-value-field (field)
  "Set the value of multivalue FIELD in metadata of current repo."
  (completing-read-multiple (format (capitalize "%s :") field)
			    (tlon-babel-get-field-metadata field (tlon-babel-get-metadata-of-repo))))

(defun tlon-babel-set-yaml-traductores ()
  "Set the value of `traductores' YAML field."
  (tlon-babel-elisp-list-to-yaml
   (tlon-babel-set-multi-value-field "traductores")))

(defun tlon-babel-set-yaml-temas ()
  "Set the value of `temas' YAML field."
  (tlon-babel-elisp-list-to-yaml
   (tlon-babel-set-multi-value-field "temas")))

(defun tlon-babel-set-yaml-path_original ()
  "Set the value of `path_original' YAML field."
  (completing-read "Locator original"
		   (tlon-babel-get-locators-in-repo (tlon-babel-get-repo) "originals")))

(defun tlon-babel-set-yaml-key_original (author)
  "Set the value of `key_original' YAML field.
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

;;;; interactive editing

(defun tlon-babel-yaml-insert (list)
  "Insert YAML LIST at point.
  If point is on a list, pre-populate the selection with the list
  elements."
  (let* ((bounds (bounds-of-thing-at-point 'line))
	 ;; retrieve the line
	 (line (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when (string-match "\\[\\(.*?\\)\\]" line)
      ;; retrieve and parse the elements in the list, remove quotes
      (let ((elems-at-point (mapcar (lambda (s)
				      (replace-regexp-in-string "\\`\"\\|\"\\'" "" s))
				    (split-string (match-string 1 line) ", "))))
	;; prompt the user to select multiple elements from the list,
	;; prefilling with previously selected items
	(let ((choices (completing-read-multiple "Selection (comma-separated): "
						 list
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

(defun tlon-babel-yaml-get-key-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'line))
	 ;; retrieve the line
	 (line (buffer-substring-no-properties (car bounds) (cdr bounds)))
	 ;; key and value are separated by a colon
	 (key (car (split-string line ":"))))
    ;; If there's a key in the line, return it. If not, return nil.
    (when (and key (> (length (string-trim key)) 0))
      (string-trim key))))

(defun tlon-babel-yaml-edit-field ()
  "Edit the YAML field at point."
  (interactive)
  (let ((key (tlon-babel-yaml-get-key-at-point)))
    (pcase key
      ("traductores" (tlon-babel-yaml-insert (tlon-babel-get-bae-translators)))
      ("temas" (tlon-babel-yaml-insert (tlon-babel-get-bae-tags)))
      ("autores" (tlon-babel-yaml-insert (tlon-babel-get-bae-authors)))
      (_ (message "No field to edit")))))

;;; get repo-specific elements

(defun tlon-babel-get-bae-authors ()
  "Get a list of BAE authors."
  (tlon-babel-get-field-metadata
   "titulo"
   (tlon-babel-get-repo-metadata tlon-babel-dir-bae)
   "file"
   (file-name-concat tlon-babel-dir-bae-translations "autores")))

(defun tlon-babel-get-bae-tags ()
  "Get a list of BAE tags."
  (tlon-babel-get-field-metadata
   "titulo"
   (tlon-babel-get-repo-metadata tlon-babel-dir-bae)
   "file"
   (file-name-concat tlon-babel-dir-bae-translations "temas")))

;;;

(defun tlon-babel-get-translators ()
  "Get a list of translators.
  Note that this searches in all repos, not just BAE."
  (tlon-babel-get-field-metadata
   "titulo"
   (tlon-babel-get-metadata-in-all-repos)))

;;;; counterparts

(defun tlon-babel-get-work-type (&optional reversed file-path)
  "Return the work type of file in FILE-PATH.
A work is either `original' or `translation'. If REVERSED is
  non-nil, return 'originals' when the work type is 'translations'
  and vice versa. If FILE-PATH is nil, return the work type of the
  file visited by the current buffer."
  (let* ((file-path (or file-path (buffer-file-name)))
	 (repo (tlon-babel-get-repo-from-file file-path))
	 (repo-path (file-relative-name file-path repo))
	 (root-dir-in-repo-path (car (split-string repo-path "/"))))
    (pcase root-dir-in-repo-path
      ("originals" (if reversed "translations" "originals"))
      ("translations" (if reversed "originals" "translations")))))

(defun tlon-babel-get-counterpart (&optional file-path)
  "Get the counterpart file path of file in FILE-PATH.
If FILE-PATH is nil, return the counterpart file path of the
file visited by the current buffer."
  (let* ((file-path (or file-path (tlon-bae-buffer-file-name)))
	 (repo (tlon-babel-get-repo 'error)))
    ;; we use a different method for getting the counterpart depending
    ;; on whether FILE-PATH is in `originals' or `translations', since
    ;; only translation files have YAML metadata.
    (if-let ((locator (tlon-babel-get-metadata-value-in-file "path_original" file-path)))
	(file-name-concat
	 (tlon-babel-get-repo)
	 (tlon-babel-get-work-type 'reversed)
	 locator)
      (tlon-babel-metadata-lookup "path_original"
				  (tlon-babel-get-locator-from-file file-path)
				  "file"
				  (tlon-babel-get-repo-metadata)))))

(defun tlon-babel-get-locator-from-file (&optional file-path)
  "Get the locator of file in FILE-PATH.
If FILE-PATH is nil, return the locator of the file visited by
the current buffer."
  (let* ((file-path (or file-path (buffer-file-name)))
	 (repo (tlon-babel-get-repo 'error))
	 (type (tlon-babel-get-work-type nil file-path)))
    (file-relative-name file-path (file-name-concat repo type))))

(defun tlon-babel-get-file-from-locator (locator)
  "Get the file path of LOCATOR in the current repo."
  (let* ((repo (tlon-babel-get-repo 'error))
	 (type (tlon-babel-get-work-type nil locator)))
    (file-name-concat repo type locator)))

(defun tlon-babel-open-counterpart (&optional print-message file-path)
  "Open the counterpart of file in FILE-PATH and move point to matching position.
If FILE-PATH is nil, open the counterpart of the file visited by
the current buffer.

When called interactively, PRINT-MESSAGE is non-nil, and the
function signals an error if the current buffer is not in
`markdown-mode' and FILE-PATH is nil."
  (interactive "p")
  (when (and print-message
	     (not (eq major-mode 'markdown-mode)))
    (user-error "Not in markdown-mode"))
  (let* ((counterpart (tlon-babel-get-counterpart
		       (or file-path (buffer-file-name))))
	 (paragraphs (- (tlon-babel-count-paragraphs file-path (point-min) (+ (point) 2)) 1)))
    (find-file counterpart)
    (goto-char (point-min))
    (forward-paragraph paragraphs)))

(defun tlon-babel-count-paragraphs (&optional file-path start end)
  "Return number of paragraphs between START and END in FILE-PATH.
If either START or END is nil, default to the beginning and end
of the buffer. If FILE-PATH is nil, count paragraphs in the
current buffer."
  (interactive)
  (let ((file-path (or file-path (buffer-file-name))))
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((start (or start (point-min)))
	    (end (or end (point-max))))
	(narrow-to-region start end)
	(goto-char (point-min))
	(- (buffer-size) (forward-paragraph (buffer-size)))))))

(defun tlon-babel-check-paragraph-number-match (&optional file-path)
  "Check that FILE-PATH and its counterpart have the same number of paragraphs.
If FILE-PATH is not provided, use the current buffer."
  (interactive)
  (let* ((part (or file-path (buffer-file-name)))
	 (counterpart (tlon-babel-get-counterpart part))
	 (paras-in-part (tlon-babel-count-paragraphs part))
	 (paras-in-counterpart (tlon-babel-count-paragraphs counterpart)))
    (if (= paras-in-part paras-in-counterpart)
	t
      (message "Paragraph number mismatch: \n%s has %s paragraphs\n%s has %s paragraphs"
	       (file-name-nondirectory part) paras-in-part
	       (file-name-nondirectory counterpart) paras-in-counterpart))))

(defun tlon-babel-check-paragraph-number-match-in-dir (dir &optional extension)
  "Check that files in DIR and counterparts have the same number of paragraphs.
If EXTENSION is provided, only check files with that extension.
  Otherwise, default to \".md\"."
  (let* ((extension (or extension ".md"))
	 (files (directory-files dir t (concat ".*\\" extension "$"))))
    (cl-loop for file in files
	     do (tlon-babel-check-paragraph-number-match file))))

;;; EAF validation

(defvar tlon-babel-eaf-p
  "forum\\.effectivealtruism\\.org/"
  "Regular expression for validating EAF URLs.")

(defvar tlon-babel-eaf-post-id-regexp
  "\\([[:alnum:]]\\{17\\}\\)"
  "Regular expression for validating post IDs.")

(defvar tlon-babel-eaf-tag-slug-regexp
  "\\([[:alnum:]-]*\\)"
  "Regular expression for validating tag slugs.")

(defun tlon-babel-eaf-p (url)
  "Return t if URL is an EAF URL, nil otherwise."
  (not (not (string-match tlon-babel-eaf-p url))))

(defun tlon-babel-eaf-post-id-p (identifier)
  "Return t if IDENTIFIER is a post ID, nil otherwise."
  (not (not (string-match (format "^%s$" tlon-babel-eaf-post-id-regexp) identifier))))

(defun tlon-babel-eaf-tag-slug-p (identifier)
  "Return t if IDENTIFIER is a tag slug, nil otherwise."
  (not (not (string-match (format "^%s$" tlon-babel-eaf-tag-slug-regexp) identifier))))

(defun tlon-babel-eaf-get-id-or-slug-from-identifier (identifier)
  "Return the EAF post ID or tag slug from IDENTIFIER, if found.
IDENTIFIER can be an URL, a post ID or a tag slug."
  (interactive "sURL: ")
  (if (ps/string-is-url-p identifier)
      (or (tlon-babel-eaf-get-id-from-identifier identifier)
	  (tlon-babel-eaf-get-slug-from-identifier identifier))
    ;; return id or slug if identifier is an id or slug
    (pcase identifier
      ((pred tlon-babel-eaf-post-id-p) identifier)
      ((pred tlon-babel-eaf-tag-slug-p) identifier))))

(defun tlon-babel-eaf-get-id-from-identifier (identifier)
  "Return the EAF post ID from IDENTIFIER, if found."
  (when-let ((id (or (when (string-match (format "^.+?forum.effectivealtruism.org/posts/%s"
						 tlon-babel-eaf-post-id-regexp)
					 identifier)
		       (match-string-no-properties 1 identifier))
		     (when (string-match (format "^.+?forum.effectivealtruism.org/s/%s/p/%s"
						 tlon-babel-eaf-post-id-regexp tlon-babel-eaf-post-id-regexp)
					 identifier)
		       (match-string-no-properties 2 identifier)))))
    id))

(defun tlon-babel-eaf-get-slug-from-identifier (identifier)
  "Return the EAF tag slug from IDENTIFIER, if found."
  (when (string-match (format "^.+?forum.effectivealtruism.org/topics/%s"
			      tlon-babel-eaf-tag-slug-regexp)
		      identifier)
    (match-string-no-properties 1 identifier)))

(defun tlon-babel-eaf-get-object (id-or-slug)
  "Return the EAF object in ID-OR-SLUG."
  (let ((object (cond ((tlon-babel-eaf-post-id-p id-or-slug)
		       'post)
		      ((tlon-babel-eaf-tag-slug-p id-or-slug)
		       'tag)
		      (t (user-error "Not an ID or slug: %S" id-or-slug)))))
    object))

;;; Clocked heading

(defvar tlon-babel-key-regexp "`\\(.+?\\)\\(\\.md\\)?`"
  "Regular expression for matching bibtex keys in clocked headings.
The second capture group handles the .md extension, which we used
previously.")

(defun tlon-babel-get-clock-key ()
  "Return bibtex key in clocked heading.
Assumes key is enclosed in backticks."
  (unless org-clock-current-task
    (user-error "No clock running"))
  (let ((clock (substring-no-properties org-clock-current-task)))
    ;; second capture group handles optional .md extension
    (if (string-match tlon-babel-key-regexp clock)
	(match-string 1 clock)
      (user-error "I wasn't able to find a file in clocked heading"))))

(defun tlon-babel-get-file-from-key (key)
  "Return the file path of KEY."
  (let ((locator (tlon-babel-metadata-lookup "key_original" key "path_original" (tlon-babel-get-repo-metadata))))
    (tlon-babel-get-file-from-locator locator)))

(defun tlon-babel-get-key-from-file (file)
  "Return the bibtex key of FILE."
  (or
   ;; when in `translations'
   (tlon-babel-metadata-lookup "file" file "key_traduccion" (tlon-babel-get-repo-metadata))
   ;; when file in `originals'
   (let ((translation (tlon-babel-get-counterpart file)))
     (tlon-babel-get-metadata-value-in-file "key_original" translation))))

(defun tlon-babel-get-clock-file ()
  "Return the file path of the clocked task."
  (let ((key (tlon-babel-get-clock-key)))
    (tlon-babel-get-file-from-key key)))

(defun tlon-babel-open-clock-file ()
  "Open file of clocked task."
  (interactive)
  (find-file (tlon-babel-get-clock-file)))

(defun tlon-babel-get-clock-topic ()
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

(defun tlon-babel-open-clock-topic ()
  "Open the topic from `orgit-forge' link in heading at point."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo))
	(topic (tlon-babel-get-clock-topic)))
    (forge-visit-issue topic)))

(defun tlon-babel-get-clock-action ()
  "Return action in heading at point.
Assumes action is first word of clocked task."
  ;; as rough validation, we check that the clocked heading contains a file
  (tlon-babel-get-clock-key)
  (let ((action (car (split-string (substring-no-properties org-clock-current-task))))
	(actions (mapcar #'cdr tlon-babel-label-actions)))
    (if (member action actions)
	action
      (user-error "I wasn't able to find a relevant action in clocked heading"))))

(defun tlon-babel-get-clock-label ()
  "Return label associated with action in heading at point."
  (let ((label (car (rassoc (tlon-babel-get-clock-action) tlon-babel-label-actions))))
    label))

(defun tlon-babel-get-clock-next-label ()
  "Return label associated with the action after the one in heading at point."
  (tlon-babel-get-next-car (tlon-babel-get-clock-label)
			   tlon-babel-label-actions))

(defun tlon-babel-get-clock-next-assignee (label)
  "Return assignee associated with LABEL."
  (alist-get label tlon-babel-label-assignees nil nil 'string=))

(defun tlon-babel-get-action-in-label (label)
  "Return action associated with LABEL."
  (let ((action (cadr (split-string label))))
    action))

(defun tlon-babel-get-forge-file-path ()
  "Get the file path of the topic at point or in current forge buffer."
  (unless (or (derived-mode-p 'magit-status-mode)
	      (derived-mode-p 'forge-topic-mode))
    (user-error "Not in a forge buffer"))
  (let* ((inhibit-message t)
	 (captured (cadr (call-interactively #'orgit-store-link))))
    (setq org-stored-links (cdr org-stored-links))
    (if (string-match tlon-babel-key-regexp captured)
	(tlon-babel-get-file-from-key (match-string 1 captured))
      (user-error "I wasn't able to find a file at point or in the forge buffer"))))

(defun tlon-babel-open-forge-file ()
  "Open the file of the topic at point or in the current forge buffer."
  (interactive)
  (find-file (tlon-babel-get-forge-file-path)))

(defun tlon-babel-open-forge-counterpart ()
  "Open the file counterpart of the topic at point or in the current forge buffer."
  (interactive)
  (tlon-babel-open-counterpart (tlon-babel-get-forge-file-path)))

(defun tlon-babel-copy-buffer (&optional file deepl)
  "Copy the contents of FILE to the kill ring.
Defaults to the current buffer if no FILE is specified. If DEEPL
  is non-nil, open DeepL."
  (let ((file (or file (buffer-file-name))))
    (with-current-buffer (find-file-noselect file)
      (copy-region-as-kill (point-min) (point-max)))
    (message "Copied the contents of `%s' to kill ring" (file-name-nondirectory file)))
  (when deepl
    (shell-command "open '/Applications/DeepL.app/Contents/MacOS/DeepL'")))

(defun tlon-babel-copy-region (beg end)
  "Copy the contents between BEG and END to the kill ring."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (copy-region-as-kill (point-min) (point-max))))
  (message "Copied the contents of the region to kill ring"))

(defun tlon-babel-copy-dwim ()
  "Copy the contents of the region or buffer to the kill ring."
  (interactive)
  (if (region-active-p)
      (tlon-babel-copy-region (region-beginning) (region-end))
    (tlon-babel-copy-buffer)))

(defun tlon-babel-set-paths-from-clock ()
  "Return paths for original and translation files based on clocked task."
  (let* ((key (tlon-babel-get-clock-key))
	 (repo (tlon-babel-get-repo-from-key key))
	 (repo-name (tlon-babel-get-name-from-repo repo))
	 (metadata (tlon-babel-get-repo-metadata repo-name))
	 (identifier (tlon-babel-metadata-lookup "key_original" key "path_original" metadata))
	 (original-path (file-name-concat repo "originals" identifier))
	 (translation-path (tlon-babel-metadata-lookup "key_original" key "file" metadata)))
    (cl-values original-path translation-path key)))

(defun tlon-babel-set-windows (original-path translation-path)
  "Open ORIGINAL-PATH and TRANSLATION-PATH in windows 1 and 2."
  (ps/window-split-if-unsplit)
  (winum-select-window-1)
  (find-file original-path)
  (winum-select-window-2)
  (find-file translation-path))

;;; Main functions

(defun tlon-babel-magit-status ()
  "Show the status of the current repository in a buffer."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo nil 'genus)))
    (magit-status)))

(defun tlon-babel-magit-get-commit-file ()
  "Get file to commit.
If more than one file is being committed, get the first one."
  (save-excursion
    (re-search-forward "Changes to be committed:\n#.*?:.  \\(.*/?.*\\)$" nil t)
    (match-string-no-properties 1)))

(defun tlon-babel-get-commit-key ()
  "Get key of commit file."
  (let ((path (file-name-concat (file-name-directory (tlon-babel-buffer-file-name))
				(tlon-babel-magit-get-commit-file))))
    (tlon-babel-get-key-from-file path)))

(defun tlon-babel-create-job ()
  "Create a new job for IDENTIFIER based on Ebib entry at point.
Creating a new job means (1) importing a document and (2)
creating a record for it. A record is (a) an issue in GitHub
and (b) a heading in `jobs.org'.

IDENTIFIER can be a URL or a PDF file path.

Note: this command cannot be used to create new tag jobs, because
we don't add tags to Ebib. To create a new tag job, use
`tlon-babel-create-tag-job'."
  (interactive)
  (tlon-babel-create-translation-entry)
  (tlon-babel-import-document)
  (tlon-babel-create-record-for-job))

(defun tlon-babel--create-entry-from-current (fields)
  "Create a BibTeX entry based on the current one with FIELDS."
  (unless (eq major-mode 'ebib-entry-mode)
    (user-error "This command must be run in Ebib"))
  (ps/ebib-valid-key-p)
  (let* ((key (ebib--get-key-at-point))
	 (file (file-name-with-extension key "md"))
	 (db ebib--cur-db)
	 (new-db-num (ps/ebib-get-db-number tlon-babel-file-fluid)))
    (ebib-switch-to-database-nth new-db-num)
    (ebib-add-entry)
    (let ((new-key (ebib--get-key-at-point))
	  (new-db ebib--cur-db))
      (dolist (field fields)
	(ebib-set-field-value (car field) (cdr field) new-key new-db)))
    (ebib--update-entry-buffer)
    (ebib-generate-autokey)
    (set-buffer-modified-p nil)
    (ebib--set-modified t db t (seq-filter (lambda (dependent)
					     (ebib-db-has-key key dependent))
					   (ebib--list-dependents db)))))

;; this should be moved to ebib
(defun tlon-babel-create-section-entry (&optional title)
  "Create a BibTeX entry for the section of the current entry.
Prompt the user for a title, unless TITLE is non-nil."
  (interactive)
  (let* ((fields `(("title" . ,(or title (read-string "Section title: ")))
		   ("eventtitle" . ,(ps/ebib-get-field-value "title"))
		   ("url" . ,(read-string "URL: " (ps/ebib-get-field-value "url")))
		   ("crossref" . ,(ebib--get-key-at-point))
		   ("author" . ,(ps/ebib-get-field-value "author"))
		   ("date" . ,(ps/ebib-get-field-value "")))))
    (tlon-babel--create-entry-from-current fields)))

(defun tlon-babel-import-document (&optional identifier target)
  "Import a document with IDENTIFIER to TARGET.
IDENTIFIER can be a URL or a PDF file path.
To import a tag, use `tlon-babel-import-tag'."
  (interactive)
  (unless (eq major-mode 'ebib-entry-mode)
    (user-error "You must be in an Ebib buffer"))
  (if-let* ((key (ebib--get-key-at-point))
	    (value (or (ps/ebib-get-field-value "url")
		       (ps/ebib-get-field-value "file")))
	    (identifier (or identifier (replace-regexp-in-string "\n\\s-*" "" value)))
	    (target (or target
			;; TODO: Revise
			;; (file-name-concat tlon-babel-dir-original-posts-dir
			;; (file-name-with-extension key "md"))
			)))
      (if (ps/string-is-url-p identifier)
	  (tlon-babel-import-html identifier target)
	(tlon-babel-import-pdf (expand-file-name identifier) target))
    (user-error "Document was not imported because no URL or file found in Ebib entry")))

(defun tlon-babel-import-tag (url-or-slug)
  "Import an EA Forum tag with URL-OR-SLUG."
  (interactive "sTag url or slug (if you are not importing a tag, please re-run `tlon-babel-import-document' from an Ebib buffer): ")
  (let* ((slug (tlon-babel-eaf-get-id-or-slug-from-identifier url-or-slug))
	 (target (file-name-concat tlon-babel-dir-bae-originals "tags"
				   (file-name-with-extension slug ".md"))))
    (tlon-babel-import-html-eaf slug target)))

(defun tlon-babel-import-html (url target)
  "Import the HTML in URL to TARGET and convert it to Markdown."
  (if-let ((id-or-slug (tlon-babel-eaf-get-id-or-slug-from-identifier url)))
      (tlon-babel-import-html-eaf id-or-slug target)
    (tlon-babel-html-to-markdown url target)))

(defun tlon-babel-import-html-eaf (id-or-slug target)
  "Import the HTML of EAF entity with ID-OR-SLUG to TARGET and convert it to MD."
  (let* ((response (tlon-babel-eaf-request id-or-slug))
	 (object (tlon-babel-eaf-get-object id-or-slug))
	 (html (pcase object
		 ('post (tlon-babel-eaf-get-post-html response))
		 ('tag (tlon-babel-eaf-get-tag-html response))))
	 (html-file (tlon-babel-save-html-to-file html)))
    (tlon-babel-html-to-markdown html-file target)
    (with-current-buffer (find-file-noselect target)
      (tlon-babel-markdown-eaf-cleanup))
    (find-file target)))

(defun tlon-babel-html-to-markdown (source target)
  "Convert HTML text in SOURCE to Markdown text in TARGET.
SOURCE can be a URL or, like TARGET, a file path."
  (let ((pandoc (if (ps/string-is-url-p source)
		    tlon-babel-pandoc-convert-from-url
		  tlon-babel-pandoc-convert-from-file)))
    (shell-command
     (format pandoc source target))
    (find-file target)))

(defvar tlon-babel-pdf2md
  (file-name-concat ps/dir-source "pdf2md/lib/pdf2md-cli.js")
  "Path to `pdf2md-cli.js' executable.")

(defvar tlon-babel-pdftotext
  (file-name-concat ps/dir-source "xpdf-tools-mac-4.04/bin64/pdftotext")
  "Path to `pdftotext' executable.")

(defun tlon-babel-import-pdf (path target)
  "Import the PDF in PATH to TARGET and convert it to Markdown.
This command requires the user to supply values for the header
and footer elements to be excluded from the conversion, which are
different for each PDF. To determine these values, measure the
distance between the top/bottom of the PDF (which will open in
the other window) and note the number of pixels until the end of
the header/footer. (You can measure the number of pixels between
two points by taking a screenshot: note the numbers next to the
pointer.) Then enter these values when prompted."
  (find-file-other-window path)
  (let ((header (read-string "Header: "))
	(footer (read-string "Footer: ")))
    (shell-command (format "'%s' -margint %s -marginb %s '%s' '%s'"
			   tlon-babel-pdftotext header footer path target))
    (find-file target)))

;; This function is not currently used because we now use pdftotext, rather
;; than pdf2md, to convert PDFs to Markdown.
(defun tlon-babel-import-pdf-to-markdown (path target)
  "Import the PDF in PATH to TARGET and convert it to Markdown."
  (let* ((path (or path (read-file-name "PDF: ")))
	 (expanded-path (expand-file-name path))
	 (temp-source-dir (make-temp-file "pdf-source" t))
	 (temp-source-file (file-name-concat temp-source-dir "source.pdf"))
	 (temp-target-dir (make-temp-file "pdf-target" t))
	 (temp-target-file (file-name-concat temp-target-dir "source.md")))
    (unwind-protect
	(progn
	  (copy-file expanded-path temp-source-file)
	  (shell-command (format "node %s --inputFolderPath='%s' --outputFolderPath='%s'"
				 tlon-babel-pdf2md temp-source-dir temp-target-dir))
	  (copy-file temp-target-file target)
	  (delete-directory temp-source-dir t)
	  (delete-directory temp-target-dir t)))))

(defun tlon-babel-create-record-for-job (&optional key)
  "Create a record based on KEY.
Creates a new record in the repository (with the format `Job:
KEY') and a new heading in the file `jobs.org'. If KEY is not
provided, the key in the Markdown buffer at point is used."
  (interactive)
  (let* ((key (or key (tlon-babel-get-key-in-buffer))))
    (tlon-babel-create-issue-for-job key)
    (tlon-babel-create-heading-for-job key 'commit)))

(defun tlon-babel-create-issue-for-job (&optional key)
  "Create an issue based on KEY.
If KEY is not provided, the key in the Markdown buffer at point
  is used."
  (let ((default-directory (tlon-babel-get-repo 'error))
	(key (or key (tlon-babel-get-key-in-buffer))))
    (call-interactively #'forge-create-issue)
    (insert (format "Job: `%s`" key))
    (call-interactively #'forge-post-submit)
    (sleep-for 2)
    (forge-pull)
    (magit-status)
    ;; needs to be done twice for some reason; FIXME
    (forge-pull)))

(defun tlon-babel-create-heading-for-job (&optional key commit)
  "Create a heading based on KEY in `jobs.org'.
If KEY is not provided, the key in the Markdown buffer at point
is used. If COMMIT is non-nil, commit the change."
  (interactive)
  (let* ((key (or key (tlon-babel-get-key-in-buffer)))
	 (heading (format "[cite:@%s]" key))
	 (file (tlon-babel-metadata-lookup "key_original" key "file" (tlon-babel-get-repo-metadata)))
	 (repo (tlon-babel-get-repo-from-file file))
	 (repo-name (tlon-babel-alist-key repo tlon-babel-project-names-and-dirs))
	 (repo-abbrev (alist-get repo-name tlon-babel-project-names-and-abbrevs nil nil 'string=)))
    (with-current-buffer (or (find-buffer-visiting tlon-babel-file-jobs)
			     (find-file-noselect tlon-babel-file-jobs))
      (widen)
      (goto-char (point-min))
      (unless (search-forward heading nil t)
	(goto-char (point-min))
	(re-search-forward tlon-babel-jobs-id nil t)
	(while (and (not (org-at-heading-p)) (not (eobp)))
	  (forward-line))
	(org-insert-heading)
	(insert heading)
	(org-todo 'todo)
	(org-set-tags repo-abbrev)
	(org-sort-entries nil ?a) ; sort entries (a)lphabetically
	(org-sort-entries nil ?o) ; sort entries by t(o)do order
	(save-buffer)))
    (when commit
      (tlon-babel-commit-and-push "Update" tlon-babel-file-jobs))))

(defun tlon-babel-mark-clocked-task-done ()
  "Mark the currently clocked task as DONE."
  (save-window-excursion
    (org-clock-goto)
    (org-todo "DONE")
    (save-buffer)))

(defun tlon-babel-mark-clocked-task-parent-done ()
  "Mark the parent task of currently clocked task as DONE."
  (save-window-excursion
    (org-clock-goto)
    (widen)
    (org-up-heading-safe)
    (org-todo "DONE")
    (save-buffer)))

(defun tlon-babel-get-key-in-heading ()
  "Get the key of the currently clocked task."
  (unless (org-at-heading-p)
    (user-error "Not in an org-mode heading"))
  (let ((heading (substring-no-properties (org-get-heading t t t t))))
    (if (string-match "\\[cite:@\\(.+?\\)\\]\\|Job: `\\(.+?\\)\\.md`" heading)
	(or (match-string 1 heading)
	    (match-string 2 heading))
      (user-error "I wasn't able to find a key in clocked heading"))))

(defun tlon-babel-mark-clocked-heading-done (&optional commit)
  "Mark the heading of the currently clocked task as DONE.
If COMMIT is non-nil, commit and push the changes."
  (let ((key (tlon-babel-get-clock-key)))
    (with-current-buffer (or (find-buffer-visiting tlon-babel-file-jobs)
			     (find-file-noselect tlon-babel-file-jobs))
      (tlon-babel-goto-heading key)
      (org-todo "DONE")
      ;; (org-sort-entries nil ?o) ; sort entries by t(o)do order
      (save-buffer))
    (when commit
      (tlon-babel-commit-and-push "Update" tlon-babel-file-jobs))))

(defun tlon-babel-goto-heading (key)
  "Move point to the heading in `jobs.org' with KEY."
  (with-current-buffer (or (find-buffer-visiting tlon-babel-file-jobs)
			   (find-file-noselect tlon-babel-file-jobs))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
	(when (string= (org-element-property :raw-value headline) (format "[cite:@%s]" key))
	  (goto-char (org-element-property :begin headline)))))))

;;; initialize & finalize functions

(defun tlon-babel-dwim ()
  "Initialize or finalize process based on clocked task."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-clock-in))
  (save-buffer)
  (let* ((action (tlon-babel-get-action-in-label (tlon-babel-get-clock-label)))
	 (stage (pcase major-mode
		  ('org-mode 'initialize)
		  ('markdown-mode 'finalize)
		  (_ (user-error "I don't know what to do in `%s`" major-mode))))
	 (fun (intern (format "tlon-babel-%s" stage)))
	 (arg (intern (format "tlon-babel-%s-%s" stage action))))
    (if (eq stage 'initialize)
	(funcall fun arg)
      (funcall fun))))

(defun tlon-babel-initialize (fun)
  "Initialize process associated with FUN.
Runs all the general initialization functions, followed by the
specific function for the process that is being initialized."
  (let* ((key (tlon-babel-get-clock-key))
	 (repo (tlon-babel-get-repo-from-key key))
	 (default-directory repo))
    (tlon-babel-check-label-and-assignee)
    (tlon-babel-check-branch "main")
    (magit-pull-from-upstream nil)
    (sleep-for 2)
    (cl-multiple-value-bind
	(original-path translation-path original-key)
	(tlon-babel-set-paths-from-clock)
      (let ((topic (tlon-babel-get-clock-topic)))
	(tlon-babel-set-windows original-path translation-path)
	(write-file translation-path)
	(winum-select-window-2)
	(orgit-topic-open topic)
	(tlon-babel-copy-buffer original-path)
	(funcall fun)))))

(defun tlon-babel-finalize ()
  "Finalize current stage of translation process."
  (save-buffer)
  (tlon-babel-check-branch "main")
  (tlon-babel-check-label-and-assignee)
  (tlon-babel-check-file)
  (cl-multiple-value-bind
      (original-path translation-path original-key)
      (tlon-babel-set-paths-from-clock)
    (let* ((current-action (tlon-babel-get-clock-action))
	   (next-label (tlon-babel-get-clock-next-label))
	   (next-assignee (tlon-babel-get-clock-next-assignee next-label)))
      (save-buffer)
      (if (string= current-action "Process")
	  (write-file original-path)
	(write-file translation-path))
      (when (string= current-action "Process")
	(tlon-babel-commit-and-push current-action original-path))
      (tlon-babel-commit-and-push current-action translation-path)
      (tlon-babel-act-on-topic original-key next-label next-assignee
			       (when (string= current-action "Review")
				 'close))
      (tlon-babel-mark-clocked-task-done)
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'"
	       next-label next-assignee)
      (when (string= current-action "Review")
	(tlon-babel-mark-clocked-task-parent-done)
	(tlon-babel-mark-clocked-heading-done 'commit)))))

(defun tlon-babel-initialize-processing ()
  "Initialize processing."
  (cl-multiple-value-bind
      (original-path)
      (tlon-babel-set-paths-from-clock)
    (tlon-babel-set-windows original-path tlon-babel-file-manual)
    (org-id-goto tlon-babel-manual-processing-id)
    (org-narrow-to-subtree)
    (ps/org-show-subtree-hide-drawers)
    (winum-select-window-2)
    (let ((topic (tlon-babel-get-clock-topic)))
      (orgit-topic-open topic))))

(defun tlon-babel-initialize-translation ()
  "Initialize translation.")

(defun tlon-babel-initialize-revision ()
  "Initialize stylistic revision.")

(defun tlon-babel-initialize-check ()
  "Initialize accuracy check."
  ;; we move the buffer displaying the issue to the right, to uncover
  ;; the original file
  (ps/window-buffer-move-dwim)
  (ps/switch-to-last-window)
  (markdown-preview)
  (read-aloud-buf))

(defun tlon-babel-initialize-review ()
  "Initialize review."
  (cl-multiple-value-bind
      (original-path translation-path original-key)
      (tlon-babel-set-paths-from-clock)
    (tlon-babel-log-buffer-latest-user-commit-ediff translation-path)))

;;; TTS

(defun tlon-babel-read-target-buffer ()
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

(defvar tlon-babel-read-aloud-next-action
  'read-aloud-buf)

(defun tlon-babel-read-target-start-or-stop ()
  "Start or stop reading the target buffer."
  (interactive)
  (let ((buffer (tlon-babel-read-target-buffer))
	(current-buffer (current-buffer)))
    (pop-to-buffer buffer)
    (when read-aloud--c-bufpos
      (goto-char read-aloud--c-bufpos))
    (read-aloud-buf)
    ;; we move point to the previous chunk, using the chunk divider
    ;; defined in `read-aloud--grab-text'
    (re-search-backward "[,.:!;]\\|\\(-\\|\n\\|\r\n\\)\\{2,\\}" nil t)
    (pop-to-buffer current-buffer)))

(defun tlon-babel--read-backward-or-forward (direction)
  "Move in DIRECTION in the target buffer."
  (interactive)
  (let ((buffer (tlon-babel-read-target-buffer))
	(current-buffer (current-buffer))
	(fun (if (eq direction 'backward)
		 're-search-backward
	       're-search-forward)))
    (when read-aloud--c-bufpos
      (read-aloud-buf))
    (pop-to-buffer buffer)
    (funcall fun "[,.:!;]\\|\\(-\\|\n\\|\r\n\\)\\{2,\\}" nil t 1)
    (pop-to-buffer current-buffer)))

(defun tlon-babel-read-backward ()
  "Move backward in the target buffer."
  (interactive)
  (tlon-babel--read-backward-or-forward 'backward))

(defun tlon-babel-read-forward ()
  "Move forward in the target buffer."
  (interactive)
  (tlon-babel--read-backward-or-forward 'forward))

;;; Sentence highlighting

;; TODO: (1) highlight sentence in target window; (2) diagnose why first
;; two characters in a sentence are matched to the previous sentence;
;; (3) diagnose performance issues, or else disable `post-command-hook'
;; and rely on other triggers; (4) use `lin-blue' as face for highlighting))))
(defvar tlon-babel-sentence-highlight-offset 0
  "Number of sentences to offset the sentence count in the source window.")

(defun tlon-babel-sentence-highlight-offset-set ()
  "Set the sentence offset.
This command should be run from the source window."
  (interactive)
  (let ((source-window-sentences (count-sentences (point-min) (point)))
	target-window-sentences)
    (with-selected-window (cadr (window-list))
      (setq target-window-sentences (count-sentences (point-min) (point))))
    (setq tlon-babel-sentence-highlight-offset
	  (- source-window-sentences target-window-sentences))))

(defun tlon-babel-remove-source-overlays ()
  "Remove all existing overlays in the source window."
  (remove-overlays (point-min) (point-max)))

(defun tlon-babel-current-window-line ()
  "Get the current line number in the window."
  (save-excursion
    (let ((end (point)))
      (move-to-window-line 0)
      (count-screen-lines (point) end))))

(defun tlon-babel-highlight-corresponding-sentence ()
  "Highlight the corresponding sentence in the source text and unhighlight others."
  (interactive)
  (let* ((source-window (cadr (window-list)))
	 (target-window (car (window-list)))
	 (target-sentence-index)
	 (overlay (make-overlay (point) (point)))
	 (target-window-line (tlon-babel-current-window-line)))
    (with-selected-window target-window
      (save-excursion
	(backward-sentence)
	(setq target-sentence-index (count-sentences (point-min) (point)))))
    (with-selected-window source-window
      (tlon-babel-remove-source-overlays)
      (let ((beg)
	    (end))
	;; +1 because otherwise `count-sentences' throws an error
	(goto-char (1+ (point-min)))
	(while (< (count-sentences (point-min) (point))
		  (+ target-sentence-index tlon-babel-sentence-highlight-offset))
	  (forward-sentence))
	(setq beg (point))
	(forward-sentence)
	(setq end (point))
	(move-overlay overlay beg end (current-buffer))
	(overlay-put overlay 'face 'highlight)
	(backward-sentence)
	(recenter target-window-line)))))

(defvar tlon-babel-enable-automatic-highlighting nil
  "Whether to automatically highlight corresponding sentences.")

(defun tlon-babel-toggle-automatic-highlighting ()
  "Toggle automatic highlighting of corresponding sentences."
  (interactive)
  (if tlon-babel-enable-automatic-highlighting
      (progn
	(remove-hook 'post-command-hook 'tlon-babel-highlight-corresponding-sentence t)
	(setq tlon-babel-enable-automatic-highlighting nil)
	(with-selected-window (cadr (window-list))
	  (tlon-babel-remove-source-overlays))
	(message "Automatic sentence highlighting disabled."))
    (add-hook 'post-command-hook 'tlon-babel-highlight-corresponding-sentence nil t)
    (setq tlon-babel-enable-automatic-highlighting t)
    (message "Automatic sentence highlighting enabled.")))

;;; Checking

(defun tlon-babel-check-branch (branch)
  "Throw an error unless current buffer is in Tlon-Babel branch BRANCH."
  (unless (string= (magit-get-current-branch) branch)
    (user-error "Please switch to the branch `%s' before proceeding" branch)
    t))

(defun tlon-babel-check-file ()
  "Throw an error unless current file matches file in clock."
  (let* ((key (tlon-babel-get-clock-key))
	 (expected-file (tlon-babel-metadata-lookup "key_original" key "file" (tlon-babel-get-repo-metadata)))
	 (actual-file (buffer-file-name)))
    (if (string= expected-file actual-file)
	t
      (user-error "Current file does not match file in clock"))))

(defun tlon-babel-check-label-and-assignee ()
  "Check that clocked action matches topic label and assignee matches user."
  (save-window-excursion
    (let* ((key (tlon-babel-get-clock-key))
	   (topic (format "Job: `%s" key))
	   (clocked-label (tlon-babel-get-clock-label)))
      (magit-status)
      (magit-section-show-level-3-all)
      (goto-char (point-min))
      (if (search-forward topic nil t)
	  (let ((label (tlon-babel-forge-get-label-at-point))
		(assignee (alist-get
			   (tlon-babel-forge-get-assignee-at-point)
			   tlon-babel-github-users nil nil 'string=)))
	    (unless (string= clocked-label label)
	      (user-error "The `org-mode' TODO says the label is `%s', but the actual topic label is `%s'"
			  clocked-label label))
	    (unless (string= user-full-name assignee)
	      (user-error "The `org-mode' TODO says the assignee is `%s', but the actual topic assignee is `%s'"
			  user-full-name assignee))
	    t)
	(user-error "No topic found for %s" key)))))

(defun tlon-babel-check-staged-or-unstaged (file-path)
  "Check if there are staged or unstaged changes in repo involving FILE-PATH."
  (catch 'found
    (dolist (flag '("staged" ""))
      (let ((git-command (format "git diff --%s --name-only %s" flag file-path)))
	(when (not (string-empty-p (shell-command-to-string git-command)))
	  (throw 'found t))))))

(defun tlon-babel-check-staged-or-unstaged-other-than (file-path)
  "Check if there are staged or unstaged changes in repo not involving FILE-PATH."
  (let* ((default-directory (tlon-babel-get-repo-from-file file-path))
	 (all-changes (magit-git-str "diff" "HEAD" "--" "."))
	 (filtered-changes (magit-git-str "diff" "HEAD" "--" file-path)))
    (unless (string= all-changes filtered-changes)
      (user-error "There are staged or unstaged changes in repo. Please commit or stash them before continuing"))))

(defun tlon-babel-check-file-title-match  ()
  "Check that FILE matches its title."
  (let* ((file (buffer-file-name))
	 (base (file-name-base file))
	 (title (tlon-babel-get-metadata-value-in-file "titulo" file))
	 (slugified-title (tlon-core-slugify title)))
    (unless (string= base slugified-title)
      (error "The file `%s' does not match its title" title))))

;;; bibtex
(defun tlon-babel-bibtex-generate-autokey (author year title)
  "Generate a BibTeX key based on AUTHOR, YEAR, and TITLE."
  ;; TODO: check that they key doesn't already exist in all metadata
  (let* ((author (tlon-babel-bibtex-autokey-get-names author))
	 (year (tlon-babel-bibtex-autokey-get-year year))
	 (title (tlon-babel-bibtex-autokey-get-title title))
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

(defun tlon-babel-bibtex-autokey-get-names (name)
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

(defun tlon-babel-bibtex-autokey-get-year (year)
  "Get formatted contents of YEAR field."
  (let* ((str (bibtex-autokey-get-field '("date" "year"))))
    (substring year (max 0 (- (length year) bibtex-autokey-year-length)))))

(defun tlon-babel-bibtex-autokey-get-title (title)
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

(defun tlon-babel-bibtex-add-lang-id ()
  "Supply missing Spanish `landid' field to all bib files."
  (interactive)
  (dolist (file `(,tlon-babel-file-fluid ,tlon-babel-file-stable))
    (with-current-buffer (or (find-buffer-visiting tlon-babel-file-jobs)
			     (find-file-noselect tlon-babel-file-jobs))
      (goto-char (point-min))
      (dolist (string '("translation = " "translator = "))
	(while (re-search-forward string nil t)
	  (bibtex-narrow-to-entry)
	  (goto-char (point-min))
	  (unless (re-search-forward "langid" nil t)
	    (bibtex-set-field "langid" "spanish"))
	  (widen)
	  (bibtex-next-entry))))))

(defun tlon-babel-act-on-topic (original-key label &optional assignee action)
  "Apply LABEL and ASSIGNEE to topic associated with ORIGINAL-KEY.
If ACTION is `convert', convert the existing issue into a pull
request. If ACTION is `close', close issue."
  (let ((topic (format "Job: `%s" original-key))
	(default-directory (tlon-babel-get-repo 'error 'genus)))
    (tlon-babel-magit-status)
    (magit-section-show-level-3-all)
    (goto-char (point-min))
    (if (search-forward topic nil t)
	(progn
	  (tlon-babel-set-parameters topic label)
	  (when assignee (tlon-babel-set-parameters topic assignee))
	  (search-forward topic nil t)
	  (pcase action
	    (`convert (call-interactively 'forge-create-pullreq-from-issue))
	    (`close (call-interactively 'forge-edit-topic-state))))
      (user-error "Could not find topic `%s' in Magit buffer" topic))))

(defun tlon-babel-set-parameters (topic &optional label-or-assignee)
  "Set label or assignee for TOPIC, depending on value of LABEL-OR-ASSIGNEE."
  (let ((assignee-p (member label-or-assignee (mapcar 'car tlon-babel-github-users))))
    (search-forward topic nil t)
    (if assignee-p
	(tlon-babel-set-assignee label-or-assignee)
      (tlon-babel-set-label label-or-assignee))
    (goto-char (point-min))))

;;; Search

(defun tlon-babel-search-topics (search-string &optional repo)
  "Search for SEARCH-STRING in GitHub REPO's issues and pull requests.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let ((repo (or repo (tlon-babel-get-repo nil 'genus)))
	(default-directory repo))
    (forge-search search-string)))

(defun tlon-babel-search-commits (search-string &optional repo)
  "Search for SEARCH-STRING in REPO's commit history.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let* ((repo (or repo (tlon-babel-get-repo nil 'genus)))
	 (default-directory repo))
    (magit-log-all (list "--grep" search-string))))

(defun tlon-babel-search-commit-diffs (search-string &optional repo)
  "Search for SEARCH-STRING in REPO's commit diff history.
If REPO is nil, use the current repo."
  (interactive "sSearch commits (-S): ")
  ;; (let* ((repo (or repo (tlon-babel-get-repo nil 'genus)))
  ;; (default-directory repo))
  (magit-log-all `("-S" ,search-string)))

(defun tlon-babel-search-files (search-string &optional repo)
"Search for SEARCH-STRING in REPO files.
If REPO is nil, use the current repo."
(interactive "sSearch string: ")
(let ((repo (or repo (tlon-babel-get-repo nil 'genus)))
      (default-directory repo))
  (consult-ripgrep tlon-babel-dir-babel search-string)))

(defun tlon-babel-search-multi (search-string &optional repo)
  "Search for SEARCH-STRING in REPO files, commit history, and GitHub issues.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let ((repo (or repo (tlon-babel-get-repo nil 'genus)))
	(win1 (selected-window)))
    (ps/window-split-if-unsplit)
    (tlon-babel-search-topics search-string repo)
    (split-window-below)
    (tlon-babel-search-commits search-string repo)
    (select-window win1)
    (tlon-babel-search-files search-string repo)))

(defun tlon-babel-commit-and-push (action file)
  "Commit and push changes to FILE.
The commit message is ACTION followed by the name of FILE."
  (let* ((default-directory (tlon-babel-get-repo-from-file file)))
    (tlon-babel-check-staged-or-unstaged-other-than file)
    (when (string= (magit-get-current-branch) "main")
      (magit-pull-from-upstream nil)
      (sleep-for 2))
    (magit-stage-file file)
    ;; we check for staged or unstaged changes to FILE because
    ;; `magit-commit-create' interrupts the process if there aren't
    (when (tlon-babel-check-staged-or-unstaged file)
      (magit-commit-create (list "-m" (format "%s %s" action (file-name-nondirectory file)))))
    (call-interactively #'magit-push-current-to-pushremote)))

;;; Change topic properties

(defun tlon-babel-select-label ()
  "Prompt the user to select a LABEL."
  (let ((label (completing-read "What should be the label? "
				tlon-babel-label-actions)))
    label))

(defun tlon-babel-set-label (label)
  "Apply LABEL to topic at point.
Note that this only works for topics listed in the main buffer."
  (interactive
   (list (tlon-babel-select-label)))
  (let* ((topic (forge-get-topic (forge-topic-at-point)))
	 (repo  (forge-get-repository topic))
	 (crm-separator ","))
    (forge--set-topic-labels
     repo topic (list label))))

(defun tlon-babel-select-assignee ()
  "Prompt the user to select an ASSIGNEE.
The prompt defaults to the current user."
  (let ((assignee (completing-read "Who should be the assignee? "
				   tlon-babel-github-users nil nil
				   (tlon-babel-find-key-in-alist
				    user-full-name
				    tlon-babel-github-users))))
    assignee))

(defun tlon-babel-set-assignee (assignee)
  "Make ASSIGNEE the assignee of topic at point."
  (interactive
   (list (tlon-babel-select-assignee)))
  (let* ((topic (forge-get-topic (forge-topic-at-point)))
	 (repo  (forge-get-repository topic))
	 (value (closql--iref topic 'assignees))
	 (choices (mapcar #'cadr (oref repo assignees)))
	 (crm-separator ","))
    (forge--set-topic-assignees
     repo topic
     (list assignee))))

(defun tlon-babel-set-initial-label-and-assignee ()
  "Set label to `Awaiting processing' and assignee to current user."
  (tlon-babel-set-label "Awaiting processing")
  (tlon-babel-set-assignee (tlon-babel-find-key-in-alist user-full-name tlon-babel-github-users)))

;; this is just a slightly tweaked version of `forge-edit-topic-labels'.
;; It differs from that function only in that it returns the selection
;; rather than submitting it.
(defun tlon-babel-forge-get-label (topic)
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

(defun tlon-babel-forge-get-assignee (topic)
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
;; `tlon-babel-forge-get-label'. I don't know how to do this
;; properly with `magit-completing-read-multiple', so I just simulate a
;; RET keypress.
(defun tlon-babel-forge-get-label-at-point ()
  "Return the label of the topic at point.
If the topic has more than one label, return the first."
  (let ((exit-minibuffer-func (lambda () (exit-minibuffer))))
    (minibuffer-with-setup-hook
	(lambda ()
	  (add-hook 'post-command-hook exit-minibuffer-func t t))
      (tlon-babel-forge-get-label (forge-current-topic)))))

(defun tlon-babel-forge-get-assignee-at-point ()
  "Return the assignee of the topic at point.
If the topic has more than one assignee, return the first."
  (let ((exit-minibuffer-func (lambda () (exit-minibuffer))))
    (minibuffer-with-setup-hook
	(lambda ()
	  (add-hook 'post-command-hook exit-minibuffer-func t t))
      (tlon-babel-forge-get-assignee (forge-current-topic)))))

(defun tlon-babel-open-original-or-translation ()
  "Open the translation if visiting the original, and vice versa."
  (interactive)
  (let* ((current-file (file-name-nondirectory (buffer-file-name))))
    (alist-get current-file tlon-babel-translation-alist
	       (lambda (key default) default))))

(defun tlon-babel-find-key-in-alist (value alist)
  "Find the corresponding key for a VALUE in ALIST."
  (let ((pair (cl-find-if (lambda (x) (equal (cdr x) value)) alist)))
    (when pair
      (car pair))))

(defun tlon-babel-get-next-car (car alist)
  "Find the car in cons following CAR in ALIST."
  (catch 'result
    (let ((found nil))
      (dolist (pair alist)
	(when (and found (not (string= car (car pair))))
	  (throw 'result (car pair)))
	(when (string= car (car pair))
	  (setq found t)))
      nil))) ;; if CAR was the last in ALIST, there's no "next"

;;;; glossary

(defun tlon-babel-glossary-alist ()
  "Read `Glossary.csv` and return it as an alist."
  (with-temp-buffer
    (insert-file-contents tlon-babel-file-glossary)
    (let ((lines (split-string (buffer-string) "\n" t))
	  (result '()))
      (dolist (line lines result)
	(let* ((elements (split-string line "\",\""))
	       (key (substring (nth 0 elements) 1)) ; removing leading quote
	       (value (if (string-suffix-p "\"" (nth 1 elements))
			  (substring (nth 1 elements) 0 -1)   ; if trailing quote exists, remove it
			(nth 1 elements)))) ; otherwise, use as-is
	  (push (cons key value) result))))))

(defun tlon-babel-glossary-dwim ()
  "Add a new entry to the glossary or modify an existing entry."
  (interactive)
  (let* ((english-terms (mapcar 'car (tlon-babel-glossary-alist)))
	 (term (completing-read "Term: " english-terms)))
    (if (member term english-terms)
	(tlon-babel-glossary-modify term)
      (tlon-babel-glossary-add term))))

(defun tlon-babel-glossary-add (&optional english spanish)
  "Add a new entry to the glossary for ENGLISH and SPANISH terms."
  (interactive)
  (let ((english (or english (read-string "English term: ")))
	(spanish (or spanish (read-string "Spanish term: ")))
	(explanation (read-string "Explanation (optional; in Spanish): ")))
    (with-current-buffer (find-file-noselect tlon-babel-file-glossary)
      (goto-char (point-max))
      (insert (format "\n\"%s\",\"%s\",\"EN\",\"ES\"" english spanish))
      (goto-char (point-min))
      (flush-lines "^$")
      (save-buffer)
      (tlon-babel-glossary-commit "add" english explanation))))

(defun tlon-babel-glossary-modify (english)
  "Modify an entry in the glossary corresponding to the ENGLISH term."
  (let* ((spanish (cdr (assoc english (tlon-babel-glossary-alist))))
	 (spanish-new (read-string "Spanish term: " spanish))
	 (explanation (read-string "Explanation (optional; in Spanish): ")))
    (with-current-buffer (find-file-noselect tlon-babel-file-glossary)
      (goto-char (point-min))
      (while (re-search-forward (format "^\"%s\",\"%s\",\"EN\",\"ES\"" english spanish) nil t)
	(replace-match (format "\"%s\",\"%s\",\"EN\",\"ES\"" english spanish-new)))
      (goto-char (point-min))
      (flush-lines "^$")
      (save-buffer)
      (tlon-babel-glossary-commit "modify" english explanation))))

(defun tlon-babel-glossary-commit (action term &optional description)
  "Commit glossary changes.
ACTION describes the action (\"add\" or \"modify\") performed on
the glossary. TERM refers to the English glossary term to which
this action was performed. These two variables are used to
construct a commit message of the form \'Glossary: ACTION
\"TERM\"\', such as \'Glossary: add \"repugnant conclusion\"\'.
Optionally, DESCRIPTION provides an explanation of the change."
  (let ((default-directory tlon-babel-dir-genus)
	(description (if description (concat "\n\n" description) "")))
    ;; save all unsaved files in repo
    (magit-save-repository-buffers)
    (magit-pull-from-upstream nil)
    ;; if there are staged files, we do not commit or push the changes
    (unless (magit-staged-files)
      (tlon-babel-check-branch "main")
      (magit-run-git "add" tlon-babel-file-glossary)
      (let ((magit-commit-ask-to-stage nil))
	(magit-commit-create (list "-m" (format  "Glossary: %s \"%s\"%s"
						 action term description)))))))
;; (call-interactively #'magit-push-current-to-pushremote))))

;;;

(defmacro tlon-babel-create-file-opening-command (file-path)
  "Create a command to open file in FILE-PATH."
  (let* ((file-base (downcase (file-name-base file-path)))
	 (file-name (file-name-nondirectory file-path))
	 (command-name (intern (concat "tlon-babel-open-" file-base))))
    `(defun ,command-name ()
       ,(format "Open `%s'." file-name)
       (interactive)
       (find-file (file-name-concat
		   tlon-babel-dir-babel
		   ,file-path)))))

(tlon-babel-create-file-opening-command "refs/Glossary.csv")
(tlon-babel-create-file-opening-command "refs/fluid.bib")
(tlon-babel-create-file-opening-command "refs/stable.bib")
(tlon-babel-create-file-opening-command "manual.org")
(tlon-babel-create-file-opening-command "readme.md")

(transient-define-prefix tlon-babel-dispatch ()
  "Dispatch a `tlon-babel' command."
  [["Main"
    ("j" "job"                            tlon-babel-create-job)
    ("r" "dwim"                           tlon-babel-dwim)
    ("m" "magit"                          tlon-babel-magit-status)
    ("n" "forge"                          tlon-babel-forge)
    """Package"
    ("p p" "update"                       tlon-babel-update)
    ("p l" "load variables"               tlon-babel-load-variables)
    ("p v" "version"                      tlon-babel-version)
    ]
   ["Add"
    ("a a" "to glossary"                  tlon-babel-glossary-dwim)
    """Search"
    ("s s" "multi"                        tlon-babel-search-multi)
    ("s c" "commits"                      tlon-babel-search-commits)
    ("s f" "files"                        tlon-babel-search-files)
    ("s i" "topics"                       tlon-babel-search-topics)
    ("s t" "translation"                  tlon-babel-search-translation)
    ]
   ["Open file"
    ("f f" "counterpart"                  tlon-babel-open-counterpart)
    ("f g" "Glossary.csv"                 tlon-babel-open-glossary)
    ("f m" "manual.md"                    tlon-babel-open-manual)
    ("f r" "readme.md"                    tlon-babel-open-readme)
    ]
   ["Open directory"
    ("d d" "repo"                         tlon-babel-open-repo)
    ("d g" "genus"                        tlon-babel-open-genus-repo)
    ("d b" "bae"                          tlon-babel-open-bae-repo)
    ("d u" "utilitarismo"                 tlon-babel-open-utilitarismo-repo)
    ("d l" "largoplacismo"                tlon-babel-open-largoplacismo-repo)
    ]
   ["Browse"
    ("b b" "file"                         tlon-babel-browse-file)
    ("b r" "repo"                         tlon-babel-browse-repo)
    """File changes"
    ("h h" "Log"                          magit-log-buffer-file)
    ("h d" "Diffs since last user change" tlon-babel-log-buffer-latest-user-commit)
    ("h e" "Ediff with last user change"  tlon-babel-log-buffer-latest-user-commit-ediff)]
   ["Clock"
    ("c c" "Issue"                        tlon-babel-open-clock-topic)
    ("c f" "File"                         tlon-babel-open-clock-file )
    ("c o" "Heading"                      org-clock-goto)
    """Issue"
    ("i i" "Open counterpart"             tlon-babel-open-forge-counterpart)
    ("i I" "Open file"                    tlon-babel-open-forge-file)
    ]
   ]
  )

(defun tlon-babel-browse-file ()
  "Browse the current file in the Tlon-Babel repository."
  (interactive)
  (if-let (file (buffer-file-name))
      (let* ((repo (tlon-babel-get-repo-from-file file))
	     (repo-name (tlon-babel-get-name-from-repo repo))
	     (repo-url (concat "https://github.com/tlon-team/" repo-name))
	     (file-url (concat "/blob/main/" (file-relative-name (buffer-file-name) repo))))
	(browse-url (concat repo-url file-url)))
    (user-error "Buffer is not visiting a file")))

(defun tlon-babel-browse-repo ()
  "Browse the Tlon-Babel repository."
  (interactive)
  (let* ((repo (tlon-babel-get-repo))
	 (repo-name (tlon-babel-get-name-from-repo repo)))
    (browse-url (concat "https://github.com/tlon-team/" repo-name))))

(defun tlon-babel-open-repo ()
  "Open the Babel repository."
  (interactive)
  (dired (tlon-babel-get-repo nil 'genus)))

(defun tlon-babel-open-bae-repo ()
  "Open the Biblioteca Altruismo Eficaz repository."
  (interactive)
  (dired tlon-babel-dir-bae))

(defun tlon-babel-open-utilitarismo-repo ()
  "Open the Utilitarismo repository."
  (interactive)
  (dired tlon-babel-dir-utilitarismo))

(defun tlon-babel-open-largoplacismo-repo ()
  "Open the Largoplacismo repository."
  (interactive)
  (dired tlon-babel-dir-largoplacismo))

(defun tlon-babel-open-genus-repo ()
  "Open the Genus repository."
  (interactive)
  (dired tlon-babel-dir-genus))

;;; request
;;;; EAF API
(defconst tlon-babel-eaf-api-url
  "https://forum.effectivealtruism.org/graphql"
  "URL for the EAF GraphQL API endpoint.")

(defvar tlon-babel-eaf-objects
  '(post tag)
  "List of entities supported by the EAF GraphQL API.")

(defun tlon-babel-eaf-post-query (id)
  "Return an EA Forum GraphQL query for post whose ID is ID."
  (concat "{\"query\":\"{\\n  post(\\n    input: {\\n      selector: {\\n        _id: \\\""
	  id
	  "\\\"\\n      }\\n    }\\n  ) {\\n    result {\\n      _id\\n      postedAt\\n      url\\n      canonicalSource\\n      title\\n      contents {\\n        markdown\\n        ckEditorMarkup\\n      }\\n      slug\\n      commentCount\\n      htmlBody\\n      baseScore\\n      voteCount\\n      pageUrl\\n      legacyId\\n      question\\n      tableOfContents\\n      author\\n      user {\\n        username\\n        displayName\\n        slug\\n        bio\\n      }\\n      coauthors {\\n        _id\\n        username\\n        displayName\\n        slug\\n      }\\n    }\\n  }\\n}\\n\"}"))

(defun tlon-babel-eaf-tag-query (slug)
  "Return an EA Forum GraphQL query for tag whose slug is SLUG."
  (concat "{\"query\":\"{\\n  tag(input: { selector: { slug: \\\""
	  slug
	  "\\\" } }) {\\n    result {\\n      name\\n      slug\\n      description {\\n        html\\n      }\\n      parentTag {\\n        name\\n      }\\n    }\\n  }\\n}\\n\"}"))

(defun tlon-babel-eaf-request (id-or-slug &optional async)
  "Run an EAF request for ID-OR-SLUG.
If ASYNC is t, run the request asynchronously."
  (let* ((object (tlon-babel-eaf-get-object id-or-slug))
	 (fun (pcase object
		('post 'tlon-babel-eaf-post-query)
		('tag 'tlon-babel-eaf-tag-query)
		(_ (error "Invalid object: %S" object))))
	 (query (funcall fun id-or-slug))
	 response)
    (request
      tlon-babel-eaf-api-url
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

(defun tlon-babel--eaf-get-post-result (response)
  "Get post details from EA Forum API RESPONSE."
  (let* ((post (cdr (assoc 'post response)))
	 (result (cdr (assoc 'result post))))
    result))

(defun tlon-babel-eaf-get-post-id (response)
  "Get post ID from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-post-result response))
	 (id (cdr (assoc '_id result))))
    id))

(defun tlon-babel-eaf-get-post-html (response)
  "Get post HTML from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-post-result response))
	 (html (cdr (assoc 'htmlBody result))))
    html))

(defun tlon-babel-eaf-get-post-title (response)
  "Get post title from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-post-result response))
	 (title (cdr (assoc 'title result))))
    title))

(defun tlon-babel-eaf-get-post-author (response)
  "Get post author from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-post-result response))
	 (author (cdr (assoc 'author result))))
    author))

(defun tlon-babel-eaf-get-post-username (response)
  "Get post author username from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-post-result response))
	 (user (cdr (assoc 'user result)))
	 (username (cdr (assoc 'username user))))
    username))

(defun tlon-babel--eaf-get-tag-result ()
  "Get tag details from EA Forum API RESPONSE."
  (let* ((tag (cdr (assoc 'tag response)))
	 (result (cdr (assoc 'result tag))))
    result))

(defun tlon-babel-eaf-get-tag-slug (response)
  "Get tag slug from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-tag-result response))
	 (slug (cdr (assoc 'slug result))))
    slug))

(defun tlon-babel-eaf-get-tag-html (response)
  "Get tag HTML from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-tag-result response))
	 (description (cdr (assoc 'description result)))
	 (html (cdr (assoc 'html description))))
    html))

(defun tlon-babel-eaf-get-tag-title ()
  "Get tag title from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-tag-result response))
	 (title (cdr (assoc 'name result))))
    (tlon-babel-shorten-title title)))

(defun tlon-babel-shorten-title (title)
  "Return a shortened version of TITLE."
  (string-match "\\([[:alnum:] ,'‘’“”@#$%*\\^`~&\"]*\\)" title)
  (match-string 1 title))

;;;; BAE API

(defun tlon-babel-get-bae-log ()
  "Get error log from BAE repo."
  (interactive)
  (require 'json)
  (require 'url)
  (let* ((url "https://altruismoeficaz.net/api/logs")
	 (url-request-method "GET")
	 (url-mime-charset-string "utf-8;q=1, iso-8859-1")
	 (response-buffer (url-retrieve-synchronously url))
	 (json-object-type 'hash-table)
	 (json-array-type 'list)
	 (output-buffer-name "*BAE error log*"))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      ;; Kill any existing buffer with the same name before creating a new one
      (when (get-buffer output-buffer-name)
	(kill-buffer output-buffer-name)) 
      (let* ((output-buffer (get-buffer-create output-buffer-name))
	     (logs (json-read)))
	(with-current-buffer output-buffer
	  (erase-buffer)) ; ensure the buffer is empty before inserting
	(dolist (log logs)
	  (tlon-babel-pretty-print-bae-hash-table log output-buffer))
	(display-buffer output-buffer)
	(switch-to-buffer output-buffer)
	(read-only-mode)
	(goto-char (point-min))))))

(defun tlon-babel-update-bae-log (&optional retries)
  "Update error log from BAE repo.
Retries 2 more times if response code is 504 before giving up. If
RETRIES is a number, it will retry that many times instead of 2."
  (interactive)
  (let* ((url "https://altruismoeficaz.net/api/update")
	 (token (tlon-babel-get-bae-token))
	 (retries (if (numberp retries) retries 0)))
    (request
      url
      :type "POST"
      :headers `(("Authorization" . ,(concat "Bearer " token)))
      :sync nil
      :status-code '((504 . (lambda (&rest _)
			      (if (< retries 2)
				  (progn
				    (message "Got 504, Gateway Timeout. Retrying...")
				    (tlon-babel-update-bae-log (1+ retries)))
				(message "Got 504, Gateway Timeout. My patience has a limit!")))))
      :complete (cl-function
		 (lambda (&key response &allow-other-keys)
		   (unless (request-response-error-thrown response)
		     (tlon-babel-get-bae-log)))))))

(defun tlon-babel-get-bae-token ()
  "Get BAE API token."
  (let* ((username (tlon-babel-alist-key user-full-name tlon-babel-github-users))
	 (url "https://altruismoeficaz.net/api/auth/login")
	 (url-request-method "POST")
	 (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (url-request-data
	  (concat "username=" (url-hexify-string username)
		  "&password=" (url-hexify-string
				(auth-source-pass-get 'secret (concat "tlon/BAE/altruismoeficaz.net/" username))))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (let* ((json-object-type 'alist)
	     (json-array-type 'list)
	     (json-key-type 'string)
	     (json-response (json-read-from-string (buffer-string))))
	(cdr (assoc "access_token" json-response))))))

(defun tlon-babel-pretty-print-bae-hash-table (hash-table buffer)
  "Print HASH-TABLE in a human-friendly way in BUFFER."
  (require 'cl)
  (with-current-buffer buffer
    (maphash
     (lambda (key value)
       (if (string= key "message")
	   (let* ((message-parts (split-string value "at filename="))
		  (text (car message-parts))
		  (raw-filename (cadr message-parts)))
	     (insert (format "%s\n" text))
	     (when raw-filename
	       (let* ((filename (replace-regexp-in-string
				 "/home/fede/biblioteca-altruismo-eficaz/translations/"
				 "~/Library/CloudStorage/Dropbox/repos/biblioteca-altruismo-eficaz/translations/"
				 raw-filename)))
		 (lexical-let ((file filename))
		   (insert-button filename
				  'action (lambda (x) (find-file file))
				  'follow-link t))))
	     (insert "\n\n"))
	 (insert (format "%s: %s\n" key value))))
     hash-table)))

;;;; fix log errors helper functions

(defun tlon-babel-collect-bibtex-keys-in-buffer ()
  "Collect all the bibtex keys in the current buffer.
Display the collected keys in a new buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (keys)
      (while (re-search-forward "@\\([[:alnum:]]+[[:digit:]]\\{4\\}[[:alnum:]]+\\)" nil t)
	(push (match-string 1) keys))
      (with-output-to-temp-buffer "*Bibtex keys*"
	(princ (mapconcat #'identity (delete-dups keys) "\n"))))))

(defun tlon-babel-move-bibtex-entries-to-genus ()
  "Move the bibtex entries in the current buffer from `old.bib' to `fluid.bib'.
If the key is not found, it is added to the list of missing keys."
  (interactive)
  (let ((missing-keys '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bibtex-key (buffer-substring-no-properties
			   (line-beginning-position)
			   (line-end-position))))
          (save-excursion
            (with-current-buffer (find-file-noselect ps/file-personal-bibliography-old)
	      (goto-char (point-min))
	      (if (re-search-forward (format "{%s," bibtex-key) nil t)
		  (call-interactively 'ps/bibtex-move-entry-to-tlon)
		(add-to-list 'missing-keys bibtex-key))))
	  (forward-line)))
      (with-output-to-temp-buffer "*Missing BibTeX Keys*"
	(dolist (key missing-keys)
	  (princ (format "%s\n" key)))))))

;;; html import

(defvar tlon-babel-pandoc-convert-from-file
  "pandoc -s '%s' -t markdown -o '%s'"
  "Command to convert from HTML file to Markdown.")

(defvar tlon-babel-pandoc-convert-from-url
  "pandoc -s -r html '%s' -o '%s'"
  "Command to convert from URL to Markdown.")

(defun tlon-babel-save-html-to-file (html)
  "Save the HTML string HTML to a temporary file."
  (let ((filename (make-temp-file "tlon-babel-request-" nil ".html")))
    (with-temp-file filename
      (insert html))
    filename))

;;; translation

(defun tlon-babel-search-translation (string)
  "Search for a Spanish translation of English STRING."
  (interactive "sString to translate: ")
  (let ((urls '("https://spanish.stackexchange.com/search?q=%s"
		"https://es.bab.la/diccionario/ingles-espanol/%s"
		"https://en.wikipedia.org/w/index.php?fulltext=Search&profile=default&search=%s"
		"https://context.reverso.net/traduccion/ingles-espanol/%s"
		"https://www.linguee.com/english-spanish/search?query=%s")))
    (dolist (url urls)
      (browse-url (format url (url-hexify-string string)) 'new-buffer)))
  (ps/goldendict-search-input string))

(defun tlon-babel-file-to-string (file-path)
  "Read the contents of FILE-PATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (unfill-region (point-min) (point-max))
    (buffer-string)))

(defun tlon-babel-gpt-rewrite ()
  "Docstring."
  (interactive)
  (let* ((text (if (region-active-p)
		   (buffer-substring-no-properties (region-beginning) (region-end))
		 (read-string "Text to rewrite: "))))
    (require 'gptel)
    (gptel-request
     (format "Por favor, genera las mejores diez variantes del siguiente texto castellano: '%s'. Por favor, devuelve todas las variantes en una única linea, separadas por '|'. No insertes un espacio ni antes ni después de '|'. No agregues ningún comentario aclaratorio: solo necesito la lista de variantes. A modo de ejemplo, para la expresión 'búsqueda de poder' el texto a devolver sería: 'ansia de poder|ambición de poder|búsqueda de autoridad|sed de poder|afán de poder|aspiración de poder|anhelo de poder|deseo de control|búsqueda de dominio|búsqueda de control' (esta lista solo pretende ilustrar el formato en que debes presentar tu respuesta). Gracias!" text)
     :callback
     (lambda (response info)
       (if (not response)
	   (message "gptel-quick failed with message: %s" (plist-get info :status))
	 (let* ((variants (split-string response "|"))
		(variant (completing-read "Variant: " variants)))
	   (delete-region (region-beginning) (region-end))
	   (kill-new variant)))))))

(defun tlon-babel-gpt-translate (text)
  "Return ten alternative translations of TEXT."
  (interactive "sText to translate: ")
  (require 'gptel)
  (gptel-request
   (format "Generate the best ten Spanish translations of the following English text: '%s'. Please return each translation on the same line, separated by '|'. Do not add a space either before or after the '|'. Do not precede your answer by 'Here are ten Spanish translations' or any comments of that sort: just return the translations. An example return string for the word 'very beautiful' would be: 'muy bello|muy bonito|muy hermoso|muy atractivo' (etc). Thanks!" text)
   :callback
   (lambda (response info)
     (if (not response)
	 (message "gptel-quick failed with message: %s" (plist-get info :status))
       (let ((translations (split-string response "|")))
	 (kill-new (completing-read "Translation: " translations)))))))

(defun tlon-babel-gpt-translate-file (file)
  "Translate FILE."
  (let* ((counterpart (tlon-babel-get-counterpart-filename file))
	 (target-path (concat
		       (file-name-sans-extension counterpart)
		       "--gpt-translated.md")))
    (gptel-request
     (concat "Translate the following text into Spanish:\n\n"
	     (tlon-babel-file-to-string file))
     :callback
     (lambda (response info)
       (if (not response)
	   (message "gptel-quick failed with message: %s" (plist-get info :status))
	 (with-temp-buffer
	   (insert response)
	   (write-region (point-min) (point-max) target-path)))))))

(defun tlon-babel-create-file-from-commit (file-path commit-hash)
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

;;; Markdown cleanup

(defun tlon-babel-split-footnotes-into-separate-paragraphs ()
  "Split footnotes into separate paragraphs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[\\^[[:digit:]]\\{1,3\\}\\]:\\)" nil t)
      (replace-match "\n\n\\1"))))

(defun tlon-babel-fix-list ()
  "Format the current paragraph into a proper list."
  (interactive)
  (save-excursion
    (let ((beg (progn (backward-paragraph) (point)))
	  (end (progn (forward-paragraph) (point))))
      (goto-char beg)
      (replace-regexp-in-region " - " "\n- " beg end))))

(defun tlon-babel-fix-footnote-punctuation ()
  "Place footnotes after punctuation mark."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; we add a character at the beginning to avoid matching the footnote targets
    (while (re-search-forward "\\(.\\)\\(\\[\\^[[:digit:]]\\{1,3\\}\\]\\)\\([[:punct:]]\\)" nil t)
      (replace-match "\\1\\3\\2"))))

(defun tlon-babel-post-translation-cleanup ()
  "Cleanup processes to be run after a translation is completed."
  (interactive)
  (tlon-babel-fix-footnote-punctuation)
  ;; potentially add more cleanup processes here
  )

(provide 'tlon-babel)
;;; tlon-babel.el ends here
