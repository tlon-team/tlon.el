;;; tlon-babel.el --- A companion package for the Babel project. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.14
;; URL: https://github.com/tlon-team/tlon-babel
;; Keywords: convenience tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(require 'citar)
(require 'consult)
(require 'doi-utils)
(require 'ebib-extras)
(require 'files-extras)
(require 'forge)
(require 'forge-search)
(require 'goldendict-ng)
(require 'magit)
(require 'magit-extra)
(require 'org)
(require 'org-clock)
(require 'org-roam)
(require 'paths)
(require 'substitute)
(require 'tlon-babel-dispatch)
(require 'tlon-babel-core)
(require 'tlon-babel-refs)
(require 'tlon-babel-yaml)
(require 'tlon-babel-counterpart)
(require 'tlon-core)
(require 'unfill)
(require 'window-extras)
(require 'winum)

;;;; Customization:

;;;; User options

(defgroup tlon-babel ()
  "A companion package for the Babel project."
  :group 'files)

;;;; Variables

;;;;; Files and dirs

(defvar tlon-babel-todos-jobs-file nil
  "Org file that contains the ID in `paths-tlon-babel-todos-jobs-id'.
This variable should not be set manually.")

(defvar tlon-babel-todos-generic-file nil
  "Org file that contains the ID in `paths-tlon-babel-todos-generic-id'.
This variable should not be set manually.")

(defvar tlon-babel-post-init-hook nil
  "Hook run at the end of `tlon-babel-init'.")

;;;;;; lookup

;; TODO: all this should be adapted following the model of
;; `tlon-babel-get-file-glossary' i.e. a repo-relative path stored in a variable
;; and a function to get it for specific languages

(defvar tlon-babel-dir-correspondences
  (file-name-concat (tlon-babel-repo-lookup :dir :name "babel-es") "correspondences/")
  "Directory where correspondence files are stored.")

(defvar tlon-babel-dir-dict
  (file-name-concat (tlon-babel-repo-lookup :dir :name "babel-es") "dict/")
  "Directory where dictionary files are stored.")

(defvar tlon-babel-file-hyphenation
  "hyphenation.json"
  "File containing hyphenation rules.")

(defvar tlon-babel-file-babel-manual
  (file-name-concat (tlon-babel-repo-lookup :dir :name "babel-core") "manual.org")
  "File containing the Babel manual.")

(defvar tlon-babel-file-jobs
  (file-name-concat (tlon-babel-repo-lookup :dir :name "babel-core") "jobs.org")
  "File containing the jobs.")

(defvar tlon-babel-file-url-correspondences
  (file-name-concat tlon-babel-dir-correspondences "url-correspondences.json")
  "File containing the URL correspondences.")

(defvar tlon-babel-file-section-correspondences
  (file-name-concat tlon-babel-dir-correspondences "section-correspondences.json")
  "File containing the section correspondences.")

(defvar tlon-babel-file-bibtex-correspondences
  (file-name-concat tlon-babel-dir-correspondences "bibtex-correspondences.json")
  "File containing the BibTeX correspondences.")

(defmacro tlon-babel-create-file-opening-command (file)
  "Create a command to open FILE."
  (let* ((file-base (downcase (file-name-base file)))
	 (file-name (file-name-nondirectory file))
	 (command-name (intern (concat "tlon-babel-open-" file-base))))
    `(defun ,command-name ()
       ,(format "Open `%s' file." file-name)
       (interactive)
       (find-file (symbol-value (intern (concat "tlon-babel-file-" ,file)))))))

(tlon-babel-create-file-opening-command "manual")
(tlon-babel-create-file-opening-command "readme")
(tlon-babel-create-file-opening-command "jobs")
(tlon-babel-create-file-opening-command "fluid")
(tlon-babel-create-file-opening-command "stable")
(tlon-babel-create-file-opening-command "url-correspondences")
(tlon-babel-create-file-opening-command "section-correspondences")
(tlon-babel-create-file-opening-command "bibtex-correspondences")

;;;;; html import

(defconst tlon-babel-pandoc-convert-from-file
  "pandoc -s '%s' -t markdown -o '%s'"
  "Command to convert from HTML file to Markdown.")

(defconst tlon-babel-pandoc-convert-from-url
  "pandoc -s -r html '%s' -o '%s'"
  "Command to convert from URL to Markdown.")
;;;;; URLs

(defconst tlon-babel-tlon-github-url
  "https://github.com/tlon-team/"
  "URL of the Tlön account on GitHub.")

;;;;; Version

(defvar tlon-babel-version "0.1.13")

;;;;; Clocked heading

(defconst tlon-babel-key-regexp "`\\(.+?\\)\\(\\.md\\)?`"
  "Regular expression for matching bibtex keys in clocked headings.
The second capture group handles the `.md' extension, which we used previously.")

;;;; Functions

;;;;; version

(defun tlon-babel-version ()
  "Return the version of the `tlon-babel' package."
  (interactive)
  (message "`tlon-babel' version %s" tlon-babel-version))

;;;;; init

(defvar org-capture-templates)
(defvar tlon-babel-refs-bibliography-files)
(defun tlon-babel-init ()
  "Initialize `tlon-babel'."
  (interactive)
  (tlon-babel-warn-if-unbound 'paths-tlon-babel-todos-jobs-id)
  (tlon-babel-warn-if-unbound 'paths-tlon-babel-todos-generic-id)
  (setq paths-files-bibliography-all
	`(,@paths-files-bibliography-personal
	  ,@tlon-babel-refs-bibliography-files))
  (run-hooks 'tlon-babel-post-init-hook)
  (message "Initialized `tlon-babel'"))

(dolist (template `(("tbJ" "Tlön: Babel: Create a new Babel job" entry
		     (id ,paths-tlon-babel-todos-jobs-id)
		     "** %c" :immediate-finish t :empty-lines 1 :jump-to-captured t)
		    ("tbG" "Tlön: Babel: Create new generic todo from GitHub" entry
		     (id ,paths-tlon-babel-todos-generic-id)
		     "** %c" :immediate-finish t :empty-lines 1 :prepend t :jump-to-captured t)))
  (push template org-capture-templates))

(defun tlon-babel-warn-if-unbound (var)
  "Signal an error if the value of VAR is not set."
  (unless (symbol-value var)
    (user-error "Please set the value of `%s'" (symbol-name var))))

;;;;; [name]

(defun tlon-babel-get-file-from-key (key)
  "Return the file path of KEY."
  (if-let ((file (tlon-babel-metadata-lookup
		  (tlon-babel-metadata-in-repos :subtype 'translations) "file" "original_key" key)))
      file
    (user-error "Metadata lookup for key `%s' returned nil" key)))

(defun tlon-babel-get-key-from-file (file)
  "Return the bibtex key of FILE."
  (or
   ;; when in `translations'
   (tlon-babel-metadata-lookup (tlon-babel-metadata-in-repo) "translation_key" "file" file)
   ;; when file in `originals'
   (let ((translation (tlon-babel-get-counterpart file)))
     (tlon-babel-metadata-get-field-value-in-file "original_key" translation))))

(defun tlon-babel-set-translation-language (language)
  "Set the translation LANGUAGE."
  (interactive (list (completing-read "Language: " tlon-babel-languages)))
  (setq tlon-babel-translation-language language))

;;;;; User commits

(defun tlon-babel-latest-user-commit-in-file (&optional file)
  "Return latest commit by the current user in FILE.
If no FILE is provided, use the file visited by the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (default-directory (file-name-directory file))
	 (user (tlon-babel-user-lookup :git :name user-full-name))
	 ;; get most recent commit in FILE by USER
	 (output (shell-command-to-string (format "git log --pretty=format:'%%h %%an %%s' --follow -- '%s' | grep -m 1 '%s' | awk '{print $1}'" file user)))
	 (commit (car (split-string output "\n"))))
    commit))

(defun tlon-babel-log-buffer-latest-user-commit (&optional file)
  "Show modifications to FILE since the latest commit by the current user.
If no FILE is provided, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-babel-latest-user-commit-in-file file)))
    (magit-diff-range commit nil (list file))))

(defun tlon-babel-log-buffer-latest-user-commit-ediff (&optional file)
  "Run `ediff' session for FILE and its state when last committed by current user.
If FILE is not provided, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-babel-latest-user-commit-in-file file))
	 (commit-file (tlon-babel-create-file-from-commit file commit)))
    (ediff-files commit-file file)))

;;;;;; Metadata

(defun tlon-babel-get-key-in-buffer ()
  "Get the BibTeX key in the current Markdown buffer."
  (tlon-babel-md-check-in-markdown-mode)
  (save-buffer)
  (let ((key (tlon-babel-metadata-get-field-value-in-file "original_key")))
    (unless key
      (user-error "No key found"))
    key))

;;;;; Clocked heading

(defun tlon-babel-get-clock ()
  "Return the currently clocked heading."
  (if org-clock-current-task
      (substring-no-properties org-clock-current-task)
    (user-error "No clock running")))

(defun tlon-babel-get-clock-key ()
  "Return bibtex key in clocked heading.
Assumes key is enclosed in backticks."
  ;; second capture group handles optional .md extension
  (if (string-match tlon-babel-key-regexp (tlon-babel-get-clock))
      (match-string 1 (tlon-babel-get-clock))
    (user-error "I wasn't able to find a file in clocked heading")))

(defun tlon-babel-get-clock-file ()
  "Return the file path of the clocked task."
  (let ((key (tlon-babel-get-clock-key)))
    (tlon-babel-get-file-from-key key)))

(defun tlon-babel-open-clock-file ()
  "Open file of clocked task."
  (interactive)
  (find-file (tlon-babel-get-clock-file)))

(defun tlon-babel-get-clock-issue ()
  "Get issue GID from `orgit-forge' link in heading at point."
  (unless org-clock-heading
    (user-error "No clock running"))
  (save-window-excursion
    (org-clock-goto)
    (org-narrow-to-subtree)
    (when (re-search-forward org-link-bracket-re)
      (let ((raw-link (org-link-unescape (match-string-no-properties 1))))
	(string-match "orgit-topic:\\(.+\\)" raw-link)
	(match-string 1 raw-link)))))

(defun tlon-babel-open-clock-issue ()
  "Open the issue from `orgit-forge' link in heading at point."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo))
	(issue (tlon-babel-get-clock-issue)))
    (forge-visit-issue issue)))

(defun tlon-babel-get-clock-action ()
  "Return action in clock.
Assumes action is first word of clocked task."
  ;; as rough validation, we check that the clocked heading contains a file
  (tlon-babel-get-clock-key)
  (let ((action (nth 1 (split-string (tlon-babel-get-clock))))
	(actions (tlon-babel-label-lookup-all :action)))
    (if (member action actions)
	action
      (user-error "I wasn't able to find a relevant action in clocked heading"))))

(defun tlon-babel-get-clock-label ()
  "Return label associated with action in heading at point."
  (let ((label (tlon-babel-label-lookup :label :action (tlon-babel-get-clock-action))))
    label))

(defun tlon-babel-get-clock-next-label ()
  "Return label associated with the action after the one in heading at point."
  (tlon-babel-next-value :label (tlon-babel-get-clock-label) tlon-babel-job-labels))

(defun tlon-babel-next-value (property value alist)
  "Return the \"next\" value of PROPERTY with VALUE in ALIST."
  (catch 'found
    (let ((found nil))
      (dolist (item alist)
	(when (and found (not (equal (plist-get item property) value)))
	  (throw 'found (plist-get item property)))
	(when (equal (plist-get item property) value)
	  (setq found t))))
    nil))

(defun tlon-babel-copy-buffer (&optional file deepl)
  "Copy the contents of FILE to the kill ring.
Defaults to the current buffer if no FILE is specified. If DEEPL is non-nil,
open DeepL."
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
	 (metadata (tlon-babel-metadata-in-repos :subtype 'translations))
	 (translation (tlon-babel-metadata-lookup metadata "file" "original_key" key))
	 (original (tlon-babel-get-counterpart translation)))
    (cl-values original translation key)))

(defun tlon-babel-set-windows (original-path translation-path)
  "Open ORIGINAL-PATH and TRANSLATION-PATH in windows 1 and 2."
  (window-extras-split-if-unsplit)
  (winum-select-window-1)
  (find-file original-path)
  (winum-select-window-2)
  (find-file translation-path))

;;;;; Magit/Forge

(defun tlon-babel-magit-status ()
  "Show the status of the current repository in a buffer."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo nil 'include-all)))
    (magit-status-setup-buffer)))

(defun tlon-babel-magit-prompt (repo)
  "Prompt the user for a REPO and show it in Magit."
  (interactive (list
		(completing-read
		 "Repo: " (tlon-babel-repo-lookup-all :name))))
  (if-let ((default-directory (tlon-babel-repo-lookup :dir :name repo)))
      (magit-status-setup-buffer)
    (user-error "Repo `%s' not found" repo)))

(defun tlon-babel-forge-update-repo (repo)
  "Update issues and notifications for REPO name."
  (let* ((default-directory (tlon-babel-repo-lookup :dir :name repo))
	 (repo (forge-get-repository 'full)))
    (save-window-excursion
      (with-current-buffer (dired-noselect default-directory)
	(forge-pull repo)))))

(defun tlon-babel-forge-update-all-repos ()
  "Update issues and notifications for all active repos."
  (interactive)
  (dolist (repo (tlon-babel-repo-lookup-all :name))
    (tlon-babel-forge-update-repo repo)))

(defun tlon-babel-get-commit-key ()
  "Get key of commit file."
  (let ((path (file-name-concat (file-name-directory (tlon-babel-core-buffer-file-name))
				(magit-extras-get-commit-file))))
    (tlon-babel-get-key-from-file path)))

(defun tlon-babel-create-translation-file (&optional repo)
  "Create a new translation file and set its front matter.
If REPO is nil, prompt the user for one."
  (interactive)
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (title (read-string "Translated title: "))
	 (dir (file-name-concat repo "translations/articulos/"))
	 (path (tlon-babel-set-file-from-title title dir)))
    (find-file path)
    (tlon-babel-yaml-set-metadata tlon-babel-yaml-article-keys title)
    (save-buffer)))

;; Currently unused; might come handy in the future, else delete
(defun tlon-babel-get-list-of-remote-repos ()
  "Return a lit of all repositories in the `tlon-babel' GitHub account."
  (let ((list (ghub-get "/users/tlon-team/repos")))
    (mapcar (lambda (repo) (alist-get 'name repo)) list)))

;;;;; Checks

(defun tlon-babel-check-branch (branch repo)
  "Throw an error unless current buffer is in REPO branch BRANCH."
  (let ((default-directory repo))
    (unless (string= (magit-get-current-branch) branch)
      (user-error "Please switch to the branch `%s' before proceeding" branch)
      t)))

(defun tlon-babel-check-file (&optional original)
  "Throw an error unless current file matches file in clock.
If ORIGINAL is non-nil, check that current file matches original; otherwise,
check that current file matches translation."
  (let* ((key (tlon-babel-get-clock-key))
	 (field (if original "original_path" "file"))
	 (expected-file (file-name-nondirectory
			 (tlon-babel-metadata-lookup (tlon-babel-metadata-in-repo) field "original_key" key)))
	 (actual-file (file-name-nondirectory
		       (buffer-file-name))))
    (if (string= expected-file actual-file)
	t
      (user-error "Current file does not match file in clock"))))

(defun tlon-babel-check-staged-or-unstaged (file)
  "Check if there are staged or unstaged modifications in repo involving FILE."
  (catch 'found
    (dolist (flag '("staged" ""))
      (let ((git-command (format "git diff --%s --name-only %s" flag file)))
	(when (not (string-empty-p (shell-command-to-string git-command)))
	  (throw 'found t))))))

(defun tlon-babel-check-staged-or-unstaged-other-than (file)
  "Check if there are staged or unstaged modifications in repo not involving FILE."
  (let* ((default-directory (tlon-babel-get-repo-from-file file))
	 (all-changes (magit-git-str "diff" "HEAD" "--" "."))
	 (filtered-changes (magit-git-str "diff" "HEAD" "--" file)))
    (unless (string= all-changes filtered-changes)
      (user-error "There are staged or unstaged changes in repo. Please commit or stash them before continuing"))))

(defun tlon-babel-check-file-title-match  (&optional file)
  "Check that FILE matches its title.
If FILE is nil, check the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (base (file-name-base file))
	 (title (tlon-babel-metadata-get-field-value-in-file "title" file))
	 (slugified-title (tlon-core-slugify title)))
    (unless (or
	     (string= base slugified-title)
	     ;; for articles with duplicate titles
	     (string-match-p (concat "^" (regexp-quote slugified-title) "-[0-9]+$") base))
      (error "The file `%s' does not match its title" title))))

;;;;; Search

(defun tlon-babel-search-issues (search-string &optional repo)
  "Search for SEARCH-STRING in GitHub REPO's issues and pull requests.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let* ((repo (or repo (tlon-babel-get-repo nil 'include-all)))
	 (default-directory repo))
    (forge-search search-string)))

(defun tlon-babel-search-commits (search-string &optional repo)
  "Search for SEARCH-STRING in REPO's commit history.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let ((default-directory (or repo default-directory)))
    (magit-log-all (list "--grep" search-string))))

(defun tlon-babel-search-commit-diffs (search-string &optional repo)
  "Search for SEARCH-STRING in REPO's commit diff history.
If REPO is nil, use the current repo."
  (interactive "sSearch commit diffs : ")
  (let ((default-directory (or repo default-directory)))
    (magit-log-all `("-S" ,search-string))))

(defun tlon-babel-search-files (search-string &optional repo)
  "Search for SEARCH-STRING in REPO files.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let* ((repo (or repo (tlon-babel-get-repo nil 'include-all))))
    (consult-ripgrep repo search-string)))

(defun tlon-babel-search-multi (search-string &optional repo)
  "Search for SEARCH-STRING in REPO files, commit history, and GitHub issues.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let ((repo (or repo (tlon-babel-get-repo nil 'include-all)))
	(win1 (selected-window)))
    (window-extras-split-if-unsplit)
    (tlon-babel-search-issues search-string repo)
    (split-window-below)
    (tlon-babel-search-commits search-string repo)
    (select-window win1)
    (tlon-babel-search-files search-string repo)))

(defun tlon-babel-commit-and-push (action file)
  "Commit and push modifications to FILE.
The commit message is ACTION followed by the name of FILE."
  (let* ((default-directory (tlon-babel-get-repo-from-file file)))
    (tlon-babel-check-staged-or-unstaged-other-than file)
    (when (string= (magit-get-current-branch) "main")
      ;; TODO: Replace interactive call with programmatic way of doing this
      (call-interactively #'magit-pull-from-upstream nil)
      (sleep-for 2))
    (magit-stage-file file)
    (tlon-babel-create-commit action file)
    (call-interactively #'magit-push-current-to-pushremote)))

(defun tlon-babel-create-commit (action file)
  "Create a commit modifications to FILE.
The commit message is ACTION followed by either FILE or its BibTeX key,
depending on whether the repo is of subtype `translations' or `biblio',
respectively."
  ;; we check for staged or unstaged changes to FILE because
  ;; `magit-commit-create' interrupts the process if there aren't any
  (when (tlon-babel-check-staged-or-unstaged file)
    (let* ((repo (tlon-babel-get-repo-from-file file))
	   (subtype (tlon-babel-repo-lookup :subtype :dir repo))
	   (file-or-key (pcase subtype
			  ('translations (tlon-babel-get-key-from-file file))
			  ('biblio (file-name-nondirectory file)))))
      (magit-commit-create (list "-m" (format "%s %s" action file-or-key))))))

;;;;; json

(defun tlon-babel-parse-json (file &optional object-type array-type)
  "Parse JSON FILE using array TYPE.
If OBJECT-TYPE is nil, default to `alist'. If ARRAY-TYPE is nil, default to
`vector'."
  (let ((json-object-type object-type)
	(json-array-type array-type)
	(json-key-type 'string)
	(json-false :json-false))
    (json-read-file file)))

(defun tlon-babel-get-keys (data)
  "Get keys from hash table DATA."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) data)
    keys))

;;;;; URL correspondences

(defun tlon-babel-url-correspondence-dwim ()
  "Add a new URL correspondence or modify an existing one."
  (interactive)
  (let* ((data (tlon-babel-parse-json tlon-babel-file-url-correspondences 'hash-table 'vector))
	 (keys (tlon-babel-get-keys data))
	 (selected-key (completing-read "Select existing URL or enter a new one: " keys))
	 (default-value (gethash selected-key data))
	 (new-value (read-string (format "Enter value for key '%s': " selected-key) default-value)))
    (puthash selected-key new-value data)
    (with-temp-file tlon-babel-file-url-correspondences
      (insert "{\n")
      (maphash (lambda (k v)
		 (insert (format "  \"%s\": \"%s\",\n" k v)))
	       data)
      ;; Remove last comma
      (goto-char (- (point) 2))
      (delete-char 1)
      (insert "\n}")
      (write-file tlon-babel-file-url-correspondences)
      (tlon-babel-url-correspondence-commit))))

;; TODO: consider adapting `tlon-babel-commit-and-push' instead
(defun tlon-babel-url-correspondence-commit ()
  "Commit modifications in `url-correspondences.json'."
  (let ((default-directory (tlon-babel-repo-lookup :dir :name "babel-es")))
    ;; save all unsaved files in repo
    (magit-save-repository-buffers)
    (call-interactively #'magit-pull-from-upstream nil)
    ;; if there are staged files, we do not commit or push the changes
    (unless (magit-staged-files)
      (tlon-babel-check-branch "main" default-directory)
      (magit-run-git "add" tlon-babel-file-url-correspondences)
      (let ((magit-commit-ask-to-stage nil))
	(magit-commit-create (list "-m" "Update URL correspondences"))))))

(defun tlon-babel-highlight-url-correspondences ()
  "Highlight source URLs in URL correspondences file."
  (interactive)
  ;; Load JSON file
  (let* ((json-data (tlon-babel-parse-json tlon-babel-file-url-correspondences 'hash-table 'vector))
	 (key-urls (tlon-babel-get-keys json-data))
	 ;; Remove URL prefixes from keys
	 (search-keywords (mapcar (lambda (url)
				    (replace-regexp-in-string "^https?://\\(www\\.\\)?" "" url))
				  key-urls))
	 ;; Build list of keys
	 (keywords-regex (regexp-opt search-keywords 'words))
	 ;; Specify a custom face for highlighting
	 (highlight-face '(:background "#D3FFD2")))

    ;; Remove the previous highlighting
    (with-silent-modifications
      (remove-list-of-text-properties (point-min) (point-max) '(font-lock-face))

      ;; Highlight each occurrence of a key from the JSON file
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward keywords-regex nil t)
	  (add-text-properties (match-beginning 0) (match-end 0)
			       `(font-lock-face ,highlight-face)))))))

;;;;; section correspondences

(defun tlon-babel-section-correspondence-dwim ()
  "Add a new section correspondence or modify an existing one."
  (interactive)
  (let* ((data (tlon-babel-parse-json tlon-babel-file-section-correspondences 'hash-table 'list))
	 (selected-key (citar-select-refs)))
    (tlon-babel-section-correspondence-check selected-key)
    (let ((default-value (gethash selected-key data))
	  (new-sectionOriginal (read-string (format "Enter value for key '%s', sectionOriginal: " selected-key)))
	  (new-sectionSpanish (read-string (format "Enter value for key '%s', sectionSpanish: " selected-key)))
	  (new-value (make-hash-table)))
      (puthash "sectionOriginal" new-sectionOriginal new-value)
      (puthash "sectionSpanish" new-sectionSpanish new-value)
      (puthash selected-key
	       (if default-value
		   (append default-value (list new-value))
		 (list new-value))
	       data)
      (with-temp-file tlon-babel-file-section-correspondences
	(insert "{\n")
	(maphash (lambda (k v)
		   (insert (format "  \"%s\": [\n" k))
		   (dolist (item v)
		     (insert (format "    %s,\n" (json-encode item))))
		   (delete-char -2)
		   (insert "\n  ],\n"))
		 data)
	;; Remove last comma
	(goto-char (- (point) 2))
	(delete-char 1)
	(insert "\n}")
	(json-pretty-print (point-min) (point-max))
	(write-file tlon-babel-file-section-correspondences)
	;; (tlon-babel-url-correspondence-commit)
	))))

(defun tlon-babel-section-correspondence-check (key)
  "Check that selected BibTeX KEY is associated with the original work."
  (save-window-excursion
    (ebib-extras-open-key key)
    (let ((langid (ebib-extras-get-field-value "langid")))
      (unless (member langid '("english" "american"))
	(unless (y-or-n-p "The BibTeX entry you selected is not in English. In the `section-correspondences.json' file, you should use the BibTeX entry associated with the original work rather than with its translation. Are you sure you want to proceed?")
	  (user-error "Aborted"))))))

;;;;; hyphenation

;; TODO: the function that adds words to the json file should also add the
;; hyphenated variant to `jinx-local-words'

;;;;; browse commands

(defun tlon-babel-browse-file ()
  "Browse the current file in the tlon-babel repository."
  (interactive)
  (if-let (file (buffer-file-name))
      (if-let ((repo (tlon-babel-get-repo-from-file file)))
	  (let* ((repo-name (tlon-babel-repo-lookup :name :dir repo))
		 (repo-url (concat "https://github.com/tlon-team/" repo-name))
		 (file-url (concat "/blob/main/" (file-relative-name (buffer-file-name) repo))))
	    (browse-url (concat repo-url file-url)))
	(user-error "File is not in a recognized repository"))
    (user-error "Buffer is not visiting a file")))

(defun tlon-babel-browse-repo ()
  "Browse a Babel repository.
If the current buffer is visiting a file in a Babel repository, browse that;
otherwise prompt for a repo."
  (interactive)
  (let* ((repo-name (tlon-babel-repo-lookup :name :dir (tlon-babel-get-repo))))
    (browse-url (concat tlon-babel-tlon-github-url repo-name))))

(defun tlon-babel-browse-entity-dir (entity &optional repo source-lang)
  "Browse the directory of ENTITY in REPO.
ENTITY should be passed as a string, in SOURCE-LANG, defaulting to English. If
REPO is nil, default to the current repository."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (source-lang (or source-lang "en"))
	 (target-lang (tlon-babel-repo-lookup :language :dir repo))
	 (dir (tlon-babel-get-bare-dir-translation target-lang source-lang entity))
	 (path (file-name-concat repo dir)))
    (dired path)))

(defun tlon-babel-open-file-in-repo (&optional repo)
  "Interactively open a file from a list of all files in REPO.
If REPO is nil, default to the current repository."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (alist (tlon-babel-files-and-display-names-alist (list repo) repo)))
    (tlon-babel-open-file-in-alist alist)))

(defun tlon-babel-open-file-in-all-repos ()
  "Interactively open a file from a list of all files in all repos."
  (interactive)
  (let* ((alist (tlon-babel-files-and-display-names-alist
		 (tlon-babel-repo-lookup-all :dir)
		 tlon-core-repo-dirs)))
    (tlon-babel-open-file-in-alist alist)))

(defun tlon-babel-files-and-display-names-alist (dirs relative-path)
  "Return a alist of files, and display names, in DIRS.
The display names are constructed by subtracting the RELATIVE-PATH."
  (mapcar (lambda (file)
	    (cons (file-relative-name file relative-path) file))
	  (apply 'append (mapcar
			  (lambda (dir)
			    (directory-files-recursively dir "^[^.][^/]*$"))
			  dirs))))

(defun tlon-babel-open-file-in-alist (alist)
  "Interactively open a file from ALIST.
The car in each cons cell is the file to open, and its cdr is the candidate
presented to the user."
  (let* ((selection (completing-read "Select file: " alist))
	 (file (cdr (assoc selection alist))))
    (find-file file)))

;;;;;;; Fix log errors helper functions

(defun tlon-babel-collect-bibtex-keys-in-buffer ()
  "Collect all the bibtex keys in the current buffer.
Display the collected keys in a new buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (keys)
      (while (re-search-forward tlon-babel-cite-pattern nil t)
	(push (match-string 1) keys))
      (with-output-to-temp-buffer "*Bibtex keys*"
	(princ (mapconcat #'identity (delete-dups keys) "\n"))))))

(defun tlon-babel-move-bibtex-entries-to-babel ()
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
	    (with-current-buffer (find-file-noselect paths-file-personal-bibliography-old)
	      (goto-char (point-min))
	      (if (re-search-forward (format "{%s," bibtex-key) nil t)
		  (call-interactively 'bibtex-extras-move-entry-to-tlon)
		(push 'missing-keys bibtex-key))))
	  (forward-line)))
      (with-output-to-temp-buffer "*Missing BibTeX Keys*"
	(dolist (key missing-keys)
	  (princ (format "%s\n" key)))))))

;; TODO: Move to appropriate section
(defun tlon-babel-create-file-from-commit (file commit-hash)
  "Create a temporary file with the state of the FILE at the COMMIT-HASH.
Return the path of the temporary file created."
  (let* ((file-name (file-name-nondirectory file))
	 (repo-root (locate-dominating-file file ".git"))
	 (relative-file (file-relative-name file repo-root))
	 (new-file-name (format "%s_%s" commit-hash file-name))
	 (new-file (make-temp-file new-file-name nil ".md"))
	 (git-command
	  (format "git show %s:\"%s\""
		  commit-hash
		  (shell-quote-argument relative-file))))
    (let ((default-directory repo-root))
      (with-temp-buffer
	(insert (shell-command-to-string git-command))
	(write-file new-file)))
    (message "File created: %s" new-file)
    new-file))

;; TODO: move to relevant section
(defun tlon-babel-search-for-translation (string)
  "Search for a Spanish translation of English STRING."
  (interactive "sString to translate: ")
  (let ((urls '("https://spanish.stackexchange.com/search?q=%s"
		"https://es.bab.la/diccionario/ingles-espanol/%s"
		"https://en.wikipedia.org/w/index.php?fulltext=Search&profile=default&search=%s"
		"https://context.reverso.net/traduccion/ingles-espanol/%s"
		"https://www.linguee.com/english-spanish/search?query=%s")))
    (dolist (url urls)
      (browse-url (format url (url-hexify-string string)) 'new-buffer)))
  (goldendict-ng-search-string string))

;;;;; Misc

(defun tlon-babel-historic-word-count (&optional repo-name days chars-per-word)
  "Compute the historic word count of REPO-NAME for past DAYS.
CHARS-PER-WORD is the average number of characters per word. The word count is
computed by dividing the file size by CHARS-PER-WORD."
  (interactive)
  (unless (executable-find "gdu")
    (user-error "Please install `gdu' (e.g. `brew install gdu')"))
  (unless (executable-find "gnuplot")
    (user-error "Please install `gnuplot' (e.g. `brew install gnuplot')"))
  (let* ((repo-name (or repo-name
			(completing-read "Repo: "
					 (tlon-babel-repo-lookup-all :name :subtype 'translations))))
	 (dir (tlon-babel-repo-lookup :dir :name repo-name))
	 (days (or days (read-number "How many days into the past? ")))
	 (chars-per-word (or chars-per-word 5.5))
	 (buffer (get-buffer-create "*Directory Size*"))
	 (script (file-name-concat (tlon-babel-repo-lookup :dir :name "babel")
				   "count/historic-word-count")))
    (shell-command (format "sh %s %s %s %s" script dir days chars-per-word) buffer)))

(provide 'tlon-babel)
;;; tlon-babel.el ends here
