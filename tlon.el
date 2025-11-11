;;; tlon.el --- A companion package for Tlön. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 1.6.7
;; URL: https://github.com/tlon-team/tlon
;; Keywords: convenience tools
;; Package-Requires: ((bib "0.1")
;;                   (bibtex-extras "0.1")
;;                   (citar "1.4.0")
;;                   (citar-extras "0.1")
;;                   (ebib "2.36")
;;                   (el-patch "0.1")
;;                   (elpaca "0.0.1")
;;                   (files-extras "0.1")
;;                   (forge "0.3.0")
;;                   (forge-search "0")
;;                   (forge-extras "0.1")
;;                   (gptel "0.1")
;;                   (gptel-extras "0.1")
;;                   (markdown-mode-extras "0.1")
;;                   (oauth2-auto "0.1")
;;                   (paths "0.1")
;;                   (pass-extras "0.1")
;;                   (read-aloud "0.0.2")
;;                   (request "0.3.3")
;;                   (reverso "0.1")
;;                   (shut-up "0.1")
;;                   (simple-extras "0.1")
;;                   (vc-extras "0.1"))
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

(require 'tlon-core)

;;;; User options

(defgroup tlon ()
  "A companion package for Tlön."
  :group 'files)

(defcustom tlon-github-dashboard-url
  "https://github.com/orgs/tlon-team/projects/9/views/15"
  "URL of the Tlön GitHub dashboard."
  :type 'string
  :group 'tlon)

;;;; Variables

(defconst tlon-version "1.6.7"
  "Version of the `tlon' package.")

;;;;; Files and dirs

(defvar tlon-todos-jobs-file nil
  "Org file that contains the ID in `paths-tlon-todos-jobs-id'.
This variable should not be set manually.")

(defvar tlon-todos-generic-file nil
  "Org file that contains the ID in `paths-tlon-todos-generic-id'.
This variable should not be set manually.")

(defvar tlon-post-init-hook nil
  "Hook run at the end of `tlon-initialize'.")

(defconst tlon-file-abstract-translations
  (file-name-concat (tlon-repo-lookup :dir :name "babel-refs")
		    "bib" "abstract-translations.json")
  "The JSON file containing the abstract translations.")

(defconst tlon-file-bare-bibliography
  (file-name-concat (tlon-repo-lookup :dir :name "babel-refs")
		    "bib" "bare-bibliography.json")
  "The JSON file containing the bare bibliography (author, date, title, key).")

;;;;;; lookup

;; TODO: all this should be adapted following the model of
;; `tlon-get-file-glossary' i.e. a repo-relative path stored in a variable
;; and a function to get it for specific languages

(defvar tlon-dir-dict
  (file-name-concat (tlon-repo-lookup :dir :name "es") "dict/")
  "Directory where dictionary files are stored.")

;; TODO: currently unused; consider implementing
(defvar tlon-file-hyphenation
  "hyphenation.json"
  "File containing hyphenation rules.")

(defvar tlon-file-babel-manual
  (file-name-concat (tlon-repo-lookup :dir :name "babel-core") "manual.org")
  "File containing the Babel manual.")

(defvar tlon-file-bibtex-correspondences
  (file-name-concat (tlon-repo-lookup :dir :name "babel-refs") "bibtex-correspondences.json")
  "File containing the BibTeX correspondences.")

;; TODO: not currently implemented
(defvar tlon-file-section-correspondences
  (file-name-concat (tlon-repo-lookup :dir :name "babel-refs") "section-correspondences.json")
  "File containing the section correspondences.")

(defvar tlon-file-bare-bibliography
  (file-name-concat (tlon-repo-lookup :dir :name "babel-refs") "bib" "bare-bibliography.json")
  "File containing the bare bibliography.")

(defmacro tlon-create-file-opening-command (file)
  "Create a command to open FILE."
  (let* ((file-base (downcase (file-name-base file)))
	 (file-name (file-name-nondirectory file))
	 (command-name (intern (concat "tlon-open-" file-base))))
    `(defun ,command-name ()
       ,(format "Open `%s' file." file-name)
       (interactive)
       (find-file (symbol-value (intern (concat "tlon-file-" ,file)))))))

(tlon-create-file-opening-command "manual")
(tlon-create-file-opening-command "readme")
(tlon-create-file-opening-command "jobs")
(tlon-create-file-opening-command "fluid")
(tlon-create-file-opening-command "stable")
(tlon-create-file-opening-command "url-correspondences")
(tlon-create-file-opening-command "section-correspondences")
(tlon-create-file-opening-command "bibtex-correspondences")

;;;;; URLs

(defconst tlon-github-url
  "https://github.com/tlon-team/"
  "URL of the Tlön account on GitHub.")

;;;; Functions

;;;;; init

(defun tlon-initialize ()
  "Initialize `tlon'."
  (interactive)
  (dolist (var '(paths-tlon-todos-jobs-id paths-tlon-todos-generic-id))
    (tlon-warn-if-unbound var))
  (setq paths-files-bibliography-all
	`(,@paths-files-bibliography-personal
	  ,@tlon-bibliography-files))
  (run-hooks 'tlon-post-init-hook)
  (message "Initialized `tlon'."))

(defvar org-capture-templates)
(with-eval-after-load 'org-capture
  (dolist (template `(("tbJ" "Tlön: Babel: Create a new Babel job" entry
		       (id ,paths-tlon-todos-jobs-id)
		       "** %c" :immediate-finish t :empty-lines 1 :jump-to-captured t)
		      ("tbG" "Tlön: Babel: Create new generic todo from GitHub" entry
		       (id ,paths-tlon-todos-generic-id)
		       "** %c" :immediate-finish t :empty-lines 1 :prepend t :jump-to-captured t)))
    (push template org-capture-templates)))

(defun tlon-warn-if-unbound (var)
  "Signal an error if the value of VAR is not set."
  (unless (symbol-value var)
    (user-error "Please set the value of `%s'" (symbol-name var))))

(autoload 'magit-git-string "magit-git")
(defun tlon-get-latest-commit (dir)
  "Return the latest commit in DIR."
  (let ((default-directory dir))
    (magit-git-string "rev-parse" "--short" "HEAD")))

(declare-function elpaca-extras-update-and-reload "elpaca-extras")
;;;###autoload
(defun tlon-update-package-and-reload ()
  "Update and reload the `tlon' package."
  (interactive)
  (elpaca-extras-update-and-reload 'tlon))

;;;###autoload
(defun tlon-copy-package-info ()
  "Copy information about the `tlon' package to the kill ring."
  (interactive)
  (let ((info (format "Package version: %s\nLatest commit: %s"
		      tlon-version (tlon-get-latest-commit tlon-package-dir))))
    (kill-new info)
    (message info)))

;;;;; [name]

(declare-function tlon-metadata-in-repos "tlon-yaml")
(defun tlon-get-file-from-key (key)
  "Return the file path of KEY."
  (if-let ((file (tlon-metadata-lookup
		  (tlon-metadata-in-repos :subtype 'translations) "file" "key" key)))
      file
    (user-error "Metadata lookup for key `%s' returned nil" key)))

(declare-function tlon-metadata-in-repo "tlon-yaml")
(declare-function tlon-get-counterpart "tlon-counterpart")
(declare-function tlon-yaml-get-key "tlon-yaml")
(defun tlon-get-key-from-file (file)
  "Return the bibtex key of FILE."
  (tlon-yaml-get-key "key" file))

(defun tlon-set-translation-language (language)
  "Set the translation LANGUAGE."
  (interactive (list (tlon-select-language 'code 'babel)))
  (setq tlon-translation-language language))

;;;;; User commits

(defun tlon-latest-user-commit-in-file (&optional file)
  "Return latest commit by the current user in FILE.
If no FILE is provided, use the file visited by the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (default-directory (file-name-directory file))
	 (user (tlon-user-lookup :git :name user-full-name))
	 ;; get most recent commit in FILE by USER
	 (output (shell-command-to-string (format "git log --pretty=format:'%%h %%an %%s' --follow -- '%s' | grep -m 1 '%s' | awk '{print $1}'" file user)))
	 (commit (car (split-string output "\n"))))
    commit))

(declare-function magit-diff-range "magit-diff")
(defun tlon-log-buffer-latest-user-commit (&optional file)
  "Show modifications to FILE since the latest commit by the current user.
If no FILE is provided, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-latest-user-commit-in-file file)))
    (magit-diff-range commit nil (list file))))

(defun tlon-log-buffer-latest-user-commit-ediff (&optional file)
  "Run `ediff' session for FILE and its state when last committed by current user.
If FILE is not provided, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-latest-user-commit-in-file file))
	 (commit-file (tlon-create-file-from-commit file commit)))
    (ediff-files commit-file file)))

;;;;;; Metadata

(declare-function tlon-ensure-markdown-mode "tlon-md")
(defun tlon-get-key-in-buffer ()
  "Get the BibTeX key in the current Markdown buffer."
  (tlon-ensure-markdown-mode)
  (save-buffer)
  (let ((key (tlon-yaml-get-key "key")))
    (unless key
      (user-error "No key found"))
    key))

;;;;; Magit/Forge

(declare-function magit-status-setup-buffer "magit-status")
(defun tlon-magit-status ()
  "Show the status of the current repository in a buffer."
  (interactive)
  (let ((default-directory (tlon-get-repo nil 'include-all)))
    (magit-status-setup-buffer)))

(defun tlon-magit-prompt (repo)
  "Prompt the user for a REPO and show it in Magit."
  (interactive (list
		(completing-read
		 "Repo: " (tlon-repo-lookup-all :name))))
  (if-let ((default-directory (tlon-repo-lookup :dir :name repo)))
      (magit-status-setup-buffer)
    (user-error "Repo `%s' not found" repo)))

(declare-function files-extras-buffer-file-name "file-extras")
(autoload 'magit-extras-get-commit-file "magit-extra")
(defun tlon-get-commit-key ()
  "Get key of commit file."
  (let* ((dir (file-name-directory (files-extras-buffer-file-name)))
	 (file (magit-extras-get-commit-file))
	 (path (file-name-concat (replace-regexp-in-string "\\.git/" "" dir) file)))
    (tlon-get-key-from-file path)))

;;;;; Search

(declare-function magit-stage-file "magit")
(declare-function magit-pull-from-upstream "magit-pull")
(declare-function magit-push-current-to-pushremote "magit-push")
(defun tlon-commit-and-push (action file)
  "Commit and push modifications to FILE.
The commit message is ACTION followed by the name of FILE."
  (let* ((default-directory (tlon-get-repo-from-file file)))
    (tlon-ensure-no-uncommitted-changes file)
    (when (string= (magit-get-current-branch) "main")
      ;; TODO: Replace interactive call with programmatic way of doing this
      (call-interactively #'magit-pull-from-upstream nil)
      (sleep-for 2))
    (magit-stage-file file)
    (tlon-create-commit action file)
    (call-interactively #'magit-push-current-to-pushremote)))

(declare-function magit-commit-create "magit-commit")
(defun tlon-create-commit (action file)
  "Create a commit modifications to FILE.
The commit message is ACTION followed by either FILE or its BibTeX key,
depending on whether the repo is of subtype `translations' or `biblio',
respectively."
  ;; we check for staged or unstaged changes to FILE because
  ;; `magit-commit-create' interrupts the process if there aren't any
  (when (tlon-check-staged-or-unstaged file)
    (let* ((repo (tlon-get-repo-from-file file))
	   (subtype (tlon-repo-lookup :subtype :dir repo))
	   (file-or-key (pcase subtype
			  ('translations (tlon-get-key-from-file file))
			  ('biblio (file-name-nondirectory file)))))
      (magit-commit-create (list "-m" (format "%s %s" action file-or-key))))))

;;;;; hyphenation

;; TODO: the function that adds words to the json file should also add the
;; hyphenated variant to `jinx-local-words'

;;;;; open file commands

(declare-function files-extras-read-file "files-extras")
(defun tlon-browse-file (&optional file)
  "Browse Tlön FILE externally."
  (interactive)
  (let* ((file (files-extras-read-file file))
	 (url (tlon-path-to-url file)))
    (browse-url url)))

(defun tlon-find-file-in-repo (&optional repo)
  "Interactively open a file from a list of all files in REPO.
If REPO is nil, default to the current repository."
  (interactive)
  (let* ((repo (or repo (tlon-get-repo 'error)))
	 (alist (tlon-files-and-display-names-alist (list repo) repo)))
    (tlon-open-file-in-alist alist)))

(autoload 'ffap-url-p "ffap")
(autoload 'eww-current-url "eww")
(defun tlon-find-file-of-url (&optional url)
  "Open the file corresponding to URL."
  (interactive)
  (let ((url (or url (read-string "URL: " (or (thing-at-point 'url)
					      (eww-current-url)
					      (ffap-url-p (current-kill 0)))))))
    (find-file (tlon-url-to-path url))))

(defun tlon-open-file-across-repos ()
  "Interactively open a file from a list of all files in all repos."
  (interactive)
  (let* ((alist (tlon-files-and-display-names-alist
		 (tlon-repo-lookup-all :dir)
		 paths-dir-tlon-repos)))
    (tlon-open-file-in-alist alist)))

(defun tlon-files-and-display-names-alist (dirs relative-path)
  "Return a alist of files, and display names, in DIRS.
The display names are constructed by subtracting the RELATIVE-PATH."
  (mapcar (lambda (file)
	    (cons (file-relative-name file relative-path) file))
	  (apply 'append (mapcar
			  (lambda (dir)
			    (directory-files-recursively dir "^[^.][^/]*$"))
			  dirs))))

(defun tlon-open-file-in-alist (alist)
  "Interactively open a file from ALIST.
The car in each cons cell is the file to open, and its cdr is the candidate
presented to the user."
  (let* ((selection (completing-read "Select file: " alist))
	 (file (cdr (assoc selection alist))))
    (find-file file)))

;;;;;; path <> url conversion

(defun tlon-path-to-url (path)
  "Convert a Tlön file PATH to its URL."
  (let* ((dir (tlon-get-repo))
	 (relative (file-relative-name path dir))
	 (sans-extension (file-name-sans-extension relative))
	 (root (tlon-repo-lookup :url :dir dir)))
    (concat root sans-extension)))

(defun tlon-url-to-path (url)
  "Convert a Tlön URL to its file path."
  (let* ((root (tlon-get-url-root url))
	 (rest (substring url (length root)))
	 (dir (tlon-repo-lookup :dir :url root))
	 ;; TODO: support non-markdown files (e.g. images)
	 (with-extension (file-name-with-extension rest "md")))
    (file-name-concat dir with-extension)))

(defun tlon-get-url-root (url)
  "Extract the root of the URL."
  (let* ((parsed-url (url-generic-parse-url url))
	 (scheme (url-type parsed-url))
	 (host (url-host parsed-url)))
    (concat scheme "://" host "/")))

;;;;; Fix log errors helper functions

(defvar tlon-mdx-cite-pattern)
(defun tlon-collect-bibtex-keys-in-buffer ()
  "Collect all the bibtex keys in the current buffer.
Display the collected keys in a new buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (keys)
      (while (re-search-forward tlon-mdx-cite-pattern nil t)
	(push (match-string 1) keys))
      (with-output-to-temp-buffer "*Bibtex keys*"
	(princ (mapconcat #'identity (delete-dups keys) "\n"))))))

(defun tlon-move-bibtex-entries-to-babel ()
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
(defun tlon-create-file-from-commit (file commit-hash)
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

;;;;; Package files

(defun tlon-open-package-files ()
  "Prompt the user to select a `tlon' feature and open its file."
  (interactive)
  (let* ((files (directory-files tlon-package-dir t "\\.el"))
	 (choices (mapcar #'file-name-base files))
	 (selection (completing-read "Feature: " choices nil t))
	 (file (file-name-concat tlon-package-dir (file-name-with-extension selection "el"))))
    (find-file file)))

;;;;; Tlön dashboard

;;;###autoload
(defun tlon-browse-dashboard ()
  "Browse the GitHub Tlön dashboard."
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (browse-url tlon-github-dashboard-url)))

(provide 'tlon)
;;; tlon.el ends here
