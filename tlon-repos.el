;;; tlon-repos.el --- Functionality for manipulating repositories -*- lexical-binding: t -*-

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

;; Functionality for manipulating repositories

;;; Code:

(require 'forge-core)
(require 'forge-commands)
(require 'vc)
(require 'vc-extras)

;;;; Variables

(defgroup tlon-repos ()
  "`tlon-repos' functionality."
  :group 'tlon)

(defcustom tlon-split-repo 'prompt
  "Whether to split the `.git' directory in a separate directory.
If nil, never split the `.git' directory. If `prompt', ask the user whether to
split the `.git' directory. If t or any other non-nil value, always split the
`.git' directory."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Prompt" prompt)
		 (boolean :tag "Always"))
  :group 'tlon-repos)

;;;; Functions

;;;###autoload
(defun tlon-create-repo (name description private)
  "Create a new Tlön repo.
NAME and DESCRIPTION are the name and description of the repo. If PRIVATE is
non-nil, make it private."
  (interactive (list (read-string "Name: ")
		     (read-string "Description: ")
		     (y-or-n-p "Private? ")))
  (vc-extras-gh-create-repo name description vc-extras-github-account-work private)
  (if (y-or-n-p "Clone? ")
      (tlon-clone-repo name)
    (message "Cloned repo `%s'" name)))

;;;###autoload
(defun tlon-clone-repo (&optional name)
  "Clone an existing Tlön repo and add it to the Forge database.
The repo will be cloned in the directory specified by `paths-dir-tlon-repos'. If
NAME is nil, prompt the user for a repo name."
  (interactive)
  (let* ((name (or name (completing-read "Repo: " (vc-extras-gh-list-repos vc-extras-github-account-work))))
	 (remote (vc-extras-get-github-remote vc-extras-github-account-work name))
	 (dir (file-name-as-directory (file-name-concat paths-dir-tlon-repos name))))
    (when (file-exists-p dir)
      (user-error "Directory `%s' already exists" dir))
    (vc-clone remote 'Git dir)
    (pcase tlon-split-repo
      ('nil)
      ('prompt (if (y-or-n-p "Move `.git' directory to separate directory?")
		   (tlon-split-repo dir)
		 (message "You can customize the `tlon-split-repo' user option to avoid this prompt.")))
      (_ (tlon-split-repo dir)))
    (if (y-or-n-p "Add to Forge? ")
	(tlon-forge-add-repository dir)
      (dired dir))))

;;;###autoload
(defun tlon-split-repo (dir)
  "Move the `.git' in DIR to a separate dir and set the `.git' file accordingly.
Normally, this command is run for repos managed by Dropbox, to protect the Git
files from possible corruption."
  (interactive "D")
  (let* ((name (file-name-nondirectory (directory-file-name dir)))
	 (source (file-name-as-directory (file-name-concat paths-dir-tlon-repos name ".git")))
	 (target (file-name-concat paths-dir-split-git name))
	 (git-file (file-name-concat paths-dir-tlon-repos name ".git")))
    (when (file-exists-p target)
      (user-error "Directory `%s' already exists" target))
    (rename-file source paths-dir-split-git t)
    (rename-file (file-name-concat paths-dir-split-git ".git") target t)
    (with-temp-file git-file
      (insert (format "gitdir: %s" target))
      (write-file git-file))))

(declare-function magit-status "magit-status")
(defun tlon-forge-add-repository (dir)
  "Add DIR to the Forge database."
  (let* ((default-directory dir)
	 (url (and-let*
		  ((repo (forge-get-repository :stub))
		   (remote (oref repo remote)))
		(magit-git-string "remote" "get-url" remote))))
    (forge-add-repository url)
    (magit-status-setup-buffer dir)))

;;;;; Pull issues

(defun tlon-pull-issues-in-repo (&optional dir)
  "Pull repository in DIR.
If DIR is nil, use the current directory."
  (interactive)
  (when-let ((default-directory (or dir default-directory))
	     (repo (forge-get-repository :tracked?))
	     (name (tlon-repo-lookup :name :dir default-directory)))
    (message "Pulling issues in %s..." name)
    (shut-up
      (forge--pull repo))))

(defun tlon-pull-issues-in-all-repos ()
  "Pull issues in all Tlön repositories."
  (interactive)
  (let ((repos (tlon-repo-lookup-all :dir)))
    (dolist (repo repos)
      (tlon-pull-issues-in-repo repo))))

;;;; Menu

;;;###autoload (autoload 'tlon-repos-menu "tlon-repos" nil t)
(transient-define-prefix tlon-repos-menu ()
  "Repos menu."
  [["Repo"
    ""
    ("c" "Create remote"     tlon-create-repo)
    ("l" "Clone remote"      tlon-clone-repo)
    ""
    ("s" "Split local"       tlon-split-repo)]
   ["Forge"
    ""
    "Issues"
    ("p" "Pull in repo"      tlon-pull-issues-in-repo)
    ("P" "Pull in all repos" tlon-pull-issues-in-all-repos)
    ""
    "Repos"
    ("a" "Add"               forge-add-repository)
    ("r" "Remove"            forge-remove-repository)]])

(provide 'tlon-repos)
;;; tlon-repos.el ends here

