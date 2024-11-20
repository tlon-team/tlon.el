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
(require 'forge-search)
(require 'tlon-core)
(require 'vc)
(require 'vc-extras)

;;;; Variables

(defgroup tlon-repos ()
  "`tlon-repos' functionality."
  :group 'tlon)

;;;; Functions

;;;;; vc

;;;###autoload
(defun tlon-create-repo (&optional name)
  "Create a new Tlön repo.
NAME and DESCRIPTION are the name and description of the repo. If PRIVATE is
non-nil, make it private."
  (interactive)
  (vc-extras-create-repo name "tlon-team"))

(declare-function forge-extras-track-repository "forge-extras")
;;;###autoload
(defun tlon-clone-repo (&optional name no-forge)
  "Clone an existing Tlön repo.
The repo will be cloned in the directory specified by `paths-dir-tlon-repos'. If
NAME is nil, prompt the user for a repo name. If NO-FORGE is non-nil, do not
prompt the user to add the repo to the Forge database."
  (interactive)
  (vc-extras-clone-repo name "tlon-team" no-forge))

;;;###autoload
(defun tlon-clone-missing-repos ()
  "Clone missing Tlön repos.
Note that this function will not prompt the user to add the repos to the Forge
database. To track these repos, use `tlon-forge-track-missing-repos'."
  (interactive)
  (let ((repos (tlon-repo-lookup-all :dir))
	(count 0))
    (dolist (repo repos)
      (unless (file-exists-p repo)
	(vc-extras-clone-repo (tlon-repo-lookup :name :dir repo) "tlon-team" 'no-forge)
	(setq count (1+ count))))
    (if (zerop count)
	(message "No repos missing")
      (message "Cloning %d missing repos" count))))

;;;###autoload
(defun tlon-delete-local-repo (name)
  "Delete the Tlön repo NAME."
  (interactive (list (completing-read "Repo: " (tlon-repo-lookup-all :name))))
  (vc-extras-delete-local-repo name "tlon-team"))

;;;;; Forge
;;;;;; Track repos

(declare-function forge-extras-track-repo-all-topics "forge-extras")
(defun tlon-forge-track-missing-repos ()
  "Add missing Tlön repos to the Forge database.
Note that this function will omit Tlön repos that do not exist locally. To add
those repos, use `tlon-clone-missing-repos'."
  (interactive)
  (dolist (repo (tlon-repo-lookup-all :dir))
    (when (file-exists-p repo)
      (let ((default-directory repo))
	(unless (forge-get-repository :tracked?)
	  (require 'forge-extras)
	  (forge-extras-track-repo-all-topics repo)
	  (while (not (forge-get-repository :tracked?))
	    (message "Adding repo %s..." (tlon-repo-lookup :name :dir repo))
	    (sleep-for 1))))))
  (message "Added all missing repos to the Forge database."))

;;;;;; Pull issues

(declare-function shut-up "shut-up")
(defun tlon-pull-issues-in-repo (&optional dir)
  "Pull repository in DIR.
If DIR is nil, use the current directory."
  (interactive)
  (when-let ((default-directory (or dir default-directory))
	     (name (tlon-repo-lookup :name :dir default-directory)))
    (message "Pulling issues in %s..." name)
    (shut-up (forge-pull))))

;;;###autoload
(defun tlon-pull-issues-in-all-repos ()
  "Pull issues in all Tlön repositories."
  (interactive)
  (let ((repos (tlon-repo-lookup-all :dir)))
    (dolist (repo repos)
      (tlon-pull-issues-in-repo repo))))

;;;; Search

;; 2024-11-05: this is a very inefficient and inelegant approach, but my
;; attempts to query the db directly all failed. try again with the best LLM in
;; a few months
;;;###autoload
(defun tlon-forge-search (string &optional repos)
  "Search for STRING in Tlön REPOS.
If REPOS is nil, search in all tracked repos."
  (interactive "sSearch string: ")
  (save-selected-window
    ;; First kill any existing search results buffers
    (dolist (buffer (buffer-list))
      (when (string-match "\\/Forge Search Results\\*" (buffer-name buffer))
        (kill-buffer buffer)))
    (let ((repos (or repos (tlon-repo-lookup-all :dir)))
          (results 0)
          (start-time (current-time)))
      (message "Running search... (it may take a few seconds)")
      (shut-up
        (dolist (repo repos)
          (let ((default-directory repo)
                (magit-buffer-existed (get-buffer (format "magit: %s"
                                                          (file-name-nondirectory (directory-file-name repo))))))
            (magit-status-setup-buffer repo)
            (when (forge-get-repository ':tracked?)
              (forge-search string))
            (unless magit-buffer-existed
              (kill-buffer (get-buffer (format "magit: %s"
                                               (file-name-nondirectory (directory-file-name repo)))))))))
      ;; Count result buffers at the end
      (setq results (length (seq-filter (lambda (buf)
                                          (string-match "\\*Forge Search Results\\*" (buffer-name buf)))
					(buffer-list))))
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (message (if (zerop results)
                     (format "No matches found for string \"%s\" (%.2f seconds)." string elapsed)
                   (format "Found results in %d repos (%.2f seconds)." results elapsed)))))))

;;;;;; Search menu

(transient-define-prefix tlon-forge-menu ()
  "Dispatch a forge command."
  [:if forge--get-repository:tracked?
       ["Create"
	("c i" "issue"             forge-create-issue)
	("c p" "pull-request"      forge-create-pullreq)
	("c u" "pull-request from issue"
	 forge-create-pullreq-from-issue
	 :if forge--get-github-repository)
	("c f" "fork or remote"    forge-fork)]
       ["Search"
	("s s" "this repo"      forge-search)
	("s a" "all repos"      tlon-forge-search)]]
  [:if forge--get-repository:tracked?
       ["List"
	("t" "topics...         "  forge-topics-menu        :transient replace)
	("n" "notifications...  "  forge-notifications-menu :transient replace)
	("r" "repositories...   "  forge-repositories-menu  :transient replace)]
       ["Fetch"
	("f f" "all topics       " forge-pull)
	("f t" "one topic        " forge-pull-topic)
	("f n" "notifications    " forge-pull-notifications)]
       ["Do"
	:if forge--get-repository:tracked?
	("C" "configure"       forge-configure)
	("M" "merge w/api"     forge-merge :level 7)]]
  [:if forge--get-repository:tracked?
       ["Visit"
	("v t" "topic"         forge-visit-topic)
	("v i" "issue"         forge-visit-issue)
	("v p" "pull-request"  forge-visit-pullreq)]
       ["Browse"
	("b t" "topic"         forge-browse-topic)
	("b i" "issue"         forge-browse-issue)
	("b p" "pull-request"  forge-browse-pullreq)]
       ["Browse"
	("b r" "remote"        forge-browse-remote)
	("b I" "issues"        forge-browse-issues)
	("b P" "pull-requests" forge-browse-pullreqs)]]
  [[:description (lambda ()
		   (if (magit-gitdir)
		       "Forge doesn't know about this Git repository yet"
		     "Not inside a Git repository"))
		 :if-not forge--get-repository:tracked?
		 ("a" "add repository to database" forge-add-repository)
		 ("f" "fetch notifications"        forge-pull-notifications)
		 ("l" "list notifications"         forge-list-notifications)]])

(advice-add 'forge-dispatch :override #'tlon-forge-menu)

;;;;; git-crypt

(declare-function pass-extras-git-crypt-unlock "pass-extras")
(defun tlon-unlock-uqbar-git-crypt ()
  "Unlock `uqbar' git-crypt repos."
  (interactive)
  (require 'pass-extras)
  (let ((uqbar-dir (tlon-repo-lookup :dir :name "uqbar")))
    (dolist (repo-name '("uqbar-api" "uqbar-front"))
      (let ((repo-dir (file-name-concat uqbar-dir repo-name))
	    (entry (concat "tlon/core/git-crypt/" repo-name)))
	(pass-extras-git-crypt-unlock repo-dir entry)))))

;;;;; Menu

;;;###autoload (autoload 'tlon-repos-menu "tlon-repos" nil t)
(transient-define-prefix tlon-repos-menu ()
  "Repos menu."
  [["Magit"
    ""
    ("l" "Clone remote repo"        tlon-clone-repo)
    ("L" "Clone all missing repos"  tlon-clone-missing-repos)
    ""
    ("c" "Create remote repo"       tlon-create-repo)
    ""
    ("s" "Split local repo"         vc-extras-split-local-repo)
    ""
    ("d" "Delete local repo"        tlon-delete-local-repo)
    ""
    ("u" "Unlock uqbar git-crypt"   tlon-unlock-uqbar-git-crypt)]
   ["Forge"
    ""
    ("a" "Track repo"               forge-extras-track-repository)
    ("A" "Track all missing repos"  tlon-forge-track-missing-repos)
    ""
    ("r" "Untrack repo"             forge-remove-repository)
    ("R" "Reset database"           forge-reset-database)
    ""
    ("t" "List tracked repos"       forge-list-repositories)
    ""
    ("p" "Pull issues in repo"      tlon-pull-issues-in-repo)
    ("P" "Pull issues in all repos" tlon-pull-issues-in-all-repos)]])

(provide 'tlon-repos)

;;; tlon-repos.el ends here
