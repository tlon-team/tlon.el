;;; tlon-repos.el --- Functionality for manipulating repositories -*- lexical-binding: t -*-

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

;; Functionality for manipulating repositories

;;; Code:

(require 'forge-core)
(require 'forge-search)
(require 'shut-up)
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

(defun tlon-get-local-repos ()
  "Prompt the user to select from a list of repo names and return the selection."
  (let* ((all-names (tlon-repo-lookup-all :name))
         (local-candidates (vc-extras-list-local-candidates "tlon-team"))
         (local-names (mapcar #'car local-candidates))
         (intersection (cl-intersection all-names local-names :test #'string=)))
    (unless intersection
      (user-error "No local Tlön repos found"))
    (completing-read "Repo: " intersection nil t)))

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
      (message "Cloning %d missing repos asynchronously..." count))))

(autoload 'vc-extras-list-local-candidates "vc-extras")
;;;###autoload
(defun tlon-delete-local-repo (&optional name)
  "Delete the Tlön repo NAME if it exists locally.
If NAME is nil, the user is prompted with the intersection of repositories
reported by `tlon-repo-lookup-all' and the local repositories (as determined by
`vc-extras--list-local-candidates'). Delegates deletion to
`vc-extras-delete-local-repo'."
  (interactive)
  (let ((name (or name (tlon-get-local-repos))))
    (vc-extras-delete-local-repo name "tlon-team")))

;;;;;; Pull changes

(declare-function magit-extra-async-pull "magit-extra")
(defun tlon-repos-pull-babel-refs ()
  "Asynchronously pull from `babel-refs'."
  (magit-extra-async-pull (tlon-repo-lookup :dir :name "babel-refs")))

(run-with-idle-timer (* 5 60) t #'tlon-repos-pull-babel-refs)

;;;;; Forge
;;;;;; Track repos

;;;###autoload
(defun tlon-forge-track-repo (&optional name)
  "Track Tlön repo NAME in the Forge database."
  (interactive)
  (let* ((name (or name (tlon-get-local-repos)))
	 (dir (tlon-repo-lookup :dir :name name)))
    (forge-extras-track-repository dir)))

(autoload 'forge-extras-track-repo-all-topics "forge-extras")
(defun tlon-forge-track-missing-repos ()
  "Add missing Tlön repos to the Forge database.
Note that this function will omit Tlön repos that do not exist locally. To add
those repos, use `tlon-clone-missing-repos'."
  (interactive)
  (dolist (repo (tlon-repo-lookup-all :dir))
    (when (file-exists-p repo)
      (let ((default-directory repo))
	(unless (forge-get-repository :tracked?)
	  (forge-extras-track-repo-all-topics repo)
	  (while (not (forge-get-repository :tracked?))
	    (message "Adding repo %s..." (tlon-repo-lookup :name :dir repo))
	    (sleep-for 1))))))
  (message "Added all missing repos to the Forge database."))

;;;;;; Pull GitHub issues

(defun tlon-pull-issues-in-repo (&optional dir)
  "Pull issues in repository located at DIR.
If DIR is nil, use the current directory."
  (interactive)
  (when-let ((default-directory (or dir default-directory))
	     (name (tlon-repo-lookup :name :dir default-directory))
	     (repo (forge-get-repository :tracked?)))
    (message "Pulling issues in %s..." name)
    (shut-up (forge--pull repo))))

;;;###autoload
(defun tlon-pull-issues-in-all-repos ()
  "Pull issues in all Tlön repositories."
  (interactive)
  (let ((repos (tlon-repo-lookup-all :dir)))
    (dolist (repo repos)
      (tlon-pull-issues-in-repo repo))))

;;;;;; Search

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
  [:if (##forge-get-repository :tracked?)
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
  [:if (##forge-get-repository :tracked?)
       ["List"
	("t" "topics...         "  forge-topics-menu        :transient replace)
	("n" "notifications...  "  forge-notifications-menu :transient replace)
	("r" "repositories...   "  forge-repositories-menu  :transient replace)]
       ["Fetch"
	("f f" "all topics       " forge-pull)
	("f t" "one topic        " forge-pull-topic)
	("f n" "notifications    " forge-pull-notifications)]
       ["Do"
	:if (##forge-get-repository :tracked?)
	("C" "configure"       forge-configure)
	("M" "merge w/api"     forge-merge :level 7)]]
  [:if (##forge-get-repository :tracked?)
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
		 :if-not (##forge-get-repository :tracked?)
		 ("a" "add repository to database" forge-add-repository)
		 ("f" "fetch notifications"        forge-pull-notifications)
		 ("l" "list notifications"         forge-list-notifications)]])

(advice-add 'forge-dispatch :override #'tlon-forge-menu)

;;;;; git-crypt

(autoload 'password-store-list "password-store")
(defun tlon--get-git-crypt-repos ()
  "Return a list of Tlön repo names that use git-crypt.
This list is derived from entries stored under \"tlon/core/git-crypt/\" in the
password store."
  (let* ((base-path "tlon/core/git-crypt/")
         (base-path-len (length base-path))
         (entries (password-store-list base-path)))
    (mapcar (lambda (entry)
              (substring entry base-path-len))
            entries)))

(autoload 'pass-extras-git-crypt-unlock "pass-extras")
(defun tlon-git-crypt-unlock ()
  "Unlock a Tlön repository using `git-crypt'.
Prompts the user to select a repository from the list derived from entries under
\"tlon/core/git-crypt/\" in the password store."
  (interactive)
  (if-let* ((repo-names (tlon--get-git-crypt-repos))
            (repo-name (completing-read "Repo: " repo-names nil t))
            (repo-dir (file-name-concat paths-dir-tlon-repos repo-name))
            (entry (concat "tlon/core/git-crypt/" repo-name)))
      (pass-extras-git-crypt-unlock repo-dir entry)
    (user-error "Repo for the selected git-crypt entry not found")))

;;;;; issues-to-repos

;;;###autoload
(defun tlon-repos-configure-issues-to-projects (&optional repo-name)
  "Configure issues-to-projects for REPO-NAME.
This command performs the equivalent of the manual steps described in
the README of the issues-to-projects repository:

1. Write REPO-NAME into config.sh at line 24.
2. Run ./automate-github-secrets.sh to set the GitHub secret.
3. Run ./batch-deploy-add-new-issues-workflow.sh to deploy the
   workflow.

When called interactively, prompt for a local Tlön repository name."
  (interactive)
  (let* ((name (or repo-name (tlon-get-local-repos)))
         (dir (tlon-repo-lookup :dir :name "issues-to-projects"))
         (config (file-name-concat dir "config.sh")))
    (unless (file-directory-p dir)
      (user-error "Repo `issues-to-projects' not found: %s" dir))
    (unless (file-exists-p config)
      (user-error "File `config.sh' not found in %s" dir))
    (let ((backup (make-temp-file "issues-to-projects-config-")))
      (copy-file config backup t t t)
      (tlon--repos--write-line-to-file config 24 (format "\"%s\"" name))
      (tlon--repos--run-issues-to-projects-scripts name config backup))))

(defun tlon--repos--write-line-to-file (file line-number text)
  "Replace content of LINE-NUMBER in FILE with TEXT and a newline."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (forward-line (1- line-number))
    (delete-region (line-beginning-position) (line-end-position))
    (insert text)
    (insert "\n")
    (write-region (point-min) (point-max) file nil 'silent)))

(defun tlon--repos--run-issues-to-projects-scripts (repo-name config-file backup-file)
  "Run the issues-to-projects setup scripts for REPO-NAME sequentially.
Restore CONFIG-FILE from BACKUP-FILE when finished."
  (let* ((dir     (tlon-repo-lookup :dir :name "issues-to-projects"))
         (secrets (expand-file-name "automate-github-secrets.sh" dir))
         (deploy  (expand-file-name "batch-deploy-add-new-issues-workflow.sh" dir))
         (buf     (get-buffer-create "*issues-to-projects*")))
    (with-current-buffer buf (erase-buffer))
    (unless (file-exists-p secrets)
      (user-error "File not found: %s" secrets))
    (unless (file-exists-p deploy)
      (user-error "File not found: %s" deploy))
    (message "Configuring GitHub secrets for %s..." repo-name)
    (let ((proc1 (start-process "itp-secrets" buf "bash" secrets)))
      (set-process-sentinel
       proc1
       (lambda (p _e)
         (if (and (eq (process-status p) 'exit)
                  (= (process-exit-status p) 0))
             (let ((proc2 (start-process "itp-deploy" buf "bash" deploy)))
               (message "Secrets configured for %s. Deploying workflow..." repo-name)
               (set-process-sentinel
                proc2
                (lambda (p2 _e2)
                  (copy-file backup-file config-file t t t)
                  (ignore-errors (delete-file backup-file))
                  (if (and (eq (process-status p2) 'exit)
                           (= (process-exit-status p2) 0))
                      (message "issues-to-projects configured for %s" repo-name)
                    (message "Failed to deploy workflow for %s (see %s)" repo-name (buffer-name buf))
                    (display-buffer buf)))))
           (copy-file backup-file config-file t t t)
           (ignore-errors (delete-file backup-file))
           (message "Failed to configure secrets for %s (see %s)" repo-name (buffer-name buf))
           (display-buffer buf)))))))

;;;;; Menu

;;;###autoload (autoload 'tlon-repos-menu "tlon-repos" nil t)
(transient-define-prefix tlon-repos-menu ()
  "Repos menu."
  [["Git"
    ""
    ("l" "Clone repo"                    tlon-clone-repo)
    ("L" "Clone all missing repos"       tlon-clone-missing-repos)
    ""
    ("s" "Split local repo"              vc-extras-split-local-repo)
    ("d" "Delete local repo"             tlon-delete-local-repo)
    ""
    ("c" "Create remote repo"            tlon-create-repo)
    ""
    ("u" "Unlock uqbar git-crypt"        tlon-git-crypt-unlock)
    ""
    ("?" "Check authentication"          vc-extras-check-gh-authenticated)
    ""
    ("I" "Configure issues-to-projects"  tlon-repos-configure-issues-to-projects)]
   ["Forge"
    ""
    ("a" "Track repo"                    tlon-forge-track-repo)
    ("A" "Track all missing repos"       tlon-forge-track-missing-repos)
    ""
    ("r" "Untrack repo"                  forge-remove-repository)
    ("R" "Untrack all repos (db reset)"  forge-reset-database)
    ""
    ("t" "List tracked repos"            forge-list-repositories)
    ""
    ("p" "Pull issues in repo"           tlon-pull-issues-in-repo)
    ("P" "Pull issues in all repos"      tlon-pull-issues-in-all-repos)
    ("u" "Unlock uqbar git-crypt"        tlon-git-crypt-unlock)]])

(provide 'tlon-repos)
;;; tlon-repos.el ends here
