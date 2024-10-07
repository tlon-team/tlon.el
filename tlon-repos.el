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
(require 'forge-search)
(require 'tlon-core)
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
		 (boolean :tag "Always" t))
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

(declare-function shut-up "shut-up")
(defun tlon-pull-issues-in-repo (&optional dir)
  "Pull repository in DIR.
If DIR is nil, use the current directory."
  (interactive)
  (when-let ((default-directory (or dir default-directory))
	     (repo (forge-get-repository :tracked?))
	     (name (tlon-repo-lookup :name :dir default-directory)))
    (message "Pulling issues in %s..." name)
    (shut-up (forge--pull repo))))

;;;###autoload
(defun tlon-pull-issues-in-all-repos ()
  "Pull issues in all Tlön repositories."
  (interactive)
  (let ((repos (tlon-repo-lookup-all :dir)))
    (dolist (repo repos)
      (tlon-pull-issues-in-repo repo))))

;;;;; Search

(defun tlon-forge-search-titles (string)
  "Search for STRING in the title of the current repo."
  (interactive "sSearch string: ")
  (tlon-forge-search string (list default-directory) nil))

(defun tlon-forge-search-titles-in-all-repos (string)
  "Search for STRING in the title of issues in all Tlön repos."
  (interactive "sSearch string: ")
  (tlon-forge-search string))

(defun tlon-forge-search-in-all-repos (string)
  "Search for STRING in the title and body of issues in all Tlön repos."
  (interactive "sSearch string: ")
  (tlon-forge-search string nil 'full))

(defun tlon-forge-search (string &optional repos full)
  "Perform a search for STRING in the title of issues and pull requests of REPOS.
If REPOS is nil, use a list of full the Tlön repos. By default, search in titles
only. If FULL is non-nil, search also in the body of issues and pull requests."
  (let* ((repos (or repos (tlon-repo-lookup-all :dir)))
	 (matches))
    (dolist (repo repos)
      (when-let* ((default-directory repo)
		  (db (forge-db))
		  (repo (forge-get-repository ':tracked?))
		  (repoid (slot-value repo 'id)))
	(emacsql-with-transaction db
	  (emacsql db [:create-virtual-table :if :not :exists search
					     :using :fts5
					     ([id haystack author date type title body])])
	  (emacsql db
		   (concat "insert into search "
			   "select id, "
			   (if full
			       "('\"' || replace(title, '\"', ' ') || ' ' || replace(body, '\"', ' ') || '\"') as haystack, "
			     "('\"' || replace(title, '\"', ' ') || '\"') as haystack, ")
			   "author, " forge-search-date-type " as date, "
			   "'\"issue\"' as type, title, body "
			   "from issue where repository = '\"" repoid "\"';"))
	  (when full
	    (emacsql db
		     (concat "insert into search "
			     "select issue_post.issue, "
			     "('\"' || replace(issue_post.body, '\"', ' ') || '\"') as haystack, "
			     "issue_post.author, "
			     "issue_post." forge-search-date-type " as date, "
			     "'\"issue_msg\"' as type, '\"\"' as title, issue_post.body "
			     "from issue_post "
			     "inner join issue on issue.id = issue_post.issue "
			     "where issue.repository = '\"" repoid "\"';")))
	  (emacsql db
		   (concat "insert into search "
			   "select id, "
			   (if full
			       "('\"' || replace(title, '\"', ' ') || ' ' || replace(body, '\"', ' ') || '\"') as haystack, "
			     "('\"' || replace(title, '\"', ' ') || '\"') as haystack, ")
			   "author, " forge-search-date-type " as date, "
			   "'\"pr\"' as type, title, body "
			   "from pullreq where repository = '\"" repoid "\"';"))
	  (when full
	    (concat "insert into search "
		    "select pullreq_post.pullreq, "
		    "('\"' || replace(pullreq_post.body, '\"', ' ') || '\"') as haystack, "
		    "pullreq_post.author, "
		    "pullreq_post." forge-search-date-type " as date, "
		    "'\"pr_msg\"' as type, '\"\"' as title, pullreq_post.body "
		    "from pullreq_post "
		    "inner join pullreq on pullreq.id = pullreq_post.pullreq "
		    "where repository = '\"" repoid "\"';"))
	  (let* ((magit-generate-buffer-name-function (lambda (_mode _value)
							(format "*Forge Search Results: %s*" (slot-value repo 'name)))))
	    (setq matches (emacsql db [:select [id author date type title body]
					       :from search
					       :where haystack :match $r1]
				   string))
	    (when matches
	      (magit-setup-buffer #'forge-search-mode t
		(matches matches)
		(search-string string))))
	  (emacsql db [:drop-table search]))))))

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
      "full"
      ("s s" "this repo"      forge-search)
      ("s a" "all repos"      tlon-forge-search-in-all-repos)
      "titles only"
      ("s S" "this repo"      tlon-forge-search-titles)
      ("s A" "all repos"      tlon-forge-search-titles-in-all-repos)]]
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

