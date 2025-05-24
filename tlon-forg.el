;;; tlon-forg.el --- Integration between forge and org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini

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

;; Integration between `forge' and `org-mode'.

;;; Code:

(require 'forge)
(require 'org)
(require 'org-refile)
(require 'org-element-ast)
(require 'shut-up)
(require 'tlon-core)
(require 'tlon-dispatch)
(require 'forge-extras)

;; Forward declarations
(declare-function tlon-get-repo "tlon-core" (&optional no-prompt include-all))
(declare-function tlon-repo-lookup "tlon-core" (key &rest key-value))
(declare-function forge-extras-gh-get-issue-fields "forge-extras" (issue-number repo-name))
(declare-function forge-extras-gh-parse-issue-fields "forge-extras" (raw-list))
(declare-function forge-extras-gh-add-issue-to-project "forge-extras" (project-node-id issue-node-id))
(declare-function forge-extras-gh-update-project-item-status-field "forge-extras" (project-node-id item-node-id field-node-id status-option-id))

;;;; Path Helpers

(defun tlon-forg--get-repo-specific-todo-file (issue)
  "Return the full path to the repo-specific Org TODO file for ISSUE.
The file is named REPO-NAME.org inside `paths-dir-tlon-todos'.
Returns nil if `paths-dir-tlon-todos' is not set, not a directory,
or if ISSUE has no repository or repository name."
  (when-let* ((repo (and issue (ignore-errors (forge-get-repository issue)))) ; Ensure issue is valid for repo
              (repo-name (and repo (oref repo name)))
              (todos-dir (and (boundp 'paths-dir-tlon-todos)
                              (stringp paths-dir-tlon-todos)
                              (file-directory-p paths-dir-tlon-todos)
                              paths-dir-tlon-todos)))
    (expand-file-name (format "%s.org" repo-name) todos-dir)))

;;;; Helper Functions for Repository and Issue Selection

(defun tlon-forg-get-or-select-repository ()
  "Return a `forge-repository` object.
Tries to get the current repository using `forge-get-repository :tracked`.
If that fails, uses `tlon-get-repo` to prompt for a known Tlön repository.
If that also fails, lists repositories from `tlon-forg-project-owner` on
GitHub using `gh` CLI and prompts for selection. The selected repository
must be locally configured in Tlön for its path to be found."
  (or (ignore-errors (forge-get-repository :tracked))
      (let ((repo-path (tlon-get-repo nil 'include-all))) ; Prompt from known Tlön repos
        (if repo-path
            (let ((default-directory repo-path)) ; Set context for forge
              (forge-get-repository :tracked))
          ;; Fallback to gh list if tlon-get-repo doesn't yield/user cancels
          (message "No repository selected from Tlön configuration. Fetching list from GitHub...")
          (let* ((gh-executable (executable-find "gh"))
                 (repo-names
                  (if gh-executable
                      (split-string
                       (shell-command-to-string
                        (format "gh repo list %s --json name --jq \".[] | .name\""
                                forge-extras-project-owner))
                       "\n" t 'omit-empty) ; omit-empty in case of trailing newline
                    (user-error "The 'gh' command-line tool is required but not found")))
                 (selected-repo-name (completing-read "Select repository from GitHub: " repo-names nil t)))
            (when selected-repo-name
              (let ((repo-dir (tlon-repo-lookup :dir :name selected-repo-name)))
                (unless repo-dir
                  (user-error "Repository '%s' not found in local Tlön configuration. Please ensure it's cloned and configured" selected-repo-name))
                (unless (file-directory-p repo-dir)
                  (user-error "Repository directory '%s' for '%s' does not exist" repo-dir selected-repo-name))
                (let ((default-directory repo-dir))
                  (forge-get-repository :tracked)))))))))

(defun tlon-forg-select-issue-from-repo (repo)
  "Prompt the user to select an open issue from REPO.
REPO must be a valid `forge-repository` object.
Ensures topics for REPO are fetched before prompting.
Returns a `forge-issue` object or nil if no issue is selected or on error."
  (when repo
    (let ((default-directory (oref repo worktree))) ; Ensure correct context
      (message "Fetching issues for %s..." (oref repo name))
      (condition-case err
          (progn
            ;; Use forge--pull to synchronously update the repository data, including issues.
            ;; Passing nil as the callback makes it synchronous.
            (shut-up (forge--pull repo nil)) ; Pull all data for the repo silently and synchronously.
            (message "Fetching issues for %s... done. Please select an issue." (oref repo name))
            ;; Call the modified tlon-get-issue-number-from-open-issues
            (let ((selected-issue-number (tlon-get-issue-number-from-open-issues repo)))
              (if selected-issue-number
                  (let* ((issue-id (caar (forge-sql [:select [id] :from issue
                                                     :where (and (= repository $s1)
                                                                 (= number $s2))]
                                                (oref repo id)
                                                selected-issue-number))))
                    (if issue-id
                        (forge-get-topic issue-id)
                      (progn
                        (message "Error: Could not retrieve details for issue #%s in %s." selected-issue-number (oref repo name))
                        nil)))
                nil))) ; No issue number selected, tlon-get-issue-number-from-open-issues returned nil
        ((debug error) ; Catch user-error and other errors, provide debug info
         (message "Error during issue selection/fetching for %s: %s" (oref repo name) err)
         (message "To debug, try M-x toggle-debug-on-error and run the command again.")
         nil)))))

;;;; User options

(defgroup tlon-forg ()
  "Integration between `forge' and `org-mode'."
  :group 'tlon)

(defcustom tlon-when-assignee-is-nil 'prompt
  "What to do when the issue has no assignee.
- `prompt': prompt the user to confirm that it will be assigned to them.
- `change': assign it to the user without prompting for confirmation.
- `warn': capture the issue as is, logging a warning in the `*Messages*' buffer.
- `capture': capture the issue as is, without logging a warning.
- `no-capture', or any other value: do not capture the issue.

The value of this user option can also be set interactively from
`tlon-forg-menu'. When set that way, the value will only persist for the
current session."
  :type '(choice (const :tag "Prompt for confirmation" prompt)
		 (const :tag "Change without prompt" change)
		 (const :tag "Capture with warning" warn)
		 (const :tag "Capture without warning" capture)
		 (const :tag "Do not capture" t))
  :group 'tlon-forg)

(defcustom tlon-when-assignee-is-someone-else 'prompt
  "What to do when the issue’s assignee is someone else.
- `prompt': prompt the user to confirm that it will be assigned to them.
- `change': assign it to the user without prompting for confirmation.
- `warn': capture the issue as is, logging a warning in the `*Messages*' buffer.
- `capture': capture the issue as is, without logging a warning.
- `no-capture', or any other value: do not capture the issue.

The value of this user option can also be set interactively from
`tlon-forg-menu'. When set that way, the value will only persist for the
current session.

Note that this user option has no effect when the tasks are captured via
`tlon-capture-all-issues'. That command always behaves as if the value of
this variable is `no-capture'."
  :type '(choice (const :tag "Prompt for confirmation" prompt)
		 (const :tag "Change without prompt" change)
		 (const :tag "Capture with warning" warn)
		 (const :tag "Capture without warning" capture)
		 (const :tag "Do not capture" t))
  :group 'tlon-forg)

(defcustom tlon-forg-when-reconciling 'prompt
  "What to do when the issue and its associated todo differ.
- `prompt': prompt the user to choose between the issue and the todo.
- `issue': update the todo to match the issue.
- `todo': update the issue to match the todo.

The value of this user option can also be set interactively from
`tlon-forg-menu'. When set that way, the value will only persist for the
current session."
  :type '(choice (const :tag "Prompt for choice" prompt)
		 (const :tag "Update issue" issue)
		 (const :tag "Update todo" todo))
  :group 'tlon-forg)

(defcustom tlon-forg-include-archived nil
  "Whether to include archived issues in capture or reconcile processes."
  :type 'boolean
  :group 'tlon-forg)

(defcustom tlon-forg-archive-todo-on-close nil
  "Whether to archive the Org TODO item when closing an issue and its TODO.
When non-nil, `tlon-close-issue-and-todo' will archive the corresponding
Org TODO subtree after marking it as DONE."
  :type 'boolean
  :group 'tlon-forg)

(defcustom tlon-forg-archive-todo-on-close nil
  "Whether to archive the Org TODO item when closing an issue and its TODO.
When non-nil, `tlon-close-issue-and-todo' will archive the corresponding
Org TODO subtree after marking it as DONE."
  :type 'boolean
  :group 'tlon-forg)

(defcustom tlon-forg-enforce-user nil
  "If non-nil, the name to be considered that of the current user.
For testing purposes."
  :type 'string
  :group 'tlon-forg)

;;;; Variables

(defconst tlon-todo-statuses
  '(("Doing" . "DOING")
    ("Next" . "NEXT")
    ("Later" . "LATER")
    ("Someday" . "SOMEDAY")
    ("Done" . "DONE"))
  "Alist mapping GitHub Project item statuses (car) to Org TODO keywords (cdr).
The `cdr` values should be present in `org-todo-keywords'.")

(defconst tlon-todo-tags
  '("PendingReview" "Later")
  "List of admissible TODO tags.")

(defconst tlon-forg-sort-by-tags-regexp
  "[[:digit:]]\\{2\\}_[[:digit:]]\\{2\\}")

;;;; Functions

;;;;; Visit

;; FIXME: this returns nil when called in an issue with no args
(defun tlon-visit-issue (&optional number repo)
  "Visit Github issue.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (interactive)
  (forge-visit-issue (tlon-get-issue number repo)))

(defun tlon-get-issue (&optional number repo)
  "Get Github issue.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (when-let* ((number (or number
			  (tlon-get-issue-number-from-heading)))
	      (repo (or repo
			(tlon-get-repo-from-heading)))
	      (default-directory repo)
	      (forge-repo (forge-get-repository :tracked))
	      (issue-id (caar (forge-sql [:select [id] :from issue
						  :where (and (= repository $s1)
							      (= number $s2))]
					 (oref forge-repo id)
					 number))))
    (forge-get-topic issue-id)))

(defun tlon-get-issue-buffer (&optional number repo)
  "Get Github issue buffer.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (save-window-excursion
    (tlon-visit-issue number repo)
    (current-buffer)))

(defun tlon-visit-todo (&optional pos-file issue)
  "Visit TODO.
If POS-FILE (a cons cell `(POSITION . FILE-PATH)`) is provided, visit TODO at
POSITION in FILE-PATH.
If POS-FILE is nil, ISSUE (a `forge-issue` object) must be provided. The
function will then determine the position and file of the TODO associated with
ISSUE using `tlon-get-todo-position-from-issue`.
If ISSUE is also nil, it defaults to the issue at point."
  (let* ((resolved-pos-file (or pos-file (tlon-get-todo-position-from-issue issue)))
         (pos (car resolved-pos-file))
         (file (cdr resolved-pos-file)))
    (if (and pos file)
        (tlon-open-todo file pos)
      (user-error "No TODO found"))))

(defun tlon-open-todo (file position)
  "Open FILE at TODO POSITION."
  (find-file file)
  (widen)
  (org-kill-note-or-show-branches)
  (goto-char position))

(defun tlon-visit-todo-or-capture ()
  "Visit the TODO associated with the current issue, creating one if necessary."
  (if-let ((pos-file (tlon-get-todo-position-from-issue))) ; Returns (POS . FILE)
      (tlon-visit-todo pos-file)
    (tlon-capture-issue)))

;;;;; Capture

;;;###autoload
(defun tlon-capture-issue (&optional issue)
  "Create a new `org-mode' TODO based on ISSUE.
If ISSUE is nil, it attempts to use the issue at point or in the current
buffer. If no issue can be determined from the context (e.g., when called
from an Org mode buffer not in a repository context), it prompts to select a
repository and then an issue from that repository."
  (interactive)
  (let (repo issue-to-capture)
    (if issue
        (setq issue-to-capture issue
              repo (forge-get-repository issue))
      ;; No explicit issue passed, try to get from context or prompt
      (setq issue-to-capture (ignore-errors (forge-current-topic)))
      (if issue-to-capture
          (setq repo (forge-get-repository issue-to-capture))
        ;; Still no issue, so prompt for repository then issue
        (setq repo (tlon-forg-get-or-select-repository))
        (unless repo
          (user-error "Repository selection failed or cancelled. Aborting capture"))
        (setq issue-to-capture (tlon-forg-select-issue-from-repo repo))
        (unless issue-to-capture
          (user-error "Issue selection failed or cancelled. Aborting capture"))))

    ;; Ensure repo is set if issue-to-capture is set (should be by now)
    (unless repo
      (if issue-to-capture
          (setq repo (forge-get-repository issue-to-capture))
        ;; This case should ideally be caught by earlier user-errors
        (user-error "Cannot determine repository. Aborting capture")))

    ;; Proceed with capture if issue and repo are valid
    (let* ((issue-name (oref issue-to-capture title))
           (default-directory (oref repo worktree))) ; Set context for subsequent operations
      (when (and (eq (tlon-get-state issue-to-capture) 'open)
                 (tlon-capture-handle-assignee issue-to-capture))
        (message "Capturing ‘%s’" issue-name)
        (if (tlon-issue-is-job-p issue-to-capture)
            (tlon-create-job-todo-from-issue issue-to-capture)
          (tlon-store-todo "tbG" nil issue-to-capture))))))

;;;###autoload
(defun tlon-capture-all-issues (arg)
  "Capture all open issues in a selected repository.
The issues captured are those not assigned to another user. If a repository
cannot be inferred from the current context, the user is prompted to select one.
Before initiating the capture process, this command performs a full pull of the
selected repository to ensure local data reflects its remote state. The window
configuration active before the command was called will be restored after
completion. This command clears the `org-refile' cache upon completion.

If called with a prefix ARG, the initial pull from forge is omitted."
  (interactive "P")
  (let ((repo (tlon-forg-get-or-select-repository)))
    (unless repo
      (user-error "No repository selected or available. Aborting capture-all-issues"))
    ;; Set default-directory for the scope of tlon-pull-silently and its callback
    (let ((default-directory (oref repo worktree)))
      (if arg
          (tlon-capture-all-issues-after-pull repo)
        (tlon-pull-silently "Pulling issues..."
                            (lambda () (tlon-capture-all-issues-after-pull repo))
                            repo)))))

(defun tlon-pull-silently (&optional message callback repo)
  "Pull all issues from forge for REPO.
If REPO is nil, attempts to determine the current repository.
If MESSAGE is non-nil, display it while the process is ongoing.
If CALLBACK is non-nil, call it after the process completes, restoring the
window configuration that was active before the pull started."
  (when message (message message))
  (let ((original-window-config (current-window-configuration))
        (forge-repo (or repo (tlon-forg-get-or-select-repository))))
    (unless forge-repo
      (user-error "Cannot determine repository for pull operation"))
    (let ((default-directory (oref forge-repo worktree))) ; Set context for forge--pull
      (shut-up
	(forge--pull forge-repo
                     (lambda () ; New callback wrapper
                       (unwind-protect ; Ensure restoration even on error in callback
                           (when callback (funcall callback))
                         (set-window-configuration original-window-config))))))))

(defun tlon-capture-all-issues-after-pull (repo)
  "Capture all issues in REPO after `forge-pull' is finished.
REPO must be a valid `forge-repository` object."
  (let ((default-directory (oref repo worktree))) ; Set context
    (dolist (issue (tlon-get-issues repo)) ; tlon-get-issues uses the repo context
      (unless (tlon-get-todo-position-from-issue issue)
        (tlon-capture-issue issue))) ; Pass the specific issue object
    (org-refile-cache-clear)
    (message "Finished capturing issues. Refile cache cleared.")))

(defun tlon-store-todo (template &optional no-action issue)
  "Store a new TODO using TEMPLATE.
If TODO already exists, do nothing. If NO-ACTION is non-nil, store a master
TODO. If ISSUE is non-nil, use it instead of the issue at point."
  (let ((issue (or issue (forge-current-topic))))
    (shut-up
      (unless (tlon-get-todo-position-from-issue issue)
	(let ((todo (tlon-make-todo-name-from-issue issue no-action nil)))
	  (kill-new todo)
	  (org-capture nil template))))))

;;;;;; Handling assignee, status, phase

(defun tlon-capture-handle-assignee (issue)
  "Take appropriate action when the user is not the assignee of ISSUE.
The appropriate action is determined by the value of
`tlon-when-assignee-is-nil' and
`tlon-when-assignee-is-someone-else'."
  (let ((capture-p t))
    (unless (tlon-assignee-is-current-user-p issue)
      (let* ((title (oref issue title))
	     (assignee (tlon-get-assignee issue))
	     (cond (if assignee
		       tlon-when-assignee-is-someone-else
		     tlon-when-assignee-is-nil))
	     (warning (if assignee
			  (format "Warning: the assignee of issue `%s' is %s." title assignee)
			(format "Warning: issue `%s' has no assignee." title))))
	(pcase cond
	  ('prompt
	   (if (y-or-n-p (concat warning " Assign to you? "))
	       (tlon-forg-change-assignee issue)
	     (setq capture-p nil)))
	  ('change (tlon-forg-change-assignee issue))
	  ('warn (message warning))
	  ('capture nil)
	  (_ (setq capture-p nil)))))
    capture-p))

(defun tlon-forg-change-assignee (&optional issue)
  "Change the assignee of ISSUE to the current user.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (tlon-set-assignee (tlon-user-lookup :github :name user-full-name) issue)
    (while (not (tlon-assignee-is-current-user-p issue))
      (tlon-pull-silently "Changing assignee...")
      (sleep-for 0.1))))

(defun tlon-capture-handle-phase (issue)
  "Take appropriate action when ISSUE has no valid job phase."
  (unless (tlon-get-phase-in-issue issue)
    (let* ((title (oref issue title))
	   (prompt (format "Warning: issue `%s' has no valid job phase. What should it be?" title))
	   (phase (completing-read prompt (tlon-label-lookup-all :label))))
      (tlon-set-labels `(,phase) 'phase issue))))

;;;;;; Capture jobs

(defun tlon-issue-is-job-p (&optional issue)
  "Return t if ISSUE is a job.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (when (string-match-p "^Job: " (oref issue title))
      t)))

(defun tlon-create-job-todo-from-issue (&optional issue)
  "Create a new `org-mode' job TODO based on ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (tlon-capture-handle-phase issue)
    (tlon-store-or-refile-job-todo issue)))

(defun tlon-store-master-job-todo (&optional set-issue issue)
  "Create a new job master TODO.
If SET-ISSUE is non-nil, set issue label to `Awaiting processing' and assignee
to the current user. If ISSUE is non-nil, use the issue at point or in the
current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (todo (tlon-make-todo-name-from-issue issue 'no-action 'no-status))
         (jobs-file (tlon-get-todos-jobs-file)))
    (if-let ((pos (tlon-get-todo-position todo jobs-file)))
	(tlon-visit-todo (cons pos jobs-file))
      (save-window-excursion
	(when set-issue
	  (tlon-set-initial-label-and-assignee))
	(tlon-store-todo "tbJ" 'master-todo issue)))))

(autoload 'org-extras-refile-goto-latest "org-extras")
(autoload 'org-extras-refile-at-position "org-extras")
(defun tlon-store-or-refile-job-todo (&optional issue)
  "Refile TODO under appropriate heading, or create new master TODO if none exists.
If ISSUE is nil, use the issue at point or in the current buffer."
  (if-let* ((issue (or issue (forge-current-topic)))
	    (pos (tlon-get-todo-position
		  (tlon-make-todo-name-from-issue issue 'no-action 'no-status)
		  (tlon-get-todos-jobs-file))))
      (save-window-excursion
	(tlon-store-todo "tbJ" nil issue)
	(let* ((inhibit-message t))
	  (org-extras-refile-at-position pos)
	  (org-extras-refile-goto-latest)))
    (when (y-or-n-p (format "No master TODO found for issue `%s'. Create?" (oref issue title)))
      (tlon-store-master-job-todo nil issue)
      (tlon-capture-issue issue))))

;;;;; Reconcile

;;;###autoload
(defun tlon-reconcile-issue-and-todo ()
  "With point on either, reconcile issue and associated TODO."
  (interactive)
  (tlon-todo-issue-funcall
   (lambda ()
     (with-current-buffer (tlon-get-issue-buffer)
       (tlon-reconcile-issue-and-todo-from-issue)))
   #'tlon-reconcile-issue-and-todo-from-issue))

;;;###autoload
(defun tlon-reconcile-all-issues-and-todos (arg)
  "Reconcile all TODOs with their issues.
Before initiating the reconciliation process, this command performs a full pull
of the current repo to ensure that local data reflects the remote state. The
window configuration active before the command was called will be restored after
completion. This command clears the `org-refile' cache upon completion.

Pull all issues from forge before initiating the reconciliation process. If
called with a prefix ARG, omit this initial pull."
  (interactive "P")
  (if arg
      (tlon-reconcile-all-issues-and-todos-after-pull)
    (tlon-pull-silently "Pulling issues..." #'tlon-reconcile-all-issues-and-todos-after-pull)))

(defun tlon-forg--get-all-todo-files ()
  "Return a list of all Org TODO files to be processed.
This includes the generic file, the jobs file, and all *.org files
in `paths-dir-tlon-todos'."
  (let ((files (list (tlon-get-todos-generic-file)
                     (tlon-get-todos-jobs-file))))
    (when (and (boundp 'paths-dir-tlon-todos)
               (stringp paths-dir-tlon-todos)
               (file-directory-p paths-dir-tlon-todos))
      (setq files (append files (directory-files paths-dir-tlon-todos t "\\.org$" t)))) ; t for full path
    ;; Ensure absolute paths and remove duplicates
    (delete-dups (mapcar #'expand-file-name files))))

(defun tlon-reconcile-all-issues-and-todos-after-pull ()
  "Reconcile TODOs with their issues after `forge-pull' is finished."
  (save-window-excursion
    (let ((todo-files (tlon-forg--get-all-todo-files)))
      (dolist (file todo-files)
        (when (file-exists-p file)
          (with-current-buffer (find-file-noselect file)
            (message "Reconciling TODOs in %s..." (file-name-nondirectory file))
            (save-excursion
              (let ((org-fold-core-style 'overlays))
                (org-fold-core-save-visibility
                    (org-fold-show-all)
                  (goto-char (point-min))
                  (while (re-search-forward org-heading-regexp nil t)
                    (when-let ((issue (tlon-get-issue))) ; tlon-get-issue gets from heading
                      (when (or (not (member org-archive-tag (org-get-tags)))
                                tlon-forg-include-archived)
                        (tlon-reconcile-issue-and-todo-from-issue issue)))))))))))
    (org-refile-cache-clear)
    (message "Finished reconciling all found issues and TODOs. Refile cache cleared.")))

(defun tlon-reconcile-issue-and-todo-from-issue (&optional issue)
  "Reconcile ISSUE and associated TODO.
If ISSUE is nil, use the issue at point."
  (let* ((issue (or issue (forge-current-topic)))
	 (pos-file-pair (tlon-get-todo-position-from-issue issue)) ; Returns (POS . FILE)
	 (issue-name (tlon-make-todo-name-from-issue issue)))
    (if pos-file-pair
        (save-window-excursion
          (tlon-visit-todo pos-file-pair) ; Pass the (POS . FILE) pair
          (let ((todo-name (substring-no-properties (org-get-heading nil nil t t))))
            (if (string= issue-name todo-name)
                (message "Issue ‘%s’ and its local TODO are in sync." (oref issue title))
              (tlon-reconcile-issue-and-todo-prompt issue-name todo-name))))
      (message "No TODO found for issue %s to reconcile." (oref issue title)))))

(defun tlon-reconcile-issue-and-todo-prompt (issue-name todo-name)
  "Prompt the user to reconcile discrepancies between ISSUE-NAME and TODO-NAME."
  (let ((choice (pcase tlon-forg-when-reconciling
		  ('prompt
		   (read-char-choice
		    (format "The issue differs from its todo. Keep (i)ssue or (t)odo?\n\nissue: `%s'\ntodo:  `%s'"
			    issue-name todo-name)
		    '(?i ?t)))
		  ('issue ?i)
		  ('todo ?t))))
    (pcase choice
      (?i (tlon-update-todo-from-issue issue-name))
      (?t (tlon-update-issue-from-todo)) ; No longer needs todo-name argument
      (_ (user-error "Aborted")))))

(defun tlon-forg--set-github-project-estimate (issue estimate-value)
  "Set GitHub Project estimate for ISSUE to ESTIMATE-VALUE.
ISSUE is a forge-topic object. ESTIMATE-VALUE is a float."
  (unless (and (boundp 'forge-extras-estimate-field-node-id)
               (stringp forge-extras-estimate-field-node-id)
               (not (string-empty-p forge-extras-estimate-field-node-id)))
    (user-error "`forge-extras-estimate-field-node-id' is not configured. Please set it"))
  (let* ((repo (forge-get-repository issue))
         (issue-number (oref issue number))
         (repo-name (oref repo name))
         (gh-fields (forge-extras-gh-get-issue-fields issue-number repo-name))
         (parsed-fields (when gh-fields (forge-extras-gh-parse-issue-fields gh-fields)))
         (issue-node-id (plist-get parsed-fields :issue-node-id))
         (current-project-item-id (plist-get parsed-fields :project-item-id))
         target-project-item-id)
    (unless parsed-fields
      (user-error "Could not retrieve project data for issue #%s. Aborting estimate update" issue-number))
    (unless issue-node-id
      (user-error "Could not retrieve GitHub Issue Node ID for #%s. Aborting estimate update" issue-number))

    (setq target-project-item-id
          (forge-extras--ensure-issue-in-project
           issue-number issue-node-id current-project-item-id
           (format "Issue #%s is not in Project %s (%s). Add it and set estimate to '%s'?"
                   issue-number forge-extras-project-number forge-extras-project-owner estimate-value)))
    (unless target-project-item-id
      (user-error "Failed to ensure issue #%s is in project. Aborting estimate update" issue-number))

    (if (forge-extras-gh-update-project-item-estimate-field
         forge-extras-project-node-id
         target-project-item-id
         forge-extras-estimate-field-node-id
         estimate-value)
        (message "Project estimate for issue #%s updated to %s." issue-number estimate-value)
      (user-error "Failed to update project estimate for issue #%s" issue-number))))

(defun tlon-forg--set-org-effort (effort-hours)
  "Set Org 'Effort' property from EFFORT-HOURS (float)."
  (let ((effort-str (org-duration-from-minutes (* effort-hours 60.0))))
    (org-entry-put nil "Effort" effort-str)
    (message "Org Effort property updated to %s." effort-str)))

(defun tlon-reconcile-estimate-from-issue (&optional issue)
  "Reconcile estimate for ISSUE and its associated TODO.
If ISSUE is nil, use the issue at point."
  (let ((issue (or issue (forge-current-topic))))
    (unless issue
      (user-error "No issue found to reconcile estimate for"))
    (let* ((repo (forge-get-repository issue)) ; Get repo object before switching buffer context
           (pos-file-pair (tlon-get-todo-position-from-issue issue)))
      (if pos-file-pair
        (save-window-excursion
          (tlon-visit-todo pos-file-pair) ; Visit the TODO, this changes current-buffer and default-directory
          ;; For subsequent GitHub/Forge operations, explicitly set default-directory
          (let ((default-directory (oref repo worktree)))
            (let* ((issue-number (oref issue number))
                   (repo-name (oref repo name)) ; repo-name is from the already fetched repo object
                   ;; GitHub Estimate (float hours)
                   (gh-fields (condition-case err
                                  (forge-extras-gh-get-issue-fields issue-number repo-name)
                                (error (progn (message "Error fetching GH fields for #%s: %s" issue-number err) nil))))
                   (parsed-gh-fields (if gh-fields (forge-extras-gh-parse-issue-fields gh-fields) nil))
                   (gh-estimate-hours (if parsed-gh-fields (plist-get parsed-gh-fields :effort) nil))
                   ;; Org Effort (float hours) - This part is Org-specific, doesn't need repo context
                   (org-effort-str (org-entry-get nil "Effort" 'inherit))
                   (org-effort-minutes (if org-effort-str (org-duration-to-minutes org-effort-str) nil))
                   (org-effort-hours (if org-effort-minutes (/ (float org-effort-minutes) 60.0) nil))
                   (epsilon 0.01)) ; Tolerance for float comparison

              (cond
               ;; Only GitHub has estimate
               ((and gh-estimate-hours (not org-effort-hours))
                (message "GitHub issue #%s has estimate %s, Org TODO has none. Updating Org TODO." issue-number gh-estimate-hours)
                (tlon-forg--set-org-effort gh-estimate-hours)) ; Org op, fine
               ;; Only Org TODO has estimate
               ((and org-effort-hours (not gh-estimate-hours))
                (message "Org TODO has effort %s, GitHub issue #%s has none. Updating GitHub issue." org-effort-str issue-number)
                (tlon-forg--set-github-project-estimate issue org-effort-hours)) ; GH op, default-directory is correctly set by outer let
               ;; Both have estimates, and they differ
               ((and gh-estimate-hours org-effort-hours (> (abs (- gh-estimate-hours org-effort-hours)) epsilon))
                (message "Estimates differ: GitHub issue #%s has %s hours, Org TODO has %s (%s hours)."
                         issue-number gh-estimate-hours org-effort-str org-effort-hours)
                (let ((choice (pcase tlon-forg-when-reconciling
                                ('prompt
                                 (read-char-choice
                                  (format "Estimates differ. Keep (g)itHub's (%s hrs) or (o)rg's (%s hrs)?"
                                          gh-estimate-hours org-effort-hours)
                                  '(?g ?o)))
                                ('issue ?g)
                                ('todo ?o)
                                (_ (user-error "Invalid `tlon-forg-when-reconciling' value: %s" tlon-forg-when-reconciling)))))
                  (pcase choice
                    (?g (message "Updating Org TODO to match GitHub estimate.")
                        (tlon-forg--set-org-effort gh-estimate-hours)) ; Org op, fine
                    (?o (message "Updating GitHub issue to match Org TODO estimate.")
                        (tlon-forg--set-github-project-estimate issue org-effort-hours)) ; GH op, default-directory is correctly set
                    (_ (user-error "Aborted estimate reconciliation")))))
               ;; Both have estimates and they are (close enough to) equal, or both are nil
               (t
                (message "Estimates for issue #%s and Org TODO are in sync or both are nil." issue-number))))))
      (message "No TODO found for issue %s to reconcile estimate." (oref issue title)))))

;;;###autoload
(defun tlon-reconcile-estimate ()
  "Reconcile estimate between current issue and its Org TODO.
Works from either context (issue buffer or TODO heading)."
  (interactive)
  (tlon-todo-issue-funcall
   (lambda () ; Called when in Org mode
     (let ((issue (tlon-get-issue)))
       (unless issue (user-error "Cannot find GitHub issue for this TODO"))
       (tlon-reconcile-estimate-from-issue issue)))
   (lambda () ; Called when in Forge mode
     (tlon-reconcile-estimate-from-issue (forge-current-topic)))))

(defun tlon-update-todo-from-issue (issue-name)
  "Update TODO to match ISSUE-NAME."
  (let ((original-visual-line-mode visual-line-mode))
    (visual-line-mode -1)
    (save-window-excursion
      (beginning-of-line)
      (re-search-forward " ")
      (org-fold-show-subtree)
      (org-kill-line)
      (insert issue-name)
      (message "TODO updated"))
    (visual-line-mode original-visual-line-mode)))

(defun tlon-update-issue-from-todo ()
  "Update the GitHub issue to match the Org TODO heading at point.
This includes title, labels (tags), and project status.
Uses functions from `forge-extras.el` for GitHub Project interactions."
  (interactive)
  (cl-block tlon-update-issue-from-todo
    (unless (org-at-heading-p)
      (user-error "Point is not on an Org heading"))
    (let ((issue (tlon-get-issue))) ; Get forge-issue from current heading context
      (unless issue
        (user-error "Could not find a corresponding GitHub issue for this TODO")
        (cl-return-from tlon-update-issue-from-todo))

      ;; Issue is confirmed to be non-nil at this point.
      (let* ((heading-element (org-element-at-point))
             (org-todo-keyword (org-element-property :todo-keyword heading-element))
             (org-raw-title-parts (org-element-property :raw-value heading-element)) ; :raw-value for title part
             (org-tags (org-element-property :tags heading-element))
             (repo (forge-get-repository issue))
             (issue-number (oref issue number))
             (repo-name (oref repo name)))

        ;; Extract base title from Org heading (stripping repo, issue link, etc.)
        ;; The raw-value is usually just the text after the keyword and before tags.
	;; If it contains the orgit-link, we need to be more careful.
	;; For now, assume raw-value is mostly the title. A more robust parsing might be needed.
	;; A simple approach: find the link, take its description, remove #num prefix.
	(let* ((link-in-title (save-excursion
				(goto-char (org-element-property :contents-begin heading-element))
				(when (re-search-forward org-link-bracket-re (org-element-property :contents-end heading-element) t)
				  (org-element-context))))
               (org-title-for-issue
		(if (and link-in-title (eq (org-element-type link-in-title) 'link))
                    (let ((desc (org-element-property :description link-in-title)))
                      (if (string-match "^#[0-9]+ \\(.*\\)$" desc) (match-string 1 desc) desc))
		  (string-trim org-raw-title-parts)))) ; Fallback to raw-value, might need refinement

	  (message "Org title for issue: '%s', Org tags: %s, Org TODO keyword: %s"
		   org-title-for-issue org-tags org-todo-keyword)

	  ;; Fetch current GH issue details, including project-specific fields
	  (let* ((gh-fields (condition-case err
				(forge-extras-gh-parse-issue-fields (forge-extras-gh-get-issue-fields issue-number repo-name))
                              (error (progn (message "Error fetching GH fields for #%s: %s" issue-number err) nil))))
		 (issue-node-id (plist-get gh-fields :issue-node-id))
		 (current-issue-title (oref issue title))
		 (current-issue-labels (sort (copy-sequence (tlon-forg-get-labels issue)) #'string<))
		 (current-gh-project-status-name (plist-get gh-fields :status))
		 (project-item-id (plist-get gh-fields :project-item-id)))

            (unless issue-node-id
              (message "Could not retrieve issue Node ID for #%s. Skipping updates." issue-number)
              (cl-return-from tlon-update-issue-from-todo))

            ;; 1. Update Title
            (unless (string= org-title-for-issue current-issue-title)
              (message "Updating issue title from '%s' to '%s'" current-issue-title org-title-for-issue)
              (forge--set-topic-title repo issue org-title-for-issue)
              (message "Issue title updated."))

            ;; 2. Update Labels (Tags)
            (let ((sorted-org-tags (sort (copy-sequence org-tags) #'string<)))
              (unless (equal sorted-org-tags current-issue-labels)
		(message "Updating issue labels from %s to %s" current-issue-labels sorted-org-tags)
		(forge--set-topic-labels repo issue sorted-org-tags)
		(message "Issue labels updated.")))

            ;; 3. Update Project Status
            (when org-todo-keyword ; Only proceed if there's an Org TODO keyword
              (let* ((target-gh-status-name (car (cl-rassoc org-todo-keyword tlon-todo-statuses :test #'string=))))
		(if (not target-gh-status-name)
                    (message
		     (concat
		      (format "Org TODO keyword '%s' does not map to a known GitHub Project status. " org-todo-keyword)
		      "Skipping status update."))
		  (unless (string= target-gh-status-name current-gh-project-status-name)
                    (message "Project status differs. Org implies '%s' (from %s), GitHub has '%s'."
                             target-gh-status-name org-todo-keyword (or current-gh-project-status-name "None/Unknown"))
                    (let ((target-status-option-id
			   (cdr (assoc target-gh-status-name forge-extras-status-option-ids-alist #'string=))))
                      (unless target-status-option-id
			(message "Cannot find Option ID for GitHub status '%s'. Skipping status update."
				 target-gh-status-name)
			(cl-return-from tlon-update-issue-from-todo))
                      (if project-item-id
			  ;; Issue is in project, update status
			  (progn
                            (message "Updating status for item %s to '%s' (Option ID: %s)"
				     project-item-id target-gh-status-name target-status-option-id)
                            (forge-extras-gh-update-project-item-status-field
			     forge-extras-project-node-id
			     project-item-id
			     forge-extras-status-field-node-id
			     target-status-option-id))
			;; Issue not in project
			(when (y-or-n-p
			       (format "Issue '%s' (#%s) is not in Project %s (%s). Add it and set status to '%s'?"
				       current-issue-title issue-number
				       forge-extras-project-number
				       forge-extras-project-owner
				       target-gh-status-name))
			  (message "Adding issue #%s to project %s..." issue-number forge-extras-project-node-id)
			  (let ((new-item-id (forge-extras-gh-add-issue-to-project
					      forge-extras-project-node-id issue-node-id)))
                            (if new-item-id
				(progn
				  (message "Issue added to project (New Item ID: %s). Now setting status to '%s'."
					   new-item-id target-gh-status-name)
				  (forge-extras-gh-update-project-item-status-field
				   forge-extras-project-node-id
				   new-item-id
				   forge-extras-status-field-node-id
				   target-status-option-id))
                              (warn "Failed to add issue #%s to project." issue-number))))))))))))
        (message "Issue update attempt complete.")))))

;;;;; Files

(defvar tlon-todos-jobs-file)
(defun tlon-get-todos-jobs-file ()
  "Get the file containing the jobs `org-mode' ID."
  (tlon-get-or-set-org-var 'tlon-todos-jobs-file
			   paths-tlon-todos-jobs-id))

(defvar tlon-todos-generic-file)
(defun tlon-get-todos-generic-file ()
  "Get the file containing the generic `org-mode' ID."
  (tlon-get-or-set-org-var 'tlon-todos-generic-file
			   paths-tlon-todos-generic-id))

(defun tlon-get-todo-position (string file &optional substring)
  "Return the position of STRING exactly matching TODO heading in FILE.
If SUBSTRING is non-nil, return the position of string matching a TODO heading
substring."
  (if substring
      (tlon-find-headline-substring-in-file string file)
    (org-find-exact-headline-in-buffer string (find-file-noselect file))))

(defun tlon-find-headline-substring-in-file (todo file)
  "Move point to TODO in FILE matching TODO."
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\*+.*" todo) nil t)
	(point)))))

(defun tlon-get-todos-file-from-issue (&optional issue)
  "Get the file where the current issue is or would be stored.
If ISSUE is nil, default to the issue at point."
  (if (tlon-issue-is-job-p issue)
      (tlon-get-todos-jobs-file)
    (tlon-get-todos-generic-file)))

(defun tlon-get-todo-position-from-issue (&optional issue)
  "Get the TODO position and file of ISSUE.
Return (POSITION . FILE) or nil. If the issue is a job, search for its heading
name in the jobs file. Else (generic issue), search first in the repo-specific
file (REPO-NAME.org in `paths-dir-tlon-todos'), then in the generic todos file,
using the `orgit-topic' ID as a substring. If ISSUE is nil, use the issue at
point."
  (when-let ((issue (or issue (forge-current-topic))))
    (if (tlon-issue-is-job-p issue)
        (let ((file (tlon-get-todos-jobs-file)))
          (when-let ((pos (tlon-get-todo-position
                           (tlon-make-todo-name-from-issue issue nil 'no-status)
                           file)))
            (cons pos file)))
      (let* ((search-string (format "\\[orgit-topic:%s\\]" (oref issue id)))
             (repo-specific-file (tlon-forg--get-repo-specific-todo-file issue))
             (pos-in-repo-file (and repo-specific-file
                                    (file-exists-p repo-specific-file)
                                    (tlon-get-todo-position search-string repo-specific-file 'substring)))
             (generic-file (tlon-get-todos-generic-file))
             (pos-in-generic-file (unless pos-in-repo-file ; Only search generic if not in repo-specific
                                    (and generic-file ; Ensure generic file path is valid
                                         (file-exists-p generic-file)
                                         (tlon-get-todo-position search-string generic-file 'substring)))))
        (cond (pos-in-repo-file (cons pos-in-repo-file repo-specific-file))
              (pos-in-generic-file (cons pos-in-generic-file generic-file))
              (t nil))))))

;;;;; Counterpart

;;;###autoload
(defun tlon-visit-counterpart ()
  "Visit the ID associated with TODO, or vice versa."
  (interactive)
  (tlon-todo-issue-funcall #'tlon-visit-issue
			   #'tlon-visit-todo))

;;;###autoload
(defun tlon-visit-counterpart-or-capture ()
  "Visit the issue associated with TODO, or vice versa, creating TODO if necessary."
  (interactive)
  (tlon-todo-issue-funcall #'tlon-visit-issue
			   #'tlon-visit-todo-or-capture))

(defun tlon-todo-issue-funcall (todo-fun issue-fun)
  "Call TODO-FUN or ISSUE-FUN depending on the current major mode."
  (pcase major-mode
    ('org-mode
     (unless (org-at-heading-p)
       (user-error "I could not find an `org-mode' heading at point"))
     (funcall todo-fun))
    ((or 'forge-topic-mode 'forge-issue-mode 'magit-status-mode)
     (unless (tlon-get-issue-name)
       (user-error "I could not find a GitHub issue at point"))
     (funcall issue-fun))
    (_ (user-error "This command cannot be invoked in `%s`" major-mode))))

;;;;; Get heading elements

(defun tlon-get-element-from-heading (regexp)
  "Get element matching REGEXP from the heading at point."
  (when (org-at-heading-p)
    (let ((heading (substring-no-properties (org-get-heading t t t t))))
      (when (string-match regexp heading)
	(match-string 1 heading)))))

(defun tlon-get-issue-number-from-heading ()
  "Get the GitHub issue number from the `org-mode' heading at point."
  (when-let ((issue-number (tlon-get-element-from-heading "#\\([[:digit:]]\\{1,4\\}\\)")))
    (string-to-number issue-number)))

(defun tlon-get-repo-from-heading ()
  "Get the repo from the heading at point."
  (let* ((abbrev-repo (tlon-get-element-from-heading "^\\[\\(.*?\\)\\]")))
    (tlon-repo-lookup :dir :abbrev abbrev-repo)))

(defun tlon-get-issue-number-from-open-issues (&optional forge-repo)
  "Prompt user to select from open issues in FORGE-REPO and return its number.
If FORGE-REPO (a forge-repository object) is nil, the repository is
determined from context or by prompting."
  (let* ((repo-to-use (or forge-repo
                          (let ((repo-path (tlon-get-repo nil 'include-all))) ; Prompts if needed
                            (when repo-path
                              (let ((default-directory repo-path))
                                (forge-get-repository :tracked))))))
         ;; Ensure repo-to-use is valid before proceeding
         (_ (unless repo-to-use (user-error "Could not determine repository for issue selection")))
         (default-directory (oref repo-to-use worktree)) ; Set context for oref issues
	 ;; Fetch all issues, but filter for open ones
	 (issue-list (mapcar #'(lambda (issue) ; issue here is a forge-issue object
				 (cons (format "#%d %s"
					       (oref issue number)
					       (oref issue title))
				       (oref issue number)))
			     (cl-remove-if-not (lambda (issue)
						 (string= (oref issue state) "open"))
					       (oref repo-to-use issues))))
	 ;; Let the user select one, requiring a match
	 (selected-issue-number (cdr (assoc (completing-read "Select an issue: " issue-list nil t) issue-list))))
    ;; Return the selected issue number
    selected-issue-number))

(defun tlon-get-issues (&optional repo)
  "Return a list of all open issues in REPO.
If REPO is nil, use the current repository."
  (let* ((repo (or repo (forge-get-repository :tracked)))
	 (repo-id (oref repo id))
	 (issues (forge-sql [:select [id]
				     :from issue
				     :where (= repository $s1)
				     :and  (= state 'open)]
			    repo-id)))
    (mapcar (lambda (issue-id)
	      (closql-get (forge-database) issue-id 'forge-issue))
	    (mapcar #'car issues))))

(defun tlon-get-latest-issue (&optional repo)
  "Return a list of the most recent open issue in REPO, or nil otherwise.
The list is in the format \\='(NUMBER TITLE). If REPO is nil, use the current
repository."
  (let* ((issues (tlon-get-issues repo))
	 (latest-issue (car (sort issues (lambda (a b)
					   (time-less-p
					    (date-to-time (oref b created))
					    (date-to-time (oref a created))))))))
    (if latest-issue
        (list (oref latest-issue number) (oref latest-issue title))
      nil)))

(defun tlon-count-issues (&optional repo)
  "Return the number of open issues in REPO.
If REPO is nil, use the current repository."
  (length (tlon-get-issues repo)))

;;;;; Set heading elements

(autoload 'org-extras-goto-beginning-of-heading-text "org-extras")
(defun tlon-set-repo-in-heading ()
  "Set the repo in the heading at point if not already present."
  (when (and (org-at-heading-p)
	     (not (tlon-get-repo-from-heading)))
    (let* ((repo-name (completing-read "Select repo: " (tlon-repo-lookup-all :name)))
	   (abbrev-repo (tlon-repo-lookup :abbrev :name repo-name)))
      (org-extras-goto-beginning-of-heading-text)
      (insert (format "[%s] " abbrev-repo)))))

(defun tlon-set-issue-number-in-heading (issue-number)
  "Set ISSUE-NUMBER in heading at point if not already present."
  (unless (tlon-get-issue-number-from-heading)
    (org-extras-goto-beginning-of-heading-text)
    ;; move past repo name
    (re-search-forward "\\[.+?\\] ")
    (insert (format "#%s " (number-to-string issue-number)))))

;;;;; Close issues, todos

;;;###autoload
(defun tlon-close-issue-and-todo ()
  "With point on either, close issue and associated TODO."
  (interactive)
  (tlon-todo-issue-funcall
   #'tlon-close-issue-and-todo-from-issue
   (lambda ()
     (tlon-visit-counterpart)
     (tlon-close-issue-and-todo-from-issue))))

(defun tlon-close-issue-and-todo-from-issue ()
  "With point on issue, close issue and associated TODO."
  (let ((issue (tlon-get-issue))) ; Get forge-issue object from context
    (when issue
      (tlon-close-issue-number (oref issue number) (oref (forge-get-repository issue) worktree))
      (let ((pos-file-pair (tlon-get-todo-position-from-issue issue)))
        (if pos-file-pair
            (progn
              (tlon-visit-todo pos-file-pair)
              (org-todo "DONE")
              (if tlon-forg-archive-todo-on-close
                  (progn
                    (org-archive-subtree-default)
                    (message "Closed issue and TODO (archived)."))
                (message "Closed issue and TODO."))
              (save-buffer)) ; Save buffer after TODO state change and potential archive
          (message "Closed issue, but no corresponding TODO found to mark as DONE."))))))

;; shouldn’t this be done using the orgit-link rather than issue-number?
(defun tlon-close-issue-number (issue-number repo)
  "Close the issue with ISSUE-NUMBER in REPO."
  (tlon-visit-issue issue-number repo)
  (tlon-close-issue))

;;;;; Get tags, status, assignees

(defun tlon-get-labels-of-type (type &optional issue)
  "Return the valid label(s) of TYPE in ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (when-let* ((labels (tlon-forg-get-labels issue)))
      (pcase type
	('tag labels)
	('phase (tlon-get-phase-in-labels labels))
	(_ (user-error "Invalid type"))))))

;;;;;; sort by tag

;;;###autoload
(defun tlon-forg-sort-by-tag ()
  "Sort org entries by the first tag that matches the relevant regexp pattern.
The pattern is defined in `tlon-forg-sort-by-tags-regexp'. Unmatched tags are
sorted to the end."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an `org-mode' buffer"))
  (org-sort-entries nil ?f #'tlon-forg-tag-sorter))

(defun tlon-forg-tag-sorter ()
  "Return the first tag of an org heading that matches the relevant regexp pattern.
The pattern is defined in `tlon-forg-sort-by-tags-regexp'. If no tag matches,
return \"~\", so that the entry is sorted to the end."
  (let ((tags (org-get-tags)))
    (catch 'match
      (dolist (tag tags)
	(when (string-match-p tlon-forg-sort-by-tags-regexp tag)
	  (throw 'match tag)))
      "~")))

;;;;;; status

(defun tlon-get-status-in-issue (&optional issue _upcased)
  "Return the GitHub Project status of ISSUE, mapped to an Org TODO keyword.
The status is derived from the \"Status\" field of the issue's project item in
the project specified by `forge-extras-project-number', fetched via `gh` CLI
by `forge-extras-gh-get-issue-fields`. Returns an uppercase string like
\"DOING\", \"NEXT\", \"DONE\", etc., or a fallback like \"TODO\" if the project
status cannot be determined or the issue is not found in the project. If ISSUE
is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
         (repo (if issue (forge-get-repository issue) nil))
         (repo-name (if repo (oref repo name) nil))
         (issue-number (if issue (oref issue number) nil)))
    (if (and issue repo repo-name issue-number) ; Ensure issue object and its details are valid
        (let* ((raw-fields (condition-case err
                               (forge-extras-gh-get-issue-fields issue-number repo-name) ; Use forge-extras
                             (error (progn
                                      (message "Error fetching GitHub project fields for #%s in %s (via forge-extras): %s" issue-number repo-name err)
                                      nil))))
               (parsed-fields (if raw-fields (forge-extras-gh-parse-issue-fields raw-fields) nil)) ; Use forge-extras
               (project-status-val (if parsed-fields (plist-get parsed-fields :status) nil))
               (org-status (if project-status-val
                               (cdr (assoc project-status-val tlon-todo-statuses #'string=))
                             nil)))
          (if org-status
              org-status
            (progn
              (message "Unknown or missing project status for #%s in Project #%s (owner: %s, repo: %s, GH status: %s). Falling back to open/closed state."
                       issue-number forge-extras-project-number forge-extras-project-owner repo-name project-status-val)
              (if (eq (oref issue state) 'completed) "DONE" "TODO"))))
      (progn
        (message "Could not determine issue/repo details for project status fetch. Falling back to open/closed state.")
        (if (and issue (eq (oref issue state) 'completed)) "DONE" "TODO")))))

(defun tlon-get-status-in-todo ()
  "Return the status of the `org-mode' heading at point.
The status is returned downcased."
  (when-let ((status (org-get-todo-state)))
    (when (member status (mapcar #'cdr tlon-todo-statuses))
      (downcase status))))

;;;;;; job phases
;; TODO: make nomenclature consistent: should be called `phases' throughout, not `labels'

(defun tlon-get-phase-in-labels (labels)
  "Return the unique valid phase in LABELS.
A label is considered a valid phase if it exists in `tlon-todo-statuses'.
If more than one phase is found, signal an error."
  (when-let ((status (cl-intersection labels
				      (tlon-label-lookup-all :label)
				      :test 'string=)))
    (if (= (length status) 1)
	(car status)
      (user-error "Issue has more than one status"))))

(defun tlon-get-phase-in-issue (&optional issue)
  "Return the unique valid job phase in ISSUE.
A label is valid job phase iff it is a member of `tlon-job-labels'.

If ISSUE is nil, use the issue at point or in the current buffer."
  (tlon-get-labels-of-type 'phase issue))


;;;;;; assignees

(defun tlon-get-assignee (&optional issue)
  "Return assignee of ISSUE.
If the issue has more than one assignee, return the first. If ISSUE is nil, use
the issue at point or in the current buffer."
  (when-let* ((issue (or issue (forge-current-topic)))
	      (assignees (car (closql-dref issue 'assignees)))
	      (assignee-name (nth 1 assignees)))
    assignee-name))

;;;;;; labels

(defun tlon-forg-get-labels (&optional issue)
  "Return the labels of ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (labels (closql-dref issue 'labels))
	 (label-names (mapcar (lambda (label)
				(nth 1 label))
			      labels)))
    label-names))

(defun tlon-get-tags-in-todo ()
  "Return the valid tags in the `org-mode' heading at point.
A tag is valid iff it is a member of `tlon-todo-tags'."
  (when-let ((tags (cl-intersection
		    (cdr (org-get-tags)) tlon-todo-tags :test 'string=)))
    tags))

;;;;; Validate

(defun tlon-assignee-is-current-user-p (issue)
  "Return t iff the assignee of ISSUE is the current user."
  (let ((assignee (tlon-get-assignee issue))
	(user (or
	       tlon-forg-enforce-user
	       (tlon-user-lookup :github :name user-full-name))))
    (string= assignee user)))

(defun tlon-todo-has-valid-status-p ()
  "Return t iff status of TODO at point it is a valid TODO status.
A status is valid iff it is one of the `cdr` values in `tlon-todo-statuses'."
  (when-let ((status (org-get-todo-state)))
    (member status (mapcar #'cdr tlon-todo-statuses))))

;;;;; Set tags, status, assignees

(defun tlon-set-labels (labels &optional type issue)
  "Set list of LABELS of TYPE in ISSUE.
If TYPE is `phase' or `status', replace the label of that type if present, while
keeping all other labels. If TYPE is any other value, add LABELS and keep all
current labels.

If ISSUE is nil, use issue at point or in the current buffer."
  (interactive)
  (let* ((issue (or issue (forge-current-topic)))
	 (repo (forge-get-repository issue))
	 (current-labels (tlon-forg-get-labels issue))
	 (status (tlon-get-status-in-issue issue))
	 (phase (tlon-get-phase-in-issue issue))
	 (trimmed-labels (pcase type
			   ('status (remove status current-labels))
			   ('phase (remove phase current-labels))
			   (_ current-labels))))
    (forge--set-topic-labels repo issue (append labels trimmed-labels))))

;; TODO: Cleanup the two functions below
(defun tlon-set-job-label ()
  "Prompt the user to select a job label."
  (let ((label (completing-read "What should be the label? "
				(tlon-label-lookup-all :label))))
    label))

(defun tlon-set-status (&optional prompt)
  "Prompt the user to select a status label.
Use PROMPT as the prompt, defaulting to \"TODO status? \"."
  (let* ((prompt (or prompt "TODO status? "))
	 (label (completing-read prompt (mapcar #'downcase (mapcar #'cdr tlon-todo-statuses)) nil t)))
    label))

(defun tlon-set-assignee (assignee &optional issue)
  "Make ASSIGNEE the assignee of ISSUE.
If ISSUE is nil, use issue at point or in the current buffer."
  (interactive
   (list (tlon-select-assignee)))
  (let* ((issue (or issue (forge-current-topic)))
	 (repo (forge-get-repository issue)))
    (forge--set-topic-assignees repo issue `(,assignee))))

(defun tlon-select-assignee (&optional prompt)
  "Prompt the user to select an ASSIGNEE.
Use PROMPT as the prompt, defaulting to \"Who should be the assignee? \"."
  (let* ((prompt (or prompt "Who should be the assignee? "))
	 (assignee (completing-read prompt
				    (tlon-user-lookup-all :github) nil nil
				    (tlon-user-lookup :github :name user-full-name))))
    assignee))

(defun tlon-set-initial-label-and-assignee ()
  "Set label to `Awaiting processing' and assignee to current user."
  (tlon-set-labels '("Awaiting processing") 'phase)
  (tlon-set-assignee (tlon-user-lookup :github :name user-full-name)))


;;;;; Get elements

(defun tlon-get-element (element &optional issue)
  "Return ELEMENT of ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (when-let ((issue (or issue (forge-current-topic))))
    (eieio-oref issue element)))

(defun tlon-get-first-element (element &optional issue)
  "Return first ELEMENT of ISSUE.
If the issue has more than one element, return the first. If ISSUE is nil, use
the issue at point or in the current buffer."
  (when-let ((issue (or issue (forge-current-topic))))
    (caar (eieio-oref issue element))))

;; TODO: should return all labels, not just first
(defun tlon-get-first-label (&optional issue)
  "Return the first label of the issue at point.
If ISSUE is nil, use the issue at point or in the current buffer."
  (tlon-get-first-element 'labels issue))

(defun tlon-get-state (&optional issue)
  "Return state of ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (when-let ((issue (or issue (forge-current-topic))))
    (oref issue state)))

;;;;; ?

(defun tlon-get-issue-name (&optional issue)
  "Get the name of ISSUE.
An issue name is its number followed by its title.

If ISSUE is nil, get the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (number (oref issue number))
	 (title (oref issue title)))
    (format "#%s %s" number title)))

(defun tlon-get-issue-link (&optional issue)
  "Get an `org-mode' link to ISSUE.
If ISSUE is nil, get the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (name (tlon-get-issue-name issue))
	 (id (oref issue id)))
    (org-link-make-string (format "orgit-topic:%s" id) name)))

(defun tlon-make-todo-name-from-issue (&optional issue no-action no-status)
  "Construct the name of TODO from ISSUE.
For job TODOs, the resulting name will have a name with the form \"[REPO] ACTION
NAME\". ACTION is optional, and used only for job TODOs. For example, if the
TODO is \"[uqbar-es] #591 Job: `Handbook2022ExerciseForRadical`\", and ACTION is
\"Process\", the function returns \"[uqbar-es] Process #591 Job:
`Handbook2022ExerciseForRadical`\".

If NO-ACTION is non-nil, omit, the ACTION element. If NO-STATUS is non-nil, omit
the STATUS element. If ISSUE is nil, use the issue at point or in the current
buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (action (if (and (tlon-issue-is-job-p issue)
			  (not no-action))
		     (or (tlon-label-lookup :action :label (tlon-get-first-label issue))
			 "")
		   ""))
	 (status (tlon-get-status-in-issue issue 'upcased))
	 (tags (tlon-forg-get-labels issue))
	 (repo-name (oref (forge-get-repository issue) name))
	 (repo-abbrev (tlon-repo-lookup :abbrev :name repo-name))
	 (todo-name (replace-regexp-in-string
		     "[[:space:]]\\{2,\\}"
		     " "
		     (concat
		      (unless no-status (format "%s " (or status "")))
		      (format "[%s] %s %s" repo-abbrev action (tlon-get-issue-link issue))
		      (when tags (format "   :%s:" (mapconcat #'identity tags ":")))))))
    todo-name))

(defvar tlon-key-regexp)
(declare-function tlon-get-file-from-key "tlon")
(defun tlon-get-file-from-issue (&optional issue)
  "Get the file path of ISSUE.
If ISSUE is nil, use the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (name (tlon-get-issue-name issue)))
    (if (string-match tlon-key-regexp name)
	(tlon-get-file-from-key (match-string 1 name))
      (user-error "I wasn't able to find a file at point or in the forge buffer"))))

;;;###autoload
(defun tlon-open-forge-file ()
  "Open the file of the issue at point or in the current buffer."
  (interactive)
  (find-file (tlon-get-file-from-issue)))

(declare-function tlon-open-counterpart "tlon-counterpart")
;;;###autoload
(defun tlon-open-forge-counterpart ()
  "Open the file counterpart of the issue at point or in the current buffer."
  (interactive)
  (tlon-open-counterpart nil (tlon-get-file-from-issue)))

;;;;; Create issues

(defun tlon-create-issue (title &optional repo body)
  "Create new GitHub issue in REPO with TITLE and BODY."
  (let* ((repo (or repo (tlon-get-repo 'error 'include-all)))
	 (body (or body ""))
	 (default-directory repo)
	 (repo (forge-get-repository :tracked))
	 (owner (oref repo owner))
	 (reponame (oref repo name))
	 (resource (format "/repos/%s/%s/issues" owner reponame))
	 (data `((title . ,title)
		 (body . ,body))))
    (ghub-post resource data
	       :auth 'forge
	       :noerror t ;; avoid showing the original large output
	       :reader 'ignore) ;; do not parse the response json
    (message "Created issue with title %s" title)))

;; currently only used for submitting bugs; maybe it can be integrated with the
;; forg code
(defun tlon-create-issue-in-dir (dir)
  "Create a new issue in the git repository at DIR."
  (magit-status-setup-buffer dir)
  (forge-create-issue))

;;;###autoload
(defun tlon-report-bug ()
  "Submit a bug report in the `tlon.el' repo."
  (interactive)
  (tlon-create-issue-in-dir (tlon-repo-lookup :dir :name "tlon.el"))
  (tlon-prepopulate-bug-report-buffer))

(defun tlon-prepopulate-bug-report-buffer ()
  "Prepopulate the bug report buffer with relevant headings and information."
  (goto-char (point-max))
  (insert (concat "\n\n"
		  "## Descripción\n\n\n"
		  "## Pasos para reproducir\n\n1.\n2.\n3.\n\n\n"
		  "## Comentarios\n<!-- Opcional -->\n\n\n"
		  "## Backtrace\n<!-- Si el backtrace es demasiado largo y GitHub lo rechaza, adjuntar como archivo txt -->\n\n\n```elisp\n\n```\n\n"
		  "## Información del sistema\n<!-- No modificar -->\n"
		  "\n- Tlon version: " tlon-version
		  "\n- Latest commit: " (tlon-get-latest-commit)
		  "\n\n"))
  (goto-char (+ (point-min) 2)))

;;;###autoload
(defun tlon-create-issue-from-todo ()
  "Create a new GitHub issue based on the current `org-mode' heading.
The repository is inferred first from the heading, then from the
current buffer's filename if it matches a known repository name.
If it cannot be inferred, the user is prompted to select a repository,
which is then added to the heading."
  (interactive)
  (tlon-ensure-org-mode)
  (when (tlon-get-issue-number-from-heading)
    (user-error "This heading already has an issue"))

  (let (repo-dir forge-repo repo-abbrev-inferred)
    ;; 1. Try to get repo from heading
    (setq repo-dir (tlon-get-repo-from-heading))

    ;; 2. If not in heading, try to infer from filename
    (unless repo-dir
      (when buffer-file-name
	(let* ((filename-sans-ext (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
	       (repo-dir-from-file (ignore-errors (tlon-repo-lookup :dir :name filename-sans-ext))))
	  (when repo-dir-from-file
	    (setq repo-dir repo-dir-from-file)
	    ;; If repo inferred from filename, ensure heading has the repo abbrev
	    (unless (tlon-get-repo-from-heading)
	      (setq repo-abbrev-inferred (tlon-repo-lookup :abbrev :dir repo-dir))
	      (when repo-abbrev-inferred
		(org-extras-goto-beginning-of-heading-text)
		(insert (format "[%s] " repo-abbrev-inferred))))))))

    ;; 3. If still no repo, prompt and add to heading
    (unless repo-dir
      (tlon-set-repo-in-heading) ; This prompts and modifies the heading
      (setq repo-dir (tlon-get-repo-from-heading))
      (unless repo-dir
	(user-error "Repository selection cancelled or failed")))

    ;; Get forge-repo object from repo-dir
    (setq forge-repo (let ((default-directory repo-dir)) (forge-get-repository :tracked)))
    (unless forge-repo
      (user-error "Could not get forge repository object for %s" repo-dir))

    ;; At this point, repo-dir and forge-repo MUST be set, and the heading should reflect the repo.
    (let (todo-linkified)
      (save-excursion
	(let* ((default-directory repo-dir) ; Set default-directory for operations that might need it implicitly
	       (heading (substring-no-properties (org-get-heading t t t t))) ; Read heading *after* potential modifications
	       (status (tlon-get-status-in-todo))
	       (tags (tlon-get-tags-in-todo))
	       ;; abbrev-repo should now reliably come from the heading
	       (abbrev-repo (tlon-get-element-from-heading "^\\[\\(.*?\\)\\]"))
	       (_ (unless abbrev-repo (error "Logic error: Repo abbrev not found in heading '%s' after setup for repo %s" heading repo-dir)))
	       (issue-title (substring heading (+ (length abbrev-repo) 3)))
	       (latest-issue-pre (car (tlon-get-latest-issue forge-repo)))
	       (latest-issue-post latest-issue-pre))

	  (tlon-create-issue issue-title repo-dir) ; Pass repo-dir, so tlon-create-issue won't prompt

	  (forge--pull forge-repo) ; Use forge--pull for synchronous pull with specific repo
	  (while (eq latest-issue-pre latest-issue-post)
	    (sleep-for 0.1)
	    (setq latest-issue-post (car (tlon-get-latest-issue forge-repo))))
	  (tlon-set-issue-number-in-heading latest-issue-post)
	  ;; tlon-visit-issue takes number and repo path
	  (tlon-visit-issue latest-issue-post repo-dir)
	  ;; Operations on the visited issue will use its context
	  (tlon-set-assignee (tlon-user-lookup :github :name user-full-name))
	  (tlon-set-labels (append `(,status) tags)))
	(setq todo-linkified (tlon-make-todo-name-from-issue nil nil 'no-status)))
      (org-edit-headline todo-linkified))))

(defun tlon-create-issue-or-todo ()
  "Create issue from TODO or vice versa."
  (interactive)
  (tlon-todo-issue-funcall #'tlon-create-issue-from-todo
			   #'tlon-capture-issue))

(declare-function tlon-get-key-in-buffer "tlon")
(defun tlon-create-issue-from-key (&optional key)
  "Create an issue based on KEY.
If KEY is not provided, the key in the Markdown buffer at point is used."
  (let ((default-directory (tlon-get-repo 'error))
	(key (or key (tlon-get-key-in-buffer))))
    (tlon-create-issue (format "Job: `%s`" key) default-directory)))

(defun tlon-close-issue (&optional issue)
  "Close ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (repo (forge-get-repository issue)))
    (when (eq 'open (oref issue state))
      (forge--set-topic-state repo issue 'completed))))

(defun tlon-get-parent-todo (todo)
  "Get parent of TODO in `tlon-todos-jobs-file'."
  (let* ((jobs-file (tlon-get-todos-jobs-file))
         (pos (tlon-get-todo-position todo jobs-file)))
    (when pos
      (save-window-excursion
        (tlon-visit-todo (cons pos jobs-file))
        (widen)
        (org-up-heading-safe)
        (org-no-properties (org-get-heading))))))

(defun tlon-mark-todo-done (todo file)
  "Mark TODO in FILE as DONE."
  (let ((pos (tlon-get-todo-position todo file)))
    (when pos
      (save-window-excursion
        (tlon-visit-todo (cons pos file))
        (org-todo "DONE")
        (save-buffer)
        (message "Marked `%s' as DONE" todo)))))

(declare-function tlon-get-clock-key "tlon-clock")
(declare-function tlon-get-clock-label "tlon-clock")
;; MAYBE: move to jobs?
(defun tlon-check-label-and-assignee (repo)
  "Check that clocked action, user match label, assignee of issue in REPO."
  (save-window-excursion
    (let* ((default-directory repo)
	   (key (tlon-get-clock-key))
	   (issue-title (format "Job: `%s" key))
	   (issue (tlon-issue-lookup issue-title))
	   (clocked-label (tlon-get-clock-label))
	   (label (tlon-get-first-label issue))
	   (assignee (tlon-user-lookup :name :github (tlon-get-assignee issue))))
      (unless (string= clocked-label label)
	(user-error "The `org-mode' TODO says the label is `%s', but the actual issue label is `%s'"
		    clocked-label label))
      (unless (string= user-full-name assignee)
	(user-error "The `org-mode' TODO says the assignee is `%s', but the actual issue assignee is `%s'"
		    user-full-name assignee))
      t)))

;;;;; misc

(declare-function org-roam-extras-node-find-special "org-roam-extras")
;;;###autoload
(defun tlon-node-find ()
  "Prompt the user to select a Tlön TODO in a list sorted by priority."
  (interactive)
  (org-roam-extras-node-find-special `(:dir ,paths-dir-tlon-todos)))

;;;;; Transient

(defun tlon-symbol-reader (prompt symbols)
  "Return a list of SYMBOLS with PROMPT to be used as an `infix' reader function."
  (let* ((input (completing-read prompt
				 (mapcar 'symbol-name symbols))))
    (intern input)))

(transient-define-infix tlon-when-assignee-is-nil-infix ()
  "Set the value of `tlon-when-assignee-is-nil' in `forg' menu."
  :class 'transient-lisp-variable
  :reader (lambda (prompt _ _)
	    (tlon-symbol-reader prompt '(prompt change warn capture no-capture)))
  :transient t
  :prompt "What to do when the assignee is nil (see docstring for details): "
  :variable 'tlon-when-assignee-is-nil)

(transient-define-infix tlon-when-assignee-is-someone-else-infix ()
  "Set the value of `tlon-when-assignee-is-someone-else' in `forg' menu."
  :class 'transient-lisp-variable
  :reader (lambda (prompt _ _)
	    (tlon-symbol-reader prompt '(prompt change warn capture no-capture)))
  :transient t
  :prompt "What to do when the assignee is someone else (see docstring for details): "
  :variable 'tlon-when-assignee-is-someone-else)

(transient-define-infix tlon-forg-when-reconciling-infix ()
  "Set the value of `tlon-forg-when-reconciling' in `forg' menu."
  :class 'transient-lisp-variable
  :reader (lambda (prompt _ _)
	    (tlon-symbol-reader prompt '(prompt issue todo)))
  :transient t
  :prompt "What to do when the issue and its associated todo differ (see docstring for details): "
  :variable 'tlon-forg-when-reconciling)

(transient-define-infix tlon-infix-toggle-include-archived ()
  "Toggle the value of `tlon-forg-include-archived' in `forg' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-forg-include-archived
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-forg-include-archived)))

(transient-define-infix tlon-infix-toggle-archive-todo-on-close ()
  "Toggle the value of `tlon-forg-archive-todo-on-close' in `forg' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-forg-archive-todo-on-close
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-forg-archive-todo-on-close)))

;;;###autoload (autoload 'tlon-forg-menu "tlon-forg" nil t)
(transient-define-prefix tlon-forg-menu ()
  "`forg' menu."
  :info-manual "(tlon) GitHub and org-mode synchronization"
  [["Actions"
    ("y" "dwim"                                             tlon-visit-counterpart-or-capture)
    ("v" "visit"                                            tlon-visit-counterpart)
    ("p" "post"                                             tlon-create-issue-from-todo)
    ("x" "close"                                            tlon-close-issue-and-todo)
    ("s" "sort"                                             tlon-forg-sort-by-tag)]
   ["Capture"
    ("c" "capture"                                          tlon-capture-issue)
    ("C" "capture all"                                      tlon-capture-all-issues)]
   ["Reconcile"
    ("r" "reconcile"                                        tlon-reconcile-issue-and-todo)
    ("R" "reconcile all"                                    tlon-reconcile-all-issues-and-todos)
    ("e" "reconcile estimate"                               tlon-reconcile-estimate)]
   ["Options"
    ("-r" "When reconciling"                                tlon-forg-when-reconciling-infix)
    ("-n" "When assignee is nil"                            tlon-when-assignee-is-nil-infix)
    ("-e" "When assignee is someone else"                   tlon-when-assignee-is-someone-else-infix)
    ""
    ("-a" "Include archived"                                tlon-infix-toggle-include-archived)
    ("-A" "Archive on close"                                tlon-infix-toggle-archive-todo-on-close)]])

(provide 'tlon-forg)
;;; tlon-forg.el ends here
