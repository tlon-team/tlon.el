;;; tlon-babel-ogh.el --- Org-GitHub integration -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon-babel
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

;; Org-GitHub integration.

;;; Code:

(require 'forge)
(require 'org)
(require 'tlon-babel)

;;;; User options

(defgroup tlon-babel-gh ()
  "Org-GitHub integration."
  :group 'tlon-babel)

(defcustom tlon-babel-ogh-warn-when-no-valid-label t
  "Whether to warn the user when the issue has no valid label.
If t, warn the user. If `prompt', prompt the user for a label. If nil, do
nothing."
  :type '(choice (const :tag "Warn" t)
                 (const :tag "Prompt" prompt)
                 (const :tag "Do nothing" nil))
  :group 'tlon-babel-gh)

;;;; Main variables

(defconst tlon-babel-ogh-todo-statuses
  '("TODO" "IMPORTANT" "URGENT" "SOMEDAY" "MAYBE")
  "List of admissible TODO statuses.
Note that your `org-todo-keywords' user option should include these labels for
`org-mode' to recognize them, and that the buffer has to be refreshed after the
value of that option is reset.")

;;;; Functions

;;;;; Movement

(defun tlon-babel-ogh-visit-issue (&optional number repo)
  "Visit Github issue.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (interactive)
  (forge-visit-issue (tlon-babel-get-issue number repo)))

(defun tlon-babel-get-issue (&optional number repo)
  "Get Github issue.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (when-let* ((number (or number
			  (tlon-babel-ogh-get-issue-number-from-heading)))
	      (repo (or repo
			(tlon-babel-ogh-get-repo-from-heading)))
	      (default-directory repo)
	      (forge-repo (forge-get-repository nil))
	      (issue-id (caar (forge-sql [:select [id] :from issue
						  :where (and (= repository $s1)
							      (= number $s2))]
					 (oref forge-repo id)
					 number))))
    (forge-get-topic issue-id)))

(defun tlon-babel-ogh-get-issue-buffer (&optional number repo)
  "Get Github issue buffer.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (save-window-excursion
    (tlon-babel-ogh-visit-issue number repo)
    (current-buffer)))

(defun tlon-babel-ogh-visit-todo (&optional pos file)
  "Visit TODO at POS in FILE.
If POS is nil, use the position of the TODO associated with the issue at point.
If FILE is nil, use the file where the issue at point would be stored (depending
on whether or not is a job)."
  (if-let ((pos (or pos (tlon-babel-ogh-get-todo-position-from-issue)))
	   (file (or file (tlon-babel-ogh-get-todos-file-from-issue))))
      (tlon-babel-ogh-open-todo file pos)
    (user-error "No TODO found")))

(defun tlon-babel-ogh-open-todo (file position)
  "Open FILE at TODO POSITION."
  (find-file file)
  (widen)
  (org-kill-note-or-show-branches)
  (goto-char position))

(defun tlon-babel-ogh-visit-todo-or-capture ()
  "Visit the TODO associated with the current issue, creating one if necessary."
  (if-let ((pos (tlon-babel-ogh-get-todo-position-from-issue)))
      (tlon-babel-ogh-visit-todo pos)
    (tlon-babel-ogh-capture-issue)))

;;;###autoload
(defun tlon-babel-ogh-capture-issue (&optional issue)
  "Create a new `org-mode' TODO based on ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer.

This command triggers one of two `org-capture' capture templates, depending on
whether the issue is or is not a job. If it is a job, it will process it as new
job if it has neither a label nor an assignee, else it will refile it under the
appropriate heading."
  (interactive)
  (let ((issue (or issue (forge-current-topic))))
    (if (tlon-babel-ogh-issue-is-job-p issue)
	(tlon-babel-ogh-create-job-todo-from-issue issue)
      (tlon-babel-ogh-create-generic-todo-from-issue issue))))

;;;###autoload
(defun tlon-babel-ogh-capture-all-issues ()
  "Capture all issues in the current repository."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo 'error 'include-all))
	(current-user (tlon-babel-user-lookup :github :name user-full-name))
	(num-captured 0))
    (dolist (issue (tlon-babel-ogh-get-open-issues))
      (let ((assignee (tlon-babel-ogh-get-assignee issue))
	    (issue-name (tlon-babel-ogh-get-issue-name issue)))
	(when (not assignee)
	  (if (y-or-n-p (format "Issue `%s' has no assignee. Assign to you?" issue-name))
	      (progn
		(tlon-babel-ogh-assign-issue issue current-user)
		(while (not (string= current-user assignee))
		  (setq assignee (tlon-babel-ogh-get-assignee issue))
		  (sleep-for 1)))
	    (message "Issue `%s' skipped: assigned to no one." issue-name)))
	(if (and (string= current-user assignee)
		 (not (tlon-babel-ogh-get-todo-position-from-issue issue)))
	    (save-window-excursion
	      (tlon-babel-ogh-capture-issue issue)
	      (message "Issue `%s' captured." issue-name)
	      (setq num-captured (1+ num-captured)))
	  (message "Issue `%s' skipped: assigned to %s." issue-name assignee))))
    (message "%s issues captured." num-captured)))

(defun tlon-babel-ogh-assign-issue (&optional issue user)
  "Assign ISSUE to USER.
If ISSUE is nil, use the issue at point or in the current buffer."
  (interactive)
  (let ((issue (or issue (forge-current-topic))))
    (unless (tlon-babel-ogh-get-assignee issue)
      (let ((user (or user
		      (completing-read (format "Issue `%s' has no assignee. Assign to "
					       (tlon-babel-ogh-get-issue-name issue))
				       (tlon-babel-get-property-of-users :github)))))
	(tlon-babel-ogh-set-assignee user issue)))))

(defun tlon-babel-ogh-get-open-issues ()
  "Return a list of all open issues in the current repository."
  (let* ((repo (forge-get-repository nil))
	 (all-issues (forge-ls-topics repo 'forge-issue)))
    (seq-filter (lambda (issue)
		  (string= (oref issue state) "open"))
		all-issues)))

(defun tlon-babel-ogh-issue-is-job-p (&optional issue)
  "Return t if ISSUE at point is a job.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (when (string-match-p "^Job: " (oref issue title))
      t)))

(defun tlon-babel-ogh-create-job-todo-from-issue (&optional issue)
  "Create a new `org-mode' job TODO based on ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (tlon-babel-ogh-check-label-or-assignee-present issue)
    (tlon-babel-ogh-check-label-present issue)
    (when (tlon-babel-ogh-capture-issue-p issue)
      (tlon-babel-ogh-store-or-refile-job-todo issue))))

;; TODO: revise so that it alerts the user when no label is present instead of
;; prompting for a label
(defun tlon-babel-ogh-create-generic-todo-from-issue (&optional issue)
  "Create a new `org-mode' generic TODO based on ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (when (tlon-babel-ogh-capture-issue-p issue)
    (let ((label (tlon-babel-ogh-get-label issue))
	  (issue (or issue (forge-current-topic))))
      (unless (tlon-babel-ogh-is-valid-status-p label issue)
	(tlon-babel-ogh-when-no-valid-label issue))
      (tlon-babel-ogh-store-todo "tbG" nil issue))))

(defun tlon-babel-ogh-when-no-valid-label (issue)
  "Take appropriate action when ISSUE has no valid label."
  (pcase tlon-babel-ogh-warn-when-no-valid-label
    ('prompt (progn
	       (tlon-babel-ogh-set-label (tlon-babel-ogh-set-status-label) issue)
	       (forge-pull-topic issue)
	       (while (not (tlon-babel-ogh-is-valid-status-p nil issue))
		 (sleep-for 1))))
    ('t (message "Issue `%s' has no valid label." (oref issue title)))
    ('nil nil)))

(defun tlon-babel-ogh-store-todo (template &optional no-action issue)
  "Store a new TODO using TEMPLATE.
If TODO already exists, signal an error. If NO-ACTION is non-nil, store a master
TODO. If ISSUE is non-nil, use it instead of the issue at point."
  (let ((issue (or issue (forge-current-topic))))
    (when (tlon-babel-ogh-get-todo-position-from-issue issue)
      (user-error "TODO `%s' already exists" (tlon-babel-ogh-get-issue-name issue)))
    (let ((todo (tlon-babel-ogh-make-todo-name-from-issue no-action nil issue)))
      (kill-new todo)
      (org-capture nil template))))

(defun tlon-babel-ogh-store-master-job-todo (&optional set-issue issue)
  "Create a new job master TODO.
If SET-ISSUE is non-nil, set issue label to `Awaiting processing' and assignee
to the current user. If ISSUE is non-nil, use the issue at point or in the
current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (todo (tlon-babel-ogh-make-todo-name-from-issue 'no-action 'no-state issue)))
    (if-let ((pos (tlon-babel-ogh-get-todo-position todo (tlon-babel-ogh-get-todos-jobs-file))))
	(tlon-babel-ogh-visit-todo pos)
      (save-window-excursion
	(when set-issue
	  (tlon-babel-ogh-set-initial-label-and-assignee))
	(tlon-babel-ogh-store-todo "tbJ" 'master-todo issue)))))

(defun tlon-babel-ogh-store-or-refile-job-todo (&optional issue)
  "Refile TODO under appropriate heading, or create new master TODO if none exists.
If ISSUE is nil, use the issue at point or in the current buffer."
  (if-let* ((issue (or issue (forge-current-topic)))
	    (pos (tlon-babel-ogh-get-todo-position
		  (tlon-babel-ogh-make-todo-name-from-issue 'no-action 'no-state issue)
		  (tlon-babel-ogh-get-todos-jobs-file))))
      (save-window-excursion
	(tlon-babel-ogh-store-todo "tbJ" nil issue)
	(let* ((inhibit-message t))
	  (org-extras-refile-at-position pos)
	  (org-extras-refile-goto-latest)))
    (when (y-or-n-p (format "No master TODO found for issue `%s'. Create?" (oref issue title)))
      (tlon-babel-ogh-store-master-job-todo nil issue)
      (tlon-babel-ogh-capture-issue issue))))

(defun tlon-babel-ogh-get-todos-jobs-file ()
  "Get the file containing the jobs `org-mode' ID."
  (or tlon-babel-todos-jobs-file
      (tlon-babel-set-value-of-var 'tlon-babel-todos-jobs-id)
      (setq tlon-babel-todos-jobs-file
	    (tlon-babel-ogh-get-file-with-id tlon-babel-todos-jobs-id))))

(defun tlon-babel-ogh-get-todos-generic-file ()
  "Get the file containing the generic `org-mode' ID."
  (or tlon-babel-todos-generic-file
      (tlon-babel-set-value-of-var 'tlon-babel-todos-generic-id)
      (setq tlon-babel-todos-generic-file
	    (tlon-babel-ogh-get-file-with-id tlon-babel-todos-generic-id))))

(defun tlon-babel-ogh-get-file-with-id (id)
  "Return the file containing the heading with the given `org-mode' ID."
  (when-let ((location (org-roam-id-find id)))
    (car location)))

(defun tlon-babel-ogh-get-todo-position (todo file &optional loose)
  "Return the position of TODO exactly matching heading in FILE.
If LOOSE is non-nil, return the position of the first TODO matching a substring
rather than strictly matching the heading."
  (if loose
      (tlon-babel-ogh-find-loose-headline-in-file todo file)
    (org-find-exact-headline-in-buffer todo (find-file-noselect file))))

(defun tlon-babel-ogh-find-loose-headline-in-file (todo file)
  "Move point to TODO in FILE matching TODO."
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\*+.*" todo) nil t)
	(point)))))

(defun tlon-babel-ogh-capture-issue-p (&optional issue)
  "Return t iff ISSUE should be captured.
An issue should be captured either if it is assigned to the current user or if
the user says so when prompted.

If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (assignee (tlon-babel-user-lookup :name :github (tlon-babel-ogh-get-assignee issue))))
    (if (string= user-full-name assignee)
	t
      (pcase (read-char-choice
	      (format "The assignee of `%s' is %s.\nSelf-assign? [y]es | no, and [c]apture | no, and do [n]ot capture"
		      (oref issue title) assignee)
	      '(?y ?c ?n))
	(?y (tlon-babel-ogh-set-assignee (tlon-babel-user-lookup :github :name user-full-name))
	    (while (not (tlon-babel-ogh-capture-issue-p issue))
	      (sleep-for 1)))
	(?c t)
	(?n nil)))))

(defun tlon-babel-ogh-check-label-or-assignee-present (&optional issue)
  "Check that ISSUE has a label or an assignee.
If not, offer to process it as a new job.

If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (assignee (tlon-babel-user-lookup :name :github (tlon-babel-ogh-get-assignee issue)))
	 (label (tlon-babel-ogh-get-label issue)))
    (unless (and assignee label)
      (if (y-or-n-p "Process issue as a new job (this will assign the issue to you, add the label 'Awaiting processing', and create a new master TODO in your org mode file)?")
	  (save-window-excursion
	    (tlon-babel-ogh-store-master-job-todo 'set-issue)
	    (while (not (and (tlon-babel-ogh-get-assignee)
			     (tlon-babel-ogh-get-label)))
	      (sleep-for 1))
	    (tlon-babel-ogh-capture-issue issue))
	(user-error "Aborted")))))

(defun tlon-babel-ogh-check-label-present (&optional issue)
  "Check that ISSUE has a label.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (label (tlon-babel-ogh-get-label issue)))
    (unless label
      (if (y-or-n-p "The issue has no label. Would you like to add one?")
	  (tlon-babel-ogh-set-label (tlon-babel-ogh-set-job-label))
	(user-error "Aborted")))))

(defun tlon-babel-ogh-get-todos-file-from-issue ()
  "Get the file where the current issue is or would be stored."
  (if (tlon-babel-ogh-issue-is-job-p)
      (tlon-babel-ogh-get-todos-jobs-file)
    (tlon-babel-ogh-get-todos-generic-file)))

(defun tlon-babel-ogh-get-todo-position-from-issue (&optional issue)
  "Get the TODO position of ISSUE, using the appropriate method.
If the issue is a job, use the heading name, else use the `orgit-topic' ID. If
ISSUE is nil, use the issue at point."
  (when-let ((issue (or issue (forge-current-topic))))
    (if (tlon-babel-ogh-issue-is-job-p issue)
	(tlon-babel-ogh-get-todo-position
	 (tlon-babel-ogh-make-todo-name-from-issue nil 'no-state issue)
	 (tlon-babel-ogh-get-todos-jobs-file))
      (tlon-babel-ogh-get-todo-position
       (oref issue id)
       (tlon-babel-ogh-get-todos-generic-file) 'loose))))

;;;###autoload
(defun tlon-babel-ogh-visit-counterpart ()
  "Visit the ID associated with TODO, or vice versa."
  (interactive)
  (tlon-babel-ogh-todo-issue-funcall #'tlon-babel-ogh-visit-issue
				     #'tlon-babel-ogh-visit-todo))

;;;###autoload
(defun tlon-babel-ogh-visit-counterpart-or-capture ()
  "Visit the issue associated with TODO, or vice versa, creating TODO if necessary."
  (interactive)
  (tlon-babel-ogh-todo-issue-funcall #'tlon-babel-ogh-visit-issue
				     #'tlon-babel-ogh-visit-todo-or-capture))

(defun tlon-babel-ogh-todo-issue-funcall (todo-fun issue-fun)
  "Call TODO-FUN or ISSUE-FUN depending on the current major mode."
  (pcase major-mode
    ('org-mode
     (unless (org-at-heading-p)
       (user-error "I could not find an `org-mode' heading at point"))
     (funcall todo-fun))
    ((or 'forge-topic-mode 'forge-issue-mode 'forge-issue-list-mode 'magit-status-mode)
     (unless (tlon-babel-ogh-get-issue-name)
       (user-error "I could not find a GitHub issue at point"))
     (funcall issue-fun))
    (_ (user-error "This command cannot be invoked in `%s`" major-mode))))

;;;;; Get heading elements

(defun tlon-babel-ogh-get-element-from-heading (regexp)
  "Get element matching REGEXP from the heading at point."
  (when (org-at-heading-p)
    (let ((heading (substring-no-properties (org-get-heading t t t t))))
      (when (string-match regexp heading)
	(match-string 1 heading)))))

(defun tlon-babel-ogh-get-issue-number-from-heading ()
  "Get the GitHub issue number from the `org-mode' heading at point."
  (when-let ((issue-number (tlon-babel-ogh-get-element-from-heading "#\\([[:digit:]]\\{1,4\\}\\)")))
    (string-to-number issue-number)))

(defun tlon-babel-ogh-get-repo-from-heading ()
  "Get the repo from the heading at point."
  (let* ((abbrev-repo (tlon-babel-ogh-get-element-from-heading "^\\[\\(.*?\\)\\]")))
    (tlon-babel-repo-lookup :dir :abbrev abbrev-repo)))

(defun tlon-babel-ogh-get-issue-number-from-open-issues ()
  "Prompt user to select from a list of open issues and return number of selection."
  (let* ((default-directory (tlon-babel-get-repo nil 'include-all))
	 (repo (forge-get-repository 'full))
	 ;; Fetch all issues, but filter for open ones
	 (issue-list (mapcar #'(lambda (issue)
				 (cons (format "#%d %s"
					       (oref issue number)
					       (oref issue title))
				       (oref issue number)))
			     (cl-remove-if-not (lambda (issue)
						 (string= (oref issue state) "open"))
					       (oref repo issues))))
	 ;; Let the user select one
	 (selected-issue (cdr (assoc (completing-read "Select an issue: " issue-list) issue-list))))
    ;; Return the selected issue number
    selected-issue))

(defun tlon-babel-ogh-get-issues (&optional repo)
  "Return a list of all open issues in REPO.
If REPO is nil, use the current repository."
  (let* ((repo (or repo (forge-get-repository t)))
	 (issues (forge-ls-issues repo)))
    issues))

(defun tlon-babel-ogh-get-latest-issue (&optional repo)
  "Return the most recently created issue in REPO.
If REPO is nil, use the current repository."
  (let* ((issues (tlon-babel-ogh-get-issues repo))
	 (latest-issue (car (sort issues (lambda (a b)
					   (time-less-p
					    (date-to-time (oref b created))
					    (date-to-time (oref a created))))))))
    (list (oref latest-issue number) (oref latest-issue title))))

(defun tlon-babel-ogh-count-issues (&optional repo)
  "Return the number of open issues in REPO.
If REPO is nil, use the current repository."
  (length (tlon-babel-ogh-get-issues repo)))

;;;;; Set heading elements

(defun tlon-babel-ogh-set-repo-in-heading ()
  "Set the repo in the heading at point if not already present."
  (when (and (org-at-heading-p)
	     (not (tlon-babel-ogh-get-repo-from-heading)))
    (let* ((repo-name (completing-read "Select repo: " (tlon-babel-get-property-of-repos :name)))
	   (abbrev-repo (tlon-babel-repo-lookup :abbrev :name repo-name)))
      (org-extras-goto-beginning-of-heading-text)
      (insert (format "[%s] " abbrev-repo)))))

(defun tlon-babel-ogh-set-issue-number-in-heading (issue-number)
  "Set ISSUE-NUMBER in heading at point if not already present."
  (unless (tlon-babel-ogh-get-issue-number-from-heading)
    (org-extras-goto-beginning-of-heading-text)
    ;; move past repo name
    (re-search-forward "\\[.+?\\] ")
    (insert (format "#%s " (number-to-string issue-number)))))


;;;;; Close issues/todo

;;;###autoload
(defun tlon-babel-ogh-close-issue-and-todo ()
  "With point on either, close issue and associated TODO."
  (interactive)
  (tlon-babel-ogh-todo-issue-funcall
   #'tlon-babel-ogh-close-issue-and-todo-from-issue
   (lambda ()
     (tlon-babel-ogh-visit-counterpart)
     (tlon-babel-ogh-close-issue-and-todo-from-issue))))

(defun tlon-babel-ogh-close-issue-and-todo-from-issue ()
  "With point on issue, close issue and associated TODO."
  (let ((issue-number (tlon-babel-ogh-get-issue-number-from-heading))
	(repo (tlon-babel-ogh-get-repo-from-heading)))
    (tlon-babel-ogh-close-issue-number issue-number repo)
    (tlon-babel-ogh-visit-todo)
    (org-todo "DONE")
    (message "Closed issue and TODO.")))

;; shouldn’t this be done using the orgit-link rather than issue-number?
(defun tlon-babel-ogh-close-issue-number (issue-number repo)
  "Close the issue with ISSUE-NUMBER in REPO."
  (tlon-babel-ogh-visit-issue issue-number repo)
  (tlon-babel-ogh-close-issue))

;;;;; Set TODO statuses

(defun tlon-babel-ogh-get-issue-status (&optional issue)
  "Get remote status of ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (if (eq (tlon-babel-ogh-get-state) 'closed)
	"DONE"
      (if-let ((label (tlon-babel-ogh-get-label issue)))
	  (upcase label)
	""))))

(defun tlon-babel-ogh-get-corresponding-label ()
  "Get TODO status for `org-mode' heading at point from corresponding issue."
  (save-window-excursion
    (tlon-babel-ogh-visit-issue)
    (tlon-babel-ogh-get-label)))

(defun tlon-babel-ogh-is-valid-status-p (&optional status issue)
  "Return t iff STATUS it is a valid TODO status.
A status is valid iff it is a member of `tlon-babel-ogh-todo-statuses'. If
STATUS is nil, use the status of heading or issue at point.

If ISSUE is nil, use the issue at point or in the current buffer."
  (if-let ((status (or status (pcase major-mode
				('org-mode (org-get-todo-state))
				((or 'forge-topic-mode 'forge-issue-mode 'forge-issue-list-mode 'magit-status-mode)
				 (tlon-babel-ogh-get-label issue))))))
      (when (or (member status tlon-babel-ogh-todo-statuses)
		(member status (mapcar #'downcase tlon-babel-ogh-todo-statuses)))
	t)
    nil))

;;;;; Re-sync

;;;###autoload
(defun tlon-babel-ogh-reconcile-issue-and-todo ()
  "With point on either, reconcile issue and associated TODO."
  (interactive)
  (tlon-babel-ogh-todo-issue-funcall
   (lambda ()
     (with-current-buffer (tlon-babel-ogh-get-issue-buffer)
       (tlon-babel-ogh-reconcile-issue-and-todo-from-issue)))
   #'tlon-babel-ogh-reconcile-issue-and-todo-from-issue))

;;;###autoload
(defun tlon-babel-ogh-reconcile-all-issues-and-todos ()
  "Reconcile all TODOs under `tlon-babel-todos-generic-id'."
  (interactive)
  (save-window-excursion
    (org-roam-id-open tlon-babel-todos-generic-id nil)
    (let ((level (org-current-level)))
      (call-interactively 'org-next-visible-heading)
      (while (> (org-current-level) level)
	(if (or (not (tlon-babel-get-issue))
		(member org-archive-tag (org-get-tags)))
	    (org-next-visible-heading 1)
	  (tlon-babel-ogh-reconcile-issue-and-todo)
	  (call-interactively 'org-next-visible-heading))))
    (message "Finished reconciling.")))

(defun tlon-babel-ogh-reconcile-issue-and-todo-from-issue ()
  "With point on issue, reconcile issue and associated TODO-NAME."
  (let ((issue-name (tlon-babel-ogh-make-todo-name-from-issue))
	(pos (tlon-babel-ogh-get-todo-position-from-issue)))
    (save-window-excursion
      (tlon-babel-ogh-visit-todo pos)
      (let ((todo-name (substring-no-properties (org-get-heading t nil t t))))
	(unless (string= issue-name todo-name)
	  (tlon-babel-ogh-reconcile-issue-and-todo-prompt issue-name todo-name))))))

(defun tlon-babel-ogh-reconcile-issue-and-todo-prompt (issue-name todo-name)
  "Prompt the user to reconcile discrepancies between ISSUE-NAME and TODO-NAME."
  (pcase (read-char-choice
	  (format "The issue differs from its todo. Keep (i)ssue | Keep (t)odo | (a)bort\nissue: `%s'\ntodo:  `%s' "
		  issue-name todo-name)
	  '(?i ?t ?a))
    (?i (tlon-babel-ogh-update-todo-from-issue issue-name))
    (?t (tlon-babel-ogh-update-issue-from-todo todo-name))
    (_ (user-error "Aborted"))))

(defun tlon-babel-ogh-update-todo-from-issue (issue-name)
  "Update TODO to match ISSUE-NAME."
  (let ((original-visual-line-mode visual-line-mode))
    (visual-line-mode -1)
    (save-window-excursion
      (beginning-of-line)
      (re-search-forward " ")
      (let ((tags (org-get-tags nil t)))
	(org-fold-show-subtree)
	(org-kill-line)
	(insert issue-name)
	(when tags
	  (org-set-tags tags)
	  (org-align-tags))
	(message "TODO updated"))
      (visual-line-mode original-visual-line-mode))))

(defun tlon-babel-ogh-update-issue-from-todo (_)
  "Update ISSUE to match TODO-NAME."
  (user-error "This command has not yet been developed. Sorry!"))
;; TODO: Develop function. The below approach doesn't work because
;; `org-complex-heading-regexp' fails to match a heading

;; (string-match org-complex-heading-regexp (concat "* " todo-name))
;; (let ((title (match-string-no-properties 4))
;; (state (match-string-no-properties 2)))
;; (message "%s %s" title state)))

;;;;; Change issue properties

(defun tlon-babel-ogh-set-property (property type &optional issue)
  "Set PROPERTY of TYPE in ISSUE.
If ISSUE is nil, use issue at point or in the current buffer."
  (interactive
   (list ))
  (let* ((fun (pcase type
		('label #'forge--set-topic-labels)
		('assignee #'forge--set-topic-assignees)
		(_ (user-error "Property type `%s' not recognized" type))))
	 (issue (or issue (forge-get-topic (forge-current-topic))))
	 (repo (forge-get-repository issue))
	 (crm-separator ","))
    (funcall fun repo issue (list property))))

;; TODO: Cleanup the three functions below
(defun tlon-babel-ogh-set-label (&optional label issue)
  "Apply LABEL to ISSUE.
If ISSUE is nil, use issue at point or in the current buffer."
  (interactive)
  (let* ((issue (or issue (forge-get-topic (forge-current-topic))))
	 (label (or label (if (tlon-babel-ogh-issue-is-job-p issue)
			      (tlon-babel-ogh-set-job-label)
			    (tlon-babel-ogh-set-status-label)))))
    (tlon-babel-ogh-set-property label 'label issue)))

(defun tlon-babel-ogh-set-job-label ()
  "Prompt the user to select a job label."
  (let ((label (completing-read "What should be the label? "
				(tlon-babel-get-property-of-labels :label))))
    label))

(defun tlon-babel-ogh-set-status-label ()
  "Prompt the user to select a status label."
  (let ((label (completing-read "TODO status? " (mapcar #'downcase tlon-babel-ogh-todo-statuses) nil t)))
    label))

(defun tlon-babel-ogh-set-assignee (assignee &optional issue)
  "Make ASSIGNEE the assignee of ISSUE.
If ISSUE is nil, use issue at point or in the current buffer."
  (interactive
   (list (tlon-babel-ogh-select-assignee)))
  (let ((issue (or issue (forge-get-topic (forge-current-topic)))))
    (tlon-babel-ogh-set-property assignee 'assignee issue)))

(defun tlon-babel-ogh-select-assignee ()
  "Prompt the user to select an ASSIGNEE.
The prompt defaults to the current user."
  (let ((assignee (completing-read "Who should be the assignee? "
				   (tlon-babel-get-property-of-users :github) nil nil
				   (tlon-babel-user-lookup :github :name user-full-name))))
    assignee))

(defun tlon-babel-ogh-set-initial-label-and-assignee ()
  "Set label to `Awaiting processing' and assignee to current user."
  (tlon-babel-ogh-set-label "Awaiting processing")
  (tlon-babel-ogh-set-assignee (tlon-babel-user-lookup :github :name user-full-name)))

(defun tlon-babel-ogh-get-element (element &optional issue)
  "Return ELEMENT of ISSUE.
If the issue has more than one element, return the first. If ISSUE is nil, use
the issue at point or in the current buffer."
  (when-let ((issue (or issue (forge-current-topic))))
    (caar (closql--iref issue element))))

(defun tlon-babel-ogh-get-assignee (&optional issue)
  "Return the assignee of the current ISSUE.
If the issue has more than one assignee, return the first. If ISSUE is nil, use
the issue at point or in the current buffer."
  (tlon-babel-ogh-get-element 'assignees issue))

(defun tlon-babel-ogh-get-label (&optional issue)
  "Return the label of the issue at point.
If the issue has more than one label, return the first. If ISSUE is nil, use the
issue at point or in the current buffer."
  (tlon-babel-ogh-get-element 'labels issue))

(defun tlon-babel-ogh-get-state (&optional issue)
  "Return state of ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (when-let ((issue (or issue (forge-current-topic))))
    (oref (forge-current-topic) state)))

;;;;; ?

(defun tlon-babel-ogh-get-issue-name (&optional issue)
  "Get the name of ISSUE.
An issue name is its number followed by its title.

If ISSUE is nil, get the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (title (oref issue title))
	 (number (oref issue number)))
    (format "#%s %s" number title)))

(defun tlon-babel-ogh-get-issue-link (&optional issue)
  "Get an `org-mode' link to ISSUE.
If ISSUE is nil, get the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (name (tlon-babel-ogh-get-issue-name issue))
	 (id (oref issue id)))
    (org-link-make-string (format "orgit-topic:%s" id) name)))

(defun tlon-babel-ogh-make-todo-name-from-issue (&optional no-action no-state issue)
  "Construct the name of TODO from ISSUE.
For job TODOs, the resulting name will have a name with the form \"[REPO] ACTION
NAME\". ACTION is optional, and used only for job TODOs. For example, if the
TODO is \"[uqbar-es] #591 Job: `Handbook2022ExerciseForRadical`\", and ACTION is
\"Process\", the function returns \"[uqbar-es] Process #591 Job:
`Handbook2022ExerciseForRadical`\".

If NO-ACTION is non-nil, omit, the ACTION element. If NO-STATE is non-nil, omit
the STATE element. If ISSUE is nil, use the issue at point or in the current
buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (action (if (and (tlon-babel-ogh-issue-is-job-p issue)
			  (not no-action))
		     (or (tlon-babel-label-lookup :action :label (tlon-babel-ogh-get-label issue))
			 "")
		   ""))
	 (state (if (tlon-babel-ogh-issue-is-job-p issue)
		    "TODO"
		  (tlon-babel-ogh-get-issue-status issue)))
	 (repo-abbrev (tlon-babel-repo-lookup :abbrev :dir (tlon-babel-get-repo 'error 'include-all)))
	 (todo-name (replace-regexp-in-string
		     "[[:space:]]\\{2,\\}"
		     " "
		     (concat
		      (unless no-state (format "%s " state))
		      (format "[%s] %s %s" repo-abbrev action (tlon-babel-ogh-get-issue-link issue))))))
    todo-name))

(defun tlon-babel-ogh-get-file-from-issue (&optional issue)
  "Get the file path of ISSUE.
If ISSUE is nil, use the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (name (tlon-babel-ogh-get-issue-name issue)))
    (if (string-match tlon-babel-key-regexp name)
	(tlon-babel-get-file-from-key (match-string 1 name))
      (user-error "I wasn't able to find a file at point or in the forge buffer"))))

;;;###autoload
(defun tlon-babel-ogh-open-forge-file ()
  "Open the file of the issue at point or in the current buffer."
  (interactive)
  (find-file (tlon-babel-ogh-get-file-from-issue)))

;;;###autoload
(defun tlon-babel-ogh-open-forge-counterpart ()
  "Open the file counterpart of the issue at point or in the current buffer."
  (interactive)
  (tlon-babel-open-counterpart nil (tlon-babel-ogh-get-file-from-issue)))

;;;;

(defun tlon-babel-ogh-create-issue (title &optional repo body)
  "Create new GitHub issue in REPO with TITLE and BODY."
  (let* ((repo (or repo (tlon-babel-get-repo 'error 'include-all)))
	 (body (or body ""))
	 (default-directory repo)
	 (repo (forge-get-repository t))
	 (owner (oref repo owner))
	 (reponame (oref repo name))
	 (resource (format "/repos/%s/%s/issues" owner reponame))
	 (data `(("title" . ,title)
		 ("body" . ,body))))
    (ghub-post resource data
	       :auth 'forge
	       :noerror t ;; avoid showing the original large output
	       :reader 'ignore) ;; do not parse the response json
    (message "Created issue with title %s" title)))

;;;###autoload
(defun tlon-babel-ogh-create-issue-from-todo ()
  "Create a new GitHub issue based on the current `org-mode' heading."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "You need to be in `org-mode' to use this function"))
  (when (tlon-babel-ogh-get-issue-number-from-heading)
    (user-error "This heading already has an issue"))
  (unless (tlon-babel-ogh-is-valid-status-p)
    (user-error "Invalid TODO status"))
  (unless (tlon-babel-ogh-get-repo-from-heading)
    (tlon-babel-ogh-set-repo-in-heading))
  (let (todo-linkified)
    (save-excursion
      (let* ((default-directory (tlon-babel-ogh-get-repo-from-heading))
	     (heading (substring-no-properties (org-get-heading t t t t)))
	     (status (downcase (org-get-todo-state)))
	     (abbrev-repo (tlon-babel-repo-lookup :abbrev :dir default-directory))
	     (issue-title (substring heading (+ (length abbrev-repo) 3)))
	     (latest-issue-pre (car (tlon-babel-ogh-get-latest-issue)))
	     (latest-issue-post latest-issue-pre))
	(tlon-babel-ogh-create-issue issue-title default-directory)
	(forge-pull)
	(message (concat "Reflect on this fine proverb while you wait: " (tlon-core-proverb)))
	(while (eq latest-issue-pre latest-issue-post)
	  (sleep-for 0.1)
	  (setq latest-issue-post (car (tlon-babel-ogh-get-latest-issue))))
	(tlon-babel-ogh-set-issue-number-in-heading latest-issue-post)
	(tlon-babel-ogh-visit-issue)
	(tlon-babel-ogh-set-assignee (tlon-babel-user-lookup :github :name user-full-name))
	(tlon-babel-ogh-set-label status)
	(setq todo-linkified (tlon-babel-ogh-make-todo-name-from-issue nil 'no-state))))
    (org-edit-headline todo-linkified)))

(defun tlon-babel-ogh-create-issue-or-todo ()
  "Create issue from TODO or vice versa."
  (interactive)
  (tlon-babel-ogh-todo-issue-funcall #'tlon-babel-ogh-create-issue-from-todo
				     #'tlon-babel-ogh-capture-issue))

(defun tlon-babel-ogh-create-issue-from-key (&optional key)
  "Create an issue based on KEY.
If KEY is not provided, the key in the Markdown buffer at point is used."
  (let ((default-directory (tlon-babel-get-repo 'error))
	(key (or key (tlon-babel-get-key-in-buffer))))
    (tlon-babel-ogh-create-issue (format "Job: `%s`" key) default-directory)))

(defun tlon-babel-ogh-issue-lookup (string &optional repo)
  "Return the first issue in REPO whose title includes STRING.
If REPO is nil, use the current repository."
  (let* ((string (concat "%" string "%"))
	 (repo (or repo (forge-get-repository nil)))
	 (issue-id (caar (emacsql (forge-db)
				  [:select [number]
					   :from 'issue
					   :where (and (= repository $s1)
						       (like title $s2))]
				  (oref repo id)
				  string))))
    (when issue-id
      (forge-get-issue repo issue-id))))

(defun tlon-babel-ogh-close-issue (&optional issue)
  "Close ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (repo (forge-get-repository issue)))
    (when (eq 'open (oref issue state))
      (forge--set-topic-state repo issue 'closed))))

(defun tlon-babel-ogh-get-parent-todo (todo)
  "Get parent of TODO in `tlon-babel-todos-jobs-file'."
  (let ((pos (tlon-babel-ogh-get-todo-position todo (tlon-babel-ogh-get-todos-jobs-file))))
    (save-window-excursion
      (tlon-babel-ogh-visit-todo pos (tlon-babel-ogh-get-todos-jobs-file))
      (widen)
      (org-up-heading-safe)
      (org-no-properties (org-get-heading)))))

(defun tlon-babel-ogh-mark-todo-done (todo file)
  "Mark TODO in FILE as DONE."
  (let ((pos (tlon-babel-ogh-get-todo-position todo file)))
    (save-window-excursion
      (tlon-babel-ogh-visit-todo pos)
      (org-todo "DONE")
      (save-buffer)
      (message "Marked `%s' as DONE" todo))))

(provide 'tlon-babel-ogh)
;;; tlon-babel-ogh.el ends here
