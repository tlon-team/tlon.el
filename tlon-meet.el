;;; tlon-meet.el --- Manage Tlön meetings -*- lexical-binding: t; fill-column: 80 -*-

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

;; Manage Tlön meetings.

;;; Code:

(require 'org)
(require 'tlon-forg)

;;;; Functions

(defun tlon-create-or-visit-meeting-issue (&optional person-or-group)
  "Create or visit issue for a meeting with PERSON-OR-GROUP."
  (interactive)
  (let ((person-or-group (or person-or-group (tlon-prompt-for-all-other-users t))))
    (if (string= person-or-group "group")
	(tlon-create-or-visit-group-meeting-issue)
      (tlon-create-or-visit-individual-meeting-issue person-or-group))))

(defun tlon-create-or-visit-individual-meeting-issue (person &optional date)
  "Create or visit issue for a meeting with PERSON on DATE."
  (interactive (list (tlon-prompt-for-all-other-users)))
  (let* ((date (or date (org-read-date)))
	 (dir (tlon-get-meeting-repo person user-full-name)))
    (tlon-create-or-visit-meeting-issue-date date dir)))

(defun tlon-create-or-visit-group-meeting-issue (&optional date)
  "Create or visit issue for a group meeting on DATE."
  (interactive)
  (let* ((date (or date (org-read-date)))
	 (dir (tlon-repo-lookup :dir :name "meetings-group")))
    (tlon-create-or-visit-meeting-issue-date date dir)))

(defun tlon-create-or-visit-meeting-issue-date (date dir)
  "Create or visit issue in DIR for a meeting on DATE."
  (let ((default-directory dir))
    (tlon-wait-until-forge-updates)
    (if-let ((issue (tlon-issue-lookup date dir)))
	(forge-visit-issue issue)
      (tlon-create-and-visit-issue date dir))))

(defun tlon-wait-until-forge-updates ()
  "Wait until Forge is finished updating the repo."
  (let* ((get-last-updated (lambda () (oref (forge-get-repository :tracked?) updated)))
	 (last-updated (funcall get-last-updated))
	 (count 0))
    (forge-pull)
    (while (and (< count 25) (string= last-updated (funcall get-last-updated)))
      (sleep-for 0.1)
      (setq count (1+ count)))))

;; TODO: generate the next three functions with function
;;;###autoload
(defun tlon-create-or-visit-meeting-issue-leo-pablo (&optional date)
  "Create or visit issue for a meeting with Leo and Pablo on DATE."
  (interactive)
  (let ((date (or date (org-read-date)))
	(person (pcase user-full-name
		  ("Pablo Stafforini" "Leonardo Picón")
		  ("Leonardo Picón" "Pablo Stafforini")
		  (_ (user-error "This command is only for Leo and Pablo meetings")))))
    (tlon-create-or-visit-individual-meeting-issue person date)))

;;;###autoload
(defun tlon-create-or-visit-meeting-issue-fede-pablo (&optional date)
  "Create or visit issue for a meeting with Fede and Pablo on DATE."
  (interactive)
  (let ((date (or date (org-read-date)))
	(person (pcase user-full-name
		  ("Pablo Stafforini" "Federico Stafforini")
		  ("Federico Stafforini" "Pablo Stafforini")
		  (_ (user-error "This command is only for Fede and Pablo meetings")))))
    (tlon-create-or-visit-individual-meeting-issue person date)))

;;;###autoload
(defun tlon-create-or-visit-meeting-issue-fede-leo (&optional date)
  "Create or visit issue for a meeting with Fede and Leo on DATE."
  (interactive)
  (let ((date (or date (org-read-date)))
	(person (pcase user-full-name
		  ("Federico Stafforini" "Leonardo Picón")
		  ("Leonardo Picón" "Federico Stafforini")
		  (_ (user-error "This command is only for Leo and Fede meetings")))))
    (tlon-create-or-visit-individual-meeting-issue person date)))

;;;###autoload
(defun tlon-advice-org-clock-in-meetings (&optional _ _)
  "Call appropriate meeting function based on heading pattern."
  (let ((heading (org-get-heading t t t t)))
    (cond
     ((string-match "Leo<>Pablo" heading)
      (tlon-create-or-visit-meeting-issue-leo-pablo (format-time-string "%Y-%m-%d")))
     ((string-match "Fede<>Pablo" heading)
      (tlon-create-or-visit-meeting-issue-fede-pablo (format-time-string "%Y-%m-%d")))
     ((string-match "Group meeting" heading)
      (tlon-create-or-visit-group-meeting-issue (format-time-string "%Y-%m-%d"))))))

(defun tlon-prompt-for-all-other-users (&optional group)
  "Ask the user to select from a list of all users except himself.
If GROUP is non-nil, include the \"group\" option in the prompt."
  (completing-read "Person: "
		   (let ((people
			  (cl-remove-if (lambda (user)
					  (string= user user-full-name))
					(tlon-user-lookup-all :name))))
		     (if group
			 (append people '("group"))
		       people))))

;; TODO: create `tlon-issue-lookup-all', analogous to `tlon-lookup-all'
(defun tlon-get-meeting-repo (participant1 participant2)
  "Get directory of meeting repo for PARTICIPANT1 and PARTICIPANT2."
  (catch 'found
    (dolist (repo tlon-repos)
      (when (and
	     (eq 'meetings (plist-get repo :subtype))
	     (member participant1 (plist-get repo :participants))
	     (member participant2 (plist-get repo :participants)))
	(throw 'found (plist-get repo :dir))))))

(defun tlon-create-and-visit-issue (title dir)
  "Create an issue with TITLE in DIR and visit it."
  (with-temp-buffer
    (cd dir)
    (when (forge-get-repository :tracked?)
      (tlon-create-issue title dir)
      (forge-pull)
      (while (not (tlon-issue-lookup title dir))
	(sleep-for 0.1))
      (forge-visit-issue (tlon-issue-lookup title dir))
      (format "*forge: %s %s*"
	      (oref (forge-get-repository :tracked?) slug)
	      (oref (forge-current-topic) slug)))))

;; TODO: generalize to all possible meetings
(defun tlon-discuss-issue-in-meeting ()
  "Create a reminder to discuss the current issue in a meeting.
We should try to follow the rule of avoiding prolonged discussions in the issue
tracker, and instead conduct these discussions in person or over a call. This
function tried to be a nudge in that direction."
  (interactive)
  (unless (derived-mode-p 'forge-issue-mode)
    (user-error "This command can only be invoked in Forge issue buffers"))
  (let (backlink)
    (save-excursion
      (let* ((repo-name (oref (forge-get-repository :tracked?) name))
	     (issue-number (oref (forge-current-issue) number))
	     (link (format "tlon-team/%s#%s" repo-name issue-number)))
	(switch-to-buffer (tlon-create-or-visit-meeting-issue))
	(let* ((repo-name (oref (forge-get-repository :tracked?) name))
	       (issue-number (oref (forge-current-issue) number)))
	  (setq backlink (format "tlon-team/%s#%s" repo-name issue-number))
	  (goto-char (point-max))
	  (forward-line -1)
	  (forge-edit-post)
	  (while (not (derived-mode-p 'forge-post-mode))
	    (sleep-for 0.1))
	  (goto-char (point-max))
	  (insert (format "- Discutir %s." link))
	  (forge-post-submit))))
    (forge-create-post)
    (while (not (derived-mode-p 'forge-post-mode))
      (sleep-for 0.1))
    (insert (format "A discutir en %s." backlink))
    (forge-post-submit)))


;;;;; Menu

;;;###autoload (autoload 'tlon-meet-menu "tlon-meet.el" nil t)
(transient-define-prefix tlon-meet-menu ()
  "`meet' menu."
  ["Meetings"
   ("l p" "Leo-Pablo"                  tlon-create-or-visit-meeting-issue-leo-pablo)
   ("f p" "Fede-Pablo"                 tlon-create-or-visit-meeting-issue-fede-pablo)
   ("f l" "Fede-Leo"                   tlon-create-or-visit-meeting-issue-fede-leo)
   ("g"   "group"                      tlon-create-or-visit-group-meeting-issue)
   ("i"    "discuss issue in meeting"  tlon-discuss-issue-in-meeting)])

(provide 'tlon-meet)
;;; tlon-meet.el ends here

