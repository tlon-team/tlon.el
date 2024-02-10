;;; tlon-babel-meet.el --- Manage Tlön meetings -*- lexical-binding: t -*-

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

;; Manage Tlön meetings.

;;; Code:

(require 'org)
(require 'tlon-babel-forg)

;;;; Functions

(defun tlon-babel-create-or-visit-meeting-issue (person date)
  "Create or visit issue for a meeting with PERSON on DATE."
  (interactive (list (tlon-babel-prompt-for-all-other-users)
		     (org-read-date)))
  (let* ((dir (tlon-babel-get-meeting-repo person user-full-name)))
    (if-let ((issue (tlon-babel-issue-lookup date dir)))
	(forge-visit-issue issue)
      (tlon-babel-create-and-visit-issue date dir))))

;; TODO: generate the next three functions with macro
;;;###autoload
(defun tlon-babel-create-or-visit-meeting-issue-leo-pablo ()
  "Create or visit issue for a meeting with Leo and Pablo."
  (interactive)
  (let ((person (pcase user-full-name
		  ("Pablo Stafforini" "Leonardo Picón")
		  ("Leonardo Picón" "Pablo Stafforini")
		  (_ (user-error "This command is only for Leo and Pablo meetings")))))
    (tlon-babel-create-or-visit-meeting-issue person (org-read-date))))

;;;###autoload
(defun tlon-babel-create-or-visit-meeting-issue-fede-pablo ()
  "Create or visit issue for a meeting with Fede and Pablo."
  (interactive)
  (let ((person (pcase user-full-name
		  ("Pablo Stafforini" "Federico Stafforini")
		  ("Federico Stafforini" "Pablo Stafforini")
		  (_ (user-error "This command is only for Fede and Pablo meetings")))))
    (tlon-babel-create-or-visit-meeting-issue person (org-read-date))))

;;;###autoload
(defun tlon-babel-create-or-visit-meeting-issue-fede-leo ()
  "Create or visit issue for a meeting with Fede and Leo."
  (interactive)
  (let ((person (pcase user-full-name
		  ("Federico Stafforini" "Leonardo Picón")
		  ("Leonardo Picón" "Federico Stafforini")
		  (_ (user-error "This command is only for Leo and Fede meetings")))))
    (tlon-babel-create-or-visit-meeting-issue person (org-read-date))))

(defun tlon-babel-prompt-for-all-other-users ()
  "Ask the user to select from a list of all users except himself."
  (completing-read "Person: "
		   (cl-remove-if (lambda (user)
				   (string= user user-full-name))
				 (tlon-babel-user-lookup-all :name))))

;; TODO: create `tlon-babel-issue-lookup-all', analogous to `tlon-babel-lookup-all'
(defun tlon-babel-get-meeting-repo (participant1 participant2)
  "Get directory of meeting repo for PARTICIPANT1 and PARTICIPANT2."
  (catch 'found
    (dolist (repo tlon-babel-repos)
      (when (and
	     (eq 'meetings (plist-get repo :subtype))
	     (member participant1 (plist-get repo :participants))
	     (member participant2 (plist-get repo :participants)))
	(throw 'found (plist-get repo :dir))))))

(defun tlon-babel-create-and-visit-issue (title dir)
  "Create an issue with TITLE in DIR and visit it."
  (with-temp-buffer
    (cd dir)
    (when (forge-current-repository)
      (tlon-babel-create-issue title dir)
      (forge-pull)
      (message (concat "Reflect on this fine proverb while you wait: " (tlon-core-proverb)))
      (while (not (tlon-babel-issue-lookup title dir))
	(sleep-for 0.1))
      (forge-visit-issue (tlon-babel-issue-lookup title dir)))))

;;;;; Menu

(transient-define-prefix tlon-babel-meet-menu ()
  "`meet' menu."
  ["Meetings"
   ("l" "Leo-Pablo"                  tlon-babel-create-or-visit-meeting-issue-leo-pablo)
   ("f" "Fede-Pablo"                 tlon-babel-create-or-visit-meeting-issue-fede-pablo)
   ("m" "Fede-Leo"                   tlon-babel-create-or-visit-meeting-issue-fede-leo)])

(provide 'tlon-babel-meet)
;;; tlon-babel-meet.el ends here

