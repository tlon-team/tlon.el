;;; tlon-email.el --- Send emails to coworkers and clients -*- lexical-binding: t -*-

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

;; Send emails to coworkers and clients.

;;; Code:

(require 'tlon-core)

;;;; Variables

(defconst tlon-email-templates-directory
  (file-name-concat (tlon-repo-lookup :dir :name "babel-core") "email-templates/")
  "Directory where email templates are stored.")

(defvar tlon-email-name nil)

(defvar tlon-email-language nil)

(defconst tlon-email-replacements
  `(("name" . ,tlon-email-name)
    ("language" . ,tlon-email-language))
  "List of replacements to be performed in email templates.
Each element is a cons cell where the car is the placeholder and the cdr is
either a string or the name of the variable whose value should replace
it.")

(defun tlon-email-unset-variables ()
  "Unset email variables."
  (setq tlon-email-language nil
	tlon-email-language nil))

;;;; Functions

(declare-function window-extras-switch-to-last-window "window-extras")
(declare-function org-msg-extras-begin-compose "org-msg-extras")
(declare-function mu4e-compose-new "mu4e-compose")
(declare-function tlon-contacts-get-property-value-of-contact "tlon-contacts")
(defun tlon-email-send (template &optional address attachment)
  "Send email to NAME based on TEMPLATE with optional ATTACHMENT with ADDRESS."
  (interactive (list (completing-read "Template: " (tlon-email-get-templates))))
  (let* ((address (or address (tlon-contacts-get-property-value-of-contact "EMAIL")))
	 (user-mail-address (concat (downcase (tlon-user-lookup :nickname :name user-full-name))
				    "@tlon.team")))
    (cl-destructuring-bind
	(subject . body) (tlon-email-get-template-content template)
      (mu4e-compose-new address)
      ;; ugly hack; otherwise the compose buffer is not set as current!
      (window-extras-switch-to-last-window)
      (window-extras-switch-to-last-window)
      (insert subject)
      (org-msg-extras-begin-compose)
      (insert body)
      ;; perform replacements
      (when attachment
	(mml-attach-file attachment))
      ;; (message-send-and-exit)
      )))

(defun tlon-email-get-templates ()
  "Return a list of available email templates."
  (directory-files tlon-email-templates-directory nil directory-files-no-dot-files-regexp ".*\\.org"))

(defun tlon-email-get-template-content (template)
  "Return the subject and body of the email TEMPLATE, as a cons cell."
  (let ((path (file-name-concat tlon-email-templates-directory template)))
    (with-current-buffer (or (find-buffer-visiting path)
			     (find-file-noselect path))
      (cons (string-trim (tlon-email-get-heading-contents "Subject"))
	    (tlon-email-get-heading-contents "Body")))))

(declare-function org-element-map "org-element")
(declare-function org-element-parse-buffer "org-element")
(declare-function org-element-property "org-element-ast")
(defun tlon-email-get-heading-contents (heading)
  "Return the contents of HEADING in the current `org-mode' buffer."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (hl)
      (when (string= (org-element-property :raw-value hl) heading)
	(let ((contents-begin (org-element-property :contents-begin hl))
	      (contents-end (org-element-property :contents-end hl)))
	  (when (and contents-begin contents-end)
	    (buffer-substring-no-properties contents-begin contents-end)))))
    nil 'first-match))

(defvar org-extras-id-auto-add-excluded-directories)
(add-to-list 'org-extras-id-auto-add-excluded-directories tlon-email-templates-directory)

(provide 'tlon-email)
;;; tlon-email.el ends here
