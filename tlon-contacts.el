;;; tlon-contacts.el --- Contacts management -*- lexical-binding: t -*-

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

;; Contacts management.

;;; Code:

(require 'tlon-core)
(require 'transient)

;;;; Variables

(defconst tlon-contacts-id "F874E332-47AF-436F-997E-1A6791DEE0BE"
  "`org-mode' ID of the contacts file.")

(defvar tlon-contacts-file nil
  "File where contacts are stored.
This variable should not be set manually.")

(defconst tlon-contacts-properties
  '(("creator" . ("EMAIL" "URL" "GENDER" "ENTITY" "UNIVERSAL-CONSENT"))
    ("translator" . ("EMAIL" "GENDER" "LANGUAGE")))
  "Association list of roles and `org-mode' properties.")

(defconst tlon-contacts-property-values
  `(("EMAIL" . nil)
    ("URL" . nil)
    ("GENDER" . ("male" "female" "other" "not applicable"))
    ("ENTITY" . ("living person" "deceased person" "non-person"))
    ("UNIVERSAL-CONSENT" . ("yes" "no" "waiting" "unknown"))
    ("LANGUAGE" . ,(tlon-get-language-candidates 'babel)))
  "Association list of properties and completion values.")

;;;; Functions

(declare-function org-id-goto "org-id")
(declare-function org-end-of-subtree "org")
(declare-function org-insert-heading "org")
;;;###autoload
(defun tlon-contacts-create ()
  "Create a new contact entry."
  (interactive)
  (with-current-buffer (or (find-buffer-visiting (tlon-contacts-get-file))
			   (find-file-noselect (tlon-contacts-get-file)))
    (save-restriction
      (widen)
      (org-id-goto tlon-contacts-id)
      (org-end-of-subtree)
      (org-insert-heading nil nil 2)
      (tlon-contacts-insert-name)
      (tlon-contacts-edit-properties)
      (tlon-sort-headings (tlon-contacts-get-file)))))

(defun tlon-contacts-get-file ()
  "Get the file containing the contacts `org-mode' ID."
  (tlon-get-or-set-org-var 'tlon-contacts-file tlon-contacts-id))

(defun tlon-contacts-insert-name ()
  "Prompt the user for a name and insert it at point."
  (let ((first-name (read-string "First name: "))
	(last-name (read-string "Last name: ")))
    (insert (format "%s, %s" last-name first-name))
    (save-buffer)))

;;;;; Properties

(declare-function org-set-property "org")
(declare-function org-entry-get "org")
;;;###autoload
(defun tlon-contacts-edit-properties (&optional role)
  "Set properties for the contact at point.
If ROLE is nil and the entry lacks a \"ROLE\" property, prompt the user to
select one."
  (interactive)
  (tlon-ensure-org-mode)
  (let ((current-role (tlon-contacts-get-role)))
    (when (and role (not (string= role current-role)))
      (user-error "Role mismatch: %s vs %s" role current-role))
    (let ((role (or role current-role (tlon-contacts-select-role))))
      (unless current-role
	(org-set-property "ROLE" role))
      (dolist (prop (alist-get role tlon-contacts-properties nil nil #'string=))
	(let ((candidates (alist-get prop tlon-contacts-property-values nil nil #'string=)))
	  (org-set-property prop (completing-read (format "%s: " prop) candidates
						  nil nil (org-entry-get (point) prop)))))
      (save-buffer))))

(defun tlon-contacts-get-property-value (&optional prop)
  "Return the value of the property PROP of the contact at point.
If PROP is nil, prompt the user to select a property."
  (let* ((props (tlon-contacts-get-nonempty-properties))
	 (prop (or prop (completing-read "Property: " props))))
    (tlon-ensure-org-mode)
    (alist-get prop props nil nil 'string=)))

;;;###autoload
(defun tlon-contacts-copy-property-value (&optional prop)
  "Copy the value of the property PROP of the contact at point to the kill ring.
If PROP is nil, prompt the user to select a property."
  (interactive)
  (let ((value (tlon-contacts-get-property-value prop)))
    (kill-new value)
    (message "Copied %s to the kill ring." value)))

(declare-function org-contacts "org-contacts")
(defun tlon-contacts-get-property-value-of-contact (&optional prop)
  "Prompt the user to select a contact and get value of PROP.
If PROP is nil, prompt the user to select a property."
  (org-contacts)
  (with-current-buffer (find-buffer-visiting tlon-contacts-file)
    (tlon-contacts-get-property-value prop)))

(defun tlon-contacts-get-nonempty-properties ()
  "Return an association list of the nonempty properties of the contact at point."
  (let ((role (tlon-contacts-get-role)))
    (mapcar (lambda (prop)
	      (cons prop (org-entry-get (point) prop)))
	    (cdr (assoc role tlon-contacts-properties)))))

(declare-function org-get-heading "org")
(defun tlon-contacts-get-contact-name ()
  "Get the first and last name contact at point.
Return a cons cell with the name and email address."
  (with-current-buffer (find-buffer-visiting tlon-contacts-file)
    (cl-destructuring-bind (last first)
	(split-string (substring-no-properties (org-get-heading t t t t)) ", ")
      (cons first last))))

;;;;; Roles

(declare-function org-narrow-to-subtree "org")
(defun tlon-contacts-get-role ()
  "Return the \"ROLE\" property of the contact at point."
  (org-entry-get (point) "ROLE"))

(defun tlon-contacts-select-role ()
  "Select a value for the \"ROLE\" from a list of available roles."
  (completing-read "Role: " (mapcar #'car tlon-contacts-properties)))

;;;;; Menu

;;;###autoload (autoload 'tlon-contacts-menu "tlon-contacts" nil t)
(transient-define-prefix tlon-contacts-menu ()
  "Menu for Tlön contacts management."
  [("s" "search"               org-contacts)
   ("c" "create"               tlon-contacts-create)
   ("e" "edit properties"      tlon-contacts-edit-properties)
   ("y" "copy property"        tlon-contacts-copy-property-value)])

(provide 'tlon-contacts)
;;; tlon-contacts.el ends here

