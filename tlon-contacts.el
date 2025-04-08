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

(eval-and-compile
  (defconst tlon-contacts-property-values
    `(("EMAIL" :shortcut "m" :candidates nil)
      ("URL" :shortcut "u" :candidates nil)
      ("GENDER" :shortcut "g" :candidates ("male" "female" "other" "not applicable"))
      ("ENTITY" :shortcut "t" :candidates ("living person" "deceased person" "non-person"))
      ("UNIVERSAL-CONSENT" :shortcut "s" :candidates ("yes" "no" "waiting" "unknown"))
      ("LANGUAGE" :shortcut "l" :candidates ,(tlon-get-language-candidates 'babel))
      ("X" :shortcut "x" :candidates nil))
    "List of (PROPERTY :shortcut SHORTCUT :candidates COMPLETION-LIST)."))

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
        ;; Retrieve the property’s entry from `tlon-contacts-property-values'
        (let* ((entry (assoc prop tlon-contacts-property-values #'string=))
               (candidates (and entry (plist-get (cdr entry) :candidates)))
               (current-value (org-entry-get (point) prop))
               (new-value
                (if (and candidates (listp candidates))
                    (completing-read (format "%s: " prop) candidates
                                     nil nil current-value)
                  (read-string (format "%s: " prop) current-value))))
          (org-set-property prop new-value)))
      (save-buffer))))

(defun tlon-contacts-get-property-value (&optional prop)
  "Return the value of the property PROP of the contact at point.
If PROP is nil, prompt the user to select a property."
  (let* ((props (tlon-contacts-get-nonempty-properties))
         (prop (or prop (completing-read "Property: " props nil t))))
    (tlon-ensure-org-mode)
    (alist-get prop props nil nil 'string=)))

;;;###autoload
(defun tlon-contacts-copy-property-value (&optional prop)
  "Copy the value of the property PROP of the contact at point to the kill ring.
If PROP is nil, prompt the user to select a property."
  (interactive)
  (let ((value (tlon-contacts-get-property-value prop)))
    (kill-new (or value ""))
    (message "Copied %s to the kill ring." value)))

(declare-function org-contacts "org-contacts")
(defun tlon-contacts-get-property-value-of-contact (&optional prop)
  "Prompt the user to select a contact and get value of PROP.
If PROP is nil, prompt the user to select a property."
  (org-contacts)
  ;; Ensure tlon-contacts-file is set before using it.
  (with-current-buffer (find-buffer-visiting (tlon-contacts-get-file))
    (tlon-contacts-get-property-value prop)))

(defun tlon-contacts-get-nonempty-properties ()
  "Return an association list of the nonempty properties of the contact at point."
  (let ((role (tlon-contacts-get-role)))
    (mapcar (lambda (prop)
              (cons prop (org-entry-get (point) prop)))
            (alist-get role tlon-contacts-properties nil nil #'string=))))

(declare-function org-get-heading "org")
(defun tlon-contacts-get-contact-name ()
  "Get the first and last name contact at point.
Return a cons cell with the name and email address."
  (with-current-buffer (find-buffer-visiting tlon-contacts-file)
    (cl-destructuring-bind (last first)
        (split-string (substring-no-properties (org-get-heading t t t t)) ", ")
      (cons first last))))

;;;;; Commands to edit specific properties

(defun tlon-contacts-edit-one-property (property)
  "Edit PROPERTY of the contact at point with completion if available.
PROPERTY must be a string that appears in `tlon-contacts-property-values'."
  (interactive
   (list (completing-read
          "Property to edit: "
          (mapcar #'car tlon-contacts-property-values)
          nil t)))
  (tlon-ensure-org-mode)
  (let* ((entry (assoc property tlon-contacts-property-values #'string=))
         (candidates (and entry (plist-get (cdr entry) :candidates)))
         (current-value (org-entry-get (point) property))
         (new-value
          (if (and candidates (listp candidates))
              (completing-read (format "%s: " property)
                               candidates nil nil current-value)
            (read-string (format "%s: " property) current-value))))
    (org-set-property property new-value)
    (save-buffer)
    (message "Set %s to %s" property new-value)))

(defmacro tlon-contacts-define-property-edit-commands ()
  "Dynamically create commands + transient menu entries for each property."
  `(progn
     ;; 1) Define one edit command for each property
     ,@(cl-loop
	for (prop . plist) in tlon-contacts-property-values
	for cmd   = (intern (concat "tlon-contacts-edit-" (downcase prop)))
	collect
	`(defalias ',cmd
           (lambda ()
             ,(format "Edit the %s property of the contact at point." prop)
             (interactive)
             (tlon-contacts-edit-one-property ,prop))
           ,(format "Edit the %s property of the contact at point." prop)))

     ;; 2) Define/override the Transient menu to include each property’s shortcut
     ;;    plus your existing commands
     (transient-define-prefix tlon-contacts-menu ()
       "Menu for Tlön contacts management."
       [:description "Tlön Contacts"
		     [""
		      ("S" "search"               org-contacts)
		      ("c" "create"               tlon-contacts-create)
		      ("e" "edit properties"      tlon-contacts-edit-properties)
		      ("y" "copy property"        tlon-contacts-copy-property-value)]
		     ;; Splice in one suffix per property
		     ["edit"
		      ,@(cl-loop
			 for (prop . plist) in tlon-contacts-property-values
			 for shortcut = (plist-get plist :shortcut)
			 do (unless shortcut
			      (error "Property %S has no :shortcut" prop))
			 collect
			 `(,shortcut ,(format "%s" (downcase prop))
				     ,(intern (concat "tlon-contacts-edit-" (downcase prop)))))]])))

;; Execute the macro so commands + menu are actually defined.
(tlon-contacts-define-property-edit-commands)

;;;;; Roles

(declare-function org-narrow-to-subtree "org")
(defun tlon-contacts-get-role ()
  "Return the \"ROLE\" property of the contact at point."
  (org-entry-get (point) "ROLE"))

(defun tlon-contacts-select-role ()
  "Select a value for the \"ROLE\" from a list of available roles."
  (completing-read "Role: " (mapcar #'car tlon-contacts-properties)))

(provide 'tlon-contacts)
;;; tlon-contacts.el ends here

