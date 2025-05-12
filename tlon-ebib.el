;;; tlon-ebib.el --- Integration with ebib -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/
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

;; Integration with ebib.

;;; Code:

(require 'tlon-core)
(require 'transient)

;;;; Variables

;;;;; Auth

(defvar tlon-ebib-auth-token nil
  "Authentication token for EA International API.")

(defvar tlon-ebib-auth-token-expiry nil
  "Expiry time for the authentication token.")

(defvar tlon-ebib-api-base-url "https://local-dev.ea.international"
  "Base URL for the EA International API.")

(defvar tlon-ebib-api-username
  (tlon-user-lookup :github :name user-full-name))

(defvar tlon-ebib-api-password (auth-source-pass-get 'secret (concat "tlon/core/ea.international/" tlon-ebib-api-username)))

;;;; Functions

(defun tlon-ebib-get-entries (&optional base-url)
  "Retrieve entries from the EA International API.
Optional BASE-URL specifies the API endpoint base URL.
If not provided, defaults to https://local-dev.ea.international."
  (interactive)
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("accept" . "text/plain")))
         (base (or base-url "https://local-dev.ea.international"))
         (endpoint "/api/entries")
         (url (concat base endpoint))
         (buffer (url-retrieve-synchronously url t)))
    (if buffer
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (search-forward-regexp "^$" nil t)
              (progn
                (forward-char)
                (let ((entries-text (buffer-substring (point) (point-max))))
                  (kill-buffer buffer)
                  (with-current-buffer (get-buffer-create "*EA Entries*")
                    (erase-buffer)
                    (insert entries-text)
                    (bibtex-mode)
                    (goto-char (point-min))
                    (display-buffer (current-buffer))
                    (message "Retrieved %d entries"
                             (how-many "@\\w+{" (point-min) (point-max))))))
            (message "Error: Could not parse response")
            (kill-buffer buffer)))
      (message "Error: Could not connect to API"))))

(defun tlon-ebib-authenticate ()
  "Authenticate with the EA International API.
Returns the authentication token or nil if authentication failed."
  (interactive)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          (concat "grant_type=password"
                  "&username=" (url-hexify-string tlon-ebib-api-username)
                  "&password=" (url-hexify-string tlon-ebib-api-password)))
         (auth-url (concat tlon-ebib-api-base-url "/api/auth/token"))
         (response-buffer (url-retrieve-synchronously auth-url t))
         auth-data)
    (when response-buffer
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (when (search-forward-regexp "^$" nil t)
          (let ((json-object-type 'hash-table)
                (json-array-type 'list)
                (json-key-type 'string))
            (setq auth-data (json-read))))
        (kill-buffer)))
    (when auth-data
      (setq tlon-ebib-auth-token (gethash "access_token" auth-data))
      ;; Set token expiry to 30 minutes from now (typical JWT expiry)
      (setq tlon-ebib-auth-token-expiry (time-add (current-time) (seconds-to-time (* 30 60))))
      tlon-ebib-auth-token)))

(defun tlon-ebib-ensure-auth ()
  "Ensure we have a valid authentication token, refreshing if needed.
Returns the token or nil if authentication failed."
  (when (or (null tlon-ebib-auth-token)
            (null tlon-ebib-auth-token-expiry)
            (time-less-p tlon-ebib-auth-token-expiry (current-time)))
    (tlon-ebib-authenticate))
  tlon-ebib-auth-token)

(defun tlon-ebib-check-name (name)
  "Check if NAME exists in the EA International database."
  (interactive "sName to check: ")
  (unless (tlon-ebib-ensure-auth)
    (error "Authentication failed"))
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("accept" . "application/json")
            ("Authorization" . ,(concat "Bearer " tlon-ebib-auth-token))))
         (url-request-data
          (json-encode `(("name" . ,name))))
         (check-url (concat tlon-ebib-api-base-url "/api/names/check"))
         (response-buffer (url-retrieve-synchronously check-url t))
         response-data)
    (when response-buffer
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (let ((status-line (buffer-substring (point) (line-end-position))))
          (if (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
              (let ((status-code (string-to-number (match-string 1 status-line))))
                (if (= status-code 200)
                    (progn
                      (when (search-forward-regexp "^$" nil t)
                        (let ((json-object-type 'hash-table)
                              (json-array-type 'list)
                              (json-key-type 'string))
                          (setq response-data (json-read))))
                      (kill-buffer)
                      (with-current-buffer (get-buffer-create "*EA Name Check*")
                        (erase-buffer)
                        (insert "Name check result for: " name "\n\n")
                        (if response-data
                            (progn
                              (insert "Name: " (gethash "name" response-data) "\n")
                              (when (gethash "exists" response-data)
                                (insert "Exists: Yes\n"))
                              (when (gethash "conflicts" response-data)
                                (insert "Conflicts: "
                                        (mapconcat #'identity
                                                   (gethash "conflicts" response-data)
                                                   ", ") 
                                        "\n")))
                          (insert "No data returned"))
                        (display-buffer (current-buffer))))
                  (progn
                    (kill-buffer response-buffer)
                    (message "Error: HTTP status %d" status-code)))))
          (kill-buffer response-buffer)))
      response-data)))

(defun ea-check-or-insert-name (name &optional)
  "Check if NAME exists in the database, and insert it if unambiguous.
If the name doesn't exist and there are no similar names, it will be inserted.
Otherwise, it will report whether the name matches exactly or is similar to
existing names. Optional USERNAME and PASSWORD for authentication if not already
authenticated."
  (interactive "sName to check or insert: ")
  (unless (tlon-ebib-ensure-auth)
    (error "Authentication failed"))
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("accept" . "application/json")
            ("Authorization" . ,(concat "Bearer " tlon-ebib-auth-token))))
         (url-request-data
          (json-encode `(("name" . ,name))))
         (check-insert-url (concat tlon-ebib-api-base-url "/api/names/check-insert"))
         (response-buffer (url-retrieve-synchronously check-insert-url t))
         response-data
         status-code)
    (when response-buffer
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (let ((status-line (buffer-substring (point) (line-end-position))))
          (if (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
              (setq status-code (string-to-number (match-string 1 status-line)))
            (setq status-code 0))
          
          (when (search-forward-regexp "^$" nil t)
            (let ((json-object-type 'hash-table)
                  (json-array-type 'list)
                  (json-key-type 'string))
              (condition-case nil
                  (setq response-data (json-read))
                (error nil))))
          (kill-buffer)))
      (with-current-buffer (get-buffer-create "*EA Name Check/Insert*")
        (erase-buffer)
        (insert "Name check/insert result for: " name "\n\n")
        (cond
         ;; Success - name was inserted or matched exactly
         ((= status-code 200)
          (insert "Status: Success\n")
          (when response-data
            (when (gethash "name" response-data)
              (insert "Name: " (gethash "name" response-data) "\n"))
            (when (gethash "message" response-data)
              (insert "Message: " (gethash "message" response-data) "\n"))))
         ;; Conflict - name is similar to existing names
         ((= status-code 409)
          (insert "Status: Conflict - Name not inserted\n")
          (when response-data
            (when (gethash "message" response-data)
              (insert "Message: " (gethash "message" response-data) "\n"))
            (when (gethash "similar_names" response-data)
              (insert "Similar names:\n")
              (dolist (similar-name (gethash "similar_names" response-data))
                (insert "  - " similar-name "\n")))))
         ;; Validation error
         ((= status-code 422)
          (insert "Status: Validation Error\n")
          (when (and response-data (gethash "detail" response-data))
            (insert "Details:\n")
            (dolist (detail (gethash "detail" response-data))
              (when (hash-table-p detail)
                (insert "  - " (gethash "msg" detail "") "\n")))))
         ;; Other error
         (t
          (insert "Status: Error (HTTP " (number-to-string status-code) ")\n")))
        (display-buffer (current-buffer))))
    (list :status status-code :data response-data)))

;;;;; Menu

;;;###autoload (autoload 'tlon-ebib-menu "tlon-ebib" nil t)
(transient-define-prefix tlon-ebib-menu ()
  "Menu for `ebib' functions."
  ;; TODO: create menu
  )

(provide 'tlon-ebib)
;;; tlon-ebib.el ends here

