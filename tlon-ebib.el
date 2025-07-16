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

(require 'bibtex)
(require 'ebib)
(require 'tlon-core)
(require 'transient)

;;;; Variables

;;;;; Auth

(defvar tlon-ebib-auth-token nil
  "Authentication token for EA International API.")

(defvar tlon-ebib-auth-token-expiry nil
  "Expiry time for the authentication token.")

(defvar tlon-ebib-api-base-url "https://ea.international"
  "Base URL for the EA International API.")

(defvar tlon-ebib-api-username
  (tlon-user-lookup :github :name user-full-name))

(defvar tlon-ebib-api-password
  (auth-source-pass-get 'secret (concat "tlon/core/ea.international/" tlon-ebib-api-username)))

;;;;; External Special Variables (from url.el)

(defvar url-request-status nil
  "Dynamically bound by url.el to the HTTP status of the last request.")
(defvar url-request-error-message nil
  "Dynamically bound by url.el to the HTTP error message of the last request.")
(defvar url-http-response-buffer nil
  "Dynamically bound by url.el to the buffer containing the HTTP response.")
(defvar url-debug nil
  "Dynamically bound by url.el to control debug output.
Set to t to enable verbose logging from url.el.")

;;;;; Files

(defvar tlon-ebib-file-db
  (file-name-concat tlon-bibtex-dir "db.bib")
  "File containing the temporary bibliography.")

(defconst tlon-ebib--result-buffer-name "*Ebib API Result*"
  "Name of the buffer used to display API call results.")

;;;; Functions

(defun tlon-ebib-initialize ()
  "Initialize the `tlon-ebib' package."
  (tlon-ebib-get-entries)
  (append paths-files-bibliography-all (list tlon-ebib-file-db)))

(defvar citar-bibliography)
(declare-function citar-select-ref "citar")
(defun tlon-ebib-get-db-entries ()
  "Prompt the user to select a work in the db and return its key."
  (let ((citar-bibliography (list tlon-file-db)))
    (citar-select-ref)))

(defun tlon-ebib-ensure-key (key)
  "Ensure that KEY is provided and exists in the bibliography."
  (when (string-empty-p key)
    (user-error "No entry key provided"))
  (unless (tlon-bibliography-lookup "=key=" key)
    (user-error "Key `%s' not found in the bibliography" key)))

;;;;; API

;;;;;; Authenticate

(defun tlon-ebib-authenticate ()
  "Authenticate with the EA International API.
Returns the authentication token or nil if authentication failed."
  (interactive)
  (let* ((data (concat "grant_type=password"
                       "&username=" (url-hexify-string tlon-ebib-api-username)
                       "&password=" (url-hexify-string tlon-ebib-api-password)))
         (headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (response-buffer (tlon-ebib--make-request "POST" "/api/auth/token" data headers nil))
         auth-data status-code raw-response-text)
    (when response-buffer
      (unwind-protect
          (progn
            (setq raw-response-text (with-current-buffer response-buffer (buffer-string)))
            (setq status-code (tlon-ebib--get-response-status-code response-buffer))
            (if (= status-code 200)
                (setq auth-data (tlon-ebib--parse-json-response response-buffer))
              (tlon-ebib--display-result-buffer
               "Authentication failed"
               #'tlon-ebib--format-post-entry-result
               `(:status ,status-code :raw-text ,raw-response-text))
              (user-error "Authentication failed: HTTP status %d. See *Ebib API Result* buffer for details"
                          status-code)))
	(kill-buffer response-buffer))) ; Ensure buffer is killed
    (when auth-data
      (setq tlon-ebib-auth-token (gethash "access_token" auth-data))
      ;; Set token expiry to 30 minutes from now (typical JWT expiry).
      (setq tlon-ebib-auth-token-expiry (time-add (current-time) (seconds-to-time (* 30 60))))
      (message "Authentication successful. Token: %s" tlon-ebib-auth-token)
      tlon-ebib-auth-token)))

(defun tlon-ebib-ensure-auth ()
  "Ensure we have a valid authentication token, refreshing if needed.
Returns the token or nil if authentication failed."
  (when (or (null tlon-ebib-auth-token)
            (null tlon-ebib-auth-token-expiry)
            (time-less-p tlon-ebib-auth-token-expiry (current-time)))
    (tlon-ebib-authenticate))
  (or tlon-ebib-auth-token
      (user-error "Authentication failed")))

;;;;;; Get entries

(defun tlon-ebib-get-entries (&optional base-url)
  "Retrieve entries from the EA International API.
Optional BASE-URL specifies the API endpoint base URL. If not provided,
defaults to `tlon-ebib-api-base-url'."
  (interactive)
  (let (entries-text)
    (when-let* ((response-buffer (tlon-ebib--make-request "GET" "/api/entries" nil
							  '(("accept" . "text/plain"))
							  nil ; No auth required
							  base-url)))
      (with-current-buffer response-buffer
	(goto-char (point-min))
	(if (search-forward-regexp "^$" nil t)
	    (progn
	      (forward-char) ; Move past the empty line separating headers and body
	      (setq entries-text (buffer-substring (point) (point-max))))
	  (user-error "Could not parse response from API"))
	(kill-buffer response-buffer)))
    (if entries-text
        (if (file-exists-p tlon-ebib-file-db)
            (let ((temp-file (make-temp-file "ebib-entries-")))
              (unwind-protect
                  (progn
                    (with-temp-buffer
                      (let ((coding-system-for-write 'utf-8-unix))
                        (insert entries-text)
                        (write-file temp-file)))
                    (if (not (tlon-ebib--files-have-same-content-p tlon-ebib-file-db temp-file))
                        (if (y-or-n-p (format "File %s exists and is different. Overwrite?"
                                              tlon-ebib-file-db))
                            (progn
                              (copy-file temp-file tlon-ebib-file-db t)
                              (message "Updated %s." tlon-ebib-file-db)
                              (with-temp-buffer
                                (insert entries-text)
                                (bibtex-count-entries)))
                          (message "Keeping existing file %s." tlon-ebib-file-db))
                      (message "File %s is already up to date." tlon-ebib-file-db)))
                (when (file-exists-p temp-file)
                  (delete-file temp-file))))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-buffer
              (insert entries-text)
              (write-file tlon-ebib-file-db)
              (bibtex-count-entries))))
      (user-error "Failed to retrieve entries"))))

(defun tlon-ebib--files-have-same-content-p (file1 file2)
  "Return t if FILE1 and FILE2 have identical content."
  (let ((content1 (with-temp-buffer
                    (insert-file-contents file1)
                    (buffer-string)))
        (content2 (with-temp-buffer
                    (insert-file-contents file2)
                    (buffer-string))))
    (string= content1 content2)))

;;;;;; Add entry

(declare-function bibtex-extras-get-key "bibtex-extras")
(declare-function bibtex-extras-get-entry-as-string "bibtex-extras")
(declare-function ebib-extras-get-field "ebib-extras")
(defun tlon-ebib-add-entry ()
  "Post the BibTeX entry at point to the EA International API.
The entry is sent as \"text/plain\".
Handles 200 (Success) and 422 (Validation Error) responses."
  (interactive)
  (tlon-ebib-ensure-auth)
  (let* ((key (pcase major-mode
                ((or 'ebib-index-mode 'ebib-entry-mode) (ebib-extras-get-field "=key="))
                ('bibtex-mode (bibtex-extras-get-key))
                (_ (user-error "Not in ebib or bibtex mode"))))
         (entry-text (bibtex-extras-get-entry-as-string key))
         (encoded-entry-text (encode-coding-string entry-text 'utf-8))
         (headers `(("Content-Type" . "text/plain; charset=utf-8")
                    ("accept" . "text/plain")))
         (result (tlon-ebib--handle-entry-request "POST" "/api/entries" encoded-entry-text headers))
         (status-code (plist-get result :status)))
    (if (or tlon-debug (not (and status-code (= status-code 200))))
        (tlon-ebib--display-result-buffer
         (format "Post entry result (Status: %s)" (if status-code (number-to-string status-code) "N/A"))
         #'tlon-ebib--format-post-entry-result
         result)
      (message "Entry posted successfully."))
    result))

;;;;;; Delete entry

(defun tlon-ebib-delete-entry (key)
  "Delete KEY from the EA International API."
  (interactive (list (tlon-ebib-get-db-entries)))
  (tlon-ebib-ensure-key key)
  (tlon-ebib-ensure-auth)
  (unless (y-or-n-p (format "Are you sure you want to delete entry '%s' (this action is irreversible)?" key))
    (user-error "Deletion cancelled"))
  (let* ((endpoint (format "/api/entries/%s" (url-hexify-string key)))
         (headers '(("accept" . "application/json")))
         (result (tlon-ebib--handle-entry-request "DELETE" endpoint nil headers t))
         (status-code (plist-get result :status)))
    (if (or tlon-debug (not (and status-code (= status-code 200))))
        (tlon-ebib--display-result-buffer
         (format "Delete entry result (Status: %s)" (if status-code (number-to-string status-code) "N/A"))
         #'tlon-ebib--format-delete-entry-result
         result)
      (message "Entry '%s' deleted successfully." key))
    result))

(defun tlon-ebib--handle-entry-request (method endpoint data headers &optional json-on-success)
  "Handle a request to an entry endpoint and process the response.
METHOD, ENDPOINT, DATA, and HEADERS are for `tlon-ebib--make-request`.
If JSON-ON-SUCCESS is non-nil, parse JSON on 200 status.
Returns a plist with :status, :data, and :raw-text."
  (let (response-buffer response-data raw-response-text status-code)
    (setq response-buffer (tlon-ebib--make-request method endpoint data headers t))
    (if (not response-buffer)
        (setq status-code nil)
      (unwind-protect
          (progn
            (setq raw-response-text (with-current-buffer response-buffer (buffer-string)))
            (condition-case _err
                (setq status-code (tlon-ebib--get-response-status-code response-buffer))
              (error
               (setq status-code nil)))
            (cond
             ((and status-code (= status-code 422))
              (setq response-data (tlon-ebib--parse-json-response response-buffer)))
             ((and json-on-success status-code (= status-code 200))
              (setq response-data (tlon-ebib--parse-json-response response-buffer)))))
        (when response-buffer (kill-buffer response-buffer))))
    (list :status status-code :data response-data :raw-text raw-response-text)))

(defun tlon-ebib--format-delete-entry-result (result)
  "Format the RESULT from `tlon-ebib-delete-entry` for display.
RESULT is a plist like (:status CODE :data JSON-DATA :raw-text TEXT-DATA)."
  (let ((status-code (plist-get result :status))
        (response-data (plist-get result :data))  ; Parsed JSON for 200 or 422
        (raw-response-text (plist-get result :raw-text))) ; Raw text for other errors
    (cond
     ((null status-code) ; Error before or during request
      (insert "Status: Request Failed\n")
      (insert (or raw-response-text "No specific error message.")))
     ((= status-code 200)
      (insert "Status: Success (200)\n")
      (insert "Response from server:\n")
      (if (stringp response-data)
          (insert response-data)
        (insert (or raw-response-text "No content returned."))))
     ((= status-code 422)
      (insert "Status: Validation Error (422)\n")
      (if response-data
          (progn
            (insert "Details (from JSON response):\n")
            (let ((detail (gethash "detail" response-data)))
              (if (listp detail) ; Standard FastAPI validation error structure
                  (dolist (item detail)
                    (if (hash-table-p item)
                        (insert (format "  - Location: %s, Message: %s, Type: %s\n"
                                        (mapconcat #'identity (gethash "loc" item) " -> ")
                                        (gethash "msg" item "")
                                        (gethash "type" item "")))
                      (insert (format "  - %s\n" item)))) ; Non-standard detail item
                (insert (format "  Unexpected detail format in JSON: %S\n" detail))))) ; Detail is not a list
        (insert "No specific validation error details found in parsed JSON response.\n"))
      (when raw-response-text
        (insert "\nRaw server response (text/plain or other):\n")
        (insert raw-response-text)))
     (t
      (insert (format "Status: Error (HTTP %d)\n" status-code))
      (insert "Response from server:\n")
      (insert (or raw-response-text "No content or error message returned."))))))

;;;;;; Check name

(defun tlon-ebib-check-name (name)
  "Check if NAME exists in the EA International database."
  (interactive "sName to check: ")
  (tlon-ebib-ensure-auth)
  (let* ((data (json-encode `(("name" . ,name))))
         (headers '(("Content-Type" . "application/json")
                    ("accept" . "application/json")))
         (response-buffer (tlon-ebib--make-request "POST" "/api/names/check" data headers t))
         response-data
         status-code)
    (when response-buffer
      (setq status-code (tlon-ebib--get-response-status-code response-buffer))
      (if (= status-code 200)
          (setq response-data (tlon-ebib--parse-json-response response-buffer))
        (message "Error checking name: HTTP status %d" status-code)) ; This message might be redundant if buffer is shown
      (kill-buffer response-buffer))
    (if (or tlon-debug (not (and status-code (= status-code 200))))
        (tlon-ebib--display-result-buffer (format "Name check result for: %s" name)
                                          #'tlon-ebib--format-check-name-result
                                          response-data)
      (message "Name check for '%s': OK." name))
    response-data))

(defun tlon-ebib-check-or-insert-name (name)
  "Check if NAME exists in the database, and insert it if unambiguous.
If the name doesn't exist and there are no similar names, it will be
inserted. Otherwise, it will report whether the name matches exactly or is
similar to existing names."
  (interactive "sName to check or insert: ")
  (unless (tlon-ebib-ensure-auth)
    (user-error "Authentication failed"))
  (let* ((data (json-encode `(("name" . ,name))))
         (headers '(("Content-Type" . "application/json")
                    ("accept" . "application/json")))
         (response-buffer (tlon-ebib--make-request "POST" "/api/names/check-insert" data headers t))
         response-data
         status-code)
    (when response-buffer
      (setq status-code (tlon-ebib--get-response-status-code response-buffer))
      ;; Parse response even on error, as it might contain details
      (setq response-data (tlon-ebib--parse-json-response response-buffer))
      (kill-buffer response-buffer))
    (if (or tlon-debug (not (and status-code (= status-code 200))))
        (tlon-ebib--display-result-buffer (format "Name check/insert result for: %s" name)
                                          #'tlon-ebib--format-check-insert-name-result
                                          (list :status status-code :data response-data))
      (message "Name check/insert for '%s': OK." name))
    (list :status status-code :data response-data)))

(defun tlon-ebib--format-check-name-result (data)
  "Format the result DATA from `tlon-ebib-check-name` for display."
  (cond
   ((null data)
    (insert "No data returned from API or error occurred parsing the response.\n"))
   ((gethash "detail" data)
    (let ((detail-msg (gethash "message" (gethash "detail" data))))
      (if detail-msg
          (insert (format "API Response: %s\n" detail-msg))
        (insert "API Response: Name not found, but no specific message provided in 'detail'.\n"))))
   ((and (gethash "name" data) (gethash "message" data))
    (insert (format "Name: %s\n" (gethash "name" data)))
    (insert (format "Message: %s\n" (gethash "message" data))))
   (t
    (insert "Unexpected response format from API.\n"))))

(defun tlon-ebib--format-check-insert-name-result (result)
  "Format the RESULT from `tlon-ebib-check-or-insert-name` for display.
RESULT is a plist like (:status CODE :data DATA)."
  (let ((status-code (plist-get result :status))
        (response-data (plist-get result :data)))
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
          (if (hash-table-p detail) ; Handle FastAPI validation error structure
              (insert (format "  - Field: %s, Error: %s\n"
                              (mapconcat #'identity (gethash "loc" detail) ".")
                              (gethash "msg" detail "")))
            (insert (format "  - %s\n" detail)))))) ; Handle simpler error messages
     ;; Other error
     (t
      (insert "Status: Error (HTTP " (number-to-string (or status-code 0)) ")\n")
      (when response-data ; Display any message if available
        (when (gethash "message" response-data)
          (insert "Message: " (gethash "message" response-data) "\n"))
        (when (gethash "detail" response-data) ; Handle FastAPI detail string
          (insert "Detail: " (gethash "detail" response-data) "\n")))))))

;;;;;; Helpers

(defun tlon-ebib--make-request (method endpoint data headers &optional auth-required base-url)
  "Make an HTTP request to the EA International API.
METHOD is the HTTP method (e.g., \"GET\", \"POST\").
ENDPOINT is the API endpoint path (e.g., \"/api/entries\").
DATA is the request body data (string or nil).
HEADERS is an alist of extra request headers.
AUTH-REQUIRED non-nil means authentication token will be added.
Optional BASE-URL overrides `tlon-ebib-api-base-url'."
  (let* ((url-request-method method)
	 (url-request-data data)
	 (url-request-extra-headers (copy-alist headers))
	 (base (or base-url tlon-ebib-api-base-url))
	 (url (concat base endpoint))
	 (lisp-error-occurred nil)
	 (lisp-error-message nil)
	 response-buffer url-request-status url-request-error-message url-http-response-buffer url-debug)
    (when auth-required
      (tlon-ebib-ensure-auth)
      (add-to-list 'url-request-extra-headers
                   `("Authorization" . ,(concat "Bearer " tlon-ebib-auth-token)) t))
    (condition-case err
        (setq response-buffer (url-retrieve-synchronously url t))
      (error
       (setq lisp-error-occurred t
             lisp-error-message (error-message-string err)
	     response-buffer nil)))
    (unless response-buffer
      (unless lisp-error-occurred
        (when (and (boundp 'url-http-response-buffer)
                   url-http-response-buffer
                   (buffer-live-p url-http-response-buffer))
          (setq response-buffer url-http-response-buffer))))
    (unless response-buffer
      (if lisp-error-occurred
          (user-error "Could not connect to API at %s or request failed critically due to a Lisp error: %s"
		      url lisp-error-message)
        (user-error "Could not connect to API at %s or request failed critically. HTTP Status: %s, Message: %s"
                    url url-request-status url-request-error-message)))
    response-buffer))

(defun tlon-ebib--get-response-status-code (buffer)
  "Extract HTTP status code from response BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((status-line (buffer-substring-no-properties (point) (line-end-position))))
      (if (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
          (string-to-number (match-string 1 status-line))
        (user-error "Could not parse HTTP status line: %s" status-line)))))

(defun tlon-ebib--parse-json-response (buffer)
  "Parse JSON data from response BUFFER.
Returns the parsed data (hash-table) or nil on error."
  (when buffer
    (with-current-buffer buffer
      (goto-char (point-min))
      (when (search-forward-regexp "^$" nil t) ; Move past headers
        (let ((json-object-type 'hash-table)
              (json-array-type 'list)
              (json-key-type 'string))
          (condition-case err
              (json-read)
            (error (message "Error parsing JSON response: %s" err) nil)))))))

(defun tlon-ebib--display-result-buffer (title formatter-fn data)
  "Display TITLE and formatted DATA (via FORMATTER-FN) in a dedicated buffer.
The buffer is named by `tlon-ebib--result-buffer-name`.
TITLE is the initial title string for the buffer content.
FORMATTER-FN is a function that takes DATA and inserts formatted content
into the current buffer."
  (let ((result-buffer (get-buffer-create tlon-ebib--result-buffer-name)))
    (with-current-buffer result-buffer
      (let ((was-read-only buffer-read-only)) ; Store original read-only state
        (when was-read-only
          (setq buffer-read-only nil)) ; Make writable if it was read-only
        (erase-buffer)
        (insert title "\n\n")
        (funcall formatter-fn data)
        (fundamental-mode) ; Or any other simple mode
        (setq buffer-read-only t))) ; Set back to read-only
    (display-buffer result-buffer)))

(defun tlon-ebib--format-post-entry-result (result)
  "Format the RESULT from `tlon-ebib-add-entry` for display.
RESULT is a plist like (:status CODE :data JSON-DATA :raw-text TEXT-DATA)."
  (let ((status-code (plist-get result :status))
        (response-data (plist-get result :data))  ; Parsed JSON for 422
        (raw-response-text (plist-get result :raw-text))) ; Raw text for 200 or other errors
    (cond
     ((null status-code) ; Error before or during request
      (insert "Status: Request Failed\n")
      (insert (or raw-response-text "No specific error message.")))
     ((= status-code 200)
      (insert "Status: Success (200)\n")
      (insert "Response from server:\n")
      (insert (or raw-response-text "No content returned.")))
     ((= status-code 422)
      (insert "Status: Validation Error (422)\n")
      (if response-data
          (progn
            (insert "Details (from JSON response):\n")
            (let ((detail (gethash "detail" response-data)))
              (if (listp detail) ; Standard FastAPI validation error structure
                  (dolist (item detail)
                    (if (hash-table-p item)
                        (insert (format "  - Location: %s, Message: %s, Type: %s\n"
                                        (mapconcat #'identity (gethash "loc" item) " -> ")
                                        (gethash "msg" item "")
                                        (gethash "type" item "")))
                      (insert (format "  - %s\n" item)))) ; Non-standard detail item
                (insert (format "  Unexpected detail format in JSON: %S\n" detail))))) ; Detail is not a list
        (insert "No specific validation error details found in parsed JSON response.\n"))
      (when raw-response-text
        (insert "\nRaw server response (text/plain or other):\n")
        (insert raw-response-text)))
     (t
      (insert (format "Status: Error (HTTP %d)\n" status-code))
      (insert "Response from server:\n")
      (insert (or raw-response-text "No content or error message returned."))))))

;;;;;; Periodic data update

(run-with-idle-timer (* 3 60 60) t #'tlon-ebib-get-entries)

;;;;; Menu

;;;###autoload (autoload 'tlon-ebib-menu "tlon-ebib" nil t)
(transient-define-prefix tlon-ebib-menu ()
  "Menu for `ebib' functions."
  ["Ebib Actions"
   ("g" "Get entries" tlon-ebib-get-entries)
   ("a" "Add entry" tlon-ebib-add-entry)
   ("d" "Delete entry" tlon-ebib-delete-entry)
   ("c" "Check name" tlon-ebib-check-name)
   ("i" "Check or insert name" tlon-ebib-check-or-insert-name)
   ""
   ("A" "Authenticate" tlon-ebib-authenticate)])

(provide 'tlon-ebib)
;;; tlon-ebib.el ends here

