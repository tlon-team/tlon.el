;;; tlon-db.el --- Db integration -*- lexical-binding: t -*-

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

;; Db integration.

;;; Code:

(require 'bibtex)
(require 'ebib)
(require 'tlon-core)
(require 'transient)
(require 'bibtex-extras)
(require 'cl-lib)
(require 'subr-x)
(require 'filenotify)

;;;; Variables

;;;;; Auth

(defvar tlon-db-auth-token nil
  "Authentication token for EA International API.")

(defvar tlon-db-auth-token-expiry nil
  "Expiry time for the authentication token.")

(defvar tlon-db-api-base-url "https://ea.international"
  "Base URL for the EA International API.")

(defvar tlon-db-api-username
  (tlon-user-lookup :github :name user-full-name))

(defvar tlon-db-api-password
  (auth-source-pass-get 'secret (concat "tlon/core/ea.international/" tlon-db-api-username)))

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

(defvar tlon-db-file-db
  (file-name-concat tlon-bibtex-dir "db.bib")
  "File containing the local BibTeX database, intended for user edits.")

(defvar tlon-db-file-db-upstream
  (file-name-concat tlon-bibtex-dir "db-upstream.bib")
  "File containing the BibTeX database state from the remote API.")

(defconst tlon-db--result-buffer-name "*Db API Result*"
  "Name of the buffer used to display API call results.")

(defconst tlon-db--sync-log-buffer-name "*Db Sync Log*"
  "Name of the buffer used to log sync operations.")

;;;;; Sync

(defvar tlon-db--db-watch-descriptor nil
  "File notification descriptor for db.bib.")

(defvar tlon-db--sync-in-progress nil
  "Flag to prevent recursive sync operations.")

;;;; Functions

(defun tlon-db-initialize ()
  "Initialize the `tlon-db' package."
  (tlon-db--initialize-sync)
  (append paths-files-bibliography-all (list tlon-db-file-db)))

(defvar citar-bibliography)
(declare-function citar-select-ref "citar")
(defun tlon-db-get-db-entries ()
  "Prompt the user to select a work in the db and return its key."
  (let ((citar-bibliography (list tlon-file-db)))
    (citar-select-ref)))

(defun tlon-db-get-key-at-point ()
  "If there is a bibtex key at point, return it."
  (pcase major-mode
    ((or 'ebib-index-mode 'ebib-entry-mode) (ebib--get-key-at-point))
    ('bibtex-mode (bibtex-extras-get-key))))

;;;;; API

;;;;;; Authenticate

(defun tlon-db-authenticate ()
  "Authenticate with the EA International API.
Returns the authentication token or nil if authentication failed."
  (interactive)
  (let* ((data (concat "grant_type=password"
		       "&username=" (url-hexify-string tlon-db-api-username)
		       "&password=" (url-hexify-string tlon-db-api-password)))
	 (headers '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (response-buffer (tlon-db--make-request "POST" "/api/auth/token" data headers nil))
	 auth-data status-code raw-response-text)
    (when response-buffer
      (unwind-protect
	  (progn
	    (setq raw-response-text (with-current-buffer response-buffer (buffer-string)))
	    (setq status-code (tlon-db--get-response-status-code response-buffer))
	    (if (= status-code 200)
		(setq auth-data (tlon-db--parse-json-response response-buffer))
	      (tlon-db--display-result-buffer
	       "Authentication failed"
	       #'tlon-db--format-post-entry-result
	       `(:status ,status-code :raw-text ,raw-response-text))
	      (user-error "Authentication failed: HTTP status %d. See *Db API Result* buffer for details"
			  status-code)))
	(kill-buffer response-buffer))) ; Ensure buffer is killed
    (when auth-data
      (setq tlon-db-auth-token (gethash "access_token" auth-data))
      ;; Set token expiry to 30 minutes from now (typical JWT expiry).
      (setq tlon-db-auth-token-expiry (time-add (current-time) (seconds-to-time (* 30 60))))
      (message "Authentication successful. Token: %s" tlon-db-auth-token)
      tlon-db-auth-token)))

(defun tlon-db-ensure-auth ()
  "Ensure we have a valid authentication token, refreshing if needed.
Returns the token or nil if authentication failed."
  (when (or (null tlon-db-auth-token)
	    (null tlon-db-auth-token-expiry)
	    (time-less-p tlon-db-auth-token-expiry (current-time)))
    (tlon-db-authenticate))
  (or tlon-db-auth-token
      (user-error "Authentication failed")))

;;;;;; Get entries

(declare-function bibtex-extras-escape-special-characters "bibtex-extras")
(defun tlon-db-get-entries (&optional base-url)
  "Retrieve entries from the EA International API and update local databases.
Optional BASE-URL specifies the API endpoint base URL. If not provided,
defaults to `tlon-db-api-base-url'.
The command first checks that there are no unsaved changes to `db.bib' and
that `db.bib' and `db-upstream.bib' are identical. If either of these
conditions is not met, an error is logged and the process is aborted."
  (interactive)
  (when (and (get-file-buffer tlon-db-file-db)
             (buffer-modified-p (get-file-buffer tlon-db-file-db)))
    (user-error "Buffer for %s has unsaved changes. Save it first" (file-name-nondirectory tlon-db-file-db)))
  (when (and (file-exists-p tlon-db-file-db)
             (file-exists-p tlon-db-file-db-upstream)
             (not (tlon-db--files-have-same-content-p tlon-db-file-db tlon-db-file-db-upstream)))
    (ediff tlon-db-file-db tlon-db-file-db-upstream)
    (user-error "Files %s and %s are not in sync. Resolve differences before fetching entries"
                (file-name-nondirectory tlon-db-file-db)
		(file-name-nondirectory tlon-db-file-db-upstream)))
  (let (entries-text)
    (when-let* ((response-buffer (tlon-db--make-request "GET" "/api/entries" nil
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
        (let ((tlon-db--sync-in-progress t))
          (with-temp-buffer
            (let ((coding-system-for-write 'utf-8-unix))
              (insert entries-text)
              (bibtex-extras-escape-special-characters)
              (write-file tlon-db-file-db-upstream)
              (write-file tlon-db-file-db)
              (message "Updated %s and %s." tlon-db-file-db tlon-db-file-db-upstream)
              (bibtex-count-entries))))
      (user-error "Failed to retrieve entries"))))

(defun tlon-db--files-have-same-content-p (file1 file2)
  "Return t if FILE1 and FILE2 have identical content."
  (let ((exists1 (file-exists-p file1))
        (exists2 (file-exists-p file2)))
    (if (not (eq exists1 exists2))
        nil ; one exists, one doesn't
      (if (not exists1)
          t ; both don't exist
        (let ((content1 (with-temp-buffer
                          (insert-file-contents-literally file1)
                          (buffer-string)))
              (content2 (with-temp-buffer
                          (insert-file-contents-literally file2)
                          (buffer-string))))
          (string= content1 content2))))))

;;;;;; Post entry

(declare-function bibtex-extras-get-entry-as-string "bibtex-extras")
(declare-function bibtex-extras-delete-entry "bibtex-extras")
(declare-function bibtex-extras-insert-entry "bibtex-extras")
(declare-function ebib-extras-get-field "ebib-extras")
(cl-defun tlon-db-post-entry (&optional key attempt)
  "Create or update KEY in the EA International API.
If called interactively, post the entry at point; otherwise use KEY."
  (interactive)
  (setq attempt (or attempt 0))
  (tlon-db-ensure-auth)
  (if-let ((entry-key (or key (tlon-db-get-key-at-point))))
      (let* ((entry-text   (bibtex-extras-get-entry-as-string entry-key nil))
             (encoded-text (encode-coding-string entry-text 'utf-8))
             (headers      '(("Content-Type" . "text/plain; charset=utf-8")
                             ("accept"       . "text/plain")))
             (result       (tlon-db--handle-entry-request
                            "POST" "/api/entries" encoded-text headers))
             (status-code  (plist-get result :status))
             (raw-text     (plist-get result :raw-text)))
        ;; Detect missing author names (HTTP 400) and offer to create them,
        ;; then retry the post once.
        (when (and status-code (= status-code 400) (< attempt 1))
          (let ((missing-names (tlon-db--extract-missing-names raw-text)))
            (when (and missing-names
                       (y-or-n-p
                        (format "Missing author names not found (%s). Create them and retry? "
                                (string-join missing-names ", "))))
              (dolist (name missing-names)
                (tlon-db-set-name name nil nil))
              (cl-return-from tlon-db-post-entry
                (tlon-db-post-entry entry-key (1+ attempt))))))
        (if (or tlon-debug (not (and status-code (= status-code 200))))
            ;; Non‑200 or debug ⇒ show whole response
            (tlon-db--display-result-buffer
             (format "Post entry result (Status: %s)"
                     (if status-code (number-to-string status-code) "N/A"))
             #'tlon-db--format-post-entry-result result)
          ;; Success ­‑— decide which text we copy locally
          (let* ((body (tlon-db--get-response-body raw-text))
                 (local-copy (or body entry-text)))
            (tlon-db--replace-entry-locally entry-key local-copy)
            (message "Entry “%s” posted and mirrored locally." entry-key))))
    (user-error "No BibTeX key found at point")))

(defun tlon-db--replace-entry-locally (key entry)
  "Replace KEY with ENTRY text in both local databases."
  (let ((coding-system-for-write 'utf-8-with-signature))
    (dolist (file (list tlon-db-file-db tlon-db-file-db-upstream))
      (with-current-buffer (find-file-noselect file)
        (bibtex-mode)
        (goto-char (point-min))
        (if (bibtex-search-entry key)
            (progn (bibtex-kill-entry) (insert entry))
          (tlon-db--insert-entry-with-newlines entry))
        (save-buffer)
        (revert-buffer nil 'no-confirm)))))

(defun tlon-db--insert-entry-with-newlines (entry)
  "Insert bibtex ENTRY at end of buffer with proper newlines."
  (goto-char (point-max))
  (delete-blank-lines)
  (unless (bobp)
    (unless (bolp)
      (insert "\n"))
    (insert "\n"))
  (insert entry))

;;;;;; Delete entry

(defun tlon-db-delete-entry (key &optional no-confirm locally)
  "Delete KEY from the EA International API.
If LOCALLY is non-nil, also delete from local db file.
Interactively, NO-CONFIRM is set with a prefix argument, and LOCALLY is t."
  (interactive (list (or (tlon-db-get-key-at-point)
			 (tlon-db-get-db-entries))
		     current-prefix-arg
		     t))
  (tlon-db-ensure-auth)
  (when (or no-confirm
	    (y-or-n-p (format "Are you sure you want to delete entry '%s' (this action is irreversible)?" key)))
    (let* ((endpoint (format "/api/entries/%s" (url-hexify-string key)))
	   (headers '(("accept" . "application/json")))
	   (result (tlon-db--handle-entry-request "DELETE" endpoint nil headers t))
	   (status-code (plist-get result :status)))
      (if (or tlon-debug (not (and status-code (= status-code 200))))
	  (tlon-db--display-result-buffer
	   (format "Delete entry result (Status: %s)" (if status-code (number-to-string status-code) "N/A"))
	   #'tlon-db--format-delete-entry-result
	   result)
	(when locally (tlon-db-delete-entry-locally key)))
      result)))

(defun tlon-db-delete-entry-locally (key)
  "Delete KEY from both local files."
  (let ((deleted nil))
    (let ((coding-system-for-write 'utf-8-unix))
      ;; db.bib
      (with-current-buffer (find-file-noselect tlon-db-file-db)
        (bibtex-mode)
        (when (bibtex-search-entry key)
          (setq deleted t)
          (bibtex-kill-entry)
          (unless tlon-db--sync-in-progress
            (save-buffer))))
      ;; db-upstream.bib
      (with-current-buffer (find-file-noselect tlon-db-file-db-upstream)
        (bibtex-mode)
        (when (bibtex-search-entry key)
          (setq deleted t)
          (bibtex-kill-entry)
          (save-buffer))))
    (if deleted
        (message "Entry “%s” deleted locally." key)
      (message "Entry “%s” not found in local db." key))))

(defun tlon-db--handle-entry-request (method endpoint data headers &optional json-on-success)
  "Handle a request to an entry endpoint and process the response.
METHOD, ENDPOINT, DATA, and HEADERS are for `tlon-db--make-request`.
If JSON-ON-SUCCESS is non-nil, parse JSON on 200 status.
Returns a plist with :status, :data, and :raw-text."
  (let (response-buffer response-data raw-response-text status-code)
    (setq response-buffer (tlon-db--make-request method endpoint data headers t))
    (if (not response-buffer)
	(setq status-code nil)
      (unwind-protect
	  (progn
	    (setq raw-response-text (with-current-buffer response-buffer (buffer-string)))
	    (condition-case _err
		(setq status-code (tlon-db--get-response-status-code response-buffer))
	      (error
	       (setq status-code nil)))
	    (cond
	     ((and status-code (= status-code 422))
	      (setq response-data (tlon-db--parse-json-response response-buffer)))
	     ((and json-on-success status-code (= status-code 200))
	      (setq response-data (tlon-db--parse-json-response response-buffer)))))
	(when response-buffer (kill-buffer response-buffer))))
    (list :status status-code :data response-data :raw-text raw-response-text)))

(defun tlon-db--format-delete-entry-result (result)
  "Format the RESULT from `tlon-db-delete-entry' for display.
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

(defun tlon-db-check-name (name)
  "Check if NAME exists in the EA International database."
  (interactive "sName to check: ")
  (tlon-db-ensure-auth)
  (let* ((data (json-encode `(("name" . ,name))))
	 (headers '(("Content-Type" . "application/json")
		    ("accept" . "application/json")))
	 (response-buffer (tlon-db--make-request "POST" "/api/names/check" data headers t))
	 response-data
	 status-code)
    (when response-buffer
      (setq status-code (tlon-db--get-response-status-code response-buffer))
      (if (= status-code 200)
	  (setq response-data (tlon-db--parse-json-response response-buffer))
	(message "Error checking name: HTTP status %d" status-code)) ; This message might be redundant if buffer is shown
      (kill-buffer response-buffer))
    (if (or tlon-debug (not (and status-code (= status-code 200))))
	(tlon-db--display-result-buffer (format "Name check result for: %s" name)
					  #'tlon-db--format-check-name-result
					  response-data)
      (message "Name check for '%s': OK." name))
    response-data))

(defun tlon-db-check-or-insert-name (name)
  "Check if NAME exists in the database, and insert it if unambiguous.
If the name doesn't exist and there are no similar names, it will be
inserted. Otherwise, it will report whether the name matches exactly or is
similar to existing names."
  (interactive "sName to check or insert: ")
  (unless (tlon-db-ensure-auth)
    (user-error "Authentication failed"))
  (let* ((data (json-encode `(("name" . ,name))))
	 (headers '(("Content-Type" . "application/json")
		    ("accept" . "application/json")))
	 (response-buffer (tlon-db--make-request "POST" "/api/names/check-insert" data headers t))
	 response-data
	 status-code)
    (when response-buffer
      (setq status-code (tlon-db--get-response-status-code response-buffer))
      ;; Parse response even on error, as it might contain details
      (setq response-data (tlon-db--parse-json-response response-buffer))
      (kill-buffer response-buffer))
    (if (or tlon-debug (not (and status-code (= status-code 200))))
	(tlon-db--display-result-buffer (format "Name check/insert result for: %s" name)
					  #'tlon-db--format-check-insert-name-result
					  (list :status status-code :data response-data))
      (message "Name check/insert for '%s': OK." name))
    (list :status status-code :data response-data)))

(defun tlon-db--format-check-name-result (data)
  "Format the result DATA from `tlon-db-check-name` for display."
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

(defun tlon-db--format-check-insert-name-result (result)
  "Format the RESULT from `tlon-db-check-or-insert-name` for display.
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

;;;;;; Set name

(defun tlon-db-set-name (name &optional new-name force)
  "Insert or update NAME in the EA International name database.
If NEW-NAME is non-nil, update NAME to NEW-NAME.  If FORCE is non-nil,
override similarity conflicts.

Interactively, prompt for NAME, optionally NEW-NAME, and whether to FORCE.
Returns a plist with :status, :data, and :raw-text."
  (interactive
   (let* ((name (read-string "Name (LAST NAME, FIRST NAME): "))
          (new-name-input (read-string "New name (leave blank for none): "))
          (new-name (unless (string-empty-p new-name-input) new-name-input))
          (force (y-or-n-p "Force insert even if similar names exist? ")))
     (list name new-name force)))
  (tlon-db-ensure-auth)
  (let* ((payload `(("name" . ,name)))
         (payload (if new-name (append payload `(("new_name" . ,new-name))) payload))
         (payload (if force (append payload '(("force" . t))) payload))
         (data (json-encode payload))
         (headers '(("Content-Type" . "application/json")
                    ("accept" . "application/json")))
         (response-buffer (tlon-db--make-request "POST" "/api/names/" data headers t))
         status-code response-data raw-response-text)
    (when response-buffer
      (setq raw-response-text (with-current-buffer response-buffer (buffer-string)))
      (setq status-code (tlon-db--get-response-status-code response-buffer))
      (setq response-data (tlon-db--parse-json-response response-buffer))
      (kill-buffer response-buffer))
    (let ((result (list :status status-code
                        :data response-data
                        :raw-text raw-response-text)))
      (if (or tlon-debug (not (and status-code (= status-code 200))))
          (tlon-db--display-result-buffer
           (format "Set name result (Status: %s)"
                   (if status-code (number-to-string status-code) "N/A"))
           #'tlon-db--format-set-name-result
           result)
        (message "Name “%s” inserted/updated successfully." name))
      result)))

(defun tlon-db--format-set-name-result (result)
  "Format RESULT from `tlon-db-set-name` for display.
RESULT is a plist with :status, :data, and :raw-text."
  (let ((status (plist-get result :status))
        (data (plist-get result :data))
        (raw  (plist-get result :raw-text)))
    (cond
     ((null status)
      (insert "Status: Request Failed\n")
      (insert (or raw "No specific error message.")))
     ((= status 200)
      (insert "Status: Success (200)\n")
      (when data
        (insert "Response:\n")
        (insert (format "%s\n" data))))
     ((= status 409)
      (insert "Status: Conflict (409)\n")
      (when data
        (when (gethash "message" data)
          (insert "Message: " (gethash "message" data) "\n"))
        (when (gethash "similar_names" data)
          (insert "Similar names:\n")
          (dolist (n (gethash "similar_names" data))
            (insert "  - " n "\n")))))
     ((= status 422)
      (insert "Status: Validation Error (422)\n")
      (when data
        (insert (format "%s\n" data))))
     (t
      (insert (format "Status: Error (HTTP %d)\n" status))
      (insert (or raw "No content."))))))

;;;;;; Get translation key

(defun tlon-db-get-translation-key (original-key &optional lang)
  "Return the BibTeX key of the translation of ORIGINAL-KEY in language LANG.

If called interactively, prompt for ORIGINAL-KEY (defaulting to the BibTeX key
at point) and LANG when they are not provided.  LANG should be an ISO language
code such as \"fr\".

On success, the translation key is returned as a string.  If no translation in
LANG exists or the request fails, signal an error."
  (interactive
   (list (or (tlon-db-get-key-at-point)
             (read-string "Original key: "))
         (tlon-read-language nil "Target language: ")))
  (unless lang
    (setq lang (tlon-read-language nil "Target language: ")))
  (let* ((endpoint (format "/api/translations/%s"
                           (url-hexify-string original-key)))
         (headers '(("accept" . "application/json")))
         (response-buffer (tlon-db--make-request "GET" endpoint nil headers nil))
         (data (and response-buffer
                    (tlon-db--parse-json-response response-buffer))))
    (when response-buffer
      (kill-buffer response-buffer))
    (unless data
      (user-error "Failed to retrieve translations for %s" original-key))
    (let* ((translations (gethash "translations" data))
           (match (cl-find (downcase lang) translations
                           :key (lambda (tr)
                                  (downcase (or (gethash "language" tr) "")))
                           :test #'string=)))
      (if match
          (let ((translation-key (gethash "key" match)))
            (when (called-interactively-p 'any)
              (message "Translation key for %s in %s: %s"
                       original-key lang translation-key))
            translation-key)
        (user-error "No translation found for %s in language %s"
                    original-key lang)))))

;;;;;; Helpers

(defun tlon-db--get-response-body (response-text)
  "Extract the body from a full HTTP RESPONSE-TEXT."
  (when response-text
    (if (string-match "\r?\n\r?\n" response-text)
        (substring response-text (match-end 0))
      response-text)))

(defun tlon-db--extract-missing-names (raw-response-text)
  "Return a list of missing author names from RAW-RESPONSE-TEXT.
The server returns a JSON body whose `detail' list may contain
`missing_names'.  Parse that structure and return the names, or nil if none
can be found."
  (when (and raw-response-text (string-match-p "missing_names" raw-response-text))
    (let* ((body (tlon-db--get-response-body raw-response-text))
           (json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (data (condition-case nil
                     (json-read-from-string body)
                   (error nil)))
           (names '()))
      (when (and data (hash-table-p data))
        (let ((detail (gethash "detail" data)))
          (when (listp detail)
            (dolist (item detail)
              (when (and (hash-table-p item)
                         (gethash "missing_names" item))
                (setq names (append names (gethash "missing_names" item))))))))
      (unless (null names) names))))

;;;###autoload
(defun tlon-db--make-request (method endpoint data headers &optional auth-required base-url)
  "Make an HTTP request to the EA International API.
METHOD is the HTTP method (e.g., \"GET\", \"POST\").
ENDPOINT is the API endpoint path (e.g., \"/api/entries\").
DATA is the request body data (string or nil).
HEADERS is an alist of extra request headers.
AUTH-REQUIRED non-nil means authentication token will be added.
Optional BASE-URL overrides `tlon-db-api-base-url'."
  (let* ((url-request-method method)
	 (url-request-data data)
	 (url-request-extra-headers (copy-alist headers))
	 (base (or base-url tlon-db-api-base-url))
	 (url (concat base endpoint))
	 (lisp-error-occurred nil)
	 (lisp-error-message nil)
	 response-buffer url-request-status url-request-error-message url-http-response-buffer url-debug)
    (when auth-required
      (tlon-db-ensure-auth)
      (add-to-list 'url-request-extra-headers
		   `("Authorization" . ,(concat "Bearer " tlon-db-auth-token)) t))
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

(defun tlon-db--get-response-status-code (buffer)
  "Extract HTTP status code from response BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((status-line (buffer-substring-no-properties (point) (line-end-position))))
      (if (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
	  (string-to-number (match-string 1 status-line))
	(user-error "Could not parse HTTP status line: %s" status-line)))))

(defun tlon-db--parse-json-response (buffer)
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

(defun tlon-db--display-result-buffer (title formatter-fn data)
  "Display TITLE and formatted DATA (via FORMATTER-FN) in a dedicated buffer.
The buffer is named by `tlon-db--result-buffer-name`.
TITLE is the initial title string for the buffer content.
FORMATTER-FN is a function that takes DATA and inserts formatted content
into the current buffer."
  (let ((result-buffer (get-buffer-create tlon-db--result-buffer-name)))
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

(defun tlon-db--format-post-entry-result (result)
  "Format the RESULT from `tlon-db-post-entry` for display.
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

;;;;;; Sync

(defun tlon-db--initialize-sync ()
  "Set up file notification to sync `tlon-db-file-db` on change."
  (when (and (not tlon-db--db-watch-descriptor) (file-exists-p tlon-db-file-db))
    ;; Add the watch.
    (setq tlon-db--db-watch-descriptor
          (file-notify-add-watch tlon-db-file-db '(change) #'tlon-db--sync-on-change))
    ;; Ensure cleanup on exit.
    (add-hook 'kill-emacs-hook #'tlon-db-remove-file-notify-watch nil t)))

(defun tlon-db-remove-file-notify-watch ()
  "Remove file notification watch."
  (when tlon-db--db-watch-descriptor
    (file-notify-rm-watch tlon-db--db-watch-descriptor)
    (setq tlon-db--db-watch-descriptor nil)))

(defun tlon-db--get-key-from-bibtex-line (line)
  "Extract BibTeX key from a LINE if it's an entry start."
  (when (string-match "^[ +-]?@\\w+{\\s-*\\([^, \t\n]+\\)" line)
    (match-string 1 line)))

(defun tlon-db--get-changed-keys-from-diff (diff-output)
  "Parse unified DIFF-OUTPUT and return a plist of changed keys."
  (let (added-keys-raw deleted-keys-raw)
    (with-temp-buffer
      (insert diff-output)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (thing-at-point 'line))
               (is-add (string-prefix-p "+" line))
               (is-del (string-prefix-p "-" line)))
          (when (or is-add is-del)
            (let ((key-for-change
                   (save-excursion
                     (catch 'key-found
                       (while (and (not (bobp)) (not (looking-at "@@")))
                         (when-let ((found-key (tlon-db--get-key-from-bibtex-line (thing-at-point 'line))))
                           (throw 'key-found found-key))
                         (forward-line -1))
                       nil)))) ; Return nil if loop finishes
              (when key-for-change
                (if is-add
                    (cl-pushnew key-for-change added-keys-raw :test #'string=)
                  (cl-pushnew key-for-change deleted-keys-raw :test #'string=))))))
        (forward-line 1)))
    (let ((modified (cl-intersection added-keys-raw deleted-keys-raw :test #'string=)))
      (list :added (cl-set-difference added-keys-raw modified :test #'string=)
            :deleted (cl-set-difference deleted-keys-raw modified :test #'string=)
            :modified modified))))

(defun tlon-db--log-sync-action (action key)
  "Append a sync ACTION for a given KEY to the sync log buffer."
  (with-current-buffer (get-buffer-create tlon-db--sync-log-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "  - %s: %s\n" action key)))))

(defun tlon-db--log-sync-action-modified (key before after)
  "Log modification of KEY with diff between BEFORE and AFTER."
  (with-current-buffer (get-buffer-create tlon-db--sync-log-buffer-name)
    (let* ((inhibit-read-only t)
           (before-file (make-temp-file "db-sync-before-"))
           (after-file (make-temp-file "db-sync-after-"))
           (diff-output ""))
      (unwind-protect
          (progn
            (with-temp-buffer
              (insert (or before ""))
              (write-file before-file nil))
            (with-temp-buffer
              (insert (or after ""))
              (write-file after-file nil))
            (with-temp-buffer
              (call-process "diff" nil t nil "-U1000" before-file after-file)
              (setq diff-output (buffer-string))))
        (delete-file before-file)
        (delete-file after-file))
      (goto-char (point-max))
      (insert (format "  - Modified: %s\n" key))
      (when (string-match-p "@@" diff-output)
        (insert "    Diff:\n")
        (with-temp-buffer
          (insert diff-output)
          (goto-char (point-min))
          (when (search-forward-regexp "^@@" nil t)
            (goto-char (line-beginning-position))
            (dolist (line (split-string (buffer-substring-no-properties (point) (point-max)) "\n" t))
              (insert (format "      %s\n" line)))))))))

(defun tlon-db--sync-on-change (event)
  "Callback for `filenotify' to sync `db.bib' modifications.
EVENT is a list of the form (FILE ACTION)."
  (let ((action (nth 1 event))
        (file (nth 2 event)))
    (when (and (eq action 'changed)
               (string-equal file (expand-file-name tlon-db-file-db))
               (file-exists-p tlon-db-file-db-upstream))
      (if tlon-db--sync-in-progress
          (message "Db sync already in progress, skipping this change.")
        (when-let* ((diff-output (tlon-db--get-diff-output tlon-db-file-db-upstream file)))
          (tlon-db--process-sync-changes diff-output file))))))

(defun tlon-db--process-sync-changes (diff-output file)
  "Process sync differences in DIFF-OUTPUT for FILE."
  (with-current-buffer (find-file-noselect file)
    (unwind-protect
        (progn
          (setq tlon-db--sync-in-progress t)
          (let* ((changes (tlon-db--get-changed-keys-from-diff diff-output))
                 (added (plist-get changes :added))
                 (deleted (plist-get changes :deleted))
                 (modified (plist-get changes :modified))
                 (proceed
                  (let ((num (+ (length added) (length modified))))
                    (or (< num 10)
                        (y-or-n-p (format "Db sync will create/modify %d entries. Proceed? " num))))))
            (when (and proceed (or added deleted modified))
              (with-current-buffer (get-buffer-create tlon-db--sync-log-buffer-name)
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (unless (bolp) (insert "\n"))
                  (insert (format-time-string "*** Sync on %Y-%m-%d %H:%M:%S ***\n"))))
              (let ((created-count 0)
                    (modified-count 0)
                    (deleted-count 0)
                    (parts '()))
                (dolist (key added)
                  (tlon-db-post-entry key)
                  (setq created-count (1+ created-count))
                  (tlon-db--log-sync-action "Created" key))
                (dolist (key modified)
                  (let ((before-text (with-current-buffer (find-file-noselect tlon-db-file-db-upstream)
                                       (bibtex-extras-get-entry-as-string key nil))))
                    (tlon-db-post-entry key)
                    (setq modified-count (1+ modified-count))
                    (let ((after-text (bibtex-extras-get-entry-as-string key nil)))
                      (tlon-db--log-sync-action-modified key before-text after-text))))
                (dolist (key deleted)
                  (tlon-db-delete-entry key nil t)
                  (setq deleted-count (1+ deleted-count))
                  (tlon-db--log-sync-action "Deleted" key))
                (when (> created-count 0) (push (format "%d created" created-count) parts))
                (when (> modified-count 0) (push (format "%d modified" modified-count) parts))
                (when (> deleted-count 0) (push (format "%d deleted" deleted-count) parts))
                (when parts
                  (run-with-timer 3 nil
                                  (lambda ()
                                    (message "Db sync: %s."
                                             (mapconcat #'identity (nreverse parts) ", ")))))))))
      (setq tlon-db--sync-in-progress nil))))

(defun tlon-db--get-diff-output (upstream-file local-file)
  "Return unified diff output between UPSTREAM-FILE and LOCAL-FILE.
If there are no differences, return nil."
  (let (diff-output)
    (let ((diff-buffer (generate-new-buffer " *db-diff*")))
      (unwind-protect
	  (let ((exit-code
		 (call-process "diff" nil diff-buffer nil "-U1000"
			       upstream-file local-file)))
	    (when (= exit-code 1)		; 0 means identical, >1 error
	      (with-current-buffer diff-buffer
		(setq diff-output (buffer-string)))))
	(when (buffer-live-p diff-buffer)
	  (kill-buffer diff-buffer))))
    diff-output))

;;;;;; Periodic data update

(run-with-idle-timer (* 3 60 60) t #'tlon-db-get-entries)

;;;;; Menu

;;;###autoload (autoload 'tlon-db-menu "tlon-db" nil t)
(transient-define-prefix tlon-db-menu ()
  "`tlon-db' menu."
  ["Db Actions"
   ("g" "Get entries" tlon-db-get-entries)
   ("p" "Post entry" tlon-db-post-entry)
   ("d" "Delete entry" tlon-db-delete-entry)
   ("c" "Check name" tlon-db-check-name)
   ("i" "Check or insert name" tlon-db-check-or-insert-name)
   ("s" "Set name" tlon-db-set-name)
   ""
   ("a" "Authenticate" tlon-db-authenticate)])

(tlon-db-initialize)

(provide 'tlon-db)
;;; tlon-db.el ends here
