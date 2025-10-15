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
(require 'diff-mode)
(require 'ebib)
(require 'tlon-core)
(require 'transient)
(require 'bibtex-extras)
(require 'cl-lib)
(require 'subr-x)
(require 'url-http)               ; ensure URL variables are declared as special
(require 'filenotify)

;;;; Variables

;;;;; Auth

(defvar tlon-db-auth-token nil
  "Authentication token for EA International API.")

(defvar tlon-db-auth-token-expiry nil
  "Expiry time for the authentication token.")

(defgroup tlon-db nil
  "Db integration with EA International."
  :group 'tlon)

(defvar tlon-db-api-base-url "https://ea.international"
  "Base URL for the EA International API.")

(defcustom tlon-db-use-local-environment nil
  "Whether to use the local EA International development API.

When non-nil, all requests are sent to
\"https://local-dev.ea.international\" and the mkcert root
certificate located at
\"~/Library/Application Support/mkcert/RootCA.pem\" is added to
`gnutls-trustfiles'."
  :type 'boolean
  :group 'tlon-db)

(defcustom tlon-db-enable-auto-sync nil
  "When non-nil, automatically sync `db.bib` changes detected by the file-watch.
The file-watch installed by `tlon-db--initialize-sync' is always active, but if
this option is nil the callback does nothing, effectively pausing automatic
synchronization."
  :type 'boolean
  :group 'tlon-db)

;;;;; Periodic refresh

(defvar tlon-db--get-entries-timer nil
  "Timer for automatic `tlon-db-get-entries-no-confirm' calls.")

(defun tlon-db--set-get-entries-interval (symbol value)
  "Set SYMBOL to VALUE and restart the periodic refresh timer."
  (set-default symbol value)
  (when (timerp tlon-db--get-entries-timer)
    (cancel-timer tlon-db--get-entries-timer))
  (setq tlon-db--get-entries-timer
        (run-with-idle-timer value t #'tlon-db-get-entries-no-confirm)))

(defcustom tlon-db-get-entries-interval (* 3 60 60)
  "Idle seconds before automatically refreshing the local BibTeX database.

Changing this variable interactively restarts the underlying idle
timer so the new value takes effect immediately."
  :type 'integer
  :set #'tlon-db--set-get-entries-interval
  :group 'tlon-db)

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

(declare-function citar-extras-refresh-bibliography "citar-extras")
(defun tlon-db-initialize ()
  "Initialize the `tlon-db' package."
  (tlon-db--initialize-sync)
  (append paths-files-bibliography-all (list tlon-db-file-db))
  (citar-extras-refresh-bibliography tlon-db-file-db))

(defvar citar-bibliography)
(declare-function citar-select-ref "citar")
(defun tlon-db-get-db-entries ()
  "Prompt the user to select a work in the db and return its key."
  (let ((citar-bibliography (list tlon-file-db)))
    (citar-select-ref)))

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
(defun tlon-db-get-entries (&optional base-url no-confirm)
  "Retrieve entries from the EA International API and update local db.
Optional BASE-URL specifies the API endpoint base URL.  If not
provided, defaults to `tlon-db-api-base-url'.

If NO-CONFIRM is non-nil, bypass the check that ensures `db.bib' and
`db-upstream.bib' are identical.  Interactively, supply a prefix
argument to enable this check bypass."
  (interactive "P")
  (let ((no-confirm (or no-confirm current-prefix-arg)))
    (when (and (get-file-buffer tlon-db-file-db)
               (buffer-modified-p (get-file-buffer tlon-db-file-db)))
      (user-error "Buffer for %s has unsaved changes. Save it first" (file-name-nondirectory tlon-db-file-db)))
    (when (and (not no-confirm)
               (file-exists-p tlon-db-file-db)
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
	(user-error "Failed to retrieve entries")))))

(defun tlon-db-get-entries-no-confirm ()
  "Retrieve entries from the EA International API without confirmation."
  (interactive)
  (tlon-db-get-entries nil t))

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
If called interactively, post the entry at point; otherwise use KEY.

ATTEMPT is used to track retries in case of missing author names."
  (interactive)
  (setq attempt (or attempt 0))
  (tlon-db-ensure-auth)
  (if-let ((entry-key (or key (tlon-get-key-at-point))))
      (progn
	(let* ((entry-text   (tlon-db--normalize-author-field (bibtex-extras-get-entry-as-string entry-key nil)))
	       (encoded-text (encode-coding-string entry-text 'utf-8))
	       (headers      '(("Content-Type" . "text/plain; charset=utf-8")
			       ("accept"       . "text/plain")))
	       (result       (tlon-db--handle-entry-request
			      "POST" "/api/entries" encoded-text headers))
	       (status-code  (plist-get result :status))
	       (raw-text     (plist-get result :raw-text)))
	  ;; Detect missing author names (HTTP 400).  Offer to create *each*
	  ;; missing name (splitting compound author fields) and retry once.  If
	  ;; the user declines, abort silently (no result buffer).
	  (when (and status-code (= status-code 400) (< attempt 1))
	    (let* ((missing-names (tlon-db--extract-missing-names raw-text))
		   ;; Split any compound author strings like
		   ;; \"Doe, John and Roe, Jane\" into individual names.
		   (all-names (cl-loop for n in missing-names
				       append (tlon-db--split-author-string n))))
	      (when all-names
		(if (y-or-n-p
		     (format "Missing author names not found (%s). Create them and retry? "
			     (string-join all-names ", ")))
		    (progn
		      (dolist (name all-names)
			(tlon-db-set-name name nil))
		      (cl-return-from tlon-db-post-entry
			(tlon-db-post-entry entry-key (1+ attempt))))
		  (user-error "Entry not posted because these author names are missing: %s"
			      (string-join all-names ", "))))))
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
	      (message "Entry “%s” posted and mirrored locally." entry-key)))
	  ;; TODO: fix this; currently it always throws an error
	  ;; (tlon-db-ensure-key-is-unique entry-key)
	  ))
    (user-error "No BibTeX key found at point")))

;; Helper to split a possibly multi-author string into individual names.
(defun tlon-db--split-author-string (author-string)
  "Split AUTHOR-STRING on the BibTeX separator \" and \" and normalize spaces.
Return a list of individual author names."
  (mapcar #'tlon-db--normalize-name
          (split-string
           (replace-regexp-in-string "[[:space:]\n]+" " " author-string)
           "\\s-+and\\s-+" t)))

(defun tlon-db--normalize-name (name)
  "Normalize NAME by converting non-breaking spaces to regular spaces."
  (string-trim
   (replace-regexp-in-string "[[:space:]\u00A0\u202F]+" " " name)))

(defun tlon-db--collapse-whitespace (s)
  "Normalise whitespace in S while *preserving* non‑breaking spaces.

Steps:
1. Convert TAB, CR and LF to an ordinary space.
2. Collapse any run of SPACE (U+0020), NBSP (U+00A0) or NNBSP (U+202F)
   to a single occurrence of *the character that opened the run*.
   This keeps the original code‑point if the user deliberately typed a
   thin space, which the EA International Names API requires in order to
   match certain translated author names."
  (let ((tmp (replace-regexp-in-string "[\t\n\r]+" " " s)))
    (replace-regexp-in-string
     "\\([ \u00A0\u202F]\\)\\(?:[ \u00A0\u202F]+\\)" "\\1" tmp)))

(defun tlon-db--normalize-author-field (entry)
  "Collapse newlines and multiple spaces inside the author field of ENTRY.
Return the modified entry string."
  (let ((case-fold-search t))
    (if (string-match "\\(author[[:space:]]*=[[:space:]]*{\\)\\([^}]*\\)\\(}\\)" entry)
        (let* ((prefix  (match-string 1 entry))
               (authors (match-string 2 entry))
               (suffix  (match-string 3 entry))
               (clean   (string-trim
                         (tlon-db--collapse-whitespace authors))))
          (replace-match (concat prefix clean suffix) t t entry 0))
      entry)))

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

;;;;;; Move entry

(declare-function tlon-ensure-bib "tlon-bib")
(declare-function ebib-extras-reload-database-no-confirm "ebib-extras")
(declare-function citar-extras-goto-bibtex-entry "citar-extras")
(declare-function ebib-extras-get-db-number "ebib-extras")
;;;###autoload
(defun tlon-db-move-entry (&optional key)
  "Move the entry with KEY to the database.
If KEY is nil, use the key of the entry at point."
  (interactive)
  (tlon-ensure-bib)
  (let* ((key (or key (tlon-get-key-at-point)))
         current-file
         filename)
    (citar-extras-goto-bibtex-entry key)
    (setq current-file (buffer-file-name))
    (setq filename (and current-file (file-name-nondirectory current-file)))
    (if (and current-file
             (or (file-equal-p (expand-file-name current-file)
                               (expand-file-name tlon-db-file-db))
                 (file-equal-p (expand-file-name current-file)
                               (expand-file-name tlon-db-file-db-upstream))))
        (message "Entry \"%s\" is already in db; no action taken." key)
      (tlon-db-post-entry)
      (bibtex-kill-entry)
      (save-buffer)
      (kill-buffer)
      (let* ((db-index (ebib-extras-get-db-number tlon-db-file-db))
             (db (tlon-db--get-ebib-database db-index)))
        (if db
            (ebib-extras-reload-database-no-confirm db)
          (message "Ebib database for %s not found; skipped reload" tlon-db-file-db)))
      (run-with-timer 3 nil
                      (lambda ()
                        (message "Entry \"%s\" added to db and removed from \"%s\"." key filename))))))

(defun tlon-db--get-ebib-database (db-index)
  "Return the Ebib database object at DB-INDEX or nil if not found.
Handles both vector and list representations of `ebib--databases'."
  (cond
   ((and (vectorp ebib--databases)
         (integerp db-index)
         (>= db-index 0)
         (< db-index (length ebib--databases)))
    (aref ebib--databases db-index))
   ((and (listp ebib--databases)
         (integerp db-index))
    (nth db-index ebib--databases))
   (t nil)))

(declare-function tlon-yaml-get-key "tlon-yaml")
(declare-function tlon-get-bibtex-key "tlon-yaml")
(declare-function tlon-get-key-at-point "tlon-bib")
(declare-function ebib-extras-get-file-of-key "ebib-extras")
(declare-function bibtex-extras-get-field "bibtex-extras")
(declare-function tlon-get-counterpart-key "tlon-counterpart")
(declare-function citar-extras-refresh-bibliography "citar-extras")
;;;###autoload
(defun tlon-db-move-entry-pair ()
  "Move the entry at point and its counterpart to the database."
  (interactive)
  (let* ((key (or (tlon-get-key-at-point)
                  (tlon-get-bibtex-key)
                  (when (derived-mode-p 'markdown-mode)
                    (tlon-yaml-get-key "key"))))
         (file (and key (ebib-extras-get-file-of-key key)))
         orig-key trans-key)
    (unless key
      (user-error "Could not determine BibTeX key"))
    (unless file
      (user-error "Could not locate file for key “%s”" key))
    (with-current-buffer (find-file-noselect file)
      (widen)
      (bibtex-search-entry key)
      (if (bibtex-extras-get-field "translation")
          (setq trans-key key)
        (setq orig-key key)))
    (citar-extras-refresh-bibliography file 'force)
    (if trans-key
        (setq orig-key (tlon-get-counterpart-key key "en"))
      (let ((lang (tlon-select-language 'code 'babel "Translation language: ")))
        (setq trans-key (tlon-get-counterpart-key key lang))))
    (when orig-key
      (tlon-db-move-entry orig-key))
    (if trans-key
        (tlon-db-move-entry trans-key)
      (message "No translation entry found. Call `M-x tlon-db-move-entry' manually from the translated BibTeX entry."))))
;;;;;; Delete entry

(defun tlon-db-delete-entry (key &optional no-confirm locally)
  "Delete KEY from the EA International API.
If LOCALLY is non-nil, also delete from local db file.
Interactively, NO-CONFIRM is set with a prefix argument, and LOCALLY is t."
  (interactive (list (or (tlon-get-key-at-point)
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

;;;;;; Get entry

(cl-defun tlon-db-get-entry (&optional entry-id)
  "Retrieve ENTRY-ID from the EA International API and display it.

If called interactively, prompt for ENTRY-ID."
  (interactive)
  (let* ((entry-id (or entry-id
                       (read-string "Entry ID: ")))
         (endpoint (format "/api/entries/%s" (url-hexify-string entry-id)))
         (headers '(("accept" . "text/plain")))
         (result (tlon-db--handle-entry-request
                  "GET" endpoint nil headers nil)))
    (tlon-db--display-result-buffer
     (format "Get entry result (Status: %s)"
             (or (plist-get result :status) "N/A"))
     #'tlon-db--format-get-entry-result
     result)))

(defun tlon-db--format-get-entry-result (result)
  "Format RESULT from `tlon-db-get-entry` for display."
  (let ((status (plist-get result :status))
        (raw    (plist-get result :raw-text)))
    (insert (format "HTTP status: %s\n\n" (or status "N/A")))
    (if raw
        (insert (tlon-db--get-response-body raw))
      (insert "No content returned."))))

;;;;;; Check name

(defun tlon-db-check-name (name)
  "Check if NAME exists in the EA International database."
  (interactive "sName to check: ")
  (tlon-db-ensure-auth)
  (let* ((endpoint (format "/api/names/%s" (url-hexify-string name)))
	 (headers '(("accept" . "application/json")))
	 (response-buffer (tlon-db--make-request "GET" endpoint nil headers t))
	 response-data
	 status-code)
    (when response-buffer
      (setq status-code (tlon-db--get-response-status-code response-buffer))
      (setq response-data (tlon-db--parse-json-response response-buffer))
      (unless (= status-code 200)
        (message "Name lookup returned HTTP status %d" status-code)) ; Informative but non-blocking
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
  "Format DATA returned by the name lookup endpoint for display.

DATA can be:
1. A list of name objects, each with \"name\" and \"translations\".
2. A hash table with a \"message\" key (e.g. 404 not found).
3. Any other hash table or nil."
  (cond
   ;; No data
   ((null data)
    (insert "No data returned from API or an error occurred parsing the response.\n"))
   ;; List of names with their translations
   ((listp data)
    (if (null data)
        (insert "No matching names found.\n")
      (dolist (item data)
        (when (hash-table-p item)
          (insert (format "Name: %s\n" (gethash "name" item)))
          (when-let ((translations (gethash "translations" item)))
            (insert "Translations:\n")
            (maphash (lambda (lang trans)
                       (insert (format "  %s: %s\n" lang trans)))
                     translations))
          (insert "\n")))))
   ;; Hash table with a message (e.g. 404)
   ((and (hash-table-p data) (gethash "message" data))
    (insert (gethash "message" data) "\n"))
   ;; Generic hash table fallback
   ((hash-table-p data)
    (maphash (lambda (k v)
               (insert (format "%s: %s\n" k v)))
             data))
   ;; Anything else
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
  ;; Normalize names to avoid Unicode space issues.
  (setq name (tlon-db--normalize-name name))
  (when new-name
    (setq new-name (tlon-db--normalize-name new-name)))
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

;;;;;; Set publication status

(defconst tlon-db-publication-statuses
  '("production" "testing" "unpublished")
  "List of valid publication statuses for entries.")

(defun tlon-db-set-publication-status (&optional entry-id status)
  "Set publication STATUS for ENTRY-ID using the EA International API.
When called interactively, determine ENTRY-ID from context: in BibTeX/Ebib
buffers use the BibTeX key at point; in Markdown buffers read the YAML
\"key\" field via `tlon-yaml-get-key'.  Offer one of \"production\",
\"testing\" or \"unpublished\", defaulting to the entry's current status
when known.  On success, show a concise message; on error or when
`tlon-debug' is non-nil, display details in *Db API Result*."
  (interactive)
  (tlon-db-ensure-auth)
  (let* ((id (or entry-id
                 (tlon-db--entry-id-from-context)
                 (read-string "Entry key: ")))
         (current (or (tlon-db--get-publication-status id) "production"))
         (pub-status (or status
                         (completing-read "Publication status: " tlon-db-publication-statuses nil t nil nil current)))
         (data (json-encode `(("publication_status" . ,pub-status))))
         (headers '(("Content-Type" . "application/json")
                    ("accept" . "application/json")))
         (endpoint (format "/api/publication_status/set/%s" (url-hexify-string id)))
         status-code response-data raw-response-text response-buffer)
    (setq response-buffer (tlon-db--make-request "POST" endpoint data headers t))
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
           (format "Set publication status (Status: %s)"
                   (if status-code (number-to-string status-code) "N/A"))
           #'tlon-db--format-set-publication-status-result
           result)
        (message "Publication status for “%s” set to “%s”." id pub-status))
      result)))

(defun tlon-db--get-publication-status (entry-id)
  "Return publication status for ENTRY-ID, or nil on failure."
  (let* ((endpoint (format "/api/publication_status/get/%s"
                           (url-hexify-string entry-id)))
         (headers '(("accept" . "application/json")))
         (buf (tlon-db--make-request "GET" endpoint nil headers t))
         code data)
    (when buf
      (setq code (tlon-db--get-response-status-code buf))
      (setq data (tlon-db--parse-json-response buf))
      (kill-buffer buf))
    (when (and code (= code 200))
      (cond
       ((stringp data) (downcase data))
       ((hash-table-p data)
        (let ((val (or (gethash "publication_status" data)
                       (gethash "status" data))))
          (and val (downcase val))))
       (t nil)))))

(defun tlon-db--entry-id-from-context ()
  "Return BibTeX key from current context or nil if not found."
  (cond
   ((derived-mode-p 'markdown-mode)
    (tlon-yaml-get-key "key"))
   (t
    (tlon-get-key-at-point))))

(defun tlon-db--format-set-publication-status-result (result)
  "Format RESULT from `tlon-db-set-publication-status' for display."
  (let ((status (plist-get result :status))
        (data   (plist-get result :data))
        (raw    (plist-get result :raw-text)))
    (cond
     ((null status)
      (insert "Status: Request Failed\n")
      (insert (or raw "No specific error message.")))
     ((= status 200)
      (insert "Status: Success (200)\n")
      (cond
       ((hash-table-p data)
        (maphash (lambda (k v)
                   (insert (format "%s: %s\n" k v)))
                 data))
       (data
        (insert (format "%s\n" data)))
       (t
        (insert (or raw "No content returned.")))))
     ((= status 422)
      (insert "Status: Validation Error (422)\n")
      (when data
        (let ((detail (and (hash-table-p data) (gethash "detail" data))))
          (cond
           ((listp detail)
            (dolist (item detail)
              (if (hash-table-p item)
                  (insert (format "  - Field: %s, Error: %s\n"
                                  (mapconcat #'identity (gethash "loc" item) ".")
                                  (gethash "msg" item "")))
                (insert (format "  - %s\n" item)))))
           (t
            (insert (format "%s\n" data))))))
      (when raw
        (insert "\nRaw server response:\n" raw)))
     (t
      (insert (format "Status: Error (HTTP %d)\n" status))
      (insert (or raw "No content."))))))

;;;;;; Get translation key

;;;###autoload
(defun tlon-db-get-translation-key (original-key &optional lang interactive-call-p)
  "Return the BibTeX key of the translation of ORIGINAL-KEY in language LANG.
If called interactively, prompt for ORIGINAL-KEY (defaulting to the BibTeX key
at point) and LANG when they are not provided.  LANG should be an ISO language
code such as \"fr\".

When INTERACTIVE-CALL-P is non-nil, the function also echoes the
translation key in the echo area.  This argument is supplied
automatically when the command is invoked interactively.

On success, the translation key is returned as a string.  If no translation in
LANG exists or the request fails, signal an error."
  (interactive
   (list (or (tlon-get-key-at-point)
             (read-string "Original key: "))
         (tlon-select-language 'code)
         t))
  (unless lang
    (setq lang (tlon-read-language nil "Target language: ")))
  (let* ((language (tlon-lookup tlon-languages-properties :standard :code lang))
	 (endpoint (format "/api/translations/%s" (url-hexify-string original-key)))
         (headers '(("accept" . "application/json")))
         (response-buffer (tlon-db--make-request "GET" endpoint nil headers nil))
         (data (and response-buffer
                    (tlon-db--parse-json-response response-buffer))))
    (when response-buffer
      (kill-buffer response-buffer))
    (unless data
      (user-error "Failed to retrieve translations for %s" original-key))
    (let* ((translations (gethash "translations" data))
           (match (cl-find language translations
                           :key (lambda (tr)
                                  (downcase (or (gethash "language" tr) "")))
                           :test #'string=)))
      (if match
          (let ((translation-key (gethash "key" match)))
            (when interactive-call-p
              (message "Translation key for %s in %s: %s"
                       original-key lang translation-key))
            translation-key)
        (user-error "No translation found for %s in language %s"
                    original-key lang)))))

;;;;;; Get translations

(cl-defun tlon-db-get-translations (&optional original-key)
  "Return a list of translations for ORIGINAL-KEY.
Each element is a plist with the properties :key and :language.

If called interactively, prompt for ORIGINAL-KEY (defaulting to the
BibTeX key at point) and display the result in the *Db API Result*
buffer.  From Lisp, return the list directly."
  (interactive
   (list (or (tlon-get-key-at-point)
             (read-string "Original key: "))))
  (unless original-key
    (setq original-key (tlon-get-key-at-point)))
  (let* ((endpoint (format "/api/translations/%s"
                            (url-hexify-string original-key)))
         (headers '(("accept" . "application/json")))
         (response-buffer (tlon-db--make-request
                           "GET" endpoint nil headers nil))
         (json-data (and response-buffer
                         (tlon-db--parse-json-response response-buffer))))
    (when response-buffer
      (kill-buffer response-buffer))
    (unless json-data
      (user-error "Failed to retrieve translations for %s" original-key))
    (let* ((translations (gethash "translations" json-data))
           (result (mapcar (lambda (tr)
                             (list :key      (gethash "key" tr)
                                   :language (gethash "language" tr)))
                           translations)))
      (if (called-interactively-p 'any)
          (tlon-db--display-result-buffer
           (format "Translations for %s" original-key)
           (lambda (data)
             (if data
                 (dolist (item data)
                   (insert (format "%s  (%s)\n"
                                   (plist-get item :key)
                                   (plist-get item :language))))
               (insert "No translations found.\n")))
           result)
        result))))

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
      (unless (null names)
        (mapcar #'tlon-db--normalize-name names)))))

(defun tlon-db--current-base-url ()
  "Return the base URL according to `tlon-db-use-local-environment'."
  (if tlon-db-use-local-environment
      "https://local-dev.ea.international"
    tlon-db-api-base-url))

(defvar gnutls-trustfiles)
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
	 (base (or base-url (tlon-db--current-base-url)))
	 (url (concat base endpoint))
         (cert-file (expand-file-name "RootCA.pem" "~/Library/Application Support/mkcert/"))
         (gnutls-trustfiles
          (if tlon-db-use-local-environment
              (let ((list (copy-sequence (or gnutls-trustfiles '()))))
                (unless (member cert-file list)
                  (setq list (cons cert-file list)))
                list)
            gnutls-trustfiles))
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

(defun tlon-db-ensure-key-is-unique (key)
  "Return an error if KEY exists in a file other than \"db.bib\"."
  (let ((bibtex-files (remove tlon-db-file-db bibtex-files)))
    (when (bibtex-search-entry key t)
      (user-error "Key \"%s\" exists outside of \"db.bib\". Remove the duplicate and try again" key))))

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
  "Parse unified DIFF-OUTPUT and classify entry modifications.
Return a plist with the keys :added, :deleted and :modified."
  (let ((table (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert diff-output)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))
               (is-add (string-prefix-p "+" line))
               (is-del (string-prefix-p "-" line)))
          (when (or is-add is-del)
            (let* ((key-for-change
                    (save-excursion
                      (let ((orig-pos (point)))
                        (catch 'key-found
                          ;; First search *backwards* within the current hunk.
                          (while (and (not (bobp)) (not (looking-at "^@@")))
                            (when-let ((found-key
					(tlon-db--get-key-from-bibtex-line
					 (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position)))))
                              (throw 'key-found found-key))
                            (forward-line -1))
                          ;; If nothing found, search *forwards* within the hunk.
                          (goto-char orig-pos)
                          (forward-line 1)
                          (while (and (not (eobp)) (not (looking-at "^@@")))
                            (when-let ((found-key
					(tlon-db--get-key-from-bibtex-line
					 (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position)))))
                              (throw 'key-found found-key))
                            (forward-line 1))
                          nil))))
                   (info (and key-for-change
                              (or (gethash key-for-change table)
                                  (list :added-entry nil
                                        :deleted-entry nil
                                        :added-lines nil
                                        :deleted-lines nil)))))
              (when info
                ;; Detect if the current diff line is the entry start itself.
                (let ((entry-start-p (tlon-db--get-key-from-bibtex-line line)))
                  (when entry-start-p
                    (when is-add (plist-put info :added-entry t))
                    (when is-del (plist-put info :deleted-entry t))))
                ;; Record the presence of any added/deleted lines inside the entry.
                (when is-add (plist-put info :added-lines t))
                (when is-del (plist-put info :deleted-lines t))
                (puthash key-for-change info table))))
          (forward-line 1))))
    (let (added deleted modified)
      (maphash
       (lambda (key info)
         (let ((added-entry   (plist-get info :added-entry))
               (deleted-entry (plist-get info :deleted-entry)))
           (cond
            ;; Newly created entry
            ((and added-entry (not deleted-entry))
             (push key added))
            ;; Completely removed entry
            ((and deleted-entry (not added-entry))
             (push key deleted))
            ;; Anything else counts as a modification
            (t
             (push key modified)))))
       table)
      (list :added (nreverse added)
            :deleted (nreverse deleted)
            :modified (nreverse modified)))))

(defun tlon-db--log-sync-action (action key)
  "Append a sync ACTION for a given KEY to the sync log buffer."
  (with-current-buffer (get-buffer-create tlon-db--sync-log-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "  - %s: %s\n" action key)))))

(defun tlon-db--log-sync-action-modified (key before after)
  "Append a colored, word‑refined diff for the entry KEY.
KEY is the identifier of the database entry being modified.
BEFORE is the previous value of the entry (string or nil).
AFTER is the new value of the entry (string or nil).

The unified diff is produced with the external program \"diff\".
Lines are coloured with the standard `diff-*` faces; if
`diff-refine-hunk` is available the added/removed sub‑words are
highlighted with `diff-refine-added` / `diff-refine-removed`."
  (with-current-buffer (get-buffer-create tlon-db--sync-log-buffer-name)
    (let* ((inhibit-read-only t)
           (diff-exe (or (executable-find "diff")
                         (user-error "Cannot find the external program “diff”")))
           (before-file (make-temp-file "db-sync-before-"))
           (after-file  (make-temp-file "db-sync-after-"))
           diff-hunk)
      (unwind-protect
          (progn
            (with-temp-file before-file (insert (or before "")))
            (with-temp-file after-file  (insert (or after "")))
            ;; Get a *single* unified diff and strip the header
            (setq diff-hunk
                  (with-temp-buffer
                    (call-process diff-exe nil t nil "-U1000" before-file after-file)
                    (goto-char (point-min))
                    (when (re-search-forward "^@@" nil t)
                      (buffer-substring-no-properties (match-beginning 0) (point-max))))))
        (delete-file before-file)
        (delete-file after-file))

      (goto-char (point-max))
      (insert (format "  - Modified: %s\n" key))
      (insert "    Diff:\n")

      ;; ---- Produce coloured / word‑refined hunk in a temp buffer ----
      (let ((refined
             (with-temp-buffer
               (insert diff-hunk)
               (diff-mode)
               (font-lock-ensure)          ; commit line‑wise `diff-*` faces
               ;; Optional: refine word‑wise if available
               (when (fboundp 'diff-refine-hunk)
                 (goto-char (point-min))
                 (while (re-search-forward "^@@" nil t)
                   (diff-refine-hunk)))
               ;; Convert any overlay face produced by `diff-refine-hunk`
               ;; into ordinary text properties, then return the string.
               (mapc (lambda (ov)
                       (when-let ((face (overlay-get ov 'face)))
                         (add-face-text-property (overlay-start ov) (overlay-end ov)
                                                 face 'append))
                       (delete-overlay ov))
                     (overlays-in (point-min) (point-max)))
               (buffer-string))))
        ;; ---- Insert the coloured hunk into the *Db Sync Log* buffer ----
        (dolist (line (split-string refined "\n" t))
          (insert "      ")                ; 6‑space indent, like before
          (let ((start (point)))
            (insert line)
            ;; Ensure basic +/-/@@ faces are present even when `diff-mode`
            ;; is not active in this buffer.
            (add-face-text-property
             start (point)
             (cond ((string-prefix-p "+" line) 'diff-added)
                   ((string-prefix-p "-" line) 'diff-removed)
                   ((string-prefix-p "@@" line) 'diff-hunk-header)
                   (t nil))
             'append))
          (insert "\n"))))))

(defun tlon-db--sync-on-change (event)
  "Callback for `filenotify' to sync `db.bib' modifications.
EVENT is a list of the form (FILE ACTION)."
  (let ((action (nth 1 event))
        (file (nth 2 event)))
    (when (and tlon-db-enable-auto-sync
               (eq action 'changed)
               (string-equal file (expand-file-name tlon-db-file-db))
               (file-exists-p tlon-db-file-db-upstream))
      (if tlon-db--sync-in-progress
          (message "Db sync already in progress, skipping this change.")
        (when-let* ((diff-output (tlon-db--get-diff-output tlon-db-file-db-upstream file)))
          (tlon-db--process-sync-changes diff-output file))))))

;;;###autoload
(defun tlon-db-sync-now ()
  "Run the db sync process used by auto-sync.
Save db.bib if modified, then diff it against db-upstream and process
the changes. If there are no differences, report and do nothing."
  (interactive)
  (unless (file-exists-p tlon-db-file-db-upstream)
    (user-error "Upstream db not found: %s" tlon-db-file-db-upstream))
  (let ((buf (get-file-buffer tlon-db-file-db)))
    (when (and buf (buffer-modified-p buf)
               (y-or-n-p "Save db.bib before syncing? "))
      (with-current-buffer buf (save-buffer))))
  (let ((diff-output (tlon-db--get-diff-output tlon-db-file-db-upstream tlon-db-file-db)))
    (if diff-output
        (tlon-db--process-sync-changes diff-output tlon-db-file-db)
      (message "No differences to sync"))))

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

;; Start (or restart) the periodic refresh timer with the current interval.
(tlon-db--set-get-entries-interval 'tlon-db-get-entries-interval
                                   tlon-db-get-entries-interval)

;;;;; Menu

;;;###autoload (autoload 'tlon-db-menu "tlon-db" nil t)
(transient-define-infix tlon-db-infix-toggle-local-environment ()
  "Toggle whether to use the local EA International API environment."
  :class 'transient-lisp-variable
  :variable 'tlon-db-use-local-environment
  :reader (lambda (_ _ _)
            (tlon-transient-toggle-variable-value 'tlon-db-use-local-environment)))

(transient-define-infix tlon-db-infix-toggle-auto-sync ()
  "Toggle automatic synchronization triggered by `db.bib` changes."
  :class 'transient-lisp-variable
  :variable 'tlon-db-enable-auto-sync
  :reader (lambda (_ _ _)
            (tlon-transient-toggle-variable-value 'tlon-db-enable-auto-sync)))

(transient-define-infix tlon-db-infix-set-get-entries-interval ()
  "Set idle seconds between automatic db refreshes."
  :class 'transient-lisp-variable
  :variable 'tlon-db-get-entries-interval
  :reader (lambda (_ _ _)
            (let ((val (read-number "Seconds between automatic refresh: "
                                    tlon-db-get-entries-interval)))
              (tlon-db--set-get-entries-interval 'tlon-db-get-entries-interval val)
              val)))

(transient-define-prefix tlon-db-menu ()
  "`tlon-db' menu."
  [["Actions"
    ("g" "Get entries (prompt)" tlon-db-get-entries)
    ("G" "Get entries (overwrite)" tlon-db-get-entries-no-confirm)
    ("e" "Get entry" tlon-db-get-entry)
    ("k" "Get translation key" tlon-db-get-translation-key)
    ("p" "Post entry" tlon-db-post-entry)
    ("m" "Move entry to db" tlon-db-move-entry)
    ("M" "Move entry and its counterpart to db" tlon-db-move-entry-pair)
    ("d" "Delete entry" tlon-db-delete-entry)
    ("c" "Check name" tlon-db-check-name)
    ("i" "Check or insert name" tlon-db-check-or-insert-name)
    ("n" "Set name" tlon-db-set-name)
    ("s" "Set publication status" tlon-db-set-publication-status)
    ("y" "Sync db now" tlon-db-sync-now)
    ""
    ("a" "Authenticate" tlon-db-authenticate)]
   ["Options"
    ("-l" "Local env" tlon-db-infix-toggle-local-environment)
    ("-s" "Auto sync" tlon-db-infix-toggle-auto-sync)
    ("-i" "Refresh interval" tlon-db-infix-set-get-entries-interval)]])

(tlon-db-initialize)

(provide 'tlon-db)
;;; tlon-db.el ends here
