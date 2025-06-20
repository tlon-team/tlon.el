;;; tlon-api.el --- Make requests with the Babel APIs -*- lexical-binding: t -*-

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

;; Make requests with the Babel APIs.

;;; Code:

(require 'request)
(require 'tlon-core)
(require 'transient)

;;;; Variables

;; TODO: add `:repo-name' property, then construct `:route' dynamically with
;; `tlon-api-get-routes'
(defconst tlon-uqbar-api-routes
  '((:route "update/babel-refs"
	    :type "POST"
	    :docstring "Apply CSL and regenerate BibTeX keys. Then run \"update/uqbar/es\" request.")
    (:route "update/babel-refs/log"
	    :type "GET"
	    :docstring "Show log of \"update/babel-refs\" request.")
    (:route "update/uqbar/%s"
	    :type "POST"
	    :docstring "Update `uqbar’ source files.")
    (:route "update/uqbar/%s/log"
	    :type "GET"
	    :docstring "Show log of \"update/uqbar\" request.")
    (:route "update/rebuild-frontend"
	    :type "POST"
	    :docstring "Rebuild `uqbar' frontend.")
    (:route "update/rebuild-frontend/log"
	    :type "GET"
	    :docstring "Show log of \"update/rebuild-frontend\" request."))
  "Routes for `uqbar' API requests.")

(defvar tlon-api-most-recent-log-buffer nil
  "The name of the most recent log buffer.")

(defconst tlon-api-local-url
  "https://local-dev.altruismoeficaz.net/"
  "Local URL for the `uqbar' API.")

;;;; Functions

;;;###autoload
(defun tlon-api-request (route &optional force-update pop-to-buffer)
  "Make a request for ROUTE with the `uqbar' API.
If FORCE-UPDATE is non-nil, or called with a prefix argument, force the update.
If POP-TO-BUFFER is non-nil, display the log buffer."
  (interactive (list (tlon-select-api-route)
		     current-prefix-arg))
  (let* ((route-url (concat (format "%sapi/%s" tlon-api-local-url route)
			    (when force-update "?force=true")))
	 (type (tlon-lookup (tlon-api-get-routes) :type :route route)))
    (tlon-api-get-token
     tlon-api-local-url
     (lambda (access-token)
       "Authenticate with ACCESS-TOKEN, then make request."
       (if (not access-token)
           (message "Failed to authenticate")
	 (request route-url
           :type type
           :headers `(("Content-Type" . "application/json")
                      ("Authorization" . ,(concat "Bearer " access-token)))
           :parser 'json-read
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
		       "Print response, and possibly make new request depending on ROUTE."
		       (tlon-api-print-response route pop-to-buffer :data data)
		       (message
			"`%s' request completed successfully. See the `Uqbar log' buffer for details." route)))))))))

(defun tlon-api-request-force (route)
  "Make a force request for ROUTE with the `uqbar' API."
  (interactive (list (tlon-select-api-route)))
  (tlon-api-request route 'force))

;;;;;; Token

(defun tlon-api-get-token (site callback)
  "Get API token for SITE.
CALLBACK is called with the token as its argument."
  (let* ((data (tlon-api-get-credentials)))
    (request (format "%sapi/auth/login" site)
      :type "POST"
      :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
      :data data
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
		  "Call CALLBACK with the access token in DATA."
                  (when data
                    (funcall callback (alist-get "access_token" data nil nil 'string=))))))))

;;;###autoload
(defun tlon-api-copy-token ()
  "Copy API token to the kill ring."
  (interactive)
  (tlon-api-get-token tlon-api-local-url (lambda (access-token)
					   (kill-new access-token)
					   (message "API token copied to the kill ring."))))

;;;;;; Logs

;;;###autoload
(defun tlon-api-open-most-recent-log ()
  "Open a buffer with the most recent log returned by the API."
  (interactive)
  (unless tlon-api-most-recent-log-buffer
    (user-error "No log buffer to open"))
  (if (get-buffer tlon-api-most-recent-log-buffer)
      (pop-to-buffer tlon-api-most-recent-log-buffer)
    (tlon-api-request tlon-api-most-recent-log-buffer nil 'pop-to-buffer)))

;;;###autoload
(defun tlon-api-open-local-log ()
  "Open a buffer with the local log in `uqbar-api'."
  (interactive)
  (let ((file (file-name-concat (tlon-repo-lookup :dir :name "uqbar-api") "logs/uqbar-api.log")))
    (find-file file)))

(cl-defun tlon-api-print-response (route pop-to-buffer &key data &allow-other-keys)
  "Print DATA returned from API ROUTE.
If POP-TO-BUFFER is non-nil, display the response in a buffer."
  (setq tlon-api-most-recent-log-buffer route)
  (with-current-buffer (get-buffer-create tlon-api-most-recent-log-buffer)
    (erase-buffer)
    (insert (json-encode data))
    (json-pretty-print-buffer)
    (tlon-fix-source-filename-paths)
    (tlon-make-paths-clickable)
    (when pop-to-buffer
      (pop-to-buffer (current-buffer)))))

(defun tlon-api-get-credentials ()
  "Return a list of credentials for `uqbar' API requests."
  (let ((username (tlon-user-lookup :github :name user-full-name))
	(inhibit-message t))
    (concat "username=" (url-hexify-string username)
            "&password=" (url-hexify-string
			  (auth-source-pass-get 'secret
						(concat "tlon/babel/altruismoeficaz.net/" username))))))

;;;;;; Routes

(defun tlon-api-get-routes ()
  "Return the `uqbar' API routes reflecting the current translation language."
  (mapcar
   (lambda (x)
     (if (and (listp x)
	      (stringp (plist-get x :route))
	      (string-match "%s" (plist-get x :route)))
	 (plist-put (copy-sequence x) :route (replace-regexp-in-string
					      "%s"
					      tlon-translation-language
					      (plist-get x :route)))
       x))
   (copy-sequence tlon-uqbar-api-routes)))

(defun tlon-select-api-route ()
  "Prompt the user to select an API route from `tlon-uqbar-api-routes'."
  (let* ((choices (mapcar (lambda (plist)
			    (let ((route (plist-get plist :route))
				  (docstring (plist-get plist :docstring)))
			      (cons (format "%-40.40s %-80.80s" route (propertize docstring 'face 'italic)) route)))
			  (tlon-api-get-routes)))
	 (user-choice (completing-read "Please select an API route: " choices nil t)))
    (cdr (assoc user-choice choices))))

;;;;; Process output buffer

(declare-function json-mode "json-mode")
(defun tlon-fix-source-filename-paths (&optional buffer)
  "Fix `:source_filename' paths in output log in BUFFER.
If BUFFER is nil, default to the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (delete-trailing-whitespace)
      (goto-char (point-min))
      (let* ((json-array-type 'list)
	     (json-object-type 'alist)
	     (json-data (json-read))
	     ;; Modify the JSON
	     (json-modified
	      (mapcar (lambda (json-object)
			(when-let* ((old-filename (cdr (assoc 'source_filename json-object)))
				    (new-filename (file-name-concat paths-dir-tlon-repos old-filename)))
			  (setf (cdr (assoc 'source_filename json-object)) new-filename))
			json-object)
		      json-data)))
	(erase-buffer)
	(insert (json-encode json-modified))
	(json-pretty-print-buffer)
	(json-mode)))))

(defun tlon-make-paths-clickable (&optional buffer)
  "Make file paths in the current buffer clickable.
The paths and also be opened with RET.

If BUFFER is nil, default to the current buffer."
  (let ((buffer (or buffer (or (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(goto-address-mode 1)
	(while (re-search-forward "\"\\([^\"]+\\)\"" nil t)
	  (let* ((match (match-string-no-properties 0))
		 (url (substring match 1 -1))
		 (path (when (file-exists-p url)
			 (abbreviate-file-name (expand-file-name url)))))
	    (when path
	      (make-button (match-beginning 0) (match-end 0)
			   'action (lambda (_) (find-file path))
			   'follow-link t))))
	(local-set-key (kbd "<RET>") 'ffap)))))

;;;;; BibTeX data

(defun tlon-api-get-citation (key &optional csl)
  "Get citation for BibTeX KEY in CSL style from the Babel API.
CSL is the citation style: it can be `long' (default), `short', `audio-long' or
`audio-short'.

If citation is not found, return nil."
  (when-let* ((csl (or csl 'long))
	      (url (tlon-api-get-citation-url key csl))
	      (json (tlon-api-get-citation-json url)))
    (let ((json-key (pcase csl
		      ('long-audio 'long)
		      ('short-audio 'short)
		      (_ csl))))
      (alist-get json-key json))))

(defun tlon-api-get-citation-url (key csl)
  "Return the URL for the citation with KEY in CSL style."
  (let* ((string-formatter (file-name-concat tlon-api-local-url "api/citations/%s/%s"))
	 (type (pcase csl ((or 'long 'short) "text") ((or 'long-audio 'short-audio) "audio"))))
    (format string-formatter key type)))

(defun tlon-api-get-citation-json (url)
  "Return the JSON response from URL."
  (let* ((command (format "curl -sS -X 'GET' \ '%s' \ -H 'accept: application/json'" url))
         (output (shell-command-to-string command)))
    (if (string-match "could not resolve host" output)
        (user-error "Failed to get citation from URL '%s'. Curl error: could not resolve host. Is your local environment set up correctly?" url)
      (let ((trimmed-output (string-trim output))) ; Trim whitespace for checks
        (unless (or (string-prefix-p "{" trimmed-output)
                    (string-prefix-p "[" trimmed-output))
          (user-error "API response from URL '%s' does not look like JSON. Received (first 200 chars): %s"
                      url (substring-no-properties trimmed-output 0 (min 200 (length trimmed-output)))))
        (condition-case err
            (with-temp-buffer
              (insert output) ; Insert original output, not trimmed
              (goto-char (point-min))
              (json-read))
          (json-readtable-error
           (user-error "JSON parsing failed for URL '%s'. Original error: %s. Received (first 200 chars): %s"
                       url err (substring-no-properties output 0 (min 200 (length output))))))))))

;;;;; File uploading

(declare-function files-extras-read-file "files-extras")
(defun tlon-upload-file-to-server (&optional file destination delete-after-upload)
  "Upload FILE asynchronously to DESTINATION in server.
If DELETE-AFTER-UPLOAD is non-nil, delete FILE after uploading."
  (interactive)
  (let* ((file (or file (files-extras-read-file file)))
	 (file-sans-fir (file-name-nondirectory file))
	 (destination (or destination (read-directory-name "Destination: "))))
    (start-process "scp-upload" "*scp-upload*"
                   "scp"
                   (expand-file-name file)
                   destination)
    (message "Uploading `%s' to `%s'..." file-sans-fir destination)
    (set-process-sentinel
     (get-buffer-process "*scp-upload*")
     (lambda (_ event)
       (when (string= event "finished\n")
	 (when delete-after-upload
	   (delete-file file))
	 (message "`%s' successfully uploaded to `%s'." file-sans-fir destination))
       (unless (string= event "finished\n")
	 (progn
           (message "Upload failed: %s" event)
           (display-buffer "*scp-upload*"))))))
  (when (derived-mode-p 'dired-mode)
    (revert-buffer)))

;;;;; Transient

;;;###autoload (autoload 'tlon-api-menu "tlon-api" nil t)
(transient-define-prefix tlon-api-menu ()
  "`api' menu."
  ["Requests"
   ("q" "uqbar"                                tlon-api-request)
   ("Q" "uqbar force"                          tlon-api-request-force)
   ""
   "Logs"
   ("l" "open most recent"                     tlon-api-open-most-recent-log)
   ("L" "open local"                           tlon-api-open-local-log)
   ""
   "Misc"
   ("t" "copy access token"                    tlon-api-copy-token)])

(provide 'tlon-api)
;;; tlon-api.el ends here

