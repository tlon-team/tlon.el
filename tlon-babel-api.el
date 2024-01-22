;;; tlon-babel-api.el --- Make requests with the Babel APIs -*- lexical-binding: t -*-

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

;; Make requests with the Babel APIs.

;;; Code:

(require 'json-mode)
(require 'request)
(require 'tlon-babel)
(require 'tlon-babel-core)

;;;; Variables

;; TODO: add `:repo-name' property, then constract `:route' dynamically with
;; `tlon-babel-api-get-routes'
(defconst tlon-babel-uqbar-api-routes
  '((:route "update/babel-refs"
	    :type "POST"
	    :docstring "Apply CSL and regenerate BibTeX keys.")
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

;;;; Functions

;;;###autoload
(defun tlon-babel-api-request (route)
  "Make a request for ROUTE with the `uqbar' API."
  (interactive (list (tlon-select-api-route)))
  (let* ((site "altruismoeficaz.net")
         (route-url (concat "https://" site "/api/" route))
         (type (tlon-babel-core-plist-lookup (tlon-babel-api-get-routes) :type :route route)))
    (tlon-babel-api-get-token
     (lambda (access-token)
       "Make request, authenticating with ACCESS-TOKEN."
       (if (not access-token)
           (message "Failed to authenticate")
         (request route-url
           :type type
           :headers `(("Content-Type" . "application/json")
                      ("Authorization" . ,(concat "Bearer " access-token)))
           :parser 'json-read
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (with-current-buffer (get-buffer-create (format "*Uqbar log for %s*" route))
                         (erase-buffer)
                         (insert (json-encode data))
                         (json-pretty-print-buffer)
                         (tlon-babel-fix-source-filename-paths)
                         (tlon-babel-api-make-paths-clickable)
                         (switch-to-buffer (current-buffer)))))))))))

(defun tlon-babel-api-get-token (callback)
  "Get `uqbar' API token.
CALLBACK is called with the token as its argument."
  (let* ((username (tlon-babel-user-lookup :github :name user-full-name))
         (data (concat "username=" (url-hexify-string username)
                       "&password=" (url-hexify-string
				     (auth-source-pass-get 'secret
							   (concat "tlon/babel/altruismoeficaz.net/" username))))))
    (request "https://altruismoeficaz.net/api/auth/login"
      :type "POST"
      :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
      :data data
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data
                    (funcall callback (alist-get "access_token" data nil nil 'string=))))))))

(defun tlon-babel-api-get-routes ()
  "Return the `uqbar' API routes reflecting the current translation language."
  (mapcar
   (lambda (x)
     (if (and (listp x)
	      (stringp (plist-get x :route))
	      (string-match "%s" (plist-get x :route)))
	 (plist-put (copy-sequence x) :route (replace-regexp-in-string
					      "%s"
					      tlon-babel-translation-language
					      (plist-get x :route)))
       x))
   (copy-sequence tlon-babel-uqbar-api-routes)))

;; TODO: consider using `marginalia' for this
;; see my questions to GPT-4 on what to implement it:
;; [[id:625AC8A3-F330-4DD2-B8F6-8FF432158057][pass text to marginalia]]
(defun tlon-select-api-route ()
  "Prompt the user to select an API route from `tlon-babel-uqbar-api-routes'."
  (let* ((choices (mapcar (lambda (plist)
			    (let ((route (plist-get plist :route))
				  (docstring (plist-get plist :docstring)))
			      (cons (format "%s  |  %s" route (propertize docstring 'face 'italic)) route)))
			  (tlon-babel-api-get-routes)))
	 (user-choice (completing-read "Please select an API route: " choices nil t)))
    (cdr (assoc user-choice choices))))

(defun tlon-babel-fix-source-filename-paths (&optional buffer)
  "Fix `:source_filename' paths in output log in BUFFER.
If BUFFER is nil, default to the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      ;; Delete trailing whitespace and newlines
      (goto-char (point-max))
      (delete-trailing-whitespace)
      ;; Parse the JSON.
      (goto-char (point-min))
      (let* ((json-array-type 'list)
	     (json-object-type 'alist)
	     (json-data (json-read))
	     ;; Modify the JSON
	     (json-modified
	      (mapcar (lambda (json-object)
			(when-let* ((old-filename (cdr (assoc 'source_filename json-object)))
				    (new-filename (file-name-concat tlon-babel-dir-repos old-filename)))
			  (setf (cdr (assoc 'source_filename json-object)) new-filename))
			json-object)
		      json-data)))
	;; Erase the buffer and insert the modified JSON, making sure it's pretty-printed
	(erase-buffer)
	(insert (json-encode json-modified))
	(json-pretty-print-buffer)
	(json-mode)))))

(defun tlon-babel-api-make-paths-clickable (&optional buffer)
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

;; TODO do this properly by getting request from `tlon-babel-api-get-routes'
(defun tlon-babel-api-magit-trigger-request (&rest _)
  "Trigger appropriate request when a commit is pushed in Magit."
  (let ((uqbar-es (tlon-babel-core-get-property-of-repo-name :dir "uqbar-es"))
	(babel-refs (tlon-babel-core-get-property-of-repo-name :dir "babel-refs"))
	route)
    ;; can’t be done with `pcase'
    (cond
     ((string= default-directory uqbar-es)
      (setq route "update/uqbar/es"))
     ((string= default-directory babel-refs)
      (setq route "update/babel-refs")))
    (when route
      (run-with-timer
       3 nil (lambda ()
	       "Run `tlon-babel-api-request' once the push is expected to complete."
	       (tlon-babel-api-request route))))))

(dolist (fun (list 'magit-push-current-to-upstream
		   'magit-push-current-to-pushremote))
  (advice-add fun :after 'tlon-babel-api-magit-trigger-request))

(provide 'tlon-babel-api)
;;; tlon-babel-api.el ends here

