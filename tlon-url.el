;;; tlon-url.el --- Dead URL checking -*- lexical-binding: t; fill-column: 80 -*-

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

;; Checker for dead URLs.

;;; Code:

(require 'tlon-core)
(eval-and-compile
  (require 'transient))

;;;; Functions

(declare-function ffap-url-p "ffap")
(defun tlon-get-urls-in-file (&optional file)
  "Return a list of all the URLs present in FILE.
If FILE is nil, use the file visited by the current buffer."
  (let ((file (or file (buffer-file-name))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((links))
	(while (re-search-forward browse-url-button-regexp nil t)
	  (when-let ((url (ffap-url-p (match-string-no-properties 0))))
	    (unless (member url links)
	      (push url links))))
	(reverse links)))))

(defun tlon-get-urls-in-dir (&optional dir extension)
  "Return a list of all the URLs in the Markdown links present in DIR files.
If DIR is nil, use the current repository, else prompt the user for one. If
EXTENSION is nil, use \"md\"."
  (let* ((extension (or extension "md"))
	 (dir (or dir (tlon-get-repo)))
	 (regexp (format "\\.%s$" extension)))
    (mapcan #'tlon-get-urls-in-file (directory-files-recursively dir regexp))))

(defun tlon-save-list-of-urls (urls)
  "Save the list of URLs in URLS to a file."
  (let ((file (make-temp-file "urls" nil ".txt")))
    (with-temp-file file
      (insert "# LinkChecker URL list\n")
      (dolist (url urls)
	(insert url "\n")))
    file))

;;;###autoload
(defun tlon-check-urls-in-file (&optional file)
  "Check all the URLs in FILE for dead links asynchronously.
If FILE is nil, use the file visited by the current buffer."
  (interactive)
  (let* ((input-file (or file (buffer-file-name)))
	 (urls (mapconcat #'identity (tlon-get-urls-in-file input-file) " "))
	 (output-file (make-temp-file "linkchecker-output" nil ".txt"))
	 (command (format "linkchecker --file-output=text/%s  --recursion-level=0 %s"
                          (shell-quote-argument output-file) urls)))
    (message "Checking URLs...")
    (set-process-sentinel
     (start-process-shell-command "LinkChecker" "/LinkChecker/" command)
     (lambda (_ event)
       (when (string-match-p "^finished" event)
	 (message "URL checking completed.")
	 (find-file output-file)
	 (goto-address-mode 1))))))

;;;###autoload
(defun tlon-get-archived (&optional url)
  "Get and copy the latest archived version of URL from Wayback Machine."
  (interactive)
  (let ((url (or url (read-string "URL: " (thing-at-point 'url t))))
	(archived (format "https://web.archive.org/web/2/%s" url)))
    (kill-new (format "https://web.archive.org/web/2/%s" url))
    archived))

(defun tlon-replace-url-across-projects (&optional url-dead url-live)
  "Replace URL-DEAD with URL-LIVE in all files across content repos.
If URL-DEAD or URL-LIVE not provided, use URL at point or prompt for them."
  (interactive)
  (let* ((url-dead (or url-dead (read-string "Dead URL: " (thing-at-point 'url t))))
	 (url-live (or url-live (read-string "Live URL: ")))
	 (files (cl-loop for dir in
			 (append (tlon-repo-lookup-all :dir :type 'content :subtype 'originals)
				 (tlon-repo-lookup-all :dir :type 'content :subtype 'translations))
			 append (directory-files-recursively dir ".*")))
	 (replacements 0)
	 (affected-dirs nil))
    (dolist (file files)
      (when (file-regular-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (when (search-forward url-dead nil t)
            (cl-incf replacements)
            (push (file-name-directory file) affected-dirs)
            (goto-char (point-min))
            (while (search-forward url-dead nil t)
              (replace-match url-live))
            (write-region (point-min) (point-max) file)))))
    (message "Made %d replacements in directories: %s"
             replacements
             (mapconcat #'identity (delete-dups affected-dirs) ", "))))

;;;;; Menu

;;;###autoload (autoload 'tlon-url-menu "tlon-url" nil t)
(transient-define-prefix tlon-url-menu ()
  "`url' menu."
  [[""
    ("a" "Get archived"                                tlon-get-archived)
    ("c" "Check URLs in file"                          tlon-check-urls-in-file)
    ("v" "Replace url across projects"                 tlon-replace-url-across-projects)]])

(provide 'tlon-url)
;;; tlon-url.el ends here

