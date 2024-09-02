;;; tlon-url.el --- Dead URL checking -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon
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

;; Checker for dead URLs.

;;; Code:

(require 'tlon-core)

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

(provide 'tlon-url)
;;; tlon-url.el ends here

