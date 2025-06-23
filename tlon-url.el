;;; tlon-url.el --- Dead URL checking -*- lexical-binding: t -*-

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
(require 'json)
(require 'cl-lib)
(eval-and-compile
  (require 'transient))

;;;; Functions

(declare-function ffap-url-p "ffap")

(defun tlon--get-wayback-machine-url (url callback)
  "Fetch the latest working archived version of URL from Wayback Machine.
Call CALLBACK with (ARCHIVE-URL ORIGINAL-URL).
ARCHIVE-URL is nil if no archive is found or an error occurs."
  (let ((api-url (format "https://web.archive.org/cdx/search/cdx?url=%s&statuscode=200&limit=1&sort=reverse"
                         (url-hexify-string url))))
    (message "Fetching latest working archive for %s..." url)
    (url-retrieve
     api-url
     (lambda (status &rest _) ; original-url is `url` from lexical scope
       (let ((buffer (current-buffer))
             (archive-url nil))
         (with-current-buffer buffer
           (if (plist-get status :error)
               (progn
                 (message "Wayback Machine request for %s failed: %S" url (plist-get status :error)))
             ;; Else: no error from url-retrieve itself
             (goto-char (point-min))
             (if (re-search-forward "^\\s-*$" nil t) ; Skip empty lines / headers if any
                 (let* ((response (buffer-substring-no-properties (point) (point-max)))
                        (lines (split-string response "\n" t)))
                   (if (and lines (> (length lines) 0))
                       (let* ((fields (split-string (car lines) " "))
                              (timestamp (nth 1 fields))
                              (original-url-from-api (nth 2 fields)) ; original URL as per API
                              (_archive-url (format "https://web.archive.org/web/%s/%s" timestamp original-url-from-api)))
                         (setq archive-url _archive-url))))
               ;; else: Could not parse response or empty response
               (message "Could not parse Wayback Machine API response or no archive found for %s." url))))
         (kill-buffer buffer) ; Clean up the *URL-Contents* buffer
         (funcall callback archive-url url)))))) ; Pass original `url` for context

(defun tlon-lychee-replace-in-file (file-path old-url new-url)
  "Replace OLD-URL with NEW-URL in FILE-PATH.
Return t if a replacement was made, nil otherwise."
  (let ((modified nil)
        (search-term (regexp-quote old-url))) ; Quote for regex search
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      (while (search-forward search-term nil t) ; Use quoted search term
        (replace-match new-url t t nil) ; fixed case, literal, no regexp in replacement
        (setq modified t))
      (when modified
        (write-region (point-min) (point-max) file-path)))
    modified))
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
(defun tlon-get-archived (url)
  "Return the latest working archived version of URL from Wayback Machine.
Also, copy the URL to the kill ring."
  (interactive "sURL: ")
  (tlon--get-wayback-machine-url
   url
   (lambda (archive-url original-url) ; original-url is the same as url here
     (if archive-url
         (progn
           (message "Latest working archive: %s" archive-url)
           (kill-new archive-url))
       (message "No working archives found for %s (or error during fetch)." original-url)))))

;;;###autoload
(defun tlon-lychee-fix-dead-links ()
  "Run lychee to find dead links and replace them with Wayback Machine versions.
This command operates on the current project's root directory, identified by
`tlon-get-repo`. It runs `lychee` to scan all supported files, parses the
JSON output, and for each dead link, attempts to find an archived version
using the Wayback Machine. If successful, it replaces the dead link in the
respective file."
  (interactive)
  (let* ((repo-dir (tlon-get-repo))
         (default-directory repo-dir) ; Ensure lychee runs in the repo root
         (lychee-executable-name "lychee")
         (lychee-executable-path (executable-find lychee-executable-name))
         (stderr-file nil) ; Will be set to a temp file path
         (cmd-string nil) ; Will be the full shell command
         (stdout-buffer (generate-new-buffer "*lychee-stdout*"))
         (proc nil) ; To store the process object
         ;; For collecting results
         (replacements-count 0)
         (processed-links-count 0)
         ;; Use a list as a mutable cell for total count, to be updated in callbacks
         (total-dead-links-ref (list 0)))

    (unless lychee-executable-path
      (error "Lychee executable '%s' not found in exec-path" lychee-executable-name))

    (setq stderr-file (make-temp-file "lychee-stderr"))
    (setq cmd-string (format "%s --no-progress --format json --dump . 2>%s"
                             (shell-quote-argument lychee-executable-path)
                             (shell-quote-argument stderr-file)))

    (message "Running Lychee: %s" cmd-string)
    (setq proc (start-process-shell-command "lychee" stdout-buffer cmd-string))

    (set-process-sentinel
     proc
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (let ((stdout-content (with-current-buffer stdout-buffer (buffer-string)))
               (stderr-content (if (file-exists-p stderr-file) ; Check if file was created
                                   (with-temp-buffer (insert-file-contents stderr-file) (buffer-string))
                                 "")))
           (kill-buffer stdout-buffer)
           (when (file-exists-p stderr-file) (delete-file stderr-file))

           (if (not (zerop (process-exit-status process)))
               (error "Lychee process failed (exit status %d): %s\nStdout (first 500 chars):\n%s\nStderr (first 500 chars):\n%s"
                      (process-exit-status process) event
                      (substring stdout-content 0 (min 500 (length stdout-content)))
                      (substring stderr-content 0 (min 500 (length stderr-content))))
             ;; Lychee succeeded (exit code 0)
             (if (string-blank-p stdout-content)
                 (progn
                   (message "Lychee produced empty output to stdout (no links processed or all excluded/cached)")
                   (unless (string-blank-p stderr-content)
                     (message "Lychee stderr (first 1000 chars):\n%s"
                              (substring stderr-content 0 (min 1000 (length stderr-content))))))
               (let ((report (condition-case err
                                 (json-read-from-string stdout-content)
                               (error (error "Failed to parse Lychee JSON from stdout: %s\nStdout (first 500 chars):\n%s\nStderr (first 500 chars):\n%s"
                                             err
                                             (substring stdout-content 0 (min 500 (length stdout-content)))
                                             (substring stderr-content 0 (min 500 (length stderr-content))))))))
                 ;; First pass: count total dead links
                 (dolist (file-entry report)
                   (let* ((_filename (car file-entry)) ; Not used in this loop
                            (link-statuses (cdr file-entry)))
                       (dolist (link-status link-statuses)
                         (let ((status (cdr (assoc 'status link-status)))
                               (target-url (cdr (assoc 'target link-status))))
                           (when (and target-url ; Ensure target-url is not nil
                                      (not (or (string-prefix-p "Ok" status)
                                               (string-prefix-p "Cached(Ok" status)
                                               (string-prefix-p "Excluded" status))))
                             (setcar total-dead-links-ref (1+ (car total-dead-links-ref))))))))

                   (if (= (car total-dead-links-ref) 0)
                       (message "Lychee found no dead links to process.")
                     (progn
                       (message "Lychee found %d dead link(s). Fetching archives and replacing..." (car total-dead-links-ref))
                       ;; Second pass: process dead links
                       (dolist (file-entry report)
                         (let* ((filename (car file-entry)) ; Relative path
                                (full-file-path (expand-file-name filename repo-dir))
                                (link-statuses (cdr file-entry)))
                           (dolist (link-status link-statuses)
                             (let ((status (cdr (assoc 'status link-status)))
                                   (target-url (cdr (assoc 'target link-status))))
                               (when (and target-url
                                          (not (or (string-prefix-p "Ok" status)
                                                   (string-prefix-p "Cached(Ok" status)
                                                   (string-prefix-p "Excluded" status))))
                                 (message "Processing dead link: %s in %s" target-url filename)
                                 (tlon--get-wayback-machine-url
                                  target-url
                                  (lambda (archive-url original-dead-url)
                                    (cl-incf processed-links-count)
                                    (if archive-url
                                        (if (tlon-lychee-replace-in-file full-file-path original-dead-url archive-url)
                                            (progn
                                              (cl-incf replacements-count)
                                              (message "Replaced: %s -> %s in %s" original-dead-url archive-url filename))
                                          (message "Archive for %s found (%s), but no replacement made in %s (URL not found in current file state?)"
                                                   original-dead-url archive-url filename))
                                      (message "No archive found for %s (from file %s)" original-dead-url filename))
                                    (when (= processed-links-count (car total-dead-links-ref))
                                      (message "Lychee dead link processing complete. Made %d replacement(s) out of %d dead links found"
                                               replacements-count (car total-dead-links-ref))
                                      (unless (string-blank-p stderr-content)
                                        (message "Lychee stderr (first 1000 chars):\n%s"
                                                 (substring stderr-content 0 (min 1000 (length stderr-content))))))))))))))))))))))))

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
    ("l" "Lychee fix dead links"                       tlon-lychee-fix-dead-links)
    ("r" "Replace URL across projects"                 tlon-replace-url-across-projects)]])

(provide 'tlon-url)
;;; tlon-url.el ends here

