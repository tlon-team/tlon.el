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
(require 'url-parse) ; For url-decode-url
(require 'url-util)  ; For url-hexify-string
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
     (lambda (status &rest _)
       (let ((buffer (current-buffer))
             (archive-url nil))
         (with-current-buffer buffer
           (if (plist-get status :error)
               (progn
                 (message "Wayback Machine request for %s failed: %S" url (plist-get status :error)))
             (goto-char (point-min))
             (if (re-search-forward "^\\s-*$" nil t)
                 (let* ((response (buffer-substring-no-properties (point) (point-max)))
                        (lines (split-string response "\n" t)))
                   (if (and lines (> (length lines) 0))
                       (let* ((fields (split-string (car lines) " "))
                              (timestamp (nth 1 fields))
                              (original-url-from-api (nth 2 fields)))
                         (setq archive-url (format "https://web.archive.org/web/%s/%s" timestamp original-url-from-api)))))
               (message "Could not parse Wayback Machine API response or no archive found for %s." url))))
         (kill-buffer buffer)
         (funcall callback archive-url url))))))

(defun tlon-lychee-replace-in-file (file-path old-url new-url)
  "Replace OLD-URL with NEW-URL in FILE-PATH.
Tries to match variations of OLD-URL (as-is, hexified, decoded).
Return t if a replacement was made, nil otherwise."
  (let ((modified nil)
        (search-candidates
         (delete-dups ; Ensure unique search terms
          (list old-url
                (url-hexify-string old-url)
                (url-decode-url old-url)))))
    (with-temp-buffer
      (insert-file-contents file-path)
      (catch 'found
        (dolist (candidate search-candidates)
          (goto-char (point-min)) ; Start search from beginning for each candidate
          (let ((search-term (regexp-quote candidate)))
            ;; Check if this candidate URL exists in the buffer at all
            (when (search-forward search-term nil t)
              ;; If found, reset point and replace all occurrences of this candidate
              (goto-char (point-min))
              (while (search-forward search-term nil t)
                (replace-match new-url t t nil) ; fixed case, literal replacement
                (setq modified t))
              ;; If modifications were made, write to file and exit dolist
              (when modified
                (write-region (point-min) (point-max) file-path)
                (throw 'found t)))))))
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
    (let ((proc (start-process-shell-command "LinkChecker" nil command)))
      (set-process-filter
       proc
       (lambda (_proc output)
         (message "LinkChecker output: %s" output)))
      (set-process-sentinel
       proc
       (lambda (_ event)
         (when (string-match-p "^finished" event)
           (message "URL checking completed.")
           (find-file output-file)
           (goto-address-mode 1)))))))

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

;;;;; Dead

;;;###autoload
(defun tlon-lychee-report ()
  "Generate a report of dead links using Lychee."
  (interactive)
  (let ((default-directory (tlon-get-repo)))
    (tlon-lychee-ensure)
    (eshell t)
    (with-current-buffer (get-buffer "*eshell*")
      (goto-char (point-max))
      (insert "lychee .")
      (eshell-send-input))))

;;;###autoload
(defun tlon-lychee-fix-dead-links ()
  "Run lychee to find dead links and replace them with Wayback Machine versions.
This command operates on the current project's root directory, identified by
`tlon-get-repo`. It runs `lychee` to scan all supported files, parses the
JSON output, and for each dead link, attempts to find an archived version
using the Wayback Machine. If successful, it replaces the dead link in the
respective file. This process is asynchronous and relies on helper functions."
  (interactive)
  (let* ((repo-dir (tlon-get-repo))
         (default-directory repo-dir) ; Ensure lychee runs in the repo root
         (stderr-file nil)
         (cmd-string nil))
    (tlon-lychee-ensure)
    (setq stderr-file (make-temp-file "lychee-stderr"))
    (setq cmd-string (format "%s --no-progress --format json . 2>%s"
                             (shell-quote-argument (executable-find "lychee"))
                             (shell-quote-argument stderr-file)))

    (message "Starting Lychee process with command: %s" cmd-string)
    (message "This may take a while depending on the number of files and links.")
    ;; For debugging, read from existing JSON output file
    (let ((output-file (expand-file-name "lychee-output.json" "~/Downloads")))
      (if (file-exists-p output-file)
          (with-temp-buffer
            (insert-file-contents output-file)
            (let ((stdout-content (buffer-string)))
              ;; Directly process the JSON content without expecting a process
              (let ((report (json-read-from-string stdout-content)))
                (unless (listp report)
                  (error "Expected report to be a list, but got: %s" (type-of report)))
                (tlon-lychee--process-parsed-report report repo-dir ""))))
        (message "Lychee output file not found: %s" output-file)))))

(defun tlon-lychee-ensure ()
  "Ensure the lychee executable is available."
  (unless (executable-find "lychee")
    (error "Lychee executable not found; please run `brew install lychee'")))

(defun tlon-lychee--run-and-process (cmd-string stdout-buffer stderr-file repo-dir)
  "Run lychee with CMD-STRING, capturing output in STDOUT-BUFFER and STDERR-FILE.
Process output via a sentinel that calls `tlon-lychee--handle-completion`
with REPO-DIR for context."
  (let ((proc (start-process-shell-command "lychee" stdout-buffer cmd-string)))
    (set-process-sentinel
     proc
     (lambda (process event)
       (tlon-lychee--handle-completion process stdout-buffer stderr-file repo-dir)))))

(defun tlon-lychee--handle-completion (process stdout-buffer stderr-file repo-dir)
  "Handle lychee process completion.
Parse JSON output from STDOUT-BUFFER, read STDERR-FILE, and call
`tlon-lychee--process-parsed-report` if successful.
Cleans up STDOUT-BUFFER and STDERR-FILE. REPO-DIR provides context."
  (when (memq (process-status process) '(exit signal))
    (let* ((stdout-content (with-current-buffer stdout-buffer (buffer-string)))
           (stderr-content (if (file-exists-p stderr-file)
                               (with-temp-buffer (insert-file-contents stderr-file) (buffer-string))
                             ""))
           (output-file (expand-file-name "lychee-output.json" "~/Downloads")))
      ;; Save JSON output to a file for inspection
      (with-temp-file output-file
        (insert stdout-content))
      (message "Lychee JSON output saved to %s" output-file)
      (kill-buffer stdout-buffer)
      (when (file-exists-p stderr-file) (delete-file stderr-file))

      (let ((report nil) (parse-error-reason nil))
        (if (string-blank-p stdout-content)
            (setq parse-error-reason "stdout was blank")
          (condition-case err
              (condition-case err
                  (progn
                    (setq report (json-read-from-string stdout-content))
                    (message "Parsed report type: %s" (type-of report))
                    (message "Parsed report content: %s" report))
                (unless (listp report)
                  (error "Expected report to be a list, but got: %s" (type-of report)))
                (json-error
                 (setq parse-error-reason (format "JSON parsing failed: %s" err))
                 (message "Error parsing JSON: %s" err))
                (error
                 (setq parse-error-reason (format "Unexpected error: %s" err))
                 (message "Unexpected error during JSON parsing: %s" err)))
            (error (setq parse-error-reason (format "JSON parsing failed: %s" err)))))

        (if parse-error-reason
            (error "Lychee%s (exit status %d) but %s.\nStdout (first 500 chars):\n%s\nStderr (first 500 chars):\n%s"
                   (if (zerop (process-exit-status process)) " reported success" " process failed")
                   (process-exit-status process)
                   parse-error-reason
                   (substring stdout-content 0 (min 500 (length stdout-content)))
                   (substring stderr-content 0 (min 500 (length stderr-content))))
          (tlon-lychee--process-parsed-report report repo-dir stderr-content))))))

(defun tlon-lychee--process-parsed-report (report repo-dir stderr-content)
  "Process the parsed lychee REPORT.
Counts dead links, and if any, calls `tlon-lychee--iterate-and-attempt-fixes`.
REPO-DIR is the repository root. STDERR-CONTENT is lychee's stderr output."
  (let* ((replacements-count-ref (list 0))
         (processed-links-count-ref (list 0))
         (total-dead-links (tlon-lychee--count-dead-links-in-report report)))

    (if (= total-dead-links 0)
        (progn
          (message "Lychee found no dead links to process.")
          (unless (string-blank-p stderr-content)
            (message "Lychee stderr (first 1000 chars):\n%s"
                     (substring stderr-content 0 (min 1000 (length stderr-content))))))
      (progn
        (message "Lychee found %d dead link(s). Fetching archives and replacing..." total-dead-links)
        (tlon-lychee--iterate-and-attempt-fixes report repo-dir total-dead-links
                                                replacements-count-ref processed-links-count-ref
                                                stderr-content)))))

(defun tlon-lychee--count-dead-links-in-report (report)
  "Count and return the number of dead links in the lychee REPORT."
  (let ((count 0)
        (error-map-alist (cdr (assoc 'error_map report))))
    (dolist (file-entry error-map-alist)
      ;; file-entry is (FILENAME-SYMBOL . LINK-STATUSES-VECTOR)
      (let ((link-statuses (cdr file-entry)))
        (when (vectorp link-statuses)
          (dotimes (i (length link-statuses))
            (let* ((link-status (aref link-statuses i)) ; link-status is an alist
                   (url (cdr (assoc 'url link-status)))
                   (status-details (cdr (assoc 'status link-status)))
                   (status-text (if (consp status-details) (cdr (assoc 'text status-details)) nil)))
              (when (and url status-text
                         (not (or (string-prefix-p "Ok" status-text)
                                  (string-prefix-p "Cached(Ok" status-text)
                                  (string-prefix-p "Excluded" status-text))))
                (cl-incf count)))))))
    count))

(defun tlon-lychee--iterate-and-attempt-fixes (report repo-dir total-dead-links
                                               replacements-count-ref processed-links-count-ref
                                               stderr-content)
  "Iterate over REPORT from lychee, attempting to fix dead links.
REPO-DIR is the root. TOTAL-DEAD-LINKS is the pre-counted total.
REPLACEMENTS-COUNT-REF and PROCESSED-LINKS-COUNT-REF are mutable counters.
STDERR-CONTENT is lychee's stderr output for final display."
  (let ((error-map-alist (cdr (assoc 'error_map report))))
    (dolist (file-entry error-map-alist)
      (let* ((filename-symbol (car file-entry)) ; This is a symbol, not a string
             (filename (symbol-name filename-symbol)) ; Convert symbol to string
             (full-file-path (expand-file-name filename repo-dir))
             (link-statuses (cdr file-entry))) ; This is a vector of link-status alists
        (when (vectorp link-statuses)
          (dotimes (i (length link-statuses))
            (let* ((link-status (aref link-statuses i))
                   (url (cdr (assoc 'url link-status)))
                   (status-details (cdr (assoc 'status link-status)))
                   (status-text (if (consp status-details) (cdr (assoc 'text status-details)) nil)))
              (when (and url status-text
                         (not (or (string-prefix-p "Ok" status-text)
                                  (string-prefix-p "Cached(Ok" status-text)
                                  (string-prefix-p "Excluded" status-text))))
                (message "Processing dead link: %s in %s" url filename)
                (tlon-lychee--attempt-single-fix full-file-path filename url
                                                 total-dead-links replacements-count-ref
                                                 processed-links-count-ref stderr-content)))))))))

(defun tlon-lychee--attempt-single-fix (full-file-path filename url
                                        total-dead-links replacements-count-ref
                                        processed-links-count-ref stderr-content)
  "Attempt to fix a single dead URL in FULL-FILE-PATH (relative FILENAME).
Uses Wayback Machine. TOTAL-DEAD-LINKS, REPLACEMENTS-COUNT-REF,
PROCESSED-LINKS-COUNT-REF, and STDERR-CONTENT are for progress tracking and
reporting."
  (tlon--get-wayback-machine-url
   url
   (lambda (archive-url original-dead-url) ; original-dead-url is url
     (tlon-lychee--handle-wayback-response archive-url original-dead-url
                                           full-file-path filename
                                           total-dead-links
                                           replacements-count-ref processed-links-count-ref
                                           stderr-content))))

(defun tlon-lychee--handle-wayback-response (archive-url original-dead-url
							 full-file-path filename
							 total-dead-links
							 replacements-count-ref processed-links-count-ref
							 stderr-content)
  "Handle response from Wayback Machine for ORIGINAL-DEAD-URL.
If ARCHIVE-URL is found, replace in FULL-FILE-PATH (relative FILENAME). Updates
counters REPLACEMENTS-COUNT-REF, PROCESSED-LINKS-COUNT-REF. Displays final
summary when all TOTAL-DEAD-LINKS are processed, including STDERR-CONTENT."
  (cl-incf (car processed-links-count-ref))
  (if archive-url
      (if (tlon-lychee-replace-in-file full-file-path original-dead-url archive-url)
          (progn
            (cl-incf (car replacements-count-ref))
            (message "Replaced: %s -> %s in %s" original-dead-url archive-url filename))
        (message "Archive for %s found (%s), but no replacement made in %s (URL not found?)"
                 original-dead-url archive-url filename))
    (message "No archive found for %s (from file %s)" original-dead-url filename))

  (when (= (car processed-links-count-ref) total-dead-links)
    (message "Lychee dead link processing complete. Made %d replacement(s) out of %d dead links found."
             (car replacements-count-ref) total-dead-links)
    (unless (string-blank-p stderr-content)
      (message "Lychee stderr (first 1000 chars):\n%s"
               (substring stderr-content 0 (min 1000 (length stderr-content)))))))

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

