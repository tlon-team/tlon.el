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
(require 'url-util)  ; For url-hexify-string
(require 'markdown-mode)
(require 'bibtex)
(eval-and-compile
  (require 'transient))

;;;; Variables

(defconst tlon-lychee-accept-option
  "--accept 200,201,202,204,206,300,301,302,303,307,308,400,401,429"
  "Lychee flag to accept these HTTP status codes.")

;;;; Functions

(declare-function ffap-url-p "ffap")
(declare-function bibtex-text-in-field "bibtex")

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
               (message "Wayback Machine request for %s failed: %S" url (plist-get status :error))
             ;; Find the end of HTTP headers (empty line)
             (goto-char (point-min))
             (if (re-search-forward "^\\s-*$" nil t)
                 (let* ((response (buffer-substring-no-properties (point) (point-max)))
                        (lines (split-string response "\n" t)))
                   (if (and lines (> (length lines) 0))
                       (let* ((fields (split-string (car lines) " "))
                              (timestamp (nth 1 fields))
                              (original-url-from-api (nth 2 fields)))
                         (when (and timestamp original-url-from-api)
                           (setq archive-url (format "https://web.archive.org/web/%s/%s" timestamp original-url-from-api))))
                     (message "No working archives found for %s" url)))
               (message "Could not parse Wayback Machine API response for %s" url))))
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
                (url-unhex-string old-url)
                (url-unhex-string (url-hexify-string old-url))))))
    (with-temp-buffer
      (insert-file-contents file-path)
      (catch 'found
        (dolist (candidate search-candidates)
          (goto-char (point-min)) ; Start search from beginning for each candidate
          (when (search-forward candidate nil t)
            ;; If found, reset point and replace all occurrences of this candidate
            (goto-char (point-min))
            (while (search-forward candidate nil t)
              (replace-match new-url t t nil) ; fixed case, literal replacement
              (setq modified t))
            ;; If modifications were made, write to file and exit dolist
            (when modified
              (write-region (point-min) (point-max) file-path)
              (throw 'found t))))))
    modified))

(defun tlon-lychee--is-wayback-url-p (url)
  "Return t if URL is a Wayback Machine URL."
  (string-match-p "\\`https?://\\(?:web\\.\\)?archive\\.org/" url))

(defun tlon-lychee-remove-url-from-file (file-path url)
  "Remove URL markup from FILE-PATH, keeping the link text.
For Markdown links [text](URL), keeps only the text.
For bare URLs, removes the URL entirely.
Return t if a removal was made, nil otherwise."
  (let ((modified nil)
        (search-candidates
         (delete-dups
          (list url
                (url-hexify-string url)
                (url-unhex-string url)
                (url-unhex-string (url-hexify-string url))))))
    (with-temp-buffer
      (insert-file-contents file-path)
      (catch 'found
        (dolist (candidate search-candidates)
          (goto-char (point-min))
          (while (re-search-forward markdown-regex-link-inline nil t)
            (let ((link-url (match-string-no-properties 6)))
              (when (string= link-url candidate)
                (let ((link-text (match-string-no-properties 3)))
                  (if (and link-text (not (string-blank-p link-text)))
                      ;; Keep text, remove link markup
                      (progn
                        (replace-match link-text t t nil 0)
                        (setq modified t))
                    ;; Empty or whitespace-only text - remove entire link
                    (progn
                      (replace-match "" t t nil 0)
                      (setq modified t)))))))
          ;; Also check for bare URLs not in Markdown links
          (goto-char (point-min))
          (while (search-forward candidate nil t)
            (let ((url-start (match-beginning 0))
                  (url-end (match-end 0)))
              ;; Check if this URL is NOT part of a Markdown link
              (save-excursion
                (goto-char url-start)
                (unless (and (> url-start 1)
                             (eq (char-before url-start) ?\()
                             (re-search-backward markdown-regex-link-inline nil t)
                             (and (>= url-start (match-beginning 6))
                                  (<= url-end (match-end 6))))
                  ;; This is a bare URL, remove it
                  (delete-region url-start url-end)
                  (setq modified t)))))
          (when modified
            (write-region (point-min) (point-max) file-path)
            (throw 'found t))))
      modified)))

(defvar tlon-lychee-whitelist-file
  (file-name-concat elpaca-directory "repos/tlon/etc/tlon-url-lychee-whitelist.txt")
  "File to store whitelisted URLs that should be skipped.")

(defun tlon-lychee--load-whitelist ()
  "Load the whitelist of URLs from file.
Return a list of URLs that should be skipped."
  (if (file-exists-p tlon-lychee-whitelist-file)
      (with-temp-buffer
        (insert-file-contents tlon-lychee-whitelist-file)
        (split-string (buffer-string) "\n" t "\\s-*"))
    nil))

(defun tlon-lychee--save-whitelist (whitelist)
  "Save WHITELIST of URLs to file."
  (with-temp-file tlon-lychee-whitelist-file
    (dolist (url whitelist)
      (insert url "\n"))))

(defun tlon-lychee--add-to-whitelist (url)
  "Add URL to the whitelist."
  (let ((whitelist (tlon-lychee--load-whitelist)))
    (unless (member url whitelist)
      (push url whitelist)
      (tlon-lychee--save-whitelist whitelist)
      (message "Added %s to whitelist" url))))

(defun tlon-lychee--is-whitelisted-p (url)
  "Return t if URL is in the whitelist."
  (member url (tlon-lychee--load-whitelist)))

;;;;; Get URLs

(defun tlon-get-urls-in-file (&optional file)
  "Return a list of all URL strings found in FILE.
If FILE is nil, use the file visited by the current buffer.  The extractor is
chosen primarily by the file name extension, falling back to the `major-mode'
of a visiting buffer if that yields better information:

- Files ending in “.bib” (or buffers in `bibtex-mode') are parsed with
  `tlon--get-urls-in-file-bibtex'.

- Markdown files (extension “.md” or buffers in `markdown-mode') are parsed with
  `tlon--get-urls-in-file-markdown'.

- Any other file type defaults to the Markdown extractor, which does a generic
  regexp scan for links."
  (let* ((file (or file (buffer-file-name)))
         (ext  (when file (downcase (file-name-extension file))))
         (visiting-buffer (and file (find-buffer-visiting file)))
         (mode (and visiting-buffer
                    (buffer-local-value 'major-mode visiting-buffer))))
    (cond
     ((or (eq mode 'bibtex-mode)
          (string= ext "bib"))
      (tlon--get-urls-in-file-bibtex file))
     ((or (eq mode 'markdown-mode)
          (member ext '("md" "markdown")))
      (tlon--get-urls-in-file-markdown file))
     (t
      (tlon--get-urls-in-file-markdown file)))))

(defun tlon--get-urls-in-file-markdown (file)
  "Return a list of all the URLs present in markdown FILE.
FILE is the file to process."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((links))
      (while (re-search-forward markdown-regex-link-inline nil t)
	(when-let ((url (ffap-url-p (match-string-no-properties 6))))
	  (unless (member url links)
	    (push url links))))
      (reverse links))))

(defun tlon--get-urls-in-file-bibtex (file)
  "Return a list of all the URLs present in bibtex FILE.
FILE is the file to process."
  (with-temp-buffer
    (insert-file-contents file)
    (bibtex-mode)
    (let ((values '())
          (field "url"))
      (bibtex-map-entries
       (lambda (_key _beg _end)
         (let ((value (bibtex-text-in-field field)))
           (when value
             (push value values)))))
      (nreverse values))))

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

;;;;; List dead URLs

;;;###autoload
(defun tlon-list-dead-urls-in-file (&optional file)
  "Check all the URLs in FILE for dead links.
If FILE is nil, use the file visited by the current buffer."
  (interactive)
  (let* ((input-file (or file (read-file-name "File: " nil nil t
                                              (file-relative-name (buffer-file-name) default-directory))))
         (abs-file (file-truename input-file)))
    (if (string-equal (file-name-extension abs-file) "bib")
        ;; Work around lychee panic on large .bib files by checking a temporary list of URLs
        (let* ((urls (tlon-get-urls-in-file abs-file))
               (tmp-file (tlon-save-list-of-urls urls)))
          (tlon-lychee--check-urls tmp-file))
      (tlon-lychee--check-urls abs-file))))

;;;###autoload
(defun tlon-list-dead-urls-in-repo ()
  "Check all the URLs in the current repository for dead links.
The command prompts whether to use JSON format for the output."
  (interactive)
  (let ((default-directory (tlon-get-repo))
        (json (y-or-n-p "JSON format? ")))
    (tlon-lychee--check-urls "." json)))

(declare-function eshell-send-input "esh-mode")
(defun tlon-lychee--check-urls (target &optional json)
  "Run lychee on TARGET and display results in eshell.
If JSON is non-nil, use JSON format."
  (tlon-lychee-ensure)
  (eshell t)
  (with-current-buffer (get-buffer "*eshell*")
    (goto-char (point-max))
    (insert (format "lychee %s %s %s"
		    tlon-lychee-accept-option
                    (if json "--format json" "")
                    (shell-quote-argument target)))
    (eshell-send-input)))

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
(defun tlon-get-earliest-archived (url)
  "Return the earliest working archived version of URL from the Wayback Machine.
Also, copy the URL to the kill ring."
  (interactive "sURL: ")
  (tlon--get-earliest-wayback-machine-url
   url
   (lambda (archive-url original-url)
     (if archive-url
         (progn
           (message "Earliest working archive: %s" archive-url)
           (kill-new archive-url))
       (message "No working archives found for %s (or error during fetch)." original-url)))))

(defun tlon--get-earliest-wayback-machine-url (url callback)
  "Fetch the earliest working archived version of URL from Wayback Machine.
Call CALLBACK with (ARCHIVE-URL ORIGINAL-URL).
ARCHIVE-URL is nil if no archive is found or an error occurs."
  (let ((api-url (format "https://web.archive.org/cdx/search/cdx?url=%s&statuscode=200&limit=1"
                         (url-hexify-string url))))
    (message "Fetching earliest working archive for %s..." url)
    (url-retrieve
     api-url
     (lambda (status &rest _)
       (let ((buffer (current-buffer))
             (archive-url nil))
         (with-current-buffer buffer
           (if (plist-get status :error)
               (message "Wayback Machine request for %s failed: %S" url (plist-get status :error))
             ;; Find the end of HTTP headers (empty line)
             (goto-char (point-min))
             (if (re-search-forward "^\\s-*$" nil t)
                 (let* ((response (buffer-substring-no-properties (point) (point-max)))
                        (lines (split-string response "\n" t)))
                   (if (and lines (> (length lines) 0))
                       (let* ((fields (split-string (car lines) " "))
                              (timestamp (nth 1 fields))
                              (original-url-from-api (nth 2 fields)))
                         (when (and timestamp original-url-from-api)
                           (setq archive-url
				 (format "https://web.archive.org/web/%s/%s" timestamp original-url-from-api))))
                     (message "No working archives found for %s" url)))
               (message "Could not parse Wayback Machine API response for %s" url))))
         (kill-buffer buffer)
         (funcall callback archive-url url))))))

;;;;; Fix dead URLs

;;;###autoload
(defun tlon-fix-dead-urls-in-file (&optional file)
  "Run lychee to find and fix dead links in a single FILE.
If FILE is nil, prompt for a file.  The interactive fixing workflow is the
same as `tlon-fix-dead-urls-in-repo', but restricted to the chosen file."
  (interactive)
  (tlon-lychee-ensure)
  (let* ((file (or file
                   (read-file-name
                    "File: " nil nil t
                    (when-let ((buf-file (buffer-file-name)))
                      (file-relative-name buf-file default-directory)))))
         (file (file-truename file))
         (repo-dir (file-name-directory file))
         (default-directory repo-dir)
         (stderr-file (make-temp-file "lychee-stderr"))
         (stdout-buffer (generate-new-buffer "*lychee-output*"))
         (cmd-string
          (format
           "%s %s --no-progress --format json %s 2>%s"
           (shell-quote-argument (executable-find "lychee"))
	   tlon-lychee-accept-option
           (shell-quote-argument file)
           (shell-quote-argument stderr-file))))
    (message "Starting Lychee process with command: %s" cmd-string)
    (tlon-lychee--run-and-process cmd-string stdout-buffer stderr-file repo-dir)))

;;;###autoload
(defun tlon-fix-dead-urls-in-repo ()
  "Run lychee to find dead links and replace them with Wayback Machine versions.
This command operates on the current project's root directory, identified by
`tlon-get-repo'. It runs `lychee' to scan all supported files, parses the
JSON output, and for each dead link, attempts to find an archived version
using the Wayback Machine. If successful, it replaces the dead link in the
respective file. This process is asynchronous and relies on helper functions."
  (interactive)
  (let* ((repo-dir (tlon-get-repo))
         (default-directory repo-dir) ; Ensure lychee runs in the repo root
         (stderr-file (make-temp-file "lychee-stderr"))
         (stdout-buffer (generate-new-buffer "*lychee-output*"))
         (cmd-string (format "%s %s --no-progress --format json . 2>%s"
			     tlon-lychee-accept-option
                             (shell-quote-argument (executable-find "lychee"))
                             (shell-quote-argument stderr-file))))
    (tlon-lychee-ensure)
    (message "Starting Lychee process with command: %s" cmd-string)
    (message "This may take a while depending on the number of files and links.")
    (tlon-lychee--run-and-process cmd-string stdout-buffer stderr-file repo-dir)))

(defun tlon-lychee-ensure ()
  "Ensure the lychee executable is available."
  (unless (executable-find "lychee")
    (error "Lychee executable not found; please run `brew install lychee'")))

(defun tlon-lychee--run-and-process (cmd-string stdout-buffer stderr-file repo-dir)
  "Run lychee with CMD-STRING, capturing output in STDOUT-BUFFER and STDERR-FILE.
Process output via a sentinel that calls `tlon-lychee--handle-completion'
with REPO-DIR for context."
  (let ((proc (start-process-shell-command "lychee" stdout-buffer cmd-string)))
    (set-process-sentinel
     proc
     (lambda (process _event)
       (tlon-lychee--handle-completion process stdout-buffer stderr-file repo-dir)))))

(defun tlon-lychee--handle-completion (process stdout-buffer stderr-file repo-dir)
  "Handle lychee process completion.
Parse JSON output from STDOUT-BUFFER, read STDERR-FILE, and call
`tlon-lychee--process-parsed-report' if successful.
Cleans up STDOUT-BUFFER and STDERR-FILE. PROCESS is the lychee process
and REPO-DIR provides context."
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
Counts dead links, and if any, calls `tlon-lychee--iterate-and-attempt-fixes'.
REPO-DIR is the repository root. STDERR-CONTENT is lychee's stderr output."
  (let* ((action-counts-ref (list :archived 0 :replaced 0 :removed 0 :whitelisted 0 :skipped 0))
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
                                                action-counts-ref processed-links-count-ref
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
                                               action-counts-ref processed-links-count-ref
                                               stderr-content)
  "Iterate over REPORT from lychee, attempting to fix dead links.
REPO-DIR is the root. TOTAL-DEAD-LINKS is the pre-counted total.
ACTION-COUNTS-REF and PROCESSED-LINKS-COUNT-REF are mutable counters.
STDERR-CONTENT is lychee's stderr output for final display."
  (let ((dead-links-queue (tlon-lychee--collect-dead-links report repo-dir)))
    (tlon-lychee--process-next-dead-link dead-links-queue total-dead-links
                                         action-counts-ref processed-links-count-ref
                                         stderr-content)))

(defun tlon-lychee--collect-dead-links (report repo-dir)
  "Collect all dead links from REPORT into a list for sequential processing.
REPO-DIR is the root directory of the repository. Each item is a plist with
:url, :file-path, :filename, :status-text."
  (let ((dead-links nil)
        (error-map-alist (cdr (assoc 'error_map report))))
    (dolist (file-entry error-map-alist)
      (let* ((filename-symbol (car file-entry))
             (filename (symbol-name filename-symbol))
             (full-file-path (expand-file-name filename repo-dir))
             (link-statuses (cdr file-entry)))
        (when (vectorp link-statuses)
          (dotimes (i (length link-statuses))
            (let* ((link-status (aref link-statuses i))
                   (url (cdr (assoc 'url link-status)))
                   (status-details (cdr (assoc 'status link-status)))
                   (status-text (if (consp status-details) (cdr (assoc 'text status-details)) nil)))
              (when (and url status-text
                         (not (or (string-prefix-p "Ok" status-text)
                                  (string-prefix-p "Cached(Ok" status-text)
                                  (string-prefix-p "Excluded" status-text)))
                         (not (tlon-lychee--is-whitelisted-p url)))
                (push (list :url url
                            :file-path full-file-path
                            :filename filename
                            :status-text status-text)
                      dead-links)))))))
    (reverse dead-links)))

(defun tlon-lychee--process-next-dead-link (dead-links-queue total-dead-links
							     action-counts-ref processed-links-count-ref
							     stderr-content &optional skip-browser-open)
  "Process the next dead link in DEAD-LINKS-QUEUE sequentially.

DEAD-LINKS-QUEUE is a list of plists containing dead link information.
TOTAL-DEAD-LINKS is the total number of dead links found.
ACTION-COUNTS-REF is a reference to the plist of action counts.
PROCESSED-LINKS-COUNT-REF is a reference to the count of processed links.
STDERR-CONTENT is the error output from the lychee command.
SKIP-BROWSER-OPEN when non-nil, skips opening the URL in browser.

Wait for user input before proceeding to the next link."
  (if (null dead-links-queue)
      (let ((archived (plist-get action-counts-ref :archived))
            (replaced (plist-get action-counts-ref :replaced))
            (removed (plist-get action-counts-ref :removed))
            (whitelisted (plist-get action-counts-ref :whitelisted))
            (skipped (plist-get action-counts-ref :skipped)))
        (message "Lychee dead link processing complete. Processed %d out of %d dead links found (including whitelisted).\nActions taken: %d archived, %d replaced, %d removed, %d whitelisted, %d skipped."
                 (car processed-links-count-ref) total-dead-links
                 archived replaced removed whitelisted skipped))
    (let* ((current-link (car dead-links-queue))
           (remaining-links (cdr dead-links-queue))
           (url (plist-get current-link :url))
           (full-file-path (plist-get current-link :file-path))
           (filename (plist-get current-link :filename))
           (current-position (- total-dead-links (length dead-links-queue) -1))
           (remaining-count (length remaining-links)))
      (message "Processing dead link: %s in %s" url filename)
      (cl-incf (car processed-links-count-ref))
      
      ;; Open the original URL in browser (unless skipping)
      (unless skip-browser-open
        (funcall browse-url-secondary-browser-function url))
      
      ;; Prompt user for action
      (let* ((is-wayback (tlon-lychee--is-wayback-url-p url))
             (prompt-text (if is-wayback
                              (format "Dead link (%d/%d): %s\n%d remaining after this one.\nChoose action: (r)eplace, (d)elete, (w)hitelist, (s)kip, (q)uit: "
                                      current-position total-dead-links url remaining-count)
                            (format "Dead link (%d/%d): %s\n%d remaining after this one.\nChoose action: (a)rchive, (r)eplace, (d)elete, (w)hitelist, (s)kip, (q)uit: "
                                    current-position total-dead-links url remaining-count)))
             (valid-choices (if is-wayback '(?r ?d ?w ?s ?q) '(?a ?r ?d ?w ?s ?q)))
             (action (read-char-choice prompt-text valid-choices)))
        (cond
         ((eq action ?a)
          (if is-wayback
              (progn
                (message "Cannot archive a Wayback Machine URL")
                (tlon-lychee--process-next-dead-link remaining-links total-dead-links
                                                     action-counts-ref processed-links-count-ref
                                                     stderr-content))
            ;; Only fetch archive when user chooses this option
            (message "Fetching archived version...")
            (tlon--get-wayback-machine-url
             url
             (lambda (archive-url original-dead-url)
               (tlon-lychee--handle-archive-response
                archive-url original-dead-url full-file-path filename
                remaining-links total-dead-links
                action-counts-ref processed-links-count-ref
                stderr-content)))))
         ((eq action ?r)
          (let ((input (read-string "Enter replacement URL: ")))
            (cond
             ((string-blank-p input)
              (message "Replacement URL cannot be empty. Returning to main prompt.")
              ;; Return to main prompt by recursively calling this function, skipping browser open
              (tlon-lychee--process-next-dead-link dead-links-queue total-dead-links
                                                   action-counts-ref processed-links-count-ref
                                                   stderr-content t))
             ((string= input url)
              (message "Replacement URL cannot be the same as the original URL. Returning to main prompt.")
              ;; Return to main prompt by recursively calling this function, skipping browser open
              (tlon-lychee--process-next-dead-link dead-links-queue total-dead-links
                                                   action-counts-ref processed-links-count-ref
                                                   stderr-content t))
             (t
              ;; Valid replacement URL
              (if (tlon-lychee-replace-in-file full-file-path url input)
                  (progn
                    (cl-incf (plist-get action-counts-ref :replaced))
                    (message "Replaced: %s -> %s in %s" url input filename))
                (message "Replacement URL specified but no replacement made in %s (URL not found?)" filename))
              ;; Continue to next link
              (tlon-lychee--process-next-dead-link remaining-links total-dead-links
                                                   action-counts-ref processed-links-count-ref
                                                   stderr-content)))))
         ((eq action ?w)
          (tlon-lychee--add-to-whitelist url)
          (cl-incf (plist-get action-counts-ref :whitelisted))
          ;; Continue to next link
          (tlon-lychee--process-next-dead-link remaining-links total-dead-links
                                               action-counts-ref processed-links-count-ref
                                               stderr-content))
         ((eq action ?d)
          (if (tlon-lychee-remove-url-from-file full-file-path url)
              (progn
                (cl-incf (plist-get action-counts-ref :removed))
                (message "Removed: %s from %s" url filename))
            (message "URL not found for removal in %s" filename))
          ;; Continue to next link
          (tlon-lychee--process-next-dead-link remaining-links total-dead-links
                                               action-counts-ref processed-links-count-ref
                                               stderr-content))
         ((eq action ?s)
          (message "Skipped: %s" url)
          (cl-incf (plist-get action-counts-ref :skipped))
          ;; Continue to next link
          (tlon-lychee--process-next-dead-link remaining-links total-dead-links
                                               action-counts-ref processed-links-count-ref
                                               stderr-content))
         ((eq action ?q)
          (let ((archived (plist-get action-counts-ref :archived))
                (replaced (plist-get action-counts-ref :replaced))
                (removed (plist-get action-counts-ref :removed))
                (whitelisted (plist-get action-counts-ref :whitelisted))
                (skipped (plist-get action-counts-ref :skipped)))
            (message "Aborted. Processed %d out of %d dead links.\nActions taken: %d archived, %d replaced, %d removed, %d whitelisted, %d skipped."
                     (car processed-links-count-ref) total-dead-links
                     archived replaced removed whitelisted skipped))))))))

(defun tlon-lychee--handle-archive-response (archive-url original-dead-url
							 full-file-path filename
							 remaining-links total-dead-links
							 action-counts-ref processed-links-count-ref
							 stderr-content)
  "Handle response from Wayback Machine for ORIGINAL-DEAD-URL.
This is called only when user chooses the archive option.

ARCHIVE-URL is the archived URL from the Wayback Machine, or nil if none found.
ORIGINAL-DEAD-URL is the original dead URL to be replaced.
FULL-FILE-PATH is the complete path to the file containing the dead link.
FILENAME is the name of the file for display purposes.
REMAINING-LINKS is the list of remaining dead links to process.
TOTAL-DEAD-LINKS is the total number of dead links found.
ACTION-COUNTS-REF is a reference to the plist of action counts.
PROCESSED-LINKS-COUNT-REF is a reference to the count of processed links.
STDERR-CONTENT is the stderr output from the lychee command."
  (if archive-url
      (progn
        (if (tlon-lychee-replace-in-file full-file-path original-dead-url archive-url)
            (progn
              (cl-incf (plist-get action-counts-ref :archived))
              (message "Replaced: %s -> %s in %s" original-dead-url archive-url filename))
          (message "Archive for %s found (%s), but no replacement made in %s (URL not found?)"
                   original-dead-url archive-url filename))
        ;; Process the next link in the queue
        (tlon-lychee--process-next-dead-link remaining-links total-dead-links
                                             action-counts-ref processed-links-count-ref
                                             stderr-content))
    ;; No archive available - prompt user for other options
    (message "No archive available for %s" original-dead-url)
    (let* ((current-link (list :url original-dead-url
                               :file-path full-file-path
                               :filename filename))
           (dead-links-queue (cons current-link remaining-links)))
      ;; Return to main prompt with current link back in queue, skip browser open
      (tlon-lychee--process-next-dead-link dead-links-queue total-dead-links
                                           action-counts-ref processed-links-count-ref
                                           stderr-content t))))

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
  [["List dead URLs"
    ("l" "In file"                      tlon-list-dead-urls-in-file)
    ("L" "In repo"                      tlon-list-dead-urls-in-repo)
    ""
    "Fix dead URLs"
    ("f" "In file"                       tlon-fix-dead-urls-in-file)
    ("F" "In repo"                       tlon-fix-dead-urls-in-repo)
    ""
    ("a" "Get archived"                  tlon-get-archived)
    ("e" "Get earliest archived"         tlon-get-earliest-archived)
    ("p" "Replace URL across projects"   tlon-replace-url-across-projects)]])

(provide 'tlon-url)
;;; tlon-url.el ends here

