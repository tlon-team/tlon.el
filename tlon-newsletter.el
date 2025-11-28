;;; tlon-newsletter.el --- Newsletter functionality for Tlön -*- lexical-binding: t -*-

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

;; Newsletter functionality for Tlön.

;;; Code:

(require 'markdown-mode)
(require 'transient)
(require 'tlon-ai)
(require 'tlon-core)

;;;; Variables

;;;;; Dirs

(defconst tlon-newsletter-repo-dir
  (tlon-repo-lookup :dir :name "boletin")
  "Path to the local clone of the newsletter repository.")

(defconst tlon-newsletter-numeros-subdir
  (file-name-concat tlon-newsletter-repo-dir "numeros/")
  "Path to the \"numeros\" subdirectory in the newsletter repository.")

;;;;; Files

(defconst tlon-newsletter-prompt-file
  (file-name-concat tlon-package-dir "etc/newsletter-prompt.md")
  "Path to the prompt file used for creating newsletter issues.")

(defconst tlon-newsletter-sample-issue-file
  (file-name-concat tlon-newsletter-numeros-subdir "2025-08.md")
  "File with a sample issue of the newsletter.")

;;;; User options

(defgroup tlon-newsletter nil
  "Newsletter functionality for Tlön."
  :group 'tlon)

(defcustom tlon-newsletter-model '("Gemini" . gemini-pro-latest)
  "Model to use for creating newsletter issues.
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, do not
use a different model for creating a newsletter issue."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-newsletter)

;;;; Functions

;;;;; Create issue

;;;###autoload
(defun tlon-newsletter-create-issue ()
  "Create a draft for a new newsletter issue using AI.
Reads the most recent file from \"boletin/numeros/\", processes each line (URL
or text) to generate a one-paragraph summary, and structures the output as a
Spanish newsletter in Markdown with sections for articles, events, and other
news. The original input file is then overwritten with this new draft."
  (interactive)
  (if-let* ((input-data (tlon-newsletter--get-latest-input-content))
	    (content (car input-data))
	    (input-file-path (cdr input-data)))
      (progn
	(tlon-newsletter-confirm-when-file-empty content)
	(tlon-newsletter-ensure-prompt-file-exists)
	(tlon-newsletter-ensure-sample-issue-file-exists)
	(tlon-newsletter--process-and-request content input-file-path))
    (user-error "Could not create newsletter issue due to previous errors")))

(defun tlon-newsletter--process-and-request (content input-file-path)
  "Process CONTENT and make AI request for INPUT-FILE-PATH."
  (let* ((raw-prompt (with-temp-buffer
		       (insert-file-contents tlon-newsletter-prompt-file)
		       (buffer-string)))
	 (sample-issue-content (with-temp-buffer
				 (insert-file-contents tlon-newsletter-sample-issue-file)
				 (buffer-string)))
	 (issue-month (tlon-newsletter-get-issue-month (file-name-base input-file-path)))
	 (content-year (tlon-newsletter-get-content-year (file-name-base input-file-path)))
	 (content-month (tlon-newsletter-get-previous-month issue-month))
	 (content-month-year (format "%s de %s" content-month content-year))
	 (final-prompt (tlon-ai-maybe-edit-prompt
			(format raw-prompt content-month-year issue-month sample-issue-content content))))
    (tlon-make-gptel-request
     final-prompt
     nil
     #'tlon-newsletter--create-issue-callback
     tlon-newsletter-model
     'skip-content-check
     nil
     (list (list "mcp-ddg-search" "search")
	   (list "mcp-ddg-search" "fetch_content")
	   (list "mcp-slack-ae-racionalidad" "slack_list_channels")
	   (list "mcp-slack-ae-racionalidad" "slack_get_channel_history"))
     input-file-path
     (list "ddg-search" "slack-ae-racionalidad"))
    (message "Requesting AI to draft newsletter issue (input: %s, prompt: %s)..."
	     input-file-path tlon-newsletter-prompt-file)))

(defun tlon-newsletter--get-latest-input-content ()
  "Return content and path of next month's issue file, creating it if missing."
  (cl-destructuring-bind (next-year . next-month) (tlon-newsletter--next-year-month)
    (let* ((filename (format "%04d-%02d.md" next-year next-month))
           (target (file-name-concat tlon-newsletter-numeros-subdir filename)))
      (tlon-newsletter-ensure-repo-dir-exists)
      (tlon-newsletter-ensure-numeros-subdir-exists)
      (unless (file-exists-p target)
	(with-temp-buffer
          (write-file target nil)))
      (cons (with-temp-buffer
              (insert-file-contents target)
              (buffer-string))
            target))))

(defun tlon-newsletter--create-issue-callback (response info)
  "Callback for `tlon-newsletter-create-issue'.
Writes the AI-generated newsletter draft to the original input file, replacing
its contents, and then opens the file. Only processes RESPONSE if it's a
string (final AI output). Otherwise, call `tlon-ai-callback-fail' with the
response INFO."
  (if (not response)
      (tlon-ai-callback-fail info)
    (when (stringp response) ; Ensure response is the final text
      (let ((original-file-path (plist-get info :context)))
        (if (and original-file-path (stringp original-file-path) (not (string-empty-p original-file-path)))
            (progn
              (with-temp-buffer
                (insert response)
                (write-file original-file-path nil)) ; Overwrites original file, creates backup
              (message "Newsletter draft updated in %s. Opening file..." original-file-path)
              (find-file original-file-path)) ; Open the updated file
          (user-error "Original input file path not found in callback info or is invalid"))))))

;;;;;; Ensure & confirm helpers

(defun tlon-newsletter-ensure-repo-dir-exists ()
  "Ensure the newsletter repository directory exists."
  (unless (file-exists-p tlon-newsletter-repo-dir)
    (user-error "Newsletter repo not found: %s" tlon-newsletter-prompt-file)))

(defun tlon-newsletter-ensure-numeros-subdir-exists ()
  "Ensure the \"numeros\" subdirectory in the newsletter repository exists."
  (unless (file-exists-p tlon-newsletter-numeros-subdir)
    (user-error "\"numeros\" subdirectory not found: %s" tlon-newsletter-numeros-subdir)))

(defun tlon-newsletter-ensure-prompt-file-exists ()
  "Ensure the prompt file for creating newsletter issues exists."
  (unless (file-exists-p tlon-newsletter-prompt-file)
    (user-error "Prompt file not found: %s" tlon-newsletter-prompt-file)))

(defun tlon-newsletter-ensure-sample-issue-file-exists ()
  "Ensure the sample issue file exists."
  (unless (file-exists-p tlon-newsletter-sample-issue-file)
    (user-error "Sample issue file not found: %s" tlon-newsletter-sample-issue-file)))

(defun tlon-newsletter-confirm-when-file-empty (content)
  "Prompt user for confirmation if CONTENT is empty."
  (when (string-blank-p content)
    (unless (y-or-n-p "The latest newsletter input file is empty. Proceed?")
      (user-error "Aborted"))))

;;;;;; Handle dates

(defun tlon-newsletter--next-year-month (&optional time)
  "Return the YEAR and MONTH of the next calendar month as a cons cell.
If TIME is non-nil, compute relative to TIME; otherwise use current time."
  (let* ((dt (decode-time (or time (current-time))))
         (year (nth 5 dt))
         (month (nth 4 dt)))
    (if (= month 12)
        (cons (1+ year) 1)
      (cons year (1+ month)))))

(defun tlon-newsletter-get-issue-month (date)
  "Return the lowercase month name for date string DATE.
DATE must be in format \"yyyy-mm\"."
  (let* ((parsed (tlon-newsletter--validate-date-format date))
         (year (car parsed))
         (month (cdr parsed)))
    (let ((system-time-locale "es_ES.UTF-8"))
      (downcase
       (format-time-string "%B" (encode-time 0 0 0 1 month year))))))

(defun tlon-newsletter-get-content-year (date)
  "Return the year of the content period from date string DATE.
DATE is the publication date, in format \"yyyy-mm\". Since the newsletter is
published on the first day of the month, the content period is the previous
month. For example, if DATE is \"2025-01\", the content period is December 2024,
so this function returns 2024."
  (let* ((parsed (tlon-newsletter--validate-date-format date))
         (year (car parsed))
         (month (cdr parsed)))
    (if (= month 1)
        (1- year)
      year)))

(defun tlon-newsletter--validate-date-format (date)
  "Validate that DATE is in format \"yyyy-mm\" and return parsed components.
Returns a cons cell (YEAR . MONTH) with numeric values, or signals an error."
  (unless (and (stringp date) (string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}$" date))
    (error "Invalid date format, expected yyyy-mm"))
  (let* ((parts (split-string date "-"))
         (year (string-to-number (nth 0 parts)))
         (month (string-to-number (nth 1 parts))))
    (unless (and (>= month 1) (<= month 12))
      (error "Invalid month value"))
    (cons year month)))

(defun tlon-newsletter-get-previous-month (month-name)
  "Return the previous month name given MONTH-NAME."
  (let* ((months '("enero" "febrero" "marzo" "abril" "mayo" "junio"
                   "julio" "agosto" "septiembre" "octubre" "noviembre" "diciembre"))
         (pos (cl-position month-name months :test 'string-equal-ignore-case)))
    (nth (mod (1- pos) 12) months)))

;;;;; Publish issue

;;;###autoload
(defun tlon-newsletter-publish-issue ()
  "Assist in publishing the current newsletter issue.
Copy the issue title to the kill ring, open the Substack publish URL, visit
the current issue Markdown file and preview it, then in the EWW preview buffer
open the page in the external browser."
  (interactive)
  (cl-destructuring-bind (content . path)
      (tlon-newsletter--get-latest-input-content)
    (ignore content)
    (let* ((base (file-name-base path))
           (ym (tlon-newsletter--validate-date-format base))
           (year (car ym))
           (month-name (tlon-newsletter-get-issue-month base))
           (title (format "Boletín de %s de %d" month-name year)))
      (kill-new title)
      (browse-url "https://altruismoeficaz.substack.com/publish/post/")
      (find-file path)
      (when (fboundp 'tlon-ensure-markdown-mode)
        (tlon-ensure-markdown-mode))
      (markdown-preview)
      (run-at-time 0.5 nil #'tlon-newsletter--open-eww-preview-externally))))

(defun tlon-newsletter--open-eww-preview-externally ()
  "Open the most recent EWW preview buffer in the external browser."
  (let ((eww-buf
         (catch 'found
           (dolist (buf (buffer-list))
             (when (with-current-buffer buf (derived-mode-p 'eww-mode))
               (throw 'found buf)))
           nil)))
    (when eww-buf
      (with-current-buffer eww-buf
        (when (fboundp 'eww-browse-with-external-browser)
          (eww-browse-with-external-browser))))))

;;;;; Menu

(transient-define-infix tlon-newsletter-infix-select-model ()
  "AI model to use for creating a newsletter issue.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-newsletter-model)

;;;###autoload (autoload 'tlon-newsletter-menu "tlon-newsletter" nil t)
(transient-define-prefix tlon-newsletter-menu ()
  "Menu for `tlon-newsletter'."
  :info-manual "(tlon) newsletter"
  [["Commands"
    ("n" "Create issue"            tlon-newsletter-create-issue)
    ("p" "Publish issue"           tlon-newsletter-publish-issue)]
   ["Options"
    ("-m" "Model" tlon-newsletter-infix-select-model)]])

(provide 'tlon-newsletter)
;;; tlon-newsletter.el ends here
