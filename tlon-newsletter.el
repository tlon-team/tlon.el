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

(require 'tlon-ai)
(require 'tlon-core)
(require 'tlon-glossary)
(require 'transient)

;;;; Variables

(defconst tlon-newsletter-sample-issue
  (file-name-concat tlon-package-dir "etc/sample-newsletter-issue.md")
  "File with a sample issue of the newsletter.")

;;;; User options

(defgroup tlon-newsletter nil
  "Newsletter functionality for Tlön."
  :group 'tlon)

(defcustom tlon-newsletter-model nil
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
      (if (string-blank-p content)
          (message "The latest newsletter input file is empty. Nothing to process.")
        (let* ((prompt-file-name "newsletter-prompt.md")
               (prompt-file-path (file-name-concat tlon-package-dir "etc" prompt-file-name)))
          (unless (file-exists-p prompt-file-path)
            (user-error "Prompt file not found: %s" prompt-file-path))
          (let* ((raw-prompt (with-temp-buffer
                               (insert-file-contents prompt-file-path)
                               (buffer-string)))
                 (sample-issue-content
                  (if (file-exists-p tlon-newsletter-sample-issue)
                      (with-temp-buffer
                        (insert-file-contents tlon-newsletter-sample-issue)
                        (buffer-string))
                    (progn
                      (message "Warning: Sample issue file not found: %s. Proceeding without example."
			       tlon-newsletter-sample-issue)
                      "")))
                 (final-prompt (format raw-prompt sample-issue-content content)))
            (if (string-blank-p raw-prompt) ; Check original prompt file for blankness
                (user-error "Prompt file %s is empty" prompt-file-path)
              (progn
                (message "Requesting AI to draft newsletter issue (input: %s, prompt: %s)..." input-file-path prompt-file-path)
                (tlon-make-gptel-request final-prompt
                                         nil
                                         #'tlon-newsletter--create-issue-callback
                                         tlon-newsletter-model
                                         t   ; Skip context check
                                         nil ; Request buffer
                                         (list "search" "fetch_content") ; Tools
                                         input-file-path)))))) ; Pass input-file-path as context-data
    (message "Could not create newsletter issue due to previous errors.")))

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

(defun tlon-newsletter--get-latest-input-content ()
  "Find and return the content of the most recent file in \"boletin/numeros/\".
Returns nil and signals a user-error if any step fails."
  (let* ((boletin-repo-dir (tlon-repo-lookup :dir :name "boletin"))
	 (numeros-subdir-name "numeros/")
	 numeros-dir files-and-attrs sorted-files latest-file-path)
    (unless boletin-repo-dir
      (user-error "Could not find the 'boletin' repository")
      (cl-return-from tlon-newsletter--get-latest-input-content nil))
    (setq numeros-dir (file-name-concat boletin-repo-dir numeros-subdir-name))
    (unless (file-directory-p numeros-dir)
      (user-error "The \"numeros\" subdirectory does not exist or is not a directory in %s" boletin-repo-dir)
      (cl-return-from tlon-newsletter--get-latest-input-content nil))
    (setq files-and-attrs (directory-files-and-attributes numeros-dir nil nil nil 'full))
    (unless files-and-attrs
      (user-error "No files found in %s" numeros-dir)
      (cl-return-from tlon-newsletter--get-latest-input-content nil))
    ;; Filter out directories, keeping only regular files
    (setq files-and-attrs (cl-remove-if-not (lambda (entry) (file-regular-p (car entry))) files-and-attrs))
    (if (not files-and-attrs)
        (let ((selected-file (read-file-name "Select a file: " numeros-dir nil t)))
	  (message "No regular files found in %s. Prompting user to select a file." numeros-dir)
          (if (and selected-file (file-exists-p selected-file))
              (cons (with-temp-buffer
                      (insert-file-contents selected-file)
                      (buffer-string))
                    selected-file) ; Return (content . path)
            (user-error "No file selected or selected file does not exist")))
      ;; Regular files were found, proceed with sorting and getting the latest
      (setq sorted-files (sort files-and-attrs
                               (lambda (a b)
                                 (time-less-p (nth 5 b) (nth 5 a))))) ; Sort by modification time, most recent first
      (setq latest-file-path (car (car sorted-files)))
      ;; Ensure latest-file-path is not nil before calling file-exists-p,
      ;; and also handle if it's a string but file doesn't exist.
      (unless (and latest-file-path (file-exists-p latest-file-path))
        (user-error "Latest file %s does not exist or could not be determined" latest-file-path)
        (cl-return-from tlon-newsletter--get-latest-input-content nil))
      (condition-case err
          (cons (with-temp-buffer
                  (insert-file-contents latest-file-path)
                  (buffer-string))
                latest-file-path) ; Return (content . path)
        (error (user-error "Error reading content from %s: %s" latest-file-path (error-message-string err))
               nil)))))

(defun tlon-newsletter--get-en-es-glossary-string ()
  "Return the English-Spanish glossary as a TSV string.
Uses `tlon-parse-glossary' and `tlon-insert-formatted-glossary' with
recipient `deepl-api'."
  (let* ((json-glossary (tlon-parse-glossary))
         (language "es")
         (recipient 'deepl-api))
    (if json-glossary
        (with-temp-buffer
          (tlon-insert-formatted-glossary json-glossary language recipient)
          (buffer-string))
      (progn
        (message "Warning: Glossary data could not be parsed for newsletter. Proceeding without glossary.")
        ""))))

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
    ("n" "Create newsletter issue" tlon-newsletter-create-issue)]
   ["Options"
    ("-m" "Model" tlon-newsletter-infix-select-model)]])

(provide 'tlon-newsletter)
;;; tlon-newsletter.el ends here
