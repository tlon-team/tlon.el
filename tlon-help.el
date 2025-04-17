;;; tlon-help.el --- Help functionality for Tlön -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo Stafforini

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

;; Provides functions to get help using AI, potentially leveraging documentation
;; from multiple repositories.

;;; Code:

(require 'tlon-core)
(require 'tlon-ai) ; For tlon-ai-callback-fail, tlon-ai-insert-in-buffer-and-switch-to-it
(require 'subr-x)  ; For string-join

(defvar elpaca-repos-directory) ; Define if not already globally available

;;;; Helper Functions

(defun tlon-help--get-documentation-files ()
  "Return a list of full paths to documentation files.
Documentation files are `.org` files within the 'doc/' subdirectories of the
'tlon' and 'dotfiles/emacs/extras' repositories managed by Elpaca."
  (let ((all-doc-files '())
        (doc-dirs (list (file-name-concat elpaca-repos-directory "tlon/doc/")
                        (file-name-concat elpaca-repos-directory "dotfiles/emacs/extras/doc/")))
        (doc-pattern "\\.org\\'")) ; Match only .org files at the end
    (dolist (doc-dir doc-dirs)
      (when (file-directory-p doc-dir)
        (setq all-doc-files (append all-doc-files
                                    (directory-files doc-dir t doc-pattern)))))
    (delete-dups all-doc-files))) ; Ensure uniqueness

;;;; AI Help Functions (Moved and Renamed)

;;;###autoload
(defun tlon-help-ask-ai ()
  "Ask a question about the tlon ecosystem using documentation files as context.
Collects documentation files from the standard tlon and extras doc directories,
adds them to the AI context, and sends the user's question."
  (interactive)
  (tlon-warn-if-gptel-context) ; Keep the warning from tlon-ai
  (let* ((question (read-string "What do you need help with? "))
         (all-doc-files (tlon-help--get-documentation-files))
         (existing-doc-files '())
         (prompt-template "Here is the documentation for the tlon Emacs package and related tools, found in %d file(s). Please answer the following question based *only* on this documentation:\n\n%s")
         full-prompt)
    (unless all-doc-files
      (user-error "No documentation files found in standard Elpaca doc directories."))

    ;; Add all found documentation files to the context
    (dolist (doc-file all-doc-files)
      (when (file-exists-p doc-file)
        (message "Adding documentation file to context: %s" (file-name-nondirectory doc-file))
        (gptel-context-add-file doc-file)
        (push doc-file existing-doc-files)))

    ;; Check if any files were actually added (existed)
    (unless existing-doc-files
      (user-error "Found documentation file entries, but none exist on disk"))

    ;; Now format the prompt with the actual number of files added
    (setq full-prompt (format prompt-template (length existing-doc-files) question))
    (tlon-make-gptel-request full-prompt nil #'tlon-help-ask-ai-callback nil 'no-context-check)
    (message "Preparing your answer using %d documentation file(s)..." (length existing-doc-files))))

(defun tlon-help-ask-ai-callback (response info)
  "Callback for `tlon-help-ask-ai'.
Displays the RESPONSE in a new buffer. If RESPONSE is nil, use
`tlon-ai-callback-fail'."
  (if (not response)
      (tlon-ai-callback-fail info) ; Use the fail callback from tlon-ai
    (let* ((buffer-name (generate-new-buffer-name "*AI Help Answer*"))
           (buffer (get-buffer-create buffer-name)))
      ;; Use the insert function from tlon-ai
      (tlon-ai-insert-in-buffer-and-switch-to-it response buffer)
      (gptel-context-remove-all))))

(provide 'tlon-help)
;;; tlon-help.el ends here

;; Local Variables:
;; coding: utf-8
;; End:
