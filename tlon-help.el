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
(require 'vterm nil t) ; Optional require for vterm fallback
(require 'aidermacs nil t) ; Optional require for aidermacs integration

;;;; Configuration

;; Define your repositories and their paths
;; Use an alist: (("repo-id" . "/path/to/repo/root") ...)
;; TODO: User should customize these paths
(defvar tlon-help-doc-repos
  '(("tlon.el" . "~/git/tlon/tlon.el") ; Example path
    ("extras" . "~/git/dotfiles/emacs/extras")) ; Example path
  "Alist mapping repository identifiers to their root paths for documentation lookup.")

;; Define the extensions for documentation files
(defvar tlon-help-doc-extensions '("org") ; Changed default to just org based on previous function
  "List of file extensions considered as documentation.")

(defvar elpaca-repos-directory) ; Define if not already globally available

;;;; Helper Functions (Moved and Renamed)

(defun tlon-help--get-documentation-files ()
  "Return a list of full paths to documentation files.
Documentation files are identified by `tlon-help-doc-extensions` within the 'doc/'
subdirectory of repositories defined in `tlon-help-doc-repos`."
  (let ((all-doc-files '()))
    (dolist (repo-entry tlon-help-doc-repos)
      (let* ((repo-path (expand-file-name (cdr repo-entry))) ; Ensure absolute path
             (doc-dir (file-name-concat repo-path "doc")))
        (when (file-directory-p doc-dir)
          (let ((doc-pattern (concat "\\.\\(" (string-join tlon-help-doc-extensions "\\|") "\\)$")))
            (setq all-doc-files (append all-doc-files
                                        (directory-files doc-dir t doc-pattern)))))))
    ;; Add extras dir separately if elpaca-repos-directory is set
    (when (boundp 'elpaca-repos-directory)
        (let* ((extras-doc-dir (file-name-concat elpaca-repos-directory "dotfiles/emacs/extras/doc/"))
               (doc-pattern (concat "\\.\\(" (string-join tlon-help-doc-extensions "\\|") "\\)$")))
          (when (file-directory-p extras-doc-dir)
            (setq all-doc-files (append all-doc-files
                                        (directory-files extras-doc-dir t doc-pattern))))))
    (delete-dups all-doc-files))) ; Ensure uniqueness

;;;; AI Help Functions (Moved and Renamed)

;;;###autoload
(defun tlon-help-ask-ai ()
  "Ask a question about the tlon ecosystem using documentation files as context.
Collects documentation files based on `tlon-help-doc-repos` and
`tlon-help-doc-extensions`, adds them to the AI context, and sends the user's
question."
  (interactive)
  (tlon-warn-if-gptel-context) ; Keep the warning from tlon-ai
  (let* ((question (read-string "What do you need help with? "))
         (all-doc-files (tlon-help--get-documentation-files))
         (existing-doc-files '())
         (prompt-template "Here is the documentation for the tlon Emacs package and related tools, found in %d file(s). Please answer the following question based *only* on this documentation:\n\n%s")
         full-prompt)
    (unless all-doc-files
      (user-error "No documentation files found. Check `tlon-help-doc-repos` and `tlon-help-doc-extensions`"))

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

;;;; Aider Integration

;;;###autoload
(defun tlon-help-ask-aider-with-external-docs (primary-repo-id)
  "Launch aider in PRIMARY-REPO-ID's directory using aidermacs or vterm.
Add documentation files (matching `tlon-help-doc-extensions` in the 'doc/'
subdirectory) from all *other* repositories defined in `tlon-help-doc-repos`
using the '--read' command-line argument.

PRIMARY-REPO-ID should be a string matching a key in `tlon-help-doc-repos`."
  (interactive
   (list (completing-read "Launch aider in repo: " (mapcar #'car tlon-help-doc-repos) nil t)))

  (let* ((primary-repo-path (cdr (assoc primary-repo-id tlon-help-doc-repos)))
         (external-repos (remove-if (lambda (repo) (string= (car repo) primary-repo-id))
                                    tlon-help-doc-repos))
         (doc-files '())
         (doc-pattern (concat "\\.\\(" (string-join tlon-help-doc-extensions "\\|") "\\)$")))

    (unless primary-repo-path
      (error "Primary repository ID '%s' not found in `tlon-help-doc-repos'" primary-repo-id))

    (setq primary-repo-path (expand-file-name primary-repo-path)) ; Ensure absolute path

    ;; Collect documentation files from external repos
    (dolist (repo external-repos)
      (let* ((repo-path (expand-file-name (cdr repo))) ; Ensure absolute path
             (doc-dir (file-name-concat repo-path "doc")))
        (when (file-directory-p doc-dir)
          (setq doc-files (append doc-files
                                  (directory-files doc-dir t doc-pattern))))))

    ;; Construct the aider arguments for --read
    (let ((aider-args (cl-loop for f in doc-files collect "--read" collect f)))

      (message "Launching aider in %s with %d external doc(s)..."
               primary-repo-path (length doc-files))

      (cond
       ;; Option 1: Use aidermacs if available
       ((fboundp 'aidermacs-start)
        (message "Using aidermacs...")
        (aidermacs-start primary-repo-path :args aider-args))

       ;; Option 2: Fallback to vterm
       ((fboundp 'vterm)
        (message "aidermacs not found, using vterm...")
        (let* ((aider-cmd-string (format "aider %s"
                                         (mapconcat #'shell-quote-argument aider-args " ")))
               (shell-command (format "cd %s && %s"
                                      (shell-quote-argument primary-repo-path)
                                      aider-cmd-string))
               (vterm-buffer (get-buffer-create "*aider-external-docs*")))
          (with-current-buffer vterm-buffer
            (unless (derived-mode-p 'vterm-mode)
              (call-interactively 'vterm)) ; Start vterm if not already running
            (vterm-send-string (concat shell-command "\n"))) ; Send the command
          (display-buffer vterm-buffer)))

       ;; Option 3: Error if neither is available
       (t
        (error "Cannot launch aider: Neither 'aidermacs-start' nor 'vterm' function found."))))))


(provide 'tlon-help)
;;; tlon-help.el ends here

;; Local Variables:
;; coding: utf-8
;; End:
