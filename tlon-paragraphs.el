;;; tlon-paragraphs.el --- Paragraph-related functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon

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

;; Paragraph-related functionality.

;;; Code:

(require 'tlon-ai)
(require 'tlon-counterpart)
(require 'transient)

;;;; Functions

(defconst tlon-paragraphs-align-with-ai-prompt
  "You are an expert editor. The file '%s' is a translation of '%s'. They have a different number of paragraphs. The original has %d paragraphs and the translation has %d.

Please edit the translation file ('%s') to ensure it has the same number of paragraphs as the original. You might need to merge or split paragraphs. Do not change the content otherwise. You must use the `edit_file` tool to apply your changes.

Here are the contents of the original file:
```markdown
%s
```

Here are the contents of the translation file:
```markdown
%s
```"
  "Prompt for aligning paragraphs between a file and its counterpart.")

;;;###autoload
(defun tlon-paragraphs-align-with-ai ()
  "Check for paragraph count mismatch and use AI to fix it."
  (interactive)
  (let* ((file (read-file-name "File to process: " nil nil t
			       (file-relative-name (buffer-file-name) default-directory))))
    (if-let ((counterpart (tlon-get-counterpart file)))
        (if-let* ((orig-paras-count (length (tlon-with-paragraphs file #'ignore t)))
                  (trans-paras-count (length (tlon-with-paragraphs counterpart #'ignore t))))
            (if (= orig-paras-count trans-paras-count)
                (message "File `%s' and counterpart `%s' have the same number of paragraphs (%d)."
                         (file-name-nondirectory file)
                         (file-name-nondirectory counterpart)
                         orig-paras-count)
              (let* ((original-content (with-temp-buffer (insert-file-contents file) (buffer-string)))
                     (counterpart-content (with-temp-buffer (insert-file-contents counterpart) (buffer-string)))
                     (prompt (format tlon-paragraphs-align-with-ai-prompt
                                     file counterpart
                                     orig-paras-count trans-paras-count
                                     counterpart
                                     original-content counterpart-content))
                     (tools '("edit_file")))
                (message "Paragraph count mismatch. Requesting AI to align paragraphs...")
                (tlon-make-gptel-request prompt nil #'tlon-paragraphs-align-with-ai-callback t nil t tools)))
          (user-error "Could not count paragraphs in one of the files"))
      (user-error "Could not find counterpart for %s" file))))

(defun tlon-paragraphs-align-with-ai-callback (response info)
  "Callback for `tlon-paragraphs-align-with-ai'.
RESPONSE is the AI's response, INFO is the response info."
  (if response
      (message "AI agent finished aligning paragraphs.")
    (tlon-ai-callback-fail info)))

;;;;; Menu

;;;###autoload (autoload 'tlon-paragraphs-menu "tlon-paragraphs" nil t)
(transient-define-prefix tlon-paragraphs-menu ()
  "Menu for `tlon-paragraphs' functions."
  [["AI"
    ("a" "Align paragraphs"             tlon-paragraphs-align-with-ai)]])

(provide 'tlon-paragraphs)
;;; tlon-paragraphs.el ends here
