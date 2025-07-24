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
(require 'tlon-md)
(require 'transient)

;;;; User options

(defgroup tlon-paragraphs nil
  "Paragraph-related functionality for Tlön."
  :group 'tlon)

(defcustom tlon-paragraphs-align-with-ai-model
  '("Claude" . claude-sonnet-4-20250514)
  "Model to use for aligning paragraphs.
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for available options. If nil, do not
use a different model for aligning paragraphs."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-paragraphs)

;;;; Functions

;;;;; Common

(autoload 'markdown-forward-paragraph "markdown-mode" nil t)
(defun tlon-with-paragraphs (file fn &optional return-positions)
  "Execute FN for each paragraph in FILE.
If RETURN-POSITIONS is non-nil, return list of (start . end) positions.
Otherwise, return list of FN's results for each paragraph.
If FILE is nil, use the current buffer."
  (let ((buffer-to-use (if (stringp file)
                           (find-file-noselect file)
                         (current-buffer)))) ; Use current buffer if file is nil or buffer object
    (with-current-buffer buffer-to-use
      (save-excursion
        (goto-char (or (cdr (tlon-get-delimited-region-pos
                             tlon-yaml-delimiter))
                       (point-min)))
	(let ((content-end (or (car (tlon-get-delimited-region-pos
                                     tlon-md-local-variables-line-start
                                     tlon-md-local-variables-line-end))
                               (point-max)))
              result)
          (while (and (< (point) content-end)
                      (not (looking-at-p tlon-md-local-variables-line-start)))
            (let ((start (point)))
              (markdown-forward-paragraph)
              (let ((end (min (point) content-end)))
		(when (and (> end start)
                           (string-match-p "[^\s\n]"
					   (buffer-substring-no-properties start end)))
                  (push (if return-positions
                            (cons start end)
			  (funcall fn start end))
			result)))))
          (nreverse result))))))

;;;;; Display paragraphs

(defun tlon-paragraphs--get-comparison-buffer-content (file counterpart orig-paras trans-paras with-header)
  "Get paragraph comparison of FILE and COUNTERPART.
Take ORIG-PARAS and TRANS-PARAS. If WITH-HEADER is non-nil, include a header."
  (let* ((max-len (max (length orig-paras) (length trans-paras)))
         pairs)
    (dotimes (i max-len)
      (push (cons (nth i orig-paras) (nth i trans-paras)) pairs))
    (setq pairs (nreverse pairs))
    (with-temp-buffer
      (when with-header
        (insert (format "Paragraph number mismatch: \n%s has %d paragraphs\n%s has %d paragraphs\n\n"
                        (file-name-nondirectory file) (length orig-paras)
                        (file-name-nondirectory counterpart) (length trans-paras))))
      (dolist (pair pairs)
        (insert "Original:\n"
                (or (car pair) "[Missing paragraph]")
                "\n\nTranslation:\n"
                (or (cdr pair) "[Missing paragraph]")
                "\n\n"
                (make-string 40 ?-)
                "\n\n"))
      (buffer-string))))

;;;###autoload
(defun tlon-display-corresponding-paragraphs (pairs-or-fn)
  "Display PAIRS-OR-FN of corresponding paragraphs in parallel.
PAIRS-OR-FN can be either the output of `tlon-get-corresponding-paragraphs'
or the function itself."
  (interactive (list #'tlon-get-corresponding-paragraphs))
  (condition-case _err
      (let* ((pairs (if (functionp pairs-or-fn)
                        (funcall pairs-or-fn)
                      pairs-or-fn))
             (buf (get-buffer-create "/Paragraph Pairs/")))
        (with-current-buffer buf
          (erase-buffer)
          (dolist (pair pairs)
            (insert "Original:\n"
                    (or (car pair) "[Missing paragraph]")
                    "\n\nTranslation:\n"
                    (or (cdr pair) "[Missing paragraph]")
                    "\n\n"
                    (make-string 40 ?-)
                    "\n\n"))
          (goto-char (point-min)))
        (display-buffer buf))
    (user-error
     (display-buffer (get-buffer "/Paragraph Pairs/")))))

(declare-function tlon-get-counterpart "tlon-counterpart")
(defun tlon-get-corresponding-paragraphs (&optional file counterpart)
  "Return pairs of paragraphs between FILE and its COUNTERPART.
Signals an error if files have different number of paragraphs, and displays the
paragraphs in a buffer only in that case. If COUNTERPART is nil, infer it from
FILE."
  (let* ((file (or file (buffer-file-name)))
         (counterpart (or counterpart (tlon-get-counterpart file)))
         (orig-paras (tlon-with-paragraphs file
					   (lambda (start end)
					     (buffer-substring-no-properties start end))))
         (trans-paras (tlon-with-paragraphs counterpart
					    (lambda (start end)
					      (buffer-substring-no-properties start end))))
         (max-len (max (length orig-paras) (length trans-paras)))
         pairs)
    (dotimes (i max-len)
      (push (cons (nth i orig-paras) (nth i trans-paras)) pairs))
    (setq pairs (nreverse pairs))
    (when (/= (length orig-paras) (length trans-paras))
      (with-current-buffer (get-buffer-create "/Paragraph Pairs/")
        (erase-buffer)
        (insert (tlon-paragraphs--get-comparison-buffer-content
                 file counterpart orig-paras trans-paras t))
        (goto-char (point-min))
        (display-buffer (current-buffer))
        (user-error "Paragraph number mismatch")))
    pairs))

;;;;; Count paragraphs

(defun tlon-count-paragraphs (&optional start end)
  "Count paragraphs in the active region or in a file.
If the region is active, count paragraphs in it. When called with START and END,
count paragraphs in that range. If called interactively without an active
region, prompt for a file and count its paragraphs."
  (interactive)
  (if (or (region-active-p) (and start end))
      (cl-destructuring-bind (start . end)
          (if (region-active-p)
              (cons (region-beginning) (region-end))
            (cons start end))
        (message "There are %d paragraphs in the selected region."
                 (tlon-get-number-of-paragraphs start end)))
    (let ((file (read-file-name "File to count paragraphs in: " nil nil t
				(file-relative-name (buffer-file-name) default-directory))))
      (with-current-buffer (find-file-noselect file)
        (message "There are %s paragraphs in the file `%s'."
                 (tlon-get-number-of-paragraphs) (file-name-nondirectory file))))))

;;;###autoload
(defun tlon-get-number-of-paragraphs (&optional start end)
  "Return the number of paragraphs between START and END.
START and END are buffer positions. If START is nil, use `point-min'.
If END is nil, use `point-max'."
  (let ((positions (tlon-with-paragraphs nil #'ignore t)))
    (cl-count-if (lambda (pos)
		   (and (>= (car pos) (or start (point-min)))
			(< (cdr pos) (or end (point-max)))))
		 positions)))

;;;;; Align paragraphs

(defconst tlon-paragraphs-align-with-ai-prompt
  "You are an expert editor. The file '%s' is a translation of '%s'. They have a different number of paragraphs. The original has %d paragraphs and the translation has %d.

Please edit the translation file ('%s') to ensure it has the same number of paragraphs as the original. You might need to merge or split paragraphs. Do not change the content otherwise. You must use the `edit_file` tool to apply your changes."
  "Prompt for aligning paragraphs between a file and its counterpart.")

(declare-function gptel-context-add-file "gptel-context")
(declare-function gptel-context-remove-all "gptel-context")
(declare-function tlon-get-content-subtype "tlon-counterpart")
;;;###autoload
(defun tlon-paragraphs-align-with-ai ()
  "Check for paragraph count mismatch and use AI to fix it."
  (interactive)
  (let* ((file (expand-file-name
		(read-file-name "File to process: " nil nil t
                                (file-relative-name (buffer-file-name) default-directory)))))
    (if-let ((counterpart (tlon-get-counterpart file)))
        (let* ((file-subtype (tlon-get-content-subtype file))
               (original-file (if (eq file-subtype 'originals) file counterpart))
               (translation-file (if (eq file-subtype 'originals) counterpart file))
               (orig-paras (tlon-with-paragraphs original-file
                                                 (lambda (start end)
                                                   (buffer-substring-no-properties start end))))
               (trans-paras (tlon-with-paragraphs translation-file
                                                  (lambda (start end)
                                                    (buffer-substring-no-properties start end))))
               (original-paras-count (length orig-paras))
               (translation-paras-count (length trans-paras)))
          (if (= original-paras-count translation-paras-count)
              (message "File `%s' and counterpart `%s' have the same number of paragraphs (%d)."
                       (file-name-nondirectory file)
                       (file-name-nondirectory counterpart)
                       original-paras-count)
            (let* ((comparison-string
                    (tlon-paragraphs--get-comparison-buffer-content
                     original-file translation-file orig-paras trans-paras nil))
                   (prompt (format (concat tlon-paragraphs-align-with-ai-prompt
                                           "\n\nHere's a paragraph-by-paragraph comparison to help you:\n\n%s")
                                   (file-name-nondirectory translation-file)
                                   (file-name-nondirectory original-file)
                                   original-paras-count
                                   translation-paras-count
                                   (file-name-nondirectory translation-file)
                                   comparison-string))
                   (tools '("edit_file")))
              (gptel-context-add-file original-file)
              (gptel-context-add-file translation-file)
              (message "Requesting AI to align paragraphs with model %S..."
                       (cdr tlon-paragraphs-align-with-ai-model))
              (tlon-make-gptel-request prompt nil #'tlon-paragraphs-align-with-ai-callback tlon-paragraphs-align-with-ai-model t nil tools)
              (gptel-context-remove-all))))
      (user-error "Could not find counterpart for %s" file))))

(defun tlon-paragraphs-align-with-ai-callback (response info)
  "Callback for `tlon-paragraphs-align-with-ai'.
RESPONSE is the AI's response, INFO is the response info."
  (if response
      (message "AI agent finished aligning paragraphs.")
    (tlon-ai-callback-fail info)))

(transient-define-infix tlon-paragraphs-infix-select-align-model ()
  "AI model to use for aligning paragraphs.
If nil, use the default model."
  :class 'tlon-ai-model-selection-infix
  :variable 'tlon-paragraphs-align-with-ai-model)

;;;;; Menu

;;;###autoload (autoload 'tlon-paragraphs-menu "tlon-paragraphs" nil t)
(transient-define-prefix tlon-paragraphs-menu ()
  "Menu for `tlon-paragraphs' functions."
  [["Commands"
    ("a" "Align paragraphs"                             tlon-paragraphs-align-with-ai)
    ("c" "Count paragraphs"                             tlon-count-paragraphs)
    ("d" "Display corresponding paragraphs"             tlon-display-corresponding-paragraphs)]
   ["Options"
    ("-a" "Align paragraphs model"                      tlon-paragraphs-infix-select-align-model)]])

(provide 'tlon-paragraphs)
;;; tlon-paragraphs.el ends here
