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
(require 'tlon-yaml)
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

(defcustom tlon-paragraphs-mode-line-format " ¶ %d/%d"
  "Format for the paragraph indicator in the mode line.
The format receives two integers: CURRENT paragraph and TOTAL paragraphs."
  :type 'string
  :group 'tlon-paragraphs)

;;;; Functions

;;;;; Common

(autoload 'markdown-forward-paragraph "markdown-mode" nil t)
(defun tlon-with-paragraphs (file &optional fn return-positions)
  "Execute FN for each paragraph in FILE.
If FN is nil, return the paragraph contents. If RETURN-POSITIONS is non-nil,
return list of (start . end) positions. Otherwise, return list of FN's results
for each paragraph. If FILE is nil, use the current buffer."
  (let ((buffer-to-use (if (stringp file)
                           (find-file-noselect file)
                         (current-buffer)))) ; Use current buffer if file is nil or buffer object
    (with-current-buffer buffer-to-use
      (save-excursion
	(let* ((bounds (tlon-paragraphs--content-boundaries))
               (content-begin (car bounds))
               (content-end (cdr bounds))
               result)
	  (goto-char content-begin)
          (while (and (< (point) content-end)
                      (not (looking-at-p tlon-md-local-variables-line-start)))
            (let ((start (point)))
              (markdown-forward-paragraph)
              (let ((end (min (point) content-end))
		    (fn (or fn (lambda (start end)
				 (buffer-substring-no-properties start end)))))
		(when (and (> end start)
                           (string-match-p "[^\s\n]"
					   (buffer-substring-no-properties start end)))
                  (push (if return-positions
                            (cons start end)
			  (funcall fn start end))
			result)))))
          (nreverse result))))))

(defun tlon-paragraphs--content-boundaries ()
  "Return cons (BEGIN . END) for the current buffer's main content.
BEGIN is the position just after the YAML front matter (if any) and END is the
position just before the local variables section (if any)."
  (save-excursion
    (let ((begin (or (cdr (tlon-get-delimited-region-pos tlon-yaml-delimiter))
                     (point-min)))
          (end (or (car (tlon-get-delimited-region-pos
                         tlon-md-local-variables-line-start
                         tlon-md-local-variables-line-end))
                   (point-max))))
      (cons begin end))))

;;;;; Display paragraphs

(defun tlon-paragraphs--get-comparison-buffer-content (translation original trans-paras orig-paras with-header)
  "Get paragraph comparison of TRANSLATION and ORIGINAL.
TRANS-PARAS and ORIG-PARAS are lists of paragraphs in TRANSLATION and ORIGINAL,
respectively. If WITH-HEADER is non-nil, include a header."
  (let* ((max-len (max (length orig-paras) (length trans-paras)))
         pairs)
    (dotimes (i max-len)
      (push (cons (nth i orig-paras) (nth i trans-paras)) pairs))
    (setq pairs (nreverse pairs))
    (with-temp-buffer
      (when with-header
        (insert (format "Paragraph number mismatch: \n%s has %d paragraphs\n%s has %d paragraphs\n\n"
                        (file-name-nondirectory original) (length orig-paras)
			(file-name-nondirectory translation) (length trans-paras))))
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
(defun tlon-get-corresponding-paragraphs (&optional translation original)
  "Return pairs of paragraphs between TRANSLATION and its ORIGINAL.
Signals an error if files have different number of paragraphs, and displays the
paragraphs in a buffer only in that case. If ORIGINAL is nil, infer it from
TRANSLATION."
  (let* ((translation (or translation (buffer-file-name)))
         (original (or original (tlon-get-counterpart translation)))
         (orig-paras (tlon-with-paragraphs original))
         (trans-paras (tlon-with-paragraphs translation))
         (max-len (max (length orig-paras) (length trans-paras)))
         pairs)
    (dotimes (i max-len)
      (push (cons (nth i orig-paras) (nth i trans-paras)) pairs))
    (setq pairs (nreverse pairs))
    (when (/= (length orig-paras) (length trans-paras))
      (with-current-buffer (get-buffer-create "/Paragraph Pairs/")
        (erase-buffer)
        (insert (tlon-paragraphs--get-comparison-buffer-content
                 translation original trans-paras orig-paras t))
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
      (tlon-count-paragraphs-in-region start end)
    (tlon-count-paragraphs-in-file)))

(defun tlon-count-paragraphs-in-region (start end)
  "Display the number of paragraphs in the active region or between START and END."
  (cl-destructuring-bind (start . end)
      (if (region-active-p)
          (cons (region-beginning) (region-end))
        (cons start end))
    (message "There are %d paragraphs in the selected region."
             (tlon-get-number-of-paragraphs start end))))

(defun tlon-count-paragraphs-in-file (&optional file)
  "Display the number of paragraphs in FILE.
If FILE is nil, prompt for it."
  (let ((file (or file
		  (read-file-name "File to count paragraphs in: " nil nil t
				  (file-relative-name (buffer-file-name) default-directory)))))
    (with-current-buffer (find-file-noselect file)
      (message "There are %s paragraphs in the file `%s'."
               (tlon-get-number-of-paragraphs-in-file file) (file-name-nondirectory file)))))

;;;###autoload
(defun tlon-get-number-of-paragraphs (&optional start end)
  "Return the number of paragraphs between START and END.
START and END are buffer positions. If START is nil, use the beginning of the
main content (after the YAML front matter). If END is nil, use the end of the
main content (before the local variables section). Exclude front matter and
local variables sections."
  (let* ((bounds (tlon-paragraphs--content-boundaries))
         (start (or start (car bounds)))
         (end   (or end   (cdr bounds)))
         (positions (tlon-with-paragraphs nil #'ignore t)))
    (cl-count-if (lambda (pos)
                   (and (>= (car pos) start)
                        (<=  (cdr pos) end)))
                 positions)))

(defun tlon-get-number-of-paragraphs-in-file (file)
  "Return the number of paragraphs in FILE."
  (with-current-buffer (find-file-noselect file)
    (tlon-get-number-of-paragraphs)))

;;;;; Align paragraphs

(defconst tlon-paragraphs-align-with-ai-prompt
  "You are an expert editor. The file '%s' is a translation of '%s'. They have a different number of paragraphs. The original has %d paragraphs and the translation has %d.

Here you can see a paragraph-by-paragraph breakdown of how the two files differ:

```
%s.
```

Please examine this string and determine what changes need to be made in the translation file (e.g. breaking down a paragraph into two, or consolidating two paragraphs into one) so that the number of paragraphs matches the original file.

Once you have done this, please edit the translation file ('%s') accordingly. Make only the changes needed to align the number of paragraphs in the two files; do not make other changes (such as changing the translation, modifying the markup, etc.) Use the `edit_file` tool to apply your changes.

If you think the task is too complex, because there are too many paragraphs and the discrepancies are extensive, you can tackle a subset of paragraphs, such as the first 10 paragraphs that differ (or however many paragraphs you are comfortable editing)."
  "Prompt for aligning paragraphs between a file and its counterpart.")

(defun tlon-paragraph-files-are-aligned-p (&optional file counterpart)
  "Return t iff FILE and COUNTERPART have the same number of pragraphs."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (counterpart (or counterpart (tlon-get-counterpart file)))
	 count-file count-counterpart)
    (with-current-buffer (find-file-noselect file)
      (setq count-file (tlon-get-number-of-paragraphs)))
    (with-current-buffer (find-file-noselect counterpart)
      (setq count-counterpart (tlon-get-number-of-paragraphs)))
    (= count-file count-counterpart)))

(declare-function gptel-context-add-file "gptel-context")
(declare-function gptel-context-remove-all "gptel-context")
(declare-function tlon-get-content-subtype "tlon-counterpart")
;;;###autoload
(defun tlon-paragraphs-align-with-ai (&optional file)
  "Use AI to fix paragraph count mismatch between FILE and its counterpart.
If FILE is nil, prompt for it."
  (interactive)
  (let* ((file (or file
		   (expand-file-name
		    (read-file-name "File to process: " nil nil t
				    (file-relative-name (buffer-file-name) default-directory))))))
    (if-let ((counterpart (tlon-get-counterpart file)))
        (let* ((file-subtype (tlon-get-content-subtype file))
               (original-file (if (eq file-subtype 'originals) file counterpart))
               (translation-file (if (eq file-subtype 'originals) counterpart file))
               (orig-paras (tlon-with-paragraphs original-file))
               (trans-paras (tlon-with-paragraphs translation-file))
               (original-paras-count (length orig-paras))
               (translation-paras-count (length trans-paras)))
          (if (tlon-paragraph-files-are-aligned-p file counterpart)
              (message "File `%s' and counterpart `%s' have the same number of paragraphs."
                       (file-name-nondirectory file)
                       (file-name-nondirectory counterpart))
            (let* ((comparison-string (tlon-paragraphs--get-comparison-buffer-content
				       translation-file original-file trans-paras orig-paras nil))
                   (prompt (format tlon-paragraphs-align-with-ai-prompt
				   translation-file
				   original-file
				   original-paras-count
				   translation-paras-count
				   comparison-string
				   (file-name-nondirectory translation-file)))
		   (tools '("edit_file" "apply_diff" "replace_file_contents")))
	      (gptel-context-add-file original-file)
	      (gptel-context-add-file translation-file)
	      (message "Requesting AI to align paragraphs with model %S..."
		       (cdr tlon-paragraphs-align-with-ai-model))
	      (tlon-make-gptel-request prompt nil #'tlon-paragraphs-align-with-ai-callback
				       tlon-paragraphs-align-with-ai-model t nil tools)
	      (gptel-context-remove-all))))
      (user-error "Could not find counterpart for %s" file))))

(defun tlon-paragraphs-align-with-ai-callback (response info)
  "Callback for `tlon-paragraphs-align-with-ai'.
RESPONSE is the AI's response, INFO is the response info."
  (if response
      (message "AI agent finished aligning paragraphs.")
    (tlon-ai-callback-fail info)))

;;;;; Navigation

;;;###autoload
(defun tlon-goto-paragraph (&optional n)
  "Go to paragraph N in the main content.
If N is nil, prompt for it with default as the current paragraph.
N is 1-based. Signal an error if there are no paragraphs."
  (interactive)
  (let* ((positions (tlon-paragraphs--positions))
         (total (length positions)))
    (unless (> total 0)
      (user-error "No paragraphs in main content"))
    (let* ((current (car (tlon-paragraphs--index-and-total)))
           (n (or n (read-number (format "Go to paragraph (1-%d, default %d): " total current)
                                 current))))
      (unless (and (integerp n) (<= 1 n total))
        (user-error "Paragraph number must be between 1 and %d" total))
      (goto-char (car (nth (1- n) positions)))
      (message "Moved to paragraph %d/%d" n total))))

;;;;; Mode line indicator

(defvar-local tlon-paragraphs-mode-line nil
  "Mode line construct used by `tlon-paragraphs-mode-line-mode'.")

(defvar-local tlon-paragraphs--positions-cache nil
  "Cached list of (START . END) paragraph positions for the current buffer.")

(defvar-local tlon-paragraphs--tick-cache 0
  "Value for `tlon-paragraphs--positions-cache' from `buffer-chars-modified-tick'.")

(add-to-list 'minor-mode-alist
             '(tlon-paragraphs-mode-line-mode tlon-paragraphs-mode-line))

(declare-function doom-modeline-set-modeline "doom-modeline-core" (modeline &optional default))
;;;###autoload
(define-minor-mode tlon-paragraphs-mode-line-mode
  "Show the paragraph number at point in the mode line."
  :init-value nil
  :lighter nil
  :group 'tlon-paragraphs
  (if tlon-paragraphs-mode-line-mode
      (progn
        (setq tlon-paragraphs-mode-line
              '(" " (:eval (tlon-paragraphs-mode-line-string))))
        (force-mode-line-update)
        (when (bound-and-true-p doom-modeline-mode)
          (when (fboundp 'doom-modeline-set-modeline)
            (doom-modeline-set-modeline 'main))))
    (setq tlon-paragraphs-mode-line nil)
    (force-mode-line-update)
    (when (bound-and-true-p doom-modeline-mode)
      (when (fboundp 'doom-modeline-set-modeline)
        (doom-modeline-set-modeline 'main)))))

(defun tlon-paragraphs-mode-line-string ()
  "Return a `mode-line-format' fragment with the paragraph indicator."
  (let* ((bounds (tlon-paragraphs--content-boundaries))
         (start (car bounds))
         (end (cdr bounds)))
    (when (and (>= (point) start) (<= (point) end))
      (let ((pair (tlon-paragraphs--index-and-total)))
        (when pair
          (format tlon-paragraphs-mode-line-format (car pair) (cdr pair)))))))

(defun tlon-paragraphs--index-and-total ()
  "Return cons (CURRENT . TOTAL) for the paragraph at point."
  (let* ((positions (tlon-paragraphs--positions))
         (total (length positions)))
    (when (> total 0)
      (let ((pt (point)) (i 0) idx)
        (while (< i total)
          (let* ((pos (nth i positions)) (s (car pos)) (e (cdr pos)))
            (if (and (>= pt s) (< pt e))
                (setq idx (1+ i) i total)
              (setq i (1+ i)))))
        (cons (or idx total) total)))))

(defun tlon-paragraphs--positions ()
  "Return cached list of paragraph positions for the current buffer."
  (let ((tick (buffer-chars-modified-tick)))
    (if (and tlon-paragraphs--positions-cache
             (= tick tlon-paragraphs--tick-cache))
        tlon-paragraphs--positions-cache
      (setq tlon-paragraphs--positions-cache (tlon-with-paragraphs nil #'ignore t)
            tlon-paragraphs--tick-cache tick))))

;;;;;; Doom modeline integration

(with-eval-after-load 'doom-modeline
  (require 'doom-modeline-core nil t)
  (require 'doom-modeline-segments nil t)
  ;; Quote macro form to avoid compile-time expansion when doom-modeline isn't available,
  ;; which would otherwise trigger a byte-compiler warning.
  (eval
   '(doom-modeline-def-segment tlon-paragraph
      (when (and (bound-and-true-p tlon-paragraphs-mode-line-mode))
        (tlon-paragraphs-mode-line-string)))))

;;;;; Menu

(transient-define-infix tlon-paragraphs-infix-select-align-model ()
  "AI model to use for aligning paragraphs.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-paragraphs-align-with-ai-model)

;;;###autoload (autoload 'tlon-paragraphs-menu "tlon-paragraphs" nil t)
(transient-define-prefix tlon-paragraphs-menu ()
  "Menu for `tlon-paragraphs' functions."
  [["Commands"
    ("p" "Toggle paragraphs in mode line"               tlon-paragraphs-mode-line-mode)
    ("a" "Align paragraphs"                             tlon-paragraphs-align-with-ai)
    ("c" "Count paragraphs"                             tlon-count-paragraphs)
    ("g" "Go to paragraph"                              tlon-goto-paragraph)
    ("d" "Display corresponding paragraphs"             tlon-display-corresponding-paragraphs)]
   ["Options"
    ("-a" "Align paragraphs model"                      tlon-paragraphs-infix-select-align-model)]])

(provide 'tlon-paragraphs)
;;; tlon-paragraphs.el ends here
