;;; tlon-babel-split.el --- Minor mode for working with split windows -*- lexical-binding: t -*-

;; Copyright (C) 22024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon-babel
;; Version: 0.1

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

;; Minor mode for working with split windows.

;;; Code:

(require 'tlon-babel-counterpart)
(require 'tlon-babel-md)

;;;; Main variables

(defvar tlon-babel-split-last-screen-line-pos nil
  "Most recent point position.")

(defvar tlon-babel-split-screen-line-threshold 60
  "Number of lines below which `tlon-babel-split-autoalign-paragraphs' is disabled.")

;;;; Functions

(define-minor-mode tlon-babel-split-mode
  "Enable `tlon-babel-split-mode' locally."
  :init-value nil)

(defun tlon-babel-split-screen-line-changed-p ()
  "Return t iff the cursor in on a different screen line."
  (let* ((current-screen-line (count-screen-lines (window-start) (point)))
	 (moved-p (not (eq tlon-babel-split-last-screen-line-pos current-screen-line))))
    (setq tlon-babel-split-last-screen-line-pos current-screen-line)
    moved-p))

;; Currently this function is not used; we use
;; `tlon-babel-split-screen-line-threshold' instead, as an imperfect approximatiion.
(defun tlon-babel-split-top-of-buffer-visible-p ()
  "Return t iff the top of the buffer is visible in the current window.
The function considers the top of the buffer to be visible if the first screen
line of the buffer, or the first screen line after the metadata if any, is
visible in the current window."
  (save-restriction
    (widen)
    (save-excursion
      (let ((first-visible-line (count-screen-lines (point-min) (window-start)))
	    (first-line
	     (progn
	       (goto-char (point-max))
	       (tlon-babel-md-beginning-of-buffer-dwim)
	       (count-screen-lines (window-start) (point)))))
	(<= first-visible-line first-line)))))

(defun tlon-babel-split-screen-line-offset ()
  "Return the difference between the screen lines in the current and other windows."
  (let* ((current-screen-line (count-screen-lines (window-start) (point)))
         (other-screen-line (save-selected-window
                              (other-window 1)
                              (count-screen-lines (window-start) (point)))))
    (- other-screen-line current-screen-line)))

(defun tlon-babel-split-align-screen-lines ()
  "Align the screen lines in the current and other windows.
The alignment is performed by scrolling up or down the other window."
  (let* ((offset (tlon-babel-split-screen-line-offset)))
    (other-window 1)
    (unless (= offset 0)
      (scroll-up offset))
    (other-window -1)))

(defun tlon-babel-split-align-paragraphs ()
  "Align the paragraphs in the current and other windows.
The alignment is performed by scrolling up or down the other window."
  (cl-destructuring-bind
      (current-scren-line . current-paragraphs)
      (save-excursion
	(goto-char (1+ (point)))
	(markdown-backward-paragraph)
	(cons (count-screen-lines (window-start) (point))
	      (tlon-babel-count-paragraphs nil (point))))
    (save-selected-window
      (other-window 1)
      (save-restriction
	(widen)
	(save-excursion
	  (tlon-babel-md-beginning-of-buffer-dwim)
	  (markdown-forward-paragraph current-paragraphs)
	  (markdown-backward-paragraph)
	  (recenter 0)
	  (scroll-down current-scren-line))))))

;; TODO: create function
(defun tlon-babel-split-align-sentences ()
  "Align the sentences in the current and other windows.
The alignment is performed by scrolling up or down the other window. It is
assumed that the paragraphs are already aligned."
  )

(defun tlon-babel-split-autoalign-paragraphs ()
  "Automatically align the paragraphs in the current and other windows."
  (when (and tlon-babel-split-mode
	     (tlon-babel-split-screen-line-changed-p)
	     (>= (count-screen-lines (point-min) (point)) tlon-babel-split-screen-line-threshold))
    (tlon-babel-split-align-paragraphs)))

(defun tlon-babel-split-mode-reset ()
  "Reset `split-mode'."
  (interactive)
  (remove-hook 'post-command-hook #'tlon-babel-split-autoalign-paragraphs)
  (add-hook 'post-command-hook #'tlon-babel-split-autoalign-paragraphs))

(tlon-babel-split-mode-reset)

;;;;; Sentence highlighting
;; TODO: the functionality in this section isn’t currently used; decide what to do with it

;; TODO: (1) highlight sentence in target window; (2) diagnose why first
;; two characters in a sentence are matched to the previous sentence;
;; (3) diagnose performance issues, or else disable `post-command-hook'
;; and rely on other triggers; (4) use `lin-blue' as face for highlighting))))

(defvar tlon-babel-sentence-highlight-offset 0
  "Number of sentences to offset the sentence count in the source window.")

(defvar tlon-babel-enable-automatic-highlighting nil
  "Whether to automatically highlight corresponding sentences.")

(defun tlon-babel-sentence-highlight-offset-set ()
  "Set the sentence offset.
This command should be run from the source window."
  (interactive)
  (let ((source-window-sentences (count-sentences (point-min) (point)))
	target-window-sentences)
    (with-selected-window (cadr (window-list))
      (setq target-window-sentences (count-sentences (point-min) (point))))
    (setq tlon-babel-sentence-highlight-offset
	  (- source-window-sentences target-window-sentences))))

(defun tlon-babel-remove-source-overlays ()
  "Remove all existing overlays in the source window."
  (remove-overlays (point-min) (point-max)))

(defun tlon-babel-current-window-line ()
  "Get the current line number in the window."
  (save-excursion
    (let ((end (point)))
      (move-to-window-line 0)
      (count-screen-lines (point) end))))

(defun tlon-babel-highlight-corresponding-sentence ()
  "Highlight the corresponding sentence in the source text and unhighlight others."
  (interactive)
  (let* ((source-window (cadr (window-list)))
	 (target-window (car (window-list)))
	 (target-sentence-index)
	 (overlay (make-overlay (point) (point)))
	 (target-window-line (tlon-babel-current-window-line)))
    (with-selected-window target-window
      (save-excursion
	(backward-sentence)
	(setq target-sentence-index (count-sentences (point-min) (point)))))
    (with-selected-window source-window
      (tlon-babel-remove-source-overlays)
      (let ((beg)
	    (end))
	;; +1 because otherwise `count-sentences' throws an error
	(goto-char (1+ (point-min)))
	(while (< (count-sentences (point-min) (point))
		  (+ target-sentence-index tlon-babel-sentence-highlight-offset))
	  (forward-sentence))
	(setq beg (point))
	(forward-sentence)
	(setq end (point))
	(move-overlay overlay beg end (current-buffer))
	(overlay-put overlay 'face 'highlight)
	(backward-sentence)
	(recenter target-window-line)))))

(defun tlon-babel-toggle-automatic-highlighting ()
  "Toggle automatic highlighting of corresponding sentences."
  (interactive)
  (if tlon-babel-enable-automatic-highlighting
      (progn
	(remove-hook 'post-command-hook 'tlon-babel-highlight-corresponding-sentence t)
	(setq tlon-babel-enable-automatic-highlighting nil)
	(with-selected-window (cadr (window-list))
	  (tlon-babel-remove-source-overlays))
	(message "Automatic sentence highlighting disabled."))
    (add-hook 'post-command-hook 'tlon-babel-highlight-corresponding-sentence nil t)
    (setq tlon-babel-enable-automatic-highlighting t)
    (message "Automatic sentence highlighting enabled.")))

(provide 'tlon-babel-split)
;;; tlon-babel-split.el ends here

