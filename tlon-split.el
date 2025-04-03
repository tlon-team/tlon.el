;;; tlon-split.el --- Minor mode for working with split windows -*- lexical-binding: t -*-

;; Copyright (C) 22024

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

;; Minor mode for working with split windows.

;;; Code:

(require 'tlon-counterpart)
(require 'tlon-md)

;;;; Main variables

(defvar tlon-split-last-screen-line-pos nil
  "Most recent point position.")

(defvar tlon-split-screen-line-threshold 60
  "Number of lines below which `tlon-split-autoalign-paragraphs' is disabled.")

;;;; Functions

;;;###autoload (autoload 'tlon-split-mode "tlon-split" nil t)
(define-minor-mode tlon-split-mode
  "Enable `tlon-split-mode' locally."
  :init-value nil)

(defun tlon-split-screen-line-changed-p ()
  "Return t iff the cursor in on a different screen line."
  (let* ((current-screen-line (count-screen-lines (window-start) (point)))
	 (moved-p (not (eq tlon-split-last-screen-line-pos current-screen-line))))
    (setq tlon-split-last-screen-line-pos current-screen-line)
    moved-p))

;; Currently this function is not used; we use
;; `tlon-split-screen-line-threshold' instead, as an imperfect approximatiion.
(defun tlon-split-top-of-buffer-visible-p ()
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
	       (tlon-md-beginning-of-buffer-dwim)
	       (count-screen-lines (window-start) (point)))))
	(<= first-visible-line first-line)))))

(defun tlon-split-screen-line-offset ()
  "Return the difference between the screen lines in the current and other windows."
  (let* ((current-screen-line (count-screen-lines (window-start) (point)))
         (other-screen-line (save-selected-window
                              (other-window 1)
                              (count-screen-lines (window-start) (point)))))
    (- other-screen-line current-screen-line)))

(defun tlon-split-align-screen-lines ()
  "Align the screen lines in the current and other windows.
The alignment is performed by scrolling up or down the other window."
  (let* ((offset (tlon-split-screen-line-offset)))
    (other-window 1)
    (unless (= offset 0)
      (scroll-up offset))
    (other-window -1)))

(defun tlon-split-align-paragraphs ()
  "Align the paragraphs in the current and other windows.
The alignment is performed by scrolling up or down the other window."
  (cl-destructuring-bind
      (current-scren-line . current-paragraphs)
      (save-excursion
	(goto-char (1+ (point)))
	(markdown-backward-paragraph)
	(cons (count-screen-lines (window-start) (point))
	      (tlon-get-number-of-paragraphs nil (point))))
    (save-selected-window
      (other-window 1)
      (save-restriction
	(widen)
	(save-excursion
	  (tlon-md-beginning-of-buffer-dwim)
	  (markdown-forward-paragraph current-paragraphs)
	  (markdown-backward-paragraph)
	  (recenter 0)
	  (scroll-down current-scren-line))))))

;; TODO: create function
(defun tlon-split-align-sentences ()
  "Align the sentences in the current and other windows.
The alignment is performed by scrolling up or down the other window. It is
assumed that the paragraphs are already aligned."
  )

(defun tlon-split-autoalign-paragraphs ()
  "Automatically align the paragraphs in the current and other windows."
  (when (and tlon-split-mode
	     (tlon-split-screen-line-changed-p)
	     (>= (count-screen-lines (point-min) (point)) tlon-split-screen-line-threshold))
    (tlon-split-align-paragraphs)))

;;;###autoload
(defun tlon-split-mode-reset ()
  "Reset `split-mode'."
  (interactive)
  (remove-hook 'post-command-hook #'tlon-split-autoalign-paragraphs)
  (add-hook 'post-command-hook #'tlon-split-autoalign-paragraphs))

(tlon-split-mode-reset)

;;;;; Sentence highlighting
;; TODO: the functionality in this section isn’t currently used; decide what to do with it

;; TODO: (1) highlight sentence in target window; (2) diagnose why first
;; two characters in a sentence are matched to the previous sentence;
;; (3) diagnose performance issues, or else disable `post-command-hook'
;; and rely on other triggers; (4) use `lin-blue' as face for highlighting))))

(defvar tlon-sentence-highlight-offset 0
  "Number of sentences to offset the sentence count in the source window.")

(defvar tlon-enable-automatic-highlighting nil
  "Whether to automatically highlight corresponding sentences.")

;;;###autoload
(defun tlon-sentence-highlight-offset-set ()
  "Set the sentence offset.
This command should be run from the source window."
  (interactive)
  (let ((source-window-sentences (count-sentences (point-min) (point)))
	target-window-sentences)
    (with-selected-window (cadr (window-list))
      (setq target-window-sentences (count-sentences (point-min) (point))))
    (setq tlon-sentence-highlight-offset
	  (- source-window-sentences target-window-sentences))))

(defun tlon-remove-source-overlays ()
  "Remove all existing overlays in the source window."
  (remove-overlays (point-min) (point-max)))

(defun tlon-current-window-line ()
  "Get the current line number in the window."
  (save-excursion
    (let ((end (point)))
      (move-to-window-line 0)
      (count-screen-lines (point) end))))

;;;###autoload
(defun tlon-highlight-corresponding-sentence ()
  "Highlight the corresponding sentence in the source text and unhighlight others."
  (interactive)
  (let* ((source-window (cadr (window-list)))
	 (target-window (car (window-list)))
	 (target-sentence-index)
	 (overlay (make-overlay (point) (point)))
	 (target-window-line (tlon-current-window-line)))
    (with-selected-window target-window
      (save-excursion
	(backward-sentence)
	(setq target-sentence-index (count-sentences (point-min) (point)))))
    (with-selected-window source-window
      (tlon-remove-source-overlays)
      (let ((beg)
	    (end))
	;; +1 because otherwise `count-sentences' throws an error
	(goto-char (1+ (point-min)))
	(while (< (count-sentences (point-min) (point))
		  (+ target-sentence-index tlon-sentence-highlight-offset))
	  (forward-sentence))
	(setq beg (point))
	(forward-sentence)
	(setq end (point))
	(move-overlay overlay beg end (current-buffer))
	(overlay-put overlay 'face 'highlight)
	(backward-sentence)
	(recenter target-window-line)))))

(defun tlon-toggle-automatic-highlighting ()
  "Toggle automatic highlighting of corresponding sentences."
  (interactive)
  (if tlon-enable-automatic-highlighting
      (progn
	(remove-hook 'post-command-hook 'tlon-highlight-corresponding-sentence t)
	(setq tlon-enable-automatic-highlighting nil)
	(with-selected-window (cadr (window-list))
	  (tlon-remove-source-overlays))
	(message "Automatic sentence highlighting disabled."))
    (add-hook 'post-command-hook 'tlon-highlight-corresponding-sentence nil t)
    (setq tlon-enable-automatic-highlighting t)
    (message "Automatic sentence highlighting enabled.")))

;;;; menu

;;;###autoload (autoload 'tlon-split-menu "tlon-split" nil t)
(transient-define-prefix tlon-split-menu ()
  "`split' menu."
  [["Split mode"
    ("s" "toggle"                     tlon-split-mode)
    ("h" "highlight"                  tlon-highlight-corresponding-sentence)
    ("o" "set offset"                 tlon-sentence-highlight-offset-set)
    ("r" "reset"                      tlon-split-mode-reset)]])

(provide 'tlon-split)
;;; tlon-split.el ends here

