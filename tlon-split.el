;;; tlon-split.el --- Minor mode for working with split windows -*- lexical-binding: t -*-

;; Copyright (C) 2024

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

(defvar-local tlon-split-last-screen-line-pos nil
  "Most recent point position.")

(defvar tlon-split-screen-line-threshold 60
  "Number of lines below which `tlon-split-autoalign-paragraphs' is disabled.")

;;;; Functions

;;;###autoload (autoload 'tlon-split-mode "tlon-split" nil t)
(define-minor-mode tlon-split-mode
  "Enable `tlon-split-mode' locally."
  :init-value nil
  (if tlon-split-mode
      (add-hook 'post-command-hook #'tlon-split-autoalign-paragraphs nil t)
    (remove-hook 'post-command-hook #'tlon-split-autoalign-paragraphs t)))

(defun tlon-split-screen-line-changed-p ()
  "Return t iff the cursor in on a different screen line."
  (let* ((current-screen-line (count-screen-lines (window-start) (point)))
	 (moved-p (not (eq tlon-split-last-screen-line-pos current-screen-line))))
    (setq tlon-split-last-screen-line-pos current-screen-line)
    moved-p))

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
  (remove-hook 'post-command-hook #'tlon-split-autoalign-paragraphs t)
  (add-hook 'post-command-hook #'tlon-split-autoalign-paragraphs nil t))

;;;; menu

;;;###autoload (autoload 'tlon-split-menu "tlon-split" nil t)
(transient-define-prefix tlon-split-menu ()
  "`split’ menu."
  [["Split mode"
    ("s" "toggle"                     tlon-split-mode)
    ("r" "reset"                      tlon-split-mode-reset)]])

(provide 'tlon-split)
;;; tlon-split.el ends here

