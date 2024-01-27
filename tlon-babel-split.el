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

;; Minor mode for working with split windows

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
  :init-value nil
  (if tlon-babel-split-mode
      (remove-hook 'post-command-hook #'tlon-babel-split-autoalign-paragraphs)
    (add-hook 'post-command-hook #'tlon-babel-split-autoalign-paragraphs nil 'local)))

(defun tlon-babel-split-screen-line-changed-p ()
  "Return t iff the cursor in on a different screen line."
  (let* ((current-screen-line (count-screen-lines (window-start) (point)))
	 (moved-p (not (eq tlon-babel-split-last-screen-line-pos current-screen-line))))
    (setq tlon-babel-split-last-screen-line-pos current-screen-line)
    moved-p))

(defun tlon-babel-split-top-of-buffer-visible-p ()
  "Return t iff the top of the buffer is visible in the current window.
The function considers the top of the buffer visible if the first line of the
buffer, or the first line after the metadata if any, is visible in the current
window."
  (save-restriction
    (widen)
    (save-excursion
      (let ((first-visible-line (progn
				  (forward-line (* -1 (count-screen-lines (window-start) (point))))
				  (line-number-at-pos)))
	    (first-line (progn
			  (goto-char (point-max))
			  (tlon-babel-md-beginning-of-buffer-dwim))))
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
	      (tlon-babel-counterpart-count-paragraphs nil (point))))
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

(defun tlon-babel-split-autoalign-paragraphs ()
  "Automatically align the paragraphs in the current and other windows."
  (when (and tlon-babel-split-mode
	     (tlon-babel-split-screen-line-changed-p))
    (tlon-babel-split-align-paragraphs)))

(provide 'tlon-babel-split)
;;; tlon-babel-split.el ends here

