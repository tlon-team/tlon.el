;;; tlon-read.el --- Functions for reading the current buffer aloud -*- lexical-binding: t -*-

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

;; Functions for reading the current buffer aloud.

;;; Code:

(require 'read-aloud)
(require 'tlon)

;;;; Functions

(defvar tlon-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "H-,") 'tlon-read-backward)
    (define-key map (kbd "H-.") 'tlon-read-forward)
    (define-key map (kbd "H-;") 'tlon-read-target-start-or-stop)
    (define-key map (kbd "H-k") 'read-aloud-extras-increase-rate)
    (define-key map (kbd "H-l") 'read-aloud-extras-decrease-rate)
    map)
  "Keymap for `tlon-read-mode'.")

(define-minor-mode tlon-read-mode
  "Enable TTS mode locally."
  :global nil
  :init-value nil
  :keymap tlon-read-mode-map)

(defun tlon-get-target-buffer ()
  "Return the buffer that `read-aloud' should read."
  (let ((buffer-list (cl-remove-if-not
		      (lambda (buffer)
			(string-match-p (regexp-quote "**markdown-output* # eww*") (buffer-name buffer)))
		      (buffer-list)))
	buffer)
    (cond ((= (length buffer-list) 1)
	   (setq buffer (car buffer-list)))
	  ((< (length buffer-list) 1)
	   (user-error "No buffer found"))
	  ((> (length buffer-list) 1)
	   (user-error "More than one buffer found")))
    buffer))

(defun tlon-read-target-start-or-stop ()
  "Start or stop reading the target buffer."
  (interactive)
  (let ((buffer (tlon-get-target-buffer))
	(current-buffer (current-buffer)))
    (pop-to-buffer buffer)
    (when read-aloud--c-bufpos
      (goto-char read-aloud--c-bufpos))
    (read-aloud-buf)
    ;; we move point to the previous chunk, using the chunk divider
    ;; defined in `read-aloud--grab-text'
    (re-search-backward "[,.:!;]\\|\\(-\\|\n\\|\r\n\\)\\{2,\\}" nil t)
    (pop-to-buffer current-buffer)))

(defun tlon-read-backward-or-forward (direction)
  "Move in DIRECTION in the target buffer."
  (interactive)
  (let ((buffer (tlon-get-target-buffer))
	(current-buffer (current-buffer))
	(fun (if (eq direction 'backward)
		 're-search-backward
	       're-search-forward)))
    (when read-aloud--c-bufpos
      (read-aloud-buf))
    (pop-to-buffer buffer)
    (funcall fun "[,.:!;]\\|\\(-\\|\n\\|\r\n\\)\\{2,\\}" nil t 1)
    (pop-to-buffer current-buffer)))

(defun tlon-read-backward ()
  "Move backward in the target buffer."
  (interactive)
  (tlon-read-backward-or-forward 'backward))

(defun tlon-read-forward ()
  "Move forward in the target buffer."
  (interactive)
  (tlon-read-backward-or-forward 'forward))

(provide 'tlon-read)
;;; tlon-read.el ends here

