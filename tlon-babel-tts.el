;;; tlon-babel-tts.el --- Text-to-speech functionality -*- lexical-binding: t -*-

;; Copyright (C) 2024

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

;; Text-to-speech functionality.

;;; Code:

(require 'read-aloud)
(require 'tlon-babel)

;;;; Functions

(define-minor-mode tlon-babel-tts-mode
  "Enable TTS mode locally."
  :global nil
  :init-value nil)

(defvar tlon-babel-tts-mode-map (make-sparse-keymap)
  "Keymap for `tlon-babel-tts-mode'.")

(define-key tlon-babel-tts-mode-map (kbd "H-,") 'tlon-babel-tts-read-backward)
(define-key tlon-babel-tts-mode-map (kbd "H-.") 'tlon-babel-tts-read-forward)
(define-key tlon-babel-tts-mode-map (kbd "H-;") 'tlon-babel-tts-read-target-start-or-stop)

(defun tlon-babel-tts-get-target-buffer ()
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

(defun tlon-babel-tts-read-target-start-or-stop ()
  "Start or stop reading the target buffer."
  (interactive)
  (let ((buffer (tlon-babel-tts-get-target-buffer))
	(current-buffer (current-buffer)))
    (pop-to-buffer buffer)
    (when read-aloud--c-bufpos
      (goto-char read-aloud--c-bufpos))
    (read-aloud-buf)
    ;; we move point to the previous chunk, using the chunk divider
    ;; defined in `read-aloud--grab-text'
    (re-search-backward "[,.:!;]\\|\\(-\\|\n\\|\r\n\\)\\{2,\\}" nil t)
    (pop-to-buffer current-buffer)))

(defun tlon-babel-tts-read-backward-or-forward (direction)
  "Move in DIRECTION in the target buffer."
  (interactive)
  (let ((buffer (tlon-babel-tts-get-target-buffer))
	(current-buffer (current-buffer))
	(fun (if (eq direction 'backward)
		 're-search-backward
	       're-search-forward)))
    (when read-aloud--c-bufpos
      (read-aloud-buf))
    (pop-to-buffer buffer)
    (funcall fun "[,.:!;]\\|\\(-\\|\n\\|\r\n\\)\\{2,\\}" nil t 1)
    (pop-to-buffer current-buffer)))

(defun tlon-babel-tts-read-backward ()
  "Move backward in the target buffer."
  (interactive)
  (tlon-babel-tts-read-backward-or-forward 'backward))

(defun tlon-babel-tts-read-forward ()
  "Move forward in the target buffer."
  (interactive)
  (tlon-babel-tts-read-backward-or-forward 'forward))

(provide 'tlon-babel-tts)
;;; tlon-babel-tts.el ends here

