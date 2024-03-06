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

