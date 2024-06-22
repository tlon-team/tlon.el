;;; tlon-dict.el --- Dictionary-related functions -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon
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

;; Dictionary-related functions.

;;; Code:

(require 'reverso)
(require 'tlon-core)

;;;; Functions

;;;###autoload
(defun tlon-reverso-get-synonyms ()
  "Advice to enforce that language is always the current language."
  (interactive)
  (let ((language (tlon-lookup tlon-languages-properties :name :code (tlon-get-language)))
	(text (read-string "Word:" (thing-at-point 'word))))
    (reverso--get-synonyms text (intern language)
			   (lambda (data)
			     (reverso--with-buffer
			      (reverso--synonyms-render text data)
			      (setq-local reverso--data data))))))

;;;;; Menu

;;;###autoload (autoload 'tlon-dict-menu "tlon-dict" nil t)
(transient-define-prefix tlon-dict-menu ()
  "`tlon-dict' menu."
  [
   ("s" "Synonyms" tlon-reverso-get-synonyms)
   ]
  )
(provide 'tlon-dict)
;;; tlon-dict.el ends here

