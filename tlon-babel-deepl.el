;;; tlon-babel-deepl.el --- Support for DeepL API calls -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon-babel
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

;; Support for DeepL API calls.

;;; Code:

(require 'tlon-babel-core)

;;;; Variables

(defconst tlon-babel-deepl-key
  (auth-source-pass-get "key" (concat "tlon/babel/deepl.com/" tlon-babel-email-shared))
  "The DeepL API key.")

;;;; Functions

;;;###autoload
(defun tlon-babel-deepl-diff (&optional translation deepl)
  "Run an `ediff' session for a TRANSLATION and the DEEPL translation of it.
If TRANSLATION is nil, use the current buffer. If DEEPL is nil, prompt the user
for a file."
  (interactive)
  (let ((translation (or translation
			 (if-let ((file (buffer-file-name)))
			     file
			   (user-error "Current buffer is not visiting a file"))))
	(deepl (or deepl (read-file-name "DeepL translation: "))))
    (ediff-files deepl translation)))

(provide 'tlon-babel-deepl)
;;; tlon-babel-deepl.el ends here

