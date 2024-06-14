;;; tlon-deepl.el --- Support for DeepL API calls -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon
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

(require 'tlon-core)
(require 'url)

;;;; Variables

(defconst tlon-deepl-key
  (auth-source-pass-get "key" (concat "tlon/babel/deepl.com/" (getenv "WORK_EMAIL")))
  "The DeepL API key.")

;;;; Functions

;;;###autoload
(defun tlon-deepl-diff (&optional translation deepl)
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

(defun tlon-create-deepl-glossary (glossary-name source-lang target-lang entries)
  "Create a glossary in DeepL with specified parameters.
GLOSSARY-NAME is the name of the glossary. SOURCE-LANG and TARGET-LANG are the
source (e.g., \"EN\") and target (e.g., \"ES\") languages, respectively. ENTRIES
are the glossary entries in TSV format (e.g., \"Hello\tHola\")."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "DeepL-Auth-Key " tlon-deepl-key))
           ("User-Agent" . "YourApp/1.2.3")))
        (url-request-data
         (json-encode `(("name" . ,glossary-name)
                        ("source_lang" . ,source-lang)
                        ("target_lang" . ,target-lang)
                        ("entries" . ,entries)
                        ("entries_format" . "tsv")))))
    (url-retrieve "https://api.deepl.com/v2/glossaries"
                  (lambda (_)
                    (goto-char (point-min))
                    (when (search-forward-regexp "^HTTP/.* \\([200-299]+\\) " nil t)
                      (if (string= "200" (match-string 1))
                          (message "Glossary created successfully!")
                        (message "Failed to create glossary: %s" (match-string 1))))
                    (kill-buffer (current-buffer)))
                  nil t)))

(provide 'tlon-deepl)
;;; tlon-deepl.el ends here

