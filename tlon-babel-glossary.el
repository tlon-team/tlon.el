;;; tlon-babel-glossary.el --- Glossary functions for the Babel project -*- lexical-binding: t -*-

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

;; Glossary functions for the Babel project

;;; Code:

(require 'tlon-babel)

;;;; Functions

(defun tlon-babel-get-file-glossary (&optional language)
  "Return the file containing the glossary for LANGUAGE.
If LANGUAGE is nil, default to the languageuage set in
`tlon-babel-core-translation-language'."
  (let* ((language (or language tlon-babel-core-translation-language))
	 (repo (tlon-babel-core-repo-lookup :dir
					    :subproject "babel"
					    :language language)))
    (file-name-concat repo "dict/Glossary.csv")))

(defun tlon-babel-glossary-alist ()
  "Read `Glossary.csv` and return it as an alist."
  (with-temp-buffer
    (insert-file-contents (tlon-babel-get-file-glossary))
    (let ((lines (split-string (buffer-string) "\n" t))
	  (result '()))
      (dolist (line lines result)
	(let* ((elements (split-string line "\",\""))
	       (key (substring (nth 0 elements) 1)) ; removing leading quote
	       (value (if (string-suffix-p "\"" (nth 1 elements))
			  (substring (nth 1 elements) 0 -1)   ; if trailing quote exists, remove it
			(nth 1 elements)))) ; otherwise, use as-is
	  (push (cons key value) result))))))

;;;###autoload
(defun tlon-babel-glossary-dwim ()
  "Add a new entry to the glossary or modify an existing entry."
  (interactive)
  (let* ((terms (mapcar 'car (tlon-babel-glossary-alist)))
	 (term (completing-read "Term: " terms)))
    (if (member term terms)
	(tlon-babel-glossary-modify term)
      (tlon-babel-glossary-add term))))

;;;###autoload
(defun tlon-babel-glossary-add (&optional original translation)
  "Add a new entry to the glossary for ORIGINAL and TRANSLATION terms."
  (interactive)
  (let ((original (or original (read-string "original term: "))))
    (cl-destructuring-bind (translation explanation) (tlon-babel-glossary-prompt translation)
      (with-current-buffer (find-file-noselect (tlon-babel-get-file-glossary translation))
	(goto-char (point-max))
	(insert (tlon-babel-glossary-regexp-pattern original translation))
	(tlon-babel-glossary-finalize "add" original explanation)))))

(defun tlon-babel-glossary-modify (original)
  "Modify an entry in the glossary corresponding to the ORIGINAL term."
  (let* ((existing-translation (cdr (assoc original (tlon-babel-glossary-alist)))))
    (cl-destructuring-bind (new-translation explanation) (tlon-babel-glossary-prompt)
      (with-current-buffer (find-file-noselect (tlon-babel-get-file-glossary))
	(goto-char (point-min))
	(while (re-search-forward (tlon-babel-glossary-regexp-pattern original existing-translation) nil t)
	  (replace-match (tlon-babel-glossary-regexp-pattern original new-translation)))
	(tlon-babel-glossary-finalize "modify" original explanation)
	(message "Remember to run a `ripgrep' search for the original translation (\"%s\") across all the Babel repos in the translation language (%s), making any necessary replacements."
		 existing-translation tlon-babel-core-translation-language)))))

(defun tlon-babel-glossary-prompt (&optional translation)
  "Prompt the user for a translation and and an explanation.
If TRANSLATION is non-nil, prompt for an explanation only."
  (let ((translation (or translation (read-string
				      (format "translation term [%s]: "
					      tlon-babel-core-translation-language))))
	(explanation (read-string (format
				   "Explanation (optional; please write it in the translation language [%s]): "
				   tlon-babel-core-translation-language))))
    (list translation explanation)))

(defun tlon-babel-glossary-regexp-pattern (original translation)
  "Get the regexp pattern for glossary entry.
The glossary entry is that corresponding to ORIGINAL and TRANSLATION."
  (format "\"%s\",\"%s\",\"EN\",\"%s\"" original translation
	  (upcase tlon-babel-core-translation-language)))

(defun tlon-babel-glossary-finalize (action original explanation)
  "Finalize the addition of a word to the glossary or its modification.
ACTION is either \"add\" or \"modify\". ORIGINAL is the term in the original
language. EXPLANATION is the explanation of the translation."
  (goto-char (point-min))
  (flush-lines "^$")
  (save-buffer)
  (tlon-babel-glossary-commit action original explanation))

;; TODO: fix this
(defun tlon-babel-glossary-commit (action term &optional explanation)
  "Commit glossary modifications.
ACTION describes the action (\"add\" or \"modify\") performed on the glossary.
TERM refers to the English glossary term to which this action was performed.
These two variables are used to construct a commit message of the form
\='Glossary: ACTION \"TERM\"\=', such as \='Glossary: add \"repugnant
conclusion\"\='. Optionally, EXPLANATION provides an explanation of the change."
  (let ((default-directory (tlon-babel-core-repo-lookup :dir :name "babel-es"))
	(explanation (if explanation (concat "\n\n" explanation) "")))
    ;; save all unsaved files in repo
    (magit-save-repository-buffers)
    (call-interactively #'magit-pull-from-upstream nil)
    ;; if there are staged files, we do not commit or push the changes
    (unless (magit-staged-files)
      (tlon-babel-check-branch "main" default-directory)
      (magit-run-git "add" (tlon-babel-get-file-glossary))
      (let ((magit-commit-ask-to-stage nil))
	(magit-commit-create (list "-m" (format  "Glossary: %s \"%s\"%s"
						 action term explanation))))))
  (call-interactively #'magit-push-current-to-pushremote))

(provide 'tlon-babel-glossary)
;;; tlon-babel-glossary.el ends here

