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

;;;; Variables

(defconst tlon-babel-file-glossary
  (file-name-concat (tlon-babel-repo-lookup :dir :name "babel-core") "glossary.json")
  "The JSON file containing the glossary.")

(defvar tlon-babel-glossary
  (tlon-babel-parse-json tlon-babel-file-glossary)
  "The glossary values.")

;;;; Functions

;;;###autoload
(defun tlon-babel-edit-glossary ()
  "Create or update a glossary entry."
  (interactive)
  (setq tlon-babel-glossary (tlon-babel-parse-json tlon-babel-file-glossary))
  (let* ((english-terms (mapcar (lambda (entry)
                                  (cdr (assoc "en" entry)))
                                tlon-babel-glossary))
         (selected-term (completing-read "Choose or add a term (type to add new): " english-terms nil nil))
         (existing-entry (seq-find (lambda (entry) (string= (cdr (assoc "en" entry)) selected-term))
                                   tlon-babel-glossary)))
    ;; If entry doesn't exist, create a new one.
    (unless existing-entry
      (setq existing-entry (list (cons "en" selected-term)))
      ;; Prompt for selecting type only when adding a new entry.
      (let ((type (completing-read "Select type (variable/invariant): " '("variable" "invariant") nil t)))
        (setq existing-entry (append existing-entry (list (cons "type" type))))
        (when (equal type "invariant")
          ;; Add the chosen term as the value for each of the glossary languages
          (let ((languages (remove "en" (mapcar 'cdr tlon-babel-languages))))
	    (dolist (lang languages)
	      (setq existing-entry (append existing-entry (list (cons lang selected-term)))))))))
    ;; Update or add the translation if type is not invariant
    (let* ((type (assoc "type" existing-entry)))
      (unless (and type (string= (cdr type) "invariant"))
        (let* ((language (tlon-babel-select-language 'two-letter 'babel))
	       (translation (assoc language existing-entry)))
          (if translation
              (setcdr translation (read-string (format "Translation for \"%s\" (%s): " selected-term language) (cdr translation)))
	    (setq existing-entry (append existing-entry (list (cons language (read-string (format "Translation for \"%s\" (%s): " selected-term language))))))))))
    ;; Update glossary by appending at the end
    (if (seq-find (lambda (entry) (equal (assoc "en" entry) (cons "en" selected-term))) tlon-babel-glossary)
	;; Update the existing entry
	(setf (car (seq-filter (lambda (entry) (equal (assoc "en" entry) (cons "en" selected-term))) tlon-babel-glossary)) existing-entry)
      ;; Append new entry at the end
      (setq tlon-babel-glossary (append tlon-babel-glossary (list existing-entry))))
    (tlon-babel-write-data tlon-babel-file-glossary tlon-babel-glossary)))

(defun tlon-babel-glossary-prompt-for-explanation ()
  "Prompt the user for an explanation of the translation."
  (read-string (format
		"Explanation (optional; please write it in the translation language [%s]): "
		tlon-babel-translation-language)))

;; TODO: fix this
(defun tlon-babel-glossary-commit (action term &optional explanation)
  "Commit glossary modifications.
ACTION describes the action (\"add\" or \"modify\") performed on the glossary.
TERM refers to the English glossary term to which this action was performed.
These two variables are used to construct a commit message of the form
\='Glossary: ACTION \"TERM\"\=', such as \='Glossary: add \"repugnant
conclusion\"\='. Optionally, EXPLANATION provides an explanation of the change."
  (let ((default-directory (tlon-babel-repo-lookup :dir :name "babel-es"))
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

;;;###autoload
(defun tlon-babel-extract-glossary (language deepl)
  "Extract a LANGUAGE glossary from our multilingual glossary.
If DEEPL is non-nil, include all entries and format them with the standard DeepL
glossary format. Otherwise, include only entries of type \"variable\",
and format them in a human-readable format."
  (interactive (list (tlon-babel-select-language 'two-letter 'babel)
		     (y-or-n-p "Extract for DeepL? ")))
  (let* ((file-name "Glossary.csv")
	 (source-path tlon-babel-file-glossary)
	 (target-path (read-file-name "Target glossary destination: "
				      paths-dir-downloads nil nil file-name))
	 json)
    (with-current-buffer (find-file-noselect source-path)
      (goto-char (point-min))
      (let ((json-array-type 'list))
	(setq json (json-read))))
    (with-current-buffer (find-file-noselect target-path)
      (erase-buffer)
      (tlon-babel-insert-formatted-glossary json language deepl)
      (save-buffer))
    (message "Glossary extracted to `%s'" target-path)))

(defun tlon-babel-insert-formatted-glossary (json language deepl)
  "Insert a properly formatted glossary in LANGUAGE from JSON data.
Format the glossary for a human reader, unless DEEPL is non-nil, in which case
format it for DeepL. When formatting it for a human reader, exclude invariant
terms (these are terms included for the sole purpose of forcing DeepL to leave
them untranslated)."
  (dolist (item json)
    (when-let* ((source-term (alist-get 'en item))
		(target-term (alist-get (intern language) item))
		(entry (if deepl
			   (format "\"%s\",\"%s\",\"EN\",\"%s\"\n"
				   source-term target-term (upcase language))
			 (format "\"%s\",\"%s\"\n"
				 source-term target-term))))
      (when (or deepl (string= (alist-get 'type item) "variable"))
	(insert entry)))))

(provide 'tlon-babel-glossary)
;;; tlon-babel-glossary.el ends here

