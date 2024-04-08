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
  "Add, delete or update a glossary entry."
  (interactive)
  (let* ((english-terms (mapcar (lambda (entry)
                                  (cdr (assoc "en" entry)))
                                tlon-babel-glossary))
         (selected-term (completing-read "Choose or add a term (type to add new): " english-terms nil nil))
         (existing-entry (seq-find (lambda (entry) (equal (cdr (assoc "en" entry)) selected-term))
                                   tlon-babel-glossary))
         (language (tlon-babel-select-language 'two-letter 'babel))
         updated-translation lang-pair)
    ;; If entry doesn't exist, create a new one
    (unless existing-entry
      (setq existing-entry (list (cons "en" selected-term)))
      (setq tlon-babel-glossary (append tlon-babel-glossary (list existing-entry))))
    (setq lang-pair (assoc language existing-entry))
    (setq updated-translation (read-string (format "Translation for \"%s\" (%s) [Empty to remove]: "
                                                   selected-term language)
                                           (cdr lang-pair)))
    ;; If updated translation is empty, remove the language entry if it exists
    (if (string-empty-p updated-translation)
        (when lang-pair
          ;; Remove language pair from the entry
          (setq existing-entry (remove lang-pair existing-entry))
          ;; If only "en" left, ask if the user wants to remove the whole entry
          (when (= (length existing-entry) 1)
            (when (yes-or-no-p "Removed the only translation. Delete the entire entry? ")
              (setq tlon-babel-glossary (remove existing-entry tlon-babel-glossary)))))
      ;; Otherwise, update or add the translation
      (if lang-pair
          (setcdr lang-pair updated-translation)  ; Update existing language pair
        ;; Add new language pair
        (push (cons language updated-translation) existing-entry)))
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
If DEEPL is non-nil, include all entries, format them with the standard DeepL
glossary format, and extract this glossary to the `dict' subdirectory within the
relevant `babel' repository. Otherwise, include only entries of type \"CN\",
format them in a human-readable format, and prompt the user to select the file
location."
  (interactive (list (tlon-babel-select-language 'two-letter 'babel)
		     (y-or-n-p "Extract for DeepL? ")))
  (let* ((file-name "Glossary.csv")
	 (source-path (file-name-concat paths-dir-tlon-repos
					"babel-core/glossary.json"))
	 (target-path (if deepl
			  (file-name-concat paths-dir-tlon-repos
					    (format "babel-%s" language)
					    "dict"
					    file-name)
			(read-file-name "Target glossary destination: "
					paths-dir-downloads nil nil file-name)))
	 json)
    (with-current-buffer (find-file-noselect source-path)
      (goto-char (point-min))
      (let ((json-array-type 'list))
	(setq json (json-read))))
    (with-current-buffer (find-file-noselect target-path)
      (erase-buffer)
      (dolist (item json)
	(let* ((source-term (alist-get 'en item))
	       (target-term (alist-get (intern language) item))
	       (entry (if deepl
			  (format "\"%s\",\"%s\",\"EN\",\"%s\"\n"
				  source-term target-term (upcase language))
			(format "\"%s\",\"%s\"\n"
				source-term target-term))))
	  (when (or deepl (string= (alist-get 'type item) "CN"))
	    (insert entry))))
      (save-buffer))
    (message "Glossary extracted to `%s'" target-path)))

(provide 'tlon-babel-glossary)
;;; tlon-babel-glossary.el ends here

