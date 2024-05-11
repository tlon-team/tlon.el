;;; tlon-glossary.el --- Glossary functions for the Babel project -*- lexical-binding: t -*-

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

;; Glossary functions for the Babel project

;;; Code:

(require 'tlon)

;;;; Variables

(defconst tlon-file-glossary-source
  (file-name-concat (tlon-repo-lookup :dir :name "babel-core") "glossary.json")
  "The JSON file containing the source glossary.")

(defconst tlon-file-glossary-target
  (file-name-concat paths-dir-downloads "Glossary.csv")
  "The CSV file containing the target glossary.")

(defvar tlon-glossary-values
  (tlon-parse-json tlon-file-glossary-source)
  "The glossary values.")

(defconst tlon-glossary-email-subject
  "New version of the glossary"
  "The subject of the email to send to translators when sharing the glossary.")

(defconst tlon-glossary-email-body
  (format "Dear translators,\n\nAttached is a new version of the glossary. Please upload it to DeepL.\n\nBest regards,\n\n%s" (tlon-user-lookup :nickname :name user-full-name))
  "The body of the email to send to translators when sharing the glossary.")

(defconst tlon-glossary-email-recipients
  '(("fr" . "tlon-french@googlegroups.com")
    ("it" . "tlon-italian@googlegroups.com"))
  "Association list of languages and Google Group addresses.")

;;;; Functions

;;;###autoload
(defun tlon-edit-glossary ()
  "Create or update a glossary entry."
  (interactive)
  (let* ((glossary (tlon-parse-glossary))
         (english-terms (tlon-get-english-terms glossary))
         (selected-term (completing-read "Choose or add a term: " english-terms nil nil))
         (existing-entry (tlon-find-entry-by-term glossary selected-term)))
    (unless existing-entry
      (let ((type (tlon-select-term-type)))
        (setq existing-entry (tlon-create-entry selected-term type))))
    (unless (and (assoc "type" existing-entry)
		 (string= (cdr (assoc "type" existing-entry)) "invariant"))
      (setq existing-entry (tlon-edit-translation-in-entry existing-entry selected-term)))
    (setq glossary (tlon-update-glossary glossary existing-entry selected-term))
    (tlon-write-data tlon-file-glossary-source glossary)))

(defun tlon-parse-glossary ()
  "Parse the glossary file into Lisp."
  (tlon-parse-json tlon-file-glossary-source))

(defun tlon-get-english-terms (glossary)
  "Extract all English terms from GLOSSARY."
  (mapcar (lambda (entry)
            (cdr (assoc "en" entry)))
          glossary))

(defun tlon-find-entry-by-term (glossary term)
  "Find an entry in GLOSSARY by its English TERM."
  (seq-find (lambda (entry) (string= (cdr (assoc "en" entry)) term))
            glossary))

(defun tlon-select-term-type ()
  "Prompt the user to select a type for a new term and return the selection."
  (completing-read "Select type (variable/invariant): " '("variable" "invariant") nil t))

(defun tlon-create-entry (term type)
  "Create a new entry for a TERM of a given TYPE."
  (let ((entry (list (cons "en" term) (cons "type" type))))
    (when (string= type "invariant")
      (let ((languages (remove "en" (mapcar 'cdr tlon-project-languages))))
        (dolist (lang languages)
          (setq entry (append entry (list (cons lang term)))))))
    entry))

(defun tlon-edit-translation-in-entry (entry term)
  "Create or update a translation in an ENTRY for a TERM."
  (let* ((language (tlon-select-language 'code 'babel))
         (translation (assoc language entry))
	 (initial-input (when translation (cdr translation)))
	 (read-translation (lambda ()
			     (read-string (format "Translation for \"%s\" (%s): " term language) initial-input))))
    (if translation
	(setcdr translation (funcall read-translation))
      (setq entry (append entry (list (cons language (funcall read-translation))))))
    entry))

(defun tlon-update-glossary (glossary entry term)
  "Update GLOSSARY with a new or modified ENTRY for a TERM."
  (if (tlon-find-entry-by-term glossary term)
      ;; Update existing entry
      (setf (car (seq-filter (lambda (e) (equal (assoc "en" e) (cons "en" term))) glossary)) entry)
    ;; Append new entry
    (setq glossary (append glossary (list entry))))
  glossary)

;; TODO: this is currently not used; fix it
(defun tlon-glossary-prompt-for-explanation ()
  "Prompt the user for an explanation of the translation."
  (read-string (format
		"Explanation (optional; please write it in the translation language [%s]): "
		tlon-translation-language)))

;; TODO: this is currently not used; fix it
(defun tlon-glossary-commit (action term &optional explanation)
  "Commit glossary modifications.
ACTION describes the action (\"add\" or \"modify\") performed on the glossary.
TERM refers to the English glossary term to which this action was performed.
These two variables are used to construct a commit message of the form
\='Glossary: ACTION \"TERM\"\=', such as \='Glossary: add \"repugnant
conclusion\"\='. Optionally, EXPLANATION provides an explanation of the change."
  (let ((default-directory (tlon-repo-lookup :dir :name "babel-es"))
	(explanation (if explanation (concat "\n\n" explanation) "")))
    ;; save all unsaved files in repo
    (magit-save-repository-buffers)
    (call-interactively #'magit-pull-from-upstream nil)
    ;; if there are staged files, we do not commit or push the changes
    (unless (magit-staged-files)
      (tlon-check-branch "main" default-directory)
      (magit-run-git "add" tlon-file-glossary-source)
      (let ((magit-commit-ask-to-stage nil))
	(magit-commit-create (list "-m" (format  "Glossary: %s \"%s\"%s"
						 action term explanation))))))
  (call-interactively #'magit-push-current-to-pushremote))

;;;###autoload
(defun tlon-extract-glossary (language deepl)
  "Extract a LANGUAGE glossary from our multilingual glossary.
If DEEPL is non-nil, include all entries and format them with the standard DeepL
glossary format. Otherwise, include only entries of type \"variable\",
and format them in a human-readable format."
  (interactive (list (tlon-select-language 'code 'babel)
		     (y-or-n-p "Extract for DeepL? ")))
  (let ((source-path tlon-file-glossary-source)
	json)
    (with-current-buffer (find-file-noselect source-path)
      (goto-char (point-min))
      (let ((json-array-type 'list))
	(setq json (json-read))))
    (with-current-buffer (find-file-noselect tlon-file-glossary-target)
      (erase-buffer)
      (tlon-insert-formatted-glossary json language deepl)
      (save-buffer))
    (if (and deepl (y-or-n-p "Share glossary with translators? "))
	(tlon-share-glossary tlon-file-glossary-target language)
      (message "Glossary extracted to `%s'" tlon-file-glossary-target))))

(defun tlon-insert-formatted-glossary (json language deepl)
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

;;;;; Share glossary

(defun tlon-share-glossary (attachment &optional language)
  "Share glossary as ATTACHMENT with translators in selected LANGUAGE."
  (interactive (list (read-file-name "Attachment: "
				     (file-name-directory tlon-file-glossary-target) nil nil
				     (file-name-nondirectory tlon-file-glossary-target))))
  (let ((language (or language (tlon-select-language 'code 'babel)))
	(user-mail-address (concat (downcase (tlon-user-lookup :nickname :name user-full-name))
				   "@tlon.team"))
	(mailbuf (generate-new-buffer "*mail*")))
    (with-current-buffer mailbuf
      (message-mail (alist-get language tlon-glossary-email-recipients nil nil #'string=)
		    tlon-glossary-email-subject)
      (message-goto-body)
      (insert tlon-glossary-email-body)
      (mml-attach-file attachment)
      (message-send-and-exit))
    (kill-buffer mailbuf)))

(provide 'tlon-glossary)
;;; tlon-glossary.el ends here

