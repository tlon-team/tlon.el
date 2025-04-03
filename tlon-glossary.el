;;; tlon-glossary.el --- Glossary functions for Tlön -*- lexical-binding: t -*-

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

;; Glossary functions for Tlön

;;; Code:

(require 'tlon)
(require 'transient)

;;;; Variables

(defconst tlon-file-glossary-source
  (file-name-concat (tlon-repo-lookup :dir :name "babel-core") "glossary.json")
  "The JSON file containing the source glossary.")

(defconst tlon-glossary-recipients
  '((:language "fr" :email "tlon-french@googlegroups.com")
    (:language "it" :email "tlon-italian@googlegroups.com")))

;;;; Functions

;;;;; Editing

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
  "Parse the glossary file into Emacs Lisp."
  (tlon-read-json tlon-file-glossary-source))

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
      (dolist (lang tlon-project-target-languages)
        (setq entry (append entry (list (cons lang term))))))
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
(defvar magit-commit-ask-to-stage)
(declare-function magit-save-repository-buffers "magit-mode")
(declare-function magit-pull-from-upstream "magit-pull")
(declare-function magit-push-current-to-pushremote "magit-push")
(declare-function magit-staged-files "magit-git")
(declare-function magit-run-git "magit-process")
(declare-function magit-commit-create "magit-commit")
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

;;;;; Extraction

;;;###autoload
(defun tlon-extract-glossary (language recipient)
  "Extract a LANGUAGE glossary from our multilingual glossary.
RECIPIENT can be `human', `deepl-editor' and `deepl-api'.

- `human': extracts a glossary intended to be shared with another human being.
 Includes only entries of type \"variable\", saves them to a \"csv\" file, and
 prompts the user to share the glossary with translators.

- `deepl-editor': extracts a glossary intended to be uploaded to the DeepL
  editor. Includes all entries and saves them to a \"csv\" file.

- `deepl-api': extracts a glossary intended to be sent via the DeepL API.
  Includes all entries and saves them a \"tsv\" file."
  (interactive (list (tlon-select-language 'code 'babel)
		     (intern (completing-read "Recipient? " '(human deepl-editor deepl-api) nil t))))
  (let* ((source-path tlon-file-glossary-source)
	 (target-path (tlon-glossary-target-path language recipient))
	 (json (tlon-read-json source-path nil 'list 'symbol)))
    (with-current-buffer (find-file-noselect target-path)
      (erase-buffer)
      (tlon-insert-formatted-glossary json language recipient)
      (save-buffer))
    (pcase recipient
      ('human (when (y-or-n-p "Share glossary with translators? ")
		(tlon-share-glossary target-path language)))
      ((or 'deepl-editor 'deepl-api) (message "Glossary extracted to `%s'" target-path)))))

;;;###autoload
(defun tlon-glossary-target-path (language recipient)
  "Return the target path for a glossary in LANGUAGE for RECIPIENT."
  (let ((target-extension (pcase recipient
			    ((or 'human 'deepl-editor) "csv")
			    ('deepl-api "tsv"))))
    (tlon-glossary-make-file language target-extension)))

(defun tlon-glossary-make-file (language extension)
  "Make a glossary file for LANGUAGE with EXTENSION."
  (file-name-concat paths-dir-downloads
		    (format "EN-%s.%s" (upcase language) extension)))

(defun tlon-insert-formatted-glossary (json language recipient)
  "Insert a properly formatted glossary in LANGUAGE from JSON data.
Format the glossary based on its RECIPIENT: if `human' or `deepl-editor', in
\"csv\" format; if `deepl-api', in \"tsv\" format. (DeepL requires different
formats depending on whether the glossary is meant to be uploaded to the editor
or via the API.)"
  (dolist (item json)
    (when-let* ((source-term (alist-get 'en item))
		(target-term (alist-get (intern language) item))
		(entry (pcase recipient
			 ('human (format "\"%s\",\"%s\"\n" source-term target-term))
			 ('deepl-editor
			  (format "\"%s\",\"%s\",\"EN\",\"%s\"\n" source-term target-term (upcase language)))
			 ('deepl-api (format "%s\t%s\n" source-term target-term)))))
      (when (or (member recipient '(deepl-editor deepl-api))
		(string= (alist-get 'type item) "variable"))
	(insert entry)))))

(defvar tlon-email-language)
(declare-function tlon-email-send "tlon-email")
;;;###autoload
(defun tlon-share-glossary (attachment &optional language)
  "Share LANGUAGE glossary with translators as ATTACHMENT."
  (interactive (list (read-file-name "Glossary file: " paths-dir-downloads) nil nil))
  (let* ((language (or language (tlon-select-language 'code 'babel)))
	 (recipient (tlon-lookup tlon-glossary-recipients :email :language language)))
    (setq tlon-email-language (tlon-lookup tlon-languages-properties :name :code language))
    (tlon-email-send "share-glossary.org" recipient attachment)))

;;;;; Menu

;;;###autoload (autoload 'tlon-glossary-menu "tlon-glossary" nil t)
(transient-define-prefix tlon-glossary-menu ()
  "Menu for glossary functions."
  [("e" "edit"              tlon-edit-glossary)
   ("x" "extract"           tlon-extract-glossary)
   ("s" "share"             tlon-share-glossary)])

(provide 'tlon-glossary)
;;; tlon-glossary.el ends here

