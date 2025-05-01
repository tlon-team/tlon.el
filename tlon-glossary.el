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
(require 'tlon-ai) ; Added for AI functions
(require 'tlon-core) ; Added for tlon-write-data, tlon-read-json, etc.
(require 'json) ; Added for json-read-from-string
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

;;;;; AI Glossary Generation

;;;###autoload
(defun tlon-ai-create-glossary-language ()
  "Use AI to generate translations for MISSING glossary terms in language.
Prompts for a target language, identifies terms missing a translation for that
language, sends only those terms to an AI model, and merges the AI-generated
translations back into the glossary file. Can be run iteratively."
  (interactive)
  (let* ((new-lang-code (tlon-select-language 'code 'babel "Target language: "))
         (new-lang-name (tlon-lookup tlon-languages-properties :name :code new-lang-code))
         (glossary-data (tlon-parse-glossary))
         ;; Filter entries missing the target language
         (missing-entries (cl-remove-if (lambda (entry) (assoc new-lang-code entry))
                                        glossary-data))
         ;; Extract English terms from missing entries
         (missing-en-terms (mapcar (lambda (entry) (cdr (assoc "en" entry)))
                                   missing-entries)))
    (unless missing-en-terms
      (message "All terms already have a translation for %s (%s)." new-lang-name new-lang-code)
      (cl-return-from tlon-ai-create-glossary-language))
    ;; Format missing terms as a simple newline-separated string for the prompt
    (let* ((missing-terms-text (mapconcat #'identity missing-en-terms "\n"))
           (prompt (format tlon-ai-create-glossary-language-prompt
                           new-lang-name missing-terms-text new-lang-name))) ; Pass lang name twice
      (message "Requesting AI to generate %d missing translations for %s (%s)..."
               (length missing-en-terms) new-lang-name new-lang-code)
      (tlon-make-gptel-request prompt nil
                               (lambda (response info)
                                 (tlon-ai-create-glossary-language-callback
                                  response info new-lang-code glossary-data missing-en-terms))
                               tlon-ai-glossary-model
                               'no-context-check))))

(defun tlon-ai-create-glossary-language-callback (raw-response info new-lang-code full-glossary-data missing-en-terms)
  "Callback function for `tlon-ai-create-glossary-language'.
Receives the RAW-RESPONSE from the first AI (translation generation). Initiates
a second AI call for verification and cleaning. INFO contains the request
information. NEW-LANG-CODE is the target language code. FULL-GLOSSARY-DATA is
the original glossary data. MISSING-EN-TERMS is the list of English terms that
were sent to the AI."
  (if (not raw-response)
      (tlon-ai-callback-fail info) ; Use the fail callback from tlon-ai
    (let* ((num-expected (length missing-en-terms))
           (target-lang-name (tlon-lookup tlon-languages-properties :name :code new-lang-code))
           ;; Define temp file path for raw response
           (raw-response-file (file-name-concat paths-dir-downloads
                                                (format "ai-glossary-%s-raw-temp.txt" new-lang-code)))
           (verify-prompt (format tlon-ai-verify-glossary-translations-prompt
                                  target-lang-name num-expected raw-response num-expected)))
      ;; Save the raw response before verification
      (with-temp-file raw-response-file
        (insert raw-response))
      (message "Received initial translations (saved to %s). Requesting AI verification/cleaning..."
               (file-name-nondirectory raw-response-file))
      (tlon-make-gptel-request verify-prompt nil
                               (lambda (verified-response verify-info)
                                 ;; Pass temp file path and other data to the final processing callback
                                 (tlon-ai-process-verified-translations-callback
                                  verified-response verify-info new-lang-code full-glossary-data missing-en-terms raw-response-file))
                               tlon-ai-glossary-verify-model ; Use specific verify model
                               'no-context-check))))

(defun tlon-ai-process-verified-translations-callback (verified-response info new-lang-code full-glossary-data missing-en-terms raw-response-file)
  "Callback function to process the VERIFIED-RESPONSE from the cleaning AI.
Parses the cleaned list, merges translations, writes the updated glossary, and
deletes RAW-RESPONSE-FILE on success. INFO contains the request information.
NEW-LANG-CODE is the target language code. FULL-GLOSSARY-DATA is the original
glossary data. MISSING-EN-TERMS is the list of English terms that were sent to
the AI."
  (if (not verified-response)
      (tlon-ai--handle-verification-failure info raw-response-file)
    (condition-case err
        (let* ((clean-response (tlon-ai--clean-verified-response verified-response))
               (received-translations (tlon-ai--parse-verified-translations clean-response))
               (validated-translations (tlon-ai--validate-translation-list received-translations missing-en-terms))
               (translation-pairs (tlon-ai--create-translation-pairs validated-translations missing-en-terms))
               (glossary-copy (copy-sequence full-glossary-data))
               (updated-count (tlon-ai--merge-translations-into-glossary translation-pairs new-lang-code glossary-copy)))
          (tlon-ai--handle-verification-success glossary-copy updated-count new-lang-code raw-response-file))
      (error
       (tlon-ai--handle-processing-error err verified-response raw-response-file)))))

(defun tlon-ai--clean-verified-response (verified-response)
  "Clean the VERIFIED-RESPONSE string by removing markdown fences and trimming."
  (let* ((lines (split-string verified-response "\n" t))
         (fence-pattern (rx-to-string '(seq bol (* space) "```" (opt (one-or-more nonl)) (* space) eol)))
         (filtered-lines (cl-remove-if (lambda (line) (string-match-p fence-pattern line)) lines)))
    (string-trim (mapconcat #'identity filtered-lines "\n"))))

(defun tlon-ai--parse-verified-translations (clean-response)
  "Parse the CLEAN-RESPONSE string into a list of translation strings."
  (split-string clean-response "\n" t))

(defun tlon-ai--validate-translation-list (received-translations missing-en-terms)
  "Validate RECEIVED-TRANSLATIONS against MISSING-EN-TERMS.
Warns if lengths differ but proceeds with the minimum available.
Returns the potentially truncated list of RECEIVED-TRANSLATIONS."
  (let ((num-received (length received-translations))
        (num-expected (length missing-en-terms)))
    (when (/= num-received num-expected)
      (message "Warning: Verification AI returned %d translation lines, but %d were expected. Processing the %d available..."
               num-received num-expected (min num-received num-expected)))
    ;; Return the list truncated to the minimum length to avoid errors later
    (seq-subseq received-translations 0 (min num-received num-expected)))) ; Maintain original order

(defun tlon-ai--create-translation-pairs (validated-translations missing-en-terms)
  "Create a list of (EN-TERM . TRANSLATION) pairs.
VALIDATED-TRANSLATIONS is the list of translation strings.
MISSING-EN-TERMS is the corresponding list of English terms.
Filters out pairs where the translation is '[TRANSLATION_UNAVAILABLE]'.
Ensures translations are strings."
  (let ((translation-pairs '())
        (num-pairs (length validated-translations))) ; Use length of validated list
    (dotimes (i num-pairs)
      (let* ((en-term (elt missing-en-terms i))
             (translation (elt validated-translations i)))
        (unless (stringp translation)
          (error "Invalid non-string translation format in verified AI response line %d: %S" (1+ i) translation))
        (unless (string= translation "[TRANSLATION_UNAVAILABLE]")
          (push (cons en-term translation) translation-pairs))))
    (nreverse translation-pairs)))

(defun tlon-ai--update-glossary-entry (entry new-lang-code translation)
  "Update a single glossary ENTRY (alist) with TRANSLATION for NEW-LANG-CODE.
Modifies ENTRY in place."
  (if (assoc new-lang-code entry)
      (setcdr (assoc new-lang-code entry) translation)
    ;; Append to the end of the alist
    (setcdr (last entry)
            (append (cdr (last entry))
                    (list (cons new-lang-code translation))))))

(defun tlon-ai--merge-translations-into-glossary (translation-pairs new-lang-code glossary-data)
  "Merge TRANSLATION-PAIRS into GLOSSARY-DATA for NEW-LANG-CODE.
Returns the number of entries successfully merged."
  (let ((updated-count 0))
    (dolist (pair translation-pairs)
      (let* ((en-term (car pair))
             (translation (cdr pair))
             (entry-to-update (cl-find-if (lambda (entry)
                                            (string= (cdr (assoc "en" entry)) en-term))
                                          glossary-data)))
        (when entry-to-update
          (tlon-ai--update-glossary-entry entry-to-update new-lang-code translation)
          (cl-incf updated-count))))
    updated-count))

(defun tlon-ai--handle-verification-success (updated-glossary updated-count new-lang-code raw-response-file)
  "Handle successful processing: write glossary, log success, delete raw file.
UPDATED-GLOSSARY is the modified glossary data. UPDATED-COUNT is the number of
entries updated. NEW-LANG-CODE is the target language code. RAW-RESPONSE-FILE is
the path to the temporary file containing the raw AI response."
  (tlon-write-data tlon-file-glossary-source updated-glossary)
  (message "Successfully merged %d non-placeholder verified AI translations for '%s' into %s"
           updated-count new-lang-code (file-name-nondirectory tlon-file-glossary-source))
  (when (file-exists-p raw-response-file)
    (delete-file raw-response-file)
    (message "Deleted temporary raw response file: %s" (file-name-nondirectory raw-response-file))))

(defun tlon-ai--handle-processing-error (err verified-response raw-response-file)
  "Handle errors during processing: log error and keep raw response.
ERR is the error object. VERIFIED-RESPONSE is the AI response. RAW-RESPONSE-FILE
is the path to the raw response file."
  (message "Error processing verified AI response: %s. Raw response kept at: %s"
           (error-message-string err) raw-response-file)
  (message "Verified Response was: %s" verified-response))

(defun tlon-ai--handle-verification-failure (info raw-response-file)
  "Handle failure reported by the verification AI itself.
INFO contains the request information. RAW-RESPONSE-FILE is the path to the raw
response file."
  (message "AI verification/cleaning step failed. Raw response kept at: %s" raw-response-file)
  (tlon-ai-callback-fail info))

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
  [["Glossary Actions"
    ("e" "Edit entry"              tlon-edit-glossary)
    ("x" "Extract glossary"        tlon-extract-glossary)
    ("s" "Share glossary"          tlon-share-glossary)]
   ["AI Actions"
    ("a" "AI Create Language"    tlon-ai-create-glossary-language)]])

(provide 'tlon-glossary)
;;; tlon-glossary.el ends here

