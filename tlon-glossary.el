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
(require 'tlon-ai)
(require 'json)
(require 'transient)

;;;; Variables

(defgroup tlon-glossary nil
  "Glossary functions for Tlön."
  :group 'tlon)

(defcustom tlon-ai-glossary-model
  '("Gemini" . gemini-2.0-flash-thinking-exp-01-21)
  "Model to use for AI glossary generation (`tlon-ai-create-glossary-language').
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, use the
default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-glossary)

(defcustom tlon-ai-glossary-verify-model
  '("Gemini" . gemini-2.0-flash-thinking-exp-01-21)
  "Model to use for verifying/cleaning AI glossary translations.
See `tlon-ai-glossary-model' for details. If nil, use the default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-glossary)

(defconst tlon-ai-create-glossary-language-prompt
  "You are an expert multilingual glossary creator.\n\nYour task is to generate translations for a list of English terms into the target language: *%s*.\n\nI will provide you with a list of English terms that currently lack a translation in the target language (one term per line).\n\nHere is the list of English terms needing translation:\n```text\n%s\n```\n\nPlease return *only* the translations for these terms into the target language (%s), one translation per line.\n\nExample output format:\n```text\nTranslation 1\nTranslation 2\nTranslation 3\n...\n```\n\n- Provide *exactly one* translation line for *every* English term in the input list.\n- Maintain the exact order of the translations corresponding to the input terms.\n- If you are unsure about a translation, provide your best guess or use the placeholder string \"[TRANSLATION_UNAVAILABLE]\". *Do not omit any terms.*\n- The total number of lines in your response *must* equal the number of lines in the input list.\n- Do *not* include the original English terms in your response.\n- Do *not* include any explanations, introductory text, numbering, bullet points, or any JSON/Markdown formatting. Return only the plain text translations, one per line."
  ;; %s will be the target language name (e.g., "German")
  ;; %s will be the newline-separated list of English terms needing translation
  ;; %s will be the target language name again (for the prompt text)
  "Prompt for generating translations for missing terms in a glossary language.")

(defconst tlon-ai-verify-glossary-translations-prompt
  "You are a text cleaning expert. I received the following text block which is *supposed* to be a list of translations into %s, one per line, corresponding to %d English terms. However, it might contain errors, extra text, incorrect formatting, or an incorrect number of lines.\n\nPlease analyze the following text block:\n```text\n%s\n```\n\nYour task is to return *only* the cleaned list of translations, one per line.\n- Ensure there are *exactly* %d lines in your output.\n- Each line should contain only the translation for the corresponding term.\n- Remove any introductory text, explanations, numbering, bullet points, JSON/Markdown formatting, or other extraneous content.\n- If the input seems corrupt or unusable for a specific line, use the placeholder \"[TRANSLATION_UNAVAILABLE]\".\n- If the input has fewer lines than expected, add placeholder lines at the end to reach the expected count.\n- If the input has more lines than expected, truncate it to the expected count.\n- Return *only* the plain text translations, one per line."
  ;; %s = target language name
  ;; %d = expected number of translations
  ;; %s = raw response from first AI
  ;; %d = expected number of translations (again)
  "Prompt for verifying and cleaning AI-generated glossary translations.")

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
         ;; Allow user to type new term or select existing (case-insensitive match)
         (input-term (completing-read "Choose or add a term: " english-terms nil nil ""))
         ;; Check if input matches an existing term case-insensitively
         (matched-term (seq-find (lambda (et) (string-equal input-term et)) english-terms))
         ;; Declare variables for the term to use and the entry to save/modify
         term-to-use
         entry-to-save)
    ;; Determine term-to-use and entry-to-save based on whether a match was found
    (if matched-term
        ;; Case 1: Existing term selected (potentially with different casing)
        (progn
          (setq term-to-use matched-term) ; Use the correctly cased term from glossary
          ;; Find the existing entry using the correctly cased term (guaranteed to exist)
          (setq entry-to-save (tlon-find-entry-by-term glossary term-to-use)))
      ;; Case 2: Genuinely new term entered
      (progn
        (setq term-to-use input-term) ; Use the user's input casing
        ;; Prompt for type and create the new entry structure
        (let ((type (tlon-select-term-type)))
          (setq entry-to-save (tlon-create-entry term-to-use type)))))
    ;; Now entry-to-save holds either the existing entry or the newly created one
    ;; Ensure entry-to-save is valid before proceeding (should always be unless error)
    (when entry-to-save
      ;; Edit translations unless it's an invariant term
      (let ((type (cdr (assoc "type" entry-to-save))))
        (unless (and type (string= type "invariant"))
          (setq entry-to-save (tlon-edit-translation-in-entry entry-to-save term-to-use))))
      ;; Update the main glossary list using the final entry state and correct term
      (setq glossary (tlon-update-glossary glossary entry-to-save term-to-use))
      ;; Save the updated glossary
      (tlon-write-data tlon-file-glossary-source glossary))))

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

(declare-function tlon-deepl-maybe-glossary-update "tlon-deepl")
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
    (tlon-deepl-maybe-glossary-update language)
    entry))

(defun tlon-update-glossary (glossary entry term)
  "Update GLOSSARY with a new or modified ENTRY for a TERM.
Returns a new glossary list with the updated or appended entry."
  (let ((result nil)
        (found nil))
    ;; Build the list in reverse, replacing the target entry if found
    (dolist (e glossary)
      (if (and (not found) (equal (assoc "en" e) (cons "en" term)))
          (progn
            (push entry result) ; Push the new/modified entry instead of e
            (setq found t))
        (push e result))) ; Push the original entry
    ;; If not found after iterating, add the new entry (it will be first after nreverse)
    (unless found
      (push entry result))
    ;; Reverse the final list to restore original order (or append new entry)
    (nreverse result)))

;;;;; AI Glossary Generation

(declare-function tlon-make-gptel-request "tlon-ai")
;;;###autoload
(defun tlon-ai-create-glossary-language ()
  "Use AI to generate translations for MISSING glossary terms in language.
Prompts for a target language, identifies terms missing a translation for that
language, sends only those terms to an AI model, and merges the AI-generated
translations back into the glossary file. Can be run iteratively."
  (interactive)
  (require 'tlon-ai)
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

(declare-function tlon-ai-callback-fail "tlon-ai")
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
RECIPIENT can be `human', `deepl-editor', `deepl-api', `ai-revision' and
`sieve'.

- `human': extracts a glossary intended to be shared with another human being.
 Includes only entries of type \"variable\", saves them to a \"csv\" file, and
 prompts the user to share the glossary with translators.

- `deepl-editor': extracts a glossary intended to be uploaded to the DeepL
  editor. Includes all entries and saves them to a \"csv\" file.

- `deepl-api': extracts a glossary intended to be sent via the DeepL API.
  Includes all entries and saves them a \"tsv\" file.

- `ai-revision': extracts a glossary intended to be shared with an AI model.
 Includes only entries of type \"variable\" and saves them to a \"csv\" file.

- `sieve': extracts a glossary as a JSON object with source terms as keys and
  target terms as values. Includes all entries and saves them to a \"json\"
  file."
  (interactive (list (tlon-select-language 'code 'babel)
		     (intern (completing-read "Recipient? " '(human deepl-editor deepl-api ai-revision sieve) nil t))))
  (let* ((source-path tlon-file-glossary-source)
	 (target-path (tlon-glossary-target-path language recipient))
	 (json (tlon-read-json source-path nil 'list 'symbol))
         ;; Render the glossary in a temporary buffer and capture its contents
         (formatted-content (with-temp-buffer
                              (tlon-insert-formatted-glossary json language recipient)
                              (buffer-string)))
         (trimmed (string-trim formatted-content)))
    ;; If no terms exist for LANGUAGE, do nothing and return nil.
    (unless (or (string= trimmed "")
                (string= trimmed "[]"))
      (with-temp-file target-path
        (insert formatted-content))
      (pcase recipient
        ('human (when (y-or-n-p "Share glossary with translators? ")
		  (tlon-share-glossary target-path language)))
        ((or 'deepl-editor 'deepl-api 'ai-revision 'sieve) (message "Glossary extracted to `%s'" target-path)))
      target-path)))

;;;###autoload
(defun tlon-glossary-target-path (language recipient)
  "Return the target path for a glossary in LANGUAGE for RECIPIENT."
  (let ((target-extension (pcase recipient
			    ((or 'human 'deepl-editor 'ai-revision) "csv")
			    ('deepl-api "tsv")
			    ('sieve "json"))))
    (tlon-glossary-make-file language target-extension)))

(defun tlon-glossary-make-file (language extension)
  "Make a glossary file for LANGUAGE with EXTENSION."
  (file-name-concat paths-dir-downloads
		    (format "EN-%s.%s" (upcase language) extension)))

(defun tlon-insert-formatted-glossary (json language recipient)
  "Insert a properly formatted glossary in LANGUAGE from JSON data.
Format the glossary based on its RECIPIENT: if `human' or `deepl-editor', in
\"csv\" format; if `deepl-api', in \"tsv\" format; if `sieve', in JSON format.
\\=(DeepL requires different formats depending on whether the glossary is meant
to be uploaded to the editor or via the API.)"
  (if (eq recipient 'sieve)
      (tlon-insert-sieve-glossary json language)
    (dolist (item json)
      (when-let* ((source-term (alist-get 'en item))
		  (target-term (alist-get (intern language) item))
		  (entry (pcase recipient
			   ((or 'human 'ai-revision) (format "\"%s\",\"%s\"\n" source-term target-term))
			   ('deepl-editor
			    (format "\"%s\",\"%s\",\"EN\",\"%s\"\n" source-term target-term (upcase language)))
			   ('deepl-api (format "%s\t%s\n" source-term target-term)))))
	(when (or (member recipient '(deepl-editor deepl-api))
		  (string= (alist-get 'type item) "variable"))
	  (insert entry))))))

(defun tlon-insert-sieve-glossary (json language)
  "Insert a sieve-formatted glossary in LANGUAGE from JSON data.
Creates a JSON object with source terms as keys and target terms as values.
Standard apostrophes (') in both source and target terms are replaced with
typographic apostrophes (’) to prevent Sieve errors."
  (let ((glossary-object '()))
    (dolist (item json)
      (when-let* ((raw-source-term (alist-get 'en item))
		  (raw-target-term (alist-get (intern language) item)))
        (let ((source-term (replace-regexp-in-string "'" "’" raw-source-term))
              (target-term (replace-regexp-in-string "'" "’" raw-target-term)))
          (push (cons source-term target-term) glossary-object))))
    (insert (json-encode (nreverse glossary-object)))))

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

(transient-define-infix tlon-ai-infix-select-glossary-model ()
  "AI model to use for glossary generation.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-ai-glossary-model)

(transient-define-infix tlon-ai-infix-select-glossary-verify-model ()
  "AI model to use for verifying glossary translations.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-ai-glossary-verify-model)

;;;###autoload (autoload 'tlon-glossary-menu "tlon-glossary" nil t)
(transient-define-prefix tlon-glossary-menu ()
  "Menu for glossary functions."
  [["Glossary Actions"
    ("e" "Edit entry"              tlon-edit-glossary)
    ("x" "Extract glossary"        tlon-extract-glossary)
    ("s" "Share glossary"          tlon-share-glossary)]
   ["AI Actions"
    ("a" "AI Create Language"    tlon-ai-create-glossary-language)
    ""
    "Models"
    ("m -g" "Glossary generation" tlon-ai-infix-select-glossary-model)
    ("m -v" "Glossary verification" tlon-ai-infix-select-glossary-verify-model)]])

(provide 'tlon-glossary)
;;; tlon-glossary.el ends here

