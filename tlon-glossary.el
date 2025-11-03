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
  "You are an expert multilingual glossary creator.\n\nYour task is to generate translations from the source language (%s) into the target language (%s).\n\nI will provide you with a list of terms in %s (one term per line) that currently lack a translation in %s.\n\nHere is the list of source-language terms needing translation:\n```text\n%s\n```\n\nPlease return *only* the translations for these terms into %s, one translation per line.\n\nExample output format:\n```text\nTranslation 1\nTranslation 2\nTranslation 3\n...\n```\n\n- Provide *exactly one* translation line for *every* input term.\n- Maintain the exact order of the translations corresponding to the input terms.\n- If you are unsure about a translation, provide your best guess or use the placeholder string \"[TRANSLATION_UNAVAILABLE]\". *Do not omit any terms.*\n- The total number of lines in your response *must* equal the number of lines in the input list.\n- Do *not* include the original source terms in your response.\n- Do *not* include any explanations, introductory text, numbering, bullet points, or any JSON/Markdown formatting. Return only the plain text translations, one per line."
  ;; %s = source language name
  ;; %s = target language name
  ;; %s = source language name
  ;; %s = target language name
  ;; %s = newline-separated list of source terms
  ;; %s = target language name
  "Prompt for generating translations for missing terms in a glossary language.")

(defconst tlon-ai-verify-glossary-translations-prompt
  "You are a text cleaning expert. I received the following text block which is *supposed* to be a list of translations into %s, one per line, corresponding to %d source terms. However, it might contain errors, extra text, incorrect formatting, or an incorrect number of lines.\n\nPlease analyze the following text block:\n```text\n%s\n```\n\nYour task is to return *only* the cleaned list of translations, one per line.\n- Ensure there are *exactly* %d lines in your output.\n- Each line should contain only the translation for the corresponding term.\n- Remove any introductory text, explanations, numbering, bullet points, JSON/Markdown formatting, or other extraneous content.\n- If the input seems corrupt or unusable for a specific line, use the placeholder \"[TRANSLATION_UNAVAILABLE]\".\n- If the input has fewer lines than expected, add placeholder lines at the end to reach the expected count.\n- If the input has more lines than expected, truncate it to the expected count.\n- Return *only* the plain text translations, one per line."
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
         (source-lang (tlon-select-language 'code 'babel "Source language for term: "))
         (source-terms (tlon-get-terms glossary source-lang))
         (input-term (completing-read "Choose or add a term: " source-terms nil nil ""))
         (matched-term (and (member input-term source-terms) input-term))
         term-to-use
         entry-to-save)
    (if matched-term
        (progn
          (setq term-to-use matched-term)
          (setq entry-to-save (tlon-find-entry-by-term-in-lang glossary term-to-use source-lang)))
      (progn
        (setq term-to-use input-term)
        (let ((type (tlon-select-term-type)))
          (setq entry-to-save (tlon-create-entry term-to-use type source-lang)))))
    (when entry-to-save
      (let ((type (cdr (assoc 'type entry-to-save))))
        (unless (and type (string= type "invariant"))
          (setq entry-to-save (tlon-edit-translation-in-entry entry-to-save term-to-use))))
      (setq glossary (tlon-update-glossary glossary entry-to-save term-to-use source-lang))
      (tlon-write-data tlon-file-glossary-source glossary))))

(defun tlon-parse-glossary ()
  "Parse the glossary file into Emacs Lisp using symbol keys."
  (tlon-read-json tlon-file-glossary-source nil 'list 'symbol))

(defun tlon-get-terms (glossary source-lang)
  "Extract all terms in SOURCE-LANG from GLOSSARY.
SOURCE-LANG is a language code string (e.g., \"en\")."
  (let ((key (intern source-lang)))
    (mapcar (lambda (entry) (alist-get key entry)) glossary)))

(defun tlon-find-entry-by-term-in-lang (glossary term source-lang)
  "Find ENTRY in GLOSSARY whose term in SOURCE-LANG equals TERM.
SOURCE-LANG is a language code string."
  (let ((key (intern source-lang)))
    (seq-find (lambda (entry)
                (string= (alist-get key entry) term))
              glossary)))

(defun tlon-select-term-type ()
  "Prompt the user to select a type for a new term and return the selection."
  (completing-read "Select type (variable/invariant): " '("variable" "invariant") nil t))

(defun tlon-create-entry (term type source-lang)
  "Create a new ENTRY for TERM of TYPE in SOURCE-LANG.
SOURCE-LANG is a language code string."
  (let* ((src-key (intern source-lang))
         (entry (list (cons src-key term) (cons 'type type))))
    (when (string= type "invariant")
      (dolist (lang tlon-project-target-languages)
        (setf entry (append entry (list (cons (intern lang) term))))))
    entry))

(declare-function tlon-deepl-maybe-glossary-update "tlon-deepl")
(defun tlon-edit-translation-in-entry (entry term)
  "Create or update a translation in ENTRY for TERM."
  (let* ((language (tlon-select-language 'code 'babel))
         (lang-key (intern language))
         (translation (assoc lang-key entry))
         (initial-input (when translation (cdr translation)))
         (read-translation (lambda ()
                             (read-string (format "Translation for \"%s\" (%s): "
                                                  term language)
                                          initial-input))))
    (if translation
        (setcdr translation (funcall read-translation))
      (setq entry (append entry (list (cons lang-key (funcall read-translation))))))
    (let* ((src-key (car (seq-find (lambda (cell)
                                     (and (symbolp (car cell))
                                          (not (eq (car cell) 'type))
                                          (stringp (cdr cell))
                                          (string= (cdr cell) term)))
                                   entry)))
           (src-code (and src-key (symbol-name src-key))))
      (if src-code
          (tlon-deepl-maybe-glossary-update (cons src-code language))
        (tlon-deepl-maybe-glossary-update language)))
    entry))

(defun tlon-update-glossary (glossary entry term key-lang)
  "Update GLOSSARY with ENTRY for TERM keyed by KEY-LANG.
KEY-LANG is a language code string used to identify the entry to update.
Return a new glossary list with the updated or appended entry."
  (let* ((key (intern key-lang))
         (result nil)
         (found nil))
    (dolist (e glossary)
      (if (and (not found)
               (equal (alist-get key e) term))
          (progn
            (push entry result)
            (setq found t))
        (push e result)))
    (unless found
      (push entry result))
    (nreverse result)))

;;;;; AI Glossary Generation

(declare-function tlon-make-gptel-request "tlon-ai")
;;;###autoload
(defun tlon-ai-create-glossary-language ()
  "Use AI to generate translations for missing terms in a target language.
Prompts for source and target languages, identifies terms that have a value in
the source but are missing in the target, sends only those source terms to an AI
model, and merges the AI-generated translations back into the glossary file."
  (interactive)
  (require 'tlon-ai)
  (let* ((src-lang-code (tlon-select-language 'code 'babel "Source language: "))
         (tgt-lang-code (tlon-select-language 'code 'babel "Target language: "))
         (src-lang-name (tlon-lookup tlon-languages-properties :standard :code src-lang-code))
         (tgt-lang-name (tlon-lookup tlon-languages-properties :standard :code tgt-lang-code))
         (glossary-data (tlon-parse-glossary))
         (src-key (intern src-lang-code))
         (tgt-key (intern tgt-lang-code))
         ;; Entries that have a source term but lack a target translation
         (missing-entries (cl-remove-if (lambda (entry) (assoc tgt-key entry))
                                        (cl-remove-if-not (lambda (entry) (alist-get src-key entry))
                                                          glossary-data)))
         ;; Extract source terms from missing entries
         (source-terms (mapcar (lambda (entry) (alist-get src-key entry))
                               missing-entries)))
    (unless source-terms
      (message "No missing %s translations for terms present in %s." tgt-lang-name src-lang-name)
      (cl-return-from tlon-ai-create-glossary-language))
    (let* ((missing-terms-text (mapconcat #'identity source-terms "\n"))
           (prompt (format tlon-ai-create-glossary-language-prompt
                           src-lang-name tgt-lang-name src-lang-name tgt-lang-name
                           missing-terms-text tgt-lang-name)))
      (message "Requesting AI to generate %d translations %s → %s..."
               (length source-terms) src-lang-name tgt-lang-name)
      (tlon-make-gptel-request prompt nil
                               (lambda (response info)
                                 (tlon-ai-create-glossary-language-callback
                                  response info src-lang-code tgt-lang-code glossary-data source-terms))
                               tlon-ai-glossary-model
                               'no-context-check))))

(declare-function tlon-ai-callback-fail "tlon-ai")
(defun tlon-ai-create-glossary-language-callback (raw-response info src-lang-code tgt-lang-code full-glossary-data source-terms)
  "Callback for `tlon-ai-create-glossary-language'.
Saves RAW-RESPONSE, then requests verification. INFO contains request info.
SRC-LANG-CODE and TGT-LANG-CODE are language codes. FULL-GLOSSARY-DATA is the
original data. SOURCE-TERMS are the terms sent to the AI."
  (if (not raw-response)
      (tlon-ai-callback-fail info)
    (let* ((num-expected (length source-terms))
           (target-lang-name (tlon-lookup tlon-languages-properties :name :code tgt-lang-code))
           (raw-response-file (file-name-concat paths-dir-downloads
                                                (format "ai-glossary-%s-%s-raw-temp.txt"
                                                        src-lang-code tgt-lang-code)))
           (verify-prompt (format tlon-ai-verify-glossary-translations-prompt
                                  target-lang-name num-expected raw-response num-expected)))
      (with-temp-file raw-response-file
        (insert raw-response))
      (message "Received initial translations (saved to %s). Requesting AI verification/cleaning..."
               (file-name-nondirectory raw-response-file))
      (tlon-make-gptel-request verify-prompt nil
                               (lambda (verified-response verify-info)
                                 (tlon-ai-process-verified-translations-callback
                                  verified-response verify-info src-lang-code tgt-lang-code full-glossary-data source-terms raw-response-file))
                               tlon-ai-glossary-verify-model
                               'no-context-check))))

(defun tlon-ai-process-verified-translations-callback (verified-response info src-lang-code tgt-lang-code full-glossary-data source-terms raw-response-file)
  "Process VERIFIED-RESPONSE from the cleaning AI.
Merge translations and write the updated glossary. Keep RAW-RESPONSE-FILE on
failure. INFO contains request info. SRC-LANG-CODE and TGT-LANG-CODE are
language codes. FULL-GLOSSARY-DATA is the original glossary data. SOURCE-TERMS
are the source terms sent to the AI."
  (if (not verified-response)
      (tlon-ai--handle-verification-failure info raw-response-file)
    (condition-case err
        (let* ((clean-response (tlon-ai--clean-verified-response verified-response))
               (received-translations (tlon-ai--parse-verified-translations clean-response))
               (validated-translations (tlon-ai--validate-translation-list received-translations source-terms))
               (translation-pairs (tlon-ai--create-translation-pairs validated-translations source-terms))
               (glossary-copy (copy-sequence full-glossary-data))
               (updated-count (tlon-ai--merge-translations-into-glossary translation-pairs tgt-lang-code glossary-copy src-lang-code)))
          (tlon-ai--handle-verification-success glossary-copy updated-count tgt-lang-code raw-response-file))
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

(defun tlon-ai--update-glossary-entry (entry new-lang-key translation)
  "Update ENTRY (alist) with TRANSLATION for NEW-LANG-KEY.
NEW-LANG-KEY is a symbol key. Modify ENTRY in place."
  (let ((cell (assoc new-lang-key entry)))
    (if cell
        (setcdr cell translation)
      (setcdr (last entry) (list (cons new-lang-key translation))))))

(defun tlon-ai--merge-translations-into-glossary (translation-pairs new-lang-code glossary-data src-lang-code)
  "Merge TRANSLATION-PAIRS into GLOSSARY-DATA.
NEW-LANG-CODE and SRC-LANG-CODE are language code strings. Return number merged."
  (let* ((src-key (intern src-lang-code))
         (tgt-key (intern new-lang-code))
         (updated-count 0))
    (dolist (pair translation-pairs)
      (let* ((src-term (car pair))
             (translation (cdr pair))
             (entry-to-update (cl-find-if (lambda (entry)
                                            (string= (alist-get src-key entry) src-term))
                                          glossary-data)))
        (when entry-to-update
          (tlon-ai--update-glossary-entry entry-to-update tgt-key translation)
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

;;;;; AI Glossary Filtering

(defconst tlon-ai-filter-glossary-prompt
  "You will receive (1) the full text of a source document in %s and (2) a \
list of candidate glossary terms (one per line).\n\nYour task is to return \
only the subset of candidate terms that are relevant to the document.\n\nA \
term is relevant if the exact term appears in the document (case-insensitive) \
or if a very obvious inflectional or spacing/hyphenation variant appears \
(e.g., plural/singular, capitalization, hyphen vs. space). Do not include \
synonyms or paraphrases that are not in the candidate list.\n\nReturn only the \
selected terms, one per line, exactly as they appear in the candidate list. Do \
not add explanations, numbering, bullet points, or any other formatting.\n\n\
Document:%sCandidate terms:%s"
  "Prompt for filtering a glossary to terms relevant to a specific document.")

;;;###autoload
(defun tlon-ai-extract-relevant-glossary (full-glossary file &optional source-language target-language)
  "Create a filtered glossary CSV for FILE using FULL-GLOSSARY.
FULL-GLOSSARY is a CSV with at least two columns: Source, Target. FILE is a
source-language file (normally English). The command asks an AI model to select
only the source terms relevant to FILE and writes a filtered two-column CSV
\\=(Source, Target) to the downloads directory.

When SOURCE-LANGUAGE or TARGET-LANGUAGE are nil, try to infer SOURCE-LANGUAGE
from FILE and prompt for TARGET-LANGUAGE."
  (interactive
   (list (read-file-name "Full glossary CSV: " paths-dir-downloads nil t)
         (read-file-name "Source document file: " nil nil t (buffer-file-name))))
  (let* ((src-lang (or source-language
                       (tlon-get-language-in-file file)
                       (tlon-select-language 'code 'babel "Source language code: ")))
         (tgt-lang (or target-language
                       (tlon-select-language 'code 'babel "Target language code: ")))
         (doc-text (tlon-get-string-dwim file))
         (pairs (tlon--glossary-read-csv-pairs full-glossary))
         (candidate-terms (mapcar #'car pairs))
         (seed-terms (tlon--glossary-terms-present doc-text candidate-terms))
         (doc-wrapped (format tlon-ai-string-wrapper doc-text))
         (cands-wrapped (format tlon-ai-string-wrapper
                                (mapconcat #'identity candidate-terms "\n")))
         (prompt (format tlon-ai-filter-glossary-prompt
                         (tlon-lookup tlon-languages-properties :standard :code src-lang)
                         doc-wrapped cands-wrapped))
         (out-path (tlon--glossary-filtered-target-path src-lang tgt-lang file)))
    (message "Filtering glossary with AI for %s → %s (candidates: %d, seeds: %d)..."
             (upcase src-lang) (upcase tgt-lang)
             (length candidate-terms) (length seed-terms))
    (tlon-make-gptel-request
     prompt nil
     (tlon-ai--wrap-plain-text-callback
      (lambda (response info)
        (if (not response)
            (progn
              (tlon-ai-callback-fail info)
              (when seed-terms
                (tlon--glossary-write-filtered out-path pairs seed-terms)
                (message "AI failed. Wrote deterministic filtered glossary with %d term(s) to %s"
                         (length seed-terms) (file-name-nondirectory out-path)))
              (unless seed-terms
                (tlon--glossary-write-filtered out-path pairs '())
                (message "AI failed. Wrote empty filtered glossary to %s"
                         (file-name-nondirectory out-path))))
          (let* ((lines (seq-uniq (split-string (string-trim response) "\n" t)))
                 (valid (seq-filter (lambda (term) (member term candidate-terms)) lines)))
            (if (null valid)
                (if seed-terms
                    (progn
                      (tlon--glossary-write-filtered out-path pairs seed-terms)
                      (message "AI returned no valid terms. Wrote deterministic filtered glossary with %d term(s) to %s"
                               (length seed-terms) (file-name-nondirectory out-path)))
                  (tlon--glossary-write-filtered out-path pairs '())
                  (message "No relevant terms found by AI or deterministic scan. Wrote empty filtered glossary to %s"
                           (file-name-nondirectory out-path)))
              (tlon--glossary-write-filtered out-path pairs valid)
              (message "Wrote AI-filtered glossary with %d term(s) to %s"
                       (length valid) (file-name-nondirectory out-path)))))))
     tlon-ai-glossary-model
     'no-context-check)
    out-path))

(defun tlon--glossary-read-csv-pairs (file)
  "Return a list of (SOURCE . TARGET) pairs parsed from CSV FILE.
Assumes the first two fields are source and target, quoted with double quotes.
Unescapes doubled quotes inside fields. Skips empty or malformed lines."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (pairs)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (pcase line
            ((and (pred (string-match-p "^\\s-*$")) _) nil)
            (_
             (when (string-match "^\"\\([^\"]*\\)\"\\s-*,\\s-*\"\\([^\"]*\\)\"" line)
               (let* ((src (replace-regexp-in-string "\"\"" "\"" (match-string 1 line) t t))
                      (tgt (replace-regexp-in-string "\"\"" "\"" (match-string 2 line) t t)))
                 (push (cons src tgt) pairs))))))
        (forward-line 1))
      (nreverse pairs))))

(defun tlon--glossary-terms-present (text terms)
  "Return TERMS that are present in TEXT with simple heuristics.
Case-insensitive. Matches whole words around the escaped term."
  (let ((case-fold-search t))
    (cl-loop for term in terms
             for rx = (concat "\\b" (regexp-quote term) "\\b")
             when (string-match-p rx text)
             collect term)))

(defun tlon--glossary-write-filtered (out-path pairs selected-terms)
  "Write a two-column CSV to OUT-PATH for SELECTED-TERMS using PAIRS.
PAIRS is an alist of (SOURCE . TARGET). SELECTED-TERMS is a list of source
terms."
  (let* ((table (make-hash-table :test 'equal)))
    (dolist (p pairs) (puthash (car p) (cdr p) table))
    (with-temp-file out-path
      (dolist (src selected-terms)
        (let ((tgt (or (gethash src table) "")))
          (insert (tlon--glossary-make-csv-row (list src tgt))))))))

(defun tlon--glossary-filtered-target-path (src-lang tgt-lang file)
  "Return target path for filtered glossary for SRC-LANG→TGT-LANG and FILE."
  (file-name-concat
   paths-dir-downloads
   (format "%s-%s-FILTERED-%s.csv"
           (upcase src-lang) (upcase tgt-lang) (file-name-base file))))

;;;;; Importing

;;;###autoload
(defun tlon-glossary-import-tsv (file)
  "Import translations from TSV FILE into the glossary.
The first line must be \"SRC<TAB>TGT\" language codes. Each subsequent line must
be \"SOURCE<TAB>TRANSLATION\". Only existing entries keyed by SRC are updated;
missing source terms are reported. Writes the updated glossary.

This command is useful for importing entries from the multilingual glossary in
Google Sheets. Just select the relevant rows in the source and target languages,
and paste the contents into a file."
  (interactive "fTSV file: ")
  (let* ((parsed (tlon--glossary-parse-tsv file))
         (src (nth 0 parsed))
         (tgt (nth 1 parsed))
         (pairs (nth 2 parsed))
         (glossary (tlon-parse-glossary))
         (tgt-key (intern tgt))
         (updated 0)
         (missing '()))
    (dolist (p pairs)
      (let* ((src-term (car p))
             (translation (cdr p))
             (entry (tlon-find-entry-by-term-in-lang glossary src-term src)))
        (if entry
            (progn
              (tlon-ai--update-glossary-entry entry tgt-key translation)
              (cl-incf updated))
          (if (y-or-n-p (format "No entry for \"%s\" in %s. Create it? " src-term (upcase src)))
              (let* ((type (tlon-select-term-type))
                     (new-entry (tlon-create-entry src-term type src)))
                (tlon-ai--update-glossary-entry new-entry tgt-key translation)
                (setq glossary (tlon-update-glossary glossary new-entry src-term src))
                (cl-incf updated))
            (push src-term missing)))))
    (tlon-write-data tlon-file-glossary-source glossary)
    (message "Imported %d translation(s) for %s→%s. Missing: %d"
             updated (upcase src) (upcase tgt) (length missing))
    (when missing
      (message "No matching entries for: %s"
               (mapconcat #'identity (nreverse missing) ", ")))
    glossary))

(defun tlon--glossary-parse-tsv (file)
  "Parse TSV FILE and return a list (SRC TGT PAIRS).
SRC and TGT are language code strings. PAIRS is a list of (SOURCE . TARGET)
strings. Errors if the header is malformed."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((s (buffer-string))
           (s (replace-regexp-in-string "\r\n?" "\n" s))
           (s (if (and (> (length s) 0) (eq (aref s 0) ?\ufeff)) (substring s 1) s))
           (lines (split-string s "\n" t))
           (header (car lines))
           (rest (cdr lines))
           (hfields (split-string header "\t")))
      (unless (= (length hfields) 2)
        (error "First line must have two tab-separated language codes"))
      (let ((src (downcase (string-trim (nth 0 hfields))))
            (tgt (downcase (string-trim (nth 1 hfields))))
            (pairs '()))
        (dolist (ln rest)
          (let* ((fields (split-string ln "\t"))
                 (a (and (nth 0 fields) (string-trim (nth 0 fields))))
                 (b (and (nth 1 fields) (string-trim (nth 1 fields)))))
            (when (and a b (> (length a) 0))
              (push (cons a b) pairs))))
        (list src tgt (nreverse pairs))))))

;;;;; Extraction

;;;###autoload
(defun tlon-extract-glossary (source-language target-language recipient)
  "Extract a glossary from SOURCE-LANGUAGE to TARGET-LANGUAGE for RECIPIENT.
RECIPIENT can be `human', `deepl-editor', `deepl-api', `ai-revision' or `sieve'.

- `human': includes only entries of type \"variable\", saves to CSV, and can
  optionally be shared with translators.

- `deepl-editor': includes all entries and saves to CSV. Writes language codes
  for both source and target.

- `deepl-api': includes all entries and saves to TSV.

- `ai-revision': includes only entries of type \"variable\" and saves to CSV.

- `sieve': includes entries that have a translation, saves to JSON mapping
  source terms to target terms."
  (interactive (list (tlon-select-language 'code 'babel "Source language: ")
                     (tlon-select-language 'code 'babel "Target language: ")
                     (intern (completing-read "Recipient? " '(human deepl-editor deepl-api ai-revision sieve) nil t))))
  (let* ((source-path tlon-file-glossary-source)
         (target-path (tlon-glossary-target-path source-language target-language recipient))
         (json (tlon-read-json source-path nil 'list 'symbol))
         (formatted-content (with-temp-buffer
                              (tlon-insert-formatted-glossary json source-language target-language recipient)
                              (buffer-string)))
         (trimmed (string-trim formatted-content)))
    (unless (or (string= trimmed "")
                (string= trimmed "[]"))
      (with-temp-file target-path
        (insert formatted-content))
      (pcase recipient
        ('human (when (y-or-n-p "Share glossary with translators? ")
                  (tlon-share-glossary target-path target-language)))
        ((or 'deepl-editor 'deepl-api 'ai-revision 'sieve)
         (message "Glossary extracted to `%s'" target-path)))
      target-path)))

;;;###autoload
(defun tlon-glossary-target-path (source-language target-language recipient)
  "Return the target path for a glossary from SOURCE-LANGUAGE to TARGET-LANGUAGE.
The file extension is chosen based on RECIPIENT."
  (let ((target-extension (pcase recipient
                            ((or 'human 'deepl-editor 'ai-revision) "csv")
                            ('deepl-api "tsv")
                            ('sieve "json"))))
    (tlon-glossary-make-file source-language target-language target-extension)))

(defun tlon-glossary-make-file (source-language target-language extension)
  "Make a glossary file for SOURCE-LANGUAGE to TARGET-LANGUAGE with EXTENSION."
  (file-name-concat paths-dir-downloads
                    (format "%s-%s.%s" (upcase source-language) (upcase target-language) extension)))

(defun tlon-insert-formatted-glossary (json source-language target-language recipient)
  "Insert a glossary from JSON data for SOURCE-LANGUAGE → TARGET-LANGUAGE.
Format depends on RECIPIENT: `human'/`deepl-editor' CSV, `deepl-api' TSV,
`sieve' JSON."
  (let* ((src-key (intern source-language))
         (tgt-key (intern target-language)))
    (if (eq recipient 'sieve)
        (tlon-insert-sieve-glossary json source-language target-language)
      (let ((pairs (tlon--glossary-build-mapping json src-key tgt-key recipient)))
        (pcase recipient
          ((or 'human 'ai-revision)
           (dolist (p pairs)
             (insert (format "\"%s\",\"%s\"\n" (car p) (cdr p)))))
          ('deepl-editor
           (dolist (p pairs)
             (insert (format "\"%s\",\"%s\",\"%s\",\"%s\"\n"
                             (car p) (cdr p)
                             (upcase source-language) (upcase target-language)))))
          ('deepl-api
           (dolist (p pairs)
             (insert (format "%s\t%s\n" (car p) (cdr p))))))))))

(defun tlon-insert-sieve-glossary (json source-language target-language)
  "Insert a sieve-formatted glossary for SOURCE-LANGUAGE → TARGET-LANGUAGE.
Creates a JSON object mapping source terms to target terms. Replace ASCII
apostrophes (') with typographic apostrophes (’) on both sides."
  (let* ((src-key (intern source-language))
         (tgt-key (intern target-language))
         (pairs (tlon--glossary-build-mapping json src-key tgt-key 'sieve))
         (fixed (mapcar (lambda (p)
                          (cons (replace-regexp-in-string "'" "’" (car p))
                                (replace-regexp-in-string "'" "’" (cdr p))))
                        pairs)))
    (insert (json-encode fixed))))

(defun tlon--glossary-build-mapping (json src-key tgt-key recipient)
  "Return ordered unique SRC-KEY→TGT-KEY pairs from JSON for RECIPIENT.
- Trim whitespace from both source and target terms.
- Skip pairs with empty source or target.
- For duplicates on the source term, prefer entries of type \"variable\"
  over \"invariant\"; otherwise keep the first seen. Emit a summary message
  when duplicates are collapsed."
  (let ((pairs '())
        (collisions 0))
    (dolist (item json)
      (when (or (member recipient '(deepl-editor deepl-api sieve))
                (string= (alist-get 'type item) "variable"))
        (let* ((raw-src (alist-get src-key item))
               (raw-tgt (alist-get tgt-key item))
               (src (tlon--glossary-normalize-term raw-src))
               (tgt (tlon--glossary-normalize-term raw-tgt)))
          (when (and src tgt)
            (let ((existing (assoc src pairs)))
              (if (not existing)
                  (push (cons src tgt) pairs)
                (setq collisions (1+ collisions))
                (when (and (string= (alist-get 'type item) "variable")
                           (not (string= (cdr existing) tgt)))
                  (setcdr existing tgt))))))))
    (when (> collisions 0)
      (message "Note: collapsed %d duplicate source terms for %s→%s"
               collisions (symbol-name src-key) (symbol-name tgt-key)))
    (nreverse pairs)))

(defun tlon--glossary-normalize-term (s)
  "Trim surrounding whitespace in S and return nil if empty."
  (let ((trimmed (and (stringp s) (string-trim s))))
    (and trimmed (> (length trimmed) 0) trimmed)))

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

;;;###autoload
(defun tlon-extract-multilingual-glossary ()
  "Extract a multilingual CSV with all existing translations for each term."
  (interactive)
  (let* ((json (tlon-read-json tlon-file-glossary-source nil 'list 'symbol))
         (languages (tlon--glossary-detect-languages json))
         (path (tlon--glossary-multilingual-target-path)))
    (with-temp-file path
      (insert (tlon--glossary-make-csv-row
               (mapcar (lambda (sym) (upcase (symbol-name sym))) languages)))
      (dolist (item json)
        (let ((row (mapcar (lambda (lang) (or (alist-get lang item) ""))
                           languages)))
          (insert (tlon--glossary-make-csv-row row)))))
    (message "Multilingual glossary extracted to `%s'" path)
    path))

(defun tlon--glossary-multilingual-target-path ()
  "Return the target filepath for the multilingual CSV."
  (file-name-concat paths-dir-downloads "ALL.csv"))

(defun tlon--glossary-make-csv-row (fields)
  "Return a CSV row for FIELDS."
  (concat (mapconcat (lambda (f)
                       (format "\"%s\""
                               (tlon--glossary-escape (format "%s" f))))
                     fields
                     ",")
          "\n"))

(defun tlon--glossary-escape (s)
  "Escape double quotes in string S for CSV."
  (replace-regexp-in-string "\"" "\"\"" s t t))

(defun tlon--glossary-detect-languages (json)
  "Detect language keys present in JSON and return an ordered list."
  (let ((present '()))
    (dolist (item json)
      (dolist (pair item)
        (let ((k (car pair)))
          (when (and (symbolp k) (not (eq k 'type)))
            (cl-pushnew k present)))))
    (setq present (cl-remove-if (lambda (k) (eq k 'en)) present))
    (let* ((preferred (mapcar #'intern
                              (cl-remove-if (lambda (s) (string= s "en"))
                                            tlon-project-target-languages)))
           (ordered (append '(en)
                            (cl-remove-duplicates
                             (seq-filter (lambda (l) (memq l present)) preferred))
                            (cl-set-difference
                             (sort (copy-sequence present)
                                   (lambda (a b)
                                     (string< (symbol-name a) (symbol-name b))))
                             preferred))))
      ordered)))

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
    ("i" "Import TSV"              tlon-glossary-import-tsv)
    ("X" "Extract multilingual (CSV)" tlon-extract-multilingual-glossary)
    ("s" "Share glossary"          tlon-share-glossary)]
   ["AI Actions"
    ("a" "AI Create Language"    tlon-ai-create-glossary-language)
    ("g" "AI Filter Glossary"    tlon-ai-extract-relevant-glossary)
    ""
    "Models"
    ("m -g" "Glossary generation" tlon-ai-infix-select-glossary-model)
    ("m -v" "Glossary verification" tlon-ai-infix-select-glossary-verify-model)]])

(provide 'tlon-glossary)
;;; tlon-glossary.el ends here

