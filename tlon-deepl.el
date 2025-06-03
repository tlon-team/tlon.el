;;; tlon-deepl.el --- Support for DeepL API calls -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon

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

(require 'tlon-ai) ; For tlon-simple-model-selection-infix
(require 'tlon-core)
(require 'tlon-glossary)
(require 'url)
(require 'seq) ; For seq-uniq

;;;; User options

(defgroup tlon-deepl nil
  "DeepL functionality for Tlön."
  :group 'tlon)

(defcustom tlon-deepl-model-type "latency_optimized"
  "Model type for DeepL translation.
Possible values are:

- `latency_optimized' (uses lower latency “classic” translation models, which
  support all language pairs; default value)

- `quality_optimized' (uses higher latency, improved quality “next-gen”
  translation models, which support only a subset of language pairs; if a
  language pair that is not supported by next-gen models is included in the
  request, it will fail. Consider using prefer_quality_optimized instead.)

- `prefer_quality_optimized' (prioritizes use of higher latency, improved
  quality “next-gen” translation models, which support only a subset of DeepL
  languages; if a request includes a language pair not supported by next-gen
  models, the request will fall back to latency_optimized classic models)

See <https://developers.deepl.com/docs/api-reference/translate#request-body-descriptions>."
  :group 'tlon-deepl
  :type '(choice (const :tag "Latency Optimized" "latency_optimized")
		 (const :tag "Quality Optimized" "quality_optimized")
		 (const :tag "Prefer Quality Optimized" "prefer_quality_optimized")))

;;;; Variables

(defconst tlon-deepl-key
  (auth-source-pass-get "key" (concat "tlon/babel/deepl.com/" (getenv "WORK_EMAIL")))
  "The DeepL API key.")

(defconst tlon-deepl-url-prefix
  "https://api.deepl.com/v2/"
  "URL common to all DeepL API calls.")

(defconst tlon-deepl-parameters
  '((translate . ("POST"
		  "translate"
		  tlon-deepl-print-translation
		  tlon-deepl-translate-encode))
    (glossary-create . ("POST"
			"glossaries"
			tlon-deepl-glossary-create-callback
			tlon-deepl-glossary-create-encode))
    (glossary-get . ("GET"
		     "glossaries"
		     tlon-deepl-glossary-get-callback))
    (glossary-delete . ("DELETE"
			tlon-deepl-glossary-delete-formatter
			tlon-deepl-glossary-delete-callback)))
  "Alist of API calls and their parameters.
The cdr of each cons cell is a list of the form
 (METHOD URL-SUFFIX CALLBACK JSON).")

(defvar tlon-deepl-glossaries nil
  "A list of glossaries retrieved from the DeepL API.")

(defvar tlon-deepl-source-language nil
  "Source language of the current API request.")

(defvar tlon-deepl-target-language nil
  "Target language of the current API request.")

(defvar tlon-deepl-text nil
  "The text to be translated in the current API request.")

(defvar tlon-deepl--override-glossary-id nil
  "When non-nil, force `tlon-deepl-glossary-delete-formatter' to use this ID.

This is a dynamically-bound helper set by `tlon-deepl-glossary-delete'
so the formatter can build the proper endpoint without re-prompting
the user.")

(defconst tlon-deepl-supported-glossary-languages
  '("da" "de" "en" "es" "fr" "it" "ja" "ko" "nb" "nl" "pl" "pt" "ro" "ru" "sv" "zh")
  "A list of the languages for which glossaries are currently supported.
See <https://developers.deepl.com/docs/api-reference/glossaries> for the
official source.")

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

;;;###autoload
(defun tlon-deepl-select-glossary ()
  "Select an existing DeepL glossary."
  (interactive)
  (let* ((glossaries (tlon-deepl-get-list-of-glossaries))
	 (selection (completing-read "Glossary: " glossaries nil t)))
    (alist-get selection glossaries nil nil #'string=)))

(defun tlon-deepl-get-list-of-glossaries ()
  "Return a cons cell of glossaries and their metadata."
  (let (cons)
    (dolist (glossary tlon-deepl-glossaries cons)
      (let* ((data (mapcar (lambda (key)
			     (alist-get key glossary nil nil #'string=))
			   '(glossary_id name source_lang target_lang entry_count)))
	     (id (car data)))
	(push
	 (cons (apply #'format "%2$20s %3$4s %4$4s %5$3s %1$40s" data) id) cons)))))

;;;;; API

(defun tlon-deepl-request-wrapper (type &optional callback no-glossary-ok)
  "Wrapper for API requests of TYPE.
If CALLBACK is nil, use the default callback for TYPE. If NO-GLOSSARY-OK is
non-nil, don't ask for confirmation when no glossary is found."
  (cl-destructuring-bind (method url-suffix-or-fun default-callback &optional json)
      (alist-get type tlon-deepl-parameters)
    (let* ((callback (or callback default-callback))
           (url (if (functionp url-suffix-or-fun)
                    (funcall url-suffix-or-fun)
                  (concat tlon-deepl-url-prefix url-suffix-or-fun)))
           (payload (when json (funcall json no-glossary-ok)))
           (temp-file (make-temp-file "deepl-payload-" nil ".json")))
      (when payload
	(with-temp-file temp-file
          (insert payload)))
      (with-temp-buffer
        (set-buffer-multibyte t)
        (set-buffer-file-coding-system 'utf-8)
        (let ((args (list "-s" "-X" method url
                          "-H" "Content-Type: application/json"
                          "-H" (concat "Authorization: DeepL-Auth-Key " tlon-deepl-key))))
          (when payload
            (setq args (append args (list "--data" (concat "@" temp-file)))))
          (apply #'call-process "curl" nil t nil args))
        (when payload (delete-file temp-file))
        (goto-char (point-min))
        (when (re-search-forward "^{\\|\\[{" nil t)
          (goto-char (match-beginning 0)))
        (condition-case err
            (funcall callback)
          (error
           (message "Error in DeepL callback: %S" err)))))))

;;;;; Translation

;;;###autoload
(defun tlon-deepl-translate (&optional text target-lang source-lang callback no-glossary-ok)
  "Translate TEXT from SOURCE-LANG into TARGET-LANG and execute CALLBACK.
If SOURCE-LANG is nil, use \"en\". If CALLBACK is nil, execute
`tlon-deepl-print-translation'. If NO-GLOSSARY-OK is non-nil, don't ask for
confirmation when no glossary is found.

Returns the translated text as a string."
  (interactive)
  (let* ((source-lang (or source-lang (tlon-select-language 'code 'babel "Source language: " t)))
	 (excluded-lang (list (tlon-lookup tlon-languages-properties :standard :code source-lang)))
	 (target-lang (or target-lang (tlon-select-language 'code 'babel "Target language: "
							    'require-match nil nil excluded-lang)))
	 (text (or text
		   (read-string "Text to translate: "
				(or (when (region-active-p)
				      (buffer-substring-no-properties (region-beginning) (region-end)))
				    (thing-at-point 'word))))))
    (setq tlon-deepl-text text
          tlon-deepl-source-language source-lang
          tlon-deepl-target-language target-lang)
    (tlon-deepl-request-wrapper 'translate callback no-glossary-ok)))

(defun tlon-deepl-print-translation ()
  "Print the translated text and copy it to the kill ring.
Returns the translated text as a string, or nil if parsing fails."
  (goto-char (point-min))
  (let (translation)
    (condition-case err
        (let* ((json-array-type 'list)
               (json-key-type 'string)
               (json-object-type 'alist)
               (json-data (json-read))
               (translations (alist-get "translations" json-data nil nil #'string=))
               (first-translation (car translations)))
          (when first-translation
            (setq translation (alist-get "text" first-translation nil nil #'string=))))
      (error (message "Error parsing DeepL JSON response: %s" err)
             nil))
    (when translation
      (kill-new translation)
      (message (concat "Copied to kill ring: "
		       (replace-regexp-in-string "%" "%%" translation))))
    translation))

(defun tlon-deepl-translate-encode (&optional no-glossary-ok)
  "Return a JSON representation of the text to be translated to language.
If NO-GLOSSARY-OK is non-nil, don't ask for confirmation when no glossary is
found or applicable.

Glossaries are only used if the source language is \"en\" and a matching
glossary for the target language exists in `tlon-deepl-glossaries'."
  (let* ((source-is-en (string= "en" tlon-deepl-source-language))
         (glossary-id (when source-is-en ; Only lookup if source is English
                        (tlon-lookup tlon-deepl-glossaries "glossary_id" "target_lang" tlon-deepl-target-language)))
         (text (vector tlon-deepl-text))
         (target-supports-glossary (member tlon-deepl-target-language tlon-deepl-supported-glossary-languages))
         (proceed nil))

    ;; Determine if we should proceed without a glossary or prompt the user
    (setq proceed
          (or glossary-id ; Glossary found and will be used
              no-glossary-ok ; Caller allows no glossary
              (not target-supports-glossary) ; Target language doesn't support glossaries anyway
              ;; Prompt needed: Glossary applicable but not found, or source not "en"
              (let ((prompt-message
                     (cond
                      ((not source-is-en) ; Source language issue
                       "Glossaries require English source language.")
                      (target-supports-glossary ; Source is EN, target supports, but lookup failed
                       (format "No \"en-%s\" glossary found in local cache." tlon-deepl-target-language))
                      (t ; Should not happen due to (not target-supports-glossary) check above
                       "Glossary not applicable."))))
                (y-or-n-p (concat prompt-message " Translate anyway?")))))
    (unless proceed
      (user-error "Aborted: Glossary required or confirmation denied"))
    (let ((json-encoding-pretty-print nil)
          (json-encoding-default-indentation "")
          (json-encoding-lisp-style-closings nil)
          (json-encoding-object-sort-predicate nil))
      (json-encode `(("text" . ,text)
                     ("source_lang" . ,tlon-deepl-source-language)
                     ("target_lang" . ,tlon-deepl-target-language)
                     ;; Include glossary_id only if it was found and source is "en"
                     ,@(when glossary-id `(("glossary_id" . ,glossary-id)))
		     ;; disabled because it fails to respect newlines
		     ("model_type" . ,tlon-deepl-model-type))))))

(defun tlon-deepl-get-language-glossary (language)
  "Return the glossary ID for LANGUAGE.
Looks up the glossary ID in `tlon-deepl-glossaries' based on the target
LANGUAGE. Returns nil if `tlon-deepl-source-language' is not \"en\"."
  (when (string= "en" tlon-deepl-source-language)
    (tlon-lookup tlon-deepl-glossaries "glossary_id" "target_lang" language)))

;;;;;; Tex

(declare-function tlon-bibliography-lookup "tlon-tex")
(declare-function ebib--get-key-at-point "ebib")
(declare-function ebib-extras-get-field "ebib-extras")
(declare-function bibtex-extras-get-key "bibtex-extras")
(declare-function bibtex-extras-get-field "bibtex-extras")
(defun tlon-deepl--get-abstract-context (&optional abstract key interactive-call-p)
  "Prepare context for abstract translation via DeepL.
Collect necessary information for translating bibliographic entry abstracts.
If INTERACTIVE-CALL-P is non-nil, determine KEY from context (entry at point in
Ebib or BibTeX modes). For non-interactive calls, KEY must be provided.

ABSTRACT, if provided, is used directly. Otherwise, the abstract is retrieved
based on KEY from the current entry or bibliography database.

Returns a list (key text source-lang-code) with all information needed for
translation, or nil if any required piece is missing."
  (let* ((key (or key
                  (if interactive-call-p
                      (pcase major-mode
                        ('ebib-entry-mode (ebib--get-key-at-point))
                        ('bibtex-mode (bibtex-extras-get-key))
                        (_ (user-error "Cannot determine key interactively in mode: %s" major-mode)))
                    (user-error "KEY argument must be provided when called non-interactively"))))
         (text (or abstract
                   (when key
                     (or (when (and interactive-call-p
				    (derived-mode-p 'ebib-entry-mode)
				    (string= key (ebib--get-key-at-point)))
                           (ebib-extras-get-field "abstract"))
                         (tlon-bibliography-lookup "=key=" key "abstract")))
                   (when interactive-call-p
                     (pcase major-mode
                       ('text-mode (buffer-string))
                       ('ebib-entry-mode (unless key (ebib-extras-get-field "abstract")))
                       ('bibtex-mode (unless key (bibtex-extras-get-field "abstract")))))))
         (source-lang-name (when key
                             (or (when (and interactive-call-p
					    (derived-mode-p 'ebib-entry-mode)
					    (string= key (ebib--get-key-at-point)))
                                   (ebib-extras-get-field "langid"))
                                 (tlon-bibliography-lookup "=key=" key "langid"))))
         (source-lang-code (when source-lang-name
                             (tlon-lookup tlon-languages-properties :code :name source-lang-name))))
    (when (and key text source-lang-code)
      (list key text source-lang-code))))

;;;###autoload
(defun tlon-deepl-translate-abstract (&optional abstract key langs interactive-call-p)
  "Translate the ABSTRACT of entry KEY into LANGS.
LANGS is a list of languages, such as `(\"spanish\" \"french\")'. If LANGS is
nil, use `tlon-project-target-languages'. INTERACTIVE-CALL-P indicates if the
function was called interactively."
  (interactive "P")
  (when-let ((context (tlon-deepl--get-abstract-context abstract key interactive-call-p)))
    (cl-destructuring-bind (key text source-lang-code) context
      (if interactive-call-p
          (tlon-deepl--translate-abstract-interactive key text source-lang-code)
        (tlon-deepl--translate-abstract-non-interactive key text source-lang-code langs)))))

(declare-function tlon-tex-remove-braces "tlon-tex")
(declare-function tlon-translate-abstract-callback "tlon-tex")
(defun tlon-deepl--translate-abstract-interactive (key text source-lang-code)
  "Handle interactive abstract translation for KEY, TEXT, SOURCE-LANG-CODE.
If a translation for the KEY into the selected target language already exists,
prompt the user for confirmation before overwriting."
  (let* ((excluded-lang (list (tlon-lookup tlon-languages-properties :standard :code source-lang-code)))
         (target-lang (tlon-select-language 'code 'babel "Target language: " 'require-match nil nil excluded-lang)))
    (when target-lang
      (let ((existing-translation (tlon-deepl--get-existing-translation key target-lang))
            (target-lang-name (tlon-lookup tlon-languages-properties :standard :code target-lang)))
        (if (and existing-translation
                 (not (y-or-n-p (format "Translation for %s into %s already exists. Retranslate?"
                                        key target-lang-name))))
            (message "Translation for %s into %s aborted by user." key target-lang-name)
          (progn
            (message "Initiating DeepL translation for %s -> %s (%s)" key target-lang-name source-lang-code)
            (tlon-deepl-translate (tlon-tex-remove-braces text) target-lang source-lang-code
                                  (lambda ()
                                    (tlon-translate-abstract-callback key target-lang 'overwrite))
                                  nil)))))))

(defvar tlon-project-target-languages)
(defun tlon-deepl--translate-abstract-non-interactive (key text source-lang-code langs)
  "Handle non-interactive abstract translation for KEY, TEXT, SOURCE-LANG-CODE.
LANGS is a list of languages, such as `(\"spanish\" \"french\")'. If LANGS is
nil, use `tlon-project-target-languages'."
  (message "Checking abstract translations for %s..." key)
  (let ((initiated-langs '()))
    (mapc (lambda (language)
            (let ((target-lang (tlon-lookup tlon-languages-properties :code :name language)))
              (unless (string= source-lang-code target-lang)
                (unless (tlon-deepl--get-existing-translation key target-lang)
                  (push language initiated-langs)
                  (message "Initiating DeepL translation for %s -> %s" key target-lang)
                  (tlon-deepl-translate (tlon-tex-remove-braces text) target-lang source-lang-code
                                        (lambda () (tlon-translate-abstract-callback key target-lang 'overwrite)))))))
          (or langs tlon-project-target-languages))
    (when initiated-langs
      (message "Finished initiating translations for abstract of `%s' into: %s"
               key (string-join (reverse initiated-langs) ", ")))))

(defvar tlon-file-fluid)
(defvar tlon-file-stable)
(declare-function tlon-tex-get-keys-in-file "tlon-tex")
;;;###autoload
(defun tlon-deepl-translate-missing-abstracts (&optional langs)
  "Translate abstracts for BibTeX entries missing translations into LANGS.
Iterate through all keys in `tlon-file-fluid' and `tlon-file-stable'. For each
key, check if abstract translations are missing for any language in LANGS.
LANGS is a list of language names, such as `(\"spanish\" \"french\")'. If LANGS
is nil, prompt the user to select languages using
`tlon-read-multiple-languages'. When a translation in a language is missing,
call `tlon-deepl-translate-abstract' for that key and the specific missing
languages."
  (interactive)
  (let ((target-languages (or langs (tlon-read-multiple-languages 'babel))))
    (unless target-languages
      (user-error "No target languages selected. Aborting."))
    ;; Proceed if target-languages is not nil
    (let* ((all-translations (tlon-read-abstract-translations))
           (keys (seq-uniq (append (tlon-tex-get-keys-in-file tlon-file-fluid)
                                   (tlon-tex-get-keys-in-file tlon-file-stable))))
           (total (length keys))
           (initiated-count 0)
           (processed 0))
      (message "Checking %d BibTeX entries for missing abstract translations..." total)
      (dolist (key keys)
        (setq processed (1+ processed))
        (let ((missing-langs '())) ; Moved let binding for missing-langs one level up
          (when-let* ((context (tlon-deepl--get-abstract-context nil key nil)) ; Get context non-interactively
                      (abstract (nth 1 context))
                      (source-lang-code (nth 2 context)))
            (progn ;; Explicit progn for when-let* body
              ;; `missing-langs` is now from the outer scope of this when-let*

              ;; Check each target language
              (dolist (target-lang-name target-languages)
                (let ((target-lang-code (tlon-lookup tlon-languages-properties :code :name target-lang-name)))
                  (let* ((key-entry (assoc key all-translations))
                         (translation-text nil)
                         (has-translation nil))
                    (when key-entry
                      (let ((lang-entry (assoc target-lang-code (cdr key-entry))))
                        (when lang-entry
                          (setq translation-text (cdr lang-entry)))))
                    (setq has-translation (and translation-text
                                               (stringp translation-text)
                                               (> (length (string-trim translation-text)) 0)))
                    (unless (or (string= source-lang-code target-lang-code) ; Skip source language
                                has-translation) ; Check if translation exists
                      (push target-lang-name missing-langs)))))) ; Add to list if missing

              ;; If any translations are missing, initiate them
              ;; This block is still inside when-let* and uses `abstract` from when-let*
              (when missing-langs
                (message "Processing key %s (missing: %s) (%d/%d)"
                         key (string-join (reverse missing-langs) ", ") processed total)
                (setq initiated-count (1+ initiated-count))
                ;; Call translate abstract non-interactively with the list of *missing* languages
                (tlon-deepl-translate-abstract abstract key (reverse missing-langs))))
            ) ; This closes the progn
          )) ; This closes when-let*, then let for missing-langs
    (message "Finished checking %d entries. Initiated translation for %d entries." total initiated-count)))

(declare-function tlon-read-abstract-translations "tlon-tex")
(defun tlon-deepl--get-existing-translation (key target-lang)
  "Check `tlon-file-abstract-translations' for existing translation.
Return the translation string for KEY and TARGET-LANG if found and non-empty,
otherwise return nil."
  (let* ((translations (tlon-read-abstract-translations)) ; Read the JSON data
         (key-entry (assoc key translations))
         translation)
    (when key-entry
      (let ((lang-entry (assoc target-lang (cdr key-entry))))
        (when lang-entry
          (setq translation (cdr lang-entry)))))
    ;; Return translation only if it's a non-empty string
    (if (and translation (stringp translation) (> (length (string-trim translation)) 0))
        translation
      nil)))

;;;;; Glossaries

;;;;;; Get glossary

;;;###autoload
(defun tlon-deepl-get-glossaries ()
  "Retrieve and display list of glossaries from the DeepL API."
  (interactive)
  (tlon-deepl-request-wrapper 'glossary-get))

(defun tlon-deepl-glossary-get-callback (&rest _)
  "Callback for `tlon-deepl-get-glossaries'.

Accepts and ignores any extra arguments so it can be safely called
even if the caller passes data."
  (goto-char (point-min))
  (let ((json-array-type 'list)
        (json-key-type 'string))
    (setq tlon-deepl-glossaries
          (alist-get "glossaries" (tlon-read-json) nil nil #'string=)))
  (message "Read glossaries from DeepL API."))

(tlon-deepl-get-glossaries)

;;;;;; Create glossary

;;;###autoload
(defun tlon-deepl-glossary-create (language)
  "Create a DeepL glossary for LANGUAGE."
  (interactive (list (tlon-select-language 'code 'babel)))
  (tlon-extract-glossary language 'deepl-api)
  (setq tlon-deepl-target-language language)
  (tlon-deepl-request-wrapper 'glossary-create))

;;;###autoload
(defun tlon-deepl-glossary-update (language)
  "Update a DeepL glossary for LANGUAGE by deleting and recreating it.

The heavy lifting is delegated to `tlon-deepl-glossary-delete' and
`tlon-deepl-glossary-create'."
  (interactive (list (tlon-select-language 'code 'babel)))
  (tlon-deepl-glossary-delete
   language
   (lambda ()
     (tlon-deepl-glossary-create language))))

(defun tlon-deepl-glossary-create-encode (&rest _)
  "Return a JSON representation of the glossary to be created."
  (let* ((extension "tsv")
	 (file (tlon-glossary-make-file (upcase tlon-deepl-target-language) extension))
	 (name (file-name-base file))
	 entries)
    (with-temp-buffer
      (insert-file-contents file) ; Assumes file is UTF-8
      ;; Pass the raw buffer string directly to json-encode.
      ;; json-encode handles JSON string escaping correctly.
      (setq entries (buffer-string))
      (json-encode `(("name" . ,name)
		     ("source_lang" . "en")
		     ("target_lang" . ,tlon-deepl-target-language)
		     ("entries" . ,entries)
		     ("entries_format" . ,extension))))))

(defun tlon-deepl-glossary-create-callback ()
  "Callback for `tlon-deepl-glossary-create'."
  (setq tlon-deepl-target-language nil)
  (goto-char (point-min))
  (let ((response (json-read)))
    (tlon-deepl-get-glossaries)
    (message "Response: %s" response)))

;;;;;; Model Type Selection

(defconst tlon-deepl--model-choices
  '(("Latency optimized" . "latency_optimized")
    ("Quality optimized" . "quality_optimized")
    ("Prefer quality optimized" . "prefer_quality_optimized"))
  "Alist of DeepL model display names and their API values.
See `tlon-deepl-model-type' for details.")

(defun tlon-deepl-model-type-reader (prompt _initval _arg)
  "PROMPT the user to select a DeepL model type.
Returns the selected model string value."
  (let* ((current-value tlon-deepl-model-type)
         (current-label (or (car (rassoc current-value tlon-deepl--model-choices))
                            (format "%s (Unknown)" current-value)))
         (prompt (format "%s (current: %s): " prompt current-label))
         (selection (completing-read prompt tlon-deepl--model-choices nil t)))
    (cdr (assoc selection tlon-deepl--model-choices))))

(defun tlon-deepl-model-type-formatter (value)
  "Formatter function to display the label for the DeepL model VALUE."
  (or (car (rassoc value tlon-deepl--model-choices))
      (format "%s (Unknown)" value)))

(transient-define-infix tlon-deepl-model-type-infix ()
  "Select the DeepL model type (`tlon-deepl-model-type')."
  :class 'transient-lisp-variable
  :variable 'tlon-deepl-model-type
  :reader 'tlon-deepl-model-type-reader
  :prompt "DeepL Model Type: ")

;;;;;; Delete glossary

;;;###autoload
(defun tlon-deepl-glossary-delete (&optional language-or-id callback)
  "Delete a DeepL glossary.
If called interactively, prompt the user to choose a glossary unless
LANGUAGE-OR-ID is supplied.  Programmatically, supply either a target
language code (≤ 3 chars, e.g., \"es\") or a glossary ID.

After successful deletion, execute CALLBACK (if non-nil)."
  (interactive)
  (let* ((id
          (cond
           ((not language-or-id) nil)
           ;; language code supplied
           ((and (stringp language-or-id)
		 (<= (length language-or-id) 3))
            (tlon-deepl-get-language-glossary language-or-id))
           ;; id supplied directly
           ((stringp language-or-id) language-or-id))))
    (let ((tlon-deepl--override-glossary-id id))
      (tlon-deepl-request-wrapper
       'glossary-delete
       (lambda ()
	 (tlon-deepl-glossary-delete-callback)
	 (when callback (funcall callback)))))))

(defun tlon-deepl-glossary-delete-formatter ()
  "URL formatter for `tlon-deepl-glossary-delete'."
  (concat tlon-deepl-url-prefix
	  "glossaries/"
	  (or tlon-deepl--override-glossary-id
	      (tlon-deepl-select-glossary))))

(defun tlon-deepl-glossary-delete-callback ()
  "Callback for `tlon-deepl-glossary-delete'."
  (tlon-deepl-get-glossaries)
  (message "Deleted glossary."))

;;;;; Menu

;;;###autoload (autoload 'tlon-deepl-menu "tlon-deepl" nil t)
(transient-define-prefix tlon-deepl-menu ()
  "DeepL menu."
  [["Translate"
    ("t" "Translate text" tlon-deepl-translate)
    ("a" "Translate current abstract" (lambda () (interactive) (tlon-deepl-translate-abstract nil nil nil t)))
    ("m" "Translate missing abstracts" tlon-deepl-translate-missing-abstracts)
    ("e" "Ediff translation" tlon-deepl-diff)]
   ["Glossaries"
    ("s" "Select" tlon-deepl-select-glossary)
    ("r" "Retrieve" tlon-deepl-get-glossaries)
    ("c" "Create" tlon-deepl-glossary-create)
    ("u" "Update" tlon-deepl-glossary-update)
    ("d" "Delete" tlon-deepl-glossary-delete)]
   ["Options"
    ("-m" "Model type" tlon-deepl-model-type-infix)]])

(provide 'tlon-deepl)
;;; tlon-deepl.el ends here

