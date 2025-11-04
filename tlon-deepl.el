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

;; NOTE: I think the below is unnecessary (2025-07-04). Consider deleting.
;; (require 'tlon-ai) ; For tlon-simple-model-selection-infix
(require 'tlon-core)
(require 'tlon-glossary)
(require 'url)
(require 'seq) ; For seq-uniq

;;;; User options

(defgroup tlon-deepl nil
  "DeepL functionality for Tlön."
  :group 'tlon)

(defcustom tlon-deepl-model-type "quality_optimized"
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

(defvar tlon-deepl--override-glossary-id nil
  "When non-nil, force `tlon-deepl-glossary-delete-formatter' to use this ID.
This is a dynamically-bound helper set by `tlon-deepl-glossary-delete'
so the formatter can build the proper endpoint without re-prompting
the user.")

(defconst tlon-deepl-supported-glossary-languages
  '("ar" "da" "de" "en" "es" "fr" "it" "ja" "ko" "nb" "nl" "pl" "pt" "ro" "ru" "sv" "zh")
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

(defun tlon-deepl-request-wrapper (type &optional callback no-glossary)
  "Wrapper for API requests of TYPE.
If CALLBACK is nil, use the default callback for TYPE. If NO-GLOSSARY is
non-nil, don't ask for confirmation when no glossary is found."
  (cl-destructuring-bind (method url-suffix-or-fun default-callback &optional json)
      (alist-get type tlon-deepl-parameters)
    (let* ((callback (or callback default-callback))
           (url (if (functionp url-suffix-or-fun)
                    (funcall url-suffix-or-fun)
                  (concat tlon-deepl-url-prefix url-suffix-or-fun)))
           (payload (when json (funcall json no-glossary)))
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

(defvar tlon-translate-text)
(defvar tlon-translate-source-language)
(defvar tlon-translate-target-language)
;;;###autoload
(defun tlon-deepl-translate (&optional text target-lang source-lang callback no-glossary)
  "Translate TEXT from SOURCE-LANG into TARGET-LANG and execute CALLBACK.
If SOURCE-LANG is nil, use \"en\". If CALLBACK is nil, execute
`tlon-deepl-print-translation'. If NO-GLOSSARY is non-nil, don't ask for
confirmation when no glossary is found. If NO-GLOSSARY is the symbol `skip',
skip the translation and return nil without signaling an error.

Returns the translated text as a string, or nil if skipped."
  (interactive)
  (if (eq no-glossary 'skip)
      nil
    (let* ((source-lang (or source-lang (tlon-select-language 'code 'babel "Source language: " t)))
	   (excluded-lang (list (tlon-lookup tlon-languages-properties :standard :code source-lang)))
	   (target-lang (or target-lang (tlon-select-language 'code 'babel "Target language: "
							      'require-match nil nil excluded-lang)))
	   (text (or text
		     (read-string "Text to translate: "
				  (or (when (region-active-p)
					(buffer-substring-no-properties (region-beginning) (region-end)))
				      (thing-at-point 'word))))))
      (setq tlon-translate-text text
	    tlon-translate-source-language source-lang
	    tlon-translate-target-language target-lang)
      (tlon-deepl-request-wrapper 'translate callback no-glossary))))

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
            (setq translation (tlon-deepl--translation-text first-translation))))
      (error (message "Error parsing DeepL JSON response: %s" err)
             nil))
    (when translation
      (kill-new translation)
      (message (concat "Copied to kill ring: "
		       (replace-regexp-in-string "%" "%%" translation))))
    translation))

(defun tlon-deepl-translate-encode (&optional no-glossary)
  "Return a JSON representation of the text to be translated to language.
If NO-GLOSSARY is non-nil, don't ask for confirmation when no glossary is
found or applicable.

Glossaries are used when both source and target languages support
DeepL glossaries and a matching source/target glossary exists in
`tlon-deepl-glossaries'."
  (let* ((src tlon-translate-source-language)
         (tgt tlon-translate-target-language)
         (pair-supported (and (member src tlon-deepl-supported-glossary-languages)
                              (member tgt tlon-deepl-supported-glossary-languages)))
         (glossary-id (when pair-supported
                        (tlon-lookup tlon-deepl-glossaries
                                     "glossary_id"
                                     "source_lang" src
                                     "target_lang" tgt)))
         (processed-text (tlon-deepl--preprocess-text tlon-translate-text))
         (text (vector processed-text))
         (proceed nil))
    ;; Determine if we should proceed without a glossary or prompt the user
    (setq proceed
          (or glossary-id                      ; Glossary found and will be used
              no-glossary                      ; Caller allows no glossary
              (not pair-supported)             ; Pair doesn't support glossaries
              ;; Prompt needed: Glossary applicable but not found
              (y-or-n-p (format "No \"%s-%s\" glossary found in Deepl local cache. Translate anyway?"
                                src tgt))))
    (unless proceed
      (user-error "Aborted: Glossary required or confirmation denied"))
    (let ((json-encoding-pretty-print nil)
          (json-encoding-default-indentation "")
          (json-encoding-lisp-style-closings nil)
          (json-encoding-object-sort-predicate nil))
      (json-encode `(("text" . ,text)
                     ("source_lang" . ,src)
                     ("target_lang" . ,tgt)
                     ;; Include glossary_id only if it was found
                     ,@(when glossary-id `(("glossary_id" . ,glossary-id)))
                     ;; disabled because it fails to respect newlines
                     ("model_type" . ,tlon-deepl-model-type))))))

(defun tlon-deepl-get-language-glossary (language &optional source)
  "Return the glossary ID for LANGUAGE paired with SOURCE.
Looks up `tlon-deepl-glossaries' based on SOURCE (defaults to
`tlon-translate-source-language') and target LANGUAGE. Returns nil
if no matching glossary is found."
  (let ((src (or source tlon-translate-source-language)))
    (tlon-lookup tlon-deepl-glossaries
                 "glossary_id"
                 "source_lang" src
                 "target_lang" language)))

;;;;;; “quality_optimized” model newlines workaround

(defconst tlon-deepl-newline-token "Nihil igitur mors est ad nos."
  "Placeholder string used to preserve newlines when using \"quality_optimized\".
The string is replaced back to newlines after translation is received.")

(defun tlon-deepl--model-uses-newline-workaround-p ()
  "Return non-nil if the newline workaround must be applied."
  (string= tlon-deepl-model-type "quality_optimized"))

(defun tlon-deepl--preprocess-text (text)
  "Replace newlines in TEXT with `tlon-deepl-newline-token' when needed."
  (if (tlon-deepl--model-uses-newline-workaround-p)
      (replace-regexp-in-string "\n" tlon-deepl-newline-token text 'fixedcase 'literal)
    text))

(defun tlon-deepl--translation-text (translation-alist)
  "Extract and post-process the \"text\" field from TRANSLATION-ALIST.

This helper guarantees the newline workaround is applied
consistently, so callers do not need to invoke
`tlon-deepl--postprocess-text' directly."
  (when translation-alist
    (tlon-deepl--postprocess-text
     (alist-get "text" translation-alist nil nil #'string=))))

(defun tlon-deepl--postprocess-text (text)
  "Replace `tlon-deepl-newline-token' in TEXT with real newlines when needed."
  (if (tlon-deepl--model-uses-newline-workaround-p)
      (let ((pattern (concat (regexp-quote tlon-deepl-newline-token) " ?")))
	(replace-regexp-in-string pattern "\n" text 'fixedcase))
    text))

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
(defun tlon-deepl-glossary-create (source-language target-language)
  "Create a DeepL glossary for SOURCE-LANGUAGE → TARGET-LANGUAGE."
  (interactive (list (tlon-select-language 'code 'babel "Source language: ")
                     (tlon-select-language 'code 'babel "Target language: ")))
  (tlon-extract-glossary source-language target-language 'deepl-api)
  (setq tlon-translate-source-language source-language
        tlon-translate-target-language target-language)
  (tlon-deepl-request-wrapper 'glossary-create))

;;;###autoload
(defun tlon-deepl-glossary-update (source-language target-language)
  "Update a DeepL glossary for SOURCE-LANGUAGE → TARGET-LANGUAGE.

The heavy lifting is delegated to `tlon-deepl-glossary-delete' and
`tlon-deepl-glossary-create'."
  (interactive (list (tlon-select-language 'code 'babel "Source language: ")
                     (tlon-select-language 'code 'babel "Target language: ")))
  (tlon-deepl-glossary-delete
   (cons source-language target-language)
   (lambda ()
     (tlon-deepl-glossary-create source-language target-language))))

;;;###autoload
(defun tlon-deepl-maybe-glossary-update (language-or-pair)
  "Update a DeepL glossary when supported.
If LANGUAGE-OR-PAIR is a cons (SRC . TGT), update that pair when both
languages support glossaries. If it is a string code, update EN → that
language for backward compatibility."
  (let* ((src (if (consp language-or-pair) (car language-or-pair) "en"))
         (tgt (if (consp language-or-pair) (cdr language-or-pair) language-or-pair)))
    (when (and (member src tlon-deepl-supported-glossary-languages)
               (member tgt tlon-deepl-supported-glossary-languages))
      (tlon-deepl-glossary-update src tgt))))

(defun tlon-deepl-glossary-create-encode (&rest _)
  "Return a JSON representation of the glossary to be created."
  (let* ((extension "tsv")
         (file (tlon-glossary-make-file tlon-translate-source-language
                                        tlon-translate-target-language
                                        extension))
         (name (file-name-base file))
         entries)
    (with-temp-buffer
      (insert-file-contents file) ; Assumes file is UTF-8
      ;; Pass the raw buffer string directly to json-encode.
      ;; json-encode handles JSON string escaping correctly.
      (setq entries (buffer-string))
      (json-encode `(("name" . ,name)
                     ("source_lang" . ,tlon-translate-source-language)
                     ("target_lang" . ,tlon-translate-target-language)
                     ("entries" . ,entries)
                     ("entries_format" . ,extension))))))

(defun tlon-deepl-glossary-create-callback ()
  "Callback for `tlon-deepl-glossary-create'."
  (setq tlon-translate-target-language nil)
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
LANGUAGE-OR-ID is supplied. Programmatically, supply either:
- a target language code (≤ 3 chars, e.g., \"es\"),
- a cons pair (SOURCE . TARGET), or
- a glossary ID string.

After successful deletion, execute CALLBACK (if non-nil)."
  (interactive)
  (let* ((id
          (cond
           ((not language-or-id) nil)
           ;; pair supplied (SOURCE . TARGET)
           ((consp language-or-id)
            (tlon-deepl-get-language-glossary (cdr language-or-id) (car language-or-id)))
           ;; language code supplied (target only, source defaults to current)
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

