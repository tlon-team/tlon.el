;;; tlon-translate.el --- Translate files -*- lexical-binding: t -*-

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

;; This module provides functionality for translating entire files using
;; different translation engines.

;;; Code:

(require 'tlon)
(require 'tlon-ai)
(require 'tlon-core)
(require 'tlon-counterpart)
(require 'tlon-deepl)
(require 'tlon-glossary)
(require 'transient)
(require 'magit)
(require 'tlon-yaml)
(require 'tlon-paragraphs)
(require 'cl-lib)

;;;; User options

(defgroup tlon-translate nil
  "File translation functionality for Tlön."
  :group 'tlon)

(defcustom tlon-translate-engine 'deepl
  "The translation engine to use for file translation."
  :group 'tlon-translate
  :type '(choice (const :tag "DeepL" deepl)))

(defcustom tlon-translate-revise-errors-model
  '("Gemini" . gemini-2.5-pro)
  "Model to use for spotting errors in translations.
See `tlon-translate-revise-errors'. The value is a cons cell whose car is the
backend and whose cdr is the model itself. See `gptel-extras-ai-models' for the
available options. If nil, use the default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-translate)

(defcustom tlon-translate-revise-flow-model
  '("Gemini" . gemini-2.5-pro)
  "Model to use for improving the flow of translations.
See `tlon-translate-revise-flow'. The value is a cons cell whose car is the
backend and whose cdr is the model itself. See `gptel-extras-ai-models' for the
available options. If nil, use the default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-translate)

(defcustom tlon-translate-revise-commit-changes nil
  "Whether to commit changes after an AI revision."
  :group 'tlon-translate
  :type 'boolean)

(defcustom tlon-translate-revise-chunk-size 10
  "Number of aligned paragraphs sent per AI revision request.
The AI will process the translation in batches of this many
paragraphs to avoid extremely large prompts."
  :group 'tlon-translate
  :type 'integer)

;;;; Variables

(defconst tlon-translate--engine-choices
  '(("DeepL" . deepl)
    ("AI" . ai))
  "Alist of translation engine display names and their symbols.")

(defconst tlon-translate-prompt-revise-prefix
  "The file `%1$s' contains a %2$s translation of the file `%3$s'. "
  "Prefix for translation revision prompts.")

(defconst tlon-translate-prompt-revise-suffix
  "Ignore the front matter section at the beginning of the article (delimited by ‘---’). Do a sentence by sentence revision. URLs and bibtex keys should appear exactly as they do in the original file. Once you are done comparing the two files and identifying the changes that should be made to the translation, write your changes to `%1$s' using the 'edit_file` tool."
  "Suffix for translation revision prompts.")

(defconst tlon-translate-revise-errors-prompt
  (concat tlon-translate-prompt-revise-prefix
	  "Your task is to read both carefully and try to spot errors in the translation: the code surrounding the translation may have been corrupted, there may be sentences and even paragraphs missing, the abbreviations may be used wrongly or inconsistently, etc. "
	  tlon-translate-prompt-revise-suffix)
  "Prompt for revising translation errors.")

(defconst tlon-translate-revise-flow-prompt
  (concat tlon-translate-prompt-revise-prefix
	  "Your task is to to read both carefully and try improve the translation for a better flow. "
	  tlon-translate-prompt-revise-suffix)
  "Prompt for improving translation flow.")

(defconst tlon-translate-glossary-prompt
  " You should always respect the terminology included in the glossary `%s'."
  "Prompt for glossary usage in translations.")

(defvar tlon-translate-source-language nil
  "Source language of the current API request.")

(defvar tlon-translate-target-language nil
  "Target language of the current API request.")

(defvar tlon-translate-text nil
  "The text to be translated in the current API request.")

;;;; Commands

;;;;; Translation

;;;;;; text

;;;###autoload
(defun tlon-translate-text (&optional text target-lang source-lang callback no-glossary-ok)
  "Translate TEXT from SOURCE-LANG into TARGET-LANG and execute CALLBACK.
If SOURCE-LANG is nil, use \"en\". If CALLBACK is nil, execute
`tlon-print-translation'. If NO-GLOSSARY-OK is non-nil, don't ask for
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
    (setq tlon-translate-text text
          tlon-translate-source-language source-lang
          tlon-translate-target-language target-lang)
    (pcase-exhaustive tlon-translate-engine
      ('deepl (tlon-deepl-request-wrapper 'translate callback no-glossary-ok))
      ;; TODO: develop this
      ;; ('ai)
      )))

;;;;;; file

(declare-function tlon-get-counterpart-in-originals "tlon-counterpart")
;;;###autoload
(defun tlon-translate-file (&optional file lang target-file)
  "Translate FILE into LANG using `tlon-translate-engine'.
If FILE is not provided, prompt for one, defaulting to the current buffer's
file. If LANG is not provided, prompt for a target language. TARGET-FILE is the
file where the translation will be saved. If nil, determine it based on the
source file and target language. If a counterpart already exists, prompt for
confirmation before overwriting it."
  (interactive
   (list (read-file-name "Translate file: " nil nil t
			 (file-relative-name (buffer-file-name) default-directory))
         (tlon-select-language 'code 'babel "Target language: " 'require-match)))
  (let* ((source-file file)
         (target-lang-code lang)
         (target-file
	  (or target-file
	      (let ((candidate (condition-case nil
				   (tlon-get-counterpart-in-originals source-file target-lang-code)
				 (error nil))))
		(cond
		 ((and candidate (file-exists-p candidate))
		  (if (y-or-n-p (format "Overwrite existing counterpart %s?" candidate))
		      candidate
		    (read-file-name "Save translation to: "
				    (file-name-directory candidate)
				    candidate)))
		 (candidate candidate)
		 (t
		  (let* ((counterpart-dir (tlon-get-counterpart-dir source-file target-lang-code))
			 (default-name (tlon-translate--default-filename source-file target-lang-code))
			 (default-path (when default-name (file-name-concat counterpart-dir default-name))))
		    (read-file-name "Save translation to: " counterpart-dir default-path))))))))
    (when target-file
      (tlon-translate--do-translate source-file target-file target-lang-code))))

(declare-function simple-extras-slugify "simple-extras")
(defun tlon-translate--default-filename (source-file target-lang-code)
  "Return a slugified filename for SOURCE-FILE translated into TARGET-LANG-CODE.
The slug is built by translating the original title with
`tlon-translate-text' and passing the result through
`simple-extras-slugify'.  If translation fails, fall back to a
slugified version of the original basename."
  (let* ((original-key (tlon-yaml-get-key "key" source-file))
	 (target-lang-name (tlon-lookup tlon-languages-properties
					:standard :code target-lang-code))
	 (translation-key (tlon-get-counterpart-key original-key target-lang-name))
         (title (or (tlon-bibliography-lookup "=key=" translation-key "title")
                    (replace-regexp-in-string "-" " " (file-name-base source-file))))
	 (source-lang-code (tlon-get-language-in-file source-file))
	 (translated (ignore-errors
                       (tlon-translate-text title
					    target-lang-code
					    source-lang-code
					    nil
					    t)))
	 (slug (simple-extras-slugify (or translated title)))
	 (ext  (file-name-extension source-file t)))
    (concat slug ext)))

(defun tlon-translate--do-translate (source-file target-file target-lang-code)
  "Translate SOURCE-FILE to TARGET-FILE into TARGET-LANG-CODE."
  (pcase tlon-translate-engine
    ('deepl
     (let* ((source-repo (tlon-get-repo-from-file source-file))
            (source-lang-code (tlon-repo-lookup :language :dir source-repo))
            (text (with-temp-buffer
                    (insert-file-contents source-file)
                    (buffer-string))))
       (tlon-deepl-translate text target-lang-code source-lang-code
                             (lambda ()
                               (let ((translated-text (tlon-translate--get-deepl-translation-from-buffer)))
                                 (when translated-text
                                   (with-temp-file target-file
                                     (insert translated-text))
                                   (message "Translated %s to %s" source-file target-file)
                                   (find-file target-file)))))))
    ;; TODO: add `ai' case; adapt `tlon-ai-translate-file' (and then remove from `tlon-ai.el')
    (_ (user-error "Unsupported translation engine: %s" tlon-translate-engine))))

(declare-function tlon-metadata-in-repo "tlon-yaml")
(defun tlon-translate--get-translation-from-original (original-file lang-code)
  "Get translation of ORIGINAL-FILE for LANG-CODE."
  (let* ((original-repo (tlon-get-repo-from-file original-file))
         (target-repo-dir
          (tlon-repo-lookup :dir
                            :subproject (tlon-repo-lookup :subproject :dir original-repo)
                            :language lang-code))
         (counterpart-file nil))
    (when target-repo-dir
      (setq counterpart-file
            (tlon-metadata-lookup (tlon-metadata-in-repo target-repo-dir)
                                  "file"
                                  "original_path"
                                  (file-relative-name original-file original-repo)))
      (unless counterpart-file
        (when-let* ((fallback-dir (tlon-get-counterpart-dir original-file lang-code))
                    (fallback-path (file-name-concat fallback-dir
                                                     (file-name-nondirectory original-file))))
          (setq counterpart-file fallback-path))))
    counterpart-file))

(defun tlon-translate--get-deepl-translation-from-buffer ()
  "Parse DeepL JSON response in current buffer and return translation."
  (goto-char (point-min))
  (when (re-search-forward "^{\\|\\[{" nil t)
    (goto-char (match-beginning 0)))
  (let* ((json-array-type 'list)
         (json-key-type 'string)
         (json-object-type 'alist)
         (json-data (json-read))
         (translations (alist-get "translations" json-data nil nil #'string=))
         (first-translation (car translations)))
    (when first-translation
      (alist-get "text" first-translation nil nil #'string=))))

(declare-function tlon-get-key-at-point "tlon-bib")
(declare-function tlon-yaml-insert-translated-tags "tlon-yaml")
;;;###autoload
(defun tlon-translate-current-file ()
  "Create a file with the translation of the bibtex key at point."
  (interactive)
  (unless (derived-mode-p 'bibtex-mode 'ebib-entry-mode 'ebib-index-mode)
    (user-error "This command should be run with point on the translation BibTeX entry"))
  (let* ((mode-ebib-p   (derived-mode-p 'ebib-entry-mode 'ebib-index-mode))
	 (get-field   (if mode-ebib-p #'ebib-extras-get-field #'bibtex-extras-get-field))
	 (orig-key (funcall get-field "translation")))
    (unless orig-key
      (user-error "This command should be run with point on the *translation* BibTeX entry, not its original"))
    (let* ((orig-file (tlon-counterpart--file-for-key orig-key "en"))
	   (target-language (funcall get-field "langid"))
	   (target-language-code (tlon-lookup tlon-languages-properties :code :name target-language))
	   (title (funcall get-field "title"))
	   (slug (simple-extras-slugify title))
	   (filename (file-name-with-extension slug "md"))
	   (target-file (file-name-concat (tlon-get-counterpart-dir orig-file target-language-code) filename))
	   (target-key (tlon-get-key-at-point)))
      (tlon-translate-file orig-file target-language-code target-file)
      (find-file target-file)
      (tlon-yaml-delete-metadata)
      (save-buffer)
      (tlon-yaml-insert-field "key" target-key)
      (tlon-yaml-insert-translated-tags)
      (save-buffer))))

;;;;;; abstract

;;;###autoload
(defun tlon-translate-abstract (&optional abstract key langs interactive-call-p)
  "Translate the ABSTRACT of entry KEY into LANGS.
LANGS is a list of languages, such as `(\"spanish\" \"french\")'. If LANGS is
nil, use `tlon-project-target-languages'. INTERACTIVE-CALL-P indicates if the
function was called interactively."
  (interactive "P")
  (when-let ((context (tlon-translate--get-abstract-context abstract key interactive-call-p)))
    (cl-destructuring-bind (key text source-lang-code) context
      (if interactive-call-p
          (tlon-translate-abstract-interactive key text source-lang-code)
        (tlon-translate-abstract-non-interactive key text source-lang-code langs)))))

(defvar tlon-file-fluid)
(defvar tlon-file-stable)
(declare-function tlon-bib-get-keys-in-file "tlon-bib")
;;;###autoload
(defun tlon-translate-missing-abstracts (&optional langs)
  "Translate abstracts for BibTeX entries missing translations into LANGS.
Iterate through all keys in `tlon-file-fluid' and `tlon-file-stable'. For each
key, check if abstract translations are missing for any language in LANGS.
LANGS is a list of language names, such as `(\"spanish\" \"french\")'. If LANGS
is nil, prompt the user to select languages using
`tlon-read-multiple-languages'. When a translation in a language is missing,
call `tlon-translate-abstract' for that key and the specific missing
languages."
  (interactive)
  (let ((target-languages (or langs (tlon-read-multiple-languages 'babel))))
    (unless target-languages
      (user-error "No target languages selected. Aborting"))
    (let* ((all-translations (tlon-read-abstract-translations))
           (keys (seq-uniq (append (tlon-bib-get-keys-in-file tlon-file-fluid)
                                   (tlon-bib-get-keys-in-file tlon-file-stable))))
           (total (length keys))
           (initiated-count 0)
           (processed 0))
      (message "Checking %d BibTeX entries for missing abstract translations..." total)
      (dolist (key keys)
        (setq processed (1+ processed))
        (let ((missing-langs '()))
          (when-let* ((context (tlon-translate--get-abstract-context nil key nil))
                      (abstract (nth 1 context))
                      (source-lang-code (nth 2 context)))
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
                  (unless (or (string= source-lang-code target-lang-code)
                              has-translation)
                    (push target-lang-name missing-langs)))))
            (when missing-langs
              (message "Processing key %s (missing: %s) (%d/%d)"
		       key (string-join (reverse missing-langs) ", ") processed total)
              (setq initiated-count (1+ initiated-count))
              (tlon-translate-abstract abstract key (reverse missing-langs))))))
      (message "Finished checking %d entries. Initiated translation for %d entries." total initiated-count))))

(declare-function tlon-bibliography-lookup "tlon-bib")
(declare-function ebib--get-key-at-point "ebib")
(declare-function ebib-extras-get-field "ebib-extras")
(declare-function bibtex-extras-get-key "bibtex-extras")
(declare-function bibtex-extras-get-field "bibtex-extras")
(defun tlon-translate--get-abstract-context (&optional abstract key interactive-call-p)
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

(declare-function tlon-read-abstract-translations "tlon-bib")
(defun tlon-translate--get-existing-abstract-translation (key target-lang)
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

(declare-function tlon-bib-remove-braces "tlon-bib")
(declare-function tlon-translate-abstract-callback "tlon-bib")
(defun tlon-translate-abstract-interactive (key text source-lang-code)
  "Handle interactive abstract translation for KEY, TEXT, SOURCE-LANG-CODE.
If a translation for the KEY into the selected target language already exists,
prompt the user for confirmation before overwriting."
  (let* ((excluded-lang (list (tlon-lookup tlon-languages-properties :standard :code source-lang-code)))
         (target-lang (tlon-select-language 'code 'babel "Target language: " 'require-match nil nil excluded-lang)))
    (when target-lang
      (let ((existing-translation (tlon-translate--get-existing-abstract-translation key target-lang))
            (target-lang-name (tlon-lookup tlon-languages-properties :standard :code target-lang)))
        (if (and existing-translation
                 (not (y-or-n-p (format "Translation for %s into %s already exists. Retranslate?"
                                        key target-lang-name))))
            (message "Translation for %s into %s aborted by user." key target-lang-name)
          (message "Initiating translation for %s -> %s (%s)" key target-lang-name source-lang-code)
          (tlon-translate-text (tlon-bib-remove-braces text) target-lang source-lang-code
                               (lambda ()
				 (tlon-translate-abstract-callback key target-lang 'overwrite))
                               nil))))))

(defvar tlon-project-target-languages)
(defun tlon-translate-abstract-non-interactive (key text source-lang-code langs)
  "Handle non-interactive abstract translation for KEY, TEXT, SOURCE-LANG-CODE.
LANGS is a list of languages, such as `(\"spanish\" \"french\")'. If LANGS is
nil, use `tlon-project-target-languages'."
  (message "Checking abstract translations for %s..." key)
  (let ((initiated-langs '()))
    (mapc (lambda (language)
            (let ((target-lang (tlon-lookup tlon-languages-properties :code :name language)))
              (unless (string= source-lang-code target-lang)
                (unless (tlon-translate--get-existing-abstract-translation key target-lang)
                  (push language initiated-langs)
                  (message "Initiating translation for %s -> %s" key target-lang)
                  (tlon-translate-text (tlon-bib-remove-braces text) target-lang source-lang-code
                                       (lambda () (tlon-translate-abstract-callback key target-lang 'overwrite)))))))
          (or langs tlon-project-target-languages))
    (when initiated-langs
      (message "Finished initiating translations for abstract of `%s' into: %s"
               key (string-join (reverse initiated-langs) ", ")))))

;;;;; Revision

;;;###autoload
(defun tlon-translate-revise-errors ()
  "Use AI to spot errors in a translation file."
  (interactive)
  (tlon-translate--revise-common 'errors))

;;;###autoload
(defun tlon-translate-revise-flow ()
  "Use AI to improve the flow of a translation file."
  (interactive)
  (tlon-translate--revise-common 'flow))

(declare-function gptel-context-add-file "gptel-context")
(declare-function gptel-context-remove-all "gptel-context")
(defun tlon-translate--revise-common (type)
  "Common function for revising a translation of TYPE.
TYPE can be `errors' or `flow'."
  (gptel-extras-warn-when-context)
  (let* ((translation-file (expand-file-name (read-file-name "Translation file: " (buffer-file-name))))
         (original-file (if-let* ((counterpart (tlon-get-counterpart translation-file)))
			    (expand-file-name counterpart)
			  (read-file-name "Original file: "))))
    (let* ((lang-code (tlon-get-language-in-file translation-file))
           (language (tlon-lookup tlon-languages-properties :standard :code lang-code))
           (prompt-template (pcase type
                              ('errors tlon-translate-revise-errors-prompt)
                              ('flow tlon-translate-revise-flow-prompt)))
           (model (pcase type
                    ('errors tlon-translate-revise-errors-model)
                    ('flow tlon-translate-revise-flow-model)))
           (tools '("edit_file" "apply_diff" "replace_file_contents"))
	   (prompt-elts (delq nil
			      (list prompt-template
				    (file-name-nondirectory translation-file)
				    language
				    (file-name-nondirectory original-file))))
	   (glossary-file (when (and (eq type 'flow)
				     (tlon-extract-glossary lang-code 'deepl-editor))
                            (tlon-glossary-target-path lang-code 'deepl-editor)))
	   (glossary-prompt (when glossary-file
			      (format tlon-translate-glossary-prompt (file-name-nondirectory glossary-file))))
           (prompt (concat (apply 'format prompt-elts) glossary-prompt)))
      ;; ensure paragraph alignment before proceeding
      (unless (tlon-paragraph-files-are-aligned-p translation-file original-file)
        (user-error "Files have different paragraph counts; align them first with `tlon-paragraphs-align-with-ai'"))

      (let* ((orig-paras  (tlon-with-paragraphs original-file))
             (trans-paras (tlon-with-paragraphs translation-file))
             (chunk-size  tlon-translate-revise-chunk-size)
             (total       (length orig-paras))
             (ranges '()))
        ;; build chunk ranges (START . END)
        (let ((i 0))
          (while (< i total)
            (push (cons i (min total (+ i chunk-size))) ranges)
            (setq i (+ i chunk-size))))
        (setq ranges (nreverse ranges))
        (when ranges
          (tlon-translate--revise-process-chunks
           ranges 0 translation-file original-file type prompt-template model
           lang-code language tools orig-paras trans-paras)
          (message "Requesting AI to revise %s in %d paragraph chunks..."
                   (file-name-nondirectory translation-file) (length ranges))))))

(declare-function magit-stage-files "magit-apply")
(defun tlon-translate--revise-process-chunks
    (ranges idx translation-file original-file type prompt-template model
            lang-code language tools orig-paras trans-paras)
  "Recursively send AI revision requests for paragraph RANGES.

RANGES is a list of cons cells (START . END) indicating paragraph
indices to process.  IDX is the zero-based index of the current
chunk."
  (when ranges
    (let* ((current (car ranges))
           (rest    (cdr ranges))
           (start   (car current))
           (end     (cdr current))
           (orig-chunk  (cl-subseq orig-paras start end))
           (trans-chunk (cl-subseq trans-paras start end))
           (comparison (tlon-paragraphs--get-comparison-buffer-content
                        translation-file original-file trans-chunk orig-chunk nil))
           (prompt (concat
                    (apply #'format (list prompt-template
                                          (file-name-nondirectory translation-file)
                                          language
                                          (file-name-nondirectory original-file)))
                    (format
                     "\n\nFocus ONLY on paragraphs %d–%d (of %d). Review the comparison below and edit the translation file accordingly:\n```\n%s\n```"
                     (1+ start) end (length orig-paras) comparison))))
      (gptel-context-add-file original-file)
      (gptel-context-add-file translation-file)
      (tlon-make-gptel-request
       prompt nil
       (lambda (response info)
         (tlon-translate--revise-callback response info translation-file type)
         (gptel-context-remove-all)
         (tlon-translate--revise-process-chunks
          rest (1+ idx) translation-file original-file type prompt-template model
          lang-code language tools orig-paras trans-paras))
       model t nil tools)))

(defun tlon-translate--revise-callback (response info file type)
  "Callback for AI revision.
RESPONSE is the AI's response. INFO is the response info. FILE is the file to
commit. TYPE is the revision type."
  (if (not response)
      (tlon-ai-callback-fail info)
    (message "AI agent finished revising %s." (file-name-nondirectory file))
    (when tlon-translate-revise-commit-changes
      (let ((default-directory (tlon-get-repo-from-file file)))
        (magit-stage-files (list file))
        (tlon-create-commit (format (pcase type
				      ('errors "Check errors in %s (AI)")
				      ('flow "Improve flow in %s (AI)"))
				    (tlon-get-key-from-file file))
			    file)))))

;;;;; Menu

(defun tlon-translate-engine-reader (prompt _initval _arg)
  "PROMPT the user to select a translation engine."
  (let* ((current-value tlon-translate-engine)
         (current-label (or (car (rassoc current-value tlon-translate--engine-choices))
                            (format "%s (Unknown)" current-value)))
         (prompt (format "%s (current: %s): " prompt current-label))
         (selection (completing-read prompt tlon-translate--engine-choices nil t)))
    (cdr (assoc selection tlon-translate--engine-choices))))

(transient-define-infix tlon-translate-engine-infix ()
  "Select the translation engine (`tlon-translate-engine')."
  :class 'transient-lisp-variable
  :variable 'tlon-translate-engine
  :reader 'tlon-translate-engine-reader
  :prompt "Translation Engine: ")

(transient-define-infix tlon-translate-infix-select-revise-errors-model ()
  "AI model to use for spotting errors in translations.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-translate-revise-errors-model)

(transient-define-infix tlon-translate-infix-select-revise-flow-model ()
  "AI model to use for improving the flow of translations.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-translate-revise-flow-model)

(transient-define-infix tlon-translate-infix-toggle-commit-changes ()
  "Toggle whether to commit changes after an AI revision."
  :class 'transient-lisp-variable
  :variable 'tlon-translate-revise-commit-changes
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-translate-revise-commit-changes)))

(transient-define-infix tlon-translate-infix-set-chunk-size ()
  "Set paragraph chunk size for AI revision."
  :class 'transient-lisp-variable
  :variable 'tlon-translate-revise-chunk-size
  :reader (lambda (_ _ _) (read-number "Chunk size (paragraphs): " tlon-translate-revise-chunk-size)))

;;;###autoload (autoload 'tlon-translate-menu "tlon-translate" nil t)
(transient-define-prefix tlon-translate-menu ()
  "`tlon-translate' menu."
  [["Translate"
    ("t t" "Translate text" tlon-translate-text)
    ("t f" "Translate file" tlon-translate-file)
    ("t a" "Translate current abstract" (lambda () (interactive) (tlon-translate-abstract nil nil nil t)))
    ("t A" "Translate missing abstracts" tlon-translate-missing-abstracts)
    ""
    "Options"
    ("t -t" "Translation engine" tlon-translate-engine-infix)
    ("t -d" "DeepL model" tlon-deepl-model-type-infix)]
   ["Revise"
    ("r e" "Spot errors" tlon-translate-revise-errors)
    ("r f" "Improve flow" tlon-translate-revise-flow)
    ""
    "Options"
    ("r -e" "Spot errors model" tlon-translate-infix-select-revise-errors-model)
    ("r -f" "Improve flow model" tlon-translate-infix-select-revise-flow-model)]
   ["General options"
    ("-c" "Commit changes" tlon-translate-infix-toggle-commit-changes)]])

(provide 'tlon-translate)
;;; tlon-translate.el ends here
