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

(defcustom tlon-translate-revise-chunk-size 5
  "Number of aligned paragraphs sent per AI revision request.
The AI will process the translation in batches of this many
paragraphs to avoid extremely large prompts."
  :group 'tlon-translate
  :type 'integer)

(defcustom tlon-translate-revise-max-parallel 3
  "Maximum number of paragraph-chunk revision requests to run in parallel.
If the number of chunks to process is less than or equal to this
value, the requests are dispatched concurrently; otherwise they
are processed sequentially."
  :group 'tlon-translate
  :type 'integer)

(defcustom tlon-translate-revise-stream nil
  "If non-nil, enable streaming when sending AI revision requests.

With streaming disabled (the default), `gptel' calls the callback exactly
once per chunk, after the model and any tools have finished.  This makes it
trivial to detect completion and schedule the next chunk.  Set this option
to t if you prefer to see partial responses in real time."
  :group 'tlon-translate
  :type 'boolean)

;;;; Variables

(defconst tlon-translate--engine-choices
  '(("DeepL" . deepl)
    ("AI" . ai))
  "Alist of translation engine display names and their symbols.")

(defconst tlon-translate-prompt-revise-prefix
  "I am sharing with you a series of paragraph pairs. Each pair consists of an original paragraph in English and a translation of it into %2$s. "
  "Prefix for translation revision prompts.")

(defconst tlon-translate-prompt-revise-suffix
  "Do not modify URLs, BibTeX keys, or tags enclosed in angular brackets (such as ‘<Roman>’, ‘<LiteralLink>’, etc.). Only after you are done comparing the paragraphs and determining all the changes that should be made to the translation, write your changes to \"%1$s\" using the 'edit_file` tool. You should use this tool ONLY ONCE, to make all your changes in one go. Note that the file (\"%1$s\") may include other paragraphs besides those I shared with you; you should only modify the paragraphs that I shared with you, leaving the rest of the file unchanged."
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
    (unless (tlon-paragraph-files-are-aligned-p translation-file original-file)
      (user-error "Files have different paragraph counts; align them first with `tlon-paragraphs-align-with-ai'"))
    (let* ((lang-code (tlon-get-language-in-file translation-file))
           (language (tlon-lookup tlon-languages-properties :standard :code lang-code))
           (model (pcase type
                    ('errors tlon-translate-revise-errors-model)
                    ('flow tlon-translate-revise-flow-model)))
           (tools '("edit_file" "apply_diff" "replace_file_contents"))
           (prompt-template (pcase type
                              ('errors tlon-translate-revise-errors-prompt)
                              ('flow tlon-translate-revise-flow-prompt)))
	   (prompt-elts (delq nil (list prompt-template translation-file (capitalize language))))
	   (glossary-file (when (and (eq type 'flow)
				     (tlon-extract-glossary lang-code 'deepl-editor))
			    (tlon-glossary-target-path lang-code 'deepl-editor)))
	   (glossary-prompt (when glossary-file
			      (format tlon-translate-glossary-prompt (file-name-nondirectory glossary-file))))
	   (prompt (concat (apply 'format prompt-elts) glossary-prompt))
	   (orig-paras  (tlon-with-paragraphs original-file))
           (trans-paras (tlon-with-paragraphs translation-file))
           (chunk-size  tlon-translate-revise-chunk-size)
           (total       (length orig-paras))
           (ranges '()))
      (setq ranges (tlon-translate--build-chunk-ranges total chunk-size))
      (when ranges
	(if (<= (length ranges) tlon-translate-revise-max-parallel)
	    ;; Few enough chunks → process them all in parallel.
	    (tlon-translate--revise-parallel
	     ranges translation-file original-file type prompt model
	     tools orig-paras trans-paras)
	  ;; More chunks than the parallel cap → process in parallel *batches*
	  ;; of `tlon-translate-revise-max-parallel'.
	  (tlon-translate--revise-parallel-batches
	   ranges translation-file original-file type prompt model
	   tools orig-paras trans-paras))))))

(defun tlon-translate--build-chunk-ranges (total chunk-size)
  "Build chunk ranges (START . END) for TOTAL items with CHUNK-SIZE."
  (let ((ranges '())
        (i 0))
    (while (< i total)
      (push (cons i (min total (+ i chunk-size))) ranges)
      (setq i (+ i chunk-size)))
    (nreverse ranges)))

;; MAYBE: support parallel requests.
(defun tlon-translate--revise-parallel (ranges translation-file original-file type prompt model tools orig-paras trans-paras)
  "Use parallel processing to revise translation in RANGES.
RANGES is a list of ranges to revise. TRANSLATION-FILE is the path to the
translation file. ORIGINAL-FILE is the path to the original file. TYPE is the
type of revision. PROMPT is the prompt to use for revision. MODEL is the AI
model to use. TOOLS are the tools available for the revision. ORIG-PARAS are the
original paragraphs. TRANS-PARAS are the translated paragraphs."
  (cl-loop for r in ranges
	   for idx from 0
	   do (tlon-translate--revise-send-range
	       r translation-file original-file type
	       prompt model tools
	       orig-paras trans-paras))
  (tlon-translate--message-revise-request translation-file ranges t))

;; -------------------------------------------------------------------
;; Batched parallel processing
;; -------------------------------------------------------------------
;; Fire at most `tlon-translate-revise-max-parallel' requests at once.
;; When the last request in a batch finishes we launch the next batch,
;; until RANGES is exhausted.
(defun tlon-translate--revise-parallel-batches
    (ranges translation-file original-file type prompt model
            tools orig-paras trans-paras)
  "Process RANGES in parallel batches of `tlon-translate-revise-max-parallel'."
  (cl-labels
      ((process (remaining idx)
         (if (null remaining)
             ;; All work done – final message.
             (message "All %d chunk%s processed for %s."
                      idx (if (= idx 1) "" "s")
                      (file-name-nondirectory translation-file))
           (let* ((batch (cl-subseq remaining
                                    0 (min tlon-translate-revise-max-parallel
                                           (length remaining))))
                  (rest  (nthcdr (length batch) remaining))
                  (pending (length batch)))
             (dolist (r batch)
               (tlon-translate--revise-send-range
                r translation-file original-file type prompt model
                tools orig-paras trans-paras
                (lambda ()
                  (setq pending (1- pending))
                  (when (= pending 0)
                    ;; Batch finished – launch next one.
                    (process rest (+ idx (length batch)))))))))))
    (process ranges 0)))

(defun tlon-translate--revise-sequential (ranges translation-file original-file type prompt model lang-code language tools orig-paras trans-paras)
  "Use sequential processing to revise translation in RANGES.
RANGES is a list of ranges to revise. TRANSLATION-FILE is the path to the
translation file. ORIGINAL-FILE is the path to the original file. TYPE is the
type of revision. PROMPT is the prompt to use for revision. MODEL is the AI
model to use. LANG-CODE is the language code for the translation. LANGUAGE is
the target language name. TOOLS are the tools available for the revision.
ORIG-PARAS are the original paragraphs. TRANS-PARAS are the translated
paragraphs."
  ;; Process chunks back-to-front so edits inserted by earlier chunks
  ;; do not shift the paragraph positions of the remaining ones.
  (let ((rev-ranges (reverse ranges)))
    (tlon-translate--revise-process-chunks
     rev-ranges 0 translation-file original-file type prompt model
     lang-code language tools orig-paras trans-paras)
    (tlon-translate--message-revise-request translation-file rev-ranges nil)))

(defun tlon-translate--message-revise-request (translation-file ranges parallel-p)
  "Display message about AI revision request for TRANSLATION-FILE with RANGES.
PARALLEL-P indicates whether processing is parallel or sequential."
  (message "Requesting AI to revise %s in %d %s chunks..."
	   (file-name-nondirectory translation-file)
	   (length ranges)
	   (if parallel-p "parallel" "paragraph")))

;;;;;; chunk helpers

(defun tlon-translate--gptel-callback-simple (translation-file type start end after-fn)
  "Return a gptel callback suitable for sequential chunk processing.
TRANSLATION-FILE is the file path where the translation will be stored. TYPE
specifies the type of translation operation being performed. START is the
starting position in the buffer for the translation chunk. END is the ending
position in the buffer for the translation chunk. AFTER-FN is a function to call
after the translation request is finished, or nil if no post-processing is
needed."
  (let ((fired nil))
    (lambda (response info)
      ;; Forward text fragments (or the sole final text) to the real handler.
      (when (stringp response)
        (tlon-translate--revise-callback
         response info translation-file type start end))
      ;; Surface errors so the user sees them, but still advance.
      (when (and (not (stringp response))
                 (or (plist-get info :error)
                     (let ((st (plist-get info :status)))
                       (and (numberp st) (>= st 400)))))
        (tlon-ai-callback-fail info))
      ;; Decide when the request is *finished* and fire AFTER-FN once.
      (when (and (not fired)
                 (or
                  ;; non-streaming mode → single call means we're done
                  (not tlon-translate-revise-stream)
                  ;; explicit done/final flags (plist or alist)
                  (plist-get info :done) (alist-get 'done info)
                  (plist-get info :final) (alist-get 'final info)
                  ;; INFO may be nil in some backends
                  (null info)))
        (setq fired t)
        (when (functionp after-fn)
          (funcall after-fn))))))

;; -------------------------------------------------------------------
;; Helper: kill transient indirect buffers
;; -------------------------------------------------------------------
(defun tlon-translate--kill-indirect-buffers-of-file (file)
  "Kill all indirect buffers whose base buffer visits FILE.
AI-revision helpers create temporary indirect buffers to build the
paragraph comparison.  They are no longer needed once the prompt has
been constructed and would otherwise accumulate."
  (dolist (buf (buffer-list))
    (let ((base (buffer-base-buffer buf)))
      (when (and base
                 (eq base (get-file-buffer file)))
        (kill-buffer buf)))))

(defun tlon-translate--revise-send-range
    (range translation-file original-file type prompt-template model
           tools orig-paras trans-paras &optional after-fn)
  "Send an AI revision request for a single paragraph RANGE.
RANGE is a cons cell (START . END) specifying the paragraph range to revise. IDX
is the chunk index and is only used for debugging/logging. TRANSLATION-FILE is
the path to the translation file being revised. ORIGINAL-FILE is the path to the
original source file. TYPE specifies the type of revision being performed.
PROMPT-TEMPLATE is the template string for constructing the AI prompt. MODEL
specifies which AI model to use for the revision request. LANG-CODE is the
language code for the translation (currently unused). TOOLS specifies AI tools
to be used in the request. ORIG-PARAS is a list of original paragraphs from the
source text. TRANS-PARAS is a list of translated paragraphs to be revised.
AFTER-FN is an optional function to call after the revision is complete."
  (let* ((start (car range))
         (end   (cdr range))
         (orig-chunk  (cl-subseq orig-paras start end))
         (trans-chunk (cl-subseq trans-paras start end))
         (comparison (tlon-paragraphs--get-comparison-buffer-content
                      translation-file original-file trans-chunk orig-chunk nil))
         (prompt (tlon-ai-maybe-edit-prompt (concat
					     prompt-template
					     comparison))))
    ;; Clean up the transient indirect buffers created for COMPARISON.
    (tlon-translate--kill-indirect-buffers-of-file translation-file)
    (tlon-translate--kill-indirect-buffers-of-file original-file)
    (tlon-make-gptel-request
     prompt nil
     (tlon-translate--gptel-callback-simple
      translation-file type start end after-fn)
     model tlon-translate-revise-stream nil tools)))

(defun tlon-translate--revise-process-chunks
    (ranges idx translation-file original-file type prompt-template model
            lang-code language tools orig-paras trans-paras)
  "Recursively send AI revision requests for paragraph RANGES.
RANGES is a list of cons cells (START . END) specifying paragraph ranges to
revise. IDX is the current chunk index used for debugging/logging.
TRANSLATION-FILE is the path to the translation file being revised.
ORIGINAL-FILE is the path to the original source file. TYPE specifies the type
of revision being performed. PROMPT-TEMPLATE is the template string for
constructing AI prompts. MODEL specifies which AI model to use for revision
requests. LANG-CODE is the language code for the translation. LANGUAGE is the
target language name. TOOLS specifies AI tools to be used in requests.
ORIG-PARAS is a list of original paragraphs from the source text. TRANS-PARAS is
a list of translated paragraphs to be revised."
  (if ranges
      (let* ((current (car ranges))
             (rest    (cdr ranges)))
        (tlon-translate--revise-send-range
         current translation-file original-file type prompt-template model
         tools orig-paras trans-paras
         (lambda ()
           (tlon-translate--revise-process-chunks
            rest (1+ idx) translation-file original-file type prompt-template model
            lang-code language tools orig-paras trans-paras))))
    ;; No ranges left → all chunks done.
    (message "All %d chunk%s processed for %s."
             idx (if (= idx 1) "" "s")
             (file-name-nondirectory translation-file))))

(declare-function magit-stage-files "magit-apply")
(defun tlon-translate--revise-callback (response info file type start end)
  "Callback for AI revision.
RESPONSE is the AI's response. INFO is the response info. FILE is the file to
commit. TYPE is the revision type. START is the starting paragraph number.
END is the ending paragraph number."
  ;; Only treat this as a failure when INFO actually contains an error
  ;; or a HTTP status ≥ 400.  `gptel' often makes a final callback with
  ;; RESPONSE=nil and no error info – that should *not* be reported as
  ;; a failure.
  (cond
   ((or (plist-get info :error)
        (let ((st (plist-get info :status)))
          (and (numberp st) (>= st 400))))
    (tlon-ai-callback-fail info))
   (response
    (run-at-time
     0 nil
     (lambda (f)
       (when-let ((buf (get-file-buffer f)))
         (with-current-buffer buf
           ;; Don’t clobber user edits – revert only if unchanged.
           (unless (buffer-modified-p)
             (revert-buffer :ignore-auto :noconfirm)))))
     file)
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

(transient-define-infix tlon-translate-infix-set-max-parallel ()
  "Set maximum number of parallel revision requests."
  :class 'transient-lisp-variable
  :variable 'tlon-translate-revise-max-parallel
  :reader (lambda (_ _ _) (read-number "Max parallel requests: " tlon-translate-revise-max-parallel)))

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
    ("r -f" "Improve flow model" tlon-translate-infix-select-revise-flow-model)
    ("r -c" "Chunk size" tlon-translate-infix-set-chunk-size)
    ("r -p" "Max parallel" tlon-translate-infix-set-max-parallel)]
   ["General options"
    ("-e" "edit prompt"                               tlon-ai-infix-toggle-edit-prompt)
    ("-d" "debug"                                     tlon-menu-infix-toggle-debug)
    ("-c" "Commit changes" tlon-translate-infix-toggle-commit-changes)]])

(provide 'tlon-translate)
;;; tlon-translate.el ends here
