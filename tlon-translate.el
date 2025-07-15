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

;;;; User options

(defgroup tlon-translate nil
  "File translation functionality for Tlön."
  :group 'tlon)

(defcustom tlon-translate-engine 'deepl
  "The translation engine to use for file translation."
  :group 'tlon-translate
  :type '(choice (const :tag "DeepL" deepl)))

(defcustom tlon-translate-revise-errors-model
  '("Gemini" . gemini-2.5-pro-preview-06-05)
  "Model to use for spotting errors in translations.
See `tlon-translate-revise-errors'. The value is a cons cell whose car is the
backend and whose cdr is the model itself. See `gptel-extras-ai-models' for the
available options. If nil, use the default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-translate)

(defcustom tlon-translate-revise-flow-model
  '("Gemini" . gemini-2.5-pro-preview-06-05)
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

;;;; Variables

(defconst tlon-translate--engine-choices
  '(("DeepL" . deepl))
  "Alist of translation engine display names and their symbols.")

(defconst tlon-translate-prompt-revise-prefix
  "The file `%1$s' contains a %2$s translation of the file `%3$s'. "
  "Prefix for translation revision prompts.")

(defconst tlon-translate-prompt-revise-suffix
  "Do a sentence by sentence revision. Once you are done comparing the two files and identifying the changes that should be made to the translation, write your changes to `%1$s' using the 'edit_file` tool."
  "Suffix for translation revision prompts.")

(defconst tlon-translate-revise-errors-prompt
  (concat tlon-translate-prompt-revise-prefix
	  "Your task is to read both carefully and try to spot errors in the translation: the code surrounding the translation may have been corrupted, there may be sentences and even paragraphs missing, the abbreviations may be used wrongly or inconsistently, etc. "
	  tlon-translate-prompt-revise-suffix)
  "Prompt for revising translation errors.")

(defconst tlon-translate-revise-flow-prompt
  (concat tlon-translate-prompt-revise-prefix
	  "Your task is to to read both carefully and try improve the translation for a better flow. You should always respect the terminology included in the glossary `%4$s'. "
	  tlon-translate-prompt-revise-suffix)
  "Prompt for improving translation flow.")

;;;; Commands

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
           (tools '("edit_file"))
           (glossary-file (when (eq type 'flow)
                            (tlon-extract-glossary lang-code 'deepl-editor)
                            (tlon-glossary-target-path lang-code 'deepl-editor)))
	   (prompt-elts (delq nil
			      (list prompt-template
				    (file-name-nondirectory translation-file)
				    language
				    (file-name-nondirectory original-file)
				    (when glossary-file
				      glossary-file))))
           (prompt (apply 'format prompt-elts)))
      (gptel-context-add-file original-file)
      (gptel-context-add-file translation-file)
      (when glossary-file
	(gptel-context-add-file glossary-file))
      (message "Requesting AI to revise %s..." (file-name-nondirectory translation-file))
      (tlon-make-gptel-request prompt nil
                               (lambda (response info)
				 (tlon-translate--revise-callback response info translation-file type))
                               model t nil tools)
      (gptel-context-remove-all))))

(declare-function magit-stage-files "magit-apply")
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
        (tlon-create-commit (format "AI: Revise (%s)" (symbol-name type)) file)))))

;;;###autoload
(defun tlon-translate-file (&optional file lang)
  "Translate FILE into LANG using `tlon-translate-engine'.
If FILE is not provided, prompt for one, defaulting to the current buffer's
file. If LANG is not provided, prompt for a target language."
  (interactive
   (list (read-file-name "Translate file: " nil nil t
			 (file-relative-name (buffer-file-name) default-directory))
         (tlon-select-language 'code 'babel "Target language: " 'require-match)))
  (let* ((source-file file)
         (target-lang-code lang)
         (target-file
          (let ((candidate (tlon-translate--get-counterpart-for-language source-file target-lang-code)))
            (if (and candidate (file-exists-p candidate))
                (if (y-or-n-p (format "Overwrite existing counterpart %s?" candidate))
                    candidate
                  (read-file-name "Save translation to: " (file-name-directory candidate)))
              (or candidate
                  (let ((counterpart-dir (tlon-get-counterpart-dir source-file target-lang-code)))
                    (read-file-name "Save translation to: " counterpart-dir)))))))
    (when target-file
      (tlon-translate--do-translate source-file target-file target-lang-code))))

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
                                   (find-file target-file))))
                             t)))
    (_ (user-error "Unsupported translation engine: %s" tlon-translate-engine))))

(defun tlon-translate--get-counterpart-for-language (file lang-code)
  "Return the counterpart of FILE for LANG-CODE."
  (let* ((repo (tlon-get-repo-from-file file))
         (subtype (tlon-repo-lookup :subtype :dir repo)))
    (pcase subtype
      ('originals
       (tlon-translate--get-translation-from-original file lang-code))
      ('translations
       (if (string= (tlon-repo-lookup :language :dir repo) lang-code)
           file
         (when-let ((original-file (tlon-get-counterpart-in-translations file)))
           (tlon-translate--get-translation-from-original original-file lang-code)))))))

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

;;;;; Menu

;;;###autoload (autoload 'tlon-translate-menu "tlon-translate" nil t)
(transient-define-prefix tlon-translate-menu ()
  "`tlon-translate' menu."
  [["Translate"
    ("f" "Translate file" tlon-translate-file)]
   ["Revise"
    ("e" "Spot errors" tlon-translate-revise-errors)
    ("f" "Improve flow" tlon-translate-revise-flow)]
   ["Translation options"
    ("-t" "Translation engine" tlon-translate-engine-infix)
    ("-d" "DeepL model" tlon-deepl-model-type-infix)
    ""
    "Revision options"
    ("-e" "Spot errors AI model" tlon-translate-infix-select-revise-errors-model)
    ("-f" "Improve flow AI model" tlon-translate-infix-select-revise-flow-model)
    ""
    "General options"
    ("-c" "Commit changes" tlon-translate-infix-toggle-commit-changes)]])

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
  :class 'tlon-ai-model-selection-infix
  :variable 'tlon-translate-revise-errors-model)

(transient-define-infix tlon-translate-infix-select-revise-flow-model ()
  "AI model to use for improving the flow of translations.
If nil, use the default model."
  :class 'tlon-ai-model-selection-infix
  :variable 'tlon-translate-revise-flow-model)

(transient-define-infix tlon-translate-infix-toggle-commit-changes ()
  "Toggle whether to commit changes after an AI revision."
  :class 'transient-lisp-variable
  :variable 'tlon-translate-revise-commit-changes
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-translate-revise-commit-changes)))

(provide 'tlon-translate)
;;; tlon-translate.el ends here
