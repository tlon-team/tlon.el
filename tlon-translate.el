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
(require 'bibtex)
(require 'citar-cache)
(require 'subr-x)

;;;; User options

(defgroup tlon-translate nil
  "File translation functionality for Tlön."
  :group 'tlon)

(defcustom tlon-translate-engine 'deepl
  "The translation engine to use for file translation."
  :group 'tlon-translate
  :type '(choice (const :tag "DeepL" deepl)))

(defcustom tlon-translate-spot-errors-model
  '("Gemini" . gemini-2.5-flash)
  "Model to use for spotting errors in translations.
See `tlon-translate-spot-errors'. The value is a cons cell whose car is the
backend and whose cdr is the model itself. See `gptel-extras-ai-models' for the
available options. If nil, use the default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-translate)

(defcustom tlon-translate-improve-flow-model
  '("Gemini" . gemini-2.5-pro)
  "Model to use for improving the flow of translations.
See `tlon-translate-improve-flow'. The value is a cons cell whose car is the
backend and whose cdr is the model itself. See `gptel-extras-ai-models' for the
available options. If nil, use the default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-translate)

(defcustom tlon-translate-revise-commit-changes nil
  "Whether to commit changes after an AI revision."
  :group 'tlon-translate
  :type 'boolean)

(defcustom tlon-translate-restrict-revision-to-paragraphs nil
  "If non-nil, restrict AI revision to an inclusive paragraph range.

The value must be a cons cell (START . END) where START and END
are 1-based paragraph numbers.  When nil (the default), the entire
file is sent to the AI for revision."
  :group 'tlon-translate
  :type '(choice (const :tag "Whole file" nil)
                 (cons (integer :tag "Start paragraph")
                       (integer :tag "End paragraph"))))

(defcustom tlon-translate-revise-chunk-size 1
  "Number of aligned paragraphs sent per AI revision request.
The AI will process the translation in batches of this many
paragraphs to avoid extremely large prompts."
  :group 'tlon-translate
  :type 'integer)

(defcustom tlon-translate-revise-max-parallel 1
  "Maximum number of paragraph-chunk revision requests to run in parallel.
If the number of chunks to process is less than or equal to this
value, the requests are dispatched concurrently; otherwise they
are processed sequentially."
  :group 'tlon-translate
  :type 'integer)

;;;; Logging

(defconst tlon-translate-log-buffer-name "*tlon-translate-log*"
  "Name of the buffer that collects tlon-translate progress logs.")

(defun tlon-translate--log (format-string &rest args)
  "Append formatted message to translation log buffer and echo it.

FORMAT-STRING and ARGS are like in `format'.  The resulting
message is appended to the buffer named by
`tlon-translate-log-buffer-name' and also shown via `message'."
  (let ((msg (apply #'format format-string args)))
    (with-current-buffer (get-buffer-create tlon-translate-log-buffer-name)
      (goto-char (point-max))
      (insert msg "\n"))
    (message "%s" msg)))

;;;###autoload
(defun tlon-translate-show-log ()
  "Display the tlon-translate log buffer in another window."
  (interactive)
  (let ((buf (get-buffer tlon-translate-log-buffer-name)))
    (unless buf
      (user-error "No log buffer found"))
    (with-current-buffer buf
      (goto-char (point-max)))
    (display-buffer buf)))

;;;; Variables

(defconst tlon-translate-revise-tools
  '("edit_file" "apply_diff" "replace_file_contents")
  "List of tools available to the AI for translation revision.")

(defconst tlon-translate--engine-choices
  '(("DeepL" . deepl)
    ("AI" . ai))
  "Alist of translation engine display names and their symbols.")

(defconst tlon-translate-prompt-revise-prefix
  "I am sharing with you a series of paragraph pairs. Each pair consists of an original paragraph in English and a translation of it into %2$s. "
  "Prefix for translation revision prompts.")

(defconst tlon-translate-prompt-revise-suffix
  "Only after you are done comparing the paragraphs and determining all the changes that should be made to the translation, write your changes to \"%1$s\" using the 'edit_file' tool. Only if this tool doesn't work, try the other tools available. Note that the file (\"%1$s\") may include other paragraphs besides those I shared with you; you should only modify the paragraphs that I shared with you, leaving the rest of the file unchanged.\n\nHere are the paragraph pairs:\n\n"
  "Suffix for translation revision prompts.")

(declare-function tlon-md-tag-list "tlon-md")
(defconst tlon-translate-spot-errors-prompt
  (concat tlon-translate-prompt-revise-prefix
	  (format "Your task is to read both carefully and try to spot errors in the translation: the code surrounding the translation may have been corrupted, there may be sentences and even paragraphs missing, the abbreviations may be used wrongly or inconsistently, etc. In addition, the custom tags we use, which should remain invariant, may have been inadvertently translated; if so, you should restore them to their original form. This is an exhaustive list of all our custom tags: %S. Do not modify any URLs or BibTeX keys. Similarly, the fields in the YAML metadata section at the beginning of the file (delimited by ‘%s’) may have been modified. The only admissible YAML fields are: (\"title\" \"html_title\" \"key\" \"original_path\" \"tags\" \"publication_status\" \"meta\" \"snippet\"). If you find a field with any other name, you should onvert it to the closest valid alternative. For example, if you find \"titre\", you should convert it to \"title\". " (tlon-md-tag-list) tlon-yaml-delimiter)
	  tlon-translate-prompt-revise-suffix)
  "Prompt for revising translation errors.")

(defconst tlon-translate-improve-flow-prompt
  (concat tlon-translate-prompt-revise-prefix
	  "Your task is to to read both carefully and try improve the translation for a better flow. Do not modify URLS, BibTeX keys, or tags enclosed in angular brackets (such as \"<Roman>\", \"<LiteralLink>\", etc.)"
	  tlon-translate-prompt-revise-suffix)
  "Prompt for improving translation flow.")

(defconst tlon-translate-glossary-prompt
  " I have attached a glossary file named `%s`. It lists English terms and their required translations into the target language. Use the attached glossary mappings exactly whenever a glossary term appears; if a term is not listed, choose a translation consistent with the glossary’s terminology and style."
  "Prompt snippet to instruct the model to use the attached glossary file.")

(defvar tlon-translate-source-language nil
  "Source language of the current API request.")

(defvar tlon-translate-target-language nil
  "Target language of the current API request.")

(defvar tlon-translate-text nil
  "The text to be translated in the current API request.")

(defvar tlon-translate--active-revision-processes nil
  "List of active processes launched by `tlon-translate' revision commands.")

(defvar tlon-translate--external-abstracts-running nil
  "Non-nil while `tlon-translate--external-abstracts' runs to prevent re-entry.")

;;;; Commands

;;;;; Translation

;;;;;; text

;;;###autoload
(defun tlon-translate-text (&optional text target-lang source-lang callback no-glossary)
  "Translate TEXT from SOURCE-LANG into TARGET-LANG and execute CALLBACK.
If SOURCE-LANG is nil, use \"en\". If CALLBACK is nil, execute
`tlon-print-translation'. If NO-GLOSSARY is non-nil, don't ask for
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
      (pcase tlon-translate-engine
	('deepl (tlon-deepl-request-wrapper 'translate callback no-glossary))
	('ai (tlon-ai-request-wrapper 'translate callback no-glossary))
	(_ (user-error "Unsupported translation engine: %s" tlon-translate-engine))))))

;;;;;; file

(declare-function tlon-get-counterpart-in-originals "tlon-counterpart")
(declare-function tlon-get-counterpart "tlon-counterpart")
(declare-function tlon-db-get-translation-key "tlon-db")
(declare-function tlon-yaml-guess-english-counterpart "tlon-yaml")
;;;###autoload
(defun tlon-translate-file (&optional file lang target-file)
  "Translate FILE into LANG using `tlon-translate-engine'.
If FILE is not provided, prompt for one, defaulting to the current buffer's
file. If LANG is not provided, prompt for a target language. TARGET-FILE is the
file where the translation will be saved. If nil, determine it based on the
source file and target language. If a counterpart already exists, prompt for
confirmation before overwriting it.

Branching rules:

- If \"articles\": require a DB translation entry for LANG; use its title for
  the default filename. After writing, set YAML key, insert translated tags,
  save, and copy associated images.

- If \"tags\" or \"authors\": take the title from YAML, translate it to LANG to
  derive the default filename. After writing, overwrite YAML title with the
  translated title, save, and run `tlon-yaml-guess-english-counterpart'.

- Otherwise: fall back to the existing behavior."
  (interactive
   (list (read-file-name "Translate file: " nil nil t
			 (file-relative-name (buffer-file-name) default-directory))
         (tlon-select-language 'code 'babel "Target language: " 'require-match)))
  (let* ((source-file file)
         (target-lang-code lang)
	 (bare-en (tlon-get-bare-dir source-file t))
	 (counterpart-dir (tlon-get-counterpart-dir source-file target-lang-code))
	 (post-fn nil)
         (target-base-dir (or counterpart-dir
                              (let* ((maybe-cp (condition-case nil
                                                   (tlon-get-counterpart source-file target-lang-code)
                                                 (error nil))))
                                (or (and maybe-cp (file-name-directory (expand-file-name maybe-cp)))
                                    (read-directory-name "Counterpart directory for saving translation: "
                                                         (file-name-directory source-file))))))
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
				    candidate
				    nil
				    (file-name-nondirectory candidate))))
		 (candidate candidate)
		 (t
		  (let* ((default-name
			  (cond
			   ((string= bare-en "articles")
			    (let* ((orig-key (tlon-yaml-get-key "key" source-file))
				   (tkey (and orig-key
					      (tlon-db-get-translation-key orig-key target-lang-code))))
			      (unless tkey
				(user-error "No DB translation entry found for %s -> %s" orig-key target-lang-code))
			      (let* ((title (tlon-bibliography-lookup "=key=" tkey "title"))
				     (slug  (simple-extras-slugify title))
				     (ext   (file-name-extension source-file t)))
				(setq post-fn
				      (lambda ()
					(tlon-yaml-insert-field "key" tkey 'overwrite)
					(tlon-yaml-insert-translated-tags)
					(save-buffer)
					(tlon-translate--copy-associated-images source-file target-file)))
				(concat slug ext))))
			   ((or (string= bare-en "tags") (string= bare-en "authors"))
			    (let* ((orig-title (tlon-yaml-get-key "title" source-file))
				   (src-code (tlon-get-language-in-file source-file))
				   (translated (ignore-errors
						 (tlon-translate-text orig-title target-lang-code src-code nil t)))
				   (final-title (or translated orig-title))
				   (slug (simple-extras-slugify final-title))
				   (ext  (file-name-extension source-file t)))
			      (setq post-fn
				    (lambda ()
				      (tlon-yaml-insert-field "title" final-title 'overwrite)
				      (save-buffer)
				      (tlon-yaml-guess-english-counterpart target-file)))
			      (concat slug ext)))
			   (t
			    (tlon-translate--default-filename source-file target-lang-code))))
			 (default-path (when default-name (file-name-concat target-base-dir default-name))))
		    (read-file-name "Save translation to: " (file-name-as-directory target-base-dir) default-path nil default-name))))))))
    (when target-file
      (tlon-translate--do-translate source-file target-file target-lang-code post-fn))))

(declare-function simple-extras-slugify "simple-extras")
(defun tlon-translate--default-filename (source-file target-lang-code)
  "Return a slugified filename for SOURCE-FILE translated into TARGET-LANG-CODE.
The slug is built by translating the original title with
`tlon-translate-text' and passing the result through
`simple-extras-slugify'.  If translation fails, fall back to a
slugified version of the original basename."
  (let* ((original-key (tlon-yaml-get-key "key" source-file))
	 (translation-key (tlon-get-counterpart-key original-key target-lang-code))
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

(defun tlon-translate--do-translate (source-file target-file target-lang-code &optional after-fn)
  "Translate SOURCE-FILE to TARGET-FILE into TARGET-LANG-CODE.
If AFTER-FN is non-nil, call it after writing TARGET-FILE."
  (let* ((source-lang-code (tlon-repo-lookup :language :dir (tlon-get-repo-from-file source-file)))
         (text (with-temp-buffer
                 (insert-file-contents source-file)
                 (buffer-string))))
    (pcase tlon-translate-engine
      ('deepl
       (tlon-deepl-translate
        text target-lang-code source-lang-code
        (lambda ()
          (tlon-translate--write-and-finalize
           (tlon-translate--get-deepl-translation-from-buffer)
           source-file target-file after-fn))))
      ('ai
       (tlon-ai-translate-text
	text target-lang-code source-lang-code
	(lambda (response info)
          (if (not response)
              (tlon-ai-callback-fail info)
            (tlon-translate--write-and-finalize response source-file target-file after-fn)))))
      (_ (user-error "Unsupported translation engine: %s" tlon-translate-engine)))))

(defun tlon-translate--write-and-finalize (content source-file target-file after-fn)
  "Write CONTENT to TARGET-FILE and finalize translation of SOURCE-FILE.
If CONTENT is a non-empty string, write it and call
`tlon-translate--finalize-written-translation' with SOURCE-FILE, TARGET-FILE,
and AFTER-FN."
  (when (and (stringp content) (> (length (string-trim content)) 0))
    (with-temp-file target-file
      (insert content))
    (tlon-translate--finalize-written-translation source-file target-file after-fn)))

(defun tlon-translate--finalize-written-translation (source-file target-file after-fn)
  "Finalize steps after writing TARGET-FILE translated from SOURCE-FILE.
If AFTER-FN is non-nil and the file type can be determined, call it."
  (message "Translated %s to %s" source-file target-file)
  (condition-case err
      (find-file target-file)
    (error (message "Opening translation failed for %s: %s"
                    (file-name-nondirectory target-file)
                    (error-message-string err))))
  (let ((file-type (condition-case nil
                       (tlon-yaml-get-type target-file)
                     (error nil))))
    (if file-type
        (when (functionp after-fn)
          (funcall after-fn))
      (message "Skipping post-processing for %s: could not determine file type; tags will not be inserted"
               (file-name-nondirectory target-file)))))

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
      (tlon-deepl--translation-text first-translation))))

(declare-function tlon-images-get-dir "tlon-images")
(defun tlon-translate--copy-associated-images (orig-file target-file)
  "Copy images associated with ORIG-FILE into TARGET-FILE’s corresponding paths."
  (let ((src-dir (tlon-images-get-dir orig-file)))
    (when (and src-dir (file-directory-p src-dir))
      (let ((files (directory-files-recursively src-dir ".*"))
            (img-dir (tlon-images-get-dir target-file))
            (lang (tlon-get-language-in-file target-file)))
        (unless (file-directory-p img-dir)
          (make-directory img-dir t))
        (dolist (file files)
          (copy-file file (tlon-get-image-counterpart file lang)))))))

;;;;;; abstract

;;;###autoload
(defun tlon-translate-abstract-dispatch (&optional abstract key langs interactive-call-p)
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
(defvar tlon-file-db)
(declare-function tlon-bib-get-keys-in-file "tlon-bib")
;;;###autoload
(defun tlon-translate-missing-abstracts (&optional langs)
  "Translate abstracts missing from both internal and external sources.
First, for internal translations (case I), iterate over `tlon-file-db' and for
any entry that is a translation (has a non-empty \"translation\" field) but
lacks an \"abstract\", translate the original entry's abstract and write it into
the translation entry's \"abstract\" field.

Second, for external works (case II), fall back to translating abstracts into
the `abstract-translations.json' store for entries cited across
`tlon-file-fluid' and `tlon-file-stable' that are not handled by (I).

If LANGS is non-nil, it is a list of language names (e.g., \\='(\"spanish\")) to
consider for case (II). When nil, prompts the user."
  (interactive)
  (tlon-translate--internal-abstracts)
  (tlon-translate--external-abstracts langs))

;;;###autoload
(defun tlon-translate-abstract-here ()
  "Translate the abstract at point, choosing DB or JSON as destination.
If point is on a DB translation entry that lacks an abstract, translate
the original's abstract into the entry's language and write it into
db.bib. Otherwise, translate the current entry's abstract into a
user-selected language and store it in abstract-translations.json."
  (interactive)
  (when-let ((context (tlon-translate--get-abstract-context nil nil t)))
    (cl-destructuring-bind (key text source-lang-code) context
      (let* ((in-db (member key (tlon-bib-get-keys-in-file tlon-file-db)))
             (translation-of (and in-db (tlon-bibliography-lookup "=key=" key "translation")))
             (current-abstract (and in-db (tlon-bibliography-lookup "=key=" key "abstract"))))
        (if (and in-db
                 (stringp translation-of) (not (string-blank-p translation-of))
                 (or (null current-abstract) (string-blank-p (string-trim current-abstract))))
            (let* ((orig-abstract (tlon-bibliography-lookup "=key=" translation-of "abstract"))
                   (orig-lang-name (tlon-bibliography-lookup "=key=" translation-of "langid"))
                   (trans-lang-name (tlon-bibliography-lookup "=key=" key "langid"))
                   (src-code (and orig-lang-name
                                  (tlon-lookup tlon-languages-properties :code :name orig-lang-name)))
                   (dst-code (and trans-lang-name
                                  (tlon-lookup tlon-languages-properties :code :name trans-lang-name))))
              (if (or (not (stringp orig-abstract))
                      (string-blank-p (string-trim orig-abstract))
                      (not (stringp src-code))
                      (not (stringp dst-code)))
                  (tlon-translate--log "Cannot translate abstract for %s: missing original abstract or language info" key)
                (let* ((src-en (string= src-code "en"))
                       (supports (member dst-code tlon-deepl-supported-glossary-languages))
                       (glossary-id (and src-en supports
                                         (tlon-lookup tlon-deepl-glossaries "glossary_id" "target_lang" dst-code)))
                       (src-name (and src-code (downcase (or (tlon-lookup tlon-languages-properties :name :code src-code) ""))))
                       (dst-name (and trans-lang-name (downcase trans-lang-name)))
                       (both-in-project (and (member src-name tlon-project-languages)
                                             (member dst-name tlon-project-languages))))
                  (cond
                   ((and both-in-project src-en supports glossary-id)
                    (tlon-translate--log "Translating abstract of %s → setting into %s" translation-of key)
                    (tlon-deepl-translate
                     (tlon-bib-remove-braces orig-abstract) dst-code src-code
                     (lambda ()
                       (let ((translated (tlon-translate--get-deepl-translation-from-buffer)))
                         (when (and translated (stringp translated)
                                    (not (string-blank-p (string-trim translated))))
                           (tlon-translate--db-set-abstract key translated)
                           (tlon-translate--log "Set abstract for %s (from %s)" key translation-of))))
                     nil))
                   ((not both-in-project)
                    (tlon-translate--log "Translating abstract of %s → setting into %s (no glossary required)" translation-of key)
                    (tlon-deepl-translate
                     (tlon-bib-remove-braces orig-abstract) dst-code src-code
                     (lambda ()
                       (let ((translated (tlon-translate--get-deepl-translation-from-buffer)))
                         (when (and translated (stringp translated)
                                    (not (string-blank-p (string-trim translated))))
                           (tlon-translate--db-set-abstract key translated)
                           (tlon-translate--log "Set abstract for %s (from %s)" key translation-of))))
                     t))
                   (t
                    (tlon-translate--log "Skipping abstract for %s -> %s: no suitable glossary found" translation-of dst-code))))))
          (tlon-translate-abstract-interactive key text source-lang-code))))))

(defun tlon-translate--external-abstracts (&optional langs)
  "Translate missing abstracts for non-DB works into JSON store.
LANGS is a list of language names such as \\='(\"spanish\" \"french\"). If nil,
prompt using `tlon-read-multiple-languages'."
  (if tlon-translate--external-abstracts-running
      (tlon-translate--log "External abstract translation already running; skipping duplicate invocation")
    (setq tlon-translate--external-abstracts-running t)
    (unwind-protect
        (let* ((prompt "Select target languages for abstract translation (comma-separated): ")
               (selected (or langs (tlon-read-multiple-languages 'babel prompt)))
               (target-names (if (listp selected) selected (list selected))))
          (unless target-names
            (user-error "No target languages selected. Aborting"))
          ;; Precompute compact indexes for performance.
          (let* ((target-codes (mapcar (lambda (name)
                                         (tlon-lookup tlon-languages-properties :code :name name))
                                       target-names))
                 ;; JSON index: key -> set of language codes with non-empty translations
                 (json-store (tlon-read-abstract-translations))
                 (json-index (let ((h (make-hash-table :test #'equal)))
                               (dolist (cell json-store h)
                                 (let ((key (car cell))
                                       (langs (cdr cell))
                                       (langset (make-hash-table :test #'equal)))
                                   (dolist (p langs)
                                     (let ((code (car p))
                                           (text (cdr p)))
                                       (when (and (stringp text)
                                                  (> (length (string-trim text)) 0))
                                         (puthash code t langset))))
                                   (puthash key langset h)))))
                 ;; Collect keys
                 (keys-fluid  (tlon-bib-get-keys-in-file tlon-file-fluid))
                 (keys-stable (tlon-bib-get-keys-in-file tlon-file-stable))
                 (keys-db     (tlon-bib-get-keys-in-file tlon-file-db))
                 (db-set      (let ((s (make-hash-table :test #'equal)))
                                (dolist (k keys-db) (puthash k t s)) s))
                 (all-keys    (seq-uniq (append keys-fluid keys-stable keys-db)))
                 (total       (length all-keys))
                 ;; Build entries map once: key -> entry alist
                 (entries (let ((tbl (make-hash-table :test #'equal)))
                            (dolist (file (list tlon-file-fluid tlon-file-stable tlon-file-db))
                              (let* ((bib (citar-cache--get-bibliography (file-truename file)))
                                     (ents (and bib (citar-cache--bibliography-entries bib))))
                                (when (hash-table-p ents)
                                  (maphash (lambda (_k entry)
                                             (let ((key (cdr (assoc "=key=" entry))))
                                               (when (stringp key)
                                                 (puthash key entry tbl))))
                                           ents))))
                            tbl))
                 ;; DB translations: original -> set of language NAMES with a DB translation
                 (db-translations (let ((idx (make-hash-table :test #'equal)))
                                    (dolist (k keys-db idx)
                                      (let* ((entry (gethash k entries))
                                             (orig  (and entry (cdr (assoc "translation" entry))))
                                             (langn (and entry (cdr (assoc "langid" entry)))))
                                        (when (and (stringp orig) (not (string-blank-p orig))
                                                   (stringp langn) (not (string-blank-p langn)))
                                          (let ((set (or (gethash orig idx)
                                                         (let ((s (make-hash-table :test #'equal)))
                                                           (puthash orig s idx) s))))
                                            (puthash (downcase langn) t set)))))
                                    idx))
		 (initiated-count 0)
		 (processed 0)
		 (skipped 0))
            (message "Checking %d BibTeX entries for missing abstract translations..." total)
            (dolist (key all-keys)
              (setq processed (1+ processed))
              (let* ((entry (gethash key entries))
                     (translation-of (and entry (cdr (assoc "translation" entry))))
                     (is-translation (and (stringp translation-of)
                                          (not (string-blank-p translation-of)))))
		(if is-translation
                    (setq skipped (1+ skipped))
                  (let* ((abstract (and entry (cdr (assoc "abstract" entry))))
			 (langname (and entry (cdr (assoc "langid" entry))))
			 (source-code (and langname (tlon-lookup tlon-languages-properties :code :name langname))))
                    (when (and (stringp abstract) (not (string-blank-p (string-trim abstract)))
                               (stringp source-code) (not (string-blank-p source-code)))
                      (let ((missing '()))
			(cl-loop for tname in target-names
				 for tcode in target-codes
				 do (let* ((same-lang (string= source-code tcode))
                                           (json-set (gethash key json-index))
                                           (has-json (and json-set (gethash tcode json-set)))
                                           (in-db (gethash key db-set))
                                           (dbset (and in-db (gethash key db-translations)))
                                           (has-db (and dbset (gethash (downcase tname) dbset))))
                                      (unless (or same-lang has-json has-db)
					(push tname missing))))
			(when missing
                          (setq initiated-count (1+ initiated-count))
                          (tlon-translate-abstract-dispatch abstract key (nreverse missing)))))))))
            (if (= initiated-count 0)
		(let ((label (if (= (length target-names) 1)
				 (car target-names)
                               (format "languages: %s" (string-join target-names ", ")))))
                  (message (concat "No entries with a missing %s translation found "
				   "(processed %d, skipped %d translation entr%s)")
                           label processed skipped (if (= skipped 1) "y" "ies")))
              (message (concat "Finished checking %d entries. "
			       "Initiated translation for %d entries; skipped %d translation entr%s.")
                       total initiated-count skipped (if (= skipped 1) "y" "ies")))))
      (setq tlon-translate--external-abstracts-running nil))))

(declare-function tlon-db-sync-now "tlon-db")
(defun tlon-translate--internal-abstracts ()
  "Translate missing abstracts for translation entries in `tlon-file-db'.
For each DB entry with a non-empty \"translation\" field and empty/missing
\"abstract\", translate the original's abstract and set it in the DB entry.
If the original entry lacks an abstract, log a message and skip."
  (let* ((buf (find-file-noselect tlon-file-db))
         (index (make-hash-table :test #'equal))
         (work '())
         (scheduled 0)
         (changed-p nil))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (bibtex-map-entries
           (lambda (key beg _end)
             (goto-char beg)
             (let ((entry (bibtex-parse-entry t)))
	       (puthash key entry index)))))
        (goto-char (point-min))
        (bibtex-map-entries
         (lambda (key beg _end)
           (goto-char beg)
           (let* ((entry (bibtex-parse-entry t))
                  (translation-of (cdr (assoc-string "translation" entry t)))
                  (current-abstract (cdr (assoc-string "abstract" entry t))))
             (when (and (stringp translation-of)
                        (not (string-blank-p translation-of))
                        (or (null current-abstract)
                            (string-blank-p (string-trim current-abstract))))
	       (let* ((orig (gethash translation-of index))
		      (orig-abstract (and orig (cdr (assoc-string "abstract" orig t))))
		      (orig-lang (and orig (cdr (assoc-string "langid" orig t))))
		      (trans-lang (cdr (assoc-string "langid" entry t)))
		      (source-code (and orig-lang (tlon-lookup tlon-languages-properties :code :name orig-lang)))
		      (target-code (and trans-lang (tlon-lookup tlon-languages-properties :code :name trans-lang))))
                 (if (or (null orig) (null orig-abstract) (string-blank-p (string-trim (or orig-abstract ""))))
                     (tlon-translate--log "Skipping %s: original %s has no abstract"
                                          key translation-of)
                   (push (list :target key
			       :source translation-of
			       :text orig-abstract
			       :src source-code
			       :dst target-code)
                         work)))))))))
    (setq work (nreverse work))
    (if (null work)
        (tlon-translate--log "No DB translation entries missing abstracts")
      (cl-labels
          ((next ()
             (let ((item (pop work)))
	       (if (null item)
                   (progn
                     (tlon-translate--log "Finished internal abstract translation queue (%d scheduled)" scheduled)
                     (when (and changed-p
                                (y-or-n-p "Abstracts were written to db.bib. Synchronize database now? "))
                       (tlon-db-sync-now)))
                 (setq scheduled (1+ scheduled))
                 (let ((text (plist-get item :text))
		       (src  (plist-get item :src))
		       (dst  (plist-get item :dst))
		       (tkey (plist-get item :target))
		       (skey (plist-get item :source)))
                   (tlon-translate--log "Translating abstract of %s → setting into %s" skey tkey)
                   (let* ((src-en (string= src "en"))
                          (supports (member dst tlon-deepl-supported-glossary-languages))
                          (glossary-id (and src-en supports
                                            (tlon-lookup tlon-deepl-glossaries "glossary_id" "target_lang" dst)))
                          (src-name (downcase (or (tlon-lookup tlon-languages-properties :name :code src) "")))
                          (dst-name (downcase (or (tlon-lookup tlon-languages-properties :name :code dst) "")))
                          (both-in-project (and (member src-name tlon-project-languages)
                                                (member dst-name tlon-project-languages))))
                     (cond
                      ((and both-in-project src-en supports glossary-id)
                       (tlon-deepl-translate
                        text dst src
                        (lambda ()
                          (let ((translated (tlon-translate--get-deepl-translation-from-buffer)))
                            (when (and translated (stringp translated) (not (string-blank-p (string-trim translated))))
                              (tlon-translate--db-set-abstract tkey translated)
                              (setq changed-p t)
                              (tlon-translate--log "Set abstract for %s (from %s)" tkey skey)))
                          (next)))
                       )
                      ((not both-in-project)
                       (tlon-deepl-translate
                        text dst src
                        (lambda ()
                          (let ((translated (tlon-translate--get-deepl-translation-from-buffer)))
                            (when (and translated (stringp translated) (not (string-blank-p (string-trim translated))))
                              (tlon-translate--db-set-abstract tkey translated)
                              (setq changed-p t)
                              (tlon-translate--log "Set abstract for %s (from %s)" tkey skey)))
                          (next)))
                       t)
                      (t
                       (tlon-translate--log "Skipping abstract for %s -> %s: no suitable glossary found" skey dst)
                       (next)))))))))
        (next)))))

(declare-function bibtex-extras-set-field "bibtex-extras")
(defun tlon-translate--db-set-abstract (key text)
  "Set the ABSTRACT field of entry KEY in `tlon-file-db' to TEXT."
  (let ((buf (find-file-noselect tlon-file-db)))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (not (bibtex-search-entry key))
              (tlon-translate--log "Entry %s not found in db file" key)
            (bibtex-extras-set-field "abstract" text)
            (save-buffer)))))))

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

(defun tlon-translate--has-translating-entry-p (key target-lang-name keys)
  "Return non-nil if another entry translates KEY into TARGET-LANG-NAME.
KEY is a BibTeX key. TARGET-LANG-NAME is the language name stored in
the \"langid\" field (e.g., \"french\"). KEYS is the list of keys to scan."
  (catch 'found
    (dolist (k keys)
      (unless (string= k key)
        (let ((lang (tlon-bibliography-lookup "=key=" k "langid"))
              (translation (tlon-bibliography-lookup "=key=" k "translation")))
          (when (and (stringp lang)
                     (stringp translation)
                     (not (string-blank-p translation))
                     (string-equal (downcase lang) (downcase target-lang-name))
                     (string= translation key))
            (throw 'found t)))))
    nil))

(declare-function tlon-bib-remove-braces "tlon-bib")
(declare-function tlon-translate-abstract-callback "tlon-bib")
(declare-function tlon-bib-unescape-escaped-characters "tlon-bib")
(defvar tlon-file-abstract-translations)
(defun tlon-translate-abstract-interactive (key text source-lang-code)
  "Handle interactive abstract translation for KEY, TEXT, SOURCE-LANG-CODE.
If a translation for the KEY into the selected target language already exists,
prompt the user for confirmation before overwriting."
  (let* ((excluded-lang (list (tlon-lookup tlon-languages-properties :standard :code source-lang-code)))
         (target-lang (tlon-select-language 'code 'babel "Target language: " 'require-match nil nil excluded-lang)))
    (when target-lang
      (let* ((existing-translation (tlon-translate--get-existing-abstract-translation key target-lang))
             (target-lang-name (tlon-lookup tlon-languages-properties :standard :code target-lang)))
        ;; Skip creating a JSON translation if a DB translation already exists for this target language.
        (when (tlon-translate--has-translating-entry-p key target-lang-name (tlon-bib-get-keys-in-file tlon-file-db))
          (message "Skipping %s -> %s: DB translation entry exists" key target-lang-name)
          (cl-return-from tlon-translate-abstract-interactive nil))
        ;; Enforce DeepL + glossary constraint (English source + supported target + glossary available).
        (let* ((src-en (string= source-lang-code "en"))
               (supports (member target-lang tlon-deepl-supported-glossary-languages))
               (glossary-id (and src-en supports
                                 (tlon-lookup tlon-deepl-glossaries "glossary_id" "target_lang" target-lang))))
          (if (not glossary-id)
              (message "Skipping %s -> %s: %s-%s glossary missing" key target-lang (upcase source-lang-code) (upcase target-lang))
            (if (and existing-translation
                     (not (y-or-n-p (format "Translation for %s into %s already exists. Retranslate?"
                                            key target-lang-name))))
                (message "Translation for %s into %s aborted by user." key target-lang-name)
              (message "Initiating translation for %s -> %s (%s)" key target-lang-name source-lang-code)
              (tlon-deepl-translate
               (tlon-bib-remove-braces text) target-lang source-lang-code
               (lambda ()
                 (tlon-translate--suppress-file-change-prompt
                  (lambda ()
                    (tlon-translate-abstract-callback key target-lang 'overwrite))))
               nil))))))))

(defvar tlon-project-target-languages)
(defun tlon-translate-abstract-non-interactive (key text source-lang-code langs)
  "Handle non-interactive abstract translation for KEY, TEXT, SOURCE-LANG-CODE.
LANGS is a list of languages, such as `(\"spanish\" \"french\")'. If LANGS is
nil, use `tlon-project-target-languages'."
  (message "Checking abstract translations for %s..." key)
  (let ((initiated-langs '()))
    (let ((languages (or (and (listp langs) langs)
                         (and langs (list langs))
                         tlon-project-target-languages)))
      (dolist (language languages)
        (let ((target-lang (tlon-lookup tlon-languages-properties :code :name language)))
          (unless (and (string= source-lang-code target-lang)
                       (tlon-translate--get-existing-abstract-translation key target-lang))
            (let* ((src-en (string= source-lang-code "en"))
                   (supports (member target-lang tlon-deepl-supported-glossary-languages))
                   (glossary-id (and src-en supports
                                     (tlon-lookup tlon-deepl-glossaries "glossary_id" "target_lang" target-lang)))
                   (src-name (downcase (or (tlon-lookup tlon-languages-properties :name :code source-lang-code) "")))
                   (dst-name (downcase language))
                   (both-in-project (and (member src-name tlon-project-languages)
                                         (member dst-name tlon-project-languages))))
              (cond
               ((and both-in-project src-en supports glossary-id)
                (push language initiated-langs)
                (message "Initiating translation for %s -> %s" key target-lang)
                (tlon-deepl-translate (tlon-bib-remove-braces text) target-lang source-lang-code
                                      (lambda ()
                                        (tlon-translate--suppress-file-change-prompt
                                         (lambda ()
                                           (tlon-translate-abstract-callback key target-lang 'overwrite))))
                                      nil))
               ((not both-in-project)
                (push language initiated-langs)
                (message "Initiating translation for %s -> %s (no glossary required)" key target-lang)
                (tlon-deepl-translate (tlon-bib-remove-braces text) target-lang source-lang-code
                                      (lambda ()
                                        (tlon-translate--suppress-file-change-prompt
                                         (lambda ()
                                           (tlon-translate-abstract-callback key target-lang 'overwrite))))
                                      t))
               (t
                (message "Skipping %s -> %s: no suitable glossary found" key target-lang)))))))
      (when initiated-langs
	(message "Finished initiating translations for abstract of `%s' into: %s"
		 key (string-join (reverse initiated-langs) ", "))))))

(defun tlon-translate--suppress-file-change-prompt (thunk)
  "Call THUNK while suppressing file-changed prompting, and refresh JSON.
THUNK is a nullary function. Temporarily set `revert-without-query' to match
all files, invoke THUNK, then silently revert any live buffer visiting
`tlon-file-abstract-translations'. Finally restore the original value."
  (let ((orig revert-without-query))
    (unwind-protect
        (progn
          (setq revert-without-query '(".*"))
          (funcall thunk)
          (when (and (boundp 'tlon-file-abstract-translations)
                     (stringp tlon-file-abstract-translations))
            (when-let ((buf (get-file-buffer tlon-file-abstract-translations)))
              (with-current-buffer buf
                (revert-buffer :ignore-auto :noconfirm)))))
      (setq revert-without-query orig))))

(declare-function tlon-write-abstract-translations "tlon-bib")
;;;###autoload
(defun tlon-translate-remove-duplicate-abstract-translations ()
  "Remove JSON abstract translations that duplicate DB translation entries.
Iterate over translation entries in `tlon-file-db' (non-empty \"translation\"),
derive the original key and language code, and delete the corresponding language
entry from `abstract-translations.json'. Drop the original key from the JSON
store if it ends up empty. Report how many per-language items were removed and
how many originals were pruned entirely."
  (interactive)
  (let* ((db-keys (tlon-bib-get-keys-in-file tlon-file-db))
         (store   (tlon-read-abstract-translations))
         (removed 0)
         (pruned-keys 0)
         (total (length db-keys))
         (processed 0)
         (progress-interval (max 1 (ceiling (/ (float total) 10)))))
    (tlon-translate--log "Scanning %d DB entr%s for duplicate abstract translations..."
                         total (if (= total 1) "y" "ies"))
    (dolist (tkey db-keys)
      (setq processed (1+ processed))
      (let* ((orig (tlon-bibliography-lookup "=key=" tkey "translation"))
             (lang-name (tlon-bibliography-lookup "=key=" tkey "langid"))
             (lang-code (and (stringp lang-name)
                             (tlon-lookup tlon-languages-properties :code :name lang-name))))
        (when (and (stringp orig) (not (string-blank-p orig))
                   (stringp lang-code) (not (string-blank-p lang-code)))
          (let ((entry (assoc orig store)))
            (when entry
              (let* ((langs (cdr entry))
                     (newlangs (cl-remove-if (lambda (p) (string= (car p) lang-code)) langs)))
                (when (< (length newlangs) (length langs))
                  (cl-incf removed)
                  (if newlangs
                      (setcdr entry newlangs)
                    (setq store (cl-remove-if (lambda (cell) (string= (car cell) orig)) store))
                    (cl-incf pruned-keys))))))))
      (when (or (= processed total)
                (zerop (mod processed progress-interval)))
        (tlon-translate--log "Progress: %d/%d processed; removed %d duplicate%s; pruned %d original entr%s"
                             processed total
                             removed (if (= removed 1) "" "s")
                             pruned-keys (if (= pruned-keys 1) "y" "ies"))))
    (if (> removed 0)
        (progn
          (tlon-write-abstract-translations store)
          (tlon-translate--log "Removed %d JSON translation%s (%d original entr%s pruned)"
                               removed (if (= removed 1) "" "s")
                               pruned-keys (if (= pruned-keys 1) "y" "ies")))
      (tlon-translate--log "No duplicate JSON translations found to remove"))))

;;;;; Revision

;;;###autoload
(defun tlon-translate-spot-errors ()
  "Use AI to spot errors in a translation file."
  (interactive)
  (tlon-translate--revise-common 'errors))

;;;###autoload
(defun tlon-translate-improve-flow ()
  "Use AI to improve the flow of a translation file."
  (interactive)
  (tlon-translate--revise-common 'flow))

;;;###autoload
(defun tlon-translate-spot-errors-in-range ()
  "Use AI to spot errors in a translation file within a paragraph range."
  (interactive)
  (let ((range (tlon-translate--read-paragraph-range)))
    (let ((tlon-translate-restrict-revision-to-paragraphs range))
      (tlon-translate-spot-errors))))

;;;###autoload
(defun tlon-translate-improve-flow-in-range ()
  "Use AI to improve the flow of a translation file within a paragraph range."
  (interactive)
  (let ((range (tlon-translate--read-paragraph-range)))
    (let ((tlon-translate-restrict-revision-to-paragraphs range))
      (tlon-translate-improve-flow))))

(defun tlon-translate--read-paragraph-range ()
  "Prompt for a paragraph range and return it as a cons (START . END).
Empty inputs mean the beginning or end of the file respectively. START and END
are 1-based. Return nil to indicate the whole file."
  (let* ((current tlon-translate-restrict-revision-to-paragraphs)
         (start-str (read-string
                     (if current
                         (format "Start paragraph (empty = first) [current: %s]: "
                                 (or (car current) "1"))
                       "Start paragraph (empty = first): ")))
         (end-str   (read-string
                     (if current
                         (format "End paragraph (empty = last) [current: %s]: "
                                 (or (cdr current) "last"))
                       "End paragraph (empty = last): ")))
         (start (unless (string-blank-p start-str)
                  (string-to-number start-str)))
         (end   (unless (string-blank-p end-str)
                  (string-to-number end-str))))
    (when (and start (<= start 0))
      (user-error "Start must be positive"))
    (when (and start end (< end start))
      (user-error "End must be ≥ start"))
    (if (and (null start) (null end))
        nil
      (cons start end))))

;;;###autoload
(defun tlon-translate-revise-abort ()
  "Abort all ongoing `tlon-translate' revision requests.

Any live asynchronous processes started by `tlon-translate-revise-*'
commands are killed.  If there are no such processes, do nothing."
  (interactive)
  ;; Filter out any nil placeholders and compute the total number of
  ;; tracked requests before attempting to abort them.  Relying solely
  ;; on `process-live-p' can mis-report when the underlying object is
  ;; not an Emacs process, so we base the user message on the number of
  ;; tracked requests instead.
  (let* ((tracked (delq nil (copy-sequence tlon-translate--active-revision-processes)))
         (total   (length tracked)))
    (dolist (proc tracked)
      (when (and (processp proc) (process-live-p proc))
        (delete-process proc)))
    (setq tlon-translate--active-revision-processes nil)
    (tlon-translate--log (if (> total 0)
                             "Aborted %d tlon-translate revision request%s."
                           "No tlon-translate revision requests in progress.")
                         total (if (= total 1) "" "s"))))

(defvar tlon-translate-revert-without-query-original
  revert-without-query
  "Original value of `revert-without-query'.")

(declare-function gptel-context-add-file "gptel-context")
(declare-function gptel-context-remove-all "gptel-context")
(defun tlon-translate--revise-common (type)
  "Common function for revising a translation of TYPE.
TYPE can be `errors' or `flow'."
  (gptel-extras-warn-when-context)
  (let* ((translation-file (expand-file-name (read-file-name "Translation file: " (buffer-file-name))))
	 (original-file
	  (let ((counterpart (condition-case _err
				 (tlon-get-counterpart translation-file)
			       (error nil))))
	    (if counterpart
		(expand-file-name counterpart)
	      (read-file-name "Original file: ")))))
    (unless (tlon-paragraph-files-are-aligned-p translation-file original-file)
      (user-error "Files have different paragraph counts; align them first with `tlon-paragraphs-align-with-ai'"))
    (let* ((lang-code (or (tlon-get-language-in-file translation-file)
                          (tlon-select-language 'code 'babel "Language of translation file: " t)))
           (language (or (tlon-lookup tlon-languages-properties :standard :code lang-code)
                         lang-code))
           (model (pcase type
                    ('errors tlon-translate-spot-errors-model)
                    ('flow tlon-translate-improve-flow-model)))
           (prompt-template (pcase type
                              ('errors tlon-translate-spot-errors-prompt)
                              ('flow tlon-translate-improve-flow-prompt)))
	   (prompt-elts (delq nil (list prompt-template translation-file (capitalize language))))
	   (glossary-type 'ai-revision)
	   (glossary-file (when (and (eq type 'flow)
				     (tlon-extract-glossary lang-code glossary-type))
			    (tlon-glossary-target-path lang-code glossary-type)))
	   (glossary-prompt (when glossary-file
			      (format tlon-translate-glossary-prompt (file-name-nondirectory glossary-file))))
	   (prompt (concat (apply 'format prompt-elts) glossary-prompt))
	   (orig-paras  (tlon-with-paragraphs original-file nil nil (eq type 'errors)))
           (trans-paras (tlon-with-paragraphs translation-file nil nil (eq type 'errors)))
           (chunk-size  tlon-translate-revise-chunk-size)
           (total       (length orig-paras))
           (ranges '())
	   (restrict tlon-translate-restrict-revision-to-paragraphs)
           (start-idx (if (and restrict (car restrict))
                          (max 0 (1- (car restrict)))
                        0))
           (end-idx   (if (and restrict (cdr restrict))
                          (min total (cdr restrict))
                        total)))
      (when (>= start-idx end-idx)
        (user-error "Invalid paragraph range %S" restrict))
      (let* ((selected-count (- end-idx start-idx))
             (base-ranges (tlon-translate--build-chunk-ranges selected-count chunk-size)))
        (setq ranges
              (mapcar (lambda (pr)
                        (cons (+ start-idx (car pr))
                              (+ start-idx (cdr pr))))
                      base-ranges)))
      (when ranges
	(if restrict
            (tlon-translate--log "\nSending %d revision chunk%s (in range %d–%d) of %s…"
                                 (length ranges)
                                 (if (= (length ranges) 1) "" "s")
                                 (or (car restrict) 1)
                                 (or (cdr restrict) total)
				 (file-name-nondirectory translation-file))
          (tlon-translate--log "\nSending %d revision chunk%s of %s…"
                               (length ranges)
                               (if (= (length ranges) 1) "" "s")
			       (file-name-nondirectory translation-file)))
	(if (<= (length ranges) tlon-translate-revise-max-parallel)
	    ;; Few enough chunks → process them all in parallel.
	    (tlon-translate--revise-parallel
	     ranges translation-file original-file type prompt model
	     orig-paras trans-paras restrict glossary-file)
	  ;; More chunks than the parallel cap → process in parallel *batches*
	  ;; of `tlon-translate-revise-max-parallel'.
	  (tlon-translate--revise-parallel-batches
	   ranges translation-file original-file type prompt model
	   orig-paras trans-paras restrict glossary-file))))))

(defun tlon-translate--build-chunk-ranges (total chunk-size)
  "Build chunk ranges (START . END) for TOTAL items with CHUNK-SIZE."
  (let ((ranges '())
        (i 0))
    (while (< i total)
      (push (cons i (min total (+ i chunk-size))) ranges)
      (setq i (+ i chunk-size)))
    (nreverse ranges)))

(defun tlon-translate--revise-parallel (ranges translation-file original-file type prompt model orig-paras trans-paras restrict glossary-file)
  "Use parallel processing to revise translation in RANGES.
RANGES is a list of ranges to revise. TRANSLATION-FILE is the path to the
translation file. ORIGINAL-FILE is the path to the original file. TYPE is the
type of revision. PROMPT is the prompt to use for revision. MODEL is the AI
model to use. ORIG-PARAS are the original paragraphs. TRANS-PARAS are the
translated paragraphs. RESTRICT is a boolean indicating whether to restrict the
revision to specific areas. GLOSSARY-FILE is the glossary file"
  (cl-loop for r in ranges
	   for idx from 0
	   do (tlon-translate--revise-send-range
	       r translation-file original-file type
	       prompt model tlon-translate-revise-tools
	       orig-paras trans-paras restrict glossary-file))
  (tlon-translate--message-revise-request translation-file ranges t))

(defun tlon-translate--message-revise-request (translation-file ranges parallel-p)
  "Display message about AI revision request for TRANSLATION-FILE with RANGES.
PARALLEL-P indicates whether processing is parallel or sequential."
  (tlon-translate--log "Requesting AI to revise %s in %d %s chunks..."
                       (file-name-nondirectory translation-file)
                       (length ranges)
                       (if parallel-p "parallel" "paragraph")))

(defun tlon-translate--revise-parallel-batches
    (ranges translation-file original-file type prompt model
            orig-paras trans-paras restrict glossary-file)
  "Process RANGES in parallel batches of `tlon-translate-revise-max-parallel'.
RANGES is a list of ranges to process. TRANSLATION-FILE is the path to the
translation file being revised. ORIGINAL-FILE is the path to the original file
being translated. TYPE specifies the type of translation or revision operation.
PROMPT is the prompt text to use for the translation model. MODEL specifies
which translation model to use. TOOLS are additional tools or parameters for the
translation process. ORIG-PARAS contains the original paragraphs from the source
text. TRANS-PARAS contains the translated paragraphs being revised. RESTRICT is
a boolean indicating whether to restrict the revision to specific areas.
GLOSSARY-FILE is the glossary file."
  (cl-labels
      ((process (remaining idx)
         (if (null remaining)
             (progn
               (tlon-translate--log "Completed processing all %d chunk%s of %s"
                                    idx (if (= idx 1) "" "s")
                                    (file-name-nondirectory translation-file)))
           (let* ((batch (cl-subseq remaining
                                    0 (min tlon-translate-revise-max-parallel
                                           (length remaining))))
                  (rest  (nthcdr (length batch) remaining))
                  (pending (length batch)))
             (dolist (r batch)
               (tlon-translate--revise-send-range
                r translation-file original-file type prompt model
                tlon-translate-revise-tools orig-paras trans-paras restrict glossary-file
                (lambda ()
                  (setq pending (1- pending))
                  (when (= pending 0)
                    ;; Batch finished – launch next one.
                    (process rest (+ idx (length batch)))))))))))
    (process ranges 0)))

;;;;;; chunk helpers

(defun tlon-translate--gptel-callback-simple (translation-file type after-fn)
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
         response info translation-file type))
      ;; Surface errors so the user sees them, but still advance.
      (when (and (not (stringp response))
                 (or (plist-get info :error)
                     (let ((st (plist-get info :status)))
                       (and (numberp st) (>= st 400)))))
        (tlon-ai-callback-fail info))
      ;; Decide when the request is *finished* and fire AFTER-FN once.
      (when (not fired)
        (setq fired t)
        (when (functionp after-fn)
          (funcall after-fn))))))

(defun tlon-translate--kill-indirect-buffers-of-file (file)
  "Kill every indirect buffer created from FILE.

Two fall-back heuristics are used:

1. *Chain match* – follow `buffer-base-buffer' up to the first real buffer and
    compare the return value of the function `buffer-file-name' with FILE using
    `file-equal-p'. If they match and the buffer in question is *not* that real
    buffer, it is an indirect clone that can be killed.

2.  *Name match* – if the buffer name looks like
    \"<basename><N>\" where <basename> is the base name of FILE and <N>
    is a decimal number in angle brackets, kill it even when the
    base-buffer chain is broken.

This catches stubborn leftovers left behind by `clone-indirect-buffer'."
  (let* ((file    (expand-file-name file))
         (base    (file-name-nondirectory file))
         (name-rx (format "^%s<[0-9]+>$" (regexp-quote base))))
    (dolist (buf (buffer-list))
      (let* ((real buf)
             (indirect-p (buffer-base-buffer buf)))
        ;; Walk up the chain, if any.
        (while (buffer-base-buffer real)
          (setq real (buffer-base-buffer real)))
        (when (or
               ;; Heuristic 1 – chain match
               (and indirect-p
                    (buffer-file-name real)
                    (file-equal-p (buffer-file-name real) file))
               ;; Heuristic 2 – name match
               (string-match-p name-rx (buffer-name buf)))
          (kill-buffer buf))))))

(defun tlon-translate--revise-send-range
    (range translation-file original-file type prompt-template model
           tools orig-paras trans-paras restrict glossary-file &optional after-fn)
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
RESTRICT is a boolean indicating whether to restrict the revision to specific
areas. GLOSSARY-FILE is the glossary file. AFTER-FN is an optional function to
call after the revision is complete."
  (let* ((start (car range))
         (end   (cdr range))
         (orig-chunk  (cl-subseq orig-paras start end))
         (trans-chunk (cl-subseq trans-paras start end))
         (comparison (tlon-paragraphs--get-comparison-buffer-content
                      translation-file original-file trans-chunk orig-chunk nil))
         (prompt (tlon-ai-maybe-edit-prompt (concat
					     prompt-template
					     comparison)))
         (single-p (= end (1+ start)))
         (chunk-desc (if single-p
			 (format "%d" (1+ start))
		       (format "%d–%d" (1+ start) end)))
         (proc nil)
         (wrapped-after-fn
          (lambda ()
            (setq tlon-translate--active-revision-processes
                  (delq proc tlon-translate--active-revision-processes))
            (if restrict
                (let ((rstart (or (car restrict) 1))
                      (rend   (or (cdr restrict)
                                  (tlon-get-number-of-paragraphs-in-file translation-file))))
                  (tlon-translate--log "Finished processing %s %s (range %d–%d) of %s"
                                       (if single-p "paragraph" "paragraphs")
                                       chunk-desc rstart rend
                                       (file-name-nondirectory translation-file)))
              (tlon-translate--log "Finished processing %s %s (out of %d) of %s"
                                   (if single-p "paragraph" "paragraphs")
                                   chunk-desc
                                   (tlon-get-number-of-paragraphs-in-file translation-file)
                                   (file-name-nondirectory translation-file)))
            (when (null tlon-translate--active-revision-processes)
              (setq revert-without-query tlon-translate-revert-without-query-original))
            (when (functionp after-fn)
              (funcall after-fn)))))
    ;; Clean up the transient indirect buffers created for COMPARISON.
    (tlon-translate--kill-indirect-buffers-of-file translation-file)
    (tlon-translate--kill-indirect-buffers-of-file original-file)
    (tlon-translate--log "Processing %s %s%s of %s"
                         (if single-p "paragraph" "paragraphs")
                         chunk-desc
                         (if restrict
                             (format " (in range %d–%d)"
                                     (or (car restrict) 1)
                                     (or (cdr restrict)
                                         (tlon-get-number-of-paragraphs-in-file translation-file)))
                           (format " (out of %d)"
                                   (tlon-get-number-of-paragraphs-in-file translation-file)))
			 (file-name-nondirectory translation-file))
    (setq revert-without-query '(".*")) ; avoid annoying prompt to confirm revert
    (let* ((req-buf (get-buffer-create (format "*tlon-revise:%s:%s*"
                                               (file-name-nondirectory translation-file)
                                               chunk-desc))))
      (with-current-buffer req-buf
        (setq-local gptel-include-reasoning nil)
        (when glossary-file
          (gptel-context-add-file glossary-file)))
      (setq proc
            (tlon-make-gptel-request
             prompt nil
             (tlon-translate--gptel-callback-simple translation-file type wrapped-after-fn)
             model 'skip-context-check req-buf tools)))
    (push proc tlon-translate--active-revision-processes)
    proc))

(declare-function magit-stage-files "magit-apply")
(defun tlon-translate--revise-callback (response info file type)
  "Callback for AI revision.
RESPONSE is the AI's response. INFO is the response info. FILE is the file to
commit. TYPE is the revision type. START is the starting paragraph number.
END is the ending paragraph number."
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
			    file)))
    (when (eq type 'errors)
      (condition-case err
          (with-current-buffer (or (get-file-buffer file)
                                   (find-file-noselect file))
            (tlon-translate-relative-links))
        (error (tlon-translate--log "tlon-translate-relative-links failed: %s"
                                    (error-message-string err))))))))

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

(transient-define-infix tlon-translate-infix-select-spot-errors-model ()
  "AI model to use for spotting errors in translations.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-translate-spot-errors-model)

(transient-define-infix tlon-translate-infix-select-improve-flow-model ()
  "AI model to use for improving the flow of translations.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-translate-improve-flow-model)

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
    ("t a" "Translate current abstract" tlon-translate-abstract-here)
    ("t A" "Translate missing abstracts" tlon-translate-missing-abstracts)
    ("t d" "Remove duplicate abstracts" tlon-translate-remove-duplicate-abstract-translations)
    ""
    "Options"
    ("t -t" "Translation engine" tlon-translate-engine-infix)
    ("t -d" "DeepL model" tlon-deepl-model-type-infix)
    ("t -a" "AI model" tlon-ai-infix-select-translation-model)]
   ["Revise"
    ("r e" "Spot errors" tlon-translate-spot-errors)
    ("r f" "Improve flow" tlon-translate-improve-flow)
    ("r E" "Spot errors in range" tlon-translate-spot-errors-in-range)
    ("r F" "Improve flow in range" tlon-translate-improve-flow-in-range)
    ""
    ("r l" "Show log" tlon-translate-show-log)
    ("r a" "Abort revision" tlon-translate-revise-abort)
    ""
    "Options"
    ("r -e" "Spot errors model" tlon-translate-infix-select-spot-errors-model)
    ("r -f" "Improve flow model" tlon-translate-infix-select-improve-flow-model)
    ""
    ("r -c" "Chunk size"     tlon-translate-infix-set-chunk-size)
    ("r -p" "Max parallel" tlon-translate-infix-set-max-parallel)]
   ["General options"
    ("-c" "Commit changes" tlon-translate-infix-toggle-commit-changes)]])

(provide 'tlon-translate)
;;; tlon-translate.el ends here
