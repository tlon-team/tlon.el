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

(require 'tlon-glossary)
(require 'tlon-core)
(require 'url)
(require 'seq) ; For seq-uniq

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
    
    (if callback
        (tlon-deepl-request-wrapper 'translate callback no-glossary-ok)
      (let* ((temp-file (make-temp-file "deepl-direct-" nil ".json"))
             (glossary-id (tlon-deepl-get-language-glossary target-lang))
             (payload (json-encode
                       (append `(("text" . ,(vector text))
                                 ("source_lang" . ,source-lang)
                                 ("target_lang" . ,target-lang))
                               (when glossary-id
                                 `(("glossary_id" . ,glossary-id))))))
             (output-buffer (generate-new-buffer " *deepl-output*"))
             translation)
        (with-temp-file temp-file
          (insert payload))
        
        (call-process "curl" nil output-buffer nil
                      "-s" "-X" "POST" 
                      (concat tlon-deepl-url-prefix "translate")
                      "-H" "Content-Type: application/json"
                      "-H" (concat "Authorization: DeepL-Auth-Key " tlon-deepl-key)
                      "--data" (concat "@" temp-file))
        
        (with-current-buffer output-buffer
          (goto-char (point-min))
          (let ((response-content (buffer-substring-no-properties (point-min) (point-max))))
            (when (string-match "\"text\":\\s-*\"\\([^\"]+\\)\"" response-content)
              (setq translation (match-string 1 response-content))
              (kill-new translation)
              (message "Translation copied to kill ring: %s" 
                       (replace-regexp-in-string "%" "%%" translation)))))
        
        (delete-file temp-file)
        (kill-buffer output-buffer)
        
        translation))))


(defun tlon-deepl-print-translation (&optional copy)
  "Print the translated text.
If COPY is non-nil, copy the translation to the kill ring instead."
  (goto-char (point-min))
  (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
        translation)
    (if (string-match "\"text\":\\s-*\"\\([^\"]+\\)\"" buffer-content)
        (setq translation (match-string 1 buffer-content))
      (goto-char (point-min))
      (condition-case nil
          (let* ((json-array-type 'list)
                 (json-key-type 'string)
                 (json-object-type 'alist)
                 (json-data (json-read))
                 (translations (alist-get 'translations json-data))
                 (first-translation (car translations)))
            (when first-translation
              (setq translation (alist-get 'text first-translation))))
        (error nil)))
    
    (when translation
      (when copy
        (kill-new translation))
      (message (concat (when copy "Copied to kill ring: ") 
                       (replace-regexp-in-string "%" "%%" translation))))
    
    translation))

(defun tlon-deepl-fix-encoding (string)
  "Fix encoding in STRING.
The encoding is misinterpreted as ISO-8859-1 when it's actually UTF-8."
  (decode-coding-string (encode-coding-string string 'iso-8859-1) 'utf-8))

(defun tlon-deepl-fix-encoding-persistent ()
  "Fix persistent encoding issues in the current buffer."
  (interactive)
  (unless (derived-mode-p 'bibtex-mode 'json-mode)
    (user-error "This command must be run in BibTeX mode or JSON mode"))
  (save-excursion
    (goto-char (point-min))
    (dolist (cons '(("\\\\\\\\\\\\\\\\3\\\\\\\\255" . "í")
		    ("\\\\\\\\303\\\\\\\\241" . "á")
		    ("\\\\\\\\303\\\\\\\\263" . "ó")
		    ("\\\\\\\\342\\\\\\\\200\\\\\\\\223" . "—")
		    ("\\\\\\\\342\\\\\\\\200\\\\\\\\235" . "’")
		    ("\\\\\\\\3\\\\\\\\263" . "ó")
		    ("\\\\\\\\3\\\\\\\\3" . "ó")
		    ;; ("\\\\\\\\303\\\\\\e\\251" . "é")
		    ("\\\\\\\\263" . "ó")
		    ("\\\\\\\\342\\\\\\\\200\\\\\\\\234" . "‘")
		    ("\\\\\\\\303\\\\\\\\251" . "é")
		    ("\\\\\\\\\Resumen generado por" . "Resumen generado por")
		    ("\\\\\\\\303\\\\\\\\255" . "í")
		    ("\\\\\\\\303\\\\\\\\272" . "ú")
		    ("\\\\\\\\303\\\\\\\\257" . "ï")
		    ("\\\\\\\\342\\\\\\\\200\\\\\\\\211" . " ")
		    ("\\\\\\\\342\\\\\\\\210\\\\\\\\222" . "-")
		    ("\\\\\\\\302\\\\\\\\261" . "±")
		    ("\\\\\\\\3303ó" . "ó")
		    ("¾3" . "ó")
		    ("ó30ó" . "ó")
		    ("\\\\\\\\%" . "%")
		    ("\\\\\\\\3" . "ó")
		    ("ó42\\\\\\\\200\\\\\\\\230" . "“")
		    ("ó42\\\\\\\\200\\\\\\\\231" . "”")
		    ("ó03\\\\\\\\1" . "é")
		    ("ó03ó" . "ó")
		    ("u00f3" . "ó")
		    ("Ã³" . "ó")
		    ("Ã330ó" . "ó")
		    ("Ã©" . "é")
		    ("ó42\\\\\\\\200\\\\\\\\224" . "—")
		    ("ó\\\\\\\\241" . "á")
		    ("ó03\\\\\\\\261" . "ñ")
		    ("ó\\\\\\\\1" . "é")
		    ("ó42\\\\\\\\200\\\\\\\\242" . "•")
		    ("342\\\\\\\\200\\\\\\\\234" . "“")
		    ("\\\\\\\\251" . "")
		    ("237↩" . "")
		    ("\\\\\\\\´342´200´223" . "—")))
      (goto-char (point-min))
      (let ((search (car cons))
	    (replace (cdr cons)))
	(while (re-search-forward search nil t)
	  (replace-match replace t t))))))

(defun tlon-deepl-translate-encode (&optional no-glossary-ok)
  "Return a JSON representation of the text to be translated to language.
If NO-GLOSSARY-OK is non-nil, don't ask for confirmation when no glossary is
found."
  (let ((glossary-id (tlon-deepl-get-language-glossary tlon-deepl-target-language))
        (text (vector tlon-deepl-text)))
    (unless
	(or glossary-id
	    no-glossary-ok
	    (not (member tlon-deepl-target-language tlon-deepl-supported-glossary-languages))
	    (y-or-n-p (concat (if (string= "en" tlon-deepl-source-language)
				  (format "No \"en-%s\" glossary found." tlon-deepl-target-language)
				"Glossaries for source languages other than English are not currently supported.")
			      " Translate anyway?")))
      (user-error "Aborted"))
    (let ((json-encoding-pretty-print nil)
          (json-encoding-default-indentation "")
          (json-encoding-lisp-style-closings nil)
          (json-encoding-object-sort-predicate nil))
      (json-encode `(("text" . ,text)
                     ("source_lang" . ,tlon-deepl-source-language)
                     ("target_lang" . ,tlon-deepl-target-language)
                     ("glossary_id" . ,glossary-id))))))

(defun tlon-deepl-get-language-glossary (language)
  "Return the glossary ID for LANGUAGE.
Since we only have glossaries for English as the source language, return nil
when the source language is not English."
  (when (string= "en" tlon-deepl-source-language)
    (tlon-lookup tlon-deepl-glossaries "glossary_id" "target_lang" language)))

;;;;;; Tex

(declare-function ebib--get-key-at-point "ebib")
(declare-function bibtex-extras-get-field "bibtex-extras") ; Needed for interactive fallback
(declare-function ebib-extras-get-field "ebib-extras") ; Needed for lookup by key
;; `citar-extras-open-in-ebib' removed
;; `ebib-extras-get-file-of-key' removed
(declare-function tlon-tex-remove-braces "tlon-tex")
(declare-function tlon-translate-abstract-callback "tlon-tex")
(declare-function bibtex-extras-get-key "bibtex-extras")
(declare-function bibtex-extras-get-field "bibtex-extras") ; Needed for interactive fallback in tlon-deepl-translate-abstract
(declare-function ebib-extras-get-field "ebib-extras") ; Needed for interactive fallback in tlon-deepl-translate-abstract
(declare-function tlon-tex-get-keys-in-file "tlon-tex")
(declare-function tlon-bibliography-lookup "tlon-tex") ; Used by tlon-deepl--abstract-translation-missing-p (now removed) and tlon-deepl-translate-abstract (now removed)

;; Assume these are defined elsewhere
(defvar tlon-file-fluid)
(defvar tlon-file-stable)
(defvar tlon-project-target-languages)
(defvar tlon-ai-batch-fun)

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

(defun tlon-deepl--translate-abstract-interactive (key text source-lang-code)
  "Handle interactive abstract translation for KEY, TEXT, SOURCE-LANG-CODE."
  (let* ((excluded-lang (list (tlon-lookup tlon-languages-properties :standard :code source-lang-code)))
         (target-lang (tlon-select-language 'code 'babel "Target language: " 'require-match nil nil excluded-lang)))
    (when target-lang
      (message "Initiating DeepL translation for %s -> %s" key target-lang)
      (tlon-deepl-translate (tlon-tex-remove-braces text) target-lang source-lang-code
                            (lambda ()
                              (tlon-translate-abstract-callback key target-lang 'overwrite))
                            nil))))

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

;;;###autoload
(defun tlon-deepl-translate-missing-abstracts (&optional langs)
  "Translate abstracts for BibTeX entries missing translations into LANGS.
Iterate through all keys in `tlon-file-fluid' and `tlon-file-stable'. For each
key, check if abstract translations are missing for any language in LANGS. LANGS
is a list of languages, such as `(\"spanish\" \"french\")'. If LANGS is nil, use
`tlon-project-target-languages'. When a translation in a language is missing,
call `tlon-deepl-translate-abstract' for that key."
  (interactive)
  (let* ((keys (seq-uniq (append (tlon-tex-get-keys-in-file tlon-file-fluid)
                                 (tlon-tex-get-keys-in-file tlon-file-stable))))
	 (translations (tlon-read-abstract-translations))
         (total (length keys))
         (count 0)
         (processed 0))
    (message "Checking %d BibTeX entries for missing abstract translations..." total)
    (dolist (key keys)
      (setq processed (1+ processed))
      (when (tlon-deepl--abstract-translation-missing-p key langs translations)
        (message "Processing key (missing translations): %s (%d/%d)" key processed total)
        (setq count (1+ count))
        (let ((abstract (tlon-bibliography-lookup "=key=" key "abstract")))
          (tlon-deepl-translate-abstract abstract key langs))))
    (message "Finished checking %d entries. Initiated translation for %d entries." total count)))

(declare-function tlon-get-abstract-translation "tlon-tex")
(defun tlon-deepl--abstract-translation-missing-p (key &optional langs translations)
  "Return t iff abstract translation for KEY is missing for any language in LANGS.
Check if the source language and abstract exist, then iterate through
`tlon-project-target-languages' and return t if any language in LANGS (that is
not the source language) lacks a non-empty entry. LANGS is a list of languages,
such as `(\"spanish\" \"french\")'. If LANGS is nil, use
`tlon-project-target-languages'.

If TRANSLATIONS is nil, get them from `tlon-read-abstract-translations'."
  (let* ((source-lang-name (tlon-bibliography-lookup "=key=" key "langid"))
         (source-lang-code (when source-lang-name
                             (tlon-lookup tlon-languages-properties :code :name source-lang-name)))
         (abstract (tlon-bibliography-lookup "=key=" key "abstract"))
         missing-translation)
    (when (and abstract source-lang-code) ; Need abstract and source lang to translate
      (dolist (target-lang-name (or langs tlon-project-target-languages))
        (unless missing-translation ; Stop checking once one missing is found
          (let* ((target-lang-code (tlon-lookup tlon-languages-properties :code :name target-lang-name))
                 ;; Use the reliable file search function (now reverted to single field)
                 (translated-abstract (tlon-get-abstract-translation key target-lang-code translations)))
            (unless (or (string= source-lang-code target-lang-code) ; Don't need translation to source lang
                        (and translated-abstract
			     (> (length (string-trim translated-abstract)) 0))) ; Check if non-empty
              (setq missing-translation t)))))) ; Set flag if *any* translation is missing
    missing-translation))

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

(defun tlon-deepl-glossary-create-encode ()
  "Return a JSON representation of the glossary to be created."
  (let* ((extension "tsv")
	 (file (tlon-glossary-make-file (upcase tlon-deepl-target-language) extension))
	 (name (file-name-base file))
	 entries)
    (with-temp-buffer
      (insert-file-contents file)
      (setq entries (encode-coding-string (buffer-string) 'utf-8))
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

;;;;;; Delete glossary

;;;###autoload
(defun tlon-deepl-glossary-delete ()
  "Delete a DeepL glossary."
  (interactive)
  (tlon-deepl-request-wrapper 'glossary-delete))

(defun tlon-deepl-glossary-delete-formatter ()
  "URL formatter for `tlon-deepl-glossary-delete'."
  (concat tlon-deepl-url-prefix "glossaries/" (tlon-deepl-select-glossary)))

(defun tlon-deepl-glossary-delete-callback ()
  "Callback for `tlon-deepl-glossary-delete'."
  (tlon-deepl-get-glossaries)
  (message "Deleted glossary."))

;;;;; Menu

;;;###autoload (autoload 'tlon-deepl-menu "tlon-deepl" nil t)
(transient-define-prefix tlon-deepl-menu ()
  "DeepL menu."
  ["Translate"
   ("t" "Translate text" tlon-deepl-translate)
   ("a" "Translate abstract (current)" tlon-deepl-translate-abstract)
   ("m" "Translate missing abstracts" tlon-deepl-translate-missing-abstracts)
   ""
   "Glossaries"
   ("l" "List" tlon-deepl-select-glossary)
   ("g" "Retrieve" tlon-deepl-get-glossaries)
   ("c" "Create" tlon-deepl-glossary-create)
   ("d" "Delete" tlon-deepl-glossary-delete)
   ""
   ("e" "Ediff" tlon-deepl-diff)])

(provide 'tlon-deepl)
;;; tlon-deepl.el ends here

