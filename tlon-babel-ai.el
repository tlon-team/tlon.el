;;; tlon-babel-ai.el --- AI functionality for the Babel project -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon-babel
;; Version: 0.1

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

;; AI functionality for the Babel project.

;;; Code:

(require 'ebib-extras)
(require 'gptel)
(require 'gptel-extras)
(require 'tlon-babel)
(require 'tlon-babel-import)
(require 'tlon-babel-dispatch)

;;;; User options

(defgroup tlon-babel-ai nil
  "AI functionality for the Babel project."
  :group 'tlon-babel)

(defcustom tlon-babel-ai-batch-fun nil
  "Function to run in batch processing."
  :type 'symbol
  :group 'tlon-babel-ai)

(defcustom tlonl-babel-ai-model "gemini-pro"
  "AI model to use."
  :type 'string
  :group 'tlon-babel-ai)

;;;; Variables

(defvar tlon-babel-ai-retries 0
  "Number of retries for AI requests.")

(defconst tlon-babel-ai-string-wrapper
  ":\n\n```\n%s\n```\n\n"
  "Wrapper for strings to be passed in prompts.")

(defconst tlon-babel-gptel-error-message
  "`gptel' failed with message: %s"
  "Error message to display when `gptel-quick' fails.")

;;;;; language detection

(defconst tlon-babel-ai-detect-language-common-prompts
  (format ":%s. Your answer should just be the language of the entry. For example, if you conclude that the language is English, your answer should be just 'english'. Moreover, your answer can be only one of the following languages: %s" tlon-babel-ai-string-wrapper
	  (mapconcat 'identity (mapcar 'car bibtex-extras-valid-languages) ", "))
  "Common prompts for language detection.")

(defconst tlon-babel-ai-detect-language-prompt
  (format "Please guess the language of the following text%s"
	  tlon-babel-ai-detect-language-common-prompts)
  "Prompt for language detection.")

(defconst tlon-babel-ai-detect-language-bibtex-prompt
  (format "Please guess the language of the work described in following BibTeX entry%s"
	  tlon-babel-ai-detect-language-common-prompts)
  "Prompt for language detection.")

;;;;; translation

(defconst tlon-babel-ai-translate-prompt
  (format "Translate the following text into Spanish:%s" tlon-babel-ai-string-wrapper)
  "Prompt for translation.")

;; TODO: generalize to arbitrary langs
(defconst tlon-babel-ai-translate-variants-prompt
  (format "Please generate the best ten Spanish translations of the following English text:%s. Please return each translation on the same line, separated by '|'. Do not add a space either before or after the '|'. Do not precede your answer by 'Here are ten Spanish translations' or any comments of that sort: just return the translations. An example return string for the word 'very beautiful' would be: 'muy bello|muy bonito|muy hermoso|muy atractivo' (etc). Thanks!" tlon-babel-ai-string-wrapper)
  "Prompt for translation variants.")

;;;;; rewriting

(defconst tlon-babel-ai-rewrite-prompt
  (format "Por favor, genera las mejores diez variantes del siguiente texto castellano:%s. Por favor, devuelve todas las variantes en una única linea, separadas por '|'. No insertes un espacio ni antes ni después de '|'. No agregues ningún comentario aclaratorio: solo necesito la lista de variantes. A modo de ejemplo, para la expresión 'búsqueda de poder' el texto a devolver sería: 'ansia de poder|ambición de poder|búsqueda de autoridad|sed de poder|afán de poder|aspiración de poder|anhelo de poder|deseo de control|búsqueda de dominio|búsqueda de control' (esta lista solo pretende ilustrar el formato en que debes presentar tu respuesta). Gracias!" tlon-babel-ai-string-wrapper)
  "Prompt for rewriting.")

;;;;; summarization

(defconst tlon-babel-ai-how-to-write-summary-prompt
  `((:prompt "Write the abstract in a sober, objective tone, avoiding cliches, excessive praise and unnecessary flourishes. In other words, draft it as if you were writing the abstract of a scientific paper. The abstract should be only one paragraph long and have a rough length of 100 to 250 words (feel free to exceed it if you really need to, but never go over 350 words). It should not mention bibliographic data of the work (such as title or author). Write the abstract directly stating what the article argues, rather than using phrases such as 'The article argues that...'. For example, instead of writing 'The article ‘The eradication of smallpox’ by William D. Tierney tells that mankind fought smallpox for centuries...', write 'Mankind fought smallpox for centuries...'. Also, please omit any disclaimers of the form 'As an AI language model, I'm unable to browse the internet in real-time.' Finally, end your abstract with the phrase ' – AI-generated abstract.'"
	     :language "en")
    (:prompt "Redacta el resumen en un tono sobrio y objetivo, evitando los lugares comunes, los elogios excesivos y las florituras innecesarias. En otras palabras, redáctalo como si estuvieras escribiendo el resumen de un artículo científico. El resumen debe constar de un solo párrafo y tener una extensión de unas 100 a 250 palabras. No debe mencionar datos bibliográficos de la obra (como el título o el autor). Escribe el resumen indicando directamente lo que argumenta el artículo, en lugar de utilizar frases como ‘El artículo argumenta que...’. Por ejemplo, en lugar de escribir ‘El artículo 'La erradicación de la viruela' de William D. Tierney sostiene que la humanidad luchó contra la viruela durante siglos...’, escribe ‘La humanidad luchó contra la viruela durante siglos...’. Además, omite cualquier descargo de responsabilidad del tipo ‘Como modelo de lenguaje de inteligencia artificial, no puedo navegar por Internet en tiempo real.’ Por último, termina tu resumen con la frase ‘ - Resumen generado por inteligencia artificial.’"
	     :language "es"))
  "Prompts for summarization common elements.")

(defconst tlon-babel-ai-get-abstract-prompts
  `((:prompt ,(format "The following work may or may not contain an abstract:%s. If it contains an abstract, please return it. Otherwise, create an abstract of it yourself. %s However, please omit this phrase if you are simply copying verbatim an abstract you found in the work."
		      tlon-babel-ai-string-wrapper
		      (tlon-babel-lookup tlon-babel-ai-how-to-write-summary-prompt
					 :prompt :language "en"))
	     :language "en")
    (:prompt ,(format "La siguiente obra puede contener o no un resumen:%s. Si contiene un resumen, devuélvelo. En caso contrario, crea tú mismo un resumen. %s Sin embargo, omite esta frase si simplemente está devolviendo un resumen que encontraste en la obra.En otras palabras, incluye la frase sólo cuando tú hayas creado el resumen."
		      tlon-babel-ai-string-wrapper
		      (tlon-babel-lookup tlon-babel-ai-how-to-write-summary-prompt
					 :prompt :language "es"))
	     :language "es"))
  "Prompts for summarization.")

(defconst tlon-babel-ai-summarize-bibtex-prompts
  `((:prompt ,(format "Please fetch an abstract of the work described by the following BibTeX entry:%s. If the work has a DOI, you should get the abstract from the web page to which the DOI points. If it is an ISBN, you should get it from Worldcat, Amazon or the Library of Congress. Otherwise, try to find it from the other bibliographic information included in the entry. It is likely that there is already an abstract or summary of the work available online: use the URL, DOI or ISBN in the entry, or other fields if those are unavailable, to locate the abstract from an official or authoritative source (such as the journal in which the work was published or the Library of Congress entry). If you do find an abstract, copy it verbatim. Otherwise, please create one yourself. %s"
		      tlon-babel-ai-string-wrapper
		      (tlon-babel-lookup tlon-babel-ai-how-to-write-summary-prompt :prompt :language "en"))
	     :language "en")
    ;; TODO: update translation to match English version
    (:prompt ,(format "Por favor, genera un resumen del artículo que describe la siguiente entrada de BibTeX:%s. %s"
		      tlon-babel-ai-string-wrapper
		      (tlon-babel-lookup tlon-babel-ai-how-to-write-summary-prompt :prompt :language "es"))
	     :language "es"))
  "Prompts for BibTeX summarization.")

;;;; Functions

;;;;; General

(defun tlon-babel-make-gptel-request (prompt string &optional callback model)
  "Make a `gptel' request with PROMPT and STRING and CALLBACK.
MODEL is the language model. If CALLBACK is nil, use
`tlon-babel-ai-generic-callback'."
  (let ((callback (or callback #'tlon-babel-ai-generic-callback)))
    (gptel-extras-model-config (or model tlonl-babel-ai-model))
    (gptel-request (format prompt string) :callback callback)))

(defun tlon-babel-ai-generic-callback (response info)
  "Generic callback function for AI requests.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-babel-ai-callback-fail info)
    response))

(defun tlon-babel-ai-callback-fail (info)
  "Callback message when `gptel' fails.
INFO is the response info."
  (message tlon-babel-gptel-error-message (plist-get info :status)))

(defun tlon-babel-ai-batch-continue ()
  "Move to the next entry and call `tlon-babel-ai-batch-fun''."
  (when tlon-babel-ai-batch-fun
    (when (y-or-n-p "Continue? ")
      (pcase major-mode
	('bibtex-mode (bibtex-next-entry))
	('ebib-entry-mode (ebib-extras-next-entry)))
      (funcall tlon-babel-ai-batch-fun))))

(defun tlon-babel-ai-try-try-try-again (original-fun)
  "Call ORIGINAL-FUN up to three times if it its response is nil, then give up."
  (while (< tlon-babel-ai-retries 3)
    (setq tlon-babel-ai-retries (1+ tlon-babel-ai-retries))
    (message "Retrying language detection (try %d of 3)..." tlon-babel-ai-retries)
    (funcall original-fun)))

(defun tlon-babel-get-string-dwim (&optional file)
  "Return FILE, region or buffer as string, depending on major mode.
If FILE is non-nil, return it as a string. Otherwise,

- If in `bibtex-mode' or in `ebib-entry-mode', return the contents of the HTML
  or PDF file associated with the current BibTeX entry, if either is found.

- If in `text-mode', return the contents of the current region, if active;
  otherwise, return the contents of the current buffer."
  (if-let ((file (or file (when (derived-mode-p 'bibtex-mode 'ebib-entry-mode)
			    (ebib-extras-get-file "html") (ebib-extras-get-file "pdf")))))
      (with-temp-buffer
	(when (string= (file-name-extension file) "pdf")
	  (let ((markdown (make-temp-file "pdf-to-markdown-")))
	    (tlon-babel-convert-pdf file markdown)
	    (setq file markdown)))
	(insert-file-contents file)
	(when (string= (file-name-extension file) "html")
	  (shr-render-buffer (current-buffer)))
	(let ((result (buffer-substring-no-properties (point-min) (point-max))))
	  (kill-buffer)
	  result))
    (when (derived-mode-p 'text-mode)
      (let ((beg (if (region-active-p) (region-beginning) (point-min)))
	    (end (if (region-active-p) (region-end) (point-max))))
	(buffer-substring-no-properties beg end)))))

;;;;; Translation

;;;;;; Translation variants

;;;###autoload
(defun tlon-babel-ai-translate (string)
  "Return ten alternative translations of STRING."
  (interactive "sText to translate: ")
  (tlon-babel-make-gptel-request tlon-babel-ai-translate-variants-prompt string
				 #'tlon-babel-ai-translate-callback))

(defun tlon-babel-ai-translate-callback (response info)
  "Callback for `tlon-babel-ai-translate'.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-babel-ai-callback-fail info)
    (let ((translations (split-string response "|")))
      (kill-new (completing-read "Translation: " translations)))))

;;;;;; File translation

(defun tlon-babel-ai-translate-file (file)
  "Translate FILE."
  (let* ((string (with-temp-buffer
		   (insert-file-contents file)
		   (buffer-string))))
    (tlon-babel-make-gptel-request tlon-babel-ai-translate-prompt string
				   (lambda (response info)
				     (tlon-babel-ai-translate-file-callback response info file)))))

(defun tlon-babel-ai-translate-file-callback (response info file)
  "Callback for `tlon-babel-ai-translate-file'.
RESPONSE is the response from the AI model and INFO is the response info. FILE
is the file to translate."
  (if (not response)
      (tlon-babel-ai-callback-fail info)
    (let* ((counterpart (tlon-babel-get-counterpart file))
	   (filename (file-name-nondirectory counterpart))
	   (target-path (concat
			 (file-name-sans-extension filename)
			 "--ai-translated.md")))
      (with-temp-buffer
	(insert response)
	(write-region (point-min) (point-max) target-path)))))

;;;;; Rewriting

;;;###autoload
(defun tlon-babel-ai-rewrite ()
  "Docstring."
  (interactive)
  (let* ((string (if (region-active-p)
		     (buffer-substring-no-properties (region-beginning) (region-end))
		   (read-string "Text to rewrite: "))))
    (tlon-babel-make-gptel-request tlon-babel-ai-rewrite-prompt string)))

(defun tlon-babel-ai-rewrite-callback (response info)
  "Callback for `tlon-babel-ai-rewrite'.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-babel-ai-callback-fail info)
    (let* ((variants (split-string response "|"))
	   (variant (completing-read "Variant: " variants)))
      (delete-region (region-beginning) (region-end))
      (kill-new variant))))

;;;;; Summarization

;;;###autoload
  "Try to fetch a summary; if unsuccessful, create one.
To fetch a summary, the function uses `tlon-babel-fetch-and-set-abstract'. See
its docstring for details.
(defun tlon-babel-get-abstract-with-or-without-ai ()

To create a summary, the function uses `tlon-babel-ai-summarize-dwim'. See its
docstring for details."
  (interactive)
  (unless (tlon-babel-fetch-and-set-abstract)
    (message "Could not fetch summary; creating one...")
    (tlon-babel-get-abstract-with-ai)))

;;;###autoload
  "Summarize the relevant content with AI.
If FILE is non-nil, summarize its contents. Otherwise,
(defun tlon-babel-get-abstract-with-ai (&optional file model)

- If in `bibtex-mode' or in `ebib-entry-mode', summarize the contents of the
  HTML or PDF file associated with the current BibTeX entry, if either is found.

- If in `text-mode', summarize the contents of the current region, if active;
  otherwise, summarize the contents of the current buffer.

 If MODEL is nil, get it from `tlonl-babel-ai-model'."
  (interactive)
  (if-let ((language (or (tlon-babel-ai-get-language-in-file file)
			 (unless tlon-babel-ai-batch-fun
			   (tlon-babel-ai-select-language)))))
      (tlon-babel-ai-get-abstract-in-language file language model)
    (tlon-babel-ai-detect-language-in-file
     file
     (lambda (response info)
       (tlon-babel-ai-get-abstract-from-detected-language response info file model)))))

  "Actually summarize FILE in LANGUAGE with MODEL."
(defun tlon-babel-get-abstract-with-ai-from-html ()
  (if-let ((string (tlon-babel-get-string-dwim file))
	   (lang-2 (tlon-babel-get-two-letter-code language))
	   (original-buffer (current-buffer)))
      (tlon-babel-ai-summarize-common
       tlon-babel-ai-summarize-prompts string lang-2
       (lambda (response info)
	 ;; we restore the original buffer to avoid a change in `major-mode'
	 (with-current-buffer original-buffer
	   (tlon-babel-ai-summarize-callback response info)))
       model)
    (tlon-babel-ai-batch-continue)))

  "If RESPONSE is non-nil, initiate a summary of FILE with MODEL.
(defun tlon-babel-ai-get-abstract-from-detected-language (response info file model)
Otherwise return INFO."
  (if (not response)
      (tlon-babel-ai-callback-fail info)
    (tlon-babel-ai-get-abstract-in-language file response model)))

(defun tlon-babel-ai-summarize-common (prompts string language callback model)
  "Common function for summarization.
PROMPTS is the prompts to use, STRING is the string to summarize, LANGUAGE is
the language of the string, and CALLBACK is the callback function. MODEL is the
language model."
  (let ((prompt (tlon-babel-lookup prompts :prompt :language language)))
    (message "Generating output. This may take 5–30 seconds, depending on length...")
    (tlon-babel-make-gptel-request prompt string callback model)))

(defun tlon-babel-ai-summarize-callback (response info)
  "If RESPONSE is non-nil, take appropriate action based on major mode.
If RESPONSE is nil, return INFO."
  (if (not response)
      (tlon-babel-ai-callback-fail info)
    (pcase major-mode
      ((or 'bibtex-mode 'ebib-entry-mode)
       (tlon-babel-ai-summarize-set-bibtex-abstract response))
      ('markdown-mode) ; set `description' YAML field to it
      (_ (kill-new response)
	 (message "Copied AI-generated summary to the kill ring:\n\n%s" response))))
  (tlon-babel-ai-batch-continue))

;;;;;; BibTeX

;;;###autoload
(defun tlon-babel-ai-summarize-bibtex-entry (&optional string)
  "Summarize the work described in the BibTeX STRING using AI.
If STRING is nil, use the current entry."
  (interactive)
  (let* ((get-string (pcase major-mode
		       ('bibtex-mode #'tlon-babel-get-entry-as-string)
		       ('ebib-entry-mode #'ebib-extras-get-or-open-entry)
		       (_ (user-error "Unsupported major mode"))))
	 (string (or string (funcall get-string))))
    (unless (tlon-babel-get-field-in-string string "abstract")
      (when-let* ((get-lang (pcase major-mode
			      ('bibtex-mode #'tlon-babel-get-field)
			      ('ebib-entry-mode #'ebib-extras-get-field)))
		  (language (funcall get-lang "langid"))
		  (lang-short (tlon-babel-get-two-letter-code language)))
	(if-let ((prompt (tlon-babel-lookup tlon-babel-ai-summarize-bibtex-prompts :prompt :language lang-short)))
	    (tlon-babel-make-gptel-request prompt string #'tlon-babel-ai-summarize-callback)
	  (user-error "No prompt defined in `tlon-babel-ai-summarize-prompts' for language %s" language))))))

(defun tlon-babel-ai-summarize-set-bibtex-abstract (abstract)
  "Set the `abstract' field of the current BibTeX entry to ABSTRACT."
  (let* ((key (pcase major-mode
		('bibtex-mode #'bibtex-extras-get-key)
		('ebib-entry-mode (ebib-extras-get-field "=key="))))
	 (set-field (pcase major-mode
		      ('bibtex-mode #'bibtex-set-field)
		      ('ebib-entry-mode #'ebib-extras-set-field))))
    (funcall set-field "abstract" abstract)
    (message "Set abstract of `%s' to %s" key abstract)
    (save-buffer)))

;;;;; Language detection

(defun tlon-babel-ai-get-language-in-file (&optional file)
  "Return the language in FILE, based on the major mode.
If FILE is nil, get the language in the current buffer or entry, depending on
the major mode."
  (pcase major-mode
    ('ebib-entry-mode (ebib-extras-get-field "langid"))
    ('bibtex-mode (tlon-babel-get-field "langid"))
    ('markdown-mode
     (let* ((file (or file (buffer-file-name)))
	    (repo (tlon-babel-get-repo-from-file file)))
       (tlon-babel-repo-lookup :language :dir repo)))))

(defun tlon-babel-ai-detect-language-in-file (&optional file callback)
  "Detect the language in FILE and call CALLBACK.
If FILE is nil, detect the language in the current buffer."
  (let ((string (tlon-babel-get-string-dwim file)))
    (tlon-babel-make-gptel-request tlon-babel-ai-detect-language-prompt string callback)))

(defun tlon-babel-ai-select-language ()
  "Prompt the user to select a LANGUAGE and return it."
  (completing-read "Language: " bibtex-extras-valid-languages))

;;;;;; BibTeX

(defun tlon-babel-ai-detect-language-in-bibtex (&optional string)
  "Detect language in STRING.
If STRING is nil, use the current BibTeX entry."
  (let ((string (or string (pcase major-mode
			     ('ebib-entry-mode (ebib-extras-get-or-open-entry))
			     ('bibtex-mode (tlon-babel-get-entry-as-string))
			     (_ (user-error "I can’t detect language in %s" major-mode))))))
    (tlon-babel-make-gptel-request tlon-babel-ai-detect-language-bibtex-prompt string)))

;;;###autoload
(defun tlon-babel-ai-set-language-bibtex ()
  "Set the language of the BibTeX entry at point to LANGUAGE.
If STRING is nil, use the current entry."
  (interactive)
  (let* ((string (tlon-babel-get-entry-as-string))
	 (callback (if (tlon-babel-get-field-in-string string "langid")
		       #'tlon-babel-ai-set-language-bibtex-when-present-callback
		     #'tlon-babel-ai-set-language-bibtex-when-absent-callback)))
    (tlon-babel-make-gptel-request tlon-babel-ai-detect-language-bibtex-prompt string callback)))

(defun tlon-babel-ai-set-language-bibtex-when-present-callback (response info)
  "Callback for `tlon-babel-ai-set-language-bibtex' when `langid' field is present.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-babel-ai-callback-fail info)
    (bibtex-beginning-of-entry)
    (if-let ((langid (tlon-babel-get-field "langid"))
	     (valid-langid (tlon-babel-validate-language langid))
	     (valid-response (tlon-babel-validate-language response)))
	(if (string= valid-langid valid-response)
	    (tlon-babel-ai-set-language-bibtex-when-equal valid-langid langid)
	  (let ((langid-2 (tlon-babel-get-two-letter-code valid-langid))
		(response-2 (tlon-babel-get-two-letter-code valid-response)))
	    (if (string= langid-2 response-2)
		(tlon-babel-ai-set-language-bibtex-when-equal valid-langid langid)
	      (tlon-babel-ai-set-language-bibtex-when-conflict langid response))))
      (user-error "The `langid' field of the current entry is not valid"))))

(defun tlon-babel-ai-set-language-bibtex-when-absent-callback (response info)
  "Callback for `tlon-babel-ai-set-language-bibtex' when `langid' field is absent.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-babel-ai-callback-fail info)
    (when-let ((language (tlon-babel-validate-language response)))
      (tlon-babel-ai-set-language-bibtex-add-langid language))))

(defun tlon-babel-ai-set-language-bibtex-when-equal (valid-lang lang)
  "Set language depending on whether VALID-LANG and LANG are equal."
  (if (string= valid-lang lang)
      (tlon-babel-ai-batch-continue)
    (tlon-babel-ai-set-language-bibtex-add-langid valid-lang)))

(defun tlon-babel-ai-set-language-bibtex-when-conflict (current detected)
  "Prompt the user to resolve a conflict between CURRENT and DETECTED languages."
  (let ((selection
	 (completing-read
	  (format
	   "The detected language (%s) differs from `langid' language (%s). Which one should we use? "
	   detected current)
	  (list current detected) nil t)))
    (tlon-babel-ai-set-language-bibtex-add-langid selection)))

(defun tlon-babel-ai-set-language-bibtex-add-langid (lang)
  "Set the value of `langid' to LANG."
  (let ((key (bibtex-extras-get-key)))
    (bibtex-set-field "langid" lang)
    (message "Set language of `%s' to %s" key lang)
    (tlon-babel-ai-batch-continue)))

;;;;; Menu

(defun tlon-babel-ai-batch-fun-reader (prompt _ _)
  "Return a list of choices with PROMPT to be used as an `infix' reader function."
  (tlon-babel-transient-read-choice prompt '(tlon-babel-get-abstract-with-or-without-ai
					     tlon-babel-ai-set-language-bibtex
					     nil)))

(transient-define-infix tlon-babel-ai-batch-infix ()
  "Change the local value of the `'tlon-babel-ai-batch-fun' variable."
  :class 'transient-lisp-variable
  :reader 'tlon-babel-ai-batch-fun-reader
  :transient t
  :prompt "Function for batch-processing: "
  :variable 'tlon-babel-ai-batch-fun)

(defun tlon-babel-abstract-overwrite-reader (prompt _ _)
  "Return a list of choices with PROMPT to be used as an `infix' reader function."
  (tlon-babel-transient-read-choice prompt '(always prompt never)))

(transient-define-infix tlon-babel-fetch-and-set-abstract-infix ()
  "Change the local value of the `tlon-babel-abstract-overwrite' variable."
  :class 'transient-lisp-variable
  :reader 'tlon-babel-abstract-overwrite-reader
  :transient t
  :prompt "Overwrite when the entry already contains an abstract? "
  :variable 'tlon-babel-abstract-overwrite)

;;;###autoload (autoload 'tlon-babel-ai-menu "'tlon-babel-ai" nil t)
(transient-define-prefix tlon-babel-ai-menu ()
  "Menu for `tlon-babel-ai'."
  [[("t" "translate"                   tlon-babel-ai-translate)]
   [("r" "rewrite"                     tlon-babel-ai-rewrite)]
    ("s b" "summarize bibtex"          tlon-babel-ai-summarize-bibtex-entry)]
   [("g g" "set language bibtex"       tlon-babel-ai-set-language-bibtex)]
    ("s s" "get abstract with or without AI"    tlon-babel-get-abstract-with-or-without-ai)
    ("s a" "get abstract with AI"               tlon-babel-get-abstract-with-ai)
   ["options"
    ("-b" "batch"                      tlon-babel-ai-batch-infix)
    ("-o" "overwrite"                  tlon-babel-fetch-and-set-abstract-infix)]])

(provide 'tlon-babel-ai)
;;; tlon-babel-ai.el ends here

;; Local Variables:
;; jinx-languages: "es en"
;; End:
