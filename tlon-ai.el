;;; tlon-ai.el --- AI functionality for the Babel project -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon
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

(require 'gptel)
(require 'gptel-extras)
(require 'shut-up)
(require 'tlon-core)

;;;; User options

(defgroup tlon-ai nil
  "AI functionality for the Babel project."
  :group 'tlon)

(defcustom tlon-ai-batch-fun nil
  "Function to run in batch mode."
  :type 'symbol
  :group 'tlon-ai)

;;;; Variables

(defvar tlon-ai-retries 0
  "Number of retries for AI requests.")

(defconst tlon-ai-string-wrapper
  ":\n\n```\n%s\n```\n\n"
  "Wrapper for strings to be passed in prompts.")

(defconst tlon-gptel-error-message
  "`gptel' failed with message: %s"
  "Error message to display when `gptel-quick' fails.")

;;;;; Language detection

(defconst tlon-ai-detect-language-common-prompts
  (format ":%s. Your answer should just be the language of the entry. For example, if you conclude that the language is English, your answer should be just 'english'. Moreover, your answer can be only one of the following languages: %s"
	  tlon-ai-string-wrapper
	  (mapconcat 'identity (mapcar (lambda (language)
					 (plist-get language :name))
				       tlon-languages-properties)
		     ", "))
  "Common prompts for language detection.")

(defconst tlon-ai-detect-language-prompt
  (format "Please guess the language of the following text%s"
	  tlon-ai-detect-language-common-prompts)
  "Prompt for language detection.")

(defconst tlon-ai-detect-language-bibtex-prompt
  (format "Please guess the language of the work described in following BibTeX entry%s"
	  tlon-ai-detect-language-common-prompts)
  "Prompt for language detection.")

;;;;; Translation

(defconst tlon-ai-translate-prompt
  (format "Translate the following text into Spanish:%s" tlon-ai-string-wrapper)
  "Prompt for translation.")

;; TODO: generalize to arbitrary langs
(defconst tlon-ai-translate-variants-prompt
  (format "Please generate the best ten Spanish translations of the following English text:%s. Please return each translation on the same line, separated by '|'. Do not add a space either before or after the '|'. Do not precede your answer by 'Here are ten Spanish translations' or any comments of that sort: just return the translations. An example return string for the word 'very beautiful' would be: 'muy bello|muy bonito|muy hermoso|muy atractivo' (etc). Thanks!" tlon-ai-string-wrapper)
  "Prompt for translation variants.")

;;;;; Rewriting

(defconst tlon-ai-rewrite-prompt
  (format "Por favor, genera las mejores diez variantes del siguiente texto castellano:%s. Por favor, devuelve todas las variantes en una única linea, separadas por '|'. No insertes un espacio ni antes ni después de '|'. No agregues ningún comentario aclaratorio: solo necesito la lista de variantes. A modo de ejemplo, para la expresión 'búsqueda de poder' el texto a devolver sería: 'ansia de poder|ambición de poder|búsqueda de autoridad|sed de poder|afán de poder|aspiración de poder|anhelo de poder|deseo de control|búsqueda de dominio|búsqueda de control' (esta lista solo pretende ilustrar el formato en que debes presentar tu respuesta). Gracias!" tlon-ai-string-wrapper)
  "Prompt for rewriting.")

;;;;; Image description

;; consider using `chatgpt-shell-describe-image'
(defconst tlon-ai-describe-image-prompt
  `((:prompt "Please provide a concise description of the following image:\n\n[[file:%s]]\n\nThe description should consist of only one paragraph and must never exceed 80 words."
	     :language "en")
    (:prompt "Por favor, describe brevemente la siguiente imagen:\n\n[[file:%s]]\n\nLa descripción debe consistir de un solo párrafo y en ningún caso debe exceder las 80 palabras."
	     :language "es")))

;;;;; Summarization

(defconst tlon-ai-how-to-write-summary-prompt
  `((:prompt "Write the abstract in a sober, objective tone, avoiding cliches, excessive praise and unnecessary flourishes. In other words, draft it as if you were writing the abstract of a scientific paper. The abstract should be only one paragraph long and have a rough length of 100 to 250 words (feel free to exceed it if you really need to, but never go over 350 words). It should not mention bibliographic data of the work (such as title or author). Write the abstract directly stating what the article argues, rather than using phrases such as 'The article argues that...'. For example, instead of writing 'The article ‘The eradication of smallpox’ by William D. Tierney tells that mankind fought smallpox for centuries...', write 'Mankind fought smallpox for centuries...'. Also, please omit any disclaimers of the form 'As an AI language model, I'm unable to browse the internet in real-time.' Finally, end your abstract with the phrase ' – AI-generated abstract.'"
	     :language "en")
    (:prompt "Redacta el resumen en un tono sobrio y objetivo, evitando los lugares comunes, los elogios excesivos y las florituras innecesarias. En otras palabras, redáctalo como si estuvieras escribiendo el resumen de un artículo científico. El resumen debe constar de un solo párrafo y tener una extensión de unas 100 a 250 palabras. No debe mencionar datos bibliográficos de la obra (como el título o el autor). Escribe el resumen indicando directamente lo que argumenta el artículo, en lugar de utilizar frases como ‘El artículo argumenta que...’. Por ejemplo, en lugar de escribir ‘El artículo 'La erradicación de la viruela' de William D. Tierney sostiene que la humanidad luchó contra la viruela durante siglos...’, escribe ‘La humanidad luchó contra la viruela durante siglos...’. Además, omite cualquier descargo de responsabilidad del tipo ‘Como modelo de lenguaje de inteligencia artificial, no puedo navegar por Internet en tiempo real.’ Por último, termina tu resumen con la frase ‘ - Resumen generado por inteligencia artificial.’"
	     :language "es")
    (:prompt "Rédigez le résumé sur un ton sobre et objectif, en évitant les clichés, les éloges excessifs et les fioritures inutiles. En d'autres termes, rédigez-le comme si vous écriviez le résumé d'un article scientifique. Le résumé ne doit comporter qu'un seul paragraphe et avoir une longueur approximative de 100 à 250 mots (n'hésitez pas à le dépasser si vous en avez vraiment besoin, mais ne dépassez jamais 350 mots). Il ne doit pas mentionner les données bibliographiques de l'ouvrage (telles que le titre ou l'auteur). Rédigez le résumé en indiquant directement ce que l'article soutient, plutôt qu'en utilisant des phrases telles que 'L'article soutient que...'. Par exemple, au lieu d'écrire 'L'article 'L'éradication de la variole' de William D. Tierney affirme que l'humanité a combattu la variole pendant des siècles...', écrivez 'L'humanité a combattu la variole pendant des siècles...'. Veuillez également omettre toute clause de non-responsabilité du type 'En tant que modèle linguistique de l'IA, je ne suis pas en mesure de naviguer sur l'internet en temps réel'. Enfin, terminez votre résumé par la phrase ' - Résumé généré par l'IA.'"
	     :language "fr")
    (:prompt "Schreiben Sie die Zusammenfassung in einem nüchternen, sachlichen Ton und vermeiden Sie Klischees, übermäßiges Lob und unnötige Schnörkel. Mit anderen Worten: Verfassen Sie sie so, als ob Sie die Zusammenfassung einer wissenschaftlichen Arbeit schreiben würden. Die Zusammenfassung sollte nur einen Absatz lang sein und eine ungefähre Länge von 100 bis 250 Wörtern haben (Sie können diese Zahl ruhig überschreiten, wenn es wirklich nötig ist, aber nie mehr als 350 Wörter). Sie sollte keine bibliografischen Daten der Arbeit (wie Titel oder Autor) enthalten. Geben Sie in der Zusammenfassung direkt an, worum es in dem Artikel geht, und verwenden Sie keine Sätze wie 'In dem Artikel wird argumentiert, dass...'. Schreiben Sie zum Beispiel statt 'Der Artikel 'Die Ausrottung der Pocken' von William D. Tierney besagt, dass die Menschheit jahrhundertelang die Pocken bekämpfte...' lieber 'Die Menschheit bekämpfte die Pocken jahrhundertelang...'. Lassen Sie bitte auch Haftungsausschlüsse der Form 'Als KI-Sprachmodell bin ich nicht in der Lage, das Internet in Echtzeit zu durchsuchen' weg. Beenden Sie Ihre Zusammenfassung schließlich mit dem Satz ' - KI-generierte Zusammenfassung.'"
	     :language "de")))

(defconst tlon-ai-get-abstract-prompts
  `((:prompt ,(format "The following work may or may not contain an abstract:%s. If it contains an abstract, please return it. Otherwise, create an abstract of it yourself. %s However, please omit this phrase if you are simply copying verbatim an abstract you found in the work."
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-summary-prompt
					 :prompt :language "en"))
	     :language "en")
    (:prompt ,(format "La siguiente obra puede contener o no un resumen:%s. Si contiene un resumen, devuélvelo. En caso contrario, crea tú mismo un resumen. %s Sin embargo, omite esta frase si simplemente está devolviendo un resumen que encontraste en la obra.En otras palabras, incluye la frase sólo cuando tú hayas creado el resumen."
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-summary-prompt
					 :prompt :language "es"))
	     :language "es")
    (:prompt ,(format "L'œuvre suivante peut ou non contenir un résumé:%s. S'il contient un résumé, veuillez le renvoyer. Sinon, créez un résumé vous-même. %s Toutefois, veuillez omettre cette phrase si vous ne faites que copier mot pour mot un résumé que vous avez trouvé dans l'œuvre."
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-summary-prompt
					 :prompt :language "fr"))
	     :language "fr")
    (:prompt ,(format "Das folgende Werk kann eine Zusammenfassung enthalten oder auch nicht: %s. Wenn es eine Zusammenfassung enthält, geben Sie sie bitte zurück. Andernfalls erstellen Sie bitte selbst eine Zusammenfassung des Werks. %s Bitte lassen Sie diesen Satz jedoch weg, wenn Sie einfach eine wortwörtliche Zusammenfassung kopieren, die Sie in dem Werk gefunden haben."
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-summary-prompt
					 :prompt :language "de"))
	     :language "de"))
  "Prompts for summarization.")

(defconst tlon-ai-summarize-bibtex-prompts
  `((:prompt ,(format "Please fetch an abstract of the work described by the following BibTeX entry:%s. If the work has a DOI, you should get the abstract from the web page to which the DOI points. If it is an ISBN, you should get it from Worldcat, Amazon or the Library of Congress. Otherwise, try to find it from the other bibliographic information included in the entry. It is likely that there is already an abstract or summary of the work available online: use the URL, DOI or ISBN in the entry, or other fields if those are unavailable, to locate the abstract from an official or authoritative source (such as the journal in which the work was published or the Library of Congress entry). If you do find an abstract, copy it verbatim. Otherwise, please create one yourself. %s"
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-summary-prompt :prompt :language "en"))
	     :language "en")
    ;; TODO: update translation to match English version
    (:prompt ,(format "Por favor, genera un resumen del artículo que describe la siguiente entrada de BibTeX:%s. %s"
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-summary-prompt :prompt :language "es"))
	     :language "es"))
  "Prompts for BibTeX summarization.")

;;;;; Phonetic transcription

(defconst tlon-ai-transcribe-phonetically-prompt
  `((:prompt ,(format "Please transcribe the following text phonetically, i.e. using the International Phonetic Alphabet (IPA).%sJust return the phonetic transcription, without any commentary. Do not enclose the transcription in slashes." tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Por favor, transcribe fonéticamente el siguiente texto, es decir, utilizando el Alfabeto Fonético Internacional (AFI).%sLimítate a devolver la transcripción fonética, sin comentarios de ningún tipo. No encierres la transcripción entre barras." tlon-ai-string-wrapper)
	     :language "es")))

;;;;; Math

(defconst tlon-ai-translate-math-prompt
  `((:prompt ,(format "Please translate this math expression to natural language, i.e. as a human would read it:%s For example, if the expression is `\\frac{1}{2} \\times 2^5 \\= 16`, you should translate \"one half times two to the fifth power equals sixteen\". The expression may not require any sophisticated treatment. For example, if I ask you to translate a letter (such as `S`), your “translation” should be that same letter. Please return only the translated expression, without comments or clarifications. If for some reason you cannot do what I ask, simply do not respond at all; in no case should you return messages such as 'I could not translate the expression' or 'Please include the mathematical expression you need me to translate.'" tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Por favor traduce esta expresión matemática a lenguaje natural, es decir, a la manera en que un humano la leería en voz alta:%sPor ejemplo, si la expresión es `\\frac{1}{2} \\times 2^5 \\= 16`, debes traducir \"un medio por dos a la quinta potencia es igual a dieciseis\". Es posible que la expresión no requiera ningún tratamiento sofisticado. Por ejemplo, si te pido que traduzcas una letra (como `S`), tu “traducción” debería ser esa misma letra (`ese`). Por favor, devuelve solamente la expresión traducida, sin comentarios ni clarificaciones. Si por alguna razón no puedes hacer lo que te pido, simplemente no respondas nada; en ningún caso debes devolver mensajes como ‘No he podido traducir la expresión’ o ‘Por favor, incluye la expresión matemática que necesitas que traduzca.’" tlon-ai-string-wrapper)
	     :language "es")))

;;;; Functions

;;;;; General

(defun tlon-make-gptel-request (prompt string &optional callback backend model)
  "Make a `gptel' request with PROMPT and STRING and CALLBACK.
BACKEND and MODEL are the language backend and model names, respectively."
  (when (and backend model)
    (gptel-extras-model-config nil backend model))
  (if tlon-ai-batch-fun
      (condition-case nil
	  (gptel-request (format prompt string) :callback callback)
	(error nil))
    (gptel-request (format prompt string) :callback callback)))

;;;;;; Generic callback functions

(defun tlon-ai-callback-return (response info)
  "If the request succeeds, return the RESPONSE string.
Otherwise emit a message with the status provided by INFO."
  (if (not response)
      (tlon-ai-callback-fail info)
    response))

(defun tlon-ai-callback-copy (response info)
  "If the request succeeds, copy the RESPONSE to the kill ring.
Otherwise emit a message with the status provided by INFO."
  (if (not response)
      (tlon-ai-callback-fail info)
    (kill-new response)
    (message "Copied `%s'" response)))

;; Is this necessary; I think `gptel-request' already does this
;; if no callback is passed to it
(defun tlon-ai-callback-insert (response info)
  "If the request succeeds, insert the RESPONSE string.
Otherwise emit a message with the status provided by INFO. The RESPONSE is
inserted at the point the request was sent."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let ((pos (marker-position (plist-get info :position))))
      (goto-char pos)
      (insert response))))

(defun tlon-ai-callback-fail (info)
  "Callback message when `gptel' fails.
INFO is the response info."
  (message tlon-gptel-error-message (plist-get info :status)))

;;;;;; Other functions

(declare-function bibtex-next-entry "bibtex")
(declare-function bibtex-extras-get-key "bibtex-extras")
(declare-function ebib-extras-next-entry "ebib-extras")
(declare-function ebib-extras-get-field "ebib-extras")
(defun tlon-ai-batch-continue ()
  "Move to the next entry and call `tlon-ai-batch-fun''."
  (when tlon-ai-batch-fun
    (message "Moving point to `%s'."
	     (pcase major-mode
	       ('bibtex-mode (bibtex-next-entry)
			     (bibtex-extras-get-key))
	       ('ebib-entry-mode (ebib-extras-next-entry)
				 (ebib-extras-get-field "=key="))))
    (funcall tlon-ai-batch-fun)))

(defun tlon-ai-try-try-try-again (original-fun)
  "Call ORIGINAL-FUN up to three times if it its response is nil, then give up."
  (while (< tlon-ai-retries 3)
    (setq tlon-ai-retries (1+ tlon-ai-retries))
    (message "Retrying language detection (try %d of 3)..." tlon-ai-retries)
    (funcall original-fun)))

(declare-function ebib-extras-get-file "ebib-extras")
(declare-function tlon-md-read-content "tlon-md")
(defun tlon-get-string-dwim (&optional file)
  "Return FILE, region or buffer as string, depending on major mode.
If FILE is non-nil, return it as a string or, if in `markdown-mode', return a
substring of its substantive contents, excluding metadata and local variables.
Otherwise,

- If the region is active, return its contents.

- If in `bibtex-mode' or in `ebib-entry-mode', return the contents of the HTML
  or PDF file associated with the current BibTeX entry, if either is found.

- If in `pdf-view-mode', return the contents of the current PDF file.

- If in `markdown-mode', return the substantive contents of the current buffer.

- If otherwise in `text-mode', return the contents of the current buffer."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if-let ((file (or file (pcase major-mode
			      ((or 'bibtex-mode 'ebib-entry-mode)
			       (or (ebib-extras-get-file "html")
				   (ebib-extras-get-file "pdf")))
			      ('pdf-view-mode (buffer-file-name))))))
	(tlon-get-file-as-string file)
      (cond ((derived-mode-p 'markdown-mode)
	     (tlon-md-read-content file))
	    ((derived-mode-p 'text-mode)
	     (buffer-substring-no-properties (point-min) (point-max)))))))

(declare-function shr-render-buffer "shr")
(declare-function tlon-convert-pdf "tlon-import")
(defun tlon-get-file-as-string (file)
  "Get the contents of FILE as a string."
  (with-temp-buffer
    (when (string= (file-name-extension file) "pdf")
      (let ((markdown (make-temp-file "pdf-to-markdown-")))
	(tlon-convert-pdf file markdown)
	(setq file markdown)))
    (insert-file-contents file)
    (when (string= (file-name-extension file) "html")
      (shr-render-buffer (current-buffer)))
    (let ((result (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer)
      result)))

;;;;; Translation

;;;;;; Translation variants

;;;###autoload
(defun tlon-ai-translate (string)
  "Return ten alternative translations of STRING."
  (interactive "sText to translate: ")
  (tlon-make-gptel-request tlon-ai-translate-variants-prompt string
				 #'tlon-ai-translate-callback))

(defun tlon-ai-translate-callback (response info)
  "Callback for `tlon-ai-translate'.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let ((translations (split-string response "|")))
      (kill-new (completing-read "Translation: " translations)))))

;;;;;; File translation

(defun tlon-ai-translate-file (file)
  "Translate FILE."
  (let* ((string (with-temp-buffer
		   (insert-file-contents file)
		   (buffer-string))))
    (tlon-make-gptel-request tlon-ai-translate-prompt string
			     (lambda (response info)
			       (tlon-ai-translate-file-callback response info file)))))

(declare-function tlon-get-counterpart "tlon-counterpart")
(defun tlon-ai-translate-file-callback (response info file)
  "Callback for `tlon-ai-translate-file'.
RESPONSE is the response from the AI model and INFO is the response info. FILE
is the file to translate."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let* ((counterpart (tlon-get-counterpart file))
	   (filename (file-name-nondirectory counterpart))
	   (target-path (concat
			 (file-name-sans-extension filename)
			 "--ai-translated.md")))
      (with-temp-buffer
	(insert response)
	(write-region (point-min) (point-max) target-path)))))

;;;;; Rewriting

;;;###autoload
(defun tlon-ai-rewrite ()
  "Docstring."
  (interactive)
  (let* ((string (if (region-active-p)
		     (buffer-substring-no-properties (region-beginning) (region-end))
		   (read-string "Text to rewrite: "))))
    (tlon-make-gptel-request tlon-ai-rewrite-prompt string
				   #'tlon-ai-callback-return)))

(defun tlon-ai-rewrite-callback (response info)
  "Callback for `tlon-ai-rewrite'.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let* ((variants (split-string response "|"))
	   (variant (completing-read "Variant: " variants)))
      (delete-region (region-beginning) (region-end))
      (kill-new variant))))

;;;;; Image description

;;;###autoload
(defun tlon-ai-describe-image (file callback &optional language)
  "Describe the contents of the image in FILE.
When the description is obtained, pass it to CALLBACK as its first argument. Use
LANGUAGE for the description; if nil, obtain the language from the current repo."
  (interactive (list (read-file-name "Image file: " )))
  (let* ((file (expand-file-name file))
	 (language (or language (tlon-repo-lookup :language :dir (tlon-get-repo))))
	 (prompt (format
		  (tlon-lookup tlon-ai-describe-image-prompt :prompt :language language)
		  file))
	 (buffer (generate-new-buffer "*Image Description*")))
    (with-current-buffer buffer
      (insert prompt)
      (org-mode)
      (gptel-extras-model-config nil "ChatGPT" "gpt-4-vision-preview")
      (gptel-send)
      (message "Generating image description. This will take around 20 seconds...")
      (run-with-timer 20 nil (lambda ()
			       (tlon-return-image-description buffer callback))))))

(defun tlon-return-image-description (buffer callback)
  "Get the image description from BUFFER and pass it to CALLBACK."
  (let ((description
	 (with-current-buffer buffer
	   (redisplay)
	   (goto-char (point-min))
	   (forward-line 6)
	   (redisplay)
	   (buffer-substring-no-properties (point) (point-max)))))
    (kill-buffer buffer)
    (funcall callback (replace-regexp-in-string "\n" " " description))))

;;;;; Summarization

(declare-function tlon-fetch-and-set-abstract "tlon-tex")
;;;###autoload
(defun tlon-get-abstract-with-or-without-ai ()
  "Try to get an abstract using non-AI methods; if unsuccessful, use AI.
To get an abstract with AI, the function uses
`tlon-fetch-and-set-abstract'. See its docstring for details.

To get an abstract without AI, the function uses
`tlon-get-abstract-with-ai'. See its docstring for details."
  (interactive)
  (unless (tlon-fetch-and-set-abstract)
    (tlon-get-abstract-with-ai)))

(declare-function tlon-abstract-may-proceed-p "tlon-tex")
;;;###autoload
(defun tlon-get-abstract-with-ai (&optional file)
  "Return an abstract of the relevant content using AI.
If FILE is non-nil, get an abstract of its contents. Otherwise,

- If in `bibtex-mode' or in `ebib-entry-mode', get an abstract of the contents
  of the HTML or PDF file associated with the current BibTeX entry, if either is
  found.

- If in `pdf-view-mode', get an abstract of the contents of the current PDF
  file.

- If in `text-mode', get an abstract of the contents of the current region, if
  active; otherwise, get an abstract of the contents of the current buffer.

In all the above cases, the AI will first look for an existing abstract and, if
it finds one, use it. Otherwise it will create an abstract from scratch.."
  (interactive)
  (if (tlon-abstract-may-proceed-p)
      (if-let ((language (or (tlon-ai-get-language-in-file file)
			     (unless tlon-ai-batch-fun
			       (tlon-select-language)))))
	  (tlon-ai-get-abstract-in-language file language)
	(tlon-ai-detect-language-in-file
	 file
	 (lambda (response info)
	   (message "Detecting language...")
	   (tlon-ai-get-abstract-from-detected-language response info file))))
    (when tlon-debug
      (message "`%s' now calls `tlon-ai-batch-continue'." "tlon-get-abstract-with-ai"))
    (tlon-ai-batch-continue)))

(defun tlon-get-abstract-with-ai-in-file (extension)
  "Return an abstract of the file with EXTENSION in the BibTeX entry at point."
  (if (tlon-abstract-may-proceed-p)
      (if-let ((file (ebib-extras-get-file extension)))
	  (tlon-get-abstract-with-ai file)
	(user-error "No unique file with extension `%s' found" extension))
    (when tlon-debug
      (message "`%s' now calls `tlon-ai-batch-continue'" "tlon-get-abstract-with-ai-in-file"))
    (tlon-ai-batch-continue)))

(defun tlon-get-abstract-with-ai-from-pdf ()
  "Return an abstract of the PDF file in the BibTeX entry at point."
  (interactive)
  (tlon-get-abstract-with-ai-in-file "pdf"))

(defun tlon-get-abstract-with-ai-from-html ()
  "Return an abstract of the HTML file in the BibTeX entry at point."
  (interactive)
  (tlon-get-abstract-with-ai-in-file "html"))

(defun tlon-ai-get-abstract-in-language (file language)
  "Get abstract from FILE in LANGUAGE."
  (if-let ((string (tlon-get-string-dwim file))
	   (lang-2 (tlon-get-iso-code language)))
      (let ((original-buffer (current-buffer))
	    (key (pcase major-mode
		   ('bibtex-mode (bibtex-extras-get-key))
		   ('ebib-entry-mode (ebib-extras-get-field "=key="))
		   (_ nil))))
	(tlon-ai-get-abstract-common
	 tlon-ai-get-abstract-prompts string lang-2
	 (lambda (response info)
	   ;; we restore the original buffer to avoid a change in `major-mode'
	   (with-current-buffer original-buffer
	     (message "Generating abstract for `%s'; starts with `%s'" key
		      (when response (substring response 0 (min (length string) 100))))
	     (tlon-get-abstract-callback response info key)))))
    (message "Could not get abstract.")
    (tlon-ai-batch-continue)))

(defun tlon-ai-get-abstract-from-detected-language (response info file)
  "If RESPONSE is non-nil, get a summary of FILE.
Otherwise return INFO."
  (if (not response)
      (tlon-ai-callback-fail info)
    (tlon-ai-get-abstract-in-language file response)))

(defun tlon-ai-get-abstract-common (prompts string language callback)
  "Common function for getting an abstract.
PROMPTS is the prompts to use, STRING is the string to summarize, LANGUAGE is
the language of the string, and CALLBACK is the callback function."
  (let ((prompt (tlon-lookup prompts :prompt :language language)))
    (tlon-make-gptel-request prompt string callback)
    (message "Getting AI abstract...")))

(defun tlon-get-abstract-callback (response info &optional key)
  "If RESPONSE is non-nil, take appropriate action based on major mode.
If RESPONSE is nil, return INFO. KEY is the BibTeX key."
  (if (not response)
      (tlon-ai-callback-fail info)
    (pcase major-mode
      ((or 'bibtex-mode 'ebib-entry-mode)
       (message "`tlon-get-abstract-callback' is setting `%s' to `%s'"
		key (when response (substring response 0 (min (length response) 100))))
       (tlon-ai-summarize-set-bibtex-abstract response key))
      ('markdown-mode) ; TODO: set `description' YAML field to it
      (_ (kill-new response)
	 (message "Copied AI-generated abstract to the kill ring:\n\n%s" response))))
  (when tlon-debug
    (message "`%s' now calls `tlon-ai-batch-continue'" "tlon-get-abstract-callback"))
  (tlon-ai-batch-continue))

;;;;;; BibTeX

(declare-function bibtex-set-field "bibtex-extras")
(declare-function bibtex-extras-get-field "bibtex-extras")
(declare-function bibtex-extras-get-entry-as-string "bibtex-extras")
(declare-function bibtex-extras-get-field-in-string "bibtex-extras")
(declare-function ebib-extras-get-or-open-entry "ebib-extras")
;;;###autoload
(defun tlon-ai-summarize-bibtex-entry (&optional string)
  "Summarize the work described in the BibTeX STRING using AI.
If STRING is nil, use the current entry."
  (interactive)
  (let* ((get-string (pcase major-mode
		       ('bibtex-mode #'bibtex-extras-get-entry-as-string)
		       ('ebib-entry-mode #'ebib-extras-get-or-open-entry)
		       (_ (user-error "Unsupported major mode"))))
	 (string (or string (funcall get-string))))
    (unless (bibtex-extras-get-field-in-string string "abstract")
      (when-let* ((get-lang (pcase major-mode
			      ('bibtex-mode #'bibtex-extras-get-field)
			      ('ebib-entry-mode #'ebib-extras-get-field)))
		  (language (funcall get-lang "langid"))
		  (lang-short (tlon-get-iso-code language)))
	(if-let ((prompt (tlon-lookup tlon-ai-summarize-bibtex-prompts :prompt :language lang-short)))
	    (tlon-make-gptel-request prompt string #'tlon-get-abstract-callback)
	  (user-error "No prompt defined in `tlon-ai-get-abstract-prompts' for language %s" language))))))

(declare-function ebib-extras-set-field "ebib-extras")
(declare-function ebib-extras-get-file-of-key "ebib-extras")
(defun tlon-ai-summarize-set-bibtex-abstract (abstract key)
  "Set the `abstract' field of entry with KEY entry to ABSTRACT."
  ;; This assumes KEY is in the current buffer. Maybe relax this assumption.
  (let* ((set-field (pcase major-mode
		      ('bibtex-mode #'bibtex-set-field)
		      ('ebib-entry-mode #'ebib-extras-set-field))))
    (with-current-buffer (find-file-noselect (ebib-extras-get-file-of-key key))
      (save-excursion
	(bibtex-search-entry key)
	(shut-up
	  (funcall set-field "abstract" abstract))))
    (message "Set abstract of `%s'" key)
    (save-buffer)))

;;;;; Language detection

(defun tlon-ai-get-language-in-file (&optional file)
  "Return the language in FILE, based on the major mode.
If FILE is nil, get the language in the current buffer or entry, depending on
the major mode."
  (pcase major-mode
    ('ebib-entry-mode (ebib-extras-get-field "langid"))
    ('bibtex-mode (bibtex-extras-get-field "langid"))
    ('markdown-mode
     (let* ((file (or file (buffer-file-name)))
	    (repo (tlon-get-repo-from-file file)))
       (tlon-repo-lookup :language :dir repo)))))

(defun tlon-ai-detect-language-in-file (&optional file callback)
  "Detect the language in FILE and call CALLBACK.
If FILE is nil, detect the language in the current buffer."
  (let ((string (tlon-get-string-dwim file)))
    (tlon-make-gptel-request tlon-ai-detect-language-prompt string callback)))

;;;;;; BibTeX

(defun tlon-ai-detect-language-in-bibtex (&optional string)
  "Detect language in STRING.
If STRING is nil, use the current BibTeX entry."
  (let ((string (or string (pcase major-mode
			     ('ebib-entry-mode (ebib-extras-get-or-open-entry))
			     ('bibtex-mode (bibtex-extras-get-entry-as-string))
			     (_ (user-error "I can’t detect language in %s" major-mode))))))
    (tlon-make-gptel-request tlon-ai-detect-language-bibtex-prompt string
			     #'tlon-ai-callback-return)))

(declare-function bibtex-beginning-of-entry "bibtex")
;;;###autoload
(defun tlon-ai-set-language-bibtex ()
  "Set the language of the BibTeX entry at point to LANGUAGE.
If STRING is nil, use the current entry."
  (interactive)
  (let* ((string (bibtex-extras-get-entry-as-string))
	 (callback (if (bibtex-extras-get-field-in-string string "langid")
		       #'tlon-ai-set-language-bibtex-when-present-callback
		     #'tlon-ai-set-language-bibtex-when-absent-callback)))
    (tlon-make-gptel-request tlon-ai-detect-language-bibtex-prompt string callback)))

(defun tlon-ai-set-language-bibtex-when-present-callback (response info)
  "Callback for `tlon-ai-set-language-bibtex' when `langid' field is present.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-ai-callback-fail info)
    (bibtex-beginning-of-entry)
    (if-let ((langid (bibtex-extras-get-field "langid"))
	     (valid-langid (tlon-validate-language langid))
	     (valid-response (tlon-validate-language response)))
	(if (string= valid-langid valid-response)
	    (tlon-ai-set-language-bibtex-when-equal valid-langid langid)
	  (let ((langid-2 (tlon-get-iso-code valid-langid))
		(response-2 (tlon-get-iso-code valid-response)))
	    (if (string= langid-2 response-2)
		(tlon-ai-set-language-bibtex-when-equal valid-langid langid)
	      (tlon-ai-set-language-bibtex-when-conflict langid response))))
      (user-error "The `langid' field of the current entry is not valid"))))

(defun tlon-ai-set-language-bibtex-when-absent-callback (response info)
  "Callback for `tlon-ai-set-language-bibtex' when `langid' field is absent.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-ai-callback-fail info)
    (when-let ((language (tlon-validate-language response)))
      (tlon-ai-set-language-bibtex-add-langid language))))

(defun tlon-ai-set-language-bibtex-when-equal (valid-lang lang)
  "Set language depending on whether VALID-LANG and LANG are equal."
  (if (string= valid-lang lang)
      (tlon-ai-batch-continue)
    (tlon-ai-set-language-bibtex-add-langid valid-lang)))

(defun tlon-ai-set-language-bibtex-when-conflict (current detected)
  "Prompt the user to resolve a conflict between CURRENT and DETECTED languages."
  (let ((selection
	 (completing-read
	  (format
	   "The detected language (%s) differs from `langid' language (%s). Which one should we use? "
	   detected current)
	  (list current detected) nil t)))
    (tlon-ai-set-language-bibtex-add-langid selection)))

(defun tlon-ai-set-language-bibtex-add-langid (lang)
  "Set the value of `langid' to LANG."
  (let ((key (bibtex-extras-get-key)))
    (bibtex-set-field "langid" lang)
    (message "Set language of `%s' to %s" key lang)
    (tlon-ai-batch-continue)))

;;;;; Phonetic transcription

(declare-function word-at-point "thingatpt")
;;;###autoload
(defun tlon-ai-phonetically-transcribe (expression language)
  "Insert the phonetic transcription of the EXPRESSION in LANGUAGE.
LANGUAGE is a two-letter ISO 639-1 code. The string is inserted at the point the
request was sent."
  (interactive (list (read-string "Text to transcribe: "
				  (if (region-active-p)
				      (buffer-substring-no-properties (region-beginning) (region-end))
				    (word-at-point)))
		     (or (tlon-repo-lookup :language :dir (tlon-get-repo 'no-prompt))
			 (tlon-select-language 'code))))
  (let ((prompt (tlon-lookup tlon-ai-transcribe-phonetically-prompt
			     :prompt :language language)))
    (tlon-make-gptel-request prompt expression #'tlon-ai-callback-copy)))

(defun tlon-phonetically-transcribe-in-buffer ()
  "Insert a phonetic transcription of each line in buffer immediately after it.
Separate the original line and the transcription with a comma."
  (interactive)
  (let ((language (tlon-select-language 'code)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
	  (goto-char (line-end-position))
	  (insert ",")
	  (tlon-ai-phonetically-transcribe line language)
	  (forward-line))))))

;;;;; Math

(defun tlon-ai-translate-math (expression language)
  "Insert the natural LANGUAGE translation of the mathematical EXPRESSION.
LANGUAGE is a two-letter ISO 639-1 code. The string is inserted at the point the
request was sent."
  (interactive (list (read-string "Math expression: "
				  (buffer-substring-no-properties (region-beginning) (region-end)))
		     (tlon-repo-lookup :language :dir (tlon-get-repo))))
  (let ((prompt (tlon-lookup tlon-ai-translate-math-prompt :prompt :language language)))
    (tlon-make-gptel-request prompt expression #'tlon-ai-callback-insert)))

;;;;; Docs

;; TODO: develop this
(defun tlon-ai-docs ()
  "Docstring."
  (interactive)
  (let ((prompt "Included below is an Emacs configuration file of the organization I work for. Please inspect it and tell me how can I search for a yasnippet snippet. Please be brief.\n\n%s")
	(string (tlon-get-file-as-string
		 "/Users/pablostafforini/Downloads/ai-docs.org")))
    (tlon-make-gptel-request prompt string #'tlon-docs-callback "Claude" "claude-3-sonnet-20240229")))

(defun tlon-docs-callback (response info)
  "If RESPONSE is non-nil, take appropriate action based on major mode.
If RESPONSE is nil, return INFO."
  (if (not response)
      (tlon-ai-callback-fail info)
    (message response)))

;;;;; Menu

(defun tlon-ai-batch-fun-reader (prompt _ _)
  "Return a list of choices with PROMPT to be used as an `infix' reader function."
  (tlon-transient-read-symbol-choice prompt '(tlon-get-abstract-with-or-without-ai
					      tlon-get-abstract-with-ai
					      tlon-fetch-and-set-abstract
					      tlon-ai-set-language-bibtex
					      nil)))

(declare-function transient-define-infix "transient")
(transient-define-infix tlon-ai-batch-fun-infix ()
  "Change the local value of the `tlon-ai-batch-fun' variable."
  :class 'transient-lisp-variable
  :variable 'tlon-ai-batch-fun
  :reader 'tlon-ai-batch-fun-reader
  :prompt "Function for batch-processing: ")

(defun tlon-abstract-overwrite-reader (prompt _ _)
  "Return a list of choices with PROMPT to be used as an `infix' reader function."
  (tlon-transient-read-symbol-choice prompt '(always prompt never)))

(transient-define-infix tlon-abstract-overwrite-infix ()
  "Change the local value of the `tlon-abstract-overwrite' variable."
  :class 'transient-lisp-variable
  :variable 'tlon-abstract-overwrite
  :reader 'tlon-abstract-overwrite-reader
  :prompt "Overwrite when the entry already contains an abstract? ")

(defvar mullvad-durations)
(defun tlon-mullvad-connection-duration-reader (prompt _ _)
  "Return a list of choices with PROMPT to be used as an `infix' reader function."
  (tlon-transient-read-number-choice prompt mullvad-durations))

(transient-define-infix tlon-mullvad-connection-duration-infix ()
  "Change the local value of the `gptel-extras-gemini-mullvad-disconnect-after'
variable."
  :class 'transient-lisp-variable
  :variable 'gptel-extras-gemini-mullvad-disconnect-after
  :reader 'tlon-mullvad-connection-duration-reader
  :prompt "Disconnect after: ")

;;;###autoload (autoload 'tlon-ai-menu "tlon-ai" nil t)
(transient-define-prefix tlon-ai-menu ()
  "Menu for `tlon-ai'."
  :info-manual "(tlon) AI"
  [[""
    ("c" "configure model"                      gptel-extras-model-config)
    ""
    ("t" "translate"                            tlon-ai-translate)
    ("r" "rewrite"                              tlon-ai-rewrite)
    ("p" "phonetically transcribe"              tlon-ai-phonetically-transcribe)
    ""
    "Bibtex"
    ("b" "set language of bibtex"               tlon-ai-set-language-bibtex)
    ""
    "Markdown"
    ("i" "describe image"                       tlon-ai-describe-image)
    ;; Create command to translate all images
    ("m" "translate math"                       tlon-ai-translate-math)
    ;; TODO: develop this
    ;; ("M" "translate all math"                   tlon-ai-translate-math-in-buffer)
    ]
   ["Summarize"
    ("s s" "get abstract with or without AI"    tlon-get-abstract-with-or-without-ai)
    ("s n" "get abstract without AI"            tlon-fetch-and-set-abstract)
    ("s a" "get abstract with AI"               tlon-get-abstract-with-ai)
    ("s h" "get abstract with AI from HTML"     tlon-get-abstract-with-ai-from-html)
    ("s p" "get abstract with AI from PDF"      tlon-get-abstract-with-ai-from-pdf)
    ("s b" "summarize bibtex entry"             tlon-ai-summarize-bibtex-entry)
    ""
    "Summarize parameters"
    ("-b" "batch"                               tlon-ai-batch-fun-infix)
    ("-m" "mullvad connection duration"         tlon-mullvad-connection-duration-infix)
    ("-o" "overwrite"                           tlon-abstract-overwrite-infix)
    ""
    ("-d" "debug"                               tlon-menu-infix-toggle-debug)]])

(provide 'tlon-ai)
;;; tlon-ai.el ends here

;; Local Variables:
;; jinx-languages: "es en it fr de"
;; End:
