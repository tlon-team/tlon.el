;;; tlon-ai.el --- AI functionality for Tlön -*- lexical-binding: t -*-

;; Copyright (C) 2024

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

;; AI functionality for Tlön.

;;; Code:

(require 'gptel)
(require 'gptel-extras)
(require 'shut-up)
(require 'tlon-core)
(require 'tlon-tex) ; needed to set variables correctly

;;;; User options

(defgroup tlon-ai nil
  "AI functionality for Tlön."
  :group 'tlon)

(defcustom tlon-ai-batch-fun nil
  "Function to run in batch mode."
  :type 'symbol
  :group 'tlon-ai)

(defcustom tlon-ai-overwrite-alt-text nil
  "Whether to overwrite existing alt text in images.
This variable only affects the behavior of
`tlon-ai-set-image-alt-text-in-buffer'; it is ignored by
`tlon-ai-set-image-alt-text', which always overwrites."
  :type 'boolean
  :group 'tlon-ai)

(defcustom tlon-ai-summarization-model
  '("Claude" . claude-3-5-haiku-20241022)
  "Model to use for summarization.
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, do not
use a different model for summarization. Note that the selected model should
have a large context window, ideally larger than 1m tokens, since otherwise some
books will not be summarized."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-ai)

(defcustom tlon-ai-use-summarization-model t
  "Whether to use a different model for summarization.
If non-nil, use the model specified in `tlon-ai-summarization-model'. Otherwise,
 use the currently active model."
  :type 'boolean
  :group 'tlon-ai)

(defcustom tlon-ai-edit-prompt nil
  "Whether to edit the prompt before sending it to the AI model."
  :type 'boolean
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

;;;;; File comparison

;; this should be a general category that compares an original file and its
;; translation, and admits of specific prompts, with ‘fix formatting’ being one
;; of these species

;; this does not need to be translated
(defconst tlon-ai-fix-markdown-format-prompt
  `((:prompt "Please take a look at the two paragraphs attached (paragraphs may contain only one word). The first, ‘%s’, is taken from the original document in %s, while the second, ‘%s’, is taken from a translation of that document into %s. In the translation, some of the original formatting (which includes not only Markdown elements but potentially HTML components and SSML tags) has been lost or altered. What I want you to do is to generate a new file with all the original formatting restored, but without changing the text of the translation. Add missing links. Add missing asterisks. Add any other missing markdown signs. Don't add any missing text. Don't change the name of the files referred to as images. And please do not surround the text in backticks. Add missing links. Add missing asterisks. Add any other missing markdown signs. Just give the output but don't add comments or clarifications, even if there's nothing to restore. Thank you!"
	     :language "en")))

;;;;; Image description

(defconst tlon-ai-describe-image-prompt
  `((:prompt "Please provide a concise description of the attached image. The description should consist of one or two sentences and must never exceed 50 words. If you need to use quotes, please use single quotes."
	     :language "en")
    (:prompt "Por favor, describe brevemente la imagen adjunta. La descripción debe consistir de una o dos oraciones y en ningún caso debe exceder las 50 palabras. Si necesitas usar comillas, por favor utiliza comillas simples."
	     :language "es")
    (:prompt "Veuillez fournir une description concise de l'image ci-jointe. La description doit consister en une ou deux phrases et ne doit pas dépasser 50 mots. Si vous devez utiliser des guillemets, veuillez utiliser des guillemets simples."
	     :language "fr")
    (:prompt "Si prega di fornire una descrizione concisa dell'immagine allegata. La descrizione deve consistere in una o due frasi e non deve mai superare le 50 parole. Se è necessario utilizzare le virgolette, si prega di utilizzare le virgolette singole."
	     :language "it")
    (:prompt "Bitte geben Sie eine kurze Beschreibung des beigefügten Bildes. Die Beschreibung sollte aus ein oder zwei Sätzen bestehen und darf 50 Wörter nicht überschreiten. Wenn Sie Anführungszeichen verwenden müssen, verwenden Sie bitte einfache Anführungszeichen."
	     :language "de")))

;;;;; Summarization

;;;;;; Abstracts

(defconst tlon-ai-how-to-write-abstract-prompt
  `((:prompt ,(format "Write the abstract in a sober, objective tone, avoiding cliches, excessive praise and unnecessary flourishes. In other words, draft it as if you were writing the abstract of a scientific paper. The abstract should be only one paragraph long and have a rough length of 100 to 250 words (feel free to exceed it if you really need to, but never go over %s words). It should not mention bibliographic data of the work (such as title or author). Write the abstract directly stating what the article argues, rather than using phrases such as 'The article argues that...'. For example, instead of writing 'The article ‘The eradication of smallpox’ by William D. Tierney tells that mankind fought smallpox for centuries...', write 'Mankind fought smallpox for centuries...'. Also, please omit any disclaimers of the form 'As an AI language model, I'm unable to browse the internet in real-time.' Finally, end your abstract with the phrase ' – AI-generated abstract.'" tlon-tex-max-abstract-length)
	     :language "en")
    (:prompt ,(format "Redacta el resumen en un tono sobrio y objetivo, evitando los lugares comunes, los elogios excesivos y las florituras innecesarias. En otras palabras, redáctalo como si estuvieras escribiendo el resumen de un artículo científico. El resumen debe constar de un solo párrafo y tener una extensión de unas 100 a 250 palabras (puedes exceder este umbral de ser necesario, pero el resumen no debe tener en ningún caso más de %s palabras). No debe mencionar datos bibliográficos de la obra (como el título o el autor). Escribe el resumen indicando directamente lo que argumenta el artículo, en lugar de utilizar frases como ‘El artículo argumenta que...’. Por ejemplo, en lugar de escribir ‘El artículo 'La erradicación de la viruela' de William D. Tierney sostiene que la humanidad luchó contra la viruela durante siglos...’, escribe ‘La humanidad luchó contra la viruela durante siglos...’. Además, omite cualquier descargo de responsabilidad del tipo ‘Como modelo de lenguaje de inteligencia artificial, no puedo navegar por Internet en tiempo real.’ Por último, termina tu resumen con la frase ‘ - Resumen generado por inteligencia artificial.’" tlon-tex-max-abstract-length)
	     :language "es")
    (:prompt ,(format "Rédigez le résumé sur un ton sobre et objectif, en évitant les clichés, les éloges excessifs et les fioritures inutiles. En d'autres termes, rédigez-le comme si vous écriviez le résumé d'un article scientifique. Le résumé ne doit comporter qu'un seul paragraphe et avoir une longueur approximative de 100 à 250 mots (n'hésitez pas à le dépasser si vous en avez vraiment besoin, mais ne dépassez jamais %s mots). Il ne doit pas mentionner les données bibliographiques de l'ouvrage (telles que le titre ou l'auteur). Rédigez le résumé en indiquant directement ce que l'article soutient, plutôt qu'en utilisant des phrases telles que 'L'article soutient que...'. Par exemple, au lieu d'écrire 'L'article 'L'éradication de la variole' de William D. Tierney affirme que l'humanité a combattu la variole pendant des siècles...', écrivez 'L'humanité a combattu la variole pendant des siècles...'. Veuillez également omettre toute clause de non-responsabilité du type 'En tant que modèle linguistique de l'IA, je ne suis pas en mesure de naviguer sur l'internet en temps réel'. Enfin, terminez votre résumé par la phrase ' - Résumé généré par l'IA.'" tlon-tex-max-abstract-length)
	     :language "fr")
    (:prompt ,(format "Scrivete l'abstract con un tono sobrio e oggettivo, evitando i cliché, le lodi eccessive e i fronzoli inutili. In altre parole, scrivetelo come se steste scrivendo l'abstract di un articolo scientifico. L'abstract dovrebbe essere lungo solo un paragrafo e avere una lunghezza approssimativa di 100-250 parole (sentitevi liberi di superarlo se ne avete davvero bisogno, ma non superate mai le %s di parole). Non deve riportare i dati bibliografici del lavoro (come il titolo o l'autore). Scrivete l'abstract indicando direttamente ciò che l'articolo sostiene, piuttosto che usare frasi come 'L'articolo sostiene che...'. Ad esempio, invece di scrivere 'L'articolo 'L'eradicazione del vaiolo' di William D. Tierney afferma che l'umanità ha combattuto il vaiolo per secoli...', scrivete 'L'umanità ha combattuto il vaiolo per secoli...'. Inoltre, omettete qualsiasi dichiarazione di non responsabilità del tipo 'Come modello linguistico dell'IA, non sono in grado di navigare in Internet in tempo reale'. Infine, terminate il vostro riassunto con la frase ' - riassunto generato dall'IA'." tlon-tex-max-abstract-length)
	     :language "it")
    (:prompt ,(format "Schreiben Sie die Zusammenfassung in einem nüchternen, sachlichen Ton und vermeiden Sie Klischees, übermäßiges Lob und unnötige Schnörkel. Mit anderen Worten: Verfassen Sie sie so, als ob Sie die Zusammenfassung einer wissenschaftlichen Arbeit schreiben würden. Die Zusammenfassung sollte nur einen Absatz lang sein und eine ungefähre Länge von 100 bis 250 Wörtern haben (Sie können diese Zahl ruhig überschreiten, wenn es wirklich nötig ist, aber nie mehr als %s Wörter). Sie sollte keine bibliografischen Daten der Arbeit (wie Titel oder Autor) enthalten. Geben Sie in der Zusammenfassung direkt an, worum es in dem Artikel geht, und verwenden Sie keine Sätze wie 'In dem Artikel wird argumentiert, dass...'. Schreiben Sie zum Beispiel statt 'Der Artikel 'Die Ausrottung der Pocken' von William D. Tierney besagt, dass die Menschheit jahrhundertelang die Pocken bekämpfte...' lieber 'Die Menschheit bekämpfte die Pocken jahrhundertelang...'. Lassen Sie bitte auch Haftungsausschlüsse der Form 'Als KI-Sprachmodell bin ich nicht in der Lage, das Internet in Echtzeit zu durchsuchen' weg. Beenden Sie Ihre Zusammenfassung schließlich mit dem Satz ' - KI-generierte Zusammenfassung.'" tlon-tex-max-abstract-length)
	     :language "de")))

(defconst tlon-ai-get-abstract-prompts
  `((:prompt ,(format "The following work may or may not contain an abstract%s. If it contains an abstract, please return it. Otherwise, create an abstract of it yourself. %s However, please omit this phrase if you are simply copying verbatim an abstract you found in the work."
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-abstract-prompt
				   :prompt :language "en"))
	     :language "en")
    (:prompt ,(format "La siguiente obra puede contener o no un resumen%s. Si contiene un resumen, devuélvelo. En caso contrario, crea tú mismo un resumen. %s Sin embargo, omite esta frase si simplemente está devolviendo un resumen que encontraste en la obra.En otras palabras, incluye la frase sólo cuando tú hayas creado el resumen."
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-abstract-prompt
				   :prompt :language "es"))
	     :language "es")
    (:prompt ,(format "L'œuvre suivante peut ou non contenir un résumé%s. S'il contient un résumé, veuillez le renvoyer. Sinon, créez un résumé vous-même. %s Toutefois, veuillez omettre cette phrase si vous ne faites que copier mot pour mot un résumé que vous avez trouvé dans l'œuvre."
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-abstract-prompt
				   :prompt :language "fr"))
	     :language "fr")
    (:prompt ,(format "Il seguente lavoro può contenere o meno un estratto%s. Se contiene un estratto, si prega di restituirlo. Altrimenti, creane uno tu stesso. %s Tuttavia, si prega di omettere questa frase se si sta semplicemente copiando alla lettera un estratto trovato nell'opera."
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-abstract-prompt
				   :prompt :language "it"))
	     :language "it")
    (:prompt ,(format "Das folgende Werk kann eine Zusammenfassung enthalten oder auch nicht%s. Wenn es eine Zusammenfassung enthält, geben Sie sie bitte zurück. Andernfalls erstellen Sie bitte selbst eine Zusammenfassung des Werks. %s Bitte lassen Sie diesen Satz jedoch weg, wenn Sie einfach eine wortwörtliche Zusammenfassung kopieren, die Sie in dem Werk gefunden haben."
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-abstract-prompt
				   :prompt :language "de"))
	     :language "de"))
  "Prompts for summarization.")

(defconst tlon-ai-shorten-abstract-prompts
  `((:prompt ,(format "Please shorten the following abstract to %s words or less. The shortened version should consist of only one paragraph.%s"
		      tlon-tex-max-abstract-length
		      tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Por favor, acorta el siguiente resumen a %s palabras o menos. La versión acortada debe constar de un solo párrafo.%s"
		      tlon-tex-max-abstract-length
		      tlon-ai-string-wrapper)
	     :language "es")
    (:prompt ,(format "Veuillez raccourcir le résumé suivant à %s mots ou moins. La version raccourcie doit se composer d'un seul paragraphe.%s"
		      tlon-tex-max-abstract-length
		      tlon-ai-string-wrapper)
	     :language "fr")
    (:prompt ,(format "Si prega di abbreviare il seguente abstract a %s parole o meno. La versione abbreviata deve essere composta da un solo paragrafo.%s"
		      tlon-tex-max-abstract-length
		      tlon-ai-string-wrapper)
	     :language "it")
    (:prompt ,(format "Bitte kürzen Sie die folgende Zusammenfassung auf %s Wörter oder weniger. Die gekürzte Version sollte nur aus einem Absatz bestehen.%s"
		      tlon-tex-max-abstract-length
		      tlon-ai-string-wrapper)
	     :language "de"))
  "Prompts for summarization.")

;;;;;; Synopsis

(defconst tlon-ai-get-synopsis-prompts
  `((:prompt ,(format "Please write an detailed abstract of the following work%s Write it in a sober, objective tone, avoiding cliches, excessive praise and unnecessary flourishes. In other words, draft it as if you were writing the abstract of a scientific paper or academic publication. The summary should provide a detail account of the work’s main claims and arguments; it may be between one and two thousand words in length. Also, please omit any disclaimers of the form 'As an AI language model, I'm unable to browse the internet in real-time.'"
		      tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Por favor, escribe un resumen detallado de la presente obra%s Redáctalo en un tono sobrio y objetivo, evitando cliches, elogios excesivos y florituras innecesarias. En otras palabras, redáctalo como si estuvieras escribiendo el resumen de un artículo científico o de una publicación académica. El resumen debe dar cuenta detallada de las principales afirmaciones y argumentos de la obra; su extensión puede oscilar entre mil y dos mil palabras. Por favor, omite también cualquier descargo de responsabilidad del tipo 'Como modelo de lenguaje de inteligencia artificial, no puedo navegar por Internet en tiempo real'." tlon-ai-string-wrapper)
	     :language "es")
    (:prompt ,(format "Veuillez rédiger un résumé détaillé de ce travail%s Rédigez-le sur un ton sobre et objectif, en évitant les clichés, les éloges excessifs et les fioritures inutiles. En d'autres termes, rédigez-le comme si vous écriviez le résumé d'un article scientifique ou d'une publication universitaire. Le résumé doit fournir un compte rendu détaillé des principales revendications et des principaux arguments du travail ; il peut compter entre un et deux mille mots. Veuillez également omettre toute clause de non-responsabilité du type \"En tant que modèle de langage d'IA, je ne suis pas en mesure de naviguer sur l'internet en temps réel\"." tlon-ai-string-wrapper)
	     :language "fr")
    (:prompt ,(format "Si prega di scrivere un riassunto esteso di questo lavoro%s Scrivetelo con un tono sobrio e oggettivo, evitando i cliché, le lodi eccessive e i fronzoli inutili. In altre parole, scrivetelo come se steste scrivendo l'abstract di un articolo scientifico o di una pubblicazione accademica. Il riassunto deve fornire un resoconto dettagliato delle principali affermazioni e argomentazioni dell'opera; può essere lungo tra le mille e le duemila parole. Inoltre, si prega di omettere qualsiasi dichiarazione di non responsabilità del tipo \"In quanto modello linguistico dell'intelligenza artificiale, non sono in grado di navigare in Internet in tempo reale\"." tlon-ai-string-wrapper)
	     :language "it")
    (:prompt ""
	     :language "de"))
  "Prompts for synopsis.")

;;;;; Phonetic transcription

(defconst tlon-ai-transcribe-phonetically-prompt
  `((:prompt ,(format "Please transcribe the following text phonetically, i.e. using the International Phonetic Alphabet (IPA).%sJust return the phonetic transcription, without any commentary. Do not enclose the transcription in slashes." tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Por favor, transcribe fonéticamente el siguiente texto, es decir, utilizando el Alfabeto Fonético Internacional (AFI).%sLimítate a devolver la transcripción fonética, sin comentarios de ningún tipo. No encierres la transcripción entre barras." tlon-ai-string-wrapper)
	     :language "es")))

;;;;; Math

(defconst tlon-ai-translate-math-prompt
  `((:prompt ,(format "Please translate this math expression to natural language, i.e. as a human would read it%s For example, if the expression is `\\frac{1}{2} \\times 2^5 \\= 16`, you should translate \"one half times two to the fifth power equals sixteen\". The expression may not require any sophisticated treatment. For example, if I ask you to translate a letter (such as `S`), your “translation” should be that same letter. Please return only the translated expression, without comments or clarifications. If for some reason you cannot do what I ask, simply do not respond at all; in no case should you return messages such as 'I could not translate the expression' or 'Please include the mathematical expression you need me to translate.'" tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Por favor traduce esta expresión matemática a lenguaje natural, es decir, a la manera en que un humano la leería en voz alta%sPor ejemplo, si la expresión es `\\frac{1}{2} \\times 2^5 \\= 16`, debes traducir \"un medio por dos a la quinta potencia es igual a dieciséis\". Es posible que la expresión no requiera ningún tratamiento sofisticado. Por ejemplo, si te pido que traduzcas una letra (como `S`), tu “traducción” debería ser esa misma letra (`ese`). Por favor, devuelve solamente la expresión traducida, sin comentarios ni clarificaciones. Si por alguna razón no puedes hacer lo que te pido, simplemente no respondas nada; en ningún caso debes devolver mensajes como ‘No he podido traducir la expresión’ o ‘Por favor, incluye la expresión matemática que necesitas que traduzca.’" tlon-ai-string-wrapper)
	     :language "es")))

(defconst tlon-ai-convert-math-prompt
  `((:prompt ,(format "Please convert this string into LaTeX%sDo not include LaTeX delimiters." tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Por favor, convierte esta cadena en LaTeX%sNo incluyas los delimitadores de LaTeX" tlon-ai-string-wrapper)
	     :language "es")))

;;;;; Encoding

(defconst tlon-ai-fix-encoding-prompt
  `((:prompt ,(format "The following text includes several encoding errors. For example, \"cuýn\", \"pronosticaci¾3\\263n\", etc.%sPlease return the same text but with these errors corrected, without any other alteration. Do not use double quotes if the text includes single quotes. When returning the corrected text, do not include any clarifications such as ‘Here is the corrected text’. Thank you." tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "El siguiente texto incluye varios errores de codificación. Por ejemplo, \"cuýn\", \"pronosticaci¾3\\263n\", etc.%sPor favor, devuélveme el mismo texto pero con estos errores corregidos, sin ninguna otra alteración. No uses nunca comillas dobles si el texto incluye comillas simples. Al devolverme el texto corregido, no incluyas ninguna aclaración como ‘Aquí tienes el texto corregido’. Gracias." tlon-ai-string-wrapper)
	     :language "es")
    (:prompt ,(format "Il testo seguente contiene diversi errori di codifica. Ad esempio, \"cuýn\", \"pronosticaci¾3\263n\", ecc.%sSi prega di restituire lo stesso testo ma con questi errori corretti, senza altre modifiche. Non utilizzare le virgolette doppie se il testo contiene virgolette singole. Quando si restituisce il testo corretto, non includere chiarimenti come ‘Ecco il testo corretto’. Grazie." tlon-ai-string-wrapper)
	     :language "it")
    (:prompt ,(format "Le texte suivant contient plusieurs erreurs d'encodage. Par exemple, \"cuýn\", \"pronosticaci¾3\263n\", etc.%sVeuillez renvoyer le même texte mais avec ces erreurs corrigées, sans aucune autre altération. N'utilisez pas de guillemets doubles si le texte comporte des guillemets simples. Lorsque vous renvoyez le texte corrigé, n'incluez pas d'éclaircissements tels que \"Voici le texte corrigé\". Je vous remercie de votre attention." tlon-ai-string-wrapper)
	     :language "fr")))

;;;; Functions

;;;;; General

(defmacro tlon-warn-if-gptel-context (&rest body)
  "Execute BODY after checking if `gptel' context is empty.
If context is not empty, ask for user confirmation before proceeding."
  `(when (or (null gptel-context--alist)
             (y-or-n-p "The `gptel' context is not empty. Proceed? "))
     ,@body))

(defun tlon-make-gptel-request (prompt string &optional callback full-model)
  "Make a `gptel' request with PROMPT and STRING and CALLBACK.
PROMPT is a formatting string containing the prompt and a slot for a string,
which is the variable part of the prompt (e.g. the text to be summarized in a
prompt to summarize text). FULL-MODEL is a cons cell whose car is the backend
and whose cdr is the model."
  (tlon-warn-if-gptel-context
   (let ((full-model (or full-model (cons (gptel-backend-name gptel-backend) gptel-model)))
	 (prompt (tlon-ai-maybe-edit-prompt prompt)))
     (cl-destructuring-bind (backend . model) full-model
       (let ((gptel-backend (alist-get backend gptel--known-backends nil nil #'string=))
	     (gptel-model full-model))
	 (if tlon-ai-batch-fun
	     (condition-case nil
		 (gptel-request (format prompt string) :callback callback)
	       (error nil))
	   (gptel-request (format prompt string) :callback callback)))))))

(defun tlon-ai-maybe-edit-prompt (prompt)
  "If `tlon-ai-edit-prompt' is non-nil, ask user to edit PROMPT, else return it."
  (if tlon-ai-edit-prompt
      (read-string "Prompt: " prompt)
    prompt))

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
    (message "Copied AI model response to kill ring.")))

(defun tlon-ai-callback-save (file)
  "If a response is obtained, save it to FILE.
Otherwise emit a message with the status provided by INFO."
  (lambda (response info)
    (if (not response)
	(tlon-ai-callback-fail info)
      (with-temp-buffer
	(erase-buffer)
	(insert response)
	(write-region (point-min) (point-max) file)))))

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
				 (ebib--get-key-at-point))))
    (funcall tlon-ai-batch-fun)))

(defun tlon-ai-try-try-try-again (original-fun)
  "Call ORIGINAL-FUN up to three times if it its response is nil, then give up."
  (while (< tlon-ai-retries 3)
    (setq tlon-ai-retries (1+ tlon-ai-retries))
    (message "Retrying language detection (try %d of 3)..." tlon-ai-retries)
    (funcall original-fun)))

(declare-function ebib-extras-get-text-file "ebib-extras")
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

- If in `eww-mode', return the contents of the current HTML file.

- If in `markdown-mode', return the substantive contents of the current buffer.

- Otherwise, return the contents of the current buffer."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if-let ((file (or file (pcase major-mode
			      ((or 'bibtex-mode 'ebib-entry-mode)
			       (ebib-extras-get-text-file))
			      ('pdf-view-mode (buffer-file-name))
			      ('eww-mode (let ((contents (buffer-string))
					       (file (make-temp-file "eww-")))
					   (with-current-buffer (find-file-noselect file)
					     (insert contents)
					     (write-file file))
					   file))))))
	(tlon-get-file-as-string file)
      (cond ((derived-mode-p 'markdown-mode)
	     (tlon-md-read-content file))
	    (t
	     (buffer-substring-no-properties (point-min) (point-max)))))))

(declare-function shr-render-buffer "shr")
(autoload 'tlon-convert-pdf "tlon-import")
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
			     (tlon-ai-translate-file-callback file))))

(declare-function tlon-get-counterpart "tlon-counterpart")
(defun tlon-ai-translate-file-callback (file)
  "Callback for `tlon-ai-translate-file'.
FILE is the file to translate."
  (lambda (response info)
    (if (not response)
	(tlon-ai-callback-fail info)
      (let* ((counterpart (tlon-get-counterpart file))
	     (filename (file-name-nondirectory counterpart))
	     (target-path (concat
			   (file-name-sans-extension filename)
			   "--ai-translated.md")))
	(with-temp-buffer
	  (insert response)
	  (write-region (point-min) (point-max) target-path))))))

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

(declare-function gptel-extras-clear-file-context "gptel-extras")
;;;###autoload
(defun tlon-ai-describe-image (&optional file callback)
  "Describe the contents of the image in FILE.
By default, print the description in the minibuffer. If CALLBACK is non-nil, use
it instead."
  (interactive)
  (let* ((previous-context gptel-context--alist)
	 (file (tlon-ai-read-image-file file))
	 (language (tlon-get-language-in-file file))
	 (default-prompt (tlon-lookup tlon-ai-describe-image-prompt :prompt :language language))
	 (prompt (tlon-ai-maybe-edit-prompt default-prompt)))
    (gptel-extras-clear-file-context)
    (gptel-context-add-file file)
    (gptel-request prompt
      :callback (or callback
		    (lambda (response info)
		      (if response
			  (message response)
			(user-error "Error: %s" (plist-get info :status)))
		      (setq gptel-context--alist previous-context))))))

(declare-function tlon-get-tag-attribute-values "tlon-md")
(declare-function tlon-md-insert-attribute-value "tlon-md")
(defun tlon-ai-set-image-alt-text ()
  "Insert a description of the image in the image tag at point.
The image tags are \"Figure\" or \"OurWorldInData\"."
  (interactive)
  (save-excursion
    (if-let* ((src (car (or (tlon-get-tag-attribute-values "Figure")
			    (tlon-get-tag-attribute-values "OurWorldInData"))))
	      (file (tlon-ai-get-image-file-from-src src))
	      (pos (point-marker)))
	(tlon-ai-describe-image file (lambda (response info)
				       (if response
					   (with-current-buffer (marker-buffer pos)
					     (goto-char pos)
					     (tlon-md-insert-attribute-value "alt" response))
					 (user-error "Error: %s" (plist-get info :status)))))
      (user-error "No \"Figure\" or \"OurWorldInData\" tag at point"))))

(declare-function tlon-md-get-tag-pattern "tlon-md")
(defun tlon-ai-set-image-alt-text-in-buffer ()
  "Insert a description of all the images in the current buffer.
If the image already contains a non-empty `alt' field, overwrite it when
`tlon-ai-overwrite-alt-text' is non-nil."
  (interactive)
  (save-excursion
    (dolist (tag '("Figure" "OurWorldInData"))
      (goto-char (point-min))
      (while (re-search-forward (tlon-md-get-tag-pattern tag) nil t)
	(when (or tlon-ai-overwrite-alt-text
		  (not (match-string 6))
		  (string-empty-p (match-string 6)))
	  (tlon-ai-set-image-alt-text))))))

(declare-function dired-get-filename "dired")
(defun tlon-ai-read-image-file (&optional file)
  "Read an image FILE from multiple sources.
In order, the sources are: the value of FILE, the value of `src' attribute in a
`Figure' MDX tag, the image in the current buffer, the image at point in Dired
and the file selected by the user."
  (or file
      (when-let ((name (car (tlon-get-tag-attribute-values "Figure"))))
	(file-name-concat (file-name-as-directory (tlon-get-repo 'no-prompt))
			  (replace-regexp-in-string "^\\.\\./" "" name)))
      (member (buffer-file-name) image-file-name-extensions)
      (when (derived-mode-p 'dired-mode)
	(dired-get-filename))
      (read-file-name "Image file: ")))

(defun tlon-ai-get-image-file-from-src (src)
  "Get the image file from the SRC attribute.
If SRC is a One World in Data URL, download the image and return the local file.
Otherwise, construct a local file path from SRC and return it."
  (if (string-match-p "ourworldindata.org" src)
      (let* ((extension ".png")
	     (url (format "https://ourworldindata.org/grapher/thumbnail/%s%s"
			  (car (last (split-string src "/"))) extension))
	     (file (make-temp-file nil nil extension)))
	(url-copy-file url file t)
	file)
    (file-name-concat (file-name-as-directory (tlon-get-repo 'no-prompt))
		      (replace-regexp-in-string "^\\.\\./" "" src))))

;;;;; File comparison

;;;;;; Fix formatting

(autoload 'gptel-context-add-file "gptel-context")
(autoload 'tlon-get-corresponding-paragraphs "tlon-counterpart")
;;;###autoload
(defun tlon-ai-fix-markdown-format (&optional file)
  "Fix Markdown format in FILE by copying the formatting in its counterpart.
Process the file paragraph by paragraph to avoid token limits. If FILE is nil,
use the file visited by the current buffer. Retry failed paragraphs up to 3
times."
  (interactive)
  (tlon-warn-if-gptel-context
   (let ((file (or file (buffer-file-name) (read-file-name "File to fix: "))))
     (unless (derived-mode-p 'markdown-mode)
       (user-error "This command must be run on a Markdown file"))
     (let* ((original-file (tlon-get-counterpart file))
            (original-lang (tlon-get-language-in-file original-file))
            (translation-lang (tlon-get-language-in-file file))
            (prompt (tlon-ai-maybe-edit-prompt
                     (tlon-lookup tlon-ai-fix-markdown-format-prompt
				  :prompt :language "en")))
            (pairs (tlon-get-corresponding-paragraphs file original-file))
            (results (make-vector (length pairs) nil))
            (completed 0)
            (failed-indices '())
            (retry-count 0)
            (active-requests 0)
            (max-concurrent 5))
       (cl-labels ((process-paragraphs
		     (indices)
		     (dolist (i indices)
		       (while (>= active-requests max-concurrent)
			 (sleep-for 0.1))
		       (let* ((pair (nth i pairs))
			      (formatted-prompt
			       (format prompt
				       (cdr pair)
				       (tlon-lookup tlon-languages-properties :standard :code original-lang)
				       (car pair)
				       (tlon-lookup tlon-languages-properties :standard :code translation-lang))))
			 (cl-incf active-requests)
			 (gptel-request formatted-prompt
			   :callback
			   (lambda (response info)
			     (cl-decf active-requests)
			     (if response
				 (progn
				   (aset results i response)
				   (setq completed (1+ completed))
				   (message "Processing paragraphs... %d%% (%d/%d)"
					    (round (* 100 (/ completed (float (length pairs)))))
					    completed
					    (length pairs))
				   (when (and (= completed (length pairs))
					      (zerop active-requests))
				     (write-fixed-file)))
			       (push i failed-indices)
			       (message "Failed to process paragraph %d: %s"
					i (plist-get info :status))))))))
                   (write-fixed-file ()
		     (message "Processing complete. Writing file...")
		     (let ((fixed-file-path (concat (file-name-sans-extension file) "--fixed.md")))
		       (with-temp-buffer
			 (dolist (para (append results nil))
                           (insert (or para "") "\n\n"))
			 (write-region (point-min) (point-max) fixed-file-path))
		       (find-file fixed-file-path)
		       (when (y-or-n-p "Done! Run ediff session? ")
			 (ediff-files file fixed-file-path))))
                   (retry-failed ()
		     (when (and failed-indices (< retry-count 3))
		       (setq retry-count (1+ retry-count))
		       (message "Retrying %d failed paragraphs (attempt %d of 3)..."
				(length failed-indices) retry-count)
		       (let ((to-retry (nreverse failed-indices)))
			 (setq failed-indices nil)
			 (process-paragraphs to-retry))
		       (run-with-timer 5 nil #'retry-failed))))
         (message "Fixing format of `%s' (%d paragraphs)..."
                  (file-name-nondirectory file) (length pairs))
         (process-paragraphs (number-sequence 0 (1- (length pairs))))
         (run-with-timer 5 nil #'retry-failed))))))

;;;;; Summarization

(declare-function tlon-fetch-and-set-abstract "tlon-tex")
;;;###autoload
(defun tlon-get-abstract-with-or-without-ai ()
  "Try to get an abstract using non-AI methods; if unsuccessful, use AI.
To get an abstract with AI, the function uses
`tlon-fetch-and-set-abstract'. See its docstring for details.

To get an abstract with AI, the function uses
`tlon-get-abstract-with-ai'. See its docstring for details."
  (interactive)
  (unless (tlon-fetch-and-set-abstract)
    (tlon-get-abstract-with-ai)))

(declare-function tlon-abstract-may-proceed-p "tlon-tex")
;;;###autoload
(defun tlon-get-abstract-with-ai (&optional file type)
  "Return an abstract of TYPE using AI.
If FILE is non-nil, get an abstract of its contents. Otherwise,

- If in `bibtex-mode' or in `ebib-entry-mode', get an abstract of the contents
  of the HTML or PDF file associated with the current BibTeX entry, if either is
  found.

- If in `pdf-view-mode', get an abstract of the contents of the current PDF
  file.

- If in `eww-mode', get an abstract of the contents of the current HTML file.

- If in `text-mode', get an abstract of the contents of the current region, if
  active; otherwise, get an abstract of the contents of the current buffer.

In all the above cases, the AI will first look for an existing abstract and, if
it finds one, use it. Otherwise it will create an abstract from scratch.."
  (interactive)
  (if (tlon-abstract-may-proceed-p)
      (if-let ((language (or (tlon-get-language-in-mode)
			     (unless tlon-ai-batch-fun
			       (tlon-select-language)))))
	  (tlon-ai-get-abstract-in-language file language type)
	(tlon-ai-detect-language-in-file
	 file (tlon-ai-get-abstract-from-detected-language file)))
    (when tlon-debug
      (message "`%s' now calls `tlon-ai-batch-continue'." "tlon-get-abstract-with-ai"))
    (tlon-ai-batch-continue)))

(defun tlon-shorten-abstract-with-ai ()
  "Shorten the abstract at point so that does not exceed word threshold."
  (interactive)
  (when-let* ((get-field (pcase major-mode
			   ('bibtex-mode #'bibtex-extras-get-field)
			   ('ebib-entry-mode #'ebib-extras-get-field)))
	      (get-key (pcase major-mode
			 ('bibtex-mode #'bibtex-extras-get-key)
			 ('ebib-entry-mode #'ebib--get-key-at-point)))
	      (abstract (funcall get-field "abstract"))
	      (language (tlon-get-iso-code (or (funcall get-field "langid")
					       (tlon-select-language))))
	      (key (funcall get-key)))
    (tlon-ai-get-abstract-common
     tlon-ai-shorten-abstract-prompts abstract language
     (tlon-get-abstract-callback key))))

(defun tlon-get-synopsis-with-ai (&optional file)
  "Return a synopsis of the relevant content using AI.
If FILE is non-nil, get an abstract of its contents. Otherwise, behave as
described in the `tlon-get-abstract-with-ai' docstring."
  (interactive)
  (tlon-get-abstract-with-ai file 'synopsis))

(declare-function ebib-extras-get-file "ebib-extras")
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

(defun tlon-ai-get-abstract-in-language (file language &optional type)
  "Get abstract from FILE in LANGUAGE.
If TYPE is `synopsis', generate a synopsis. If TYPE is `abstract', nil, or any
other value, generate an abstract."
  (if-let ((string (tlon-get-string-dwim file))
	   (lang-2 (tlon-get-iso-code language)))
      (let ((original-buffer (current-buffer))
	    (key (pcase major-mode
		   ('bibtex-mode (bibtex-extras-get-key))
		   ('ebib-entry-mode (ebib--get-key-at-point))
		   (_ nil))))
	(tlon-ai-get-abstract-common
	 (pcase type
	   ('synopsis tlon-ai-get-synopsis-prompts)
	   (_ tlon-ai-get-abstract-prompts))
	 string lang-2 (tlon-get-abstract-callback key type original-buffer)))
    (message "Could not get abstract.")
    (tlon-ai-batch-continue)))

(defun tlon-ai-get-abstract-from-detected-language (file)
  "If RESPONSE is non-nil, get a summary of FILE.
Otherwise return INFO."
  (lambda (response info)
    (message "Detecting language...")
    (if (not response)
	(tlon-ai-callback-fail info)
      (tlon-ai-get-abstract-in-language file response))))

(defun tlon-ai-get-abstract-common (prompt string language callback)
  "Common function for getting an abstract.
PROMPT is the prompt to use, STRING is the string to summarize, LANGUAGE is
the language of the string, and CALLBACK is the callback function."
  (if-let ((prompt (tlon-lookup prompt :prompt :language language)))
      (let ((model (when tlon-ai-use-summarization-model
		     tlon-ai-summarization-model)))
	(tlon-make-gptel-request prompt string callback model)
	(message "Getting AI abstract..."))
    (user-error "Could not get prompt for language %s" language)))

(defun tlon-get-abstract-callback (&optional key type buffer)
  "Process the response, taking appropriate action based on major mode.
KEY is the BibTeX key. If TYPE is `synopsis', copy the response to the kill
ring. If type is `abstract', nil, or any other value, take the action
appropriate for an abstract. BUFFER is the buffer where the abstract should be
inserted; if nil, use the current buffer."
  (lambda (response info)
    (if (not response)
	(tlon-ai-callback-fail info)
      (with-current-buffer (or buffer (current-buffer))
	(pcase type
	  ('synopsis
	   (kill-new response)
	   (message "Copied AI-generated abstract to the kill ring:\n\n%s" response))
	  (_ (pcase major-mode
	       ((or 'bibtex-mode 'ebib-entry-mode)
		(when tlon-debug
		  (message "`tlon-get-abstract-callback' is setting `%s' to `%s'"
			   key (when response (substring response 0 (min (length response) 100)))))
		(tlon-ai-summarize-set-bibtex-abstract response key))
	       ;; ('markdown-mode) ; TODO: set `description' YAML field to it
	       (_ (kill-new response)
		  (message "Copied AI-generated abstract to the kill ring:\n\n%s" response))))))
      (when tlon-debug
	(message "`%s' now calls `tlon-ai-batch-continue'" "tlon-get-abstract-callback"))
      (tlon-ai-batch-continue))))

;;;;;; BibTeX

(declare-function bibtex-set-field "bibtex")
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
    (when (derived-mode-p 'bibtex-mode)
      (save-buffer))))

;;;;; Language detection

(defun tlon-ai-detect-language-in-file (&optional file callback)
  "Detect the language in FILE and call CALLBACK.
If FILE is nil, detect the language in the current buffer."
  (let* ((string (tlon-get-string-dwim file))
	 (sample (substring string 0 (min (length string) 1000))))
    (tlon-make-gptel-request tlon-ai-detect-language-prompt sample callback)))

(defun tlon-ai-set-language-in-file (&optional file)
  "Detect language of FILE and set it as the spell checker language.
If FILE is nil, use the current buffer."
  (interactive)
  (with-current-buffer
      (if file
	  (find-file-noselect file)
	(current-buffer))
    (tlon-ai-detect-language-in-file file #'tlon-ai-set-language-in-file-callback)))

(autoload 'jinx-languages "jinx")
(defvar jinx-save-languages)
(defun tlon-ai-set-language-in-file-callback (response info)
  "Callback for `tlon-ai-set-language-in-file'.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let ((lang (tlon-get-iso-code response)))
      (let ((jinx-save-languages))
	(jinx-languages lang)))))

;;;;;; BibTeX

(declare-function ebib-extras-get-or-open-entry "bibtex-extras")
(declare-function bibtex-extras-get-entry-as-string "bibtex-extras")
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
(declare-function bibtex-extras-get-field-in-string "bibtex-extras")
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
		     (or (tlon-get-language-in-file) (tlon-select-language 'code))))
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

(defun tlon-ai-convert-math (&optional expression language)
  "Convert math EXPRESSION in LANGUAGE into LaTeX."
  (interactive)
  (tlon-ai-process-math 'convert expression language))

(defun tlon-ai-translate-math (&optional expression language)
  "Translate math EXPRESSION in LANGUAGE into alt text."
  (interactive)
  (tlon-ai-process-math 'translate expression language))

(declare-function tlon-looking-at-tag-p "tlon-md")
(defun tlon-ai-process-math (action &optional expression language)
  "Take ACTION with math EXPRESSION in LANGUAGE.
If ACTION is `convert', convert expression into LaTeX. If ACTION is `translate',
translate LaTeX into alt text.

If EXPRESSION is nil, prompt the user for it. As initial input, use the region,
if active, or the contents of the `Math' tag at point, if any. LANGUAGE is a
two-letter ISO 639-1 code.

If point is on a `Math' tag, insert the string as the value of the `alt'
attribute. Otherwise, print and copy the string to the kill ring."
  (cl-destructuring-bind (var message)
      (pcase action
	('convert `(,tlon-ai-convert-math-prompt  "Converting"))
	('translate `(,tlon-ai-translate-math-prompt  "Translating")))
    (let* ((expression (or expression
			   (read-string "Math expression: "
					(if (region-active-p)
					    (buffer-substring-no-properties (region-beginning) (region-end))
					  (when (tlon-looking-at-tag-p "Math")
					    (match-string-no-properties 2))))))
	   (language (or language (tlon-get-language-in-file) (tlon-select-language 'code)))
	   (prompt (tlon-lookup var :prompt :language language)))
      (tlon-make-gptel-request prompt expression (tlon-ai-insert-math-text expression action))
      (message "%s math..." message))))

(defun tlon-ai-insert-math-text (content action)
  "Insert RESPONSE based on ACTION.
CONTENT is the content of the `Math' element. If RESPONSE is nil, return INFO.
`tlon-ai-process-math'."
  (lambda (response info)
    (if (not response)
	(tlon-ai-callback-fail info)
      (if (tlon-looking-at-tag-p "Math")
	  (apply #'tlon-md-insert-attribute-value
		 "alt" (pcase action
			 ('convert (list (car (tlon-get-tag-attribute-values "Math")) response))
			 ('translate (list response content))))
	(kill-new response)
	(message response)))))

;;;;; Docs

;; TODO: develop this
(defun tlon-ai-docs ()
  "Docstring."
  (interactive)
  (let ((prompt "Included below is an Emacs configuration file of the organization I work for. Please inspect it and tell me how can I search for a yasnippet snippet. Please be brief.\n\n%s")
	(string (tlon-get-file-as-string
		 "/Users/pablostafforini/Downloads/ai-docs.org")))
    (tlon-make-gptel-request prompt string #'tlon-docs-callback)))

(defun tlon-docs-callback (response info)
  "If RESPONSE is non-nil, take appropriate action based on major mode.
If RESPONSE is nil, return INFO."
  (if (not response)
      (tlon-ai-callback-fail info)
    (message response)))

;;;;; Fix encoding

(defun tlon-ai-fix-encoding-in-string (&optional string language)
  "Fix encoding in STRING containing text in LANGUAGE.
Copy the result to the kill ring."
  (interactive)
  (let* ((json (tlon-ai-return-json-value-at-point))
	 (string (or string (cdr json)))
	 (language (or language (car json)))
	 (prompt (tlon-lookup tlon-ai-fix-encoding-prompt :prompt :language language)))
    (tlon-make-gptel-request prompt string)))

(defun tlon-ai-replace-encoded-string (response info)
  "Replace STRING with RESPONSE.
If RESPONSE is nil, return INFO."
  (if (not response)
      (tlon-ai-callback-fail info)
    (insert response)))

;; quick 'n dirty function to get the JSON key and value at point
(defun tlon-ai-return-json-value-at-point ()
  "Return the JSON key and value at point as a cons cell."
  (interactive)
  (let (car cdr)
    (save-excursion
      (re-search-backward "\"\\([a-z]\\{2\\}\\)\"" nil t)
      (setq car (match-string-no-properties 1))
      (goto-char (+ (point) 7))
      (let ((begin (point)))
	(search-forward "\"" nil t)
	(setq cdr (buffer-substring-no-properties begin (1- (point))))
	(delete-region begin (1- (point))))
      (cons car cdr))))

(defun tlon-ai-fix-encoding-in-buffer ()
  "Fix encoding in the current JSON buffer."
  (interactive)
  (let ((prompt (tlon-lookup tlon-ai-fix-encoding-prompt :prompt :language "en"))
	(point 187527)) ; start of first sexp in file
    (dotimes (i 463)
      (let* ((cons (tlon-ai-get-json-chunk point 1500))
	     (string (cdr cons))
	     (file (file-name-concat paths-dir-babel-refs "bib"
				     (format "chunk%s.json" (+ 14 i)))))
	(message (format "file: %s ; begins: %s" (+ 14 i) point))
	(setq point (car cons))
	(unless (file-exists-p file)
	  (tlon-make-gptel-request prompt string (tlon-ai-callback-save file))
	  (when tlon-ai-use-summarization-model
	    tlon-ai-summarization-model))))))

(defun tlon-ai-get-json-chunk (begin size)
  "In current buffer, get the longest possible string less than SIZE from BEGIN."
  (let ((word-count 0)
	(temp-begin begin))
    (goto-char temp-begin)
    (while (< word-count size)
      (forward-sexp)
      (setq word-count (+ word-count (count-words-region temp-begin (point))))
      (setq temp-begin (point)))
    (cons (point) (buffer-substring-no-properties begin (point)))))

(defun tlon-ai-join-files ()
  "Concatenate contents of files named ~file0~ to ~file9~ into a single file."
  (interactive)
  (let* ((dir (file-name-concat paths-dir-babel-refs "bib/"))
	 (output-file "fixed.json"))
    (with-temp-buffer
      (dotimes (i 477)
	(let ((file-name (file-name-concat dir (format "chunk%s.json" i))))
	  (when (file-exists-p file-name)
	    (goto-char (point-max))
	    (insert-file-contents file-name))))
      (write-file output-file))))

;;;;; Menu

(transient-define-infix tlon-ai-infix-toggle-overwrite-alt-text ()
  "Toggle the value of `tlon-ai-overwrite-alt-text' in `ai' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-ai-overwrite-alt-text
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-ai-overwrite-alt-text)))

(transient-define-infix tlon-ai-infix-toggle-use-summarization-model ()
  "Toggle the value of `tlon-ai-use-summarization-model' in `ai' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-ai-use-summarization-model
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-ai-use-summarization-model)))

(transient-define-infix tlon-ai-infix-toggle-edit-prompt ()
  "Toggle the value of `tlon-ai-edit-prompt' in `ai' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-ai-edit-prompt
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-ai-edit-prompt)))

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
  (tlon-transient-read-symbol-choice prompt '(always ask never)))

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

(autoload 'gptel--infix-provider "gptel-transient")
;;;###autoload (autoload 'tlon-ai-menu "tlon-ai" nil t)
(transient-define-prefix tlon-ai-menu ()
  "Menu for `tlon-ai'."
  :info-manual "(tlon) AI"
  [["Summarize"
    ("s s" "get abstract with or without AI"          tlon-get-abstract-with-or-without-ai)
    ("s n" "get abstract without AI"                  tlon-fetch-and-set-abstract)
    ("s a" "get abstract with AI"                     tlon-get-abstract-with-ai)
    ("s h" "get abstract with AI from HTML"           tlon-get-abstract-with-ai-from-html)
    ("s p" "get abstract with AI from PDF"            tlon-get-abstract-with-ai-from-pdf)
    ("s y" "get synopsis with AI"                     tlon-get-synopsis-with-ai)
    ("s h" "shorten abstract with AI"                 tlon-shorten-abstract-with-ai)
    ""
    "Summarize options"
    ("s -b" "batch"                                   tlon-ai-batch-fun-infix)
    ("s -m" "mullvad connection duration"             tlon-mullvad-connection-duration-infix)
    ("s -o" "overwrite abstract"                      tlon-abstract-overwrite-infix)
    ("s -s" "use summarization model for summaries"   tlon-ai-infix-toggle-use-summarization-model)
    ""]
   ["Images"
    ("i d" "describe image"                           tlon-ai-describe-image)
    ("i s" "set alt text"                             tlon-ai-set-image-alt-text)
    ("i S" "set alt text in buffer"                   tlon-ai-set-image-alt-text-in-buffer)
    ""
    "Image options"
    ("i -o" "overwrite alt text"                      tlon-ai-infix-toggle-overwrite-alt-text)]
   ["Math"
    ("c" "convert"                                    tlon-ai-convert-math)
    ("t" "translate"                                  tlon-ai-translate-math)]
   ["Misc"
    ("b" "set language of bibtex"                     tlon-ai-set-language-bibtex)
    ("e" "fix encoding"                               tlon-ai-fix-encoding-in-string)
    ("f" "fix Markdown format"                        tlon-ai-fix-markdown-format)
    ("p" "phonetically transcribe"                    tlon-ai-phonetically-transcribe)
    ("r" "rewrite"                                    tlon-ai-rewrite)
    ("l" "translate"                                  tlon-ai-translate)
    ;; Create command to translate all images
    ;; TODO: develop this
    ;; ("M" "translate all math"                      tlon-ai-translate-math-in-buffer)
    ]
   ["General options"
    ("-e" "edit prompt"                               tlon-ai-infix-toggle-edit-prompt)
    ("-d" "debug"                                     tlon-menu-infix-toggle-debug)
    ""
    (gptel--infix-provider)]])

(provide 'tlon-ai)
;;; tlon-ai.el ends here

;; Local Variables:
;; jinx-languages: "es en it fr de"
;; End:
