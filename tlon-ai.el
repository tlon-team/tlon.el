;;; tlon-ai.el --- AI functionality for Tlön -*- lexical-binding: t -*-

;; Copyright (C) 2025

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
(require 'gptel-curl)
(require 'gptel-extras)
(require 'json)
(require 'shut-up)
(require 'tlon)
(require 'tlon-core)
(require 'tlon-tex) ; needed to set variables correctly
(require 'tlon-counterpart)

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

(defcustom tlon-ai-edit-prompt nil
  "Whether to edit the prompt before sending it to the AI model."
  :type 'boolean
  :group 'tlon-ai)

(defcustom tlon-ai-auto-proofread nil
  "Whether to automatically proofread reference articles."
  :type 'boolean
  :group 'tlon-ai)

;;;;; Custom models

(defcustom tlon-ai-summarization-model
  '("Gemini" . gemini-2.0-flash-thinking-exp-01-21)
  "Model to use for summarization.
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, do not
use a different model for summarization.

Note that the selected model should have a large context window, ideally larger
than 1m tokens, since otherwise some books will not be summarized."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-ai)

(defcustom tlon-ai-markdown-fix-model
  '("Gemini" . gemini-2.0-flash-thinking-exp-01-21)
  "Model to use for fixing the Markdown.
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, do not
use a different model for fixing the Markdown."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-ai)

(defcustom tlon-ai-create-reference-article-model nil
  "Model to use for creating reference articles.
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, do not
use a different model for creating a reference article."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-ai)

(defcustom tlon-ai-proofread-reference-article-model
  '("ChatGPT" . gpt-4.5-preview)
  "Model to use for proofreading reference articles.
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, do not
use a different model for proofreading the reference article."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-ai)

(defcustom tlon-ai-help-model
  '("Gemini" . gemini-2.0-flash-lite-preview-02-05)
  "Model to use for the AI help command (`tlon-ai-ask-for-help').
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, use the
default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-ai)

;;;; Variables

(defvar tlon-ai-retries 0
  "Number of retries for AI requests.")

(defvar tlon-file-bare-bibliography (file-name-concat (tlon-repo-lookup :dir :name "babel-refs") "bare-bibliography.json")
  "Path to the JSON file containing the bare bibliography.")

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

;;;;; Writing

;; TODO: instruct the model to use `Cite' tags in Chicago-style citations
(defconst tlon-ai-write-reference-article-prompt
  `((:prompt "You are an encyclopedia writer, and are currently writing a series of articles for an encyclopedia of effective altruism. Please write an entry on the topic of ‘%1$s’.\n\nYou should write the article *primarily* based on the text files attached, though you may also rely on your general knowledge of the topic. Each of these articles discusses the topic of the entry. So you should inspect each of these files closely and make an effort to understand what they claim thoroughly. Then, once you have inspected and understood the contents of all of these files, make a synthesis of the topic (%1$s) and write the article based on this synthesis.\n\nWrite the article in a sober, objective tone, avoiding cliches, excessive praise and unnecessary flourishes. In other words, draft it as if you were writing an article for a reputable encyclopedia, such as the Encyclopaedia Britannica (but remember that this is not a general encyclopedia, but specifically an encyclopdia of effective altruism, so it should be written from that perspective).\n\nWhen you make a claim traceable to a specific source, please credit this source in a footnote. In general, try not to have more than one footnote per paragraph, though you may include multiple references in a given footnote. Do not include a references section at the end. Use Markdown syntax for composing the article."
	     :language "en")
    (:prompt "Eres un escritor de enciclopedias y estás escribiendo una serie de artículos para una enciclopedia sobre el altruismo eficaz. Por favor, escribe una entrada sobre el tema ‘%1$s’.\n\nDebes escribir el artículo *principalmente* basándote en los archivos de texto adjuntos, aunque también puedes tener en cuenta tu conocimiento general del tema. Cada uno de estos artículos trata el tema de la entrada. Por lo tanto, debes examinar detenidamente cada uno de estos archivos y esforzarte por comprender a fondo lo que sostiene. Luego, una vez que hayas inspeccionado y comprendido el contenido de todos estos archivos, haz una síntesis del tema (%1$s) y escribe el artículo basándote en esta síntesis.\n\nAdjunto también un glosario sobre terminología relacionada con el altruismo eficaz. Procura utilizar estos términos para vertir al castellano expresiones peculiares de ese movimiento.\n\nEscribe el artículo en un tono sobrio y objetivo, evitando clichés, elogios excesivos y florituras innecesarias. En otras palabras, redáctalo como si estuvieras escribiendo un artículo para una enciclopedia de prestigio, como la Encyclopaedia Britannica (pero recuerda que no se trata de una enciclopedia general, sino específicamente de una enciclopedia aobre el altruismo eficaz, por lo que debe redactarse desde esa perspectiva).\n\nCuando hagas una afirmación que pueda atribuirse a una fuente específica, menciona dicha fuente en una nota al pie. En general, procure que no haya más de una nota a pie de página por párrafo, aunque puede incluir varias referencias en una misma nota. No incluyas una sección de referencias al final. Utiliza sintaxis de Markdown para redactar el artículo."
	     :language "es"))
  "Prompt for writing a reference article.")

(defconst tlon-ai-proofread-reference-article-prompt
  `((:prompt "You are an expert proofreader. Please proofread the following article. The article is intended for an encyclopedia of effective altruism. Your task is to correct any errors you find, especially factual errors, calculation errors, and any other errors that you think are important.\n\n%s"
	     :language "en")
    (:prompt "Eres un corrector experto. Por favor, corrije el siguiente artículo. El artículo está destinado a una enciclopedia sobre altruismo eficaz. Tu tarea consiste en corregir los errores que encuentres, especialmente errores fácticos, errores de cálculo y cualquier otro error que consideres importante.\n\n%s"
	     :language "es")))

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
  `((:prompt "Please take a look at the two paragraphs attached (paragraphs may contain only one word). The first, ‘%s’, is taken from the original document in %s, while the second, ‘%s’, is taken from a translation of that document into %s. In the translation, some of the original formatting (which includes not only Markdown elements but potentially HTML components and SSML tags) has been lost or altered. What I want you to do is to generate a new paragraph with all the original formatting restored, but without changing the text of the translation. Add missing links. Add missing asterisks. Add any other missing markdown signs. Don't add any missing text. Don't change the name of the files referred to as images. And please do not surround the text in backticks. Add missing links. Add missing asterisks. Add any other missing markdown signs. Just give the output but don't add comments or clarifications, even if there's nothing to restore. Thank you!"
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

;;;;; Bibliographic Reference Extraction

(defconst tlon-ai-extract-references-prompt
  (format "You are an expert academic assistant. Please carefully scan the following text and extract all bibliographic references you can find.%s Return each distinct reference on a new line. Do not include any commentary, numbering, or bullet points, just the references themselves. Examples of references might look like 'Author (Year) Title', 'Author, A. B. (Year). Title of work.', etc."
          tlon-ai-string-wrapper)
  "Prompt for extracting bibliographic references from text.")

(defconst tlon-ai-get-bibkeys-prompt
  "You are an expert bibliographic database lookup tool. You will be given an 'Input Reference' string and a 'Database' in JSON format containing bibliographic entries.\n\nYour task is to find the *single best matching entry* in the Database for the Input Reference. The match should be based on semantic similarity (author, title, year), even if the strings are not identical. \n\nOnce you find the best match, you must return *only* the value of the 'key' field for that matching entry. Do not return anything else - no explanations, no 'The key is:', just the key string itself.\n\nIf you cannot find a reasonably good match in the Database, return the exact string 'NOT_FOUND'.\n\nInput Reference:\n%s\n\nDatabase:\n```json\n%s\n```\n\nKey:"
  ;; %s will be the single input reference
  ;; %s will be the single input reference
  ;; %s will be the JSON database string
  "Prompt for finding a BibTeX key for a single reference against a JSON database.")

(defconst tlon-ai-extract-exact-references-prompt
  (format "You are an expert academic assistant. Please carefully scan the following text and extract all bibliographic references you can find.%s Return each distinct reference *exactly* as it appears in the text, including all original punctuation and spacing. Each reference should be on a new line. Do not include any commentary, numbering, or bullet points, just the exact reference strings themselves."
          tlon-ai-string-wrapper)
  "Prompt for extracting bibliographic references exactly as found.")

(defconst tlon-ai-get-bibkeys-batch-prompt
  "You are an expert bibliographic database lookup tool. You will be given a list of 'Input References' (one per line) and a 'Database' in JSON format containing bibliographic entries.\n\nYour task is to find the *single best matching entry* in the Database for *each* Input Reference. The match should be based on semantic similarity (author, title, year), even if the strings are not identical.\n\nReturn a list of keys, one per line, corresponding *exactly* to the order of the Input References. For each Input Reference:\n- If you find a good match, return the value of the 'key' field for that entry.\n- If you cannot find a reasonably good match, return the exact string 'NOT_FOUND'.\n\nDo not return anything else - no explanations, no numbering, just the list of keys (or 'NOT_FOUND'), one per line.\n\nInput References:\n```\n%s\n```\n\nDatabase:\n```json\n%s\n```\n\nKeys:"
  ;; %s will be the newline-separated list of input references
  ;; %s will be the JSON database string
  "Prompt for finding BibTeX keys for multiple references against a JSON database.")

;;;; Functions

;;;;; General

(defun tlon-make-gptel-request (prompt &optional string callback full-model no-context-check)
  "Make a `gptel' request with PROMPT and STRING and CALLBACK.
When STRING is non-nil, PROMPT is a formatting string containing the prompt and
a slot for a string, which is the variable part of the prompt (e.g. the text to
be summarized in a prompt to summarize text). When STRING is nil (because there
is no variable part), PROMPT is the full prompt. FULL-MODEL is a cons cell whose
car is the backend and whose cdr is the model.

By default, warn the user if the context is not empty. If NO-CONTEXT-CHECK is
non-nil, bypass this check."
  (unless tlon-ai-batch-fun
    (tlon-warn-if-gptel-context no-context-check))
  (let ((full-model (or full-model (cons (gptel-backend-name gptel-backend) gptel-model)))
	(prompt (tlon-ai-maybe-edit-prompt prompt)))
    (cl-destructuring-bind (backend . model) full-model
      (let* ((gptel-backend (alist-get backend gptel--known-backends nil nil #'string=))
	     (full-prompt (if string (format prompt string) prompt))
	     (request (lambda () (gptel-request full-prompt :callback callback))))
	(if tlon-ai-batch-fun
	    (condition-case nil
		(funcall request)
	      (error nil))
	  (funcall request))))))

(defun tlon-ai-maybe-edit-prompt (prompt)
  "If `tlon-ai-edit-prompt' is non-nil, ask user to edit PROMPT, else return it."
  (if tlon-ai-edit-prompt
      (read-string "Prompt: " prompt)
    prompt))

(defun tlon-warn-if-gptel-context (&optional no-context-check)
  "Prompt for confirmation to proceed when `gptel' context is not empty.
If NO-CONTEXT-CHECK is non-nil, by pass the check."
  (unless (or no-context-check
	      (null gptel-context--alist)
	      (y-or-n-p "The `gptel' context is not empty. Proceed? "))
    (let ((message "Aborted"))
      (when (y-or-n-p "Clear the `gptel' context? ")
	(gptel-context-remove-all)
	(setq message (concat message " (context cleared)")))
      (user-error message))))

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

(autoload 'bibtex-next-entry "bibtex")
(declare-function bibtex-extras-get-key "bibtex-extras")
(autoload 'ebib-extras-next-entry "ebib-extras")
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

(autoload 'ebib-extras-get-text-file "ebib-extras")
(autoload 'tlon-md-read-content "tlon-md")
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

(autoload 'shr-render-buffer "shr")
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

;;;;; Writing

;;;;;; Reference article

(declare-function tlon-yaml-get-key "tlon-yaml")
(defun tlon-ai-create-reference-article ()
  "Create a new reference article using AI."
  (interactive)
  (if-let ((title (tlon-yaml-get-key "title")))
      (let* ((lang (tlon-get-language-in-file nil 'error))
	     (prompt (format (tlon-lookup tlon-ai-write-reference-article-prompt
					  :prompt :language lang)
			     title)))
	(tlon-warn-if-gptel-context)
	(tlon-add-add-sources-to-context)
	(tlon-add-glossary-to-context lang)
	(tlon-make-gptel-request prompt nil #'tlon-ai-create-reference-article-callback
				 tlon-ai-create-reference-article-model 'no-context-check)
	(message "Creating reference article with %S..."
		 (or (cdr tlon-ai-create-reference-article-model) gptel-model)))
    (user-error "No \"title\" value found in front matter")))

(declare-function markdown-mode "markdown-mode")
(defun tlon-ai-create-reference-article-callback (response info)
  "Callback for `tlon-ai-create-reference-article'.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let* ((title (tlon-ai-get-reference-article-title response))
	   (buffer-name (if title
			    (generate-new-buffer-name title)
			  (generate-new-buffer-name "*new-article*")))
	   (buffer (get-buffer-create buffer-name)))
      (tlon-ai-insert-in-buffer-and-switch-to-it response buffer)
      (gptel-context-remove-all)
      (when (y-or-n-p "Proofread the article? ")
	(tlon-ai-proofread-reference-article)))))

(defun tlon-ai-get-reference-article-title (response)
  "Return the title of the reference article in RESPONSE."
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (when (looking-at "# \\(.*\\)$")
      (match-string 1))))

(defun tlon-ai-proofread-reference-article ()
  "Proofread the current reference article using AI."
  (interactive)
  (let ((prompt (tlon-lookup tlon-ai-proofread-reference-article-prompt
			     :prompt :language (or (tlon-get-language-in-file nil)
						   (tlon-select-language 'code)))))
    (tlon-make-gptel-request prompt (buffer-string) #'tlon-ai-proofread-reference-article-callback
			     tlon-ai-proofread-reference-article-model)
    (message "Proofreading reference article with %S..."
	     (or (cdr tlon-ai-proofread-reference-article-model) gptel-model))))

(defun tlon-ai-proofread-reference-article-callback (response info)
  "Callback for `tlon-ai-proofread-reference-article'.
RESPONSE is the response from the AI model and INFO is the response info."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let* ((title (tlon-ai-get-reference-article-title response))
           (buffer-name (if title
                            (generate-new-buffer-name (format "Comments on %s" title))
                          (generate-new-buffer-name "*Comments on article*")))
           (buffer (get-buffer-create buffer-name)))
      (tlon-ai-insert-in-buffer-and-switch-to-it response buffer))))

(defun tlon-ai-insert-in-buffer-and-switch-to-it (response buffer)
  "Insert RESPONSE in BUFFER and switch to it."
  (with-current-buffer buffer
    (erase-buffer)
    (insert response)
    (markdown-mode)
    (goto-char (point-min)))
  (switch-to-buffer buffer))

(autoload 'markdown-narrow-to-subtree "markdown-mode")
(declare-function tlon-md-get-tag-section "tlon-md")
(defun tlon-ai-get-keys-in-section ()
  "Return a list of BibTeX keys in the \"Further reading\" section."
  (let* ((lang (tlon-get-language-in-file nil 'error))
	 (section (tlon-md-get-tag-section "Further reading" lang))
	 keys)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(re-search-forward (format "^#\\{1,\\} %s" section))
	(markdown-narrow-to-subtree)
	(while (re-search-forward (tlon-md-get-tag-pattern "Cite") nil t)
	  (push (match-string-no-properties 3) keys))
	keys))))

(declare-function bibtex-extras-get-entry-as-string "bibtex-extras")
(defun tlon-ai-add-source-to-context (key)
  "Add the PDF file associated with KEY to the context."
  (if-let ((field (bibtex-extras-get-entry-as-string key "file")))
      (let* ((files (split-string field ";"))
	     (pdf-files (seq-filter (lambda (file)
				      (string-match-p "\\.pdf$" file))
				    files)))
	(tlon-ai-ensure-one-file key pdf-files)
	;; we convert to text because some AI models limit the number or pages
	;; of PDF files
	(let ((text (tlon-get-string-dwim (car pdf-files)))
	      (file (make-temp-file "pdf-to-text-")))
	  (with-temp-buffer
	    (insert text)
	    (write-region (point-min) (point-max) file))
	  (gptel-context-add-file file)))
    (user-error "No `file' field found in entry %s" key)))

(defun tlon-add-add-sources-to-context ()
  "Add all PDF files in the current buffer to the context."
  (mapc (lambda (key)
	  (tlon-ai-add-source-to-context key))
	(tlon-ai-get-keys-in-section))
  (message "Added all PDF files of the keys in the current buffer to the `gptel' context."))

(declare-function tlon-extract-glossary "tlon-glossary")
(declare-function tlon-glossary-target-path "tlon-glossary")
(defun tlon-add-glossary-to-context (lang)
  "Add the glossary of LANG to the context."
  (unless (string= lang "en")
    (tlon-extract-glossary lang 'deepl-editor)
    (gptel-context-add-file (tlon-glossary-target-path lang 'deepl-editor))))

(defun tlon-ai-ensure-one-file (key pdf-files)
  "Ensure PDF-FILES has exactly one PDF file.
KEY is the key of the entry containing the PDF files."
  (let ((file-count (length pdf-files)))
    (when (/= file-count 1)
      (pcase file-count
	(0 (user-error "No PDF files found in %s" key))
	(_ (user-error "Multiple PDF files found in %s" key))))))

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
(defun tlon-ai-describe-image (&optional file callback)
  "Describe the contents of the image in FILE.
By default, print the description in the minibuffer. If CALLBACK is non-nil, use
it instead."
  (interactive)
  ;; we warn here because this command adds files to the context, so the usual
  ;; check downstream must be bypassed via `no-context-check'
  (tlon-warn-if-gptel-context)
  (let* ((previous-context gptel-context--alist)
         (file (tlon-ai-read-image-file file))
         (language (tlon-get-language-in-file file))
         (default-prompt (tlon-lookup tlon-ai-describe-image-prompt :prompt :language language))
         (custom-callback (lambda (response info)
                            (when callback
                              (funcall callback response info))
                            (unless callback
                              (if response
                                  (message response)
                                (user-error "Error: %s" (plist-get info :status))))
                            (setq gptel-context--alist previous-context))))
    (gptel-context-remove-all)
    (gptel-context-add-file file)
    (tlon-make-gptel-request default-prompt nil custom-callback nil 'no-context-check)))

(autoload 'tlon-get-tag-attribute-values "tlon-md")
(autoload 'tlon-md-insert-attribute-value "tlon-md")
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

(autoload 'tlon-md-get-tag-pattern "tlon-md")
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

(autoload 'dired-get-filename "dired")
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
use the file visited by the current buffer.
Aborts the whole process if any paragraph permanently fails (after 3 attempts),
logging detailed error information for debugging.
If `tlon-ai-use-markdown-fix-model' is non-nil, use the model specified in
`tlon-ai-markdown-fix-model'; otherwise, use the active model.
Messages refer to paragraphs with one-based numbering."
  (interactive)
  (let* ((file (or file (buffer-file-name) (read-file-name "File to fix: ")))
         (original-file (tlon-get-counterpart file))
         (original-lang (tlon-get-language-in-file original-file))
         (translation-lang (tlon-get-language-in-file file))
         (prompt (tlon-ai-maybe-edit-prompt
                  (tlon-lookup tlon-ai-fix-markdown-format-prompt
                               :prompt :language "en")))
         (pairs (tlon-get-corresponding-paragraphs file original-file))
         (all-pairs-count (length pairs))
         (results (make-vector all-pairs-count nil))
         (completed 0)
         (active-requests 0)
         (max-concurrent 3)           ; lower concurrency helps avoid rate limiting
         (retry-table (make-hash-table :test 'equal))
         (abort-flag nil)
         ;; Define ISSUE-REQUEST: use the fix model if defined.
         (issue-request
          (lambda (full-prompt callback)
            (if tlon-ai-markdown-fix-model
                (tlon-make-gptel-request full-prompt nil callback tlon-ai-markdown-fix-model)
              (gptel-request full-prompt :callback callback)))))
    (cl-labels
        ;; CHECK-COMPLETION: When all paragraphs are done, write the file.
        ((check-completion ()
           (when (and (= completed all-pairs-count)
                      (= active-requests 0)
                      (not abort-flag))
             (write-fixed-file)))
         ;; WRITE-FIXED-FILE: Write the fixed content.
         (write-fixed-file ()
           (message "Processing complete. Writing fixed file...")
           (let ((fixed-file-path (concat (file-name-sans-extension file)
                                          "--fixed.md")))
             (with-temp-buffer
               (dotimes (i all-pairs-count)
                 (insert (or (aref results i) "") "\n\n"))
               (write-region (point-min) (point-max) fixed-file-path))
             (find-file fixed-file-path)
             (when (y-or-n-p "Done! Run ediff session? ")
               (ediff-files file fixed-file-path))))
         ;; PROCESS-SINGLE-PARAGRAPH: Process one paragraph (i is zero-based, but messages show i+1).
         (process-single-paragraph (i)
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
             (funcall issue-request
                      formatted-prompt
                      (lambda (response info)
                        (cl-decf active-requests)
                        (if response
                            (progn
                              (aset results i response)
                              (cl-incf completed)
                              (message "Processed paragraph %d (%d%%)"
                                       (1+ i)
                                       (round (* 100 (/ (float completed)
                                                        all-pairs-count))))
                              (check-completion))
                          (let* ((status (plist-get info :status))
                                 (retry-count (or (gethash i retry-table) 0)))
                            (if (< retry-count 3)
                                (progn
                                  (puthash i (1+ retry-count) retry-table)
                                  (message "Paragraph %d failed with %S; retrying attempt %d of 3..."
                                           (1+ i) status (1+ retry-count))
                                  ;; Exponential backoff: wait longer on subsequent failures.
                                  (run-with-timer (* 2 (1+ retry-count)) nil
                                                  (lambda ()
                                                    (process-single-paragraph i))))
                              (setq abort-flag t)
                              (error "Aborting: Paragraph %d permanently failed after 3 attempts. Status: %S, info: %S"
                                     (1+ i) status info)))))))))
      (message "Fixing format of `%s' (%d paragraphs)..."
               (file-name-nondirectory file) all-pairs-count)
      (dotimes (i all-pairs-count)
        (process-single-paragraph i)))))

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

(autoload 'tlon-abstract-may-proceed-p "tlon-tex")
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
it finds one, use it. Otherwise it will create an abstract from scratch.

TYPE is either `abstract' or `synopsis'."
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
      (progn
	(tlon-make-gptel-request prompt string callback tlon-ai-summarization-model)
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

;;;;; Help

;;;###autoload
(defun tlon-ai-ask-for-help ()
  "Ask a question about the tlon ecosystem using documentation files as context.
Collects documentation files from the standard tlon and extras doc directories,
adds them to the AI context, and sends the user's question using the model
specified in `tlon-ai-help-model'."
  (interactive)
  (tlon-warn-if-gptel-context)
  (let* ((question (read-string "What do you need help with? "))
         (all-doc-files (tlon-ai-get-documentation-files))
         (existing-doc-files '())
         (prompt-template (format "Here is the documentation for all processes and programs within the Tlön organization. They range from project readme files to docs for all the Emacs functionality used by this organization (including extensions for common Emacs packages or features (‘extras’), modules that provide new functionality within the comprehensive ‘tlon’ package, and a detailed configuration file (‘config.org’). An employee has asked a question that may relate to any of these processes, and I want you to answer it to the best of your ability. Please format your answer using %S syntax. Here is the question:\n\n%%s" gptel-default-mode))
         full-prompt)
    (unless all-doc-files
      (user-error "No documentation files found in standard Elpaca doc directories"))
    ;; Add all found documentation files to the context
    (dolist (doc-file all-doc-files)
      (when (file-exists-p doc-file)
        (shut-up (gptel-context-add-file doc-file))
        (push doc-file existing-doc-files)))
    ;; Check if any files were actually added (existed)
    (unless existing-doc-files
      (user-error "Found documentation file entries, but none exist on disk"))
    ;; Now format the prompt with the actual number of files added
    (setq full-prompt (format prompt-template question))
    ;; Use the specific help model
    (tlon-make-gptel-request full-prompt nil
                             ;; Pass question to the callback via closure
                             (lambda (response info)
                               (tlon-ai-ask-for-help-callback response info question))
                             tlon-ai-help-model 'no-context-check)
    (message "Preparing your answer using %d documentation file(s) with model %S..."
             (length existing-doc-files) (or tlon-ai-help-model gptel-model))))

(declare-function gptel-mode "gptel")
(defvar gptel-default-mode)
(defun tlon-ai-ask-for-help-callback (response info question)
  "Callback for `tlon-ai-ask-for-help'.
Displays the QUESTION and RESPONSE in a new `gptel-mode' buffer. If RESPONSE is
nil, use `tlon-ai-callback-fail'. INFO is the context information passed to the
request. QUESTION is the original user question."
  (if (not response)
      (tlon-ai-callback-fail info) ; Use the fail callback from tlon-ai
    (let* ((buffer-name (generate-new-buffer-name "*AI Help Answer*"))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (erase-buffer)
        ;; Insert question and answer
        (insert (format "*** %s\n\n" question))
        (insert response)
        ;; Set mode, enable gptel-mode, make writable
        (funcall gptel-default-mode)
        (gptel-mode 1)
        (setq buffer-read-only nil)
        (when (eq gptel-default-mode 'org-mode)
          (org-fold-show-all)) ; Ensure content under heading is visible
        (goto-char (point-max))) ; Move point to end for follow-up
      (switch-to-buffer buffer)
      ;; Ask about clearing context
      (let ((clear-context (y-or-n-p "Clear the gptel context (recommended)? ")))
        (if clear-context
            (gptel-context-remove-all)
          (message "Context not cleared. Remember to clear it manually with `M-x gptel-context-remove-all` when finished."))))))

(declare-function paths-dir-dotemacs "tlon-paths")
(defvar elpaca-repos-directory)
(defun tlon-ai-get-documentation-files ()
  "Return a list of full paths to documentation files.
Documentation files are collected from:
1. `.org` files within the \"doc/\" subdirectories of specified Elpaca repos.
2. `readme.org` or `readme.md` files in specified Elpaca repos.
3. Specific individual files."
  (let* ((all-doc-files '())
         ;; 1. Directories containing .org documentation files
         (doc-dirs (list (file-name-concat elpaca-repos-directory "tlon/doc/")
                         (file-name-concat elpaca-repos-directory "dotfiles/emacs/extras/doc/")))
         (doc-pattern "\\.org\\'")
         ;; 2. Repositories to check for readme files
         (repos (tlon-lookup-all tlon-repos :dir :help t))
         (readme-patterns '("readme.org" "readme.md"))
         ;; 3. Specific individual files
         (files (list (file-name-concat paths-dir-dotemacs "config.org"))))
    ;; Collect files from doc-dirs
    (dolist (doc-dir doc-dirs)
      (when (file-directory-p doc-dir)
        (setq all-doc-files (append all-doc-files
                                    (directory-files doc-dir t doc-pattern)))))
    ;; Collect readme files from repos
    (dolist (repo-dir repos)
      (when (file-directory-p repo-dir)
        (dolist (pattern readme-patterns)
          (let ((readme-file (file-name-concat repo-dir pattern)))
            (when (file-exists-p readme-file)
              (push readme-file all-doc-files))))))
    ;; Collect individual files
    (dolist (file files)
      (when (file-exists-p file)
        (push file all-doc-files)))
    ;; Remove duplicates and return
    (delete-dups all-doc-files)))

;;;;;; BibTeX

(declare-function bibtex-set-field "bibtex")
(autoload 'ebib-extras-set-field "ebib-extras")
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

(autoload 'ebib-extras-get-or-open-entry "bibtex-extras")
(autoload 'bibtex-extras-get-entry-as-string "bibtex-extras")
(defun tlon-ai-detect-language-in-bibtex (&optional string)
  "Detect language in STRING.
If STRING is nil, use the current BibTeX entry."
  (let ((string (or string (pcase major-mode
			     ('ebib-entry-mode (ebib-extras-get-or-open-entry))
			     ('bibtex-mode (bibtex-extras-get-entry-as-string))
			     (_ (user-error "I can’t detect language in %s" major-mode))))))
    (tlon-make-gptel-request tlon-ai-detect-language-bibtex-prompt string
			     #'tlon-ai-callback-return)))

(autoload 'bibtex-beginning-of-entry "bibtex")
(autoload 'bibtex-extras-get-field-in-string "bibtex-extras")
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

(autoload 'word-at-point "thingatpt")
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

;;;;; Audio transcription

(autoload 'tlon-tts-openai-get-or-set-key "tlon-tts")
(declare-function request "request")
(defun tlon-transcribe-audio (file callback)
  "Asynchronously transcribe the audio in FILE using OpenAI's Whisper API via curl.
FILE is the audio file to transcribe. CALLBACK is a function that is called with
the transcript string on success, or nil if no transcript is available or an
error occurs."
  (interactive "fChoose audio file: ")
  (let* ((api-key (tlon-tts-openai-get-or-set-key))
         (endpoint "https://api.openai.com/v1/audio/transcriptions"))
    (unless api-key
      (error "Could not retrieve API key"))
    (message "Uploading %s to OpenAI via curl asynchronously..." file)
    (let* ((output-buffer (generate-new-buffer "/openai-transcribe-output/"))
           (args (list "-s" "-X" "POST"
		       endpoint
		       "-H" (concat "Authorization: Bearer " api-key)
		       "-F" "model=whisper-1"
		       "-F" (concat "file=@" (expand-file-name file)))))
      (let ((proc (apply 'start-process "openai-transcribe-process" output-buffer "curl" args)))
	(set-process-sentinel
	 proc
	 (lambda (process _)
	   (when (eq (process-status process) 'exit)
	     (with-current-buffer (process-buffer process)
	       (let ((output (buffer-string)))
		 (condition-case err
		     (let* ((json-object-type 'hash-table)
			    (json-array-type 'list)
			    (json-key-type 'string)
			    (response (json-read-from-string output))
			    (transcript (gethash "text" response)))
		       (if transcript
			   (progn
			     (message "Transcription complete.")
			     (funcall callback transcript))
			 (progn
			   (message "No transcript returned. Full response: %s" output)
			   (funcall callback nil))))
		   (error
		    (message "Error parsing JSON response: %s" (error-message-string err))
		    (message "Response was: %s" output)
		    (funcall callback nil)))))
	     (kill-buffer (process-buffer process)))))))))

;;;;; Math

(defun tlon-ai-convert-math (&optional expression language)
  "Convert math EXPRESSION in LANGUAGE into LaTeX."
  (interactive)
  (tlon-ai-process-math 'convert expression language))

(defun tlon-ai-translate-math (&optional expression language)
  "Translate math EXPRESSION in LANGUAGE into alt text."
  (interactive)
  (tlon-ai-process-math 'translate expression language))

(autoload 'tlon-looking-at-tag-p "tlon-md")
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

;;;;; Bibliography Extraction

;;;###autoload
(defun tlon-ai-extract-references (&optional use-region)
  "Scan the current buffer or region for bibliographic references using AI.
Display the found references in the *Messages* buffer and copies
them (newline-separated) to the kill ring. With prefix argument USE-REGION,
operate only on the active region."
  (interactive "P")
  (let ((text (if use-region
                  (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (user-error "Region not active"))
                (buffer-string))))
    (when (string-empty-p text)
      (user-error "Buffer or region is empty"))
    (message "Requesting AI to extract references...")
    (tlon-make-gptel-request tlon-ai-extract-references-prompt text
                             #'tlon-ai-extract-references-callback)))

(defun tlon-ai-extract-references-callback (response info)
  "Callback for `tlon-ai-extract-references'.
Displays the found references and copies them to the kill ring. RESPONSE is the
AI's response, INFO is the response info."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let* ((references (split-string (string-trim response) "\n" t)) ; Split by newline, remove empty
           (count (length references)))
      (kill-new (mapconcat #'identity references "\n"))
      (message "AI found %d potential reference(s). Copied to kill ring." count)
      ;; Optionally display the references (might be long)
      (when (> count 0)
        (message "References:\n%s%s"
                 (mapconcat (lambda (ref) (concat "- " ref))
                            (seq-take references (min count 10)) ; Show first 10
                            "\n")
                 (if (> count 10) "\n..." ""))))))

;;;;;; Bibkey Lookup Command and Helpers

(defvar tlon-ai--bibkey-state nil
  "Internal state variable for asynchronous bibkey lookup.")

;;;###autoload
(defun tlon-ai-get-bibkeys-from-references (beg end)
  "Replace bibliographic references in region with <Cite> tags using AI.
Scans each *non-blank line* in the active region (BEG END), treats the
*entire line* as a single reference, and asks the AI in a *single batch
request* to find the corresponding BibTeX keys in
`tlon-file-bare-bibliography`. Replaces each original reference line with
`<Cite bibKey=\"KEY\" />` if a key is found. Lines where no key is found or an
error occurs are left unchanged.

WARNING: This function replaces the *entire line*. If a line contains text
other than the reference (e.g., footnote markers) or multiple references, the
replacement might be incorrect. Consider using
`tlon-ai-extract-and-replace-references` (bound to `X` in the menu) for more
precise replacement."
  (interactive "r") ; Require region
  (unless (file-exists-p tlon-file-bare-bibliography)
    (user-error "Bibliography file not found: %s" tlon-file-bare-bibliography))

  (let* ((references-with-pos '()) ; List of (text start end)
         (reference-texts '())     ; List of just the text strings
         (db-string (with-temp-buffer
                      (insert-file-contents tlon-file-bare-bibliography)
                      (buffer-string)))
         (source-buffer (current-buffer))) ; Store the buffer where the command was invoked

    ;; Parse region line by line, storing text and positions
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               ;; Ensure we don't go past the original region end
               (actual-end (min line-end end))
               (line-text (buffer-substring-no-properties line-start actual-end)))
          (unless (string-blank-p line-text)
            (let ((trimmed-text (string-trim line-text)))
              (push (list trimmed-text line-start actual-end) references-with-pos)
              (push trimmed-text reference-texts))))
        (forward-line 1)))

    ;; Reverse lists to maintain original order
    (setq references-with-pos (nreverse references-with-pos))
    (setq reference-texts (nreverse reference-texts))

    (when (string-empty-p db-string)
      (user-error "Bibliography file is empty: %s" tlon-file-bare-bibliography))
    (unless references-with-pos
      (user-error "No non-blank reference lines found in region"))

    ;; Initialize state for the asynchronous process
    (setq tlon-ai--bibkey-state
          `(:references-with-pos ,references-with-pos ; List of (text start end)
				 :db-string ,db-string
				 :results () ; Will store (start . end . key)
				 :source-buffer ,source-buffer))

    (let* ((references-block (mapconcat #'identity reference-texts "\n"))
           (prompt (format tlon-ai-get-bibkeys-batch-prompt references-block db-string)))
      (message "Starting batch BibTeX key lookup for %d references in region..."
               (length references-with-pos))
      ;; Make the single batch request
      (tlon-make-gptel-request prompt nil #'tlon-ai--batch-bibkey-result-handler nil t) ; Bypass context check
      (message "AI request sent. Waiting for BibTeX keys..."))))

(defun tlon-ai--batch-bibkey-result-handler (response info)
  "Callback function to handle the result of a batch bibkey lookup.
Parses the newline-separated keys, associates them with original references, and
triggers replacements. RESPONSE is the AI's response, INFO is the response info."
  (let* ((state tlon-ai--bibkey-state)
         (references-with-pos (plist-get state :references-with-pos))
         (num-references (length references-with-pos))
         (results '())) ; Build the results list here

    (unless response
      (message "AI batch request failed. Status: %s" (plist-get info :status))
      (setq tlon-ai--bibkey-state nil) ; Clean up state
      (cl-return-from tlon-ai--batch-bibkey-result-handler))

    (let ((returned-keys (split-string (string-trim response) "\n" t)))
      (unless (= (length returned-keys) num-references)
        (message "Error: AI returned %d keys, but %d references were sent. Aborting replacements."
                 (length returned-keys) num-references)
        (message "AI Response:\n%s" response) ; Log response for debugging
        (setq tlon-ai--bibkey-state nil) ; Clean up state
        (cl-return-from tlon-ai--batch-bibkey-result-handler))

      ;; Associate keys with positions
      (dotimes (i num-references)
        (let* ((entry (nth i references-with-pos))
               (start-pos (nth 1 entry))
               (end-pos (nth 2 entry))
               (key (nth i returned-keys)))
          (push (list start-pos end-pos key) results)))

      ;; Store the final results (reversed to match original order implicitly)
      (setf (plist-get state :results) (nreverse results))
      (setq tlon-ai--bibkey-state state) ; Update state with results

      ;; All references processed, apply replacements
      (tlon-ai--apply-bibkey-replacements))))

(defun tlon-ai--apply-bibkey-replacements ()
  "Apply the BibTeX key replacements in the source buffer."
  (let* ((state tlon-ai--bibkey-state)
         (results (plist-get state :results)) ; List of (start end key)
         (source-buffer (plist-get state :source-buffer))
         (replacements-made 0)
         (errors-occurred 0))

    (unless (buffer-live-p source-buffer)
      (message "Source buffer is no longer live. Aborting replacements.")
      (setq tlon-ai--bibkey-state nil)
      (cl-return-from tlon-ai--apply-bibkey-replacements))

    ;; Sort results by start position in REVERSE order to avoid messing up positions
    (setq results (sort results (lambda (a b) (> (car a) (car b)))))

    (with-current-buffer source-buffer
      (dolist (result results)
        (let ((start (nth 0 result))
              (end (nth 1 result))
              (key (nth 2 result)))
          ;; Check if key is valid (not an error marker)
          (if (or (string= key "NOT_FOUND") (string= key "ERROR_AI"))
              (progn
                (message "No valid key found for text at %d-%d (Result: %s). Skipping." start end key)
                (cl-incf errors-occurred))
            ;; Perform replacement
            (let ((replacement-text (format "<Cite bibKey=\"%s\" />" key)))
              (goto-char start) ; Go to start before deleting
              (delete-region start end)
              (insert replacement-text)
              (cl-incf replacements-made))))))

    (message "BibTeX key replacement complete. Replaced %d reference(s). Skipped %d due to errors or no match."
             replacements-made errors-occurred)
    ;; Clean up state variable
    (setq tlon-ai--bibkey-state nil)))
 
;;;;;; Extract and Replace Command
 
(defvar tlon-ai--extract-replace-state nil
  "Internal state variable for asynchronous reference extraction and replacement.")
 
;;;###autoload
(defun tlon-ai-extract-and-replace-references (&optional use-region)
  "Extract references in buffer/region, get BibKeys, and replace with <Cite> tags.
Uses AI to find references, then AI again to find BibKeys against
`tlon-file-bare-bibliography`. Replaces the *first found occurrence* (searching
backwards) of each *exact* reference string with a <Cite> tag.
 
WARNING: Relies on AI returning exact reference strings. May fail or replace
incorrectly if the AI modifies the string or if the same reference appears
multiple times.
 
With prefix argument USE-REGION, operate only on the active region."
  (interactive "P")
  (unless (file-exists-p tlon-file-bare-bibliography)
    (user-error "Bibliography file not found: %s" tlon-file-bare-bibliography))
 
  (let* ((beg (if use-region (region-beginning) (point-min)))
         (end (if use-region (region-end) (point-max)))
         (text (buffer-substring-no-properties beg end))
         (db-string (with-temp-buffer
                      (insert-file-contents tlon-file-bare-bibliography)
                      (buffer-string)))
         (source-buffer (current-buffer)))
    (when (string-empty-p text)
      (user-error "Buffer or region is empty"))
    (when (string-empty-p db-string)
      (user-error "Bibliography file is empty: %s" tlon-file-bare-bibliography))
 
    ;; Initialize state
    (setq tlon-ai--extract-replace-state
          `(:source-buffer ,source-buffer
			   :region-start ,beg
			   :region-end ,end
			   :db-string ,db-string
			   :extracted-references () ; List of strings from AI
			   :reference-positions ()  ; Alist: (ref-string . list-of-(start . end))
			   :unique-references ()    ; List of unique ref strings
			   :key-map ()              ; Hash table: ref-string -> key
			   :keys-to-fetch 0
			   :keys-fetched 0))
 
    (message "Requesting AI to extract exact references...")
    (tlon-make-gptel-request tlon-ai-extract-exact-references-prompt text
                             #'tlon-ai--extract-references-exact-callback)))
 
(defun tlon-ai--extract-references-exact-callback (response info)
  "Callback for the initial reference extraction.
Finds positions and starts key lookup. RESPONSE is the AI's response, INFO is
the response info."
  (if (not response)
      (progn
        (setq tlon-ai--extract-replace-state nil) ; Clean up state
        (tlon-ai-callback-fail info))
    (let* ((state tlon-ai--extract-replace-state)
           (extracted-refs (split-string (string-trim response) "\n" t)))
      (setf (plist-get state :extracted-references) extracted-refs)
      (message "AI extracted %d potential references. Finding positions..." (length extracted-refs))
      (setf (plist-get state :reference-positions)
            (tlon-ai--find-reference-positions
             extracted-refs
             (plist-get state :source-buffer)
             (plist-get state :region-start)
             (plist-get state :region-end)))
      (let ((unique-refs (cl-delete-duplicates
			  (mapcar #'car (plist-get state :reference-positions)) :test #'string=)))
        (setf (plist-get state :unique-references) unique-refs)
        (setf (plist-get state :keys-to-fetch) (length unique-refs))
        (setf (plist-get state :key-map) (make-hash-table :test 'equal))
        (if (zerop (plist-get state :keys-to-fetch))
            (progn
              (message "No references found or positions located.")
              (setq tlon-ai--extract-replace-state nil)) ; Clean up
          (message "Found positions for %d unique references. Requesting BibTeX keys in batch..." (length unique-refs))
          (tlon-ai--get-keys-for-extracted-references)))))) ; Start batch key lookup
 
(defun tlon-ai--find-reference-positions (references buffer beg end)
  "Search for occurrences of REFERENCES strings within BUFFER between BEG and END.
Returns an alist: (ref-string . list-of-(start . end))."
  (let ((positions-alist '()))
    (with-current-buffer buffer
      (dolist (ref references positions-alist)
        (let ((ref-positions '()))
          (save-excursion
            (goto-char end) ; Start searching backwards from the end
            (while (search-backward ref beg t)
              (let* ((match-start (match-beginning 0))
                     (match-end (match-end 0))
                     (line-start (line-beginning-position))
                     (adjusted-start match-start))
                ;; Check if the match is preceded by a footnote marker at line start
                (save-excursion
                  (goto-char match-start)
                  (when (and (= (point) line-start) ; Ensure match starts exactly at line beginning
                             (looking-at "\\[\\^[0-9]+\\]: "))
                    ;; If marker found, adjust start position past the marker
                    (setq adjusted-start (match-end 0))))
                (push (cons adjusted-start match-end) ref-positions))))
          (when ref-positions
            (push (cons ref ref-positions) positions-alist)))))))
 
(defun tlon-ai--get-keys-for-extracted-references ()
  "Initiate a single batch request to get BibTeX keys for extracted references."
  (let* ((state tlon-ai--extract-replace-state)
         (unique-refs (plist-get state :unique-references))
         (db-string (plist-get state :db-string)))
    (if (null unique-refs)
        (progn
          (message "No unique references to look up keys for.")
          (setq tlon-ai--extract-replace-state nil)) ; Clean up state
      (let* ((references-block (mapconcat #'identity unique-refs "\n"))
             (prompt (format tlon-ai-get-bibkeys-batch-prompt references-block db-string)))
        ;; Make the single batch request
        (tlon-make-gptel-request prompt nil #'tlon-ai--extracted-batch-bibkey-result-handler nil t))))) ; Use new batch callback
 
(defun tlon-ai--extracted-batch-bibkey-result-handler (response info)
  "Callback to handle the result of batch bibkey lookup for extracted references.
Parses the newline-separated keys, populates the key-map, and triggers
replacements. RESPONSE is the AI's response, INFO is the response info."
  (let* ((state tlon-ai--extract-replace-state)
         (unique-refs (plist-get state :unique-references))
         (key-map (plist-get state :key-map)) ; Hash table: ref-string -> key
         (num-references (length unique-refs)))
    
    (if response
        ;; Process valid response
        (let ((returned-keys (split-string (string-trim response) "\n" t)))
          (if (= (length returned-keys) num-references)
              ;; Correct number of keys returned
              (progn
                ;; Populate the key-map hash table
                (dotimes (i num-references)
                  (let ((ref-string (nth i unique-refs))
                        (key (nth i returned-keys)))
                    (puthash ref-string key key-map)))
                (message "All keys fetched via batch request. Applying replacements...")
                (tlon-ai--apply-extracted-reference-replacements)) ; Proceed to replacements
            ;; Incorrect number of keys returned
            (message "Error: AI returned %d keys, but %d unique references were sent. Aborting replacements."
                     (length returned-keys) num-references)
            (message "AI Response:\n%s" response) ; Log response for debugging
            (setq tlon-ai--extract-replace-state nil))) ; Clean up state
      ;; Handle failed AI request
      (message "AI batch key lookup request failed. Status: %s" (plist-get info :status))
      (setq tlon-ai--extract-replace-state nil)))) ; Clean up state

(defun tlon-ai--apply-extracted-reference-replacements ()
  "Apply the BibTeX key replacements in the source buffer for extracted references."
  (let* ((state tlon-ai--extract-replace-state)
         ;; Check if state is nil (might have been cleaned up due to error)
         (_ (unless state (user-error "State lost, likely due to previous error. Aborting")))
         (source-buffer (plist-get state :source-buffer))
         (ref-positions (plist-get state :reference-positions)) ; (ref-string . list-of-(start . end))
         (key-map (plist-get state :key-map))
         (replacements '()) ; List of (start end replacement-text)
         (replacements-made 0)
         (errors-occurred 0)
         (not-found-count 0))
    (unless (buffer-live-p source-buffer)
      (message "Source buffer is no longer live. Aborting replacements.")
      (setq tlon-ai--extract-replace-state nil)
      (cl-return-from tlon-ai--apply-extracted-reference-replacements))
    ;; Build the list of replacements
    (dolist (pos-entry ref-positions)
      (let* ((ref-string (car pos-entry))
             (positions (cdr pos-entry))
             (key (gethash ref-string key-map "ERROR_AI"))) ; Default to error if somehow missing
        (if (or (string= key "NOT_FOUND") (string= key "ERROR_AI"))
            (progn
              (when (string= key "NOT_FOUND") (cl-incf not-found-count))
              (when (string= key "ERROR_AI") (cl-incf errors-occurred)))
          ;; Valid key found, create replacement entries for all found positions
          (let ((replacement-text (format "<Cite bibKey=\"%s\" />" key)))
            (dolist (pos positions)
              (push (list (car pos) (cdr pos) replacement-text) replacements))))))
    ;; Sort replacements by start position in REVERSE order
    (setq replacements (sort replacements (lambda (a b) (> (car a) (car b)))))
    ;; Apply replacements
    (with-current-buffer source-buffer
      (dolist (replacement replacements)
        (let ((start (nth 0 replacement))
              (end (nth 1 replacement))
              (text (nth 2 replacement)))
          ;; Check if the region still contains the expected text? (Might be too complex/slow)
          (goto-char start)
          (delete-region start end)
          (insert text)
          (cl-incf replacements-made))))
    (message "Reference replacement complete. Replaced %d instance(s). Skipped %d (key not found), %d (AI error)."
             replacements-made not-found-count errors-occurred)
    ;; Clean up state variable
    (setq tlon-ai--extract-replace-state nil)))

;;;;; Change propagation

;;;;;; Change Propagation Command

;;;###autoload
(defun tlon-ai-propagate-changes ()
  "Propagate modifications from the latest commit of the current file.
Gets the diff of the latest commit affecting the current file in its repository.
Then, for each corresponding file in other \"uqbar\" content
repositories (originals and translations), asks the AI to apply the semantically
equivalent changes. Finally, commits the changes made by the AI in each target
repository."
  (interactive)
  (let* ((source-file (buffer-file-name))
         (_ (unless source-file (user-error "Current buffer is not visiting a file")))
         (source-repo (tlon-get-repo-from-file source-file 'no-prompt))
         (_ (unless source-repo (user-error "Could not determine repository for %s" source-file)))
         (source-repo-name (tlon-repo-lookup :name :dir source-repo)) ; Get the repo name
         (_ (unless source-repo-name (user-error "Could not determine repository name for %s" source-repo)))
         (source-lang (tlon-repo-lookup :language :dir source-repo))
         (latest-commit (tlon-latest-user-commit-in-file source-file))
         (_ (unless latest-commit (user-error "Could not find latest commit for %s" source-file)))
         (diff (tlon-ai--get-commit-diff latest-commit source-file source-repo))
         (all-content-repos (append (tlon-lookup-all tlon-repos :dir :subproject "uqbar" :subtype 'originals)
                                    (tlon-lookup-all tlon-repos :dir :subproject "uqbar" :subtype 'translations)))
         (target-repos (remove source-repo all-content-repos)))

    (unless diff
      (user-error "No changes found in commit %s for file %s. Aborting" latest-commit source-file))

    (unless target-repos
      (user-error "No target repositories found to propagate changes to"))

    (message "Found commit %s for %s in %s. Propagating changes..."
             (substring latest-commit 0 7) (file-name-nondirectory source-file) source-repo-name)
    (message "Diff:\n%s" diff) ; Log the diff for debugging/info

    (dolist (target-repo target-repos)
      (let* ((target-lang (tlon-repo-lookup :language :dir target-repo))
             (target-file (tlon-ai--find-target-file source-file source-repo target-repo)))
        (if target-file
            (let* ((target-content (with-temp-buffer
                                     (insert-file-contents target-file)
                                     (buffer-string)))
                   ;; Construct the prompt carefully
                   (prompt (format
                            (concat
                             "You are an expert code/text synchronizer.\n"
                             "The following diff shows changes made to a file in %s (source language), located at relative path '%s'.\n\n"
                             "--- DIFF START ---\n%s\n--- DIFF END ---\n\n"
                             "Your task is to apply the *semantic equivalent* of these changes to the corresponding file in %s (target language).\n"
                             "The *entire current content* of the target file is provided below.\n\n"
                             "--- TARGET FILE CONTENT START ---\n%s\n--- TARGET FILE CONTENT END ---\n\n"
                             "Please output *only* the complete, modified content of the target file. Do not add explanations, comments, apologies, or markdown formatting (like ```) around the output.")
                            source-lang
                            (file-relative-name source-file source-repo)
                            diff
                            target-lang
                            target-content)))
              (message "Requesting AI to update %s (lang: %s) in repo %s..."
                       (file-name-nondirectory target-file) target-lang (file-name-nondirectory target-repo))
              ;; Make the request - the callback handles writing and committing
              (tlon-make-gptel-request prompt nil
                                       (lambda (response info) ; Wrap callback to pass context
                                         (tlon-ai--propagate-changes-callback response info target-file target-repo source-repo-name latest-commit)) ; Pass source-repo-name
                                       nil ; Use default model for now, consider a custom one
                                       t)) ; Bypass context check as we manage context implicitly
          ;; Message is now handled inside tlon-ai--find-target-file
          )))
    (message "AI change propagation requests initiated for all target repositories.")))

;;;;;; Helper functions

(defun tlon-ai--get-commit-diff (commit file repo-path)
  "Get the diff for FILE at COMMIT hash within REPO-PATH.
Returns the diff string or nil if no changes or error."
  (when (and commit file repo-path)
    (let* ((default-directory repo-path) ; Ensure git runs in the correct repo
           (relative-file (file-relative-name file repo-path))
           (command (format "git show %s -- %s"
                            (shell-quote-argument commit)
                            (shell-quote-argument relative-file)))
           (diff (condition-case err
                     (shell-command-to-string command)
                   (error (message "Error getting diff for %s in %s: %s" relative-file commit err)
                          nil))))
      ;; Check if diff is empty or indicates no changes (git show includes commit info)
      (if (or (null diff)
              (string-blank-p diff)
              ;; Check if the actual diff section marker is present
              (not (string-match-p "diff --git" diff)))
          (progn
            (message "No effective changes found for %s in commit %s." relative-file commit)
            nil)
        ;; Return the full output, including commit info and diff
        diff))))

(defun tlon-ai--find-target-file (source-file source-repo target-repo)
  "Find the corresponding target file path in TARGET-REPO.
Based on SOURCE-FILE in SOURCE-REPO."
  (let* ((source-subtype (tlon-repo-lookup :subtype :dir source-repo))
         (target-subtype (tlon-repo-lookup :subtype :dir target-repo))
         (target-repo-name (or (tlon-repo-lookup :name :dir target-repo) ; Get repo name
                               (file-name-nondirectory target-repo))) ; Fallback
         (target-path nil))
    (cond
     ;; Case 1: Source is original, Target is translation
     ((and (eq source-subtype 'originals) (eq target-subtype 'translations))
      (let ((source-filename (file-name-nondirectory source-file)))
        ;; Look for a file in the target repo whose 'original_path' matches the source filename
        (setq target-path (tlon-metadata-lookup (tlon-metadata-in-repo target-repo)
                                                "file" ; The key to return (the full file path)
                                                "original_path" ; The key to match
                                                source-filename)))) ; The value to match

     ;; Case 2: Source is translation, Target is original
     ((and (eq source-subtype 'translations) (eq target-subtype 'originals))
      (if-let ((original-relative-path (tlon-yaml-get-key "original_path" source-file)))
          (let* ((source-bare-dir (tlon-get-bare-dir source-file))
                 (source-lang (tlon-repo-lookup :language :dir source-repo))
                 (target-lang "en") ; Originals are always 'en' in this setup
                 (original-bare-dir (tlon-get-bare-dir-translation target-lang source-lang source-bare-dir)))
            ;; Ensure target repo path ends with a slash before concatenating
            (setq target-path (file-name-concat (directory-file-name target-repo)
                                                original-bare-dir
                                                original-relative-path)))
        (message "Warning: Could not find 'original_path' in source file %s" source-file)))

     ;; Case 3: Source is translation, Target is another translation
     ((and (eq source-subtype 'translations) (eq target-subtype 'translations))
      (if-let ((original-relative-path (tlon-yaml-get-key "original_path" source-file)))
          ;; Find the translation in the target repo that points to the same original
          (setq target-path (tlon-metadata-lookup (tlon-metadata-in-repo target-repo)
                                                  "file"
                                                  "original_path"
                                                  original-relative-path))
        (message "Warning: Could not find 'original_path' in source file %s" source-file)))
     ;; Other cases (e.g., original to original) are not expected for propagation
     (t (message "Warning: Unsupported propagation from %s (%s) to %s (%s)."
		 (file-name-nondirectory source-repo) source-subtype
		 (file-name-nondirectory target-repo) target-subtype)))
    ;; Final check and warning generation
    (let ((warning-message nil))
      (cond
       ((null target-path)
        (setq warning-message
              (format "Target file for %s in %s could not be determined (metadata lookup failed)."
                      (file-name-nondirectory source-file)
                      target-repo-name))) ; Use repo name
       ((not (file-exists-p target-path))
	(setq warning-message
              (format "Target file path %s determined for %s in %s, but file does not exist."
                      target-path
                      (file-name-nondirectory source-file)
                      target-repo-name))))
      (if warning-message
	  (progn (message "Warning: %s Skipping." warning-message) nil)
	;; Only return target-path if it exists
	target-path))))

(defun tlon-ai--commit-in-repo (repo-path file-path message)
  "Stage FILE-PATH and commit it in REPO-PATH with MESSAGE."
  (let ((default-directory repo-path) ; Crucial for git commands
        (relative-file (file-relative-name file-path repo-path)))
    (message "Staging '%s' in repo '%s'" relative-file (file-name-nondirectory repo-path))
    (if (zerop (call-process "git" nil nil nil "add" relative-file))
        (progn
          (message "Committing '%s' with message: %s" relative-file message)
          (if (zerop (call-process "git" nil nil nil "commit" "-m" message))
              (let ((new-commit (string-trim (shell-command-to-string "git rev-parse HEAD"))))
                (message "Successfully committed %s in %s (commit: %s)"
                         relative-file (file-name-nondirectory repo-path) (substring new-commit 0 7))
                ;; Open Magit status showing the new commit
                (when (and (not noninteractive) (fboundp 'magit-show-commit))
                  (magit-show-commit new-commit)))
            (message "Error: Failed to commit %s in %s" relative-file (file-name-nondirectory repo-path))))
      (message "Error: Failed to stage %s in %s" relative-file (file-name-nondirectory repo-path)))))

(defun tlon-ai--propagate-changes-callback (response info target-file target-repo source-repo-name source-commit)
  "Callback for AI change propagation. Writes file and commits.
RESPONSE is the AI's response, INFO is the response info. TARGET-FILE is the
file to write the changes to, TARGET-REPO is the target repository,
SOURCE-REPO-NAME is the name of the source repository, and SOURCE-COMMIT is the
commit hash."
  (if (not response)
      (progn
        (message "AI failed to process changes for %s. Status: %s"
                 (file-name-nondirectory target-file) (plist-get info :status))
        (tlon-ai-callback-fail info)) ; Use existing fail message
    (progn
      (message "AI provided changes for %s. Applying..." (file-name-nondirectory target-file))
      ;; Overwrite the target file with the AI's response
      (condition-case err
          (with-temp-file target-file ; Overwrites atomically
            (insert response))
        (error (message "Error writing AI changes to %s: %s" target-file err)
               nil)) ; Prevent commit if write fails

      ;; If write succeeded, commit the changes
      (when (file-exists-p target-file) ; Double check write didn't fail silently
        (let ((commit-message (format "AI: Propagate changes from commit %s in %s"
                                      (substring source-commit 0 7) source-repo-name)))
          (tlon-ai--commit-in-repo target-repo target-file commit-message))))))

;;;;; Menu

(transient-define-infix tlon-ai-infix-toggle-overwrite-alt-text ()
  "Toggle the value of `tlon-ai-overwrite-alt-text' in `ai' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-ai-overwrite-alt-text
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-ai-overwrite-alt-text)))

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

;;;;;; Abstract overwrite

(transient-define-infix tlon-abstract-overwrite-infix ()
  "Change the local value of the `tlon-abstract-overwrite' variable."
  :class 'transient-lisp-variable
  :variable 'tlon-abstract-overwrite
  :reader 'tlon-abstract-overwrite-reader
  :prompt "Overwrite when the entry already contains an abstract? ")

(defun tlon-abstract-overwrite-reader (prompt _ _)
  "Return a list of choices with PROMPT to be used as an `infix' reader function."
  (tlon-transient-read-symbol-choice prompt '(always ask never)))

;;;;;; Mullvad

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

;;;;;; Model selection

(defclass tlon-ai-model-selection-infix (transient-infix)
  ((variable :initarg :variable)
   (choices  :initarg :choices)
   (default-label :initarg :default-label :initform "Default model"))
  "A transient infix for selecting AI models or using the default.")

(cl-defmethod transient-init-value ((obj tlon-ai-model-selection-infix))
  "Initialize OBJ's value slot."
  (oset obj value (symbol-value (oref obj variable))))

(cl-defmethod transient-infix-read ((obj tlon-ai-model-selection-infix))
  "Read a new value for OBJ's variable."
  (let* ((choices
          (append
           '(("Default model" . nil))
           (cl-loop for (backend-name . backend) in gptel--known-backends
                    append (cl-loop for model in (gptel-backend-models backend)
                                    collect (cons (format "%s: %s"
                                                          backend-name
                                                          (gptel--model-name model))
                                                  (cons backend-name model))))))
         (choice (completing-read
                  (format "Select model (current: %s): "
                          (if (symbol-value (oref obj variable))
                              (format "%s: %s"
                                      (car (symbol-value (oref obj variable)))
                                      (cdr (symbol-value (oref obj variable))))
                            (oref obj default-label)))
                  choices nil t)))
    (cdr (assoc choice choices))))

(cl-defmethod transient-infix-set ((obj tlon-ai-model-selection-infix) value)
  "Set the value of OBJ's variable to VALUE."
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj tlon-ai-model-selection-infix))
  "Format OBJ's value for display."
  (let ((value (symbol-value (oref obj variable))))
    (propertize (if value
                    (format "%s: %s" (car value) (cdr value))
                  (oref obj default-label))
                'face 'transient-value)))

(transient-define-infix tlon-ai-infix-select-summarization-model ()
  "AI model to use for summarizing.
If nil, use the default model."
  :class 'tlon-ai-model-selection-infix
  :variable 'tlon-ai-summarization-model)

(transient-define-infix tlon-ai-infix-select-markdown-fix-model ()
  "AI model to use for fixing the markdown.
If nil, use the default model."
  :class 'tlon-ai-model-selection-infix
  :variable 'tlon-ai-markdown-fix-model)

(transient-define-infix tlon-ai-infix-select-create-reference-article-model ()
  "AI model to use for creating a reference article.
If nil, use the default model."
  :class 'tlon-ai-model-selection-infix
  :variable 'tlon-ai-create-reference-article-model)

(transient-define-infix tlon-ai-infix-select-proofread-reference-article-model ()
  "AI model to use for proofreading a reference article.
If nil, use the default model."
  :class 'tlon-ai-model-selection-infix
  :variable 'tlon-ai-proofread-reference-article-model)

(transient-define-infix tlon-ai-infix-select-help-model ()
  "AI model to use for asking for help.
If nil, use the default model."
  :class 'tlon-ai-model-selection-infix
  :variable 'tlon-ai-help-model)
 
;;;;;; Main menu

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
    ("s S" "shorten abstract with AI"                 tlon-shorten-abstract-with-ai)
    ""
    "Summarize options"
    ("s -b" "batch"                                   tlon-ai-batch-fun-infix)
    ("s -m" "mullvad connection duration"             tlon-mullvad-connection-duration-infix)
    ("s -o" "overwrite abstract"                      tlon-abstract-overwrite-infix)
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
    ("t" "translate"                                  tlon-ai-translate-math)
    ""
    "Reference articles"
    ("w w" "create reference article"                 tlon-ai-create-reference-article)
    ("w p" "proofread reference article"              tlon-ai-proofread-reference-article)
    ""
    "Propagation"
    ("f" "fix Markdown format"                        tlon-ai-fix-markdown-format)
    ("p" "Propagate latest commit changes"            tlon-ai-propagate-changes)
    ""
    "Bibliography"
    ("x" "Extract references from buffer/region" tlon-ai-extract-references)
    ("k" "Get BibKeys for references (region - line based)"   tlon-ai-get-bibkeys-from-references)
    ("X" "Extract & Replace References (buffer/region - precise)" tlon-ai-extract-and-replace-references)]
   ["Help"
    ("a a" "Ask for help"                               tlon-ai-ask-for-help)]
   ["Misc"
    ("b" "set language of bibtex"                     tlon-ai-set-language-bibtex)
    ("e" "fix encoding"                               tlon-ai-fix-encoding-in-string)
    ("h" "phonetically transcribe"                    tlon-ai-phonetically-transcribe)
    ("r" "rewrite"                                    tlon-ai-rewrite)
    ("l" "translate"                                  tlon-ai-translate)
    ;; Create command to translate all images
    ;; TODO: develop this
    ;; ("M" "translate all math"                      tlon-ai-translate-math-in-buffer)
    ""
    "General options"
    ("-e" "edit prompt"                               tlon-ai-infix-toggle-edit-prompt)
    ("-d" "debug"                                     tlon-menu-infix-toggle-debug)
    ""
    "Models"
    ("m -f" "Markdown fix" tlon-ai-infix-select-markdown-fix-model)
    ("m -s" "Summarization" tlon-ai-infix-select-summarization-model)
    ("w -w" "Create reference article" tlon-ai-infix-select-create-reference-article-model)
    ("w -p" "Proofread reference article" tlon-ai-infix-select-proofread-reference-article-model)
    ("a -a" "Help model" tlon-ai-infix-select-help-model)]])

(provide 'tlon-ai)
;;; tlon-ai.el ends here

;; Local Variables:
;; jinx-languages: "es en it fr de"
;; End:
