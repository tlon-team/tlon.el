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

;;;; User options

(defgroup tlon-ai nil
  "AI functionality for Tlön."
  :group 'tlon)

(defcustom tlon-ai-batch-fun nil
  "Function to run in batch mode."
  :type 'symbol
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
itself. See `gptel-extras-ai-models' for available options. If nil, do not
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

(defcustom tlon-ai-summarize-commit-diffs-model
  '("Gemini" . gemini-2.5-pro-preview-06-05)
  "Model to use for summarizing commit diffs.
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, do not
use a different model for summarizing commit diffs."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-ai)

(defcustom tlon-ai-help-model
  '("Gemini" . gemini-2.0-flash-thinking-exp-01-21)
  "Model to use for the AI help command (`tlon-ai-ask-for-help').
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, use the
default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-ai)

(defcustom tlon-ai-translation-model nil
  "Model to use for AI-powered translation when using the 'ai' engine.
The value is a cons cell whose car is the BACKEND name (string) and whose cdr is
the MODEL symbol. See `gptel-extras-ai-models' for available options. If nil,
use the default `gptel-model'."
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

(defconst tlon-ai-changelog-file
  (file-name-concat paths-dir-dotemacs "extras/gptel-extras-changelog-template.org")
  "The file with the changelog template for `tlon-ai-summarize-commit-diffs'.")

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

(defconst tlon-ai-write-reference-article-prompt
  `((:prompt "You are an encyclopedia writer, and are currently writing a series of articles for an encyclopedia of effective altruism. Please write an entry on the topic of ‘%1$s’.\n\nYou should write the article *primarily* based on the text files attached, though you may also rely on your general knowledge of the topic. Each of these articles discusses the topic of the entry. So you should inspect each of these files closely and make an effort to understand what they claim thoroughly. Then, once you have inspected and understood the contents of all of these files, make a synthesis of the topic (%1$s) and write the article based on this synthesis.\n\nWrite the article in a sober, objective tone, avoiding cliches, excessive praise and unnecessary flourishes. In other words, draft it as if you were writing an article for a reputable encyclopedia, such as the Encyclopaedia Britannica (but remember that this is not a general encyclopedia, but specifically an encyclopedia of effective altruism, so it should be written from that perspective).\n\nWhen you make a claim traceable to a specific source, please cite this source in a footnote. The first line of each file included indicates how that work should be cited. If you would like to cite other works besides those in the files I'm including, please use the format ‘Author(s), year, title’. In general, try not to have more than one footnote per paragraph, though you may include multiple references in a given footnote. Do not include a references section at the end. Use Markdown syntax for composing the article."
	     :language "en")
    (:prompt "Eres un escritor de enciclopedias y estás escribiendo una serie de artículos para una enciclopedia sobre el altruismo eficaz. Por favor, escribe una entrada sobre el tema ‘%1$s’.\n\nDebes escribir el artículo *principalmente* basándote en los archivos de texto adjuntos, aunque también puedes tener en cuenta tu conocimiento general del tema. Cada uno de estos artículos trata el tema de la entrada. Por lo tanto, debes examinar detenidamente cada uno de estos archivos y esforzarte por comprender a fondo lo que sostiene. Luego, una vez que hayas inspeccionado y comprendido el contenido de todos estos archivos, haz una síntesis del tema (%1$s) y escribe el artículo basándote en esta síntesis.\n\nAdjunto también un glosario sobre terminología relacionada con el altruismo eficaz. Procura utilizar estos términos para vertir al castellano expresiones peculiares de ese movimiento.\n\nEscribe el artículo en un tono sobrio y objetivo, evitando clichés, elogios excesivos y florituras innecesarias. En otras palabras, redáctalo como si estuvieras escribiendo un artículo para una enciclopedia de prestigio, como la Encyclopaedia Britannica (pero recuerda que no se trata de una enciclopedia general, sino específicamente de una enciclopedia aobre el altruismo eficaz, por lo que debe redactarse desde esa perspectiva).\n\nCuando hagas una afirmación que pueda atribuirse a una fuente específica, menciona dicha fuente en una nota al pie. Indiqa al menos el autor o autores, el título y el año de publicación en todas tus citas. En general, procure que no haya más de una nota a pie de página por párrafo, aunque puede incluir varias referencias en una misma nota. No incluyas una sección de referencias al final. Utiliza sintaxis de Markdown para redactar el artículo."
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
(defconst tlon-ai-fix-markdown-formatting-prompt
  "Please take a look at the two paragraphs attached (paragraphs may contain only one word). The first, ‘%s’, is taken from the original document in %s, while the second, ‘%s’, is taken from a translation of that document into %s. In the translation, some of the original formatting (which includes not only Markdown elements but potentially HTML components and SSML tags) has been lost or altered. What I want you to do is to generate a new paragraph with all the original formatting restored, but without changing the text of the translation. Add missing links. Add missing asterisks. Add any other missing markdown signs. Don't add any missing text. Don't change the name of the files referred to as images. And please do not surround the text in backticks. Add missing links. Add missing asterisks. Add any other missing markdown signs. Just give the output but don't add comments or clarifications, even if there's nothing to restore. Thank you!")

;;;;; Summarization

;;;;;; Abstracts

(defconst tlon-ai-how-to-write-abstract-prompt
  `((:prompt ,(format "Write the abstract in a sober, objective tone, avoiding cliches, excessive praise and unnecessary flourishes. In other words, draft it as if you were writing the abstract of a scientific paper. The abstract should be only one paragraph long and have a rough length of 100 to 250 words (feel free to exceed it if you really need to, but never go over %s words). It should not mention bibliographic data of the work (such as title or author). Write the abstract directly stating what the article argues, rather than using phrases such as 'The article argues that...'. For example, instead of writing 'The article ‘The eradication of smallpox’ by William D. Tierney tells that mankind fought smallpox for centuries...', write 'Mankind fought smallpox for centuries...'. Also, please omit any disclaimers of the form 'As an AI language model, I'm unable to browse the internet in real-time.' Finally, end your abstract with the phrase ' – AI-generated abstract.'" tlon-max-abstract-length)
	     :language "en")
    (:prompt ,(format "Redacta el resumen en un tono sobrio y objetivo, evitando los lugares comunes, los elogios excesivos y las florituras innecesarias. En otras palabras, redáctalo como si estuvieras escribiendo el resumen de un artículo científico. El resumen debe constar de un solo párrafo y tener una extensión de unas 100 a 250 palabras (puedes exceder este umbral de ser necesario, pero el resumen no debe tener en ningún caso más de %s palabras). No debe mencionar datos bibliográficos de la obra (como el título o el autor). Escribe el resumen indicando directamente lo que argumenta el artículo, en lugar de utilizar frases como ‘El artículo argumenta que...’. Por ejemplo, en lugar de escribir ‘El artículo 'La erradicación de la viruela' de William D. Tierney sostiene que la humanidad luchó contra la viruela durante siglos...’, escribe ‘La humanidad luchó contra la viruela durante siglos...’. Además, omite cualquier descargo de responsabilidad del tipo ‘Como modelo de lenguaje de inteligencia artificial, no puedo navegar por Internet en tiempo real.’ Por último, termina tu resumen con la frase ‘ - Resumen generado por inteligencia artificial.’" tlon-max-abstract-length)
	     :language "es")
    (:prompt ,(format "Rédigez le résumé sur un ton sobre et objectif, en évitant les clichés, les éloges excessifs et les fioritures inutiles. En d'autres termes, rédigez-le comme si vous écriviez le résumé d'un article scientifique. Le résumé ne doit comporter qu'un seul paragraphe et avoir une longueur approximative de 100 à 250 mots (n'hésitez pas à le dépasser si vous en avez vraiment besoin, mais ne dépassez jamais %s mots). Il ne doit pas mentionner les données bibliographiques de l'ouvrage (telles que le titre ou l'auteur). Rédigez le résumé en indiquant directement ce que l'article soutient, plutôt qu'en utilisant des phrases telles que 'L'article soutient que...'. Par exemple, au lieu d'écrire 'L'article 'L'éradication de la variole' de William D. Tierney affirme que l'humanité a combattu la variole pendant des siècles...', écrivez 'L'humanité a combattu la variole pendant des siècles...'. Veuillez également omettre toute clause de non-responsabilité du type 'En tant que modèle linguistique de l'IA, je ne suis pas en mesure de naviguer sur l'internet en temps réel'. Enfin, terminez votre résumé par la phrase ' - Résumé généré par l'IA.'" tlon-max-abstract-length)
	     :language "fr")
    (:prompt ,(format "Scrivete l'abstract con un tono sobrio e oggettivo, evitando i cliché, le lodi eccessive e i fronzoli inutili. In altre parole, scrivetelo come se steste scrivendo l'abstract di un articolo scientifico. L'abstract dovrebbe essere lungo solo un paragrafo e avere una lunghezza approssimativa di 100-250 parole (sentitevi liberi di superarlo se ne avete davvero bisogno, ma non superate mai le %s di parole). Non deve riportare i dati bibliografici del lavoro (come il titolo o l'autore). Scrivete l'abstract indicando direttamente ciò che l'articolo sostiene, piuttosto che usare frasi come 'L'articolo sostiene che...'. Ad esempio, invece di scrivere 'L'articolo 'L'eradicazione del vaiolo' di William D. Tierney afferma che l'umanità ha combattuto il vaiolo per secoli...', scrivete 'L'umanità ha combattuto il vaiolo per secoli...'. Inoltre, omettete qualsiasi dichiarazione di non responsabilità del tipo 'Come modello linguistico dell'IA, non sono in grado di navigare in Internet in tempo reale'. Infine, terminate il vostro riassunto con la frase ' - riassunto generato dall'IA'." tlon-max-abstract-length)
	     :language "it")
    (:prompt ,(format "Schreiben Sie die Zusammenfassung in einem nüchternen, sachlichen Ton und vermeiden Sie Klischees, übermäßiges Lob und unnötige Schnörkel. Mit anderen Worten: Verfassen Sie sie so, als ob Sie die Zusammenfassung einer wissenschaftlichen Arbeit schreiben würden. Die Zusammenfassung sollte nur einen Absatz lang sein und eine ungefähre Länge von 100 bis 250 Wörtern haben (Sie können diese Zahl ruhig überschreiten, wenn es wirklich nötig ist, aber nie mehr als %s Wörter). Sie sollte keine bibliografischen Daten der Arbeit (wie Titel oder Autor) enthalten. Geben Sie in der Zusammenfassung direkt an, worum es in dem Artikel geht, und verwenden Sie keine Sätze wie 'In dem Artikel wird argumentiert, dass...'. Schreiben Sie zum Beispiel statt 'Der Artikel 'Die Ausrottung der Pocken' von William D. Tierney besagt, dass die Menschheit jahrhundertelang die Pocken bekämpfte...' lieber 'Die Menschheit bekämpfte die Pocken jahrhundertelang...'. Lassen Sie bitte auch Haftungsausschlüsse der Form 'Als KI-Sprachmodell bin ich nicht in der Lage, das Internet in Echtzeit zu durchsuchen' weg. Beenden Sie Ihre Zusammenfassung schließlich mit dem Satz ' - KI-generierte Zusammenfassung.'" tlon-max-abstract-length)
	     :language "de")))

(defconst tlon-ai-get-abstract-prompts
  `((:prompt ,(format "Please create an abstract of the following work%s%s"
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-abstract-prompt
				   :prompt :language "en"))
	     :language "en")
    (:prompt ,(format "Por favor, crea un resumen de la presente obra%s%s"
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-abstract-prompt
				   :prompt :language "es"))
	     :language "es")
    (:prompt ,(format "Veuillez créer un résumé de ce travail%s%s"
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-abstract-prompt
				   :prompt :language "fr"))
	     :language "fr")
    (:prompt ,(format "Si prega di creare un riassunto del seguente lavoro%s%s"
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-abstract-prompt
				   :prompt :language "it"))
	     :language "it")
    (:prompt ,(format "Bitte erstellen Sie eine Zusammenfassung der folgenden Arbeit%s%s"
		      tlon-ai-string-wrapper
		      (tlon-lookup tlon-ai-how-to-write-abstract-prompt
				   :prompt :language "de"))
	     :language "de"))
  "Prompts for summarization.")

(defconst tlon-ai-shorten-abstract-prompts
  `((:prompt ,(format "Please shorten the following abstract to %s words or less. The shortened version should consist of only one paragraph.%s"
		      tlon-max-abstract-length
		      tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Por favor, acorta el siguiente resumen a %s palabras o menos. La versión acortada debe constar de un solo párrafo.%s"
		      tlon-max-abstract-length
		      tlon-ai-string-wrapper)
	     :language "es")
    (:prompt ,(format "Veuillez raccourcir le résumé suivant à %s mots ou moins. La version raccourcie doit se composer d'un seul paragraphe.%s"
		      tlon-max-abstract-length
		      tlon-ai-string-wrapper)
	     :language "fr")
    (:prompt ,(format "Si prega di abbreviare il seguente abstract a %s parole o meno. La versione abbreviata deve essere composta da un solo paragrafo.%s"
		      tlon-max-abstract-length
		      tlon-ai-string-wrapper)
	     :language "it")
    (:prompt ,(format "Bitte kürzen Sie die folgende Zusammenfassung auf %s Wörter oder weniger. Die gekürzte Version sollte nur aus einem Absatz bestehen.%s"
		      tlon-max-abstract-length
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

;;;;; Help

(defconst tlon-ai-help-prompt-template
  (format "Here is the documentation for all processes and programs within the Tlön organization. They range from project readme files to docs for all the Emacs functionality used by this organization (including extensions for common Emacs packages or features ('extras'), modules that provide new functionality within the comprehensive 'tlon' package, and a detailed configuration file ('config.org'). An employee has asked a question that may relate to any of these processes, and I want you to answer it to the best of your ability. Please format your answer using %S syntax. Here is the question:\n\n%%s" gptel-default-mode)
  "Template for AI help prompt using documentation files as context.")

;;;;; Phonetic transcription

(defconst tlon-ai-transcribe-phonetically-prompt
  `((:prompt ,(format "Please transcribe the following text phonetically, i.e. using the International Phonetic Alphabet (IPA).%sJust return the phonetic transcription, without any commentary. Do not enclose the transcription in slashes." tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Por favor, transcribe fonéticamente el siguiente texto, es decir, utilizando el Alfabeto Fonético Internacional (AFI).%sLimítate a devolver la transcripción fonética, sin comentarios de ningún tipo. No encierres la transcripción entre barras." tlon-ai-string-wrapper)
	     :language "es")
    (:prompt ,(format "Per favore, trascrivi foneticamente il seguente testo, cioè utilizzando l'Alfabeto Fonetico Internazionale (AFI).%sRestituisci solo la trascrizione fonetica, senza alcun commento. Non racchiudere la trascrizione tra barre." tlon-ai-string-wrapper)
	     :language "it")
    (:prompt ,(format "Veuillez transcrire phonétiquement le texte suivant, c'est-à-dire en utilisant l'Alphabet Phonétique International (API).%sRetournez seulement la transcription phonétique, sans aucun commentaire. N'encadrez pas la transcription entre barres obliques." tlon-ai-string-wrapper)
	     :language "fr")
    (:prompt ,(format "Por favor, transcreva foneticamente o seguinte texto, ou seja, usando o Alfabeto Fonético Internacional (AFI).%sApenas retorne a transcrição fonética, sem qualquer comentário. Não coloque a transcrição entre barras." tlon-ai-string-wrapper)
	     :language "pt")
    (:prompt ,(format "Bitte transkribieren Sie den folgenden Text phonetisch, das heißt mit dem Internationalen Phonetischen Alphabet (IPA).%sGeben Sie nur die phonetische Transkription zurück, ohne jegliche Kommentare. Schließen Sie die Transkription nicht in Schrägstriche ein." tlon-ai-string-wrapper)
	     :language "de")))

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

;;;;; Meta Description

(defconst tlon-ai-create-meta-description-prompt
  `((:prompt ,(format "Your task is to generate a short text optimized for the 'meta' attribute of the HTML 'meta' element based on the content of the informative article included below: %s. The text should consist of a single sentence that does not exceed 160 characters, adequately summarizes the main topic of the article, and naturally incorporates the main topic/keywords. Return *only* the text of the meta description itself, without introductory phrases, comments, or Markdown formatting." tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Tu tarea es generar un texto breve optimizado para el atributo \"meta\" del elemento HTML \"meta\" basado en el contenido del artículo informativo que se incluye a continuación: %s. El texto debe consistir en una sola oración no supere los 160 caracteres, resuma adecuadamente el tema principal del artículo, e incorpore de forma natural el tema principal/palabras clave. Devuelve *sólo* el texto de la meta descripción en sí, sin frases introductorias, comentarios o formato Markdown." tlon-ai-string-wrapper)
	     :language "es"))
  "Prompts for creating a meta description for an article.")

;;;;; Title Generation

(defconst tlon-ai-create-title-prompt
  `((:prompt ,(format "Your task is to generate an SEO-optimized title for the article content provided below: %s. Follow these rules:\n\n1. Length: Keep the title around 50-60 characters to avoid truncation in search results. If longer, ensure the first 55 characters convey the core message.\n2. Primary keyword first: Start with the main keyword or topic, naturally integrated.\n3. Clarity: Make the title clearly describe the content. Use words that reflect the main idea.\n4. Uniqueness: Create a unique title that won't duplicate existing ones.\n5. Engagement: Use numbers for lists, questions for Q&A content (\"How...?\" or \"Why...?\"), compelling adjectives (\"complete guide\", \"effective strategies\"), or action verbs (\"Learn to...\", \"Discover...\").\n6. Natural language: Write like a human headline, not keyword stuffing.\n7. Tone: Match an informative, respectful tone. Avoid sensational language unless warranted.\n\nReturn *only* the title itself, without quotes, introductory phrases, or formatting." tlon-ai-string-wrapper)
	     :language "en")
    (:prompt ,(format "Tu tarea es generar un título optimizado para SEO basado en el contenido del artículo que se proporciona a continuación: %s. Sigue estas reglas:\n\n1. Longitud: Mantén el título alrededor de 50-60 caracteres para evitar truncamiento en los resultados de búsqueda. Si es más largo, asegúrate de que los primeros 55 caracteres transmitan el mensaje principal.\n2. Palabra clave principal primero: Comienza con la palabra clave o tema principal, integrado de forma natural.\n3. Claridad: Haz que el título describa claramente el contenido. Usa palabras que reflejen la idea principal.\n4. Unicidad: Crea un título único que no duplique otros existentes.\n5. Engagement: Usa números para listas, preguntas para contenido de Q&A (\"¿Cómo...?\" o \"¿Por qué...?\"), adjetivos atractivos (\"guía completa\", \"estrategias efectivas\"), o verbos de acción (\"Aprende a...\", \"Descubre...\").\n6. Lenguaje natural: Escribe como un titular humano, no relleno de palabras clave.\n7. Tono: Coincide con un tono informativo y respetuoso. Evita el lenguaje sensacionalista a menos que esté justificado.\n8. Español correcto: Asegúrate de usar ortografía, gramática y puntuación correctas en español, incluyendo todas las tildes y signos de puntuación invertidos necesarios.\n\nDevuelve *solo* el título en sí, sin comillas, frases introductorias o formato." tlon-ai-string-wrapper)
	     :language "es"))
  "Prompts for creating an SEO-optimized title for an article.")

;;;;; Change Propagation

(defconst tlon-ai-propagate-changes-prompt
  "You are an expert code/text synchronizer.\n The following diff shows changes made to a file in %s (source language), located at relative path '%s'.\n\n --- DIFF START ---\n%s\n--- DIFF END ---\n\n Your task is to apply the *semantic equivalent* of these changes to the corresponding file in %s (target language).\n The *entire current content* of the target file is provided below.\n\n --- TARGET FILE CONTENT START ---\n%s\n--- TARGET FILE CONTENT END ---\n\n Please output *only* the complete, modified content of the target file. Do not add explanations, comments, apologies, or markdown formatting (like ```) around the output.\n\nNote the following special instructions:\n\n- In French, we use double angle quotation marks (‘«’ and ‘»’) instead of the normal quotation marks (\"\") we use in all other languages. So if you see e.g. ‘«’ in a French file, you should not propagate this to the counterparts in other languages.\n\n- Similarly, in French, but not in other languages, we use a narrow no-break space (‘ ’) before punctuation marks such as colons (‘:’), semicolons (‘;’), question marks (‘?’), exclamation marks (‘!’), and double angle quotation marks (‘«’ and ‘»’). So if you see a non-break space preceding one of these characters in a French file, you should not propagate it to the counterparts in other languages."
  "Prompt for propagating changes across files in different languages.")


;;;;; Commit Summarization

(defconst tlon-ai-summarize-commit-diffs-system-prompt
  "You are a software developer's assistant focused on git commit analysis. \
Be concise but thorough when analyzing changes. Group related changes together if \
you notice patterns. If commit messages are included, use them to inform your analysis."
  "System prompt for summarizing commit diffs.")

(defconst tlon-ai-summarize-commit-diffs-prompt
  "Here are several git commit diffs:\n\n%s\n\nPlease analyze these commits and
provide a concise summary of the main changes. Include any significant patterns \
you notice. Write the summary using org-mode syntax, NOT Markdown. When writing the summary, \
focus on making it useful for someone who is already familiar with the code and \
wants to learn about the changes made in these commits, so that they can quickly \
determine if they need to handle any breaking changes or if they want to start \
using any of the new functionality. Organize the summary into sections, one for \
each package or feature, following this model:\n\n%s"
  "Prompt for summarizing commit diffs.")

;;;; Functions

;;;;; General

(defun tlon-make-gptel-request
    (prompt &optional printf-arg callback full-model
            skip-context-check request-buffer tools context-data mcp-servers)
  "Send PROMPT through gptel, pinning BACKEND+MODEL for tool follow-ups.
FULL-MODEL is a cons cell of the form (BACKEND . MODEL), where BACKEND is a
string (e.g. \"Gemini\") or a backend object, and MODEL is a symbol or string;
if nil, use the buffer/global `gptel-backend' and `gptel-model'.

TOOLS is a list of tool names (symbols/strings) that `gptel-get-tool' can
resolve.

MCP-SERVERS is a list of MCP *server names* (strings).  Each server is
passed to `gptel-mcp-connect' before the request is issued; this
automatically starts the server (via mcp-hub) and connects it if it
wasn’t already active.

PRINTF-ARG, if non-nil, is formatted into PROMPT via (format PROMPT PRINTF-ARG).
CALLBACK and CONTEXT-DATA are passed through to `gptel-request'. REQUEST-BUFFER,
if non-nil, is the buffer used for the conversation.

This function ensures that any *follow-up* tool calls performed by gptel reuse
the same BACKEND+MODEL by setting them buffer-locally before dispatch."
  (unless (or tlon-ai-batch-fun skip-context-check)
    (when (fboundp 'gptel-extras-warn-when-context)
      (gptel-extras-warn-when-context)))
  (let* ((buf (or request-buffer (current-buffer)))
         ;; resolve backend/model pair
         (pair (tlon--resolve-backend+model
                (or full-model
                    (cons (gptel-backend-name gptel-backend) gptel-model))))
         (backend-obj (car pair))
         (model-sym   (cdr pair))
         ;; ensure specified MCP servers are running and connected
         (_ (tlon--ensure-mcp-servers mcp-servers))
         ;; normalize tools to structs if provided (after servers are ready)
         (tool-structs (when tools
                         (mapcar #'gptel-get-tool tools)))
         (full-prompt (if printf-arg (format prompt printf-arg) prompt)))
    (with-current-buffer buf
      ;; Pin scope for initial request *and* tool follow-ups.
      (setq-local gptel-backend backend-obj
		  gptel-model model-sym
		  gptel-tools tool-structs
		  gptel-use-tools (and gptel-tools t)
		  gptel-include-reasoning nil)
      ;; Dispatch the request; gptel will consult buffer-local vars later, too.
      (gptel-request full-prompt
	:callback   callback
	:buffer     buf
	:context    context-data
	:transforms gptel-prompt-transform-functions))))

;; Helper: normalize (backend . model), allow strings, symbols, or objects
(defun tlon--resolve-backend+model (full-model)
  "Return a cons (BACKEND-OBJ . MODEL-SYM) from FULL-M0DEL.
FULL-MODEL may be:
  - a cons (BACKEND . MODEL)
  - BACKEND name/object, using current `gptel-model'
If BACKEND is a string, it is resolved against `gptel--known-backends'.
MODEL may be a string or symbol; we return a symbol for consistency."
  (pcase full-model
    (`(,backend . ,model)
     (let* ((backend-obj (if (stringp backend)
                             (alist-get backend gptel--known-backends nil nil #'string=)
                           backend))
            (model-sym   (if (symbolp model) model (intern (format "%s" model)))))
       (cons backend-obj model-sym)))
    (_
     (let* ((backend (or full-model (gptel-backend-name gptel-backend)))
            (backend-obj (if (stringp backend)
                             (alist-get backend gptel--known-backends nil nil #'string=)
                           backend))
            (model-sym   (if (symbolp gptel-model) gptel-model (intern (format "%s" gptel-model)))))
       (cons backend-obj model-sym)))))

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
    (message response)))

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

(defun tlon-ai--wrap-plain-text-callback (callback)
  "Wrap CALLBACK so it only handles plain-text responses or final results.
This helps avoid calling CALLBACK for non-text streaming chunks."
  (lambda (response info)
    (when (or (null response) (stringp response))
      (funcall callback response info))))

;;;;;; Other functions

(autoload 'bibtex-next-entry "bibtex")
(declare-function bibtex-extras-get-key "bibtex-extras")
(autoload 'ebib-extras-next-entry "ebib-extras")
(declare-function ebib-extras-get-field "ebib-extras")
(declare-function ebib--get-key-at-point "ebib")
(defun tlon-ai-batch-continue ()
  "Move to the next entry and call `tlon-ai-batch-fun'."
  (when tlon-ai-batch-fun
    (let ((next-key-info "N/A")) ; Default message part
      (pcase major-mode
	('bibtex-mode
	 (bibtex-next-entry)
	 (setq next-key-info (or (bibtex-extras-get-key) "N/A")))
	('ebib-entry-mode
	 (ebib-extras-next-entry)
	 (setq next-key-info (or (ebib--get-key-at-point) "N/A"))))
      ;; Always message, even if mode didn't match or key is nil
      (message "Moving point to next entry (key: %s)." next-key-info))
    ;; Always call the batch function if set
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

(autoload 'tlon-convert-pdf "tlon-import")
(defun tlon-get-file-as-string (file)
  "Get the contents of FILE as a string.
If FILE is a PDF, convert it to Markdown first. If FILE is HTML, render it to
text using `eww`."
  (let ((original-file-path file))
    (cond
     ((string= (file-name-extension original-file-path) "html")
      (let ((rendered-text nil))
	(save-selected-window
	  (eww-browse-url (concat "file://" (expand-file-name original-file-path)))
	  (setq rendered-text (buffer-substring-no-properties (point-min) (point-max)))
	  (kill-buffer (current-buffer)))
	rendered-text))
     ((string= (file-name-extension original-file-path) "pdf")
      (with-temp-buffer
	(let ((markdown-temp-file (make-temp-file "pdf-to-markdown-")))
	  (tlon-convert-pdf original-file-path markdown-temp-file)
	  (insert-file-contents markdown-temp-file)
	  (delete-file markdown-temp-file)
	  (buffer-substring-no-properties (point-min) (point-max)))))
     (t
      (with-temp-buffer
	(insert-file-contents original-file-path)
	(buffer-substring-no-properties (point-min) (point-max)))))))

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

;;;;;; Engine wrappers for tlon-translate

(declare-function gptel-context-add-file "gptel-context")
(declare-function gptel-context-remove-all "gptel-context")
(declare-function tlon-extract-glossary "tlon-glossary")

(defun tlon-ai--maybe-add-glossary-to-context (source-lang target-lang &optional request-buffer)
  "Attach a temporary glossary file for TARGET-LANG to the GPTel context.

SOURCE-LANG and TARGET-LANG are ISO 639-1 codes. When translating from English
to a non-English TARGET-LANG, copy/extract the glossary to a temporary file and
add that file to the GPTel context associated with REQUEST-BUFFER (or the
current buffer if nil). Return the temporary glossary file path, or nil if none
was added."
  (when (and (string= source-lang "en")
             (stringp target-lang)
             (not (string= target-lang "en")))
    (require 'tlon-glossary)
    (when-let ((orig (tlon-extract-glossary target-lang 'ai-revision)))
      (when (and (stringp orig) (file-exists-p orig))
        (let ((tmp (make-temp-file (format "tlon-glossary-%s-" target-lang) nil ".txt")))
          (copy-file orig tmp t)
          (with-current-buffer (or request-buffer (current-buffer))
            (gptel-context-add-file tmp))
          tmp)))))

(defun tlon-ai-request-wrapper (type &optional callback _no-glossary)
  "Dispatch AI-backed request of TYPE for translation.
TYPE is a symbol, currently only 'translate. CALLBACK is a gptel-style
callback that receives (RESPONSE INFO). _NO-GLOSSARY is ignored."
  (pcase type
    ('translate
     (let* ((src tlon-translate-source-language)
            (tgt tlon-translate-target-language)
            (text tlon-translate-text)
            (prompt (tlon-ai--build-translation-prompt src tgt))
            (req-buf (generate-new-buffer (format "*tlon-translate:%s->%s*" src tgt)))
            (glossary-temp (with-current-buffer req-buf
                             (tlon-ai--maybe-add-glossary-to-context src tgt req-buf)))
            (user-callback (or callback #'tlon-ai-callback-copy)))
       (when (fboundp 'gptel-extras-warn-when-context)
         (gptel-extras-warn-when-context))
       (tlon-make-gptel-request
        prompt text
        (lambda (response info)
          (unwind-protect
              (funcall (tlon-ai--wrap-plain-text-callback user-callback) response info)
            (when (buffer-live-p req-buf)
              (with-current-buffer req-buf
                (ignore-errors (gptel-context-remove-all))))
            (when (and glossary-temp (file-exists-p glossary-temp))
              (ignore-errors (delete-file glossary-temp)))
            (when (buffer-live-p req-buf)
              (kill-buffer req-buf))))
        tlon-ai-translation-model
        t
        req-buf)))
    (_ (user-error "Unsupported AI request type: %s" type))))

(defun tlon-ai-translate-text (text target-lang source-lang callback &optional _no-glossary)
  "Translate TEXT from SOURCE-LANG into TARGET-LANG and call CALLBACK.
TARGET-LANG and SOURCE-LANG are ISO 639-1 two-letter codes. CALLBACK is a
gptel-style function that receives (RESPONSE INFO). _NO-GLOSSARY is ignored."
  (let* ((prompt (tlon-ai--build-translation-prompt source-lang target-lang))
         (req-buf (generate-new-buffer (format "*tlon-translate:%s->%s*" source-lang target-lang)))
         (glossary-temp (with-current-buffer req-buf
                          (tlon-ai--maybe-add-glossary-to-context source-lang target-lang req-buf))))
    (when (fboundp 'gptel-extras-warn-when-context)
      (gptel-extras-warn-when-context))
    (tlon-make-gptel-request
     prompt text
     (lambda (response info)
       (unwind-protect
           (funcall (tlon-ai--wrap-plain-text-callback callback) response info)
         (when (buffer-live-p req-buf)
           (with-current-buffer req-buf
             (ignore-errors (gptel-context-remove-all))))
         (when (and glossary-temp (file-exists-p glossary-temp))
           (ignore-errors (delete-file glossary-temp)))
         (when (buffer-live-p req-buf)
           (kill-buffer req-buf))))
     tlon-ai-translation-model
     t
     req-buf)))

(defun tlon-ai--build-translation-prompt (source-lang target-lang)
  "Return an LLM prompt to translate from SOURCE-LANG to TARGET-LANG.
Both SOURCE-LANG and TARGET-LANG are ISO 639-1 codes."
  (format "Translate the following text from %s to %s. Return only the translated text, without any quotes, backticks, or Markdown code fences:%s"
          (tlon-lookup tlon-languages-properties :standard :code source-lang)
          (tlon-lookup tlon-languages-properties :standard :code target-lang)
          tlon-ai-string-wrapper))

;;;;; Writing

;;;;;; Reference article

(declare-function tlon-yaml-get-key "tlon-yaml")
(declare-function tlon-yaml-insert-field "tlon-yaml")
(defun tlon-ai-create-reference-article ()
  "Create a new reference article using AI."
  (interactive)
  (if-let ((title (tlon-yaml-get-key "title")))
      (let* ((lang (tlon-get-language-in-file nil 'error))
	     (prompt (format (tlon-lookup tlon-ai-write-reference-article-prompt
					  :prompt :language lang)
			     title)))
	(gptel-extras-warn-when-context)
	(tlon-add-add-sources-to-context)
	(tlon-add-glossary-to-context lang)
	(tlon-make-gptel-request prompt nil #'tlon-ai-create-reference-article-callback
				 tlon-ai-create-reference-article-model t)
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
(declare-function tlon-md-get-tag-pattern "tlon-md")
(defun tlon-ai-get-citations-in-section ()
  "Return a list of (KEY . CITE-TAG-STRING) pairs from \"Further reading\"."
  (let* ((lang (tlon-get-language-in-file nil 'error))
	 (section (tlon-md-get-tag-section "Further reading" lang))
	 (citations '()))
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(when (re-search-forward (format "^#\\{1,\\} %s" section) nil t)
	  (markdown-narrow-to-subtree)
	  (goto-char (point-min)) ; Start search within the narrowed section
	  (while (re-search-forward (tlon-md-get-tag-pattern "Cite") nil t)
	    (let ((key (match-string-no-properties 3)) ; Group 3 is bibKey value
		  (tag-string (match-string-no-properties 0))) ; Full tag match
	      (push (cons key tag-string) citations))))))
    (nreverse citations))) ; Return in order of appearance

(declare-function bibtex-extras-get-entry-as-string "bibtex-extras")
(defun tlon-ai-add-source-to-context (key cite-tag-string)
  "Add the relevant PDF content associated with KEY to the context.
If CITE-TAG-STRING (the full <Cite .../> tag from the buffer) contains a
`locator' attribute, prompt the user for the path to the PDF containing only
that section. Otherwise, add the full PDF associated with KEY from the BibTeX
entry."
  (let (pdf-path locator)
    ;; Check if the cite tag has a locator
    (when (string-match (tlon-md-get-tag-pattern "Cite") cite-tag-string)
      (setq locator (match-string-no-properties 5 cite-tag-string))) ; Group 5 is locator value

    (if (and locator (not (string-empty-p locator)))
	;; Locator found, prompt user for specific file
	(setq pdf-path (read-file-name (format "Select PDF for %s (%s): " key locator)))
      ;; No locator, get the full PDF from BibTeX entry
      (if-let ((field (bibtex-extras-get-entry-as-string key "file")))
	  (let* ((files (split-string field ";"))
		 (pdf-files (seq-filter (lambda (file)
					  (string-match-p "\\.pdf$" file))
					files)))
	    (tlon-ai-ensure-one-file key pdf-files)
	    (setq pdf-path (car pdf-files)))
	(user-error "No `file' field found in entry %s" key)))

    ;; Proceed with the determined pdf-path (either full or section)
    (when pdf-path
      ;; we convert to text because some AI models limit the number or pages
      ;; of PDF files
      (let ((text (tlon-get-string-dwim pdf-path))
	    (file (make-temp-file "pdf-to-text-")))
	(with-temp-buffer
	  (insert (format "Please cite this work as ‘%s’\n\n" cite-tag-string)) ; Use full tag string
	  (insert text)
	  (write-region (point-min) (point-max) file))
	(gptel-context-add-file file)))))

(defun tlon-add-add-sources-to-context ()
  "Add relevant PDF content for each citation in the buffer to the context.
Checks for `locator' attributes in <Cite> tags."
  (mapc (lambda (citation-pair)
	  (let ((key (car citation-pair))
		(tag-string (cdr citation-pair)))
	    (tlon-ai-add-source-to-context key tag-string)))
	(tlon-ai-get-citations-in-section))
  (message "Added relevant PDF content for citations in the buffer to the `gptel' context."))

(declare-function tlon-extract-glossary "tlon-glossary")
(declare-function tlon-glossary-target-path "tlon-glossary")
(defun tlon-add-glossary-to-context (lang)
  "Add the glossary of LANG to the context."
  (unless (string= lang "en")
    (require 'tlon-glossary)
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

;;;;; File comparison

;;;;;; Fix formatting

(autoload 'gptel-context-add-file "gptel-context")
(declare-function tlon-display-corresponding-paragraphs "tlon-paragraphs")
(declare-function tlon-get-corresponding-paragraphs "tlon-paragraphs")
;;;###autoload
(defun tlon-ai-fix-markdown-formatting (&optional file)
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
	 (prompt (tlon-ai-maybe-edit-prompt tlon-ai-fix-markdown-formatting-prompt))
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
			   (car pair)
			   (tlon-lookup tlon-languages-properties :standard :code original-lang)
			   (cdr pair)
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

(declare-function tlon-fetch-and-set-abstract "tlon-bib")
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

(autoload 'tlon-abstract-may-proceed-p "tlon-bib")
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
      (message "`%s' is scheduling `tlon-ai-batch-continue' via timer." "tlon-get-abstract-with-ai"))
    ;; Use a timer to avoid deep recursion in batch mode when skipping many items
    (run-with-idle-timer 0 nil #'tlon-ai-batch-continue)))

(declare-function bibtex-extras-get-field "bibtex-extras")
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
	(cond
	 (key
	  (pcase type
	    ('synopsis ; Synopses are just copied, not added to BibTeX entry
	     (kill-new response)
	     (message "Copied AI-generated synopsis to the kill ring:\n\n%s" response))
	    (_ ; Default is abstract, set it in the BibTeX entry
	     (when tlon-debug
	       (message "`tlon-get-abstract-callback' is setting abstract for key `%s' to `%s...'"
			key (when response (substring response 0 (min (length response) 100)))))
	     (tlon-ai-summarize-set-bibtex-abstract response key))))
	 ;; If no key, handle based on type (likely summarizing region/buffer)
	 (t
	  (pcase type
	    ('synopsis
	     (kill-new response)
	     (message "Copied AI-generated synopsis to the kill ring:\n\n%s" response))
	    (_ ; Default is abstract, just copy to kill ring
	     (kill-new response)
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
  (gptel-extras-warn-when-context)
  (let* ((question (read-string "What do you need help with? "))
	 (all-doc-files (tlon-ai-get-documentation-files))
	 (existing-doc-files (tlon-ai-add-existing-doc-files all-doc-files))
	 (full-prompt (format tlon-ai-help-prompt-template question)))
    (unless existing-doc-files
      (user-error "No documentation files found in standard Elpaca doc directories or none exist on disk"))
    (tlon-make-gptel-request full-prompt nil #'tlon-ai-ask-for-help-callback tlon-ai-help-model t)
    (message "Preparing your answer using %d documentation files with model %S..."
	     (length existing-doc-files) (cdr tlon-ai-help-model))))

(defun tlon-ai-add-existing-doc-files (doc-files)
  "Add existing DOC-FILES to GPTel context and return a list of files that exist."
  (message "Adding doc files to context...")
  (let ((existing-doc-files '()))
    (dolist (doc-file doc-files)
      (when (file-exists-p doc-file)
	(shut-up (gptel-context-add-file doc-file))
	(push doc-file existing-doc-files)))
    existing-doc-files))

(declare-function gptel-mode "gptel")
(declare-function gptel-extras-set-backend-and-model "gptel-extras")
(defvar gptel-default-mode)
(defun tlon-ai-ask-for-help-callback (response info)
  "Callback for `tlon-ai-ask-for-help'.
Displays the QUESTION and RESPONSE in a new `gptel-mode' buffer. If RESPONSE is
nil, use `tlon-ai-callback-fail'. INFO is the context information passed to the
request. The original user question is extracted from INFO."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let* ((question (tlon-ai--extract-question-from-info info))
	   (buffer-name (generate-new-buffer-name "*AI Help Answer*"))
	   (buffer (get-buffer-create buffer-name))
	   (model (cdr tlon-ai-help-model))
	   (backend (car tlon-ai-help-model)))
      (with-current-buffer buffer
	(erase-buffer)
	(insert (format "*** %s\n\n" question))
	(insert response)
	(funcall gptel-default-mode)
	(gptel-mode 1)
	(gptel-extras-set-backend-and-model backend model)
	(setq buffer-read-only nil)
	(when (eq gptel-default-mode 'org-mode)
	  (org-fold-show-all))
	(goto-char (point-max)))
      (switch-to-buffer buffer)
      (let ((clear-context (y-or-n-p "Clear the gptel context (if you have no follow-up questions)? ")))
	(if clear-context
	    (gptel-context-remove-all)
	  (message "Context not cleared. Clear it manually with `M-x gptel-context-remove-all` when finished."))))))

(defun tlon-ai--extract-question-from-info (info)
  "Extract the original user question from the INFO plist.
INFO is the plist provided to the `gptel' callback. Handles potential variations
in the :data structure, including Gemini's format (which may use vectors)."
  (let* ((data (plist-get info :data))
	 (contents (when (plistp data) (plist-get data :contents)))
	 ;; Find the user message part in the contents sequence (list or vector)
	 (user-part (when (seqp contents) ; Use seqp for list or vector
		      (cl-find-if (lambda (part)
				    (and (plistp part)
					 ;; Ensure :role exists and is the string "user"
					 (equal (plist-get part :role) "user")))
				  contents)))
	 (text-parts (when (plistp user-part) (plist-get user-part :parts)))
	 ;; The actual text seems to be in the first element of the parts sequence
	 (first-part (when (and (seqp text-parts) (> (length text-parts) 0)) ; Check sequence and non-empty
		       (elt text-parts 0))) ; Use elt for list or vector
	 (full-prompt (when (plistp first-part) (plist-get first-part :text)))
	 (question-marker "Here is the question:\n\n"))
    (if (and full-prompt (stringp full-prompt)
	     (string-match question-marker full-prompt))
	(substring full-prompt (match-end 0))
      (progn
	(message "Warning: Could not extract question from `info' plist. Relevant data: %S"
		 ;; Log relevant parts for debugging
		 `(:data ,data :contents ,contents :user-part ,user-part :text-parts ,text-parts :first-part ,first-part :full-prompt ,full-prompt))
	"Unknown Question"))))

(defvar elpaca-repos-directory)
(defun tlon-ai-get-documentation-files ()
  "Return a list of full paths to documentation files.
Documentation files are collected from:
1. \".org\" files within the \"doc/\" subdirectories of specified Elpaca repos.
2. \"readme.org\" or \"readme.md\" files in specified Elpaca repos.
3. Specific individual files."
  (let* ((all-doc-files '())
	 ;; 1. Directories containing .org documentation files
	 (doc-dirs (append (list (tlon-repo-lookup :dir :name "tlon-docs"))
			   (mapcar (lambda (subdir)
				     (file-name-concat elpaca-repos-directory subdir))
				   '("tlon/doc/"
				     "dotfiles/emacs/extras/doc/"))))
	 (doc-pattern "\\.org\\'")
	 ;; 2. Repositories to check for readme files
	 (repos (append (tlon-lookup-all tlon-repos :dir :help t)
			(mapcar (lambda (subdir)
				  (file-name-concat elpaca-repos-directory subdir))
				'("annas-archive"
				  "bib"
				  "gdrive"
				  "goldendict-ng"
				  "gptel-plus"
				  "init"
				  "macos"
				  "mullvad"
				  "pdf-tools-pages"
				  "pomodoro-centile"
				  "scihub"))))
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
  (let ((bib-file (ebib-extras-get-file-of-key key)))
    (unless bib-file
      (error "Could not find BibTeX file for key %s" key))
    (let ((target-buffer (find-file-noselect bib-file)))
      (with-current-buffer target-buffer
	(let ((set-field (pcase major-mode
			   ('bibtex-mode #'bibtex-set-field)
			   (_ (error "Unsupported major mode in BibTeX file: %s" major-mode)))))
	  (save-excursion
	    (goto-char (point-min))
	    (when (bibtex-search-entry key)
	      (shut-up
		(funcall set-field "abstract" abstract))
	      (message "Set abstract of `%s' in %s" key (buffer-name))
	      (when (derived-mode-p 'bibtex-mode)
		(save-buffer)))
	    (unless (bibtex-search-entry key)
	      (error "Could not find entry for key %s in buffer %s" key (buffer-name)))))))))

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
  (let* ((prompt (or (tlon-lookup tlon-ai-transcribe-phonetically-prompt
                                  :prompt :language language)
                     (tlon-lookup tlon-ai-transcribe-phonetically-prompt
                                  :prompt :language "en"))))
    (unless prompt
      (user-error "No prompt available for phonetic transcription in language %s" language))
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

(declare-function tlon-md-insert-attribute-value "tlon-md")
(declare-function tlon-get-tag-attribute-values "tlon-md")
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

;;;;; Meta Description Generation Helper

(defun tlon-ai--generate-and-insert-meta-description-for-file (file-path &optional for-interactive-command)
  "Generate and insert meta description for FILE-PATH.
Non-interactive helper. If FOR-INTERACTIVE-COMMAND is non-nil and language
cannot be determined from the file, prompt the user. Otherwise, error."
  (with-current-buffer (find-file-noselect file-path) ; Makes the buffer current temporarily
    (let* ((article-content (tlon-md-read-content)) ; Reads from current buffer (which is file-path)
	   (language
	    (or (tlon-get-language-in-file nil) ; nil means current buffer
		(if for-interactive-command
		    (tlon-select-language 'code)
		  (user-error "Cannot determine language for %s" (file-name-nondirectory file-path))))))
      (unless language ; If tlon-select-language was used and user aborted, language is nil
	(user-error "Language selection aborted for %s" (file-name-nondirectory file-path)))

      (if (string-empty-p (string-trim article-content))
	  (message "Skipping %s: Article content is empty." (file-name-nondirectory file-path))
	(if-let ((prompt (tlon-lookup tlon-ai-create-meta-description-prompt :prompt :language language)))
	    (progn
	      (message "Requesting AI meta description for %s (lang: %s)..."
		       (file-name-nondirectory file-path)
		       (or (tlon-validate-language language 'name) language))
	      (tlon-make-gptel-request prompt
				       article-content
				       #'tlon-ai-create-meta-description-callback
				       tlon-ai-summarization-model))
	  (message "Skipping %s: No meta description prompt for language %s."
		   (file-name-nondirectory file-path)
		   (or (tlon-validate-language language 'name) language)))))))


;;;;;; Slack

;;;;; Meta Description Generation

;;;###autoload
(defun tlon-ai-create-meta-description ()
  "Generate and set the \"meta\" description field in the YAML front matter.
Uses AI to generate a meta description based on the current buffer's content."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (unless current-file
      (user-error "Current buffer is not visiting a file"))
    (tlon-ai--generate-and-insert-meta-description-for-file current-file t))) ; Pass t for interactive behavior

;;;###autoload
(defun tlon-ai-create-title ()
  "Generate and set the \"title\" field in the YAML front matter.
Uses AI to generate an SEO-optimized title based on the current buffer's
content."
  (interactive)
  (let* ((current-file (buffer-file-name))
	 (article-content (tlon-md-read-content))
	 (language (or (tlon-get-language-in-file nil)
		       (tlon-select-language 'code))))
    (unless current-file
      (user-error "Current buffer is not visiting a file"))
    (unless language
      (user-error "Language selection aborted"))
    (if (string-empty-p (string-trim article-content))
	(message "Cannot generate title: Article content is empty.")
      (if-let ((prompt (tlon-lookup tlon-ai-create-title-prompt :prompt :language language)))
	  (progn
	    (message "Requesting AI title generation for %s (lang: %s)..."
		     (file-name-nondirectory current-file)
		     (or (tlon-validate-language language 'name) language))
	    (tlon-make-gptel-request prompt
				     article-content
				     #'tlon-ai-create-title-callback
				     tlon-ai-summarization-model))
	(message "No title generation prompt available for language %s."
		 (or (tlon-validate-language language 'name) language))))))

(defun tlon-ai-create-meta-description-callback (response info)
  "Callback for `tlon-ai-create-meta-description'.
If RESPONSE is valid, insert it as the \"meta\" field in the YAML front matter
of the original buffer. Otherwise, call `tlon-ai-callback-fail' with the
response INFO."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let* ((original-buffer (plist-get info :buffer))
	   (original-file-name (if (buffer-live-p original-buffer)
				   (buffer-file-name original-buffer)
				 nil)))
      (if (not original-file-name)
	  (user-error "Original buffer for meta description is not visiting a file or is no longer live")
	(with-current-buffer original-buffer ;; Ensure context for tlon-yaml-insert-field
	  (tlon-yaml-insert-field "meta" response)
	  (message "Meta description set for %s." (file-name-nondirectory original-file-name)))))))

(defun tlon-ai-create-title-callback (response info)
  "Callback for `tlon-ai-create-title'.
If RESPONSE is valid, insert it as the \"title\" field in the YAML front matter
of the current buffer. Otherwise, call `tlon-ai-callback-fail' with the
response INFO."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let ((cleaned-title (string-trim response)))
      (tlon-yaml-insert-field "title" cleaned-title)
      (message "Title set to: %s" cleaned-title))))

(defun tlon-ai-create-meta-descriptions-in-directory (directory)
  "Iterate over Markdown files in DIRECTORY, creating AI meta descriptions.
Skips files that already have a meta description or if language cannot be
determined."
  (interactive "DDirectory: ")
  (let ((md-files (directory-files-recursively directory "\\.md$")))
    (unless md-files
      (message "No Markdown files found in %s" directory)
      (cl-return-from tlon-ai-create-meta-descriptions-in-directory))

    (message "Found %d Markdown files. Starting meta description generation..." (length md-files))
    (dolist (file md-files)
      (message "Processing %s..." (file-name-nondirectory file))
      (if (tlon-yaml-get-key "meta" file)
	  (message "Skipping %s: meta description already exists." (file-name-nondirectory file))
	(condition-case err
	    ;; Call the helper, not for interactive command (nil for second arg)
	    (tlon-ai--generate-and-insert-meta-description-for-file file nil)
	  (user-error ; Catch user-errors from helper (e.g., language issues)
	   (message "Skipping %s: %s" (file-name-nondirectory file) (error-message-string err)))
	  (error ; Catch other unexpected errors
	   (message "Error processing %s: %s" (file-name-nondirectory file) (error-message-string err)))))
      (sit-for 0.5)) ; Allow messages to display and avoid overwhelming services
    (message "Finished processing all files in %s." directory)))

;;;;; Change propagation

(require 'magit-extras)
(declare-function magit-extras-repo-is-dirty-p "magit-extras" (&optional dir-or-repo))

(defun tlon-ai--get-files-changed-in-commit (commit repo-path)
  "Return a list of absolute file paths changed in COMMIT within REPO-PATH."
  (when (and commit repo-path (file-directory-p repo-path))
    (let* ((default-directory repo-path) ; Ensure git runs in the correct repo
	   (command (format "git diff-tree --no-commit-id --name-only -r %s"
			    (shell-quote-argument commit)))
	   (output (condition-case err
		       (shell-command-to-string command)
		     (error (message "Error getting files changed in commit %s: %s" commit err)
			    nil))))
      (when output
	(mapcar (lambda (relative-path)
		  (expand-file-name relative-path repo-path))
		(split-string (string-trim output) "\n" t))))))

;;;;;; Change Propagation Command

;;;###autoload
(defun tlon-ai-propagate-changes ()
  "Propagate modifications from all files in the latest commit of the current repo.
For each file changed in the latest commit of the current repository, get its
diff. Then, for each corresponding file in other \"uqbar\" content
repositories (originals and translations), ask the AI to apply the semantically
equivalent changes. Finally, commit the changes made by the AI in each target
repository."
  (interactive)
  (when (magit-extras-repo-is-dirty-p)
    (user-error "This command propagates changes from the most recent commit, but you have uncommitted changes"))
  (let* ((source-repo (tlon-get-repo 'no-prompt)) ; Get current repo, or prompt
	 (_ (unless source-repo (user-error "Could not determine current repository")))
	 (source-repo-name (tlon-repo-lookup :name :dir source-repo))
	 (_ (unless source-repo-name (user-error "Could not determine repository name for %s" source-repo)))
	 (source-lang (tlon-repo-lookup :language :dir source-repo))
	 (latest-commit (tlon-get-latest-commit source-repo)) ; Get latest commit for the repo
	 (_ (unless latest-commit (user-error "Could not find the latest commit for repository %s" source-repo-name)))
	 (source-files-in-commit (tlon-ai--get-files-changed-in-commit latest-commit source-repo))
	 (_ (unless source-files-in-commit (user-error "No files found in commit %s for repository %s. Aborting"
						       (substring latest-commit 0 7) source-repo-name)))
	 (all-content-repos (append (tlon-lookup-all tlon-repos :dir :subproject "uqbar" :subtype 'originals)
				    (tlon-lookup-all tlon-repos :dir :subproject "uqbar" :subtype 'translations)))
	 (target-repos (remove source-repo all-content-repos)))
    (unless target-repos
      (user-error "No target repositories found to propagate changes to"))
    (message "Found commit %s in %s. Propagating changes for %d file(s)..."
	     (substring latest-commit 0 7) source-repo-name (length source-files-in-commit))
    (dolist (source-file source-files-in-commit)
      (message "Processing source file: %s" (file-relative-name source-file source-repo))
      (let ((diff (tlon-ai--get-commit-diff latest-commit source-file source-repo)))
	(if (not diff)
	    (message "  No effective changes found for %s in commit %s. Skipping."
		     (file-relative-name source-file source-repo) (substring latest-commit 0 7))
	  (dolist (target-repo target-repos)
	    (let* ((target-lang (tlon-repo-lookup :language :dir target-repo))
		   (target-file (tlon-ai--find-target-file source-file source-repo target-repo)))
	      (if target-file
		  (let* ((target-content (with-temp-buffer
					   (insert-file-contents target-file)
					   (buffer-string)))
			 (prompt (format tlon-ai-propagate-changes-prompt source-lang
					 (file-relative-name source-file source-repo)
					 diff target-lang target-content)))
		    (message "  Requesting AI to update %s (lang: %s) in repo %s (target for %s)..."
			     (file-name-nondirectory target-file) target-lang
			     (file-name-nondirectory target-repo)
			     (file-name-nondirectory source-file))
		    (with-temp-buffer
		      (let ((gptel-track-media nil))
			(tlon-make-gptel-request
			 prompt nil
			 (lambda (response info)
			   (tlon-ai--propagate-changes-callback
			    response info target-file target-repo source-repo-name latest-commit))
			 nil t (current-buffer)))))
		(message "  No target file found in repo %s for source file %s. Skipping."
			 (file-name-nondirectory target-repo) (file-name-nondirectory source-file))))))))
    (message "AI change propagation requests initiated for all applicable files and target repositories.")))

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

(declare-function tlon-metadata-in-repo "tlon-yaml")
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

;;;;; Commit summarization

(autoload 'magit-commit-at-point "magit-git")
(autoload 'magit-git-insert "magit-git")
;;;###autoload
(defun tlon-ai-summarize-commit-diffs (beg end &optional include-stats)
  "Summarize the diffs of commits in the selected region using an LLM.
BEG and END mark the region of commits to summarize in `magit-log-mode'. When
INCLUDE-STATS is non-nil (with prefix arg), include diffstats in the prompt."
  (interactive "r\nP")
  (unless (derived-mode-p 'magit-log-mode)
    (user-error "This function is meant to be called from the Magit log (`M-x magit RET ll')"))
  (save-excursion
    (let* ((commits (save-restriction
                      (narrow-to-region beg end)
                      (goto-char (point-min))
                      (cl-loop while (not (eobp))
                               collect (magit-commit-at-point)
                               do (forward-line))))
           (commit-diffs
            (with-temp-buffer
              (apply #'magit-git-insert "show" "--patch"
                     (append (unless include-stats '("--unified=3"))
                             commits))
              (buffer-string)))
           (prompt (format
                    (concat tlon-ai-summarize-commit-diffs-system-prompt
                            "\n\n"
                            tlon-ai-summarize-commit-diffs-prompt)
                    commit-diffs
                    (with-temp-buffer
                      (insert-file-contents tlon-ai-changelog-file)
                      (buffer-string)))))
      (tlon-make-gptel-request prompt nil (tlon-ai-summarize-commit-diffs-callback commits)
                               tlon-ai-summarize-commit-diffs-model t))))

(defun tlon-ai-summarize-commit-diffs-callback (commits)
  "Callback for `tlon-ai-summarize-commit-diffs'.
COMMITS is a list of commit hashes."
  (lambda (response info)
    (if (not response)
        (tlon-ai-callback-fail info)
      (with-current-buffer (get-buffer-create "*Commit Summary*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (read-only-mode -1)
          (insert "Commit Summary:\n"
                  "==============\n\n"
                  (format "Selected commits: %s\n\n"
                          (mapconcat #'identity commits ", ")))
          (insert response)
          (goto-char (point-min)))
        (pop-to-buffer (current-buffer))))))

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

;;;;;;; Common

(defclass tlon-model-selection-infix (transient-infix)
  ((variable :initarg :variable)
   (choices  :initarg :choices)
   (default-label :initarg :default-label :initform "Default model"))
  "A transient infix for selecting AI models or using the default.")

(cl-defmethod transient-init-value ((obj tlon-model-selection-infix))
  "Initialize OBJ's value slot."
  (oset obj value (symbol-value (oref obj variable))))

(cl-defmethod transient-infix-read ((obj tlon-model-selection-infix))
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

(cl-defmethod transient-infix-set ((obj tlon-model-selection-infix) value)
  "Set the value of OBJ's variable to VALUE."
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj tlon-model-selection-infix))
  "Format OBJ's value for display."
  (let ((value (symbol-value (oref obj variable))))
    (propertize (if value
		    (format "%s: %s" (car value) (cdr value))
		  (oref obj default-label))
		'face 'transient-value)))

;;;;;;; Specific models

(transient-define-infix tlon-ai-infix-select-summarization-model ()
  "AI model to use for summarizing.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-ai-summarization-model)

(transient-define-infix tlon-ai-infix-select-markdown-fix-model ()
  "AI model to use for fixing the markdown.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-ai-markdown-fix-model)

(transient-define-infix tlon-ai-infix-select-create-reference-article-model ()
  "AI model to use for creating a reference article.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-ai-create-reference-article-model)

(transient-define-infix tlon-ai-infix-select-proofread-reference-article-model ()
  "AI model to use for proofreading a reference article.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-ai-proofread-reference-article-model)

(transient-define-infix tlon-ai-infix-select-help-model ()
  "AI model to use for asking for help.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-ai-help-model)

(transient-define-infix tlon-ai-infix-select-summarize-commit-diffs-model ()
  "AI model to use for summarizing commit diffs.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-ai-summarize-commit-diffs-model)

(transient-define-infix tlon-ai-infix-select-translation-model ()
  "AI model to use for translation.
If nil, use the default model."
  :class 'tlon-model-selection-infix
  :variable 'tlon-ai-translation-model)

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
    ("s m" "create meta description"                  tlon-ai-create-meta-description)
    ""
    "Summarize options"
    ("s -b" "batch"                                   tlon-ai-batch-fun-infix)
    ("s -m" "mullvad connection duration"             tlon-mullvad-connection-duration-infix)
    ("s -o" "overwrite abstract"                      tlon-abstract-overwrite-infix)
    ""]
   ["Math"
    ("c" "convert"                                    tlon-ai-convert-math)
    ("t" "translate"                                  tlon-ai-translate-math)
    ""
    "Reference articles"
    ("w w" "create reference article"                 tlon-ai-create-reference-article)
    ("w p" "proofread reference article"              tlon-ai-proofread-reference-article)
    ""
    "Propagation"
    ("d" "summarize commit diffs"                     tlon-ai-summarize-commit-diffs)
    ("p" "Propagate latest commit changes"            tlon-ai-propagate-changes)
    ""]
   ["Misc"
    ("b" "set language of bibtex"                     tlon-ai-set-language-bibtex)
    ("f" "fix Markdown formatting"                    tlon-ai-fix-markdown-formatting)
    ("h" "phonetically transcribe"                    tlon-ai-phonetically-transcribe)
    ("r" "rewrite"                                    tlon-ai-rewrite)
    ("l" "translate"                                  tlon-ai-translate)
    ("t" "create title"                             tlon-ai-create-title)
    ;; Create command to translate all images
    ;; TODO: develop this
    ;; ("M" "translate all math"                      tlon-ai-translate-math-in-buffer)
    ""
    "General options"
    ("-e" "edit prompt"                               tlon-ai-infix-toggle-edit-prompt)
    ("-d" "debug"                                     tlon-menu-infix-toggle-debug)
    ""
    "Models"
    ("m -d" "Summarize commit diffs" tlon-ai-infix-select-summarize-commit-diffs-model)
    ("m -f" "Markdown fix" tlon-ai-infix-select-markdown-fix-model)
    ("m -s" "Summarization" tlon-ai-infix-select-summarization-model)
    ("w -w" "Create reference article" tlon-ai-infix-select-create-reference-article-model)
    ("w -p" "Proofread reference article" tlon-ai-infix-select-proofread-reference-article-model)
    ("a -a" "Help model" tlon-ai-infix-select-help-model)]])

(defun tlon--ensure-mcp-servers (servers)
  "Ensure SERVERS are connected through MCP before a gptel request.

SERVERS is a list of server names (strings).  For each name we try
to call `gptel-mcp-connect'.  Errors are ignored so a missing or
failing server never aborts the main request."
  (when (and servers (fboundp 'gptel-mcp-connect))
    (require 'gptel-mcp nil t)
    ;; `gptel-mcp-connect' will start the server if necessary.
    (dolist (srv servers)
      (ignore-errors
        (gptel-mcp-connect srv)))))

(provide 'tlon-ai)
;;; tlon-ai.el ends here

;; Local Variables:
;; jinx-languages: "es en it fr de"
;; End:
