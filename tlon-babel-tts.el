;;; tlon-babel-tts.el --- Text-to-speech functionality -*- lexical-binding: t -*-

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

;; Text-to-speech functionality.

;;; Code:

(require 'markdown-mode)
(require 'tlon-babel-core)
(require 'tlon-babel-md)

;;;; User options

(defgroup tlon-babel-tts ()
  "Text-to-speech functionality."
  :group 'tlon-babel)

(defcustom tlon-babel-azure-tts-settings
  "audio-16khz-64kbitrate-mono-mp3"
  "Settings for the Azure text-to-speech service.
Here's a description of the main options:

- `audio-24khz-160kbitrate-mono-mp3': Offers higher quality due to a higher
  bitrate and sample rate. This means the audio will sound clearer, especially
  for more complex sounds or music. However, the file size will also be larger.

- `audio-16khz-64kbitrate-mono-mp3': Reduces the bitrate, which will result in a
  smaller file at the cost of lower audio quality. Useful when network bandwidth
  or storage is limited.

- `raw-16khz-16bit-mono-pcm': Provides raw audio data without compression. This
  is useful if you plan to further process the audio yourself or need lossless
  quality. Note that the files will be significantly larger.

- `riff-16khz-16bit-mono-pcm': Similar to the RAW format but wrapped in the
  Waveform Audio File Format, which includes headers making it compatible with
  more playback devices and software.

- `riff-24khz-16bit-mono-pcm': Offers a higher sample rate compared to the 16kHz
  versions, which can provide better audio quality at the expense of larger file
  sizes."
  :type 'string
  :group 'tlon-babel-tts)

;; TODO: add OpenAI settings
(defcustom tlon-babel-openai-tts-settings nil
  "Settings for the OpenAI text-to-speech service."
  :type 'string
  :group 'tlon-babel-tts)

;;;; Variables

;;;;; Paths

(defconst tlon-babel-dir-tts
  (file-name-concat (tlon-babel-repo-lookup :dir :name "babel-core") "tts/")
  "Directory for files related to text-to-speech functionality.")

(defconst tlon-babel-file-phonetic-transcriptions
  (file-name-concat tlon-babel-dir-tts "phonetic-transcriptions.json")
  "File with phonetic transcriptions.")

(defconst tlon-babel-file-phonetic-replacements
  (file-name-concat tlon-babel-dir-tts "phonetic-replacements.json")
  "File with replacements.")

(defconst tlon-babel-file-abbreviations
  (file-name-concat tlon-babel-dir-tts "abbreviations.json")
  "File with abbreviations.")

;;;;; Current values

(defvar tlon-babel-tts-main-voice ""
  "The main TTS voice.")

(defvar tlon-babel-tts-alternative-voice ""
  "The alternative TTS voice.")

(defvar tlon-babel-tts-current-language ""
  "The language used in the current text-to-speech process.")

;;;;; SSML tag pairs & patterns

(defconst tlon-babel-tts-ssml-break
  "<break time=\"%s\" />"
  "SSML pattern for break tag, with time placeholder.
<https://learn.microsoft.com/en-us/previous-versions/office/developer/communication-server-2007/bb813930(v=office.12)>")

;;;;;; lang

(defconst tlon-babel-tts-ssml-lang
  '("<lang xml:lang=\"%s\">" . "</lang>")
  "SSML pattern for language tag, with locale code placeholder.")

;;;;;; voice

(defconst tlon-babel-tts-ssml-voice
  '("<voice name=\"%s\">" . "</voice>")
  "SSML pair for voice tag, with voice name placeholder.")

(defconst tlon-babel-tts-ssml-voice-pattern
  (tlon-babel-make-tag-replace-pattern tlon-babel-tts-ssml-voice)
  "SSML pattern for voice tag, with voice name and text placeholders.")

(defconst tlon-babel-tts-ssml-double-voice-pattern
  (concat (cdr tlon-babel-tts-ssml-voice)
	  (tlon-babel-make-tag-replace-pattern tlon-babel-tts-ssml-voice)
	  (car tlon-babel-tts-ssml-voice))
  "SSML pattern for voice tag, with two voice name placeholders and a text placeholder.")

;;;;;; phoneme

(defconst tlon-babel-tts-ssml-phoneme
  '("<phoneme alphabet=\"%s\" ph=\"%s\">" . "</phoneme>")
  "SSML pattern for phoneme tag, with phoneme and text placeholders.
Note that, as of 2024-04-05, none of the Spanish Azure TTS voices support the
`phoneme' tag:
<https://learn.microsoft.com/en-us/azure/ai-services/speech-service/language-support?tabs=tts>.
So for the time being we should remove it from the files when generating the
audio, to avoid errors.")

(defconst tlon-babel-tts-ssml-phoneme-replace-pattern
  (tlon-babel-make-tag-replace-pattern tlon-babel-tts-ssml-phoneme)
  "Pattern to replace `phoneme' tags.")

;;;;;; emphasis

(defconst tlon-babel-tts-ssml-emphasis
  '("<emphasis level=\"%s\">" . "</emphasis>")
  "SSML pattern for emphasis tag, with level and text placeholders.
Note that this tag is not supported by Azure TTS except for a handful of voices:
<https://learn.microsoft.com/en-us/azure/ai-services/speech-service/speech-synthesis-markup-voice#adjust-emphasis>.
As with the `phoneme' tag, we should for the time being remove it when
generating the audio.")

(defconst tlon-babel-tts-ssml-emphasis-levels
  '("none" "reduced" "moderate" "strong")
  "Admissible emphasis strengths for the `emphasis' SSML tag.")

(defconst tlon-babel-tts-ssml-emphasis-default-level
  "moderate"
  "Default emphasis level for the `emphasis' SSML tag.")

;;;;; Azure

(defconst tlon-babel-azure-ssml-template
  (format "<speak version='1.0' xml:lang='%%s'>%s</speak>"
	  tlon-babel-tts-ssml-voice-pattern)
  "SSML template for Azure TTS.")

(defconst tlon-babel-azure-tts-request
  "curl --location --request POST 'https://eastus.tts.speech.microsoft.com/cognitiveservices/v1' \
--header 'Ocp-Apim-Subscription-Key: %s' \
--header 'Content-Type: application/ssml+xml' \
--header 'X-Microsoft-OutputFormat: %s' \
--header 'User-Agent: curl' \
--data-raw %s > '%s'"
  "Curl command to send a request to the Azure text-to-speech engine.")

(defconst tlon-babel-azure-voices
  '((:voice "es-US-AlonsoNeural" :language "es" :gender "male")
    (:voice "es-US-PalomaNeural" :language "es" :gender "female"))
  "Preferred Azure voices for different languages.
All the voices in this property list are neural and multilingual, and are the
best male and female voices we were able to identify in each language.

A list of available voices may be found here:
<https://github.com/MicrosoftDocs/azure-docs/blob/main/articles/ai-services/speech-service/includes/language-support/tts.md>")

(defvar tlon-babel-azure-key nil
  "Azure subscription key for the text-to-speech service.")

(defconst tlon-babel-azure-char-limit (* 9 60 14)
  "Maximum number of characters that Azure can process per request.
Azure can process up to 10 minutes of audio at a time. This estimate assumes 14
characters per second, and uses nine minutes.")

;;;;; OpenAI

;; TODO: complete this; as of 2024-04-08, OpenAI offers no SSML support
;; (https://community.openai.com/t/what-about-to-implement-ssml-on-the-new-tts-api-service/485686/5)
;; so not a priority
(defconst tlon-babel-openai-tts-request
  "curl https://api.openai.com/v1/audio/speech \
  -H \"Authorization: Bearer %s\" \
  -H \"Content-Type: application/json\" \
  -d '{
    \"model\": \"tts-1\",
    \"input\": \"%s\",
    \"voice\": \"alloy\"
  }' \
  --output '%s'"
  "Curl command to send a request to the OpenAI text-to-speech engine.")

;;;;; Currencies

(defconst tlon-babel-tts-currencies
  '(("₿" . "BTC")
    ("Ξ" . "ETH")
    ("£" . "GBP")
    ("₪" . "ILS")
    ("₹" . "INR")
    ("¥" . "JPY")
    ("$" . "USD"))
  "Currency symbols and their associated three-letter codes.")

(defconst tlon-babel-tts-currency-ssml
  "<say-as interpret-as=\"currency\">%s %s</say-as>"
  "SSML pattern for currency symbols.
The first placeholder is the currency amount, and the second is the currency
code.

For more information, see <https://learn.microsoft.com/en-us/azure/ai-services/speech-service/speech-synthesis-markup-pronunciation#say-as-element>.")

;;;;; File-local variables

(defvar-local tlon-babel-file-local-abbreviations '()
  "In-text abbreviations and their spoken equivalent.")

(defvar-local tlon-babel-file-local-replacements '()
  "File local replacements.")

;;;;; Abbreviations

(defvar tlon-babel-tts-abbreviations
  (tlon-babel-parse-json tlon-babel-file-abbreviations)
  "Standard abbreviations and their spoken equivalent in each language.")

;;;;; Phonetic replacements

(defvar tlon-babel-tts-phonetic-replacements
  (tlon-babel-parse-json tlon-babel-file-phonetic-replacements)
  "Phonetic replacements for terms.")

;;;;; Phonetic transcriptions

(defvar tlon-babel-tts-phonetic-transcriptions
  (tlon-babel-parse-json tlon-babel-file-phonetic-transcriptions)
  "Phonetic transcriptions for terms.")

;;;;; Listener cues

(defconst tlon-babel-tts-cue-delimiter
  (concat "\n" (format tlon-babel-tts-ssml-break "0.5s"))
  "Delimiter for listener cues.")

;;;;;; Notes

(defconst tlon-babel-tts-note-cues
  '(("en" "A note starts here." . "The note ends here.")
    ("es" "Aquí empieza una nota." . "Aquí termina la nota."))
  "Listener cues for notes.")

;;;;;; Quotes

(defconst tlon-babel-tts-quote-cues
  '(("en" "A quote starts here." . "The quote ends here.")
    ("es" "Aquí empieza una cita." . "Aquí termina la cita."))
  "Listener cues for asides.")

;;;;;; Asides

(defconst tlon-babel-tts-aside-cues
  '(("en" "An aside starts here." . "The aside ends here.")
    ("es" "Aquí empieza un inciso." . "Aquí termina el inciso."))
  "Listener cues for asides.")

;;;;;; Images

(defconst tlon-babel-tts-image-cues
  '(("en" "Here’s an image." . "The image ends here.")
    ("es" "Aquí hay una imagen." ."Aquí termina la imagen."))
  "Listener cues for images.")

;;;; Functions

(defun tlon-babel-tts-get-alternative-voice ()
  "Return the voice in the current language that is not the current voice."
  (let ((voices (tlon-babel-lookup-all tlon-babel-azure-voices
				       :voice :language tlon-babel-tts-current-language)))
    (car (delete tlon-babel-tts-main-voice voices))))

(defun tlon-babel-tts-get-voice-locale ()
  "Return the locale of the current voice."
  (substring tlon-babel-tts-main-voice 0 5))

;;;;; Reading

(defun tlon-babel-tts-read-content (content &optional chunk-size)
  "Read CONTENT and return it as a string ready for TTS processing.
If CHUNK-SIZE is non-nil, split string into chunks no larger than that size."
  (with-temp-buffer
    (insert content)
    (tlon-babel-tts-prepare-buffer)
    (if chunk-size
	(tlon-babel-break-into-chunks chunk-size)
      (buffer-string))))

(defun tlon-babel-tts-read-file (file)
  "Return substantive content of FILE, handling in-text abbreviations."
  (unless (string= (file-name-extension file) "md")
    (user-error "File `%s' is not a Markdown file" file))
  (let ((tlon-babel-file-local-abbreviations))
    (with-current-buffer (find-file-noselect file)
      ;; to make `tlon-babel-tts-process-file-local-abbreviations' work, we
      ;; let-bound the variable above and now set its value to that of its
      ;; file-local counterpart
      (setq tlon-babel-file-local-abbreviations tlon-babel-file-local-abbreviations)
      (concat (tlon-babel-tts-get-metadata) (tlon-babel-md-read-content file)))))

;;;;; Azure

;;;###autoload
(defun tlon-babel-azure-narrate-content (&optional voice file)
  "Narrate content with VOICE using Azure's text-to-speech service.
If region is active, read the region. Otherwise, read FILE.

Save the narration to the `audio' directory in the same repository as FILE or,
if region is active, save it to the downloads directory."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-get-repo-from-file file))
	 (language (setq tlon-babel-tts-current-language
			 (tlon-babel-repo-lookup :language :dir repo)))
	 (voice (setq tlon-babel-tts-main-voice
		      (or voice (completing-read "Voice: " (tlon-babel-lookup-all
							    tlon-babel-azure-voices :voice
							    :language language)))))
	 (content (if (region-active-p)
		      (buffer-substring-no-properties (region-beginning) (region-end))
		    (tlon-babel-tts-read-file file)))
	 (chunks (tlon-babel-tts-read-content content tlon-babel-azure-char-limit))
	 (file-name-sans-extension (file-name-sans-extension
				    (file-name-nondirectory file)))
	 ;; FIXME: extension should be set based on `tlon-babel-azure-tts-settings'
	 (file-name (file-name-with-extension file-name-sans-extension "mp3"))
	 (destination (if (region-active-p)
			  (file-name-concat paths-dir-downloads file-name)
			(file-name-concat repo "audio" file-name)))
	 (nth 1))
    (setq tlon-babel-unprocessed-chunks
	  (tlon-babel-get-chunk-names destination (length chunks)))
    (dolist (chunk chunks)
      (tlon-babel-azure-generate-audio
       chunk voice (tlon-babel-get-chunk-name destination nth))
      (setq nth (1+ nth)))))

(defvar tlon-babel-unprocessed-chunks nil
  "The chunks to process in the current TTS session.")

(defun tlon-babel-azure-generate-audio (text voice destination)
  "Generate an audio file from TEXT using VOICE and save it to DESTINATION."
  (let* ((locale (tlon-babel-tts-get-voice-locale))
	 (ssml (shell-quote-argument
		(format tlon-babel-azure-ssml-template locale voice text)))
	 (command (format tlon-babel-azure-tts-request
			  (tlon-babel-get-or-set-azure-key)
			  tlon-babel-azure-tts-settings ssml destination))
	 (process (start-process-shell-command "generate audio" nil command)))
    (set-process-sentinel process
			  (lambda (process event)
			    (when (string= event "finished\n")
			      (if (region-active-p)
				  (shell-command (format "open %s" destination))
				(setq tlon-babel-unprocessed-chunks
				      (remove destination tlon-babel-unprocessed-chunks))
				(unless tlon-babel-unprocessed-chunks
				  (let ((file (tlon-babel-get-original-name destination)))
				    (tlon-babel-join-chunks file)
				    (tlon-babel-delete-chunks file)))))))))

(defun tlon-babel-get-or-set-azure-key ()
  "Get or set the Azure key."
  (or tlon-babel-azure-key
      (setq tlon-babel-azure-key
	    (auth-source-pass-get "key1" "tlon/core/live.com/tlon.shared@gmail.com [Azure]"))))

;;;;; Chunk processing

(defun tlon-babel-break-into-chunks (chunk-size)
  "Break text in current buffer into chunks no larger than CHUNK-SIZE."
  (let ((chunks '())
	(begin 1)
	end)
    (while (not (eobp))
      (goto-char (min
		  (point-max)
		  (+ begin chunk-size)))
      (setq end (if (eobp)
		    (point)
		  (backward-sentence))) ; avoid mid-sentence breaks
      (push (string-trim (buffer-substring-no-properties begin end)) chunks)
      (setq begin end))
    (nreverse chunks)))

(defun tlon-babel-join-chunks (file)
  "Join chunks of FILE back into a single file."
  (let* ((files (tlon-babel-get-list-of-chunks file))
	 (list-of-files (tlon-babel-create-list-of-chunks files)))
    (shell-command (format "ffmpeg -f concat -safe 0 -i %s -c copy %s"
			   list-of-files file))))

(defun tlon-babel-get-list-of-chunks (file)
  "Return a list of the file chunks for FILE."
  (let ((nth 1)
	file-chunk
	files)
    (while (file-exists-p (setq file-chunk (tlon-babel-get-chunk-name file nth)))
      (push file-chunk files)
      (setq nth (1+ nth)))
    (nreverse files)))

(defun tlon-babel-delete-chunks (file)
  "Delete the chunks of FILE."
  (dolist (file (tlon-babel-get-list-of-chunks file))
    (delete-file file)))

(defun tlon-babel-create-list-of-chunks (files)
  "Create a temporary file with a list of audio FILES for use with `ffmpeg'."
  (let ((temp-file-list (make-temp-file "files" nil ".txt")))
    (with-temp-file temp-file-list
      (dolist (file files)
	(insert (format "file '%s'\n" (expand-file-name file)))))
    temp-file-list))

(defun tlon-babel-get-chunk-name (file nth)
  "Return the name of the NTH chunk of FILE."
  (let ((extension (file-name-extension file))
	(file-name-sans-extension (file-name-sans-extension file)))
    (format "%s-%03d.%s" file-name-sans-extension nth extension)))

(defun tlon-babel-get-chunk-names (file n)
  "Return a list of the first N chunk names of FILE."
  (let ((names '()))
    (dotimes (i n names)
      (push (tlon-babel-get-chunk-name file (1+ i)) names))))

(defun tlon-babel-get-original-name (chunk-name)
  "Return the original file name before it was chunked, given CHUNK-NAME."
  (let* ((base-name (file-name-sans-extension chunk-name))
         (extension (file-name-extension chunk-name))
         (original-base-name (replace-regexp-in-string "-[0-9]+\\'" "" base-name)))
    (format "%s.%s" original-base-name extension)))

;;;;; Metadata

;; Should also include summary?
(defun tlon-babel-tts-get-metadata ()
  "Add title and author."
  (let* ((metadata (tlon-babel-yaml-format-values-of-alist (tlon-babel-yaml-get-metadata)))
	 (title (alist-get "title" metadata nil nil #'string=))
	 (authors (alist-get "authors" metadata nil nil #'string=))
	 (author-string (tlon-babel-concatenate-list authors)))
    (format "%s.\n\nPor %s.\n\n" title author-string)))

;;;;; Cleanup

(defun tlon-babel-tts-prepare-buffer ()
  "Prepare the current buffer for audio narration."
  (save-excursion
    (tlon-babel-tts-process-notes)
    (tlon-babel-tts-process-citations)
    (tlon-babel-tts-process-formatting)
    (tlon-babel-tts-process-headings)
    ;; replace small caps
    ;; check all other elements in markdown-menu
    (tlon-babel-tts-process-file-local-abbreviations)
    (tlon-babel-tts-process-file-local-replacements)
    (tlon-babel-tts-process-abbreviations)
    (tlon-babel-tts-process-phonetic-replacements)
    (tlon-babel-tts-process-phonetic-transcriptions)
    (tlon-babel-tts-process-alternative-voice)
    (tlon-babel-tts-process-asides)
    (tlon-babel-tts-process-quotes)
    (tlon-babel-tts-process-images) ; should be before links
    (tlon-babel-tts-process-links)
    (tlon-babel-tts-process-numbers)
    (tlon-babel-tts-process-currencies)
    (tlon-babel-tts-process-math-expressions))
  (goto-char (point-min)))

;; TODO: it seems Azure is not making an extra pause between paragraphs; decide whether to add some extra silence

;;;;;; Notes

(defun tlon-babel-tts-process-notes ()
  "Replace note reference with its content, if it is a sidenote, else delete it.
Move the note to the end of the sentence if necessary.

Note: the function assumes that the citation is in MDX, rather than Pandoc
citation key, format. Hence, it must be run *before*
`tlon-babel-tts-process-citations'."
  (goto-char (point-min))
  (while (re-search-forward markdown-regex-footnote nil t)
    (let (reposition)
      (markdown-footnote-kill)
      (unless (looking-back (concat "\\.\\|" markdown-regex-footnote) (line-beginning-position))
	(setq reposition t))
      (when (eq (tlon-babel-get-note-type (current-kill 0)) 'sidenote)
	(when reposition
	  (forward-sentence))
	(insert (tlon-babel-tts-handle-note (string-trim (current-kill 0))))))))

(defun tlon-babel-tts-handle-note (note)
  "Handle NOTE for audio narration."
  (let ((clean-note (replace-regexp-in-string tlon-babel-sidenote-marker "" note)))
    (tlon-babel-tts-listener-cue-full-enclose tlon-babel-tts-note-cues clean-note)))

;;;;;; Citations

(defun tlon-babel-tts-process-citations ()
  "Replace our custom MDX cite tags with a pandoc-style citation.
For example `<Cite bibKey={\"Clark2015SonAlsoRises\"} />' will be replaced with
`[@Clark2015SonAlsoRises]'."
  (goto-char (point-min))
  (while (re-search-forward tlon-babel-cite-pattern nil t)
    (replace-match (format "[@%s]"(match-string 1)) nil nil)))

;;;;;; Tag removal

;; TODO: I think we should just use `tlon-babel-tts-remove-formatting' for this,
;; together with the patterns defined in `tlon-babel-md'


;;;;;; Formatting

(defun tlon-babel-tts-process-formatting ()
  "Remove formatting from text."
  (tlon-babel-tts-process-boldface)
  (tlon-babel-tts-process-boldface))
  (tlon-babel-tts-process-visually-shown))

(defun tlon-babel-tts-remove-formatting (type)
  "Remove formatting TYPE from text."
  (cl-destructuring-bind (pattern . group)
      (pcase type
	('boldface (cons markdown-regex-bold 4))
	('italics (cons markdown-regex-italic 3))
	;; TODO: determine if other types should be added
	('visually-shown (cons tlon-babel-mdx-visually-shown-search-pattern nil))
	(_ (user-error "Invalid formatting type: %s" type)))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match (format " %s" (match-string group)) t nil))))

(defun tlon-babel-tts-process-boldface ()
  "Remove boldface from text."
  (tlon-babel-tts-remove-formatting 'boldface))

(defun tlon-babel-tts-process-italics ()
  "Remove italics from text."
  (tlon-babel-tts-remove-formatting 'italics))

(defun tlon-babel-tts-process-visually-hidden ()
  "Remove `VisuallyHidden' MDX tag."
  (tlon-babel-tts-remove-formatting 'visually-hidden))

(defun tlon-babel-tts-process-visually-shown ()
  "Remove `VisuallyShown' MDX tag."
  (tlon-babel-tts-remove-formatting 'visually-shown))

;;;;;; Headings

(defun tlon-babel-tts-process-headings ()
  "Remove heading markers from headings."
  (let ((insert-pause (format tlon-babel-tts-ssml-break "1s")))
    (goto-char (point-min))
    (while (re-search-forward markdown-regex-header nil t)
      (replace-match (format "%s %s" insert-pause (match-string 5))))))

;;;;;; File-local abbreviations

(defun tlon-babel-tts-process-file-local-abbreviations ()
  "Replace file-local abbreviations with their spoken equivalent.
In-text abbreviations are those that are introduced in the text itself,
typically in parenthesis after the first occurrence of the phrase they
abbreviate. We store these abbreviations on a per file basis, in the file-local
variable `tlon-babel-file-local-abbreviations'"
  (let ((case-fold-search nil))
    (dolist (entry tlon-babel-file-local-abbreviations)
      (cl-destructuring-bind (abbrev . expansion) entry
	(let ((abbrev-introduced (format "%s (%s)" expansion abbrev)))
	  ;; we first replace the full abbrev introduction, then the abbrev itself
	  (dolist (cons (list (cons abbrev-introduced expansion) entry))
	    (goto-char (point-min))
	    (while (re-search-forward (car cons) nil t)
	      (replace-match (cdr cons) t nil))))))))

;;;;;; File-local replacements

(defun tlon-babel-tts-process-file-local-replacements ()
  "Perform replacements indicated in `tlon-babel-file-local-replacements'."
  (dolist (pair tlon-babel-file-local-replacements)
    (let ((find (car pair)))
      (goto-char (point-min))
      (while (re-search-forward find nil t)
	(replace-match (cdr pair) t)))))

;;;;;; Project-wide replacements

;;;;;;; Common

(defun tlon-babel-tts-process-terms (terms replacement-fun)
  "Replace TERMS using REPLACEMENT-FUN."
  (let ((case-fold-search nil))
    (dolist (term terms)
      (let ((find (format "\\b%s\\b" (car term)))
	    (replacement (cdr term)))
	(goto-char (point-min))
	(while (re-search-forward find nil t)
	  (funcall replacement-fun replacement))))))

(defun tlon-babel-tts-get-associated-terms (var)
  "Get associated terms for the current language in VAR.
For each cons cell in VAR for the language in the current text-to-speech
process, return its cdr."
  (let ((result '()))
    (dolist (term var result)
      (when (member tlon-babel-tts-current-language (car term))
	(setq result (append result (cadr term)))))
    result))

;;;;;;; Abbreviations

(defun tlon-babel-tts-process-abbreviations ()
  "Replace terms with their pronunciations."
  (tlon-babel-tts-process-terms
   (tlon-babel-get-abbreviations)
   'tlon-babel-replace-abbreviations))

(defun tlon-babel-replace-abbreviations (replacement)
  "When processing abbreviations, replace match with REPLACEMENT."
  (replace-match replacement t))

(defun tlon-babel-get-abbreviations ()
  "Get abbreviations."
  (tlon-babel-tts-get-associated-terms tlon-babel-tts-abbreviations))

;;;;;;; Phonetic replacements

(defun tlon-babel-tts-process-phonetic-replacements ()
  "Replace terms with their counterparts."
  (tlon-babel-tts-process-terms
   (tlon-babel-get-phonetic-replacements)
   'tlon-babel-replace-phonetic-replacements))

(defun tlon-babel-replace-phonetic-replacements (replacement)
  "When processing simple replacements, replace match with REPLACEMENT."
  (replace-match replacement t))

(defun tlon-babel-get-phonetic-replacements ()
  "Get simple replacements."
  (tlon-babel-tts-get-associated-terms tlon-babel-tts-phonetic-replacements))

;;;;;;; Phonetic transcriptions

(defun tlon-babel-tts-process-phonetic-transcriptions ()
  "Replace terms with their pronunciations."
  (tlon-babel-tts-process-terms
   (tlon-babel-get-phonetic-transcriptions)
   'tlon-babel-replace-phonetic-transcriptions))

(defun tlon-babel-replace-phonetic-transcriptions (replacement)
  "When processing phonetic transcriptions, replace match with pattern.
REPLACEMENT is the cdr of the cons cell for the term being replaced."
  (replace-match (format tlon-babel-tts-ssml-phoneme-replace-pattern
			 "ipa" replacement (match-string-no-properties 0)) t))

(defun tlon-babel-get-phonetic-transcriptions ()
  "Get the phonetic transcriptions."
  (tlon-babel-tts-get-associated-terms tlon-babel-tts-phonetic-transcriptions))

;;;;;; Listener cues

;;;;;;; General functions

(defun tlon-babel-tts-enclose-in-listener-cues (type text)
  "Enclose TEXT in listener cues of TYPE."
  (cl-destructuring-bind (cue-begins . cue-ends)
      (alist-get tlon-babel-tts-current-language type nil nil #'string=)
    (format "%s %s %s" cue-begins text cue-ends)))

(defun tlon-babel-tts-enclose-in-voice-tag (string &optional voice)
  "Enclose STRING in `voice' SSML tags.
If VOICE is nil, default to the alternative voice.

Note that this function actually inserts two pairs of `voice' tags: the inner
pair to set the voice for the string that it encloses, and an outer pair of tags
in reverse order, to close the opening `voice' tag that wraps the entire
document, and then reopen it."
  (let ((voice (or voice (tlon-babel-tts-get-alternative-voice))))
    (format tlon-babel-tts-ssml-double-voice-pattern
	    (tlon-babel-tts-get-alternative-voice) string
	    tlon-babel-tts-main-voice)))

(defun tlon-babel-tts-enclose-in-cue-delimiter (string)
  "Enclose STRING in listener cue delimiter."
  (format "%1$s%s%1$s" tlon-babel-tts-cue-delimiter string))

(defun tlon-babel-tts-listener-cue-full-enclose (type text)
  "Enclose TEXT in listener cue of TYPE, and in turn in `voice' SSML tags."
  (tlon-babel-tts-enclose-in-cue-delimiter
   (tlon-babel-tts-enclose-in-voice-tag
    (tlon-babel-tts-enclose-in-listener-cues type text))))

(defun tlon-babel-tts-process-element (type)
  "Add listener cues for text enclosed in tags of TYPE."
  (cl-destructuring-bind (pattern cues group)
      (pcase type
	('aside (list tlon-babel-mdx-aside-search-pattern tlon-babel-tts-aside-cues 2))
	('quote (list markdown-regex-blockquote tlon-babel-tts-quote-cues 3))
	('image (list markdown-regex-link-inline tlon-babel-tts-image-cues 3))
	;; TODO: determine if other types should be added
	(_ (user-error "Invalid formatting type: %s" type)))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match
       (tlon-babel-tts-listener-cue-full-enclose cues (match-string-no-properties group))))))

;;;;;;; Specific elements

(defun tlon-babel-tts-process-quotes ()
  "Add listener cues for blockquotes."
  (tlon-babel-tts-process-element 'quote))

(defun tlon-babel-tts-process-asides ()
  "Add listener cues for asides."
  (tlon-babel-tts-process-element 'aside))

(defun tlon-babel-tts-process-images ()
  "Add listener cues for images."
  (tlon-babel-tts-process-element 'image))

;;;;;; Links

(defun tlon-babel-tts-process-links ()
  "Replace links with their text.
Note: this function should be run after `tlon-babel-tts-process-images' because
image links are handled differently."
  (goto-char (point-min))
  (while (re-search-forward markdown-regex-link-inline nil t)
    (replace-match (match-string 3))))

;;;;;; Numbers

(defun tlon-babel-tts-process-numbers ()
  "Remove number separators."
  (let* ((separator (alist-get tlon-babel-tts-current-language
			       tlon-babel-md-number-separators nil nil #'string=)))
    (goto-char (point-min))
    (while (re-search-forward (format tlon-babel-md-number-separator-pattern separator) nil t)
      (replace-match "\\1\\3"))))

;;;;;; Currencies

(defun tlon-babel-tts-process-currencies ()
  "Format currency with appropriate SSML tags."
  (dolist (cons tlon-babel-tts-currencies)
    (let ((symbol (car cons))
	  (code (cdr cons)))
      (goto-char (point-min))
      (while (re-search-forward (format "%s.?\\([0-9,.]+\\)\\b" (regexp-quote symbol)) nil t)
	(replace-match (format tlon-babel-tts-currency-ssml
			       (match-string-no-properties 1) code))))))

;;;;;; Math expressions

(declare-function tlon-babel-ai-translate-math "tlon-babel-ai")
(defun tlon-babel-tts-process-math-expressions ()
  "Replace math expressions with their spoken equivalent."
  (dolist (pattern (list tlon-babel-math-inline-search-pattern
			 tlon-babel-math-display-search-pattern))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (let ((math (match-string-no-properties 2)))
	(replace-match "" nil nil)
	(tlon-babel-ai-translate-math math tlon-babel-tts-current-language)))))

;;;;; Alternative voice

(defun tlon-babel-tts-process-alternative-voice ()
  "Replace the `AlternativeVoice' tag with an SSML `voice' tag.
The `voice' tag is set to the alternative voice for the current language."
  (goto-char (point-min))
  (while (re-search-forward tlon-babel-mdx-alternative-voice-search-pattern nil t)
    (replace-match (tlon-babel-tts-enclose-in-voice-tag (match-string 2)) t)))

;;;;; Project-wide

;;;;;; Common

(defun tlon-babel-tts-edit-entry (data file)
  "Add or revise an entry in DATA and write it to FILE."
  (interactive)
  (let* ((names (mapcan (lambda (group)
			  (mapcar #'car (cadr group)))
			data))
	 (term (completing-read "Term: " names nil nil))
	 (current-entry (catch 'current-entry
			  (dolist (group data)
			    (dolist (pair (cadr group))
			      (when (string= (car pair) term)
				(throw 'current-entry (cdr pair)))))
			  nil)))
    (if current-entry
	(let ((cdr (read-string (format "Updated entry for %s: " term) current-entry)))
	  (tlon-babel-tts-revise-entry data term cdr))
      (tlon-babel-tts-add-new-entry data term))
    (tlon-babel-write-data file data)))

(defun tlon-babel-tts-revise-entry (data term cdr)
  "Update the CDR for an existing TERM in DATA."
  (dolist (group data)
    (dolist (pair (cadr group))
      (when (string= (car pair) term)
	(setcdr pair cdr)))))

(defun tlon-babel-tts-add-new-entry (data term)
  "Add a new TERM to DATA."
  (let* ((languages (tlon-babel-select-language 'two-letter 'babel 'multiple))
         (dict-entry (read-string "Term: "))
         (new-entry (cons term dict-entry))
         (added nil))
    (dolist (group data)
      (when (and (not added)
                 (equal (car group) languages))
        (setf (cadr group) (cons new-entry (cadr group)))
        (setq added t)))
    (unless added
      (nconc data (list (list languages (list new-entry)))))))

;;;;;; Abbreviations

;;;###autoload
(defun tlon-babel-edit-abbreviations ()
  "Edit abbreviations."
  (interactive)
  (tlon-babel-tts-edit-entry
   tlon-babel-tts-abbreviations
   tlon-babel-file-abbreviations))

;;;;;; Phonetic replacements

;;;###autoload
(defun tlon-babel-edit-phonetic-replacements ()
  "Edit phonetic replacements."
  (interactive)
  (tlon-babel-tts-edit-entry
   tlon-babel-tts-phonetic-replacements
   tlon-babel-file-phonetic-replacements))

;;;;;; Phonetic transcriptions

;;;###autoload
(defun tlon-babel-edit-phonetic-transcriptions ()
  "Edit phonetic transcriptions."
  (interactive)
  (tlon-babel-tts-edit-entry
   tlon-babel-tts-phonetic-transcriptions
   tlon-babel-file-phonetic-transcriptions))

;;;;; File-local

;;;;;; Common

(defun tlon-babel-add-in-text-cons-cell (prompts var)
  "Add an in-text cons-cell to the file-local named VAR.
PROMPTS is a cons cell with the corresponding prompts."
  (let* ((var-value (symbol-value var))
	 (key (substring-no-properties
	       (completing-read (car prompts) var-value)))
	 (default-expansion (alist-get key var-value nil nil #'string=))
	 (value (substring-no-properties
		 (completing-read (cdr prompts) (mapcar #'cdr var-value)
				  nil nil default-expansion)))
	 (cons-cell (cons key value)))
    (set var
	 (cl-remove-if (lambda (existing-cell)
			 "If a new element was set for an existing cell, remove it."
			 (string= (car existing-cell) (car cons-cell)))
		       var-value))
    (modify-file-local-variable var (push cons-cell var-value) 'add-or-replace)
    (hack-local-variables)))

;;;;;; Abbreviations

;;;###autoload
(defun tlon-babel-add-file-local-abbreviation ()
  "Add an in-text abbreviation to the file-local list."
  (interactive)
  (tlon-babel-add-in-text-cons-cell '("Abbrev: " . "Expanded abbrev: ")
				    'tlon-babel-file-local-abbreviations))

;;;;;; Replacements

;;;###autoload
(defun tlon-babel-add-file-local-replacement ()
  "Add an in-text replacement to the file-local list."
  (interactive)
  (tlon-babel-add-in-text-cons-cell '("Text to replace: " . "Replacement: ")
				    'tlon-babel-file-local-replacements))

(provide 'tlon-babel-tts)
;;; tlon-babel-tts.el ends here
