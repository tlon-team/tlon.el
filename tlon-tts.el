;;; tlon-tts.el --- Text-to-speech functionality -*- lexical-binding: t -*-

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

;; Text-to-speech functionality.

;;; Code:

(require 'tlon-core)
(require 'tlon-md)

;;;; User options

(defgroup tlon-tts ()
  "Text-to-speech functionality."
  :group 'tlon)

;;;; Variables

;;;;; Paths

(defconst tlon-dir-tts
  (file-name-concat (tlon-repo-lookup :dir :name "babel-core") "tts/")
  "Directory for files related to text-to-speech functionality.")

(defconst tlon-file-phonetic-transcriptions
  (file-name-concat tlon-dir-tts "phonetic-transcriptions.json")
  "File with phonetic transcriptions.")

(defconst tlon-file-phonetic-replacements
  (file-name-concat tlon-dir-tts "phonetic-replacements.json")
  "File with replacements.")

(defconst tlon-file-abbreviations
  (file-name-concat tlon-dir-tts "abbreviations.json")
  "File with abbreviations.")

;;;;; Current values

(defvar tlon-tts-main-voice ""
  "The main TTS voice.")

(defvar tlon-tts-alternative-voice ""
  "The alternative TTS voice.")

(defvar tlon-tts-current-language ""
  "The language used in the current text-to-speech process.")

;;;;; Chunk processing

(defvar tlon-unprocessed-chunks nil
  "The chunks to process in the current TTS session.")

;;;;; SSML tag pairs & patterns

;;;;;; `break'

(defconst tlon-tts-ssml-break
  "<break time=\"%s\" />"
  "SSML pattern for break tag, with time placeholder.
<https://learn.microsoft.com/en-us/previous-versions/office/developer/communication-server-2007/bb813930(v=office.12)>.")

;; (defconst tlon-tts-ssml-break-search-pattern
;; FIXME: this is not working; `tlon-make-tag-search-pattern' needs to be adjusted to support self-closing tags
;; (tlon-make-tag-search-pattern tlon-tts-ssml-break)
;; "Pattern to search for `break' tags.")

;;;;;; `emphasis'

(defconst tlon-tts-ssml-emphasis
  '("<emphasis level=\"%s\">" . "</emphasis>")
  "SSML pattern for emphasis tag, with level and text placeholders.")

(defconst tlon-tts-ssml-emphasis-levels
  '("none" "reduced" "moderate" "strong")
  "Admissible emphasis strengths for the `emphasis' SSML tag.")

(defconst tlon-tts-ssml-emphasis-default-level
  "moderate"
  "Default emphasis level for the `emphasis' SSML tag.")

(defconst tlon-tts-ssml-emphasis-search-pattern
  (tlon-make-tag-search-pattern tlon-tts-ssml-emphasis)
  "Pattern to search for `emphasis' tags.")

;;;;;; `lang'

(defconst tlon-tts-ssml-lang
  '("<lang xml:lang=\"%s\">" . "</lang>")
  "SSML pattern for language tag, with locale code placeholder.")

(defconst tlon-tts-ssml-lang-search-pattern
  (tlon-make-tag-search-pattern tlon-tts-ssml-lang)
  "Pattern to search for `lang' tags.")

;;;;;; `phoneme'

(defconst tlon-tts-ssml-phoneme
  '("<phoneme alphabet=\"%s\" ph=\"%s\">" . "</phoneme>")
  "SSML pattern for phoneme tag, with phoneme and text placeholders.")

(defconst tlon-tts-ssml-phoneme-search-pattern
  (tlon-make-tag-search-pattern tlon-tts-ssml-phoneme)
  "Pattern to search for `phoneme' tags.")

(defconst tlon-tts-ssml-phoneme-replace-pattern
  (tlon-make-tag-replace-pattern tlon-tts-ssml-phoneme)
  "Pattern to replace `phoneme' tags.")

;;;;;; `voice'

(defconst tlon-tts-ssml-voice
  '("<voice name=\"%s\">" . "</voice>")
  "SSML pair for voice tag, with voice name placeholder.")

(defconst tlon-tts-ssml-voice-search-pattern
  (tlon-make-tag-search-pattern tlon-tts-ssml-voice)
  "Pattern to search for voice tags.")

(defconst tlon-tts-ssml-voice-replace-pattern
  (tlon-make-tag-replace-pattern tlon-tts-ssml-voice)
  "SSML pattern for voice tag, with voice name and text placeholders.")

(defconst tlon-tts-ssml-double-voice-replace-pattern
  (concat (cdr tlon-tts-ssml-voice)
	  (tlon-make-tag-replace-pattern tlon-tts-ssml-voice)
	  (car tlon-tts-ssml-voice))
  "SSML pattern for voice tag, with 2 voice name placeholders and text placeholder.")

;;;;;; Common

(defconst tlon-tts-supported-tags
  `((:tag break
	  :polly t
	  :azure t
	  :google t
	  :openai nil)
    (:tag emphasis
	  :polly nil
	  :azure nil ; https://bit.ly/azure-ssml-emphasis
	  :google t
	  :openai nil
	  :pattern ,tlon-tts-ssml-emphasis-search-pattern)
    (:tag lang
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :pattern ,tlon-tts-ssml-lang-search-pattern)
    (:tag mark
	  :polly t
	  :azure t
	  :google t
	  :openai nil)
    (:tag p
	  :polly t
	  :azure t
	  :google t
	  :openai nil)
    (:tag phoneme
	  :polly t
	  :azure nil ; https://bit.ly/azure-ssml-phoneme
	  :google t
	  :openai nil
	  :pattern ,tlon-tts-ssml-phoneme-search-pattern)
    (:tag prosody
	  :polly t ; partial support
	  :azure t
	  :google t
	  :openai nil)
    (:tag s
	  :polly t
	  :azure t
	  :google t
	  :openai nil)
    (:tag say-as
	  :polly t ; partial support
	  :azure t
	  :google t
	  :openai nil)
    (:tag speak
	  :polly t
	  :azure t
	  :google t
	  :openai nil)
    (:tag sub
	  :polly t
	  :azure t
	  :google t
	  :openai nil)
    (:tag voice
	  :polly nil
	  :azure t
	  :google t
	  :openai nil
	  :pattern tlon-tts-ssml-voice-search-pattern)
    (:tag w
	  :polly t
	  :azure t
	  :google t
	  :openai nil))
  "SSML tags supported by different TTS engines.
- Amazon Polly:
  <https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html>.

- Microsoft Azure:

- Google Cloud <https://cloud.google.com/text-to-speech/docs/ssml>.:

- OpenAI:
  <https://community.openai.com/t/what-about-to-implement-ssml-on-the-new-tts-api-service/485686/5>.")

;;;;; Engine settings

;;;;;; Microsoft Azure

(defconst tlon-microsoft-azure-request
  "curl --location --request POST 'https://eastus.tts.speech.microsoft.com/cognitiveservices/v1' \
--header 'Ocp-Apim-Subscription-Key: %s' \
--header 'Content-Type: application/ssml+xml' \
--header 'X-Microsoft-OutputFormat: %s' \
--header 'User-Agent: curl' \
--data-raw '%s' \
> '%s'"
  "Curl command to send a request to the Microsoft Azure text-to-speech engine.
The placeholders are: API key, settings, SSML, and destination.")

(defconst tlon-microsoft-azure-voices
  '((:voice "es-US-AlonsoNeural" :language "es" :gender "male")
    (:voice "es-US-PalomaNeural" :language "es" :gender "female"))
  "Preferred Microsoft Azure voices for different languages.
All the voices in this property list are neural and multilingual, and are the
best male and female voices we were able to identify in each language.

A list of available voices may be found here:
<https://github.com/MicrosoftDocs/azure-docs/blob/main/articles/ai-services/speech-service/includes/language-support/tts.md>.")

(defcustom tlon-microsoft-azure-audio-settings
  "audio-16khz-64kbitrate-mono-mp3"
  "Audio settings for the Microsoft Azure text-to-speech service.
Here's a description of the main options:

- `\"audio-24khz-160kbitrate-mono-mp3\"': Offers higher quality due to a higher
  bitrate and sample rate. This means the audio will sound clearer, especially
  for more complex sounds or music. However, the file size will also be larger.

- `\"audio-16khz-64kbitrate-mono-mp3\"': Reduces the bitrate, which will result
  in a smaller file at the cost of lower audio quality. Useful when network
  bandwidth or storage is limited.

- `\"raw-16khz-16bit-mono-pcm\":' Provides raw audio data without compression.
  This is useful if you plan to further process the audio yourself or need
  lossless quality. Note that the files will be significantly larger.

- `\"riff-16khz-16bit-mono-pcm\"': Similar to the RAW format but wrapped in the
  Waveform Audio File Format, which includes headers making it compatible with
  more playback devices and software.

- `\"riff-24khz-16bit-mono-pcm\"': Offers a higher sample rate compared to the
  16kHz versions, which can provide better audio quality at the expense of
  larger file sizes.

For details, see <https://learn.microsoft.com/en-us/azure/ai-services/speech-service/rest-text-to-speech?tabs=streaming#audio-outputs>."
  :type 'string
  :group 'tlon-tts)

(defvar tlon-microsoft-azure-key nil
  "Microsoft Azure subscription key for the text-to-speech service.")

(defconst tlon-microsoft-azure-char-limit (* 9 60 14)
  "Maximum number of characters that Microsoft Azure can process per request.
Microsoft Azure can process up to 10 minutes of audio at a time. This estimate
assumes 14 characters per second, and uses nine minutes.")

;;;;;; Google Cloud

(defconst tlon-google-cloud-request
  "curl -H 'Authorization: Bearer %s' \
-H 'x-goog-user-project: api-project-781899662791' \
-H 'Content-Type: application/json; charset=utf-8' \
--data '%s' 'https://texttospeech.googleapis.com/v1/text:synthesize' | jq -r .audioContent | base64 --decode > '%s'"
  "Curl command to send a request to the Google Cloud text-to-speech engine.
The placeholders are: token, JSON payload and destination.")

(defconst tlon-google-cloud-voices
  '((:voice "en-US-Studio-Q" :language "en" :gender "male")
    (:voice "en-US-Studio-O" :language "en" :gender "female")
    (:voice "es-US-Studio-B" :language "es" :gender "male")
    (:voice "es-US-Neural2-A" :language "es" :gender "female"))
  "Preferred Google Cloud voices for different languages.
The male voice is a \"studio\" voice, the highest quality voice type currently
offered by Google Cloud. Unfortunately, as of 2024-04-12, Google Cloud does not
offer a female studio voice for Spanish, so we use a \"neural\" voice.

A list of available voices may be found here:
<https://cloud.google.com/text-to-speech/docs/voices>.")

(defcustom tlon-google-cloud-audio-settings
  "MP3"
  "Audio settings for the Google Cloud text-to-speech service.
The options are:

- `\"MP3\"': MPEG Audio Layer III (lossy). MP3 encoding is a Beta feature and
  only available in v1p1beta1. See the RecognitionConfig reference documentation
  for details.

- `\"FLAC\"': Free Lossless Audio Codec (lossless) 16-bit or 24-bit required
  for streams LINEAR16 Linear PCM Yes 16-bit linear pulse-code modulation (PCM)
  encoding. The header must contain the sample rate.

- `\"MULAW\"': μ-law (lossy). 8-bit PCM encoding.

- `\"AMR\"': Adaptive Multi-Rate Narrowband (lossy). Sample rate must be 8000 Hz.

- `\"AMR_WB\"': Adaptive Multi-Rate Wideband (lossy). Sample rate must be 16000
  Hz.

- `\"OGG_OPUS\"': Opus encoded audio frames in an Ogg container (lossy). Sample
  rate must be one of 8000 Hz, 12000 Hz, 16000 Hz, 24000 Hz, or 48000 Hz.

- `\"SPEEX_WITH_HEADER_BYTE\"': Speex wideband (lossy). Sample rate must be
  16000 Hz.

- `\"WEBM_OPUS\"': WebM Opus (lossy). Sample rate must be one of 8000 Hz, 12000
  Hz, 16000 Hz, 24000 Hz, or 48000 Hz.

For details, see <https://cloud.google.com/speech-to-text/docs/encoding>."
  :type 'string
  :group 'tlon-tts)

(defvar tlon-google-cloud-key nil
  "Google Cloud subscription key for the text-to-speech service.")

(defconst tlon-google-cloud-char-limit (* 5000 0.9)
  "Maximum number of characters that Google Cloud can process per request.
Google Cloud TTS can process up to 5000 bytes per request. We use a slightly
lower number for safety.

See <https://cloud.google.com/text-to-speech/quotas>.")

;;;;;; Amazon Polly

(defconst tlon-amazon-polly-request
  "aws polly synthesize-speech --output-format %s --voice-id %s --engine neural --text-type ssml --text '%s' --region %s '%s'"
  "AWS command to synthesize speech using Amazon Polly.
The placeholders are: output format, voice ID, SSML, region, and destination
file.")

(defconst tlon-amazon-polly-voices
  '((:voice "Joanna" :language "en" :gender "female")
    (:voice "Matthew" :language "en" :gender "male")
    (:voice "Lupe" :language "es" :gender "female")
    (:voice "Pedro" :language "es" :gender "male"))
  "Preferred Amazon Polly voices for different languages.
Joanna and Matthew are some of the available Polly voices for English.")

(defvar tlon-amazon-polly-region "us-east-1"
  "Default AWS region for Amazon Polly requests.")

(defvar tlon-amazon-polly-output-format "mp3"
  "Default output format for synthesized audio from Amazon Polly.
Asmissible values are `\"ogg_vorbis\"', `\"json\"', `\"mp3\"' and `\"pcm\"'.")

(defconst tlon-amazon-polly-char-limit (* 1500 0.9)
  "Maximum number of characters that Amazon Polly can process per request.
The limit for Amazon Polly is 1500 characters but using a slightly lower number
for safety.")

;;;;;; OpenAI

(defconst tlon-openai-tts-request
  "curl https://api.openai.com/v1/audio/speech \
  -H \"Authorization: Bearer %s\" \
  -H \"Content-Type: application/json\" \
  -d '{
    \"model\": \"tts-1\",
    \"input\": \"%s\",
    \"voice\": \"%s\"
  }' \
  --output %s"
  "Curl command to send a request to the OpenAI text-to-speech engine.
The placeholders are the API key, the text to be synthesized, the voice, and the
file destination.")

(defconst tlon-openai-voices
  '((:voice "echo" :language "es" :gender "male")
    (:voice "nova" :language "es" :gender "female"))
  "Preferred OpenAI voices for different languages.
All the voices in this property list are neural and multilingual, and are the
best male and female voices we were able to identify in each language.

A list of available voices may be found here:
<https://platform.openai.com/docs/guides/text-to-speech>.")

(defvar tlon-openai-key nil
  "OpenAI API key for the text-to-speech service.")

(defconst tlon-openai-char-limit (* 4096 0.9)
  "Maximum number of characters that OpenAI can process per request.
OpenAI can process up to 4096 bytes per request. We use a slightly
lower number for safety.

See <https://help.openai.com/en/articles/8555505-tts-api#h_273e638099>.")

;;;;; Engines

(defconst tlon-tts-engines
  `((:name "Microsoft Azure"
	   :voices-var tlon-microsoft-azure-voices
	   :request-fun tlon-microsoft-azure-make-request
	   :char-limit ,tlon-microsoft-azure-char-limit)
    (:name "Google Cloud"
	   :voices-var tlon-google-cloud-voices
	   :request-fun tlon-google-cloud-make-request
	   :char-limit ,tlon-google-cloud-char-limit)
    (:name "Amazon Polly"
           :voices-var tlon-amazon-polly-voices
           :request-fun tlon-generate-audio-polly
           :char-limit ,tlon-amazon-polly-char-limit)
    (:name "OpenAI"
	   :voices-var tlon-openai-voices
	   :request-fun tlon-openai-make-request
	   :char-limit ,tlon-openai-char-limit)))

;; needs to use double quotes for Azure, but maybe single quotes for Google Cloud?
;; cannot be in single quotes because the entire string is itself enclosed in single quotes
(defconst tlon-ssml-wrapper
  (append
   (mapcar (lambda (service)
             (cons service
                   (format "<speak version=\"1.0\" xmlns=\"http://www.w3.org/2001/10/synthesis\" xml:lang=\"%%s\">%s</speak>"
                           tlon-tts-ssml-voice-replace-pattern)))
           '("Microsoft Azure" "Google Cloud" "Amazon Polly"))
   '(("OpenAI" . "%3$s")))
  "SSML wrapper for the TTS request.")

;;;;; Currencies

(defconst tlon-tts-currencies
  '(("₿" . "BTC")
    ("Ξ" . "ETH")
    ("£" . "GBP")
    ("₪" . "ILS")
    ("₹" . "INR")
    ("¥" . "JPY")
    ("$" . "USD"))
  "Currency symbols and their associated three-letter codes.")

(defconst tlon-tts-currency-ssml
  "<say-as interpret-as=\"currency\">%s %s</say-as>"
  "SSML pattern for currency symbols.
The first placeholder is the currency amount, and the second is the currency
code.

For more information, see <https://learn.microsoft.com/en-us/azure/ai-services/speech-service/speech-synthesis-markup-pronunciation#say-as-element>.")

;;;;; File-local variables

(defvar-local tlon-file-local-abbreviations '()
  "In-text abbreviations and their spoken equivalent.")

(defvar-local tlon-file-local-replacements '()
  "File local replacements.")

;;;;; Abbreviations

(defvar tlon-tts-abbreviations
  (tlon-parse-json tlon-file-abbreviations)
  "Standard abbreviations and their spoken equivalent in each language.")

;;;;; Phonetic replacements

(defvar tlon-tts-phonetic-replacements
  (tlon-parse-json tlon-file-phonetic-replacements)
  "Phonetic replacements for terms.")

;;;;; Phonetic transcriptions

(defvar tlon-tts-phonetic-transcriptions
  (tlon-parse-json tlon-file-phonetic-transcriptions)
  "Phonetic transcriptions for terms.")

;;;;; Listener cues

(defconst tlon-tts-cue-delimiter
  (concat "\n" (format tlon-tts-ssml-break "0.5s"))
  "Delimiter for listener cues.")

;;;;;; Notes

(defconst tlon-tts-note-cues
  '(("en" "A note starts here." . "The note ends here.")
    ("es" "Aquí empieza una nota." . "Aquí termina la nota."))
  "Listener cues for notes.")

;;;;;; Quotes

(defconst tlon-tts-quote-cues
  '(("en" "A quote starts here." . "The quote ends here.")
    ("es" "Aquí empieza una cita." . "Aquí termina la cita."))
  "Listener cues for asides.")

;;;;;; Asides

(defconst tlon-tts-aside-cues
  '(("en" "An aside starts here." . "The aside ends here.")
    ("es" "Aquí empieza un inciso." . "Aquí termina el inciso."))
  "Listener cues for asides.")

;;;;;; Images

(defconst tlon-tts-image-cues
  '(("en" "Here’s an image." . "The image ends here.")
    ("es" "Aquí hay una imagen." ."Aquí termina la imagen."))
  "Listener cues for images.")

;;;; Functions

(defun tlon-tts-get-alternative-voice ()
  "Return the voice in the current language that is not the current voice."
  (let ((voices (tlon-lookup-all tlon-microsoft-azure-voices
				       :voice :language tlon-tts-current-language)))
    (car (delete tlon-tts-main-voice voices))))

;;;;; Narration

(defun tlon-tts-read-content (content &optional chunk-size)
  "Read CONTENT and return it as a string ready for TTS processing.
If CHUNK-SIZE is non-nil, split string into chunks no larger than that size."
  (with-temp-buffer
    (insert content)
    (tlon-tts-prepare-buffer)
    (if chunk-size
	(tlon-break-into-chunks chunk-size)
      (buffer-string))))

(defun tlon-tts-read-file (file)
  "Return substantive content of FILE, handling in-text abbreviations."
  (unless (string= (file-name-extension file) "md")
    (user-error "File `%s' is not a Markdown file" file))
  (let ((tlon-file-local-abbreviations))
    (with-current-buffer (find-file-noselect file)
      ;; to make `tlon-tts-process-file-local-abbreviations' work, we
      ;; let-bound the variable above and now set its value to that of its
      ;; file-local counterpart
      (setq tlon-file-local-abbreviations tlon-file-local-abbreviations)
      (concat (tlon-tts-get-metadata) (tlon-md-read-content file)))))

;;;###autoload
(defun tlon-narrate-content (engine &optional voice file)
  "Narrate content with VOICE using text-to-speech ENGINE.
If region is active, read the region. Otherwise, read FILE.

Save the narration to the `audio' directory in the same repository as FILE or,
if region is active, save it to the downloads directory."
  (interactive (list (completing-read "Engine: " (tlon-lookup-all tlon-tts-engines :name))))
  (let* ((file-or-buffer (or file (buffer-file-name) (buffer-name)))
	 (repo (tlon-get-repo-from-file file-or-buffer))
	 (language (setq tlon-tts-current-language
			 (or (tlon-repo-lookup :language :dir repo)
			     (tlon-select-language 'code 'babel))))
	 (voice (setq tlon-tts-main-voice
		      (or voice (tlon-get-voices engine language))))
	 (content (if (region-active-p)
		      (buffer-substring-no-properties (region-beginning) (region-end))
		    (tlon-tts-read-file file-or-buffer)))
	 (file-name-sans-extension (file-name-sans-extension
				    (file-name-nondirectory file-or-buffer)))
	 ;; FIXME: extension should be set based on audio settings (e.g. `tlon-microsoft-azure-audio-settings')
	 (file-name (file-name-with-extension file-name-sans-extension "mp3"))
	 (destination (if (region-active-p)
			  (file-name-concat paths-dir-downloads file-name)
			(file-name-concat repo "audio" file-name)))
	 (char-limit (tlon-lookup tlon-tts-engines :char-limit :name engine))
	 (chunks (tlon-tts-read-content content char-limit))
	 (nth 1))
    (setq tlon-unprocessed-chunks
	  (tlon-get-chunk-names destination (length chunks)))
    (dolist (chunk chunks)
      (tlon-generate-audio engine chunk voice (tlon-get-chunk-name destination nth))
      (setq nth (1+ nth)))))

(defun tlon-get-voices (engine language)
  "Get available voices in LANGUAGE for TTS ENGINE."
  (let ((voices (tlon-lookup tlon-tts-engines :voices-var :name engine)))
    (completing-read "Voice: " (tlon-lookup-all (symbol-value voices) :voice :language language))))

(defun tlon-tts-get-voice-locale ()
  "Return the locale of the current voice."
  (catch 'found
    (dolist (var (tlon-lookup-all tlon-tts-engines :voices-var))
      (when-let* ((code (tlon-lookup (symbol-value var) :language :voice tlon-tts-main-voice))
		  (locale (tlon-lookup tlon-languages-properties :locale :code code)))
	(throw 'found locale)))))

(defun tlon-generate-audio (engine text voice destination)
  "Generate an audio file from TEXT using ENGINE VOICE and save it to DESTINATION."
  (let* ((locale (tlon-tts-get-voice-locale))
	 (ssml-wrapper (alist-get engine tlon-ssml-wrapper nil nil #'string=))
	 (content (format ssml-wrapper locale voice text))
	 (fun (tlon-lookup tlon-tts-engines :request-fun :name engine))
	 (request (funcall fun content voice locale destination))
	 (process (start-process-shell-command "generate audio" nil request)))
    (set-process-sentinel process
			  (lambda (process event)
			    (when (string= event "finished\n")
			      (if (region-active-p)
				  (shell-command (format "open %s" destination))
				(setq tlon-unprocessed-chunks
				      (remove destination tlon-unprocessed-chunks))
				(unless tlon-unprocessed-chunks
				  (let ((file (tlon-get-original-name destination)))
				    (tlon-join-chunks file)
				    ;; (tlon-delete-chunks file)
				    ))))))))

;;;;;; Microsoft Azure

(defun tlon-microsoft-azure-make-request (content _voice _locale destination)
  "Make a request to the Microsoft Azure text-to-speech service.
CONTENT is the content of the request. DESTINATION is the output file path."
  (format tlon-microsoft-azure-request
	  (tlon-microsoft-azure-get-or-set-key) tlon-microsoft-azure-audio-settings
	  content destination))

(defun tlon-microsoft-azure-get-or-set-key ()
  "Get or set the Microsoft Azure key."
  (or tlon-microsoft-azure-key
      (setq tlon-microsoft-azure-key
	    (auth-source-pass-get "key1" "tlon/core/azure.com/tlon.shared@gmail.com"))))

;;;;;; Google Cloud

(defun tlon-google-cloud-make-request (content voice locale destination)
  "Make a request to the Google Cloud text-to-speech service.
CONTENT is the content of the request. VOICE is voice ID. LOCALE is the
locate. DESTINATION is the output file path"
  (format tlon-google-cloud-request
	  (tlon-google-cloud-get-token)
	  (tlon-google-cloud-format-ssml content voice locale)
	  destination))

(defun tlon-google-cloud-format-ssml (ssml voice locale)
  "Convert SSML string to JSON object for Google Cloud TTS.
VOICE and LOCALE are used to construct the JSON object."
  (let* ((gender (tlon-lookup tlon-google-cloud-voices :gender :voice voice))
	 (payload (json-encode
		   `((input (ssml . ,ssml))
		     (voice (languageCode . ,locale)
			    (name . ,voice)
			    (ssmlGender . ,(upcase gender)))
		     (audioConfig (audioEncoding . ,tlon-google-cloud-audio-settings))))))
    payload))

(defun tlon-google-cloud-get-token ()
  "Get or set the Google Cloud token key."
  (string-trim (shell-command-to-string "gcloud auth print-access-token")))

(defun tlon-google-cloud-get-or-set-key ()
  "Get or set the Google Cloud key."
  (or tlon-google-cloud-key
      (setq tlon-google-cloud-key
	    (auth-source-pass-get "key" "tlon/babel/cloud.google.com/pablo.stafforini@gmail.com"))))

;;;;;; Amazon Polly

(defun tlon-amazon-polly-make-request (content voice _locale destination)
  "Construct the AWS CLI command to call Amazon Polly.
CONTENT is the content of the request. VOICE is the voice ID. DESTINATION is
the output file path."
  (format tlon-amazon-polly-request
          tlon-amazon-polly-output-format voice content tlon-amazon-polly-region destination))

(defun tlon-generate-audio-polly (text voice locale destination)
  "Generate audio file for TEXT with Amazon Polly VOICE and save it to DESTINATION.
LOCALE is the locale of the voice."
  (let* ((ssml (format tlon-ssml-wrapper locale voice text)) ; revise
	 (request (tlon-amazon-polly-make-request ssml voice locale destination))
	 (process (start-process-shell-command "generate audio" nil request)))
    (set-process-sentinel process
			  (lambda (process event)
			    (when (string= event "finished\n")
			      (message "Audio generated at: %s" destination))))))

;;;;;; OpenAI

(defun tlon-openai-make-request (ssml voice _locale destination)
  "Make a request to the OpenAI text-to-speech service.
SSML is the content of the request. VOICE is the TTS voice. DESTINATION is the
file to save the audio to."
  (format tlon-openai-tts-request
	  (tlon-openai-get-or-set-key) ssml voice destination))

(defun tlon-openai-get-or-set-key ()
  "Get or set the Microsoft Azure key."
  (or tlon-openai-key
      (setq tlon-openai-key
	    (auth-source-pass-get "key" (concat "tlon/core/openai.com/" tlon-email-shared)))))

;;;;;; Chunk processing

(defun tlon-break-into-chunks (chunk-size)
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

(defun tlon-join-chunks (file)
  "Join chunks of FILE back into a single file."
  (let* ((files (tlon-get-list-of-chunks file))
	 (list-of-files (tlon-create-list-of-chunks files)))
    (shell-command (format "ffmpeg -y -f concat -safe 0 -i %s -c copy %s"
			   list-of-files file))))

(defun tlon-get-list-of-chunks (file)
  "Return a list of the file chunks for FILE."
  (let ((nth 1)
	file-chunk
	files)
    (while (file-exists-p (setq file-chunk (tlon-get-chunk-name file nth)))
      (push file-chunk files)
      (setq nth (1+ nth)))
    (nreverse files)))

(defun tlon-delete-chunks (file)
  "Delete the chunks of FILE."
  (dolist (file (tlon-get-list-of-chunks file))
    (delete-file file)))

(defun tlon-create-list-of-chunks (files)
  "Create a temporary file with a list of audio FILES for use with `ffmpeg'."
  (let ((temp-file-list (make-temp-file "files" nil ".txt")))
    (with-temp-file temp-file-list
      (dolist (file files)
	(insert (format "file '%s'\n" (expand-file-name file)))))
    temp-file-list))

(defun tlon-get-chunk-name (file nth)
  "Return the name of the NTH chunk of FILE."
  (let ((extension (file-name-extension file))
	(file-name-sans-extension (file-name-sans-extension file)))
    (format "%s-%03d.%s" file-name-sans-extension nth extension)))

(defun tlon-get-chunk-names (file n)
  "Return a list of the first N chunk names of FILE."
  (let ((names '()))
    (dotimes (i n names)
      (push (tlon-get-chunk-name file (1+ i)) names))))

(defun tlon-get-original-name (chunk-name)
  "Return the original file name before it was chunked, given CHUNK-NAME."
  (let* ((base-name (file-name-sans-extension chunk-name))
	 (extension (file-name-extension chunk-name))
	 (original-base-name (replace-regexp-in-string "-[0-9]+\\'" "" base-name)))
    (format "%s.%s" original-base-name extension)))

;;;;; Metadata

;; Should also include summary?
;; Perhaps should be narrated by alternative voice?
(defun tlon-tts-get-metadata ()
  "Add title and author."
  (let* ((metadata (tlon-yaml-format-values-of-alist (tlon-yaml-get-metadata)))
	 (title (alist-get "title" metadata nil nil #'string=))
	 (authors (alist-get "authors" metadata nil nil #'string=))
	 (author-string (tlon-concatenate-list authors)))
    (format "%s.\n\nPor %s.\n\n" title author-string)))

;;;;; Cleanup

(defun tlon-tts-prepare-buffer ()
  "Prepare the current buffer for audio narration."
  (save-excursion
    (tlon-tts-process-notes)
    (tlon-tts-process-citations)
    (tlon-tts-process-formatting)
    (tlon-tts-process-headings)
    ;; replace small caps
    ;; check all other elements in markdown-menu
    (tlon-tts-process-file-local-abbreviations)
    (tlon-tts-process-file-local-replacements)
    (tlon-tts-process-abbreviations)
    (tlon-tts-process-phonetic-replacements)
    ;; (tlon-tts-process-phonetic-transcriptions) ; not currently supported
    (tlon-tts-process-alternative-voice)
    (tlon-tts-process-asides)
    (tlon-tts-process-quotes)
    (tlon-tts-process-images) ; should be before links
    (tlon-tts-process-links)
    (tlon-tts-process-numbers)
    (tlon-tts-process-currencies)
    (tlon-tts-process-math-expressions))
  (goto-char (point-min)))

;; TODO: it seems Microsoft Azure is not making an extra pause between paragraphs; decide whether to add some extra silence

;;;;;; Notes

(defun tlon-tts-process-notes ()
  "Replace note reference with its content, if it is a sidenote, else delete it.
Move the note to the end of the sentence if necessary.

Note: the function assumes that the citation is in MDX, rather than Pandoc
citation key, format. Hence, it must be run *before*
`tlon-tts-process-citations'."
  (goto-char (point-min))
  (while (re-search-forward markdown-regex-footnote nil t)
    (let (reposition)
      (markdown-footnote-kill)
      (unless (looking-back (concat "\\.\\|" markdown-regex-footnote) (line-beginning-position))
	(setq reposition t))
      (when (eq (tlon-get-note-type (current-kill 0)) 'sidenote)
	(when reposition
	  (forward-sentence))
	(insert (tlon-tts-handle-note (string-trim (current-kill 0))))))))

(defun tlon-tts-handle-note (note)
  "Handle NOTE for audio narration."
  (let ((clean-note (replace-regexp-in-string tlon-sidenote-marker "" note)))
    (tlon-tts-listener-cue-full-enclose tlon-tts-note-cues clean-note)))

;;;;;; Citations

(defun tlon-tts-process-citations ()
  "Replace our custom MDX cite tags with a pandoc-style citation.
For example `<Cite bibKey={\"Clark2015SonAlsoRises\"} />' will be replaced with
`[@Clark2015SonAlsoRises]'."
  (goto-char (point-min))
  (while (re-search-forward tlon-cite-pattern nil t)
    (replace-match (format "[@%s]"(match-string 1)) nil nil)))

;;;;;; Showing and hiding

;; TODO: I think we should just use `tlon-tts-remove-formatting' for this,
;; together with the patterns defined in `tlon-md'

;;;;;; Formatting

(defun tlon-tts-process-formatting ()
  "Remove formatting from text."
  (tlon-tts-process-boldface)
  (tlon-tts-process-italics)
  (tlon-tts-process-visually-hidden)
  (tlon-tts-process-visually-shown))

(defun tlon-tts-remove-formatting (type)
  "Remove formatting TYPE from text."
  (cl-destructuring-bind (pattern . group)
      (pcase type
	('boldface (cons markdown-regex-bold 4))
	('italics (cons markdown-regex-italic 3))
	('visually-hidden (cons tlon-mdx-visually-hidden-search-pattern 2))
	('visually-shown (cons tlon-mdx-visually-shown-search-pattern nil))
	(_ (user-error "Invalid formatting type: %s" type)))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (let ((replacement (if group (format " %s" (match-string group)) "")))
	(replace-match replacement t nil)))))

(defun tlon-tts-process-boldface ()
  "Remove boldface from text."
  (tlon-tts-remove-formatting 'boldface))

(defun tlon-tts-process-italics ()
  "Remove italics from text."
  (tlon-tts-remove-formatting 'italics))

(defun tlon-tts-process-visually-hidden ()
  "Remove `VisuallyHidden' MDX tag."
  (tlon-tts-remove-formatting 'visually-hidden))

(defun tlon-tts-process-visually-shown ()
  "Remove `VisuallyShown' MDX tag."
  (tlon-tts-remove-formatting 'visually-shown))

;;;;;; Headings

(defun tlon-tts-process-headings ()
  "Remove heading markers from headings."
  (let ((insert-pause (format tlon-tts-ssml-break "1s")))
    (goto-char (point-min))
    (while (re-search-forward markdown-regex-header nil t)
      (replace-match (format "%s %s" insert-pause (match-string 5))))))

;;;;;; File-local abbreviations

(defun tlon-tts-process-file-local-abbreviations ()
  "Replace file-local abbreviations with their spoken equivalent.
In-text abbreviations are those that are introduced in the text itself,
typically in parenthesis after the first occurrence of the phrase they
abbreviate. We store these abbreviations on a per file basis, in the file-local
variable `tlon-file-local-abbreviations'"
  (let ((case-fold-search nil))
    (dolist (entry tlon-file-local-abbreviations)
      (cl-destructuring-bind (abbrev . expansion) entry
	(let ((abbrev-introduced (format "%s (%s)" expansion abbrev)))
	  ;; we first replace the full abbrev introduction, then the abbrev itself
	  (dolist (cons (list (cons abbrev-introduced expansion) entry))
	    (goto-char (point-min))
	    (while (re-search-forward (car cons) nil t)
	      (replace-match (cdr cons) t nil))))))))

;;;;;; File-local replacements

(defun tlon-tts-process-file-local-replacements ()
  "Perform replacements indicated in `tlon-file-local-replacements'."
  (dolist (pair tlon-file-local-replacements)
    (let ((find (car pair)))
      (goto-char (point-min))
      (while (re-search-forward find nil t)
	(replace-match (cdr pair) t)))))

;;;;;; Project-wide replacements

;;;;;;; Common

(defun tlon-tts-process-terms (terms replacement-fun)
  "Replace TERMS using REPLACEMENT-FUN."
  (let ((case-fold-search nil))
    (dolist (term terms)
      (let ((find (format "\\b%s\\b" (car term)))
	    (replacement (cdr term)))
	(goto-char (point-min))
	(while (re-search-forward find nil t)
	  (funcall replacement-fun replacement))))))

(defun tlon-tts-get-associated-terms (var)
  "Get associated terms for the current language in VAR.
For each cons cell in VAR for the language in the current text-to-speech
process, return its cdr."
  (let ((result '()))
    (dolist (term var result)
      (when (member tlon-tts-current-language (car term))
	(setq result (append result (cadr term)))))
    result))

;;;;;;; Abbreviations

(defun tlon-tts-process-abbreviations ()
  "Replace terms with their pronunciations."
  (tlon-tts-process-terms
   (tlon-get-abbreviations)
   'tlon-replace-abbreviations))

(defun tlon-replace-abbreviations (replacement)
  "When processing abbreviations, replace match with REPLACEMENT."
  (replace-match replacement t))

(defun tlon-get-abbreviations ()
  "Get abbreviations."
  (tlon-tts-get-associated-terms tlon-tts-abbreviations))

;;;;;;; Phonetic replacements

(defun tlon-tts-process-phonetic-replacements ()
  "Replace terms with their counterparts."
  (tlon-tts-process-terms
   (tlon-get-phonetic-replacements)
   'tlon-replace-phonetic-replacements))

(defun tlon-replace-phonetic-replacements (replacement)
  "When processing simple replacements, replace match with REPLACEMENT."
  (replace-match replacement t))

(defun tlon-get-phonetic-replacements ()
  "Get simple replacements."
  (tlon-tts-get-associated-terms tlon-tts-phonetic-replacements))

;;;;;;; Phonetic transcriptions

(defun tlon-tts-process-phonetic-transcriptions ()
  "Replace terms with their pronunciations."
  (tlon-tts-process-terms
   (tlon-get-phonetic-transcriptions)
   'tlon-replace-phonetic-transcriptions))

(defun tlon-replace-phonetic-transcriptions (replacement)
  "When processing phonetic transcriptions, replace match with pattern.
REPLACEMENT is the cdr of the cons cell for the term being replaced."
  (replace-match (format tlon-tts-ssml-phoneme-replace-pattern
			 "ipa" replacement (match-string-no-properties 0)) t))

(defun tlon-get-phonetic-transcriptions ()
  "Get the phonetic transcriptions."
  (tlon-tts-get-associated-terms tlon-tts-phonetic-transcriptions))

;;;;;; Listener cues

;;;;;;; General functions

(defun tlon-tts-enclose-in-listener-cues (type text)
  "Enclose TEXT in listener cues of TYPE."
  (cl-destructuring-bind (cue-begins . cue-ends)
      (alist-get tlon-tts-current-language type nil nil #'string=)
    (format "%s %s %s" cue-begins text cue-ends)))

(defun tlon-tts-enclose-in-voice-tag (string &optional voice)
  "Enclose STRING in `voice' SSML tags.
If VOICE is nil, default to the alternative voice.

Note that this function inserts two pairs of `voice' tags: the inner pair to set
the voice for the string that it encloses, and an outer pair of tags in reverse
order, to close the opening `voice' tag that wraps the entire document, and to
then reopen it."
  (let ((voice (or voice (tlon-tts-get-alternative-voice))))
    (format tlon-tts-ssml-double-voice-replace-pattern voice string tlon-tts-main-voice)))

(defun tlon-tts-enclose-in-cue-delimiter (string)
  "Enclose STRING in listener cue delimiter."
  (format "%1$s%s%1$s" tlon-tts-cue-delimiter string))

(defun tlon-tts-listener-cue-full-enclose (type text)
  "Enclose TEXT in listener cue of TYPE, and in turn in `voice' SSML tags."
  (tlon-tts-enclose-in-cue-delimiter
   (tlon-tts-enclose-in-voice-tag
    (tlon-tts-enclose-in-listener-cues type text))))

(defun tlon-tts-process-element (type)
  "Add listener cues for text enclosed in tags of TYPE."
  (cl-destructuring-bind (pattern cues group)
      (pcase type
	('aside (list tlon-mdx-aside-search-pattern tlon-tts-aside-cues 2))
	('quote (list markdown-regex-blockquote tlon-tts-quote-cues 3))
	('image (list markdown-regex-link-inline tlon-tts-image-cues 3))
	;; TODO: determine if other types should be added
	(_ (user-error "Invalid formatting type: %s" type)))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match
       (tlon-tts-listener-cue-full-enclose cues (match-string-no-properties group))))))

;;;;;;; Specific elements

(defun tlon-tts-process-quotes ()
  "Add listener cues for blockquotes."
  (tlon-tts-process-element 'quote))

(defun tlon-tts-process-asides ()
  "Add listener cues for asides."
  (tlon-tts-process-element 'aside))

(defun tlon-tts-process-images ()
  "Add listener cues for images."
  (tlon-tts-process-element 'image))

;;;;;; Links

(defun tlon-tts-process-links ()
  "Replace links with their text.
Note: this function should be run after `tlon-tts-process-images' because
image links are handled differently."
  (goto-char (point-min))
  (while (re-search-forward markdown-regex-link-inline nil t)
    (replace-match (match-string 3))))

;;;;;; Numbers

(defun tlon-tts-process-numbers ()
  "Remove number separators."
  (let* ((separator (alist-get tlon-tts-current-language
			       tlon-md-number-separators nil nil #'string=)))
    (goto-char (point-min))
    (while (re-search-forward (format tlon-md-number-separator-pattern separator) nil t)
      (replace-match "\\1\\3"))))

;;;;;; Currencies

(defun tlon-tts-process-currencies ()
  "Format currency with appropriate SSML tags."
  (dolist (cons tlon-tts-currencies)
    (let ((symbol (car cons))
	  (code (cdr cons)))
      (goto-char (point-min))
      (while (re-search-forward (format "%s.?\\([0-9,.]+\\)\\b" (regexp-quote symbol)) nil t)
	(replace-match (format tlon-tts-currency-ssml
			       (match-string-no-properties 1) code))))))

;;;;;; Math expressions

(declare-function tlon-ai-translate-math "tlon-ai")
(defun tlon-tts-process-math-expressions ()
  "Replace math expressions with their spoken equivalent."
  (dolist (pattern (list tlon-math-inline-search-pattern
			 tlon-math-display-search-pattern))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (let ((math (match-string-no-properties 2)))
	(replace-match "" nil nil)
	(tlon-ai-translate-math math tlon-tts-current-language)))))

;;;;; Alternative voice

(defun tlon-tts-process-alternative-voice ()
  "Replace the `AlternativeVoice' tag with an SSML `voice' tag.
The `voice' tag is set to the alternative voice for the current language."
  (goto-char (point-min))
  (while (re-search-forward tlon-mdx-alternative-voice-search-pattern nil t)
    (replace-match (tlon-tts-enclose-in-voice-tag (match-string 2)) t)))

;;;;; Project-wide

;;;;;; Common

(defun tlon-tts-edit-entry (variable file)
  "Add or revise an entry in VARIABLE and write it to FILE."
  (interactive)
  (set variable (tlon-parse-json file))
  (let* ((names (mapcan (lambda (group)
			  (mapcar #'car (cadr group)))
			(symbol-value variable)))
	 (term (completing-read "Term: " names nil nil))
	 (current-entry (catch 'current-entry
			  (dolist (group (symbol-value variable))
			    (dolist (pair (cadr group))
			      (when (string= (car pair) term)
				(throw 'current-entry (cdr pair)))))
			  nil)))
    (if current-entry
	(let ((cdr (read-string (format "Updated entry for %s: " term) current-entry)))
	  (tlon-tts-revise-entry (symbol-value variable) term cdr))
      (tlon-tts-add-entry (symbol-value variable) term))
    (tlon-write-data file (symbol-value variable))))

(defun tlon-tts-revise-entry (data term cdr)
  "Update the CDR for an existing TERM in DATA."
  (dolist (group data)
    (dolist (pair (cadr group))
      (when (string= (car pair) term)
	(setcdr pair cdr)))))

(defun tlon-tts-add-entry (data term)
  "Add a new TERM to DATA."
  (let* ((languages (tlon-select-language 'code 'babel 'multiple))
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
(defun tlon-edit-abbreviations ()
  "Edit abbreviations."
  (interactive)
  (tlon-tts-edit-entry
   'tlon-tts-abbreviations
   tlon-file-abbreviations))

;;;;;; Phonetic replacements

;;;###autoload
(defun tlon-edit-phonetic-replacements ()
  "Edit phonetic replacements."
  (interactive)
  (tlon-tts-edit-entry
   'tlon-tts-phonetic-replacements
   tlon-file-phonetic-replacements))

;;;;;; Phonetic transcriptions

;;;###autoload
(defun tlon-edit-phonetic-transcriptions ()
  "Edit phonetic transcriptions."
  (interactive)
  (tlon-tts-edit-entry
   'tlon-tts-phonetic-transcriptions
   tlon-file-phonetic-transcriptions))

;;;;; File-local

;;;;;; Common

(declare-function modify-file-local-variable "files-x")
(defun tlon-add-in-text-cons-cell (prompts var)
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
(defun tlon-add-file-local-abbreviation ()
  "Add an in-text abbreviation to the file-local list."
  (interactive)
  (tlon-add-in-text-cons-cell '("Abbrev: " . "Expanded abbrev: ")
				    'tlon-file-local-abbreviations))

;;;;;; Replacements

;;;###autoload
(defun tlon-add-file-local-replacement ()
  "Add an in-text replacement to the file-local list."
  (interactive)
  (tlon-add-in-text-cons-cell '("Text to replace: " . "Replacement: ")
				    'tlon-file-local-replacements))

(provide 'tlon-tts)
;;; tlon-tts.el ends here
