;;; tlon-tts.el --- Text-to-speech functionality -*- lexical-binding: t -*-

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

;; Text-to-speech functionality.

;;; Code:

(require 'tlon-core)
(require 'tlon-md)
(eval-and-compile
  (require 'eieio)
  (require 'transient))

;;;; User options

(defgroup tlon-tts ()
  "Text-to-speech functionality."
  :group 'tlon)

;;;;; Common

(defcustom tlon-tts-global-engine "ElevenLabs"
  "The TTS engine to use when creating the staging buffer."
  :group 'tlon-tts
  :type '(choice (const :tag "Microsoft Azure" :azure)
		 (const :tag "Google Cloud" :google)
		 (const :tag "Amazon Polly" :polly)
		 (const :tag "OpenAI" :openai)
		 (const :tag "ElevenLabs" :elevenlabs)))

(defcustom tlon-tts-use-alternate-voice nil
  "Whether to use an alternate voice for reading notes, asides, etc."
  :group 'tlon-tts
  :type 'boolean)

(defcustom tlon-tts-delete-file-chunks nil
  "Whether to delete file chunks after they have been merged into the main file."
  :group 'tlon-tts
  :type 'boolean)

(defcustom tlon-tts-narrate-sidenotes nil
  "Whether to narrate sidenotes.
If non-nil, the content of sidenotes will be read aloud, typically repositioned
to the end of the sentence containing the reference. If nil (the default),
sidenotes will be removed like footnotes."
  :group 'tlon-tts
  :type 'boolean)

(defcustom tlon-tts-generate-missing-chunks-only t
  "Whether `tlon-tts-narrate-staged-buffer' should only process missing chunks.
If non-nil (the default), narration will start from the first chunk whose audio
file does not exist. If nil, all chunks will be processed, potentially
overwriting existing audio files. This option does *not* affect
`tlon-tts-narrate-staged-chunks', which always attempts to generate the selected
chunks."
  :group 'tlon-tts
  :type 'boolean)

(defcustom tlon-tts-normalize-audio nil
  "Whether to normalize audio chunks during final processing.
If non-nil, an ffmpeg loudness normalization filter will be applied to each
chunk before joining. If nil (the default), this step is skipped."
  :group 'tlon-tts
  :type 'boolean)

;; TODO: it looks like this is not being used; decide what to do about it
(defcustom tlon-tts-prompt
  nil
  "Generic prompt to use in the TTS request.
Selection candidates for each language are listed in `tlon-tts-prompts'."
  :group 'tlon-tts
  :type 'string)

(defconst tlon-tts-prompts
  '(("ar" . ("أنت متحدث باللغة العربية الأصلية. تتحدث بلهجة عربية محايدة."))
    ("de" . ("Sie sind ein Muttersprachler des Deutschen. Sie sprechen mit einem neutralen deutschen Akzent."))
    ("en" . ("You are a native English speaker. You speak with a neutral English accent."))
    ("es" . ("Eres un hablante nativo de español. Hablas con un acento español neutro."))
    ("fr" . ("Vous êtes un locuteur natif du français. Vous parlez avec un accent français neutre."))
    ("it" . ("Sei un madrelingua italiano. Parli con un accento italiano neutro."))
    ("ja" . ("あなたは日本語のネイティブスピーカーです。中立的な日本語アクセントで話します。"))
    ("ko" . ("당신은 한국어 원어민입니다. 중립적인 한국어 악센트로 말합니다."))
    ("pt" . ("Você é um falante nativo de português. Você fala com um sotaque português neutro."))
    ("tr" . ("Türkçe ana dili konuşucususunuz. Nötr bir Türkçe aksanla konuşuyorsunuz.")))
  "List of prompts to select from in each language.")

;;;;;; `break'

;; Note that, apparently, ElevenLabs *replaces* the pause that the narrator
;; would make without an explicit `break' tag with the duration specified in the
;; tag. This is different from the behavior of other engines, which *add* the
;; duration specified in the tag to the default pause duration.

(defcustom tlon-tts-paragraph-break-duration "0.8s"
  "Duration of the break after a paragraph."
  :group 'tlon-tts
  :type 'string)

(defcustom tlon-tts-listener-cue-break-duration "0.5s"
  "Duration of the break for a listener cue."
  :group 'tlon-tts
  :type 'string)

;;;;; Microsoft Azure

(defcustom tlon-microsoft-azure-audio-settings
  '("audio-24khz-160kbitrate-mono-mp3" . "mp3")
  "Output format and associated extension for the Microsoft Azure TTS service.
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

For a full list of audio outputs, see
<https://learn.microsoft.com/en-us/azure/ai-services/speech-service/rest-text-to-speech?tabs=streaming#audio-outputs>."
  :group 'tlon-tts
  :type '(cons (string :tag "Name") (string :tag "Extension")))

(defconst tlon-microsoft-azure-audio-choices
  '(("amr-wb-16000hz")
    ("audio-16khz-128kbitrate-mono-mp3" . "mp3")
    ("audio-16khz-16bit-32kbps-mono-opus" . "opus")
    ("audio-16khz-32kbitrate-mono-mp3" . "mp3")
    ("audio-16khz-64kbitrate-mono-mp3" . "mp3")
    ("audio-24khz-160kbitrate-mono-mp3" . "mp3")
    ("audio-24khz-16bit-24kbps-mono-opus" . "opus")
    ("audio-24khz-16bit-48kbps-mono-opus" . "opus")
    ("audio-24khz-48kbitrate-mono-mp3" . "mp3")
    ("audio-24khz-96kbitrate-mono-mp3" . "mp3")
    ("audio-48khz-192kbitrate-mono-mp3" . "mp3")
    ("audio-48khz-96kbitrate-mono-mp3" . "mp3")
    ("ogg-16khz-16bit-mono-opus" . "opus")
    ("ogg-24khz-16bit-mono-opus" . "opus")
    ("ogg-48khz-16bit-mono-opus" . "opus")
    ("raw-16khz-16bit-mono-pcm" . "pcm")
    ("raw-16khz-16bit-mono-truesilk" . "sil")
    ("raw-22050hz-16bit-mono-pcm" . "pcm")
    ("raw-24khz-16bit-mono-pcm" . "pcm")
    ("raw-24khz-16bit-mono-truesilk" . "sil")
    ("raw-44100hz-16bit-mono-pcm" . "pcm")
    ("raw-48khz-16bit-mono-pcm" . "pcm")
    ("raw-8khz-16bit-mono-pcm" . "pcm")
    ("raw-8khz-8bit-mono-alaw" . "alaw")
    ("raw-8khz-8bit-mono-mulaw" . "mulaw")
    ("webm-16khz-16bit-mono-opus" . "opus")
    ("webm-24khz-16bit-24kbps-mono-opus" . "opus")
    ("webm-24khz-16bit-mono-opus" . "opus"))
  "Output format and associated extension for the Microsoft Azure TTS service.")

;;;;; Google Cloud

(defcustom tlon-google-cloud-audio-settings
  '("MP3" . "mp3")
  "Output format and associated extension the Google Cloud TTS service.
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
  :group 'tlon-tts
  :type '(cons (string :tag "Name") (string :tag "Extension")))

(defconst tlon-google-cloud-audio-choices
  '(("AMR" . "amr")
    ("AMR_WB" . "amr_wb")
    ("FLAC" . "flac")
    ("MP3" . "mp3")
    ("MULAW" . "mulaw")
    ("OGG_OPUS" . "ogg")
    ("SPEEX_WITH_HEADER_BYTE" . "speex")
    ("WEBM_OPUS" . "webm"))
  "Output format and associated extension for the Google Cloud TTS service.")

;;;;; Amazon Polly

(defcustom tlon-amazon-polly-audio-settings
  '("mp3" . "mp3")
  "Output format and associated extension for the Amazon Polly TTS service.
Admissible values are `\"ogg_vorbis\"', `\"pcm\"' and `\"mp3\"'."
  :group 'tlon-tts
  :type '(cons (string :tag "Name") (string :tag "Extension")))

(defconst tlon-amazon-polly-audio-choices
  '(("mp3" . "mp3")
    ("ogg_vorbis" . "ogg")
    ("pcm" . "pcm"))
  "Output format and associated extension for the Amazon Polly TTS service.")

;;;;; OpenAI

(defcustom tlon-openai-audio-settings
  '("wav" . "wav")
  "Output format and associated extension for the OpenAI TTS service."
  :group 'tlon-tts
  :type '(cons (string :tag "Name") (string :tag "Extension")))

(defconst tlon-openai-audio-choices
  '(("mp3" . "mp3")
    ("opus" . "opus")
    ("wav" . "wav"))
  "Output format and associated extension for the OpenAI TTS service.")

(defcustom tlon-openai-model
  "gpt-4o-mini-tts"
  "Model to use for the OpenAI TTS.
Options are

- `\"gpt-4o-mini-tts\"': Latest model.

- `\"tts-1\"': Standard model. Provides the lowest latency.

- `\"tts-1-hd\"': Higher quality model.

<https://platform.openai.com/docs/guides/text-to-speech/audio-quality>"
  :group 'tlon-tts
  :type'string)

;;;;; ElevenLabs

(defcustom tlon-elevenlabs-audio-settings
  '("mp3_44100_192" . "mp3")
  "Output format and associated extension for the ElevenLabs TTS service.
The options are:

- `\"mp3_44100_32\"': mp3 with 44.1kHz sample rate at 32kbps.

- `\"mp3_44100_64\"': mp3 with 44.1kHz sample rate at 64kbps.

- `\"mp3_44100_96\"': mp3 with 44.1kHz sample rate at 96kbps.

- `\"mp3_44100_128\"': mp3 with 44.1kHz sample rate at 128kbps.

- `\"mp3_44100_192\"': mp3 with 44.1kHz sample rate at 192kbps. Requires you at
  least a Creator tier subscription.

- `\"pcm_16000\"': PCM format (S16LE) with 16kHz sample rate.

- `\"pcm_22050\"': PCM format (S16LE) with 22.05kHz sample rate.

- `\"pcm_24000\"': PCM format (S16LE) with 24kHz sample rate.

- `\"pcm_44100\"': PCM format (S16LE) with 44.1kHz sample rate. Requires you to
  be subscribed to Pro tier or above.

- `\"ulaw_8000\"': μ-law format (sometimes written mu-law, often approximated as
  u-law) with 8kHz sample rate. Note that this format is commonly used for
  Twilio audio inputs."
  :group 'tlon-tts
  :type '(cons (string :tag "Name") (string :tag "Extension")))

(defconst tlon-elevenlabs-audio-choices
  '(("mp3_44100_128" . "mp3")
    ("mp3_44100_192" . "mp3")
    ("mp3_44100_32" . "mp3")
    ("mp3_44100_64" . "mp3")
    ("mp3_44100_96" . "mp3")
    ("pcm_16000" . "pcm")
    ("pcm_22050" . "pcm")
    ("pcm_24000" . "pcm")
    ("pcm_44100" . "pcm")
    ("ulaw_8000" . "ulaw")))

(defcustom tlon-elevenlabs-model
  "eleven_multilingual_v2"
  "Model to use for the ElevenLabs TTS.
Options are

- `\"eleven_monolingual_v1\"': \"Our very first model, English v1, set the
  foundation for what's to come. This model was created specifically for English
  and is the smallest and fastest model we offer. Trained on a focused,
  English-only dataset, it quickly became the go-to choice for English-based
  tasks. As our oldest model, it has undergone extensive optimization to ensure
  reliable performance but it is also the most limited and generally the least
  accurate.\"

- `\"eleven_multilingual_v1\"': \"Taking a step towards global access and usage,
  we introduced Multilingual v1 as our second offering. Has been an experimental
  model ever since release. To this day, it still remains in the experimental
  phase. However, it paved the way for the future as we took what we learned to
  improve the next iteration. Multilingual v1 currently supports a range of
  languages.\"

- `\"eleven_multilingual_v2\"': \"Introducing our latest model, Multilingual v2,
  which stands as a testament to our dedication to progress. This model is a
  powerhouse, excelling in stability, language diversity, and accuracy in
  replicating accents and voices. Its speed and agility are remarkable
  considering its size.\"

- `\"eleven_turbo_v2\"': \"Using cutting-edge technology, this is a highly
  optimized model for real-time applications that require very low latency, but
  it still retains the fantastic quality offered in our other models. Even if
  optimized for real-time and more conversational applications, we still
  recommend testing it out for other applications as it is very versatile and
  stable.\ As of 2024-07-15, it does not support multilingual voices.\"

- `\"eleven_turbo_v2_5\"': \"Turbo v2.5 generates human-like text to speech in
  32 languages with low latency. We recommend Turbo v2.5 for users building real
  time, conversational interfaces in non-English languages. It’s 300% faster
  than Multilingual v2 and adds Vietnamese, Hungarian and Swedish to our
  existing 29 languages. A highly optimized model, specifically tailored for
  low-latency applications without sacrificing vocal performance and keeping
  inline with the quality standard that people have come to expect from our
  models.Because of its very optimized nature, it does have slightly lower
  accuracy than multilingual V2 and is missing the style slider, which adds
  latency when used. However, the accuracy is still very good when using a
  properly created instant voice clone, and it is very stable.\"

<https://help.elevenlabs.io/hc/en-us/articles/17883183930129-What-models-do-you-offer-and-what-is-the-difference-between-them>"
  :group 'tlon-tts
  :type 'string)

;;;; Variables

;;;;; Paths

(defconst tlon-dir-tts
  (file-name-concat (tlon-repo-lookup :dir :name "babel-core") "tts/")
  "Directory for files related to text-to-speech functionality.")

(defconst tlon-file-global-abbreviations
  (file-name-concat tlon-dir-tts "abbreviations.json")
  "File with abbreviations.")

(defconst tlon-file-global-phonetic-replacements
  (file-name-concat tlon-dir-tts "phonetic-replacements.json")
  "File with replacements.")

(defconst tlon-file-global-phonetic-transcriptions
  (file-name-concat tlon-dir-tts "phonetic-transcriptions.json")
  "File with phonetic transcriptions.")

;;;;; Staging buffer

(defconst tlon-tts-staging-buffer-formatter
  "%1$sTTS: %s%1$s"
  "Formatter for the name of the staging buffer for TTS processes.
The first placeholder is for the asterisks enclosing the buffer name, which may
or may not need to be escaped. The second placeholder is for the base of the
file name.")

(defconst tlon-tts-chunk-comment-regex
  "^<!-- Chunk \\([0-9]+\\) -->"
  "Regular expression to match chunk comments in the staging buffer.")

;;;;;; Local variables

(defvar tlon-tts-source)
(defvar tlon-tts-language)
(defvar tlon-tts-engine)
(defvar tlon-tts-audio)
(defvar tlon-tts-voice)
(defvar tlon-tts-voice-id)
(defvar tlon-tts-locale)

(defconst tlon-tts-local-variables-section-start
  "^<!-- Local Variables: -->"
  "Pattern to match the beginning of the local variables section.")

;;;;; SSML tag pairs & patterns

;;;;;; `voice'

(defconst tlon-tts-ssml-double-voice-replace-pattern
  (concat (cdr (tlon-md-format-tag "voice" nil 'get-placeholders))
	  (tlon-md-get-tag-to-fill "voice")
	  (car (tlon-md-format-tag "voice" nil 'get-placeholders)))
  "SSML pattern for voice tag, with 2 voice name placeholders and text placeholder.")

;;;;;; common

(defconst tlon-tts-supported-tags
  `((:tag break
	  :tlon t
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :elevenlabs t
	  :if-unsupported remove
	  :replacement (,(tlon-md-get-tag-pattern "break")))
    (:tag emphasis
	  :tlon t
	  :polly nil
	  :azure nil ; https://bit.ly/azure-ssml-emphasis
	  :google t
	  :openai nil
	  :elevenlabs nil ; content is read, but tag is ignored
	  :if-unsupported remove
	  :replacement ,(tlon-md-get-tag-pattern "emphasis"))
    (:tag lang
	  :tlon t
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :elevenlabs nil ; content is read, but tag is ignored
	  :if-unsupported remove
	  :replacement ,(tlon-md-get-tag-pattern "lang"))
    (:tag mark
	  :tlon nil
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :if-unsupported remove)
    (:tag p
	  :tlon nil
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :if-unsupported remove)
    (:tag phoneme
	  :tlon t
	  :polly t
	  :azure nil ; https://bit.ly/azure-ssml-phoneme
	  :google t
	  :openai nil
	  :elevenlabs nil
	  :if-unsupported remove
	  ;; it works with v2 turbo but not with v2 multilingual, and turbo is
	  ;; not currently multilingual
	  ;; <https://elevenlabs.io/docs/speech-synthesis/prompting#pronunciation>
	  :replacement ,(tlon-md-get-tag-pattern "phoneme")
	  ;; TODO: ideally it should be replaced by a mapping from IPA to
	  ;; closest alphabetical equivalent
	  )
    (:tag prosody
	  :tlon nil
	  :polly t ; partial support
	  :azure t
	  :google t
	  :openai nil
	  :if-unsupported remove)
    (:tag s
	  :tlon nil
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :if-unsupported remove)
    (:tag say-as
	  :tlon t
	  :polly t ; partial support
	  :azure t
	  :google t
	  :openai nil
	  :elevenlabs nil ; content is sometimes read, sometimes not read
	  :if-unsupported remove)
    (:tag speak
	  :tlon t
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :elevenlabs t ; I assume so?
	  :if-unsupported remove
	  ;; :replacement ; Do we need a pattern for this tag, given it's only used in the wrapper?
	  )
    (:tag sub
	  :tlon nil
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :if-unsupported remove)
    (:tag voice
	  :tlon t
	  :polly nil
	  :azure nil ; stopped working
	  :google t
	  :openai nil
	  :elevenlabs nil
	  :if-unsupported chunkify
	  :replacement ,(tlon-md-get-tag-pattern "voice"))
    (:tag w
	  :tlon nil
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :if-unsupported remove))
  "SSML tags supported by this package and by various TTS engines.

- Amazon Polly:
  <https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html>.

- Microsoft Azure:

- Google Cloud: <https://cloud.google.com/text-to-speech/docs/ssml>.

- OpenAI:
  <https://community.openai.com/t/what-about-to-implement-ssml-on-the-new-tts-api-service/485686/5>.

- ElevenLabs: <https://elevenlabs.io/docs/speech-synthesis/prompting>. Only two
  tags are explicitly mentioned, so maybe none of the others are supported?

The value of `:replacement' is either a regexp pattern to replace with its
second capture group when removing unsupported tags (via
`tlon-tts-remove-unsupported-tags'), or a cons cell whose car is the replacement
pattern and whose cdr is the the number of the capture group to replace with; if
the cdr is nil, the entire tag is removed. We use the second capture group by
default because that is normally the group containing the text enclosed by the
tag. When `:replacement' is nil, the tag is simply removed.")

;;;;; Engine settings

;;;;;; Microsoft Azure

(defconst tlon-microsoft-azure-request
  "curl -v --location --request POST 'https://eastus.tts.speech.microsoft.com/cognitiveservices/v1' \
--header 'Ocp-Apim-Subscription-Key: %s' \
--header 'Content-Type: application/ssml+xml' \
--header 'X-Microsoft-OutputFormat: %s' \
--header 'User-Agent: curl' \
--data-raw '%s' \
-o '%4$s'"
  "Curl command to send a request to the Microsoft Azure text-to-speech engine.
The placeholders are: API key, output format, SSML, destination for the audio
file, and destination for the log file.")

(defconst tlon-microsoft-azure-voices
  '((:id "es-AR-ElenaNeural" :language "es" :gender "female")
    (:id "es-AR-TomasNeural" :language "es" :gender "male")
    (:id "es-CO-GonzaloNeural" :language "es" :gender "male")
    (:id "es-CO-SalomeNeural" :language "es" :gender "female")
    (:id "es-MX-DaliaNeural" :language "es" :gender "female" :role "alternate")
    (:id "es-MX-JorgeNeural" :language "es" :gender "male" :role "alternate")
    (:id "es-US-AlonsoNeural" :language "es" :gender "male" :role "main")
    (:id "es-US-PalomaNeural" :language "es" :gender "female" :role "main")
    (:id "fr-FR-HenriNeural" :language "fr" :gender "male"))
  "Preferred Microsoft Azure voices for different languages.
All the voices in this property list are neural and multilingual, and are the
best male and female voices we were able to identify in each language.

A list of available voices may be found here:
<https://github.com/MicrosoftDocs/azure-docs/blob/main/articles/ai-services/speech-service/includes/language-support/tts.md>.")

(defconst tlon-microsoft-azure-char-limit nil
  "Maximum number of characters that Microsoft Azure can process per request.
Microsoft Azure can process up to 10 minutes of audio at a time. This estimate
assumes 14 characters per second, and uses nine minutes to err on the safe side.")

(defvar tlon-microsoft-azure-key nil
  "API key for the Microsoft Azure TTS service.")

;;;;;; Google Cloud

(defconst tlon-google-cloud-request
  "curl -H 'Authorization: Bearer %s' \
-H 'x-goog-user-project: api-project-781899662791' \
-H 'Content-Type: application/json; charset=utf-8' \
--data '%s' 'https://texttospeech.googleapis.com/v1/text:synthesize' | jq -r .audioContent | base64 --decode > '%s'"
  "Curl command to send a request to the Google Cloud text-to-speech engine.
The placeholders are: token, JSON payload and destination.")

(defconst tlon-google-cloud-voices
  '((:id "en-US-Studio-O" :language "en" :gender "female")
    (:id "en-US-Studio-Q" :language "en" :gender "male")
    (:id "es-US-Neural2-A" :language "es" :gender "female")
    (:id "es-US-Studio-B" :language "es" :gender "male"))
  "Preferred Google Cloud voices for different languages.
The male voice is a \"studio\" voice, the highest quality voice type currently
offered by Google Cloud. Unfortunately, as of 2024-04-12, Google Cloud does not
offer a female studio voice for Spanish, so we use a \"neural\" voice.

A list of available voices may be found here:
<https://cloud.google.com/text-to-speech/docs/voices>.")

(defconst tlon-google-cloud-char-limit (* 5000 0.9)
  "Maximum number of characters that Google Cloud can process per request.
Google Cloud TTS can process up to 5000 bytes per request. We use a slightly
lower number to err on the safe side.

See <https://cloud.google.com/text-to-speech/quotas>.")

(defvar tlon-google-cloud-key nil
  "API key for the Google Cloud TTS service.")

;;;;;; Amazon Polly

(defconst tlon-amazon-polly-request
  "aws polly synthesize-speech \
    --output-format %s \
    --voice-id %s \
    --engine neural \
    --text-type ssml \
    --text '<speak>%s</speak>' \
    --region %s \
    '%s'"
  "AWS command to synthesize speech using Amazon Polly.
The placeholders are: output format, voice ID, SSML, region, and destination
file.")

(defconst tlon-amazon-polly-voices
  '((:id "Joanna" :language "en" :gender "female")
    (:id "Lupe" :language "es" :gender "female")
    (:id "Matthew" :language "en" :gender "male")
    (:id "Pedro" :language "es" :gender "male"))
  "Preferred Amazon Polly voices for different languages.
Joanna and Matthew are some of the available Polly voices for English.")

(defconst tlon-amazon-polly-char-limit (* 1500 0.9)
  "Maximum number of characters that Amazon Polly can process per request.
The limit for Amazon Polly is 1500 characters but using a slightly lower number
to err on the safe side.")

(defvar tlon-amazon-polly-region "us-east-1"
  "Default AWS region for Amazon Polly requests.")

;;;;;; OpenAI

(defconst tlon-openai-tts-request
  "curl https://api.openai.com/v1/audio/speech \\
  -H 'Authorization: Bearer %s' \\
  -H 'Content-Type: application/json' \\
  -d '{
    \"model\": \"%s\",
    \"input\": %s,
    \"voice\": \"%s\"
  }' \\
  --output '%s'"
  "Curl command to send a request to the OpenAI text-to-speech engine.
The placeholders are the API key, the TTS model, the text to be synthesized, the
voice, and the file destination.")

(defconst tlon-openai-voices
  '((:id "allow" :language "es" :gender "female")
    (:id "ash" :language "es" :gender "male")
    (:id "ballad" :language "es" :gender "male")
    (:id "echo" :language "es" :gender "male")
    (:id "fable" :language "es" :gender "female")
    (:id "nova" :language "es" :gender "female")
    (:id "onyx" :language "es" :gender "male")
    (:id "sage" :language "es" :gender "female")
    (:id "shimmer" :language "es" :gender "female")
    (:id "verse" :language "es" :gender "male"))
  "Preferred OpenAI voices for different languages.
All the voices in this property list are neural and multilingual, and are the
best male and female voices we were able to identify in each language.

A list of available voices may be found here:
<https://platform.openai.com/docs/guides/text-to-speech>.")

(defconst tlon-openai-char-limit (* 4096 0.9)
  "Maximum number of characters that OpenAI can process per request.
OpenAI can process up to 4096 bytes per request. We use a slightly
lower number to err on the safe side.

See <https://help.openai.com/en/articles/8555505-tts-api#h_273e638099>.")

(defvar tlon-openai-key nil
  "API key for OpenAI TTS service.")

;;;;;; ElevenLabs

(defconst tlon-elevenlabs-voices
  '((:name "Amelia" :id "Lpn2A60EAsgGCWjFue20" :language "multilingual" :gender "female" :role "alternate")
    (:name "Brian" :id "rncjssM0aAEg1ApKehUP" :language "multilingual" :gender "male")
    (:name "Bruce" :id "qUqZ27WoGID6BUp35xTV" :language "multilingual" :gender "male" :role "main")
    (:name "Cristian" :id "6hfb8itl0CXl6ZA7WVIA" :language "multilingual" :gender "male")
    (:name "Hades" :id "y3uxYtdWYpmzg8Wwx2k3" :language "multilingual" :gender "male")
    (:name "Mariluz" :id "m1VE7dnwBN0zMer3LcKv" :language "multilingual" :gender "female" :role "main")
    (:name "Michael" :id "8mLUlN9GCPCERe4bI7Wx" :language "multilingual" :gender "male" :role "alternate")
    (:name "Nate Silver" :id "IT5b7X3vlpkwXPfi0xBA" :language "multilingual" :gender "main"
	   :stability 0.40 :similarity_boost 0.80 :style 0 :use_speaker_boost t :speed 1)
    (:name "Neal" :id "6JpiWMuXFTicEyWjwDLn" :language "multilingual" :gender "male")
    (:name "Ricardo" :id "CoAqFXxZEa3kpJmE7rDr" :language "multilingual" :gender "male" :role "main")
    (:name "Rob Wiblin" :id "t2fWaFXjl9r1VVHfyeK0" :language "multilingual" :gender "main"
	   :stability 0.45 :similarity_boost 0.80 :style 0 :use_speaker_boost t :speed 1)
    (:name "Victoria" :id "lm0dJr2LmYD4zn0kFH9E" :language "multilingual" :gender "female" :role "main")
    (:name "Antonio Farina" :id "uScy1bXtKz8vPzfdFsFw" :language "multilingual" :gender "male"))
  "Preferred ElevenLabs voices for different languages.
A list of available voices may be found here:
<https://elevenlabs.io/app/voice-library>. To get information about the voices,
including the voice ID, run `tlon-tts-elevenlabs-get-voices'.")

(defconst tlon-elevenlabs-char-limit nil
  "Maximum number of characters that Elevenlabs can process per request.
Elevenlabs can process up to 5000 characters per request.

When set to nil (the default), text will be chunked by paragraph regardless of
size, which helps mitigate voice degradation issues for longer texts. When set
to a number (e.g., (* 5000 0.9)), text will be chunked based on character count.

With paragraph-based chunking, we use ElevenLabs' request stitching feature to
maintain natural prosody between paragraphs by providing context from
surrounding paragraphs in each request.

See <https://elevenlabs.io/app/subscription> (scroll down to \"Frequently asked
questions\").")

(defconst tlon-elevenlabs-tts-url
  "https://api.elevenlabs.io/v1/text-to-speech/%s/stream?output_format=%s"
  "Base URL for the ElevenLabs TTS API.")

(defvar tlon-elevenlabs-key nil
  "API key for the ElevenLabs TTS service.")

;;;;; Engines

(defconst tlon-tts-engines
  `((:name "Amazon Polly"
	   :voices-var tlon-amazon-polly-voices
	   :audio-var tlon-amazon-polly-audio-settings
	   :choices-var tlon-amazon-polly-audio-choices
	   :request-fun tlon-tts-amazon-polly-make-request
	   :char-limit tlon-amazon-polly-char-limit
	   :property :polly)
    (:name "ElevenLabs"
	   :voices-var tlon-elevenlabs-voices
	   :audio-var tlon-elevenlabs-audio-settings
	   :choices-var tlon-elevenlabs-audio-choices
	   :request-fun tlon-tts-elevenlabs-make-request
	   :char-limit tlon-elevenlabs-char-limit
	   :property :elevenlabs)
    (:name "Google Cloud"
	   :voices-var tlon-google-cloud-voices
	   :audio-var tlon-google-cloud-audio-settings
	   :choices-var tlon-google-cloud-audio-choices
	   :request-fun tlon-tts-google-cloud-make-request
	   :char-limit tlon-google-cloud-char-limit
	   :property :google)
    (:name "Microsoft Azure"
	   :voices-var tlon-microsoft-azure-voices
	   :audio-var tlon-microsoft-azure-audio-settings
	   :choices-var tlon-microsoft-azure-audio-choices
	   :request-fun tlon-tts-microsoft-azure-make-request
	   :char-limit tlon-microsoft-azure-char-limit
	   :property :azure)
    (:name "OpenAI"
	   :voices-var tlon-openai-voices
	   :audio-var tlon-openai-audio-settings
	   :request-fun tlon-tts-openai-make-request
	   :char-limit tlon-openai-char-limit
	   :property :openai))
  "Text-to-speech engines and associated properties.")

(defun tlon-tts--engine-char-limit ()
  "Return the current char-limit for `tlon-tts-engine'.
If the :char-limit entry in `tlon-tts-engines' is a symbol, look up its
present value; otherwise return the value itself."
  (let ((prop (tlon-lookup tlon-tts-engines
                           :char-limit :name tlon-tts-engine)))
    (if (symbolp prop) (symbol-value prop) prop)))

;; needs to use double quotes for Azure, but maybe single quotes for Google Cloud?
;; cannot be in single quotes because the entire string is itself enclosed in single quotes
(defconst tlon-ssml-wrapper
  (mapcar (lambda (service)
	    (cons service
		  (format "<speak version=\"1.0\" xmlns=\"http://www.w3.org/2001/10/synthesis\" xml:lang=\"%%s\">%s</speak>"
			  (tlon-md-return-tag "voice" '("%s") "%s" 'get-values))))
	  '("Microsoft Azure" "Google Cloud"))
  "SSML wrapper for the TTS request.")

;;;;; `ffmpeg'

(defconst tlon-tts-ffmpeg-normalize
  "ffmpeg -i %s -af loudnorm=I=-16:TP=-1.5:LRA=11 -y %s"
  "Command to normalize an audio file using ffmpeg loudnorm filter.
-i: input file
-af loudnorm: apply the loudness normalization filter
  I=-16: Integrated loudness target (LUFS)
  TP=-1.5: True peak target (dBTP)
  LRA=11: Loudness range target (LU)
-y: overwrite output file without asking
First %s is input file, second %s is output file.")

(defconst tlon-tts-ffmpeg-convert
  "ffmpeg -i \"%s\" -acodec libmp3lame -ar 44100 -b:a 128k -ac 1 \"%s\""
  "Command to convert an audio file to MP3 format with settings optimized for tts.
The first placeholder is the input file, and the second is the output file.")

;;;;; Report

(defconst tlon-tts-report-buffer-name
  "*TTS Report*"
  "The name of the TTS report buffer.")

(defconst tlon-tts-maybe-chemical-symbol
  "</SmallCaps><sub>"
  "Pattern that may match a chemical symbol.")

;;;;; Links

(defconst tlon-tts-self-referential-link
  '(("ar" . ("هنا" . "هنا (في النص هناك رابط يشير إلى صفحة وي"))
    ("de" . ("hier" . "hier (im Text gibt es einen Link, der auf eine Webseite zeigt)"))
    ("en" . ("here" . "here (in the text there is a link here pointing to a web page)"))
    ("es" . ("aquí" . "aquí (en el texto hay un enlace que apunta a una página web)"))
    ("fr" . ("ici" . "ici (dans le texte, il y a un lien qui pointe vers une page web)"))
    ("it" . ("qui" . "qui (nel testo c'è un link che punta a una pagina web)"))
    ("ja" . ("ここ" . "ここ (テキストには、ウェブページを指すリンクがあります)"))
    ("ko" . ("여기" . "여기 (텍스트에는 웹 페이지를 가리키는 링크가 있습니다)"))
    ("pt" . ("aqui" . "aqui (no texto há um link que aponta para uma página web)"))
    ("tr" . ("burada" . "burada (metinde bir web sayfasına işaret eden bir bağlantı var)")))
  "List of self-referential link texts and replacements.")

;;;;; Tables

(defconst tlon-tts-table-cell
  "\\(?: .*?|\\)"
  "Regular expression to match a table cell.")

(defconst tlon-tts-table-row
  (format "\\(?:^|%s+\n\\)" tlon-tts-table-cell)
  "Regular expression to match a table row.")

(defconst tlon-tts-table-pattern
  (format "\\(?1:%s+\\)" tlon-tts-table-row)
  "Regular expression to match a table.")

(defconst tlon-tts-table-separator
  "|\\(?: ?-+ ?|\\)+\n"
  "Regular expression to match the separator between table rows.")

(defconst tlon-tts-table-header
  (format "\\(?1:%s\\)%s" tlon-tts-table-row tlon-tts-table-separator)
  "Regular expression to match a table header.
The expression matches the header followed by the separator, capturing the
former in group 1.")

;;;;; Numbers

(defconst tlon-tts-regular-exponent-pattern
  '(("ar" . "إلى %s")
    ("de" . "zur %s")
    ("en" . "to the power of %s")
    ("es" . "a la %s")
    ("fr" . "à la %s")
    ("it" . "alla %s")
    ("ja" . "%s 乗")
    ("ko" . "%s 제곱")
    ("pt" . "à %s")
    ("tr" . "%s üssü"))
  "Pattern for regular exponents.")

(defconst tlon-tts-irregular-exponents
  '((2 . (("ar" . "إلى السطح الثاني")
	  ("de" . "zum Quadrat")
	  ("en" . "squared")
	  ("es" . "al cuadrado")
	  ("fr" . "au carré")
	  ("it" . "al quadrato")
	  ("ja" . "二乗")
	  ("ko" . "제곱")
	  ("pt" . "ao quadrado")
	  ("tr" . "kare")))
    (3 . (("ar" . "إلى السطح الثالث")
	  ("de" . "zum Kubik")
	  ("en" . "cubed")
	  ("es" . "al cubo")
	  ("fr" . "au cube")
	  ("it" . "al cubo")
	  ("ja" . "三乗")
	  ("ko" . "세제곱")
	  ("pt" . "ao cubo")
	  ("tr" . "küp")))
    (4 . (("ar" . "إلى السطح الرابع")
	  ("de" . "zur vierten")
	  ("en" . "to the fourth")
	  ("es" . "a la cuarta")
	  ("fr" . "à la quatrième")
	  ("it" . "alla quarta")
	  ("ja" . "四乗")
	  ("ko" . "네제곱")
	  ("pt" . "à quarta")
	  ("tr" . "dördüncü kuvvet")))
    (5 . (("ar" . "إلى السطح الخامس")
	  ("de" . "zur fünften")
	  ("en" . "to the fifth")
	  ("es" . "a la quinta")
	  ("fr" . "à la cinquième")
	  ("it" . "alla quinta")
	  ("ja" . "五乗")
	  ("ko" . "다섯제곱")
	  ("pt" . "à quinta")
	  ("tr" . "beşinci kuvvet")))
    (6 . (("ar" . "إلى السطح السادس")
	  ("de" . "zur sechsten")
	  ("en" . "to the sixth")
	  ("es" . "a la sexta")
	  ("fr" . "à la sixième")
	  ("it" . "alla sesta")
	  ("ja" . "六乗")
	  ("ko" . "여섯제곱")
	  ("pt" . "à sexta")
	  ("tr" . "altıncı kuvvet")))
    (7 . (("ar" . "إلى السطح السابع")
	  ("de" . "zur siebten")
	  ("en" . "to the seventh")
	  ("es" . "a la séptima")
	  ("fr" . "à la septième")
	  ("it" . "alla settima")
	  ("ja" . "七乗")
	  ("ko" . "일곱제곱")
	  ("pt" . "à sétima")
	  ("tr" . "yedinci kuvvet")))
    (8 . (("ar" . "إلى السطح الثامن")
	  ("de" . "zur achten")
	  ("en" . "to the eighth")
	  ("es" . "a la octava")
	  ("fr" . "à la huitième")
	  ("it" . "all'ottava")
	  ("ja" . "八乗")
	  ("ko" . "여덟제곱")
	  ("pt" . "à oitava")
	  ("tr" . "sekizinci kuvvet")))
    (9 . (("ar" . "إلى السطح التاسع")
	  ("de" . "zur neunten")
	  ("en" . "to the ninth")
	  ("es" . "a la novena")
	  ("fr" . "à la neuvième")
	  ("it" . "alla nona")
	  ("ja" . "九乗")
	  ("ko" . "아홉제곱")
	  ("pt" . "à nona")
	  ("tr" . "dokuzuncu kuvvet"))))
  "List of exponents and their irregular verbal equivalents.")

(defconst tlon-tts-80000
  '(("ar" . "ثمانون ألف")
    ("de" . "Achtzigtausend")
    ("en" . "Eighty thousand")
    ("es" . "Ochenta mil")
    ("fr" . "Quatre-vingt mille")
    ("it" . "Ottantamila")
    ("ja" . "八万")
    ("ko" . "팔만")
    ("pt" . "Oitenta mil")
    ("tr" . "Seksen bin"))
  "The number 80000 in different languages.")

(defconst tlon-tts-thousands-separator
  '((:language "ar" :separator ",")
    (:language "de" :separator ".")
    (:language "en" :separator ",")
    (:language "es" :separator ".")
    (:language "fr" :separator " ")
    (:language "it" :separator ".")
    (:language "ja" :separator ",")
    (:language "ko" :separator ",")
    (:language "tr" :separator "."))
  "List of language-specific thousands separators.
The specified separator will replace existing thousands separators, so that the
TTS engine pronounces the numbers correctly.")

(defconst tlon-tts-decimals-separator
  '((:language "ar" :separator "")
    (:language "de" :separator "")
    (:language "en" :separator "")
    (:language "es" :separator "")
    (:language "fr" :separator "")
    (:language "it" :separator "")
    (:language "ja" :separator "")
    (:language "ko" :separator "")
    (:language "tr" :separator ""))
  "List of language-specific decimals separators.
The specified separator will replace existing thousands separators, so that the
TTS engine pronounces the numbers correctly.")

;;;;; Currencies

(defconst tlon-tts-currencies
  '(("$" . (("en" . ("dollar" . "dollars"))
	    ("es" . ("dólar" . "dólares"))))
    ("£" . (("en" . ("pound" . "pounds"))
	    ("es" . ("libra" . "libras"))))
    ("¥" . (("en" . ("yen" . "yens"))
	    ("es" . ("yen" . "yenes"))))
    ("Ξ" . (("en" . ("ether" . "ether"))
	    ("es" . ("éter" . "éter")))) ("₪" . (("en" . ("shekel" . "shekels"))
	    ("es" . ("séquel" . "séqueles"))))
					; https://www.fundeu.es/recomendacion/shekel-shequel-sekel-sequel/
    ("₹" . (("en" . ("rupee" . "rupees"))
	    ("es" . ("rupia" . "rupias"))))
    ("₿" . (("en" . ("bitcoin" . "bitcoins"))
	    ("es" . ("bitcoin" . "bitcoins")))))
  "Currency symbols and their associated three-letter codes.")

(defconst tlon-tts-currency-units
  '(("en" . ("trillion" "billion" "million" "thousand"))
    ("es" . ("millones" "millón" "mil" "billones" "billón" "trillones" "trillón")))
  "List of currency unit words by language.
Order longer units before shorter ones if they share prefixes, for correct regex
matching.")

(defconst tlon-tts-currency-unit-prepositions
  '(("es" . (("billones" . " de ")
             ("billón" . " de ")
             ("mil" . " ")
             ("millones" . " de ")
             ("millón" . " de ")
             ("trillones" . " de ")
             ("trillón" . " de ") ; e.g., "siete mil dólares"
             (t . " ")))  ; Default preposition (a space) if unit present but not listed
    ("en" . ((t . " ")))) ; Default for English (e.g., "seven million dollars")
  "Prepositions to use between currency unit and currency name, by language.
The car of each sub-alist entry is the unit word (or t for default), and the cdr
is the preposition string (often including leading/trailing spaces as needed).")

;;;;; Tag sections

(defconst tlon-tts-tag-section-names
  '(("ar" . "\\(?:مزيد من المعلومات\\|مقالات ذات صلة\\|روابط خارجية\\)")
    ("de" . "\\(?:weitere Informationen\\|verwandte Einträge\\|externe Links\\)")
    ("en" . "\\(?:further readong\\|related entries\\|external links\\)")
    ("es" . "\\(?:más información\\|entradas relacionadas\\|enlaces externos\\)")
    ("fr" . "\\(?:plus d'informations\\|entrées connexes\\|liens externes\\)")
    ("it" . "\\(?:ulteriori informazioni\\|voci correlate\\|collegamenti esterni\\)")
    ("ja" . "\\(?:詳細情報\\|関連エントリ\\|外部リンク\\)")
    ("ko" . "\\(?:추가 정보\\|관련 항목\\|외부 링크\\)")
    ("pt" . "\\(?:mais informações\\|entradas relacionadas\\|links externos\\)")
    ("tr" . "\\(?:daha fazla bilgi\\|ilgili girişler\\|dış bağlantılar\\)"))
  "Names of the sections to be removed in each language.")

(defconst tlon-tts-tag-section-patterns
  (let ((format-string "^\\(?1:## %s\\)"))
    (mapcar (lambda (pair)
	      (let ((lang (car pair))
		    (names (cdr pair)))
		(cons lang (format format-string names))))
	    tlon-tts-tag-section-names))
  "Match the name of a section to be removed until the end of the buffer.")

;;;;; File-local variables

;;;;;; File variables

;; These are the file-local variables whose values we read from the source files

(defvar-local tlon-local-abbreviations '()
  "Local abbreviations and their spoken equivalent.")

(defvar-local tlon-local-replacements '()
  "Local replacements.")

(defvar-local tlon-tts-voice-chunks '()
  "List of voice chunk data.
Each element is a cons cell where the car is the begin position of the voice
chunk and the cdr voice to be used to narrate this chunk.")

;;;;;; Chunk data structure indices

(defconst tlon-tts-chunk-index-text 0 "Index for the text content in a chunk.")
(defconst tlon-tts-chunk-index-voice-params 1 "Index for voice parameters in a chunk.")
(defconst tlon-tts-chunk-index-filename 2 "Index for the audio filename in a chunk.")
(defconst tlon-tts-chunk-index-request-id 3 "Index for the ElevenLabs request ID in a chunk.")
(defconst tlon-tts-chunk-index-status 4 "Index for the processing status in a chunk.")
(defconst tlon-tts-chunk-index-staging-buffer-name 5 "Index for the staging buffer name in a chunk.")
(defconst tlon-tts-chunk-index-header-filename 6 "Index for the header filename in a chunk.")
(defconst tlon-tts-chunk-index-begin-marker 7 "Index for the begin marker in a chunk.")
(defconst tlon-tts-chunk-index-end-marker 8 "Index for the end marker in a chunk.")
(defconst tlon-tts-chunk-index-chunk-number 9 "Index for the 1-based chunk number.")

(defvar-local tlon-tts-chunks nil
  "List of chunks to be narrated. Buffer-local to the staging buffer.
Each element is a list accessed using `tlon-tts-chunk-index-*' constants:

- TEXT: The text content of the chunk.

- VOICE-PARAMS: A cons cell like `(tlon-tts-voice . VOICE-ID)' or nil.

- FILENAME: The path to the generated audio file for this chunk.

- REQUEST-ID: The xi-request-id returned by ElevenLabs (nil
  otherwise/initially).

- STATUS: Processing status symbol (\\='pending, \\='running, \\='completed,
  \\='failed).

- STAGING-BUFFER-NAME: The name of the staging buffer this chunk belongs to.

- HEADER-FILENAME: Path to the temporary file storing curl headers for this
  chunk.

- BEGIN-MARKER: Marker for the beginning of the chunk's text in the staging
  buffer.

- END-MARKER: Marker for the end of the chunk's text in the staging buffer.

- CHUNK-NUMBER: The 1-based number of this chunk.")

(defvar tlon-tts-chunks-to-process 0
  "Number of chunks left to process for full buffer narration.")

(defvar-local tlon-tts-user-selected-chunks-to-process nil
  "List of 0-based indices of user-selected chunks to process.
This is used by `tlon-tts-narrate-staged-chunks` for asynchronous processing.")

;;;;;; Local abbrevs

;; These are the variables that store the file-local variable values for a particular tts session
;; TODO: decide if they should also be file-local

(defvar tlon-local-abbreviations-for-session '()
  "Value of `tlon-local-abbreviations' for the file being processed.")

(defvar tlon-local-replacements-for-session '()
  "Value of `tlon-local-replacements' for the file being processed.")

;;;;; Listener cues

(defconst tlon-tts-cue-delimiter
  ""
  "Delimiter for listener cues.")

(defconst tlon-tts-listener-cues
  '((aside
     ("ar" "في الجانب." . "\nنهاية الجانب.")
     ("de" "Beiseite." . "\nEnde des Beiseites.")
     ("en" "Aside." . "\nEnd of the aside.")
     ("es" "Inciso." . "\nFin del inciso.")
     ("fr" "Incise." . "\nFin de l'incise.")
     ("it" "Parentesi." . "\nFine della parentesi.")
     ("ja" "横に." . "\n横の終わり.")
     ("ko" "옆에." . "\n옆의 끝.")
     ("tr" "Yan not." . "\nYan notun sonu."))
    (blockquote
     ("ar" "اقتباس." . "\nنهاية الاقتباس.")
     ("de" "Zitat." . "\nEnde des Zitats.")
     ("en" "Quote." . "\nEnd of quote.")
     ("es" "Cita." . "\nFin de la cita.")
     ("fr" "Citation." . "\nFin de la citation.")
     ("it" "Citazione." . "\nFine della citazione.")
     ("ja" "引用." . "\n引用の終わり.")
     ("ko" "인용문." . "\n인용문의 끝.")
     ("tr" "Alıntı." . "\nAlıntının sonu."))
    (heading
     ("ar" "عنوان." . "")
     ("de" "Überschrift." . "")
     ("en" "Heading." . "")
     ("es" "Sección." ."")
     ("fr" "Titre." . "")
     ("it" "Titolo." . "")
     ("ja" "見出し." . "")
     ("ko" "제목." . "")
     ("pt" "Título." . "")
     ("tr" "Başlık." . ""))
    (image
     ("ar" "هناك صورة هنا." . "\nنهاية الصورة.")
     ("de" "Hier ist ein Bild." . "\nEnde des Bildes.")
     ("en" "There’s an image here." . "\nEnd of image.")
     ("es" "Aquí hay una imagen." . "\nFin de la imagen.")
     ("fr" "Il y a une image ici." . "\nFin de l'image.")
     ("it" "C'è un'immagine qui." . "\nFine dell'immagine.")
     ("ja" "ここに画像があります。" . "\n画像の終わり。")
     ("ko" "여기에 이미지가 있습니다." . "\n이미지의 끝.")
     ("tr" "Burada bir resim var." . "\nResmin sonu."))
    (image-caption
     ("ar" . " تتبع الصورة تعليقًا يقول: ")
     ("de" . " Das Bild wird von einer Bildunterschrift gefolgt, die lautet: ")
     ("en" . " The image is followed by a caption that reads: ")
     ("es" . " A la imagen le sigue una leyenda que dice: ")
     ("fr" . " L'image est suivie d'une légende qui dit: ")
     ("it" . " L'immagine è seguita da una didascalia che recita: ")
     ("ja" . " 画像の後には次のようなキャプションが続きます: ")
     ("ko" . " 이미지 뒤에는 다음과 같은 캡션이 있습니다: ")
     ("pt" . " A imagem é seguida por uma legenda que diz: ")
     ("tr" . " Resmin ardından şu açıklama gelir: "))
    (note
     ("ar" "ملاحظة." . "\nنهاية الملاحظة.")
     ("de" "Anmerkung." . "\nEnde der Anmerkung.")
     ("en" "Note." . "\nEnd of the note.")
     ("es" "Nota." . "\nFin de la nota.")
     ("fr" "Note." . "\nFin de la note.")
     ("it" "Nota." . "\nFine della nota.")
     ("ja" "ノート." . "\nノートの終わり.")
     ("ko" "노트." . "\n노트의 끝.")
     ("tr" "Not." . "\nNotun sonu."))
    (embedded
     ("ar" "رسم بياني." . "\nنهاية الرسم البياني.")
     ("de" "Diagramm." . "\nEnde des Diagramms.")
     ("en" "Chart." . "\nEnd of chart.")
     ("es" "Cuadro." . "\nFin del cuadro.")
     ("fr" "Tableau." . "\nFin du tableau.")
     ("it" "Grafico." . "\nFine del grafico.")
     ("ja" "チャート." . "\nチャートの終わり.")
     ("ko" "차트." . "\n차트의 끝.")
     ("tr" "Grafik." . "\nGrafiğin sonu."))
    (quote
     ("ar" "(اقتباس)" . "(نهاية الاقتباس)")
     ("de" "(Zitat)" . "(Ende des Zitats)")
     ("en" "(quote)" . "(End of quote)")
     ("es" "(cita)" . "(Fin de la cita)")
     ("fr" "(citation)" . "(Fin de la citation)")
     ("it" "(citazione)" . "(Fine della citazione")
     ("ja" "(引用)" . "(引用の終わり")
     ("ko" "(인용문)" . "(인용문의 끝)")
     ("tr" "(alıntı)" . "(alıntının sonu)"))
    (subheading
     ("ar" "عنوان فرعي." . "")
     ("de" "Unterüberschrift." . "")
     ("en" "Subheading." . "")
     ("es" "Subsección." . "")
     ("fr" "Sous-titre." . "")
     ("it" "Sottotitolo." . "")
     ("ja" "サブタイトル." . "")
     ("ko" "소제목." . "")
     ("pt" "Subtítulo." . "")
     ("tr" "Alt başlık." . ""))
    (table
     ("ar" "هناك جدول هنا.\n" . "\nنهاية الجدول.")
     ("de" "Hier ist eine Tabelle.\n" . "\nEnde der Tabelle.")
     ("en" "There’s a table here.\n" . "\nEnd of the table.")
     ("es" "Aquí hay una tabla.\n" . "\nFin de la tabla.")
     ("fr" "Il y a un tableau ici.\n" . "\nFin du tableau.")
     ("it" "C'è una tabella qui.\n" . "\nFine della tabella.")
     ("ja" "ここにテーブルがあります。\n" . "\nテーブルの終わり。")
     ("ko" "여기에 표가 있습니다.\n" . "\n표의 끝.")
     ("tr" "Burada bir tablo var.\n" . "\nTablonun sonu.")))
  "Listener cues for different types of content.")

(defconst tlon-tts-listener-cue-patterns
  `((aside ,(tlon-md-get-tag-pattern "Aside") . 2)
    (blockquote ,tlon-md-blockquote . 1)
    (heading ,tlon-md-heading . 5)
    (embedded ,(tlon-md-get-tag-pattern "Embedded") . 6)
    (quote ,(tlon-md-get-tag-pattern "q") . 2)
    (subheading ,tlon-md-subheading . 5)
    (table ,(tlon-md-get-tag-pattern "SimpleTable") . 4)))

;;;;;; Lists

;; TODO: develop function for processing lists

;;;; Functions

;;;;; Stage

(declare-function flycheck-mode "flycheck")
(declare-function tlon-get-number-of-paragraphs "tlon-counterpart")
;;;###autoload
(defun tlon-tts-stage-content (&optional content file)
  "Stage the content to be narrated.
If CONTENT is nil, read the region if selected, FILE if non-nil, or the file
visited by the current buffer otherwise. Preserves the relative paragraph
position of the cursor from the source buffer."
  (interactive)
  (let* (;; Store original position only if content is not explicitly provided
         (source-buffer (unless content (current-buffer)))
         (source-point (unless content (point)))
         (paragraph-index (when source-buffer
                            (require 'tlon-counterpart) ; Ensure function is loaded
                            (with-current-buffer source-buffer
                              (tlon-get-number-of-paragraphs (point-min) source-point))))
         (file (or file (buffer-file-name)))
         (content (or content (tlon-tts-get-content nil file))))
    (with-current-buffer (get-buffer-create (tlon-tts-get-staging-buffer-name file))
      (let ((inhibit-read-only t)) ; Allow modification even if buffer was read-only
        (erase-buffer)
        (insert content)
        (markdown-mode) ; Set mode early
        (flycheck-mode -1))
      (tlon-tts-set-file-local-vars file)
      (tlon-tts-prepare-staging-buffer)
      (tlon-tts-calculate-chunks)
      (tlon-tts-insert-chunk-comments)
      (tlon-tts-restore-position paragraph-index))))

(defun tlon-tts-restore-position (paragraph-index)
  "Restore point position in the staging buffer.
PARAGRAPH-INDEX is 0-based index of content paragraph in source.
Metadata (title, author) are usually chunks 1 and 2. So, the first content
paragraph corresponds to chunk 3 (1-based)."
  (when paragraph-index
    (tlon-tts-goto-chunk (+ paragraph-index 3))))

(defun tlon-tts-goto-chunk (chunk-number)
  "Move point to the beginning of the CHUNK-NUMBER'th chunk.
CHUNK-NUMBER is 1-based. Assumes the current buffer is a TTS staging buffer
with `<!-- Chunk N -->` comments."
  (interactive "nChunk number (1-based): ")
  (goto-char (point-min))
  (let ((chunk-found nil))
    (while (and (not chunk-found) (re-search-forward tlon-tts-chunk-comment-regex nil t))
      (let ((num-in-comment (string-to-number (match-string 1))))
        (when (= num-in-comment chunk-number)
          (goto-char (match-beginning 0))
          (setq chunk-found t))))
    (unless chunk-found
      (message "Chunk %d not found." chunk-number)
      (goto-char (point-max)))))

(defun tlon-tts-get-chunk-number-at-point ()
  "Return the chunk number *N* recorded in the `<!-- Chunk N -->` comment.
If no such comment is found, return nil."
  (save-excursion
    (let ((comment-regex tlon-tts-chunk-comment-regex))
      (beginning-of-line)
      (cond
       ((looking-at comment-regex)
        (string-to-number (match-string 1)))
       ;; Search backward from current line start, but not beyond buffer start
       ((re-search-backward comment-regex nil t)
        (string-to-number (match-string 1)))
       (t nil)))))

;; MAYBE: make it work with non-file-visiting buffers
(defun tlon-tts-get-content (&optional content file)
  "Return the content to be read.
If CONTENT is nil, read the region if selected, FILE if non-nil, or the file
visited by the current buffer otherwise."
  (or content (if (region-active-p)
		  (tlon-tts-get-selection)
		(tlon-tts-read-file file))))

(defun tlon-tts-get-staging-buffer-name (file)
  "Return the name of the staging buffer for FILE.
If FILE is nil, use file of the current TTS process."
  (format tlon-tts-staging-buffer-formatter "*" (file-name-base file)))

(defun tlon-tts-set-file-local-vars (&optional source language engine audio voice locale)
  "Set all file-local variables.
SOURCE, LANGUAGE, ENGINE, AUDIO, VOICE and LOCALE are the values to set."
  (tlon-tts-set-staging-buffer)
  (tlon-tts-set-source source)
  (tlon-tts-set-language language)
  (tlon-tts-set-engine engine)
  (tlon-tts-set-audio audio)
  (tlon-tts-set-voice voice)
  (tlon-tts-set-locale locale))

(defun tlon-tts-read-file (&optional file)
  "Return the substantive content of FILE, handling in-text abbreviations."
  (let ((file (or file (buffer-file-name))))
    (unless (string= (file-name-extension file) "md")
      (user-error "File `%s' is not a Markdown file" file))
    (with-current-buffer (find-file-noselect file)
      (setq tlon-local-abbreviations-for-session tlon-local-abbreviations
	    tlon-local-replacements-for-session tlon-local-replacements)
      (concat (tlon-tts-get-metadata) (tlon-md-read-content file)))))

(declare-function tlon-bib-replace-keys-with-citations "tlon-bib")
(defun tlon-tts-prepare-staging-buffer ()
  "Prepare the current buffer for audio narration."
  (save-excursion
    (tlon-tts-ensure-all-images-have-alt-text)
    (tlon-tts-ensure-all-tables-have-alt-text)
    (tlon-tts-process-notes) ; should be before `tlon-bib-replace-keys-with-citations'
    (tlon-tts-remove-tag-sections) ; should probably be before `tlon-tts-process-listener-cues'
    (tlon-tts-remove-horizontal-lines) ; should be before `tlon-tts-process-paragraphs'
    (tlon-bib-replace-keys-with-citations nil 'audio) ; should be before `tlon-tts-escape-xml-special-characters'
    (tlon-tts-process-listener-cues) ; should be before `tlon-tts-process-links', `tlon-tts-process-paragraphs'
    (tlon-tts-process-links) ; should probably be before `tlon-tts-process-formatting'
    (tlon-tts-process-all-abbreviations)
    (tlon-tts-process-formatting) ; should be before `tlon-tts-process-paragraphs'
    (tlon-tts-process-paragraphs)
    (tlon-tts-process-currencies) ; should be before `tlon-tts-process-numerals'
    (tlon-tts-process-numerals)
    (tlon-tts-remove-final-break-tag)
    ;; FIXME: the below is not working properly.
    ;; `tlon-tts-get-replacement-for-unsupported-ssml-tags' is not returning the
    ;; correct value when run with the `phoneme' tag. Fixing it is not a
    ;; priority because Elevenlabs does not support this tag anyway.
    (tlon-tts-process-unsupported-ssml-tags)
    (tlon-tts-remove-extra-newlines)
    ;; Chunk comments will be inserted by `tlon-tts-narrate-staged-buffer`
    ;; after chunks are prepared.
    ;; FIXME: this is breaking the SSML tags
    ;; (tlon-tts-escape-xml-special-characters)
    (tlon-tts-generate-report)))

;;;;; Narrate

;;;;;; Buffer

;;;###autoload
(defun tlon-tts-narrate-staged-buffer ()
  "Narrate the content staged in the current buffer."
  (interactive)
  (unless (tlon-tts-staging-buffer-p)
    (user-error "Not in a staging buffer"))
  (setq tlon-tts-user-selected-chunks-to-process nil) ; Ensure we are in full buffer mode
  ;; If tlon-tts-chunks is not populated, it means initial staging
  ;; might not have completed or this is a fresh call.
  ;; In this case, calculate chunks and insert comments.
  (tlon-tts-ensure-chunks-ready)
  ;; Now, tlon-tts-chunks should be populated, and the comments in the
  ;; buffer should be in sync with it.
  (tlon-tts-process-chunks))

(defun tlon-tts-staging-buffer-p ()
  "Return t iff the current buffer is a staging buffer."
  (bound-and-true-p tlon-tts-staging-buffer-p))

;;;;;; Chunks

;;;###autoload
(defun tlon-tts-narrate-staged-chunks (&optional beg end)
  "Generate audio for chunk(s) in the TTS staging buffer.
If region is active, generate audio for all chunks starting within the
region (from BEG to END). Otherwise, generate audio for the chunk at point.
This overwrites the corresponding audio file(s) named using the chunk
number (e.g., `basename-chunk-005.mp3`)."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (unless (tlon-tts-staging-buffer-p)
    (user-error "Not in a TTS staging buffer. Run `tlon-tts-stage-content' first"))
  ;; Ensure chunks are calculated so tlon-tts-chunks is populated
  ;; and comments in buffer reflect it.
  (save-excursion
    (tlon-tts-ensure-chunks-ready))
  (unless tlon-tts-chunks
    (user-error "No TTS chunks found. Staging buffer might be empty or invalid, or `tlon-tts-calculate-chunks` failed"))
  (let (chunks-to-generate)
    (if (and beg end)
        ;; --- Region is active ---
        (save-excursion
          (goto-char beg)
          (let (current-chunk-number)
            (while (< (point) end)
              ;; Calculate chunk number at current position
              (setq current-chunk-number (tlon-tts-get-chunk-number-at-point))
              ;; Add to list if not already present and valid
              (when current-chunk-number
                (cl-pushnew current-chunk-number chunks-to-generate))
              ;; Move to the start of the next line to check for next chunk comment
              (condition-case nil
                  (progn
                    (forward-line 1)
                    ;; Ensure we don't get stuck if forward-line doesn't move
                    (when (and (= (point) beg) (< beg end)) (forward-char 1)))
                (error (goto-char end))))) ; Move to end if error (e.g., end of buffer)
          (setq chunks-to-generate (nreverse chunks-to-generate))) ; Maintain order
      ;; --- No region active, generate chunk at point ---
      (let ((chunk-at-point (tlon-tts-get-chunk-number-at-point)))
        (when chunk-at-point
          (setq chunks-to-generate (list chunk-at-point)))))
    ;; --- Generate audio for selected chunks ---
    (if chunks-to-generate
        (let ((selected-indices ; Convert 1-based chunk numbers to 0-based indices
               (mapcar (lambda (chunk-number) (1- chunk-number))
                       chunks-to-generate)))
          (if selected-indices
              (progn
                (setq tlon-tts-user-selected-chunks-to-process selected-indices)
                (message "Queueing generation for %d selected chunk(s)..." (length selected-indices))
                ;; Start processing the first selected chunk
                (tlon-tts-generate-audio (car tlon-tts-user-selected-chunks-to-process)))
            (message "No chunks selected for generation."))) ; Should not happen if chunks-to-generate is non-nil
      (unless chunks-to-generate (message "No chunks identified in the selection.")))))

(defun tlon-tts-execute-generation-request (chunk-number chunk-index chunk-text chunk-filename voice-params)
  "Execute the TTS request to generate CHUNK-TEXT.
CHUNK-NUMBER (1-based) is used for logging and filename.
CHUNK-INDEX (0-based) is used for context calculation in the request function.
CHUNK-FILENAME is the output file.
VOICE-PARAMS are the specific voice parameters for this chunk."
  (let* ((fun (tlon-lookup tlon-tts-engines :request-fun :name tlon-tts-engine))
         ;; Pass the 0-based chunk-index to the request function
         (request (funcall fun chunk-text chunk-filename voice-params chunk-index))
         (process-name (format "generate audio chunk %d" chunk-number))
         (process (start-process-shell-command process-name nil request)))

    (message "Generating chunk %d with voice %s into %s..."
             chunk-number
             (tlon-tts-get-voice-friendly-name
              (when (and (consp voice-params) (eq (car voice-params) 'tlon-tts-voice))
                (cdr voice-params)))
             (file-name-nondirectory chunk-filename))
    ;; Wait for completion
    (while (process-live-p process)
      (accept-process-output process 0.1))
    ;; Return the process for status checking
    process))

(defun tlon-tts-handle-generation-result (process chunk-number chunk-filename)
  "Handle the result of the generation PROCESS.
CHUNK-NUMBER and CHUNK-FILENAME are used for logging."
  (if (= (process-exit-status process) 0)
      (message "Chunk %d generated successfully into %s."
               chunk-number (file-name-nondirectory chunk-filename))
    (message "Error generating chunk %d. Check *Messages* buffer." chunk-number)
    (when-let ((err-buffer (process-buffer process)))
      (with-current-buffer err-buffer
        (message "Error output for chunk %d generation:\n%s"
                 chunk-number (buffer-string))))))

(defun tlon-tts--generate-single-chunk-by-number (chunk-number)
  "Generate audio for the chunk specified by CHUNK-NUMBER.
CHUNK-NUMBER is the 1-based index of the chunk."
  (unless (and chunk-number (> chunk-number 0))
    (user-error "Invalid chunk number provided: %s" chunk-number))

  (let* ((chunk-index (1- chunk-number)) ; 0-based index for list access
         (chunk-info  (if (< chunk-index (length tlon-tts-chunks))
                          (nth chunk-index tlon-tts-chunks)
                        nil))
         (voice-params (when chunk-info (nth tlon-tts-chunk-index-voice-params chunk-info)))
         (chunk-text (when chunk-info (nth tlon-tts-chunk-index-text chunk-info)))
         (chunk-filename (when chunk-info (nth tlon-tts-chunk-index-filename chunk-info))))

    (unless chunk-info
      (user-error "Could not find chunk data for chunk number %d (index %d)" chunk-number chunk-index))
    (unless chunk-text
      (user-error "Could not retrieve text for chunk number %d" chunk-number))
    (unless chunk-filename
      (user-error "Could not retrieve filename for chunk number %d" chunk-number))

    ;; Pass the specific voice-params and the 0-based chunk-index to the execution function
    (let ((process (tlon-tts-execute-generation-request
                    chunk-number chunk-index chunk-text chunk-filename voice-params)))
      (tlon-tts-handle-generation-result process chunk-number chunk-filename))))

(defun tlon-tts--get-processed-text-from-markers (begin-marker end-marker)
  "Read text from buffer between BEGIN-MARKER and END-MARKER, process and trim it.
Processing involves removing a trailing break tag and then trimming whitespace.
Returns the processed text, or nil if markers are invalid or region is empty."
  (when (and begin-marker (marker-buffer begin-marker)
             end-marker (marker-buffer end-marker)
             (marker-position begin-marker) ; Ensure markers are valid
             (marker-position end-marker)
             (< (marker-position begin-marker) (marker-position end-marker)))
    (let* ((raw-text (buffer-substring-no-properties begin-marker end-marker))
           ;; Remove trailing break tag and surrounding whitespace before final trim
           (text-no-break (replace-regexp-in-string
                           (format "[ \t\n]*%s[ \t\n]*\\'" (tlon-md-get-tag-pattern "break"))
                           "" raw-text)))
      (string-trim text-no-break))))

(defun tlon-tts-get-voice-friendly-name (voice-id)
  "Return the friendly name for VOICE-ID for the current engine.
If VOICE-ID is nil or not found, return \"default\"."
  (if voice-id
      (let* ((voices-var (tlon-lookup tlon-tts-engines :voices-var :name tlon-tts-engine))
             (voices (when voices-var (symbol-value voices-var)))
             (name (tlon-lookup voices :name :id voice-id)))
        (or name voice-id "default")) ; Fallback to ID if name not found, then default
    "default"))

;;;;;; Prepare chunks

(defun tlon-tts-ensure-chunks-calculated ()
  "Ensure tlon-tts-chunks is populated, calculating if necessary."
  (unless tlon-tts-chunks
    (tlon-tts-calculate-chunks)))

(defun tlon-tts-ensure-chunks-ready ()
  "Ensure chunks are calculated and comments are inserted in the buffer."
  (tlon-tts-ensure-chunks-calculated)
  (tlon-tts-ensure-chunk-comments-inserted))

(defun tlon-tts-ensure-chunk-comments-inserted ()
  "Ensure chunk comments are present in the buffer."
  (save-excursion
    (goto-char (point-min))
    ;; Check if chunk comments already exist
    (unless (re-search-forward tlon-tts-chunk-comment-regex nil t)
      (tlon-tts-insert-chunk-comments))))

(defun tlon-tts-remove-existing-chunk-comments ()
  "Remove existing chunk comments from the narratable content area.
This ensures chunking logic operates on clean text without pre-existing
`<!-- Chunk N -->` comments."
  (save-excursion
    (save-restriction
      (widen)
      (let ((limit (save-excursion ; Determine end of content before local vars
                     (goto-char (point-min))
                     (if (re-search-forward tlon-tts-local-variables-section-start nil t)
                         (match-beginning 0)
                       (point-max)))))
        (goto-char (point-min)) ; Start search from beginning of buffer
        ;; Remove comments only within the content area (up to 'limit')
        (while (re-search-forward "^<!-- Chunk [0-9]+ -->\n?" limit t)
          (replace-match "" t t))))))

(defun tlon-tts-calculate-chunks ()
  "Calculate chunk boundaries and populate the tlon-tts-chunks data structure.
This function analyzes the current buffer content and determines how to split
it into chunks based on engine limits and voice changes, but does not modify
the buffer content itself. It ensures that chunking logic operates on text
free of pre-existing `<!-- Chunk N -->` comments by temporarily removing them
before analysis. For any engine, if `*-char-limit' is nil, will chunk by
paragraph regardless of size to work around voice degradation issues for
longer texts."
  (let ((char-limit (tlon-tts--engine-char-limit))
        (destination (tlon-tts-set-destination))
        (original-point (point))
        chunks)
    (tlon-tts-remove-existing-chunk-comments)
    (setq chunks (tlon-tts-read-into-chunks char-limit destination))
    (goto-char original-point)
    (setq tlon-tts-chunks chunks)))

(defun tlon-tts-set-destination ()
  "Set the path where the audio file will be saved."
  (let* ((staged-file (file-name-base tlon-tts-source))
	 (extension (cdr tlon-tts-audio))
	 (file-name (file-name-with-extension staged-file extension)))
    (file-name-concat default-directory file-name)))

(defun tlon-tts-read-into-chunks (&optional chunk-size destination)
  "Read the current buffer and return its content as a list of chunks.
If CHUNK-SIZE is non-nil, split string into chunks no larger than that size.
DESTINATION is the base output filename."
  (let ((local-vars (tlon-tts-pop-file-local-vars))
        (chunks (tlon-tts-break-into-chunks chunk-size destination))) ; Pass destination
    ;; Re-insert local variables section at the end without save-excursion
    (goto-char (point-max))
    (insert local-vars)
    chunks))

(defun tlon-tts-pop-file-local-vars ()
  "Remove and return the file-local variables section from the current buffer."
  (if (tlon-tts-staging-buffer-p)
      (save-excursion
	(goto-char (point-min))
	(re-search-forward tlon-tts-local-variables-section-start)
	(let* ((section (buffer-substring-no-properties (match-beginning 0) (point-max))))
	  (delete-region (match-beginning 0) (point-max))
	  section))))

(defun tlon-tts-break-into-chunks (chunk-size destination)
  "Break text in current buffer into chunks.
If CHUNK-SIZE is non-nil, break text into chunks no larger than CHUNK-SIZE,
respecting paragraph boundaries. Each chunk will include the maximum number of
paragraphs that fit within the size limit. DESTINATION is the base output
filename.

For any engine, if CHUNK-SIZE is nil the text is split paragraph-wise.
Each paragraph becomes a separate chunk, breaking before voice changes.

Voice changes specified in `tlon-tts-voice-chunks' always force a chunk break.

Each chunk stores its 1-based chunk number."
  ;; No save-excursion needed here; point management is internal to chunking.
  (let* ((content-start (tlon-tts--get-content-start-pos))
         (staging-buffer-name (tlon-tts-get-staging-buffer-name destination)) ; Calculate once
         (voice-chunk-list tlon-tts-voice-chunks)
         (use-paragraph-chunks (null chunk-size))
         chunks current-voice next-voice-change-pos next-voice-id
         (chunk-number-counter 0) ; Initialize 1-based chunk counter
         ;; Determine initial voice and update voice-chunk-list
         (initial-state (tlon-tts--determine-initial-voice voice-chunk-list)))
    (setq current-voice (car initial-state)
          voice-chunk-list (cdr initial-state))
    (goto-char content-start) ; Start chunking from actual content
    (let ((begin (point)))
      (while (< begin (point-max))
        ;; Determine position of the next voice change, if any
        (setq next-voice-change-pos (if voice-chunk-list (marker-position (caar voice-chunk-list)) most-positive-fixnum)
              next-voice-id (when voice-chunk-list (cdar voice-chunk-list)))
        ;; Calculate the end position for the current chunk
        (let ((end (tlon-tts--calculate-chunk-end begin chunk-size next-voice-change-pos use-paragraph-chunks)))
          ;; --- Add chunk ---
          (when (> end begin) ; Ensure we made progress
            (let* ((raw-text (buffer-substring-no-properties begin end))
                   ;; Remove trailing break tag and surrounding whitespace before final trim
                   (text-no-break (replace-regexp-in-string
                                   (format "[ \t\n]*%s[ \t\n]*\\'" (tlon-md-get-tag-pattern "break"))
                                   "" raw-text))
                   (trimmed-text (string-trim text-no-break)))
              ;; Only add chunk if trimmed text is not empty AND not just a break tag
              (when (and (not (string-empty-p trimmed-text))
                         (not (string-match-p (format "^%s$" (tlon-md-get-tag-pattern "break")) trimmed-text)))
                (setq chunk-number-counter (1+ chunk-number-counter)) ; Increment chunk number
                (let* ((filename (tlon-tts-get-chunk-name destination chunk-number-counter)) ; Use chunk number for filename
		       (voice-params (when current-voice (cons 'tlon-tts-voice current-voice)))
		       ;; Store original begin/end markers
		       (begin-marker (copy-marker begin))
                       (end-marker (copy-marker end))
                       ;; Create chunk using defined indices
                       (new-chunk (list trimmed-text        ; tlon-tts-chunk-index-text
                                        voice-params       ; tlon-tts-chunk-index-voice-params
                                        filename           ; tlon-tts-chunk-index-filename
                                        nil                ; tlon-tts-chunk-index-request-id
                                        'pending           ; tlon-tts-chunk-index-status
                                        staging-buffer-name ; tlon-tts-chunk-index-staging-buffer-name
                                        nil                ; tlon-tts-chunk-index-header-filename
                                        begin-marker       ; tlon-tts-chunk-index-begin-marker
                                        end-marker         ; tlon-tts-chunk-index-end-marker
                                        chunk-number-counter))) ; tlon-tts-chunk-index-chunk-number
                  ;; Only add the chunk if we got a valid filename
                  (when filename
                    (push new-chunk chunks))))))
          ;; --- Prepare for next iteration ---
          ;; Final safety check: If end <= begin here, force minimal progress.
          (when (and (<= end begin) (< begin (point-max)))
	    (message "Warning: Forcing minimal progress at position %d due to end <= begin" begin)
	    (goto-char begin)
	    (forward-char 1)
	    (setq end (point)))
	  (setq begin end) ; Update begin for the next loop iteration
	  ;; Update voice state if we reached a voice change point
	  (let ((new-voice-state (tlon-tts--update-voice-state begin next-voice-change-pos next-voice-id current-voice voice-chunk-list)))
	    (setq current-voice (car new-voice-state)
		  voice-chunk-list (cdr new-voice-state))))))
    (nreverse chunks)))

;;;;;; Chunking Helpers

(defun tlon-tts--get-content-start-pos ()
  "Return the starting position of narratable content in the current buffer.
This is typically after the YAML metadata section, or at the beginning of the
buffer if only a file-local variables section is present."
  (save-excursion
    (goto-char (point-min))
    (or (cdr (tlon-get-delimited-region-pos tlon-yaml-delimiter)) ; After YAML if present
        (point-min)))) ; Otherwise, start at the beginning

(defun tlon-tts--determine-initial-voice (voice-chunk-list)
  "Determine the initial voice and the remaining VOICE-CHUNK-LIST.
Returns (cons INITIAL-VOICE REMAINING-VOICE-CHUNK-LIST)."
  (let ((first-voice-chunk (car voice-chunk-list))
	initial-voice remaining-list)
    (if (and first-voice-chunk (= (marker-position (car first-voice-chunk)) (point-min)))
	;; If first voice change is at the very beginning
	(setq initial-voice (cdr first-voice-chunk)
	      remaining-list (cdr voice-chunk-list))
      ;; Otherwise, use the default voice for the file if bound, else nil
      (setq initial-voice (when (boundp 'tlon-tts-voice) tlon-tts-voice)
	    remaining-list voice-chunk-list))
    (cons initial-voice remaining-list)))

(defun tlon-tts--calculate-chunk-end (begin chunk-size next-voice-change-pos use-paragraph-chunks)
  "Calculate the end position for the next chunk.
BEGIN is the start position of the chunk, CHUNK-SIZE is the maximum size of the
chunk, NEXT-VOICE-CHANGE-POS is the position of the next voice change, and
USE-PARAGRAPH-CHUNKS is a boolean indicating whether to use paragraph chunking."
  (let (end)
    (cond
     ;; --- Case 1: Paragraph chunking ---
     (use-paragraph-chunks
      (goto-char begin)
      (condition-case nil (forward-paragraph) (error (goto-char (point-max))))
      (setq end (min (point) next-voice-change-pos)))
     ;; --- Case 2: Character limit chunking ---
     (t
      (let (potential-end)
	(setq potential-end (min (point-max) (+ begin chunk-size) next-voice-change-pos))
	(goto-char potential-end)
	;; If we are not at a voice change point or end of buffer, move back to paragraph boundary
	(unless (or (= potential-end next-voice-change-pos) (eobp))
	  (backward-paragraph)
	  ;; Ensure we don't move back before 'begin' if a paragraph is huge
	  (when (< (point) begin) (goto-char begin)))
	(setq end (point))
	;; If moving back put us exactly at 'begin', force progress.
	(when (and (= end begin) (< begin (point-max)))
	  (setq end potential-end))
	;; Adjust end point for SSML break tags (Only for char limit mode)
	(let ((adjusted-end end))
	  (goto-char adjusted-end)
	  (tlon-tts-move-point-before-break-tag) ; This moves point backward
	  ;; Only accept the adjustment if it doesn't move us back to or before 'begin'
	  (when (> (point) begin)
	    (setq adjusted-end (point)))
	  (setq end adjusted-end))))) ; Update end with the potentially adjusted value
    end))

(defun tlon-tts--update-voice-state (begin next-voice-change-pos next-voice-id current-voice voice-chunk-list)
  "Update current-voice and voice-chunk-list if BEGIN is at a voice change point.
CURRENT-VOICE is the current voice, VOICE-CHUNK-LIST is the list of voice
changes, NEXT-VOICE-CHANGE-POS is the position of the next voice change, and
NEXT-VOICE-ID is the ID of the next voice."
  (if (= begin next-voice-change-pos)
      (cons next-voice-id (cdr voice-chunk-list))
    (cons current-voice voice-chunk-list)))

(defun tlon-tts-move-point-before-break-tag ()
  "Move point before `break' tag if it immediately follows it.
This is to prevent Elevenlabs from inserting weird audio artifacts."
  (let ((break-tag (tlon-md-get-tag-pattern "break")))
    (while (looking-back (concat break-tag "[ \t\n]*") nil)
      (re-search-backward break-tag nil t)
      (goto-char (match-beginning 0)))))

;;;;;; Process chunks

(defun tlon-tts-process-chunks ()
  "Start processing chunks.
If `tlon-tts-skip-existing-chunks-when-narrating-buffer' is non-nil, start from
the first chunk whose audio file doesn't exist. Otherwise, start from the first
chunk. Subsequent chunks are triggered by the sentinel."
  (let ((total-chunks (length tlon-tts-chunks))
        (start-index 0))
    (when (and tlon-tts-generate-missing-chunks-only (> total-chunks 0))
      ;; Find the first chunk index whose file does not exist
      (let ((found-missing nil)
            (current-index 0))
        (while (and (not found-missing) (< current-index total-chunks))
          (let ((chunk-file (nth tlon-tts-chunk-index-filename (nth current-index tlon-tts-chunks))))
            (unless (file-exists-p chunk-file)
              (setq start-index current-index
                    found-missing t)))
          (setq current-index (1+ current-index)))
        ;; If all files exist and we only process missing, set start index beyond total
        (unless found-missing
          (setq start-index total-chunks))))
    (setq tlon-tts-chunks-to-process (- total-chunks start-index))
    (if (> tlon-tts-chunks-to-process 0)
        (progn
          (message "Starting TTS processing from chunk %d/%d." (1+ start-index) total-chunks)
          (tlon-tts-generate-audio start-index)) ; Start from the determined index
      (if tlon-tts-generate-missing-chunks-only
          (message "All %d chunk audio files already exist. Nothing to process." total-chunks)
        (message "No TTS chunks to process.")))))

(defun tlon-tts-generate-audio (chunk-index)
  "Generate audio for the chunk at CHUNK-INDEX.
Triggers the engine-specific request function and sets up the process sentinel.
The text for the chunk is read live from the buffer using its markers."
  (let* ((chunk-data (nth chunk-index tlon-tts-chunks))
         (begin-marker (nth tlon-tts-chunk-index-begin-marker chunk-data))
         (end-marker (nth tlon-tts-chunk-index-end-marker chunk-data))
         (string (tlon-tts--get-processed-text-from-markers begin-marker end-marker))
         (voice-params (nth tlon-tts-chunk-index-voice-params chunk-data))
         (file (nth tlon-tts-chunk-index-filename chunk-data))
         (fun (tlon-lookup tlon-tts-engines :request-fun :name tlon-tts-engine))
         ;; Pass chunk-index to the request function
         (request (if (and string (not (string-empty-p string))) ; Only proceed if text is non-empty
                      (funcall fun string file voice-params chunk-index)
                    nil))) ; No request if string is nil or empty

    (if request
        (progn
          ;; Mark chunk as running
          (setf (nth tlon-tts-chunk-index-status chunk-data) 'running)
          (when tlon-debug (message "Debug: Running command for chunk %d: %s" chunk-index request))
          (let ((process (start-process-shell-command (format "generate audio %d" chunk-index) nil request))
                (originating-buffer-name (buffer-name))) ; Capture the name of the current (staging) buffer
            (set-process-sentinel process
                                  (lambda (process event)
                                    ;; Pass the captured buffer name to the sentinel handler
                                    (tlon-tts-process-chunk-sentinel process event chunk-index originating-buffer-name)))))
      ;; If string was nil or empty, mark as completed (or failed) and trigger next if in full buffer mode
      (progn
        (message "Skipping chunk %d as its content is empty after processing." (1+ chunk-index))
        (setf (nth tlon-tts-chunk-index-status chunk-data) 'completed) ; Or 'skipped
        ;; Manually trigger sentinel logic for next chunk if in full buffer mode
        ;; This mimics parts of tlon-tts-process-chunk-sentinel's "finished" branch
        (unless tlon-tts-user-selected-chunks-to-process ; Only if in full buffer mode
          (setq tlon-tts-chunks-to-process (1- tlon-tts-chunks-to-process))
          (let ((next-chunk-index (1+ chunk-index)))
            (if (and (> tlon-tts-chunks-to-process 0)
                     (< next-chunk-index (length tlon-tts-chunks)))
                (tlon-tts-generate-audio next-chunk-index)
              (when (<= tlon-tts-chunks-to-process 0)
                (message "All buffer chunks processed (or skipped). Call `M-x tlon-tts-finalize-audio-processing' (`H-r z f') to join and finalize.")))))))))

(defun tlon-tts-get-chunk-name (file chunk-number)
  "Return the name of the chunk file for CHUNK-NUMBER of FILE.
CHUNK-NUMBER is 1-based."
  (let ((extension (file-name-extension file))
        (file-name-sans-extension (file-name-sans-extension file)))
    (format "%s-chunk-%03d.%s" file-name-sans-extension chunk-number extension)))

(defun tlon-tts-open-file (file)
  "Open generated TTS FILE."
  (shell-command (format "open %s" file)))

(defun tlon-tts-process-chunk-sentinel (process event chunk-index captured-staging-buffer-name)
  "Process sentinel for TTS chunk generation at CHUNK-INDEX.
PROCESS is the process object, EVENT is the event string, and
CAPTURED-STAGING-BUFFER-NAME is the name of the buffer this process belongs to."
  (let ((target-buffer (get-buffer captured-staging-buffer-name)))
    (if (not (buffer-live-p target-buffer)) ; Check if buffer is live
        (progn
          (message "Error: Staging buffer '%s' no longer exists or is dead. Cannot process sentinel for chunk %d."
                   captured-staging-buffer-name chunk-index)
          ;; Attempt to gracefully stop further processing if possible, though state might be inconsistent.
          (setq tlon-tts-chunks-to-process 0) ; Stop further full-buffer processing
          (setq tlon-tts-user-selected-chunks-to-process nil) ; Stop further selected-chunk processing
          (error "Staging buffer %s for chunk %d sentinel processing is missing or dead"
                 captured-staging-buffer-name chunk-index))
      ;; --- Buffer exists and is live, proceed within its context ---
      (with-current-buffer target-buffer
        (let* ((chunk-data (nth chunk-index tlon-tts-chunks)) ; tlon-tts-chunks is now correctly resolved
               (file (when chunk-data (nth tlon-tts-chunk-index-filename chunk-data)))
               (staging-buffer-name-from-chunk (when chunk-data (nth tlon-tts-chunk-index-staging-buffer-name chunk-data)))
               (header-file (when chunk-data (nth tlon-tts-chunk-index-header-filename chunk-data))))

          ;; If chunk_data is nil (e.g. tlon-tts-chunks was cleared or chunk-index is out of bounds)
          (unless chunk-data
            (message "Error: Could not retrieve chunk data for chunk %d in buffer '%s'. Sentinel cannot proceed."
                     chunk-index captured-staging-buffer-name)
            (error "Missing chunk data for chunk %d in sentinel" chunk-index)
            (cl-return-from tlon-tts-process-chunk-sentinel)) ; Exit sentinel

          ;; Defensive check
          (unless (equal staging-buffer-name-from-chunk captured-staging-buffer-name)
            (error "Sentinel context mismatch: captured '%s', chunk data '%s' for chunk %d"
                   captured-staging-buffer-name staging-buffer-name-from-chunk chunk-index))

          (cond
           ((string-match "finished" event) ; Process finished successfully
            (let* (;; Read headers from the temporary file
                   (output (if (and header-file (file-exists-p header-file))
                               (with-temp-buffer
                                 (insert-file-contents header-file)
                                 (buffer-string))
                             ""))
                   (request-id (tlon-tts--parse-elevenlabs-request-id output)))

              (when (and header-file (file-exists-p header-file))
                (delete-file header-file))

              (setf (nth tlon-tts-chunk-index-request-id chunk-data) request-id)
              (setf (nth tlon-tts-chunk-index-status chunk-data) 'completed)
              (setq tlon-tts-chunks-to-process (1- tlon-tts-chunks-to-process))
              (message "Chunk %d processed successfully. (%s)"
                       (1+ chunk-index)
                       (if tlon-tts-user-selected-chunks-to-process "selected" "full buffer"))

              (if tlon-tts-user-selected-chunks-to-process
                  (progn
                    (setq tlon-tts-user-selected-chunks-to-process (cdr tlon-tts-user-selected-chunks-to-process))
                    (if tlon-tts-user-selected-chunks-to-process
                        ;; If more user-selected chunks, process the next one
                        (tlon-tts-generate-audio (car tlon-tts-user-selected-chunks-to-process))
                      ;; All user-selected chunks are done. Do NOT finalize here.
                      (message "All selected chunks processed.")))
                ;; --- Not processing a user-selected list (i.e., full buffer narration) ---
                (let ((next-chunk-index (1+ chunk-index)))
                  (if (and (> tlon-tts-chunks-to-process 0) ; Still chunks left in full buffer mode
                           (< next-chunk-index (length tlon-tts-chunks)))
                      (tlon-tts-generate-audio next-chunk-index)
                    (when (<= tlon-tts-chunks-to-process 0)
                      (message "All buffer chunks processed. Use M-x tlon-tts-finalize-audio-processing to join and finalize.")))))))

           ((string-match "exited abnormally" event)
            (setf (nth tlon-tts-chunk-index-status chunk-data) 'failed)
            (message "Error processing chunk %d (%s): %s" chunk-index file event)
            (when-let ((buffer (process-buffer process)))
              (with-current-buffer buffer
                (message "Error output for chunk %d:\n%s" chunk-index (buffer-string)))))
           (t
            (message "Process %s (chunk %d): Event occurred - %s" (process-name process) chunk-index event))))))))

(defun tlon-tts--parse-elevenlabs-request-id (output)
  "Parse the request-id header from curl OUTPUT by checking lines."
  (let (request-id found)
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      ;; Iterate through each line
      (while (not (eobp))
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (line (buffer-substring-no-properties line-start line-end)))
         ;; Check if line starts with "request-id:", case-insensitive
         (let ((case-fold-search t)) ; Enable case-insensitive matching for this check
           (when (string-match-p (rx-to-string '(seq bol (0+ space) "request-id:" (0+ space)))
                                 line)
             ;; Extract the value after the colon, trimming whitespace
             (setq request-id (string-trim (substring line (match-end 0))))
             ;; Stop searching once found
             (setq found t)
             (goto-char (point-max))))) ; Exit loop early
       (unless found (forward-line 1)))) ; Move to next line if not found
   request-id)) ; Return the found ID or nil

(defun tlon-tts-finish-processing (last-chunk-file)
  "Finalize TTS process.
Append silence (sync), then normalize & join (async), delete, open.
LAST-CHUNK-FILE is the last chunk file processed."
  (let* ((final-output-file (tlon-tts-get-original-filename last-chunk-file))
         (api-generated-chunks (tlon-tts-get-list-of-chunks final-output-file))
         temp-silence-appended-files-to-cleanup) ; Will hold list of temp files if silence is appended
    (message "All API chunks processed. Starting final audio processing for %s..."
             (file-name-nondirectory final-output-file))
    (if (not api-generated-chunks)
        (user-error "No API-generated chunk files found for %s" final-output-file)
      (let (files-for-next-step input-files-are-temporary-p)
        (if (tlon-tts-append-silence-to-chunks-p final-output-file)
            (progn
              (setq temp-silence-appended-files-to-cleanup (tlon-tts-append-silence-to-chunks api-generated-chunks))
              (setq files-for-next-step temp-silence-appended-files-to-cleanup)
              (setq input-files-are-temporary-p t)) ; Silence-appended files are temporary
          (setq files-for-next-step api-generated-chunks)
          (setq input-files-are-temporary-p nil)) ; Original API chunks are not considered temp here

        (if files-for-next-step
            (if tlon-tts-normalize-audio
                (progn
                  (message "Normalization enabled. Starting normalization for %s..." (file-name-nondirectory final-output-file))
                  (tlon-tts-async-start-normalization files-for-next-step
                                                      final-output-file
                                                      api-generated-chunks
                                                      temp-silence-appended-files-to-cleanup))
              (progn
                (message "Normalization skipped. Proceeding to join %s..." (file-name-nondirectory final-output-file))
                (tlon-tts-async-start-joining files-for-next-step
                                              final-output-file
                                              api-generated-chunks
                                              temp-silence-appended-files-to-cleanup
                                              input-files-are-temporary-p))) ; Pass flag
          (user-error "No chunk files available for %s to process" final-output-file))))))

(defun tlon-tts-async-start-normalization (files-to-normalize final-output-file api-generated-chunk-files temp-silence-appended-files-to-cleanup)
  "Asynchronously normalize FILES-TO-NORMALIZE to temporary files.
FINAL-OUTPUT-FILE is the final output file path. FILES-TO-NORMALIZE are the
files that ffmpeg will process (either original API chunks or temp
silence-appended ones). API-GENERATED-CHUNK-FILES are the original files from
the TTS engine. TEMP-SILENCE-APPENDED-FILES-TO-CLEANUP are temporary files
created by appending silence, if any."
  (message "Starting asynchronous normalization for %d chunks..." (length files-to-normalize))
  (let* ((temp-normalized-output-files (mapcar (lambda (_) (make-temp-file "tts_normalized_" nil ".mp3"))
                                               files-to-normalize))
         (commands (mapconcat
                    (lambda (pair)
                      (let ((input-file (nth 0 pair))  ; First element of the sublist
                            (output-file (nth 1 pair))) ; Second element of the sublist
                        (unless (stringp input-file)
                          (error "Invalid input file in normalization pair: %S (output: %S)" input-file output-file))
                        (unless (stringp output-file)
                          (error "Invalid output file (temp normalized) in normalization pair: %S (input: %S)" output-file input-file))
                        (format tlon-tts-ffmpeg-normalize
                                (shell-quote-argument input-file)
                                (shell-quote-argument output-file))))
                    (cl-mapcar #'list files-to-normalize temp-normalized-output-files)
                    " && "))) ; Chain commands with &&

    (when tlon-debug (message "Normalization command: %s" commands))

    (let ((process (start-process-shell-command
                    "tts-normalize"
                    nil ; No dedicated buffer needed for combined command output
                    commands)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (tlon-tts-normalization-sentinel proc event
                                          final-output-file
                                          temp-normalized-output-files ; These are the files to join
                                          api-generated-chunk-files
                                          temp-silence-appended-files-to-cleanup
                                          t)))))) ; Normalized files are temporary

(defun tlon-tts-normalization-sentinel (process event final-output-file temp-normalized-output-files api-generated-chunk-files temp-silence-appended-files-to-cleanup normalized-files-are-temporary-p)
  "Sentinel for the asynchronous normalization PROCESS.
EVENT is the event string, FINAL-OUTPUT-FILE is the final output file path,
TEMP-NORMALIZED-OUTPUT-FILES are the normalized files, API-GENERATED-CHUNK-FILES
are the original files from the TTS engine,
TEMP-SILENCE-APPENDED-FILES-TO-CLEANUP are temporary files created by appending
silence, and NORMALIZED-FILES-ARE-TEMPORARY-P is a boolean indicating if the
normalized files are temporary and should be cleaned up after joining."
  (cond
   ((string-match "finished" event)
    (message "Normalization finished. Starting asynchronous joining...")
    (tlon-tts-async-start-joining temp-normalized-output-files ; Files to join
                                  final-output-file
                                  api-generated-chunk-files
                                  temp-silence-appended-files-to-cleanup
                                  normalized-files-are-temporary-p)) ; Pass the flag
   ((string-match "exited abnormally" event)
    (message "Error during asynchronous normalization: %s" event)
    (when-let ((buffer (process-buffer process)))
      (with-current-buffer buffer
        (message "Normalization error output:\n%s" (buffer-string))))
    ;; Clean up temp normalized files on error
    (dolist (temp-file temp-normalized-output-files)
      (when (file-exists-p temp-file) (delete-file temp-file)))
    ;; Also clean up silence-appended files if they exist and normalization failed
    ;; Also clean up silence-appended files if they exist and normalization failed
    (when temp-silence-appended-files-to-cleanup
      (dolist (temp-file temp-silence-appended-files-to-cleanup)
        (when (file-exists-p temp-file) (delete-file temp-file))))
    (message "Normalization failed. All temporary files related to normalization cleaned up."))
   (t (message "Normalization process event: %s" event))))

(defun tlon-tts-async-start-joining (files-to-join-input final-output-file api-generated-chunk-files temp-silence-appended-files-if-any input-files-are-temporary-p)
  "Asynchronously join FILES-TO-JOIN-INPUT into FINAL-OUTPUT-FILE.
API-GENERATED-CHUNK-FILES are the original files from the TTS engine.
TEMP-SILENCE-APPENDED-FILES-IF-ANY are temporary files created by appending
silence, if any. INPUT-FILES-ARE-TEMPORARY-P indicates if
FILES-TO-JOIN-INPUT should be cleaned up."
  (let* ((ffmpeg-list-file (tlon-tts-create-list-of-chunks files-to-join-input))
        (command (format "ffmpeg -y -f concat -safe 0 -i %s -c copy %s"
                         (shell-quote-argument ffmpeg-list-file)
                         (shell-quote-argument final-output-file))))
    (when tlon-debug (message "Joining command: %s" command))
    (let ((process (start-process-shell-command
                    "tts-join"
                    nil ; No dedicated buffer
                    command)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (tlon-tts-joining-sentinel proc event
                                    final-output-file
                                    files-to-join-input ; Pass the files that were joined
                                    ffmpeg-list-file
                                    api-generated-chunk-files ; Pass original API chunks
                                    temp-silence-appended-files-if-any
                                    input-files-are-temporary-p)))))) ; Pass the flag

(defun tlon-tts-joining-sentinel (process event final-output-file files-that-were-joined ffmpeg-list-file original-api-chunks temp-silence-appended-files-if-any joined_files_were_temporary_p)
  "Sentinel for the asynchronous joining PROCESS.
EVENT is the event string. FINAL-OUTPUT-FILE is the final output file path.
FILES-THAT-WERE-JOINED are the files directly input to the ffmpeg concat
command. FFMPEG-LIST-FILE is the temporary list file used by ffmpeg.
ORIGINAL-API-CHUNKS are the initial chunk files from the TTS engine.
TEMP-SILENCE-APPENDED-FILES-IF-ANY are temporary files created by appending
silence. JOINED_FILES_WERE_TEMPORARY_P indicates if FILES-THAT-WERE-JOINED
should be cleaned up."
  (let (success)
    (cond
     ((string-match "finished" event)
      (message "Successfully joined chunks into %s." (file-name-nondirectory final-output-file))
      (setq success t)
      ;; Deletion of original API chunks is handled below, based on tlon-tts-delete-file-chunks
      (dired (file-name-directory final-output-file))
      (shell-command (format "open '%s'" final-output-file))
      (message "TTS narration complete for %s" (file-name-nondirectory final-output-file)))
     ((string-match "exited abnormally" event)
      (message "Error joining chunks asynchronously: %s" event)
      (when-let ((buffer (process-buffer process)))
        (with-current-buffer buffer
          (message "Joining error output:\n%s" (buffer-string))))
      (setq success nil))
     (t (message "Joining process event: %s" event)))

    ;; Clean up temporary files
    (message "Cleaning up temporary files...")
    (when joined_files_were_temporary_p
      (dolist (temp-file files-that-were-joined)
        (when (file-exists-p temp-file)
          (message "Deleting joined temporary file: %s" temp-file)
          (delete-file temp-file))))

    (when temp-silence-appended-files-if-any
      (dolist (temp-file temp-silence-appended-files-if-any)
        ;; Avoid deleting twice if files-that-were-joined were the silence-appended files
        (unless (and joined_files_were_temporary_p (member temp-file files-that-were-joined))
          (when (file-exists-p temp-file)
            (message "Deleting temporary silence-appended file: %s" temp-file)
            (delete-file temp-file)))))

    (when (file-exists-p ffmpeg-list-file)
      (message "Deleting ffmpeg list file: %s" ffmpeg-list-file)
      (delete-file ffmpeg-list-file))

    ;; Delete original API-generated chunk files if the user option is set
    (when (and success tlon-tts-delete-file-chunks)
      (message "Deleting original API-generated chunk files as per user option...")
      (dolist (chunk-file original-api-chunks)
        (when (file-exists-p chunk-file)
          (message "Deleting original API chunk: %s" chunk-file)
          (delete-file chunk-file))))
    
    (message "Temporary file cleanup finished.")

    (when (and success (derived-mode-p 'dired-mode) (string= (buffer-file-name) (file-name-directory final-output-file)))
      (revert-buffer))))

;;;###autoload
(defun tlon-tts-finalize-audio-processing (&optional file)
  "Manually trigger final audio processing for FILE.
This includes appending silence (if applicable), normalizing, and joining
chunks. Prompts for FILE if not provided or determinable from context."
  (interactive)
  (let* ((base-file (tlon-tts-get-base-audio-file-interactive file))
         (chunks (tlon-tts-get-list-of-chunks base-file))
         (first-chunk-file (car chunks)))
    (unless first-chunk-file
      (user-error "No chunk files found for %s to finalize" base-file))
    (message "Manually starting final audio processing for %s..." (file-name-nondirectory base-file))
    (tlon-tts-finish-processing first-chunk-file)))

(defun tlon-tts-get-list-of-chunks (file)
  "Return a list of the existing file chunks for FILE.
The file chunks are the files in the same directory as FILE that have the same
base name and extension as FILE, but with '-chunk-NNN' appended to the base
name before the extension. The list is sorted alphabetically, so that
numeric and letter-suffixed chunk files (e.g., 002, 002a, 002b, 003) are
ordered as required. If no chunks are found, return nil."
  (let* ((dir (file-name-directory file))
         (base-name (file-name-base file))
         (extension (file-name-extension file))
         ;; Match base-chunk-DDD[a-z...].ext
         (pattern (concat "^" (regexp-quote base-name) "-chunk-[0-9]+[a-z]*\\." (regexp-quote extension) "$"))
         (files (directory-files dir t pattern)))
    (sort files #'string-lessp)))

(defun tlon-tts-get-base-audio-file-interactive (&optional file-hint)
  "Return the base audio file path for operations.
If FILE-HINT is nil, use the file visited by the current buffer, the file at
point in Dired, or prompt the user for a file (removing chunk numbers if
necessary)."
  (let ((file (or file-hint
                  (buffer-file-name)
                  (and (derived-mode-p 'dired-mode) (dired-get-filename))
                  (read-file-name "Base audio file: "))))
    (if (string-match-p "-chunk-[0-9]+\\." file)
        (tlon-tts-get-original-filename file)
      file)))

(defun tlon-tts-delete-chunks-of-file (&optional file)
  "Delete the chunks of FILE. Also delete the staging buffer."
  (interactive)
  (let* ((base-file (tlon-tts-get-base-audio-file-interactive file))
	 (buffer-name (tlon-tts-get-staging-buffer-name base-file)))
    (dolist (chunk-file (tlon-tts-get-list-of-chunks base-file))
      (delete-file chunk-file 'trash))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (when (derived-mode-p 'dired-mode)
      (revert-buffer))))

(defun tlon-tts-create-list-of-chunks (files)
  "Create a temporary file with a list of audio FILES for use with `ffmpeg'."
  (let ((temp-file-list (make-temp-file "files" nil ".txt")))
    (with-temp-file temp-file-list
      (dolist (file files)
	(insert (format "file '%s'\n" (expand-file-name file)))))
    temp-file-list))

(defun tlon-tts-get-first-n-chunk-files (file n)
  "Return a list of the first N chunk filenames of FILE."
  (let ((all-chunks (tlon-tts-get-list-of-chunks file)))
    (cl-subseq all-chunks 0 (min n (length all-chunks)))))

(defun tlon-tts-get-original-filename (file)
  "Return the filename from which chunk FILE derives.
Removes the '-chunk-NNN' part."
  (let* ((base-name (file-name-sans-extension file))
         (extension (file-name-extension file))
         (original-base-name (replace-regexp-in-string "-chunk-[0-9]+\\'" "" base-name)))
    (format "%s.%s" original-base-name extension)))

;;;;;; Set file-local variables

(defun tlon-tts-get-file-local-or-override (var-names parameters)
  "Return the values of VAR-NAMES if found in parameters, else the file-local var.
VAR-NAMES is a list of variable names. PARAMETERS is an alist (list of cons
cells) where the car is the name of the file-local variable and the cdr is its
overriding value."
  (let (values)
    (dolist (var-name var-names)
      (if-let ((override (assoc var-name parameters)))
	  (push (cdr override) values)
	(when (boundp var-name)
	  (push (symbol-value var-name) values))))
    (nreverse values)))

;;;;;;; Staging buffer

(defun tlon-tts-set-staging-buffer ()
  "Set `tlon-tts-staging-buffer-p' to t."
  (tlon-tts-set-file-local-variable
   'tlon-tts-staging-buffer-p t))

;;;;;;; File or buffer

(defun tlon-tts-set-source (&optional file)
  "Set the file or buffer name with the content for the current TTS process.
If FILE is nil, use the name of the file visited by the current buffer, or the
buffer name if the buffer is not visiting a file."
  (tlon-tts-set-file-local-variable
   'tlon-tts-source (or file (buffer-file-name) (buffer-name))))

(defun tlon-tts-get-selection ()
  "Return the current selection, plus any footnotes referenced therein."
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end)))
	(footnotes (make-hash-table :test 'equal))
	(beg (region-beginning))
	(end (region-end)))
    (with-current-buffer (current-buffer)
      (save-excursion
	(goto-char beg)
	(while (re-search-forward markdown-regex-footnote end t)
	  (let* ((ref (match-string 2))
		 (note-number (string-to-number ref))
		 (note-content (tlon-md-get-note note-number 'content-only)))
	    (puthash ref note-content footnotes)))))
    (let ((footnotes-section
	   (mapconcat (lambda (key)
			(concat "[^" key "]: " (gethash key footnotes)))
		      (hash-table-keys footnotes) "\n")))
      (if (string-empty-p footnotes-section)
	  text
	(concat text "\n\n" footnotes-section)))))

;;;;;;; Language

(defun tlon-tts-set-language (&optional language)
  "Set the value of the language of the current process.
If LANGUAGE is nil, look up the language of the current file or prompt the user
to select it."
  (tlon-tts-set-file-local-variable 'tlon-tts-language (tlon-tts-get-current-language language)))

;; TODO: decide if this getter function should also be used for the other values (engine, voice, etc)
(defun tlon-tts-get-current-language (&optional language)
  "Return the value of the language of the current process.
If LANGUAGE is non-nil, return it. Otherwise, get the value of
`tlon-tts-set-language', look up the language of the current file or prompt the
user to select it, in that order."
  (or language
      (when (boundp 'tlon-tts-language)
	tlon-tts-language)
      (when (boundp 'tlon-tts-source)
	(tlon-repo-lookup :language :dir (tlon-get-repo-from-file tlon-tts-source)))
      (tlon-get-language-in-file)
      (tlon-select-language 'code 'babel)))

;;;;;;; Engine

(defun tlon-tts-set-engine (&optional engine)
  "Set the engine of the current voice.
If ENGINE is nil, use the globally set TTS engine."
  (tlon-tts-set-file-local-variable
   'tlon-tts-engine (or engine tlon-tts-global-engine)))

;;;;;;; Audio

(defun tlon-tts-set-audio (&optional audio)
  "Set the audio settings for the current process.
If AUDIO is nil, use the audio settings of the current engine."
  (tlon-tts-set-file-local-variable
   'tlon-tts-audio (or audio (tlon-tts-get-output-format))))

(defun tlon-tts-get-output-format ()
  "Return the output format for the current TTS engine.
The output format is a cons cell with the format name and extension."
  (symbol-value (tlon-lookup tlon-tts-engines :audio-var :name tlon-tts-engine)))

;;;;;;; Voice

(defun tlon-tts-set-voice (&optional voice)
  "Set the main voice for the current process.
If VOICE is nil, prompt the user to select a voice."
  (let* ((voice (or voice (tlon-tts-select-voice))))
    (tlon-tts-set-file-local-variable 'tlon-tts-voice voice)))

(defun tlon-tts-select-voice ()
  "Select from available voices in the current language for the current TTS engine."
  (let* ((voices (symbol-value (tlon-lookup tlon-tts-engines :voices-var :name tlon-tts-engine)))
	 (voices-in-lang (apply #'append
				(mapcar (lambda (language)
					  "Return lists of monolingual and multilingual voices."
					  (tlon-lookup-all voices :id :language language))
					`(,(tlon-tts-get-current-language) "multilingual"))))
	 (voices-cons (mapcar (lambda (id)
				"Return a cons cell of voice metadata and voices."
				(let* ((gender (tlon-lookup voices :gender :id id))
				       (name (tlon-lookup voices :name :id id))
				       (role (or (tlon-lookup voices :role :id id) ""))
				       (display-name (if name name id))
				       (metadata (format "%-20.20s %-10.20s %-10.20s"
							 display-name gender role)))
				  (cons metadata id)))
			      voices-in-lang)))
    (alist-get (completing-read "Voice: " voices-cons) voices-cons nil nil #'string=)))

(defun tlon-tts-get-voice-at-point ()
  "Get the value of the \"name\" attribute of the `voice' tag enclosing point.
When point is not enclosed by a `voice' tag, return the value of
`tlon-tts-voice'. This is because each chunk is wrapped around a `voice' tag
`tlon-tts-voice' as its value when sent via the API. Thus, although the tag is
not present in the buffer, the text will be read as if it was present."
  (or (when (thing-at-point-looking-at (tlon-md-get-tag-pattern "voice"))
	(match-string 4))
      tlon-tts-voice))

;;;;;;; Locale

(defun tlon-tts-set-locale (&optional locale)
  "Set the locale of the current voice.
If LOCALE is nil, use the locale of the current voice."
  (tlon-tts-set-file-local-variable
   'tlon-tts-locale (or locale (tlon-tts-get-locale))))

(defun tlon-tts-get-locale ()
  "Get the locale of the current voice."
  (let ((voice (when (boundp 'tlon-tts-voice) tlon-tts-voice)))
    (catch 'found
      (dolist (var (tlon-lookup-all tlon-tts-engines :voices-var))
	(when-let* ((code (tlon-lookup (symbol-value var) :language :id voice))
		    (locale (tlon-lookup tlon-languages-properties :locale :code code)))
	  (throw 'found locale))))))

;;;;;; Audio truncation

;;;###autoload
(defun tlon-tts-truncate-audio-file (file miliseconds)
  "Remove the last MILISECONDS from FILE."
  (interactive (list (files-extras-read-file)
		     (read-number "Miliseconds: ")))
  (let ((duration (tlon-tts-get-new-duration file miliseconds))
	(output-filename (tlon-tts-get-output-filename file miliseconds)))
    (shell-command (format "mp3splt 0.0 %s %2$s -o %2$s" duration file))
    (shell-command (format "mv %s %s" output-filename file))))

;;;###autoload
(defun tlon-tts-normalize-audio-files (&optional files)
  "Normalize the selected audio FILES using ffmpeg loudnorm filter.
If invoked from dired with marked files, those are used.  Otherwise
prompt for one file.  Each file is normalized in place, using the
settings in `tlon-tts-ffmpeg-normalize'."
  (interactive)
  (let* ((targets (or files
                      (if (derived-mode-p 'dired-mode)
                          (dired-get-marked-files)
                        (list (files-extras-read-file)))))
         (done 0))
    (dolist (file targets)
      (when (file-exists-p file)
        (let* ((tmp (make-temp-file "tts_norm_" nil
                                    (concat "." (file-name-extension file))))
               (cmd (format tlon-tts-ffmpeg-normalize
                            (shell-quote-argument file)
                            (shell-quote-argument tmp))))
          (message "Normalizing %s..." (file-name-nondirectory file))
          (when (zerop (shell-command cmd))
            (rename-file tmp file t)
            (cl-incf done)
            (message "Normalized %s" (file-name-nondirectory file))))))
    (message "Normalized %d file(s)" done)))

(defun tlon-tts-get-output-filename (file miliseconds)
  "Return the output filename for FILE minus MILISECONDS."
  (let* ((elts (split-string (tlon-tts-get-new-duration file miliseconds) "\\."))
	 (dir (file-name-directory file))
	 (base (file-name-base file))
	 (filename (apply 'format "%s_00m_00s__%sm_%ss_%sh.mp3" base elts)))
    (file-name-concat dir filename)))

(defun tlon-tts-get-new-duration (file miliseconds)
  "Return the duration of FILE minus MILISECONDS.
The duration is returned in the format minutes.seconds.hundreds, which is the
format used by `mp3split'."
  (let* ((output (string-chop-newline
		  (shell-command-to-string
		   (format "ffmpeg -i \"%s\"  2>&1 | grep \"Duration\" | awk '{print $2}' | tr -d ,"
			   file)))))
    (cl-destructuring-bind (h m s) (split-string output ":")
      (let* ((seconds (+ (* (string-to-number h) 3600)
			 (* (string-to-number m) 60)
			 (string-to-number s)))
	     (new-duration (/ (- (* 1000 seconds) miliseconds) 1000.0)))
	(format "%02d.%05.2f" (/ new-duration 60) (mod new-duration 60))))))

;;;;;; Audio silences

(defun tlon-tts-create-silence-file (duration file)
  "Create a mono silence file of DURATION seconds with the name FILE."
  ;; TODO: the audio parameters are hard-coded; they should be obtained from the
  ;; relevant `audio-settings' variable
  (shell-command (format "ffmpeg -y -f lavfi -i anullsrc=r=44100:cl=mono -t %s -b:a 128k %s"
			 duration
			 (shell-quote-argument file))))

(defun tlon-tts-concatenate-files (input-file silence-file output-file)
  "Concatenate INPUT-FILE and SILENCE-FILE into OUTPUT-FILE.
We do it this way, via an intermediary container, because the silences generated
with the simpler version were not recognized by some audio players."
  (let ((concat-file (make-temp-file "ffmpeg-concat" nil ".txt"))
	(intermediate-file (concat (file-name-sans-extension output-file) ".ts")))
    (with-temp-file concat-file
      (insert (format "file '%s'\nfile '%s'\n"
		      (shell-quote-argument input-file)
		      (shell-quote-argument silence-file))))
    (shell-command (format "ffmpeg -y -f concat -safe 0 -i %s -c copy -f mpegts %s"
			   (shell-quote-argument concat-file)
			   (shell-quote-argument intermediate-file)))
    (shell-command (format "ffmpeg -y -i %s -c copy %s"
			   (shell-quote-argument intermediate-file)
			   (shell-quote-argument output-file)))
    (delete-file concat-file)
    (delete-file intermediate-file)))

(defun tlon-tts-append-silence-to-file (input-file duration)
  "Append silence of DURATION to INPUT-FILE, returning path to new temp file.
The new temporary file contains the original content followed by silence."
  (let* ((silence-file (make-temp-file "tts_silence_" nil ".mp3"))
	 (output-file (make-temp-file "tts_appended_" nil ".mp3")))
    (tlon-tts-create-silence-file duration silence-file)
    (tlon-tts-concatenate-files input-file silence-file output-file)
    (delete-file silence-file)
    output-file)) ; Return the path to the new file with appended silence

(defun tlon-tts-append-silence-to-chunks (original-chunk-files)
  "Append silence to each file in ORIGINAL-CHUNK-FILES.
Returns a list of paths to new temporary files with silence appended."
  (let ((duration (replace-regexp-in-string "[[:alpha:]]" "" tlon-tts-paragraph-break-duration))
        temp-silence-appended-files)
    (message "Appending silence of %s to %d chunks (creating temporary files)..."
             tlon-tts-paragraph-break-duration (length original-chunk-files))
    (setq temp-silence-appended-files
          (mapcar (lambda (chunk-file)
                    (tlon-tts-append-silence-to-file chunk-file duration))
                  original-chunk-files))
    temp-silence-appended-files))

(defun tlon-tts-append-silence-to-chunks-p (file)
  "Return t iff FILE is a file to which silence should be appended."
  (let* ((staging-buffer-name (tlon-tts-get-staging-buffer-name file))
	 (engine (tlon-tts-get-buffer-local-variable-value 'tlon-tts-engine staging-buffer-name)))
    (string= engine "ElevenLabs")))

(defun tlon-tts-get-buffer-local-variable-value (var-name buffer-name)
  "Return the name of the buffer-local variable VAR-NAME for BUFFER-NAME."
  (with-current-buffer buffer-name
    (symbol-value var-name)))

;;;;;; TTS engines

;;;;;;; Microsoft Azure

(defun tlon-tts-microsoft-azure-make-request (string destination parameters &optional _chunk-index)
  "Make a request to the Microsoft Azure text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a cons cell of parameters to use when generating the audio, where
the car is the name of the file-local variable the cdr is its overriding value.
CHUNK-INDEX is ignored for Microsoft Azure but included for API consistency."
  (let ((vars (tlon-tts-get-file-local-or-override
	       '(tlon-tts-audio
		 tlon-tts-locale
		 tlon-tts-voice)
	       (when parameters (list parameters)))))
    (cl-destructuring-bind (audio locale voice) vars
      (let* ((ssml-wrapper (alist-get "Microsoft Azure" tlon-ssml-wrapper nil nil #'string=))
	     (wrapped-string (format ssml-wrapper locale voice
				     ;; handle single quotes in `string' without breaking `curl' command
				     (replace-regexp-in-string "'" "'\"'\"'" string))))
	(format tlon-microsoft-azure-request
		(tlon-tts-microsoft-azure-get-or-set-key) (car audio)
		wrapped-string destination)))))

(defun tlon-tts-microsoft-azure-get-or-set-key ()
  "Get or set the Microsoft Azure key."
  (or tlon-microsoft-azure-key
      (setq tlon-microsoft-azure-key
	    (auth-source-pass-get "tts1" (concat "tlon/core/azure.com/" tlon-email-shared)))))

;;;;;;; Google Cloud

(defun tlon-tts-google-cloud-make-request (string destination parameters &optional _chunk-index)
  "Make a request to the Google Cloud text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a cons cell of parameters to use when generating the audio, where
the car is the name of the file-local variable the cdr is its overriding value.
CHUNK-INDEX is ignored for Google Cloud but included for API consistency."
  (let ((vars (tlon-tts-get-file-local-or-override
	       '(tlon-tts-locale
		 tlon-tts-voice)
	       (when parameters (list parameters)))))
    (cl-destructuring-bind (locale voice) vars
      (let* ((ssml-wrapper (alist-get "Google Cloud" tlon-ssml-wrapper nil nil #'string=))
	     (wrapped-string (format ssml-wrapper locale voice string)))
	(format tlon-google-cloud-request
		(tlon-tts-google-cloud-get-token)
		;; note: `wrapped-string' already includes voice and locale info,
		;; but `tlon-tts-google-cloud-format-ssml' adds this info as part of the
		;; JSON payload. Is this correct?
		(tlon-tts-google-cloud-format-ssml wrapped-string)
		destination)))))

;; If this keeps failing, consider using the approach in `tlon-tts-elevenlabs-make-request'
(defun tlon-tts-google-cloud-format-ssml (ssml)
  "Convert SSML string to JSON object for Google Cloud TTS."
  (let* ((gender (tlon-lookup tlon-google-cloud-voices :gender :voice tlon-tts-voice))
	 (payload (json-encode
		   `((input (ssml . ,ssml))
		     (voice (languageCode . ,tlon-tts-locale)
			    (name . ,tlon-tts-voice)
			    (ssmlGender . ,(upcase gender)))
		     (audioConfig (audioEncoding . ,(car tlon-tts-audio)))))))
    payload))

(defun tlon-tts-google-cloud-get-token ()
  "Get or set the Google Cloud token key."
  (string-trim (shell-command-to-string "gcloud auth print-access-token")))

;;;;;;; Amazon Polly

(defun tlon-tts-amazon-polly-make-request (string destination parameters &optional _chunk-index)
  "Construct the AWS CLI command to call Amazon Polly.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a cons cell of parameters to use when generating the audio, where
the car is the name of the file-local variable the cdr is its overriding value.
CHUNK-INDEX is ignored for Amazon Polly but included for API consistency."
  (let ((vars (tlon-tts-get-file-local-or-override
	       '(tlon-tts-audio
		 tlon-tts-voice)
	       (when parameters (list parameters)))))
    (cl-destructuring-bind (audio voice) vars
      (format tlon-amazon-polly-request
	      (car audio) voice string tlon-amazon-polly-region destination))))

;;;;;;; OpenAI

(defun tlon-tts-openai-make-request (string destination parameters &optional _chunk-index)
  "Make a request to the OpenAI text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a cons cell of parameters to use when generating the audio, where
the car is the name of the file-local variable the cdr is its overriding value.
CHUNK-INDEX is ignored for OpenAI but included for API consistency."
  (let ((vars (tlon-tts-get-file-local-or-override
	       '(tlon-tts-voice)
	       (when parameters (list parameters)))))
    (cl-destructuring-bind (voice) vars
      (format tlon-openai-tts-request
	      (tlon-tts-openai-get-or-set-key)
	      tlon-openai-model
	      (json-encode-string string)
	      voice
	      destination))))

(defun tlon-tts-openai-get-or-set-key ()
  "Get or set the OpenAI API key."
  (or tlon-openai-key
      (setq tlon-openai-key
	    (auth-source-pass-get "gptel" (concat "tlon/core/openai.com/" tlon-email-shared)))))

;;;;;;; ElevenLabs

(defun tlon-tts-elevenlabs-make-request (string destination parameters &optional chunk-index)
  "Make a request to the ElevenLabs text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a cons cell like (tlon-tts-voice . VOICE-ID) or nil, providing
override values. CHUNK-INDEX is the optional index of the current chunk (used
for context)."
  (let* (;; Wrap parameters in a list if non-nil before passing to the override function
         (vars (tlon-tts-get-file-local-or-override
                '(tlon-tts-voice
                  tlon-tts-audio)
                (when parameters (list parameters)))) ; Ensure parameters is a list
         ;; Get context only if chunk-index is provided and paragraph chunking is active
         (use-context (and chunk-index (null tlon-elevenlabs-char-limit)))
         (before-text (when (and use-context (> chunk-index 0))
                        (let* ((prev-chunk-data (nth (1- chunk-index) tlon-tts-chunks))
                               (prev-begin-marker (nth tlon-tts-chunk-index-begin-marker prev-chunk-data))
                               (prev-end-marker (nth tlon-tts-chunk-index-end-marker prev-chunk-data)))
                          (tlon-tts--get-processed-text-from-markers prev-begin-marker prev-end-marker))))
         (after-text (when (and use-context (< (1+ chunk-index) (length tlon-tts-chunks)))
                       (let* ((next-chunk-data (nth (1+ chunk-index) tlon-tts-chunks))
                              (next-begin-marker (nth tlon-tts-chunk-index-begin-marker next-chunk-data))
                              (next-end-marker (nth tlon-tts-chunk-index-end-marker next-chunk-data)))
                         (tlon-tts--get-processed-text-from-markers next-begin-marker next-end-marker))))
         (previous-chunk-id (when use-context (tlon-tts-get-previous-chunk-id chunk-index)))
         (next-chunk-id (when use-context (tlon-tts-get-next-chunk-id chunk-index)))
         (voice-settings-params '(:stability :similarity_boost :style :use_speaker_boost :speed)))
    (cl-destructuring-bind (voice audio) vars
      ;; Look up the full voice definition using the voice ID
      (let* ((voice-definition (cl-find-if (lambda (entry) (equal (plist-get entry :id) voice))
                                           tlon-elevenlabs-voices))
             (voice-settings (tlon-tts-build-voice-settings voice-definition voice-settings-params))
             ;; Pass use-context and chunk IDs to define payload parts
             (payload-parts (tlon-tts-define-payload-parts string before-text after-text previous-chunk-id next-chunk-id use-context))
             ;; Add voice settings if they exist
             (final-payload-parts
              (if voice-settings
		  ;; Ensure the voice_settings alist itself is the value for the "voice_settings" key
		  (append payload-parts `(("voice_settings" . ,voice-settings)))
		payload-parts))
             (payload (json-encode final-payload-parts))
             ;; Generate temp file for headers, use 999 if chunk-index is nil
             (header-file (make-temp-file (format "tts-header-%d-" (or chunk-index 999)) nil ".txt")))
        ;; Store header file path in chunk data only if chunk-index is valid
        (when chunk-index
          ;; Use the correct constant for the header filename index (6)
          (setf (nth tlon-tts-chunk-index-header-filename (nth chunk-index tlon-tts-chunks)) header-file))
        ;; Construct curl command
	(mapconcat 'shell-quote-argument
                   (list "curl"
			 "--request" "POST"
			 "--url" (format tlon-elevenlabs-tts-url voice (car audio))
			 "--header" "Content-Type: application/json"
			 "--header" (format "xi-api-key: %s" (tlon-tts-elevenlabs-get-or-set-key))
			 ;; Dump headers to the temporary file
			 "--dump-header" header-file
			 "--data" payload
			 ;; Write audio body to destination file
			 "--output" destination)
		   " ")))))

(defun tlon-tts-get-previous-chunk-id (chunk-index)
  "Get the ID of the previous chunk for chunk at CHUNK-INDEX."
  (when (> chunk-index 0)
    (let ((prev-chunk (nth (1- chunk-index) tlon-tts-chunks)))
      (when (eq (nth tlon-tts-chunk-index-status prev-chunk) 'completed) ; Check status
        (nth tlon-tts-chunk-index-request-id prev-chunk)))))

(defun tlon-tts-get-next-chunk-id (chunk-index)
  "Get the ID of the next chunk for chunk at CHUNK-INDEX."
  (let ((next-chunk-idx (1+ chunk-index)))
    (when (< next-chunk-idx (length tlon-tts-chunks))
      (let ((next-chunk (nth next-chunk-idx tlon-tts-chunks)))
        ;; Only pass if the next chunk is completed (e.g., re-generating a middle chunk)
        (when (eq (nth tlon-tts-chunk-index-status next-chunk) 'completed)
          (nth tlon-tts-chunk-index-request-id next-chunk))))))

(defun tlon-tts-define-payload-parts (string before-text after-text previous-chunk-id next-chunk-id use-context)
  "Define the payload parts for ElevenLabs API request.
STRING is the text to be converted to speech. BEFORE-TEXT, AFTER-TEXT,
PREVIOUS-CHUNK-ID, NEXT-CHUNK-ID, and stitch_audio are only included if
USE-CONTEXT is non-nil."
  `(("text" . ,string)
    ("model_id" . ,tlon-elevenlabs-model)
    ,@(when (and use-context before-text) `(("before_text" . ,before-text)))
    ,@(when (and use-context after-text) `(("after_text" . ,after-text)))
    ,@(when (and use-context previous-chunk-id) `(("previous_request_ids" . (,previous-chunk-id))))
    ,@(when (and use-context next-chunk-id) `(("next_request_ids" . (,next-chunk-id))))
    ("stitch_audio" . ,(if use-context t nil))))

(defun tlon-tts-build-voice-settings (voice-definition voice-settings-params)
  "Build the voice settings from the VOICE-DEFINITION and VOICE-SETTINGS-PARAMS."
  (delq nil
        (mapcar (lambda (param)
                  (when-let ((value (plist-get voice-definition param)))
                    (cons (format "%s" (substring (symbol-name param) 1))
                          (if (eq param :use_speaker_boost)
                              (if value t nil) ; Use standard Elisp booleans t/nil
                            value))))
                voice-settings-params)))

(declare-function json-mode "json-mode")
(defun tlon-tts-elevenlabs-get-voices ()
  "Get a list of all ElevenLabs available voices."
  (interactive)
  (let ((response (shell-command-to-string "curl -s --request GET \
  --url https://api.elevenlabs.io/v1/voices")))
    (with-current-buffer (get-buffer-create "*ElevenLabs Voices*")
      (erase-buffer)
      (insert response)
      (json-mode)
      (json-pretty-print (point-min) (point-max))
      (switch-to-buffer (current-buffer))
      (goto-char (point-min)))))

(defun tlon-tts-elevenlabs-get-or-set-key ()
  "Get or set the ElevenLabs API key."
  (or tlon-elevenlabs-key
      (setq tlon-elevenlabs-key
	    (auth-source-pass-get "key" (concat "tlon/core/elevenlabs.io/" tlon-email-shared)))))

;;;;; Metadata

;; MAYBE: Include summary?
(defun tlon-tts-get-metadata ()
  "Return the metadata of the current file."
  (let ((separator "\n\n"))
    (when-let* ((metadata (tlon-yaml-format-values-of-alist (tlon-yaml-get-metadata)))
		(title (alist-get "title" metadata nil nil #'string=))
		(title-part (concat (format "%s." title) separator)))
      (if-let* ((authors (alist-get "authors" metadata nil nil #'string=))
		(author-string (tlon-concatenate-list authors))
		(pattern (tlon-lookup tlon-authorship-pattern :pattern :language (tlon-tts-get-current-language)))
		(author-part (concat (format pattern author-string) separator)))
	  (concat title-part author-part)
	title-part))))

;;;;; Get SSML

(defun tlon-tts-get-ssml-break (time)
  "Return an SSML `break' tag with `time' attribute of TIME."
  (tlon-md-get-tag-filled "break" `(,time)))

;;;;; File uploading

(declare-function magit-stage-files "magit-apply")
(declare-function magit-extras-stage-commit-and-push "magit-extra")
;;;###autoload
(defun tlon-tts-move-file-to-audio-server (&optional file)
  "Move audio FILE to the audio repo, then stage, commit, and push the change.
The language and bare directory (e.g., \"articulos\", \"temas\") are inferred
from the source FILE's path. The file will be moved to a path like
\"uqbar-audio/lang/bare-dir/filename.ext\"."
  (interactive)
  (let* ((file (files-extras-read-file file))
         (repo (tlon-get-repo-from-file file))
	 (lang (tlon-repo-lookup :language :dir repo))
         (bare-dir (tlon-get-bare-dir file)) ; Get bare-dir from the source audio file
         (audio-repo-dir (tlon-repo-lookup :dir :name "uqbar-audio"))
         (destination-dir (tlon-tts-get-audio-directory lang bare-dir)) ; Pass bare-dir
         (destination-file-name (file-name-nondirectory file))
	 (destination (file-name-concat destination-dir destination-file-name))
         (commit-message (format "Add %s/%s/%s" lang bare-dir destination-file-name)))
    (unless lang
      (user-error "Could not determine a valid language from the file path: %s (derived lang: %s)" file lang))
    (unless bare-dir
      (user-error "Could not determine bare-dir for %s" file))
    (unless audio-repo-dir
      (user-error "Could not find the 'uqbar-audio' repository directory"))
    (unless (file-directory-p destination-dir)
      (make-directory destination-dir t))
    (rename-file file destination 0)
    (message "Moved `%s' to `%s'." file destination)
    (when (derived-mode-p 'dired-mode)
      (revert-buffer nil t t))
    (when (y-or-n-p (format "Commit and push %s to audio server?" destination-file-name))
      (let ((default-directory audio-repo-dir))
        (magit-stage-files (list destination)) ; Ensure the file is staged
        (magit-extras-stage-commit-and-push commit-message destination)
        (message "Committed and pushed `%s' to audio server." destination-file-name)))
    (when-let ((chunks (tlon-tts-get-list-of-chunks file)))
      (if (y-or-n-p (format "Delete %d audio chunk(s) for %s?" (length chunks) (file-name-nondirectory file)))
	  (tlon-tts-delete-chunks-of-file file)
	(message "Audio chunks for %s were not deleted." (file-name-nondirectory file))))))

(defun tlon-tts-get-audio-directory (&optional lang bare-dir)
  "Return the directory where audio files are stored for LANG.
If BARE-DIR is provided, return the subdirectory for that bare-dir within the
language directory (e.g., \"uqbar-audio/lang/bare-dir/\"). If BARE-DIR is nil,
return the language root directory (e.g., \"uqbar-audio/lang/\").
If LANG is nil, get it from the current language process."
  (let ((audio-root (tlon-repo-lookup :dir :name "uqbar-audio"))
	(current-lang (tlon-tts-get-current-language lang)))
    (if bare-dir
        (file-name-concat audio-root current-lang bare-dir)
      (file-name-concat audio-root current-lang))))

;;;###autoload
(defun tlon-tts-open-audio-directory (&optional lang)
  "Open the directory where the audio files for LANG are stored."
  (interactive)
  (dired (tlon-tts-get-audio-directory lang)))

;;;;; Cleanup

;;;;;; Notes

(defun tlon-tts-process-notes ()
  "Replace note reference with its content, if it is a sidenote, else delete it.
Move the note to the end of the sentence if necessary.

Note: the function assumes that the citation is in MDX, rather than Pandoc
citation key, format. Hence, it must be run *before*
`tlon-bib-replace-keys-with-citations'."
  (goto-char (point-min))
  (while (re-search-forward markdown-regex-footnote nil t)
    (let ((note (tlon-tts-get-note))
	  reposition)
      (markdown-footnote-kill)
      (when (not (string-empty-p note))
	(unless (looking-back (concat "\\.\\|" markdown-regex-footnote) (line-beginning-position))
	  (setq reposition t))
(when (and (eq (tlon-get-note-type note) 'sidenote)
	   tlon-tts-narrate-sidenotes) ; Only insert if option is non-nil
  (when reposition
    (forward-sentence)
    (while (thing-at-point-looking-at tlon-md-blockquote)
      (forward-sentence) (forward-char) (forward-char)))
  (insert (tlon-tts-handle-note note)))))))

(defun tlon-tts-get-note ()
  "Return the note at point."
  (save-excursion
    (markdown-footnote-goto-text)
    (let ((begin (point)))
      (if (re-search-forward "^[\\^[[:digit:]]+" nil t)
	  (progn
	    (beginning-of-line)
	    (backward-char))
	(goto-char (or (when (re-search-forward tlon-tts-local-variables-section-start nil t)
			 (match-beginning 0))
		       (point-max))))
      (string-chop-newline (buffer-substring-no-properties begin (point))))))

(defun tlon-tts-handle-note (note)
  "Handle NOTE for audio narration."
  (let ((clean-note (replace-regexp-in-string (tlon-md-get-tag-pattern "Sidenote") "" note)))
    (format "\n\n%s\n" (tlon-tts-listener-cue-full-enclose 'note clean-note))))

;;;;;; Formatting

;;;;;;; General

;; TODO: should have more descriptive name
(defun tlon-tts-process-formatting ()
  "Remove formatting from text."
  (tlon-tts-process-boldface)
  (tlon-tts-process-italics)
  (tlon-tts-process-sup)
  (tlon-tts-process-sub)
  (tlon-tts-process-slashes)
  (tlon-tts-process-literal-link)
  (tlon-tts-process-visually-hidden)
  (tlon-tts-process-replace-audio)
  (tlon-tts-process-voice-role)
  (tlon-tts-process-small-caps)
  (tlon-tts-process-math))

(defun tlon-tts-remove-formatting (type)
  "Remove formatting TYPE from text."
  (cl-destructuring-bind (pattern . groups)
      (pcase type
	('boldface (cons markdown-regex-bold '(1 4)))
	('italics (cons markdown-regex-italic '(1 4)))
	('visually-hidden (cons (tlon-md-get-tag-pattern "VisuallyHidden") '(2)))
	('literal-link (cons (tlon-md-get-tag-pattern "LiteralLink") '(2)))
	('sup (cons (tlon-md-get-tag-pattern "sup") '(2)))
	('sub (cons (tlon-md-get-tag-pattern "sub") '(2)))
	('small-caps (cons (tlon-md-get-tag-pattern "SmallCaps") '(2)))
	(_ (user-error "Invalid formatting type: %s" type)))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (let ((replacement (if groups
			     (mapconcat (lambda (group)
					  (match-string group))
					groups)
			   "")))
	(replace-match replacement t t)))))

;;;;;;; Specific

(defun tlon-tts-process-boldface ()
  "Remove boldface from text."
  (tlon-tts-remove-formatting 'boldface))

(defun tlon-tts-process-italics ()
  "Remove italics from text."
  (tlon-tts-remove-formatting 'italics))

(defun tlon-tts-process-visually-hidden ()
  "Remove `VisuallyHidden' MDX tag."
  (tlon-tts-remove-formatting 'visually-hidden))

(defun tlon-tts-process-small-caps ()
  "Replace small caps with their full form."
  (tlon-tts-remove-formatting 'small-caps))

(defun tlon-tts-process-sup ()
  "Remove `sup' tag."
  (tlon-tts-remove-formatting 'sup))

(defun tlon-tts-process-literal-link ()
  "Replace text enclosed in a `LiteralLink' MDX tag with its contents'."
  (tlon-tts-remove-formatting 'literal-link))

(defun tlon-tts-process-sub ()
  "Remove `sub' tag."
  (tlon-tts-remove-formatting 'sub))

(defun tlon-tts-process-math ()
  "Replace math expressions with their alt text.
If no alt text is present, replace with the expression itself."
  (goto-char (point-min))
  (while (re-search-forward (tlon-md-get-tag-pattern "Math") nil t)
    (replace-match (or (match-string 4) (match-string 2)) t t)))

(defun tlon-tts-process-slashes ()
  "Conditionally replace slashes between alphanumeric characters.
Whether slashes are replaced, and by what character, depends on the TTS engine."
  (let ((replacement (pcase tlon-tts-engine
		       ("ElevenLabs" "/") ; do not replace
		       ("Microsoft Azure" " ")
		       (_ "-")))) ; fallback; for engines we haven’t yet tested
    (goto-char (point-min))
    (while (re-search-forward "\\(?1:[[:alpha:]]\\)/\\(?2:[[:alpha:]]\\)" nil t)
      (unless (looking-at-p markdown-regex-link-inline)
	(replace-match (concat (match-string 1) replacement (match-string 2)))))))

(defun tlon-tts-process-replace-audio ()
  "Replace text enclosed in a `ReplaceAudio' MDX tag with its `text' attribute.
Uses a two-pass approach."
  (let ((replacements '()) ; List to store (start end replacement-string)
        (pattern (tlon-md-get-tag-pattern "ReplaceAudio")))
    ;; Pass 1: Collect replacements
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (text (or (match-string 4) "")) ; Default to empty string
               (role (match-string 6))
               replacement voice)
          (save-match-data ; Crucial inside the loop if using functions that might change match data
            (setq voice (tlon-tts-get-voice-of-role role))
            (if (string-empty-p text)
                (setq replacement "")
              (setq replacement (if (string= voice (tlon-tts-get-voice-at-point))
                                    text
                                  (tlon-tts-enclose-in-voice-tag text voice)))))
          ;; Store replacement info (start end replacement)
          (push (list start end replacement) replacements))))

    ;; Pass 2: Apply replacements in reverse buffer order (list is already reversed due to push)
    (dolist (replacement-info replacements)
      (cl-destructuring-bind (start end replacement-string) replacement-info
        (delete-region start end)
        (goto-char start)
        (insert replacement-string)))))

(defun tlon-tts-process-voice-role ()
  "Replace text enclosed in a `VoiceRole' MDX tag with a SMML `voice' tag."
  (goto-char (point-min))
  (while (re-search-forward (tlon-md-get-tag-pattern "VoiceRole") nil t)
    (let* ((content (match-string 2))
	   (role (match-string 4))
	   replacement voice)
      (save-match-data
	(setq voice (tlon-tts-get-voice-of-role role))
	(setq replacement (tlon-tts-enclose-in-voice-tag content voice)))
      (replace-match replacement t t))))

;;;;;; Paragraphs

(defun tlon-tts-process-paragraphs ()
  "Add a pause at the end of a paragraph.
The time length of the pause is determined by
`tlon-tts-paragraph-break-duration'."
  (goto-char (point-min))
  (while (not (eobp))
    (forward-paragraph)
    (unless (eobp)
      (backward-char))
    (unless (or
	     (looking-back (tlon-md-get-tag-pattern "break") (line-beginning-position))
	     (looking-back "^\\s-*$" (line-beginning-position)))
      (insert (concat "\n" (tlon-tts-get-ssml-break tlon-tts-paragraph-break-duration))))
    (unless (eobp)
      (forward-char))))

;;;;;; Tag sections

(defun tlon-tts-remove-tag-sections ()
  "Remove the ‘further reading’, ‘related entries’ and ‘external links’ sections."
  (let* ((lang (tlon-tts-get-current-language))
	 (bare-dir (tlon-lookup tlon-core-bare-dirs "en" lang (tlon-get-bare-dir tlon-tts-source))))
    (when (string= bare-dir "tags")
      (goto-char (point-min))
      (when-let ((pattern (alist-get lang tlon-tts-tag-section-patterns nil nil #'string=)))
	(when (re-search-forward pattern nil t)
	  (goto-char (match-beginning 0))
	  (let ((begin (point)))
	    (tlon-md-end-of-buffer-dwim)
	    (delete-region begin (point))))))))

;;;;;; Lines

(defun tlon-tts-remove-horizontal-lines ()
  "Remove horizontal lines from text."
  (goto-char (point-min))
  (while (re-search-forward "^---+\n\n" nil t)
    (replace-match (concat (tlon-tts-get-ssml-break tlon-tts-paragraph-break-duration) "\n\n") t t)))

;;;;;; Abbreviations

(defun tlon-tts-process-abbreviations (type)
  "Replace abbreviations of TYPE with their spoken equivalent."
  (let ((case-fold-search nil)) ; Default to case-sensitive for standalone abbrev replacement
    (dolist (entry (pcase type
                     ('local tlon-local-abbreviations-for-session)
                     ('global (tlon-tts-get-global-abbreviations))))
      (cl-destructuring-bind (abbrev . expansion) entry
        (let* ((abbrev-is-regex-p (string-match-p "[\\\\\\[\\(\\|\\?\\*\\+\\{\\.\\^\\$]" abbrev))
               (abbrev-pattern (if abbrev-is-regex-p
                                   abbrev ; Use raw string as regex pattern
                                 (format "\\b%s\\b" (regexp-quote abbrev))))) ; Quote and add word boundaries

          ;; Skip introduction pattern check if abbrev is a regex
          (unless abbrev-is-regex-p
            (let ((abbrev-introduced-pattern
                   (format (concat "\\(?:" ; Non-capturing group for alternation
                                   ;; Expansion (Abbrev) or Expansion [Abbrev]
                                   "\\(?1:%s\\)[ \t]*([ \t]*\\(?2:%s\\)[ \t]*)\\|"
                                   "\\(?1:%s\\)[ \t]*\\[[ \t]*\\(?2:%s\\)[ \t]*\\]\\|"
                                   ;; Abbrev (Expansion) or Abbrev [Expansion]
                                   "\\(?2:%s\\)[ \t]*([ \t]*\\(?1:%s\\)[ \t]*)\\|"
                                   "\\(?2:%s\\)[ \t]*\\[[ \t]*\\(?1:%s\\)[ \t]*\\]"
                                   "\\)") ; Close non-capturing group
                           (regexp-quote expansion) (regexp-quote abbrev)
                           (regexp-quote expansion) (regexp-quote abbrev)
                           (regexp-quote abbrev) (regexp-quote expansion)
                           (regexp-quote abbrev) (regexp-quote expansion))))
              ;; Replace the full introduction (case-insensitive search)
              (goto-char (point-min))
              (let ((case-fold-search t)) ; Bind locally for this search only
                (while (re-search-forward abbrev-introduced-pattern nil t)
                  ;; Replace with the matched expansion (group 1) to preserve original capitalization
                  (replace-match (match-string 1) t t)))))
          ;; Replace remaining occurrences of the abbreviation itself (case-sensitive search)
          (goto-char (point-min))
          (while (re-search-forward abbrev-pattern nil t)
            ;; Pass the expansion to the replacement function
            (tlon-tts-punctuate-abbrev-replacement expansion)))))))

(defun tlon-tts-process-all-abbreviations ()
  "Process all abbreviations."
  (tlon-tts-process-local-abbreviations)
  (tlon-tts-process-global-abbreviations)
  (tlon-tts-process-local-phonetic-replacements)
  (tlon-tts-process-global-phonetic-replacements)
  (tlon-tts-process-global-phonetic-transcriptions))

(defun tlon-tts-punctuate-abbrev-replacement (replacement)
  "When processing abbreviations, replace match with REPLACEMENT.
If the abbreviation occurs at the end of a sentence, do not remove the period."
  (let ((replacement (if (tlon-tts-abbrev-ends-sentence-p)
			 (concat replacement ".")
		       replacement)))
    (replace-match replacement t t)))

(defun tlon-tts-abbrev-ends-sentence-p ()
  "Return t iff the abbreviation at point ends the sentence."
  (looking-at-p "\n\\| [A-Z]"))

;;;;;;; Local

(defun tlon-tts-process-local-abbreviations ()
  "Replace local abbreviations with their spoken equivalent.
In-text abbreviations are those that are introduced in the text itself,
typically in parenthesis after the first occurrence of the phrase they
abbreviate. We store these abbreviations on a per file basis, in the file-local
variable `tlon-local-abbreviations'."
  (tlon-tts-process-abbreviations 'local))

;;;;;;; Global

(defun tlon-tts-process-global-abbreviations ()
  ;; TODO: explain how different to local abbrevs.
  "Replace global abbreviations with their spoken equivalent."
  (tlon-tts-process-abbreviations 'global))

(defun tlon-tts-get-global-abbreviations ()
  "Get global abbreviations.
We throw an error if no global abbreviations are found, since (unlike local
abbreviations) this list should always be non-empty."
  (or (tlon-tts-get-associated-terms (tlon-read-json tlon-file-global-abbreviations))
      (user-error "Warning: no global abbreviations found; check `tlon-file-global-abbreviations'")))

;;;;;; Phonetic replacements

(defun tlon-tts-process-terms (terms replacement-fun &optional word-boundary)
  "Replace TERMS using REPLACEMENT-FUN.
If WORD-BOUNDARY is non-nil, enclose TERMS in a word boundary specifier. If so,
the terms will match only if they are adjacent to non-word characters."
  (let ((case-fold-search nil)
	(pattern (if word-boundary "\\b%s\\b" "%s")))
    (dolist (term terms)
      (let ((find (format pattern (car term)))
	    (replacement (cdr term)))
	(goto-char (point-min))
	(while (re-search-forward find nil t)
	  (save-match-data (funcall replacement-fun replacement)))))))

(defun tlon-tts-get-associated-terms (var)
  "Get associated terms for the current language in VAR.
For each cons cell in VAR for the language in the current text-to-speech
process, return its cdr."
  (let ((result '()))
    (dolist (cons var result)
      (let* ((term (car cons))
	     (lang (tlon-tts-get-current-language))
	     (assoc (or (alist-get lang (cdr cons) nil nil #'string=)
			(alist-get "default" (cdr cons) nil nil #'string=))))
	(when assoc
	  (push (cons term assoc) result))))
    result))

;;;;;;; Local

(defun tlon-tts-process-local-phonetic-replacements ()
  "Perform replacements indicated in `tlon-local-replacements'."
  (dolist (pair tlon-local-replacements-for-session)
    (let ((find (car pair)))
      (goto-char (point-min))
      (while (re-search-forward find nil t)
	(replace-match (cdr pair) t t)))))

;;;;;;; Global

(defun tlon-tts-process-global-phonetic-replacements ()
  "Replace terms with their counterparts."
  (tlon-tts-process-terms
   (tlon-tts-get-global-phonetic-replacements)
   'tlon-tts-replace-global-phonetic-replacements 'word-boundary))

(defun tlon-tts-get-global-phonetic-replacements ()
  "Get simple replacements."
  (tlon-tts-get-associated-terms (tlon-read-json tlon-file-global-phonetic-replacements)))

(defun tlon-tts-replace-global-phonetic-replacements (replacement)
  "When processing simple replacements, replace match with REPLACEMENT."
  (replace-match replacement t t))

;;;;;; Phonetic transcriptions

(defun tlon-tts-process-global-phonetic-transcriptions ()
  "Replace terms with their pronunciations."
  (tlon-tts-process-terms
   (tlon-tts-get-global-phonetic-transcriptions)
   'tlon-tts-replace-global-phonetic-transcriptions 'word-boundary))

(defun tlon-tts-get-global-phonetic-transcriptions ()
  "Get the phonetic transcriptions."
  (tlon-tts-get-associated-terms (tlon-read-json tlon-file-global-phonetic-transcriptions)))

(defun tlon-tts-replace-global-phonetic-transcriptions (replacement)
  "When processing phonetic transcriptions, replace match with pattern.
REPLACEMENT is the cdr of the cons cell for the term being replaced."
  (replace-match (format (tlon-md-get-tag-to-fill "phoneme")
			 "ipa" replacement (match-string-no-properties 0))
		 t t))

;;;;;; Listener cues

;;;;;;; General

(defun tlon-tts-process-listener-cues ()
  "Add listener cues to relevant elements."
  (tlon-tts-process-tables)
  (tlon-tts-process-quotes)
  (tlon-tts-process-blockquotes)
  (tlon-tts-process-asides)
  (tlon-tts-process-headings)
  (tlon-tts-process-subheadings)
  (tlon-tts-process-images)
  (tlon-tts-process-embedded))

;; TODO: consider revising so that each listener cue type has an associated
;; break duration. This may be worth it if it feels like some cues (like section
;; and subsection cues) should insert a longer silence than others.
(defun tlon-tts-add-listener-cues (type)
  "Add listener cues for text enclosed in tags of TYPE."
  (cl-destructuring-bind (pattern . group) (alist-get type tlon-tts-listener-cue-patterns)
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (if-let* ((match (match-string-no-properties group))
		(text (pcase type
			('blockquote (replace-regexp-in-string "^[[:blank:]]*?> ?" "" match))
			('table (tlon-tts-add-listener-cues-in-table match))
			(_ match))))
	  (replace-match
	   (tlon-tts-listener-cue-full-enclose type (string-chop-newline text)) t t)
	(user-error "Could not process cue for %s; match: %s" (symbol-name type) match)))))

(defun tlon-tts-add-listener-cues-in-table (match)
  "Add listener cues to the table in MATCH."
  (save-match-data
    (let* ((table-contents (match-string-no-properties 2))
	   (include-field (match-string 5))
	   (include-value (progn
			    (string-match "include=\"\\(?1:.*\\)\"" include-field)
			    (match-string 1 include-field)))
	   (content (pcase include-value
		      ("nothing" "")
		      ("everything" (replace-regexp-in-string tlon-tts-table-separator "" table-contents))
		      ("body" (replace-regexp-in-string tlon-tts-table-header "\\1" table-contents)))))
      (format "%s\n%s" match (tlon-tts-separate-table-cells content)))))

(defun tlon-tts-separate-table-cells (table)
  "Replace TABLE separators with periods."
  (replace-regexp-in-string " *|\\(?1:.*?\\) *|" "\\1." table))

(defun tlon-tts-enclose-in-listener-cues (type text)
  "Enclose TEXT in listener cues of TYPE."
  (cl-destructuring-bind (cue-begins . cue-ends)
      (alist-get (tlon-tts-get-current-language) type nil nil #'string=)
    (format "%s %s %s" cue-begins text cue-ends)))

(defun tlon-tts-enclose-in-voice-tag (string &optional voice)
  "Enclose STRING in `voice' SSML tags.
If VOICE is nil, default to the alternate voice.

Note that this function inserts two pairs of `voice' tags: the inner pair to set
the voice for the string that it encloses, and an outer pair of tags in reverse
order, to close the opening `voice' tag that wraps the entire document, and to
then reopen it."
  (let ((voice (or voice (tlon-tts-get-voice-of-role "alternate"))))
    (format tlon-tts-ssml-double-voice-replace-pattern voice string tlon-tts-voice)))

(defun tlon-tts-get-voice-of-role (&optional role)
  "Return the voice associated with ROLE.
If ROLE is nil, default to `\"inherit\"'.

For the relevant roles, consult the docstring of
`tlon-md-replace-audio-voice-reader'."
  (let* ((role (or role "inherit"))
	 (voices-var (tlon-lookup tlon-tts-engines :voices-var :name tlon-tts-engine))
	 (gender (tlon-lookup (symbol-value voices-var) :gender :id tlon-tts-voice))
	 (other-gender (if (string= gender "male") "female" "male"))
	 (other-role (if (tlon-tts-looking-at-listener-cue-tag-p) "main" "alternate")))
    (pcase role
      ("inherit" (tlon-tts-get-voice-at-point))
      ((or "main" "alternate") (tlon-lookup (symbol-value voices-var) :id :role role :gender gender))
      ((or "male" "female") (tlon-lookup (symbol-value voices-var) :id :role other-role :gender role))
      ("alternate-gender" (tlon-lookup (symbol-value voices-var) :id :role role :gender other-gender)))))

;; TODO: replace with function that detects voice at point
(defun tlon-tts-looking-at-listener-cue-tag-p ()
  "Return t iff point is looking at a listener cue tag."
  (let ((patterns (mapcar 'cadr tlon-tts-listener-cue-patterns)))
    (seq-some 'thing-at-point-looking-at patterns)))

(defun tlon-tts-enclose-in-cue-delimiter (string)
  "Enclose STRING in listener cue delimiter."
  (format "%1$s%s%1$s\n" tlon-tts-cue-delimiter string))

(defun tlon-tts-listener-cue-full-enclose (type text)
  "Enclose TEXT in listener cue of TYPE and, if appropriate, in `voice' SSML tags.
Whether TEXT is enclosed in `voice' tags is determined by the value of
`tlon-tts-use-alternate-voice'."
  (let ((cues (alist-get type tlon-tts-listener-cues)))
    (tlon-tts-enclose-in-cue-delimiter
     (if tlon-tts-use-alternate-voice
	 (tlon-tts-use-alternate-voice-maybe-with-cue type text)
       (tlon-tts-enclose-in-listener-cues cues text)))))

(defun tlon-tts-use-alternate-voice-maybe-with-cue (type text)
  "Read TEXT with alternate voice.
Depending on TYPE, also enclose TEXT in listener cues."
  (let* ((cues (alist-get type tlon-tts-listener-cues))
	 (content (pcase type
		    ('quote text)
		    (_ (tlon-tts-enclose-in-listener-cues cues text)))))
    (tlon-tts-enclose-in-voice-tag content)))

;;;;;;; Specific

(defun tlon-tts-process-quotes ()
  "Add listener cues for quotes."
  (tlon-tts-add-listener-cues 'quote))

(defun tlon-tts-process-blockquotes ()
  "Add listener cues for blockquotes."
  (tlon-tts-add-listener-cues 'blockquote))

(defun tlon-tts-process-asides ()
  "Add listener cues for asides."
  (tlon-tts-add-listener-cues 'aside))

(defun tlon-tts-process-headings ()
  "Add listener cues for headings."
  (tlon-tts-add-listener-cues 'heading))

(defun tlon-tts-process-subheadings ()
  "Add listener cues for subheadings."
  (tlon-tts-add-listener-cues 'subheading))

(defun tlon-tts-process-images ()
  "Add listener cues for `Figure` tags.
If the `ignore-content` attribute is present in the `Figure` tag, only the
`alt` text is used. Otherwise, the `alt` text, a caption cue, and the
tag's content (caption) are used."
  (goto-char (point-min))
  (while (re-search-forward (tlon-md-get-tag-pattern "Figure") nil t)
    (if-let* ((alt (match-string-no-properties 6))) ; Alt text is in group 6
        (let* ((raw-caption (match-string-no-properties 2)) ; Tag content (caption) is in group 2
               (ignore-content-flag (match-string-no-properties 7)) ; ignore-content attribute is group 7
               caption-cue
               text)
          (if ignore-content-flag
              (setq text alt) ; Only use alt text if ignore-content is present
            ;; If ignore-content is not present, use alt text, cue, and caption
            (setq text alt) ; Start with alt text
            (unless (string-empty-p raw-caption)
              (setq caption-cue (alist-get (tlon-tts-get-current-language)
                                           (alist-get 'image-caption tlon-tts-listener-cues)
                                           nil nil #'string=))
              (setq text (concat text (or caption-cue "") raw-caption)))) ; Append cue (if any) and caption
          (replace-match (tlon-tts-listener-cue-full-enclose 'image (string-chop-newline text)) t t))
      (user-error "Missing alt text; match: %s" alt))))

(defun tlon-tts-ensure-all-images-have-alt-text ()
  "Ensure all images have alt text."
  (dolist (tag '("Figure" "Embedded"))
    (goto-char (point-min))
    (while (re-search-forward (tlon-md-get-tag-pattern tag) nil t)
      (unless (match-string 6)
	(user-error "Some images are missing alt text; run `M-x tlon-ai-set-image-alt-text-in-buffer'")))))

(defun tlon-tts-process-embedded ()
  "Add listener cues for embedded charts."
  (tlon-tts-add-listener-cues 'embedded))

;;;;;; Report

;;;###autoload
(defun tlon-tts-generate-report ()
  "Generate a report of TTS issues potentially worth addressing.
The report checks the content of the current staging buffer."
  (interactive)
  (unless (tlon-tts-staging-buffer-p)
    (user-error "Not in a TTS staging buffer"))
  (let ((staging-buffer (current-buffer))
	(abbreviations (tlon-tts-get-missing-abbreviations)) ; Called within staging buffer context
	(chemical-symbols-p (tlon-tts-check-chemical-symols))
	(emphasis-p (tlon-tts-check-emphasis))
	(en-dashes-p (tlon-tts-check-en-dashes))
	(numerals-sans-separator-p (tlon-tts-get-numerals-sans-separator))
	report-buffer)
    (with-current-buffer (get-buffer-create tlon-tts-report-buffer-name)
      (setq report-buffer (current-buffer))
      (erase-buffer)
      (org-mode)
      (insert "* TTS Report")
      (when abbreviations
	(insert "\n** Missing abbreviations\n\n")
	(dolist (abbreviation abbreviations)
	  (insert (format "- %s\n" abbreviation))))
      (when chemical-symbols-p
	(insert (format "\n** Chemical symbols\n\nSearch for ‘%s’\n"
			tlon-tts-maybe-chemical-symbol)))
      (when emphasis-p
	(insert (format "\n** Emphasis\n\nRun =M-x tlon-manual-fixe-emphasis=\n")))
      (when en-dashes-p
	(insert (format "\n** En dashes\n\nSearch for ‘–’\n")))
      (when numerals-sans-separator-p
	(insert (format "\n** Numerals sans separator\n\nRun =M-x tlon-manual-fix-add-thousands-separators=\n"))))
    (if (= (buffer-size report-buffer) 0)
	(kill-buffer report-buffer)
      (progn
        (switch-to-buffer report-buffer)
        (goto-char (point-min))))
    (other-window 1)
    (switch-to-buffer staging-buffer)))

;;;;;;; Missing abbreviatios

(defun tlon-tts-get-missing-abbreviations ()
  "Return list of missing abbreviations present in the current buffer.
This function is intended to be called from within the staging buffer."
  (let ((abbrevs (append tlon-local-abbreviations-for-session
			 (tlon-tts-get-global-abbreviations)))
	(case-fold-search nil)
	missing)
    (goto-char (point-min))
    (while (re-search-forward "\\b\\(?:[A-Z]\\{2,\\}\\(?:\\. ?\\)?\\)+\\.?\\b" nil t)
      (let ((found-abbrev-in-buffer (match-string-no-properties 0))
            (is-known nil))
        ;; Iterate through known abbreviations to see if any match the found string
        (cl-dolist (known-abbrev-pair abbrevs)
          (let ((known-abbrev-pattern (car known-abbrev-pair)))
            ;; Assume known-abbrev-pattern is a string that represents a regex.
            ;; We check if this regex fully matches the found-abbrev-in-buffer.
            (when (string-match-p (format "\\`%s\\'" known-abbrev-pattern) found-abbrev-in-buffer)
              (setq is-known t)
              (cl-return)))) ; cl-return works with cl-dolist
        
        (unless (or is-known
                    (tlon-tts-looking-at-excluded-tag-p))
          (push found-abbrev-in-buffer missing))))
    (delete-dups missing)))

(defun tlon-tts-looking-at-excluded-tag-p ()
  "Return t iff point is looking at an excluded tag.
An excluded tag is one enclosing text that does not contain abbreviatios to be
processed."
  (seq-some (lambda (tag)
	      (thing-at-point-looking-at (tlon-md-get-tag-pattern tag)))
	    '("Cite" "Math" "ReplaceAudio" "Roman" "SmallCaps")))

;;;;;;; Unprocessed strings

(defun tlon-tts-check-unprocessed (string)
  "Return t iff the current buffer appears to have unprocessed STRING."
  (catch 'found
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward string nil t)
	(unless (or (thing-at-point-looking-at (tlon-md-get-tag-pattern "ReplaceAudio"))
		    ;; if en dash, exclude matches in locators
		    (when (string= string "–")
		      (thing-at-point-looking-at (tlon-md-get-tag-pattern "Cite"))))
	  (throw 'found t))))))

(defun tlon-tts-get-numerals-sans-separator ()
  "Return it iff the current buffer has numerals without thousands separators.
We need to use separators to get some TTS engines (such as Elevenlabs) to
pronounce the numbers correctly. This requires manual processing since some
numbers, such as years, should not be separated."
  (tlon-tts-check-unprocessed tlon-numerals-sans-separator))

(defun tlon-tts-check-chemical-symols ()
  "Return t iff the current buffer appears to have unprocessed chemical symbols."
  (tlon-tts-check-unprocessed tlon-tts-maybe-chemical-symbol))

(defun tlon-tts-check-emphasis ()
  "Return t iff the current buffer appears to have unprocessed emphasis markers."
  (tlon-tts-check-unprocessed markdown-regex-italic))

(defun tlon-tts-check-en-dashes ()
  "Return t iff the current buffer appears to have unprocessed en dashes."
  (tlon-tts-check-unprocessed "–"))

;;;;;; Links

(defun tlon-tts-process-links ()
  "Replace links with their text.
Note: this function should be run after `tlon-tts-process-images' because
image links are handled differently."
  (goto-char (point-min))
  (while (re-search-forward markdown-regex-link-inline nil t)
    (cl-destructuring-bind (self-link-text . self-link-replace)
	(alist-get (tlon-tts-get-current-language) tlon-tts-self-referential-link nil nil #'string=)
      (let* ((link-text (match-string-no-properties 3))
	     (replacement (if (string= link-text self-link-text)
			      self-link-replace
			    link-text)))
	(replace-match replacement t t)))))

;;;;;; Tables

(defun tlon-tts-process-tables ()
  "Format tables for text-to-speech."
  (tlon-tts-add-listener-cues 'table)
  (tlon-tts-remove-table-separator))

(defun tlon-tts-remove-table-separator ()
  "Remove table separators in buffer."
  (goto-char (point-min))
  (while (re-search-forward tlon-tts-table-separator nil t)
    (replace-match "" t t)))

(defun tlon-tts-ensure-all-tables-have-alt-text ()
  "Ensure all tables have alt text."
  (goto-char (point-min))
  (while (re-search-forward tlon-tts-table-pattern nil t)
    (unless (thing-at-point-looking-at (tlon-md-get-tag-pattern "SimpleTable"))
      (user-error "Some tables are missing alt text"))))

;;;;;; Numbers

;;;;;;; General

(defun tlon-tts-process-numerals ()
  "Process numbers as appropriate."
  (tlon-tts-process-numerals-convert-powers)
  (tlon-tts-process-numerals-convert-roman)
  (tlon-tts-process-numerals-set-thousands-separators)
  (tlon-tts-process-80000))

;;;;;;; Specific

;;;;;;;; Convert powers

(defun tlon-tts-process-numerals-convert-powers ()
  "Replace powers with their verbal equivalents."
  (let ((language (tlon-tts-get-current-language)))
    (goto-char (point-min))
    (while (re-search-forward tlon-md-math-power nil t)
      (let* ((base (match-string-no-properties 1))
	     (raw-exponent (match-string-no-properties 2))
	     (exponent (if (string-match "[[:digit:]]+" raw-exponent)
			   (string-to-number raw-exponent)
			 raw-exponent))
	     (verbal-exponent (tlon-tts-get-verbal-exponent exponent language)))
	(replace-match (format "%s %s" base verbal-exponent) t t)))))

(defun tlon-tts-get-verbal-exponent (exponent language)
  "Return the verbal equivalent of EXPONENT in LANGUAGE."
  (or (tlon-tts-get-irregular-verbal-exponent exponent language)
      (tlon-tts-get-regular-verbal-exponent exponent language)))

(defun tlon-tts-get-regular-verbal-exponent (exponent language)
  "Return the irregular verbal equivalent of EXPONENT in LANGUAGE."
  (let ((pattern (alist-get language tlon-tts-regular-exponent-pattern nil nil #'string=)))
    (format pattern exponent)))

(defun tlon-tts-get-irregular-verbal-exponent (exponent language)
  "Return the irregular verbal equivalent of EXPONENT in LANGUAGE."
  (when-let* ((inner-list (alist-get exponent tlon-tts-irregular-exponents)))
    (alist-get language inner-list nil nil #'string=)))

;;;;;;;; Convert Roman numerals

(autoload 'rst-roman-to-arabic "rst")
(defun tlon-tts-process-numerals-convert-roman ()
  "Replace Roman numerals with their Arabic equivalents."
  (goto-char (point-min))
  (while (re-search-forward (tlon-md-get-tag-pattern "Roman") nil t)
    (let* ((roman (match-string-no-properties 2))
	   (arabic (rst-roman-to-arabic roman)))
      (replace-match (number-to-string arabic) t t))))

;;;;;;;; Replace thousands separators

(defun tlon-tts-process-numerals-set-thousands-separators ()
  "Replace narrow space with language-specific thousands separator.
Some TTS engines do not read numbers correctly when they are not separated by
periods or commas (depending on the language)."
  (goto-char (point-min))
  (let* ((lang (tlon-tts-get-current-language))
	 (default-separator (tlon-lookup tlon-tts-thousands-separator
					 :separator :language lang))
	 (separator (pcase tlon-tts-engine
		      ("ElevenLabs" default-separator)
		      ("Microsoft Azure" "")
		      (_ default-separator) ; fallback; for engines we haven’t yet tested
		      )))
    (while (re-search-forward (tlon-get-number-separator-pattern lang tlon-default-thousands-separator) nil t)
      (replace-match (replace-regexp-in-string tlon-default-thousands-separator separator (match-string 1)) t t))))

;;;;;;;; Misc

(defun tlon-tts-process-80000 ()
  "Replace 80000 with its verbal equivalent.
Bizarrely, the TTS engine reads 80000 as \"eight thousand\", at least in Spanish."
  (goto-char (point-min))
  (while (re-search-forward "80000" nil t)
    (replace-match (alist-get (tlon-tts-get-current-language) tlon-tts-80000 nil nil #'string=) t t)))

;;;;;; Currencies

(defun tlon-tts-process-currencies ()
  "Format currency with appropriate SSML tags."
  (let ((lang (tlon-tts-get-current-language)))
    (dolist (currency-spec tlon-tts-currencies)
      (let* ((currency-symbol-str (car currency-spec))
             (currency-symbol-re (regexp-quote currency-symbol-str))
             (number-pattern-core (tlon-get-number-separator-pattern lang tlon-default-thousands-separator))
             (lang-units (alist-get lang tlon-tts-currency-units))
             (unit-pattern-optional-group
              (if lang-units
                  ;; Use [[:space:]]* to match zero or more whitespace characters
                  (format "\\(?:[[:space:]]*\\(%s\\)\\)?" ; Group 2 for unit
                          (mapconcat 'regexp-quote lang-units "\\|"))
                ""))
             (full-pattern (format "%s%s%s" currency-symbol-re number-pattern-core unit-pattern-optional-group))
             (currency-words-pair (alist-get lang (cdr currency-spec) nil nil #'string=)))

        (when currency-words-pair
          (goto-char (point-min))
          (while (re-search-forward full-pattern nil t)
            (let* ((amount-str (match-string-no-properties 1))
                   (unit-str (match-string-no-properties 2))
                   (number (tlon-string-to-number amount-str
                                                  tlon-default-thousands-separator
                                                  (tlon-get-decimal-separator lang)))
                   (currency-word (if (= number 1) (car currency-words-pair) (cdr currency-words-pair)))
                   replacement)
              (if unit-str
                  (let* ((lang-prepositions (alist-get lang tlon-tts-currency-unit-prepositions))
                         (preposition (or (alist-get unit-str lang-prepositions nil nil #'string=)
                                          (alist-get t lang-prepositions))))
                    (setq replacement (format "%s %s%s%s" amount-str unit-str preposition currency-word)))
                (setq replacement (format "%s %s" amount-str currency-word)))
              (replace-match replacement t t))))))))

;;;;;; Process unsupported SSML tags

(defun tlon-tts-process-unsupported-ssml-tags ()
  "Remove SSML tags not supported by the current TTS engine."
  (let* ((property (tlon-lookup tlon-tts-engines :property :name tlon-tts-engine))
	 (unsupported-by-them (tlon-lookup-all tlon-tts-supported-tags :tag property nil))
	 (supported-by-us (tlon-lookup-all tlon-tts-supported-tags :tag :tlon t))
	 (tags (cl-intersection unsupported-by-them supported-by-us))
	 tags-to-remove
	 tags-to-chunkify)
    (dolist (tag tags)
      (let ((action (tlon-lookup tlon-tts-supported-tags :if-unsupported :tag tag)))
	(pcase action
	  ('remove (push tag tags-to-remove))
	  ('chunkify (push tag tags-to-chunkify)))))
    (tlon-tts-remove-unsupported-ssml-tags tags-to-remove)
    (tlon-tts-chunkify-unsupported-ssml-tags tags-to-chunkify)))

;;;;;;; Remove

(defun tlon-tts-remove-unsupported-ssml-tags (tags)
  "Remove unsupported SSML TAGS."
  (dolist (tag tags)
    (let ((cons (tlon-tts-get-cons-for-unsupported-ssml-tags tag)))
      (goto-char (point-min))
      (while (re-search-forward (car cons) nil t)
	(let ((replacement (tlon-tts-get-replacement-for-unsupported-ssml-tags cons)))
	  (replace-match replacement t t))))))

(defun tlon-tts-get-replacement-for-unsupported-ssml-tags (cons)
  "Return the replacement text from the CONS cell of an unsupported SSML tag.
The car of CONS is the search pattern and its cdr is the number of the group
capturing the replacement text. If the cdr is nil, replace with an empty string."
  (let* ((cdr (cdr cons))
	 (replacement (cond
		       ((null cdr)
			"")
		       ((numberp cdr)
			(match-string-no-properties cdr))
		       (t (match-string 2)))))
    (replace-regexp-in-string "\\\\" "\\\\\\\\" replacement)))

(defun tlon-tts-get-cons-for-unsupported-ssml-tags (tag)
  "Return a cons cell for an unsupported SSML TAG.
The car of the cons cell is the search pattern and its cdr is the number of
group capturing the replacement text. If the cdr is nil, replace with an empty
string. See the end of the `tlon-tts-supported-tags' docstring for details."
  (let ((replacement (or (tlon-lookup tlon-tts-supported-tags :replacement :tag tag)
			 (tlon-md-get-tag-pattern (symbol-name tag))))
	(cdr 2)
	car)
    (if (listp replacement)
	(setq car (car replacement)
	      cdr (cdr replacement))
      (setq car replacement))
    (cons car cdr)))

;;;;;;; Chunkify

(defun tlon-tts-get-voice-id-from-name (name)
  "Return the voice ID for the voice with friendly NAME for the current engine.
Signals an error if the name is not found."
  (let* ((voices-var (tlon-lookup tlon-tts-engines :voices-var :name tlon-tts-engine))
	 (voices (when voices-var (symbol-value voices-var)))
	 (voice-id (tlon-lookup voices :id :name name)))
    (unless voice-id
      (user-error "Voice name '%s' not found for engine '%s'" name tlon-tts-engine))
    voice-id))

;; This assumes only one function will chunkify, since otherwise successive
;; functions will change positions of their predecessors
(defun tlon-tts-chunkify-unsupported-ssml-tags (tags)
  "Chunkify unsupported SSML TAGS.
This removes the tag but stores its position and the associated voice ID in
`tlon-tts-voice-chunks'. It handles cases where the tag's `name' attribute
contains either a friendly name or a voice ID."
  (tlon-tts-reposition-closing-voice-tag)
  (setq tlon-tts-voice-chunks '())
  (dolist (tag tags)
    (let ((cons (tlon-tts-get-cons-for-unsupported-ssml-tags tag))
	  extracted-name-or-id voice-id replacement)
      (goto-char (point-min))
      (while (re-search-forward (car cons) nil t)
	;; Get value from tag attribute (group 4) - could be name or ID
	(setq extracted-name-or-id (match-string-no-properties 4))
	;; Try to look up the extracted value as a friendly name
	(setq voice-id (ignore-errors (tlon-tts-get-voice-id-from-name extracted-name-or-id)))
	;; If lookup failed, assume the extracted value was already the ID
	(unless voice-id
	  (setq voice-id extracted-name-or-id))
        ;; Check if we actually got a valid ID (might still be nil if tag was malformed)
        (unless voice-id
          (warn "Could not determine voice ID for tag at position %d. Skipping chunk marker." (match-beginning 0))
          ;; Skip adding to tlon-tts-voice-chunks if ID is invalid
          (setq replacement (tlon-tts-get-replacement-for-unsupported-ssml-tags cons))
          (replace-match replacement t t))
        ;; If we have a valid voice ID
        (when voice-id
          ;; Get the replacement text (usually the content inside the tag)
          (setq replacement (tlon-tts-get-replacement-for-unsupported-ssml-tags cons))
          ;; Replace the tag with its content
          (replace-match replacement t t)
          ;; Store the position and the determined voice ID
          (goto-char (match-beginning 0)) ; Point is now at the start of the replaced content
          (push (cons (point-marker) voice-id) tlon-tts-voice-chunks)))))
  (setq tlon-tts-voice-chunks (nreverse tlon-tts-voice-chunks)))

(defun tlon-tts-reposition-closing-voice-tag ()
  "Reposition the trailing closing `voice' tag to the end of the buffer."
  (let ((tag "</voice>")) ; TODO: get this properly from `tlon-tag-specs'
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward tag nil t)
	(replace-match "" t t)
	(tlon-md-end-of-buffer-dwim)
	(insert (concat tag "\n\n"))))))

;;;;;; Remove final `break' tag

;; Having a final break tag sometimes caused Elevenlabs to add a strange sound at the end of the narration.

(defun tlon-tts-remove-final-break-tag ()
  "Remove the final `break' tag from the buffer."
  (goto-char (point-max))
  (when (re-search-backward (tlon-md-get-tag-pattern "break") nil t)
    (replace-match "" t t)))

;;;;;; Remove extra newlines

(defun tlon-tts-remove-extra-newlines ()
  "Remove extra newlines in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\n\\{3,\\}" nil t)
    (replace-match "\n\n" t t)))

;;;;;; Escape xml special chars

(defun tlon-tts-escape-xml-special-characters ()
  "Escape XML special characters in the current buffer for SSML."
  (let ((escaped-content (tlon-tts-escape-xml-special-characters-in-text (buffer-string))))
    (erase-buffer)
    (insert escaped-content)))

(defun tlon-tts-escape-xml-special-characters-in-text (text)
  "Escape XML special characters in TEXT for SSML."
  (replace-regexp-in-string "[&<>\"']"
			    (lambda (match)
			      (pcase match
				("&" "&amp;")
				("<" "&lt;")
				(">" "&gt;")
				("\"" "&quot;")
				("'" "&apos;")))
			    text))

;;;;;; Chunk comments

(defun tlon-tts-insert-chunk-comments ()
  "Insert \"<!-- Chunk N -->\" comments before each chunk in the staging buffer.
This function iterates over `tlon-tts-chunks`, using the `begin-marker`
and `chunk-number` from each chunk's data. It assumes `tlon-tts-chunks`
is populated and the current buffer is the staging buffer."
  (when (tlon-tts-staging-buffer-p)
    (save-excursion
      ;; First, remove any existing chunk comments to avoid duplicates
      (goto-char (point-min))
      (while (re-search-forward "^<!-- Chunk [0-9]+ -->\n?" nil t)
        (replace-match "" t t))

      ;; Then, insert new chunk comments
      (dolist (chunk-data tlon-tts-chunks)
        (let ((chunk-number (nth tlon-tts-chunk-index-chunk-number chunk-data))
              (begin-marker (nth tlon-tts-chunk-index-begin-marker chunk-data)))
          (when (and chunk-number begin-marker (marker-buffer begin-marker))
            (goto-char begin-marker)
            ;; Ensure the comment itself will start on a new line.
            (unless (bolp)
              (insert "\n"))

            ;; For chunks after the first one, ensure a blank line *before* this comment line.
            ;; Point is currently at the beginning of the line where the comment will be inserted.
            (when (> chunk-number 1)
              (save-excursion
                ;; Move to the end of the previous line.
                (backward-char 1)
                ;; If the character at the end of the previous line is not a newline
                ;; (i.e., the previous line was not already blank), insert one.
                (unless (eq (char-before) ?\n)
                  (insert "\n"))))
            
            (insert (format "<!-- Chunk %d -->\n" chunk-number))))))))

;;;;; Global

;;;;;; Abbreviations

(declare-function tlon-edit-json-mapping "tlon-core")
;;;###autoload
(defun tlon-tts-edit-global-abbreviations ()
  "Add or edit a global abbreviation."
  (interactive)
  (tlon-edit-json-mapping tlon-file-global-abbreviations "Abbreviation: " "Spoken form: "))

;;;;;; Phonetic replacements

;;;###autoload
(defun tlon-tts-edit-global-phonetic-replacements ()
  "Add or edit a global phonetic replacement."
  (interactive)
  (tlon-edit-json-mapping tlon-file-global-phonetic-replacements "Term to replace: " "Replacement: "))

;;;;;; Phonetic transcriptions

;;;###autoload
(defun tlon-tts-edit-global-phonetic-transcriptions ()
  "Add or edit a global phonetic transcription."
  (interactive)
  (tlon-edit-json-mapping tlon-file-global-phonetic-transcriptions "Term to transcribe: " "IPA Transcription: "))

;;;;; Local

;;;;;; Common

(defun tlon-tts-add-in-text-cons-cell (prompts var)
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
    (tlon-tts-set-file-local-variable var (push cons-cell var-value) 'save-excursion)))

(autoload 'modify-file-local-variable "files-x")
(declare-function back-button-local "back-button")
(defun tlon-tts-set-file-local-variable (var value &optional save-excursion)
  "Set VALUE of VAR in file-local variables section.
If VAR has a value already, overwrite it. If SAVE-EXCURSION is non-nil, do not
move point to the file-local variables section."
  (modify-file-local-variable var value 'add-or-replace)
  (hack-local-variables)
  (when save-excursion
    (back-button-local 1)))

;;;;;; Abbreviations

;;;###autoload
(defun tlon-add-local-abbreviation ()
  "Add an in-text abbreviation to the local list."
  (interactive)
  (tlon-tts-add-in-text-cons-cell '("Abbrev: " . "Expanded abbrev: ")
				  'tlon-local-abbreviations))

;;;;;; Replacements

;;;###autoload
(defun tlon-add-local-replacement ()
  "Add an in-text replacement to the local list."
  (interactive)
  (tlon-tts-add-in-text-cons-cell '("Text to replace: " . "Replacement: ")
				  'tlon-local-replacements))

;;;;; Menu

;;;;;; Settings

;;;;;;; Engine

(defun tlon-tts-menu-infix-set-engine-action (prompt &optional _initial _history)
  "Action to take when engine is changed via the transient menu.
Sets `tlon-tts-global-engine' to value selected via completion.
PROMPT is used as the prompt string, _INITIAL and _HISTORY are ignored."
  (let ((value (completing-read prompt (tlon-lookup-all tlon-tts-engines :name))))
    (setq tlon-tts-global-engine value)
    ;; Force refresh of the transient menu to update settings infix
    (transient--redisplay)
    value))  ; Return the selected value

(transient-define-infix tlon-tts-menu-infix-set-engine ()
  "Set the engine."
  :class 'transient-lisp-variable
  :variable 'tlon-tts-global-engine
  :reader 'tlon-tts-menu-infix-set-engine-action)

;;;;;;; Engine settings

(defclass tlon-tts-global-engine-settings-infix (transient-infix)
  ((variable :initarg :variable
	     :initform nil
	     :type (or null symbol)
	     :documentation "Variable to hold some symbol.")
   (custom-value :initarg :custom-value
		 :initform nil
		 :type t
   :documentation "Custom value for the setting."))
  "Infix class for setting engine settings.")

(cl-defmethod transient-tts-model-infix-read ((_obj tlon-tts-global-engine-settings-infix))
  "Read the value for engine settings infix OBJ."
  (tlon-tts-global-engine-settings-reader nil nil nil))

(defun tlon-tts-global-engine-settings-reader (_ _ _)
  "Reader for `tlon-tts-menu-infix-set-engine-settings'.
Reads audio format choices based on the currently selected engine."
  (let* ((choices-var (tlon-lookup tlon-tts-engines :choices-var :name tlon-tts-global-engine))
	 (choices (when choices-var (symbol-value choices-var)))
	 (selection (completing-read
		     (format "Engine settings for %s: " tlon-tts-global-engine)
		     choices)))
    (assoc selection choices)))

(cl-defmethod transient-tts-model-infix-set ((_object tlon-tts-global-engine-settings-infix) value)
  "Set the value of the infix OBJECT to VALUE."
  (let* ((variable-name (tlon-lookup tlon-tts-engines :audio-var :name tlon-tts-global-engine))
	 (variable (and variable-name (intern-soft variable-name))))
    (when variable
      (set variable value)
      value)))

(defun tlon-tts-menu-infix-set-engine-settings-action ()
  "Set the engine settings."
  (interactive)
  (let* ((infix (transient-suffix-object 'tlon-tts-menu-infix-set-engine-settings))
	 (value (transient-tts-model-infix-read infix)))
    (transient-tts-model-infix-set infix value)
    (transient--show)))

(transient-define-infix tlon-tts-menu-infix-set-engine-settings ()
  "Set the engine settings."
  :class 'tlon-tts-global-engine-settings-infix
  :argument "-s"
  :description "Settings"
  :key "-s"
  :command #'tlon-tts-menu-infix-set-engine-settings-action)

;;;;;;; Prompt

(transient-define-infix tlon-tts-menu-infix-set-prompt ()
  "Set the prompt."
  :class 'transient-lisp-variable
  :variable 'tlon-tts-prompt
  :reader 'tlon-tts-prompt-reader)

(defun tlon-tts-prompt-reader (_ _ _)
  "Reader for `tlon-tts-menu-infix-set-prompt'."
  (let* ((language (tlon-tts-get-current-language))
	 (prompts (alist-get language tlon-tts-prompts nil nil #'string=)))
    (completing-read "Prompt: " prompts)))

;;;;;;; Paragraph break duration

(transient-define-infix tlon-tts-paragraph-break-duration-infix ()
  :class 'transient-lisp-variable
  :variable 'tlon-tts-paragraph-break-duration
  :reader 'tlon-tts-paragraph-break-duration-reader)

(defun tlon-tts-paragraph-break-duration-reader (_ _ _)
  "Reader for `tlon-tts-paragraph-break-duration-infix'."
  (read-string "Duration (seconds): " tlon-tts-paragraph-break-duration))

;;;;;;; Alternate voice

(transient-define-infix tlon-tts-menu-infix-toggle-alternate-voice ()
  "Toggle the value of `tlon-tts-use-alternate-voice' in `tts' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-tts-use-alternate-voice
  :reader 'tlon-tts-alternate-voice-reader)

(defun tlon-tts-alternate-voice-reader (_ _ _)
  "Reader for `tlon-tts-menu-infix-toggle-alternate-voice'."
  (tlon-transient-toggle-variable-value 'tlon-tts-use-alternate-voice))

;;;;;;; Delete chunks

(transient-define-infix tlon-tts-menu-infix-toggle-delete-file-chunks-after-finalizing ()
  "Toggle the value of `tlon-tts-delete-file-chunks' in `tts' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-tts-delete-file-chunks
  :reader 'tlon-tts-delete-file-chunks-reader)

(defun tlon-tts-delete-file-chunks-reader (_ _ _)
  "Reader for `tlon-tts-menu-infix-toggle-delete-file-chunks-after-finalizing'."
  (tlon-transient-toggle-variable-value 'tlon-tts-delete-file-chunks))

;;;;;;; Generate missing chunks only

(transient-define-infix tlon-tts-menu-infix-toggle-generate-missing-chunks-only ()
  "Toggle the value of `tlon-tts-generate-missing-chunks-only' in `tts' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-tts-generate-missing-chunks-only
  :reader 'tlon-tts-generate-missing-chunks-only-reader)

(defun tlon-tts-generate-missing-chunks-only-reader (_ _ _)
  "Reader for `tlon-tts-menu-infix-toggle-generate-missing-chunks-only'."
  (tlon-transient-toggle-variable-value 'tlon-tts-generate-missing-chunks-only))

;;;;;;; Narrate sidenotes

(transient-define-infix tlon-tts-menu-infix-toggle-narrate-sidenotes ()
  "Toggle the value of `tlon-tts-narrate-sidenotes' in `tts' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-tts-narrate-sidenotes
  :reader 'tlon-tts-narrate-sidenotes-reader)

(defun tlon-tts-narrate-sidenotes-reader (_ _ _)
  "Reader for `tlon-tts-menu-infix-toggle-narrate-sidenotes'."
  (tlon-transient-toggle-variable-value 'tlon-tts-narrate-sidenotes))

;;;;;;; Normalize audio

(transient-define-infix tlon-tts-menu-infix-toggle-normalize-audio ()
  "Toggle the value of `tlon-tts-normalize-audio' in `tts' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-tts-normalize-audio
  :reader 'tlon-tts-normalize-audio-reader)

(defun tlon-tts-normalize-audio-reader (_ _ _)
  "Reader for `tlon-tts-menu-infix-toggle-normalize-audio'."
  (tlon-transient-toggle-variable-value 'tlon-tts-normalize-audio))

;;;;;; Main menu

;;;###autoload (autoload 'tlon-tts-menu "tlon-tts" nil t)
(transient-define-prefix tlon-tts-menu ()
  "`tts' menu."
  [["Narration"
    ("s" "Stage content"                                        tlon-tts-stage-content)
    ("b" "Narrate buffer"                                       tlon-tts-narrate-staged-buffer)
    ("c" "Narrate chunks"                                       tlon-tts-narrate-staged-chunks)
    ""
    ("e" "Generate report"                                      tlon-tts-generate-report)
    """"
    "Narration options"
    ("-b" "Paragraph break duration"                            tlon-tts-paragraph-break-duration-infix)
    ("-p" "Prompt"                                              tlon-tts-menu-infix-set-prompt)
    ("-s" "Narrate sidenotes"                                   tlon-tts-menu-infix-toggle-narrate-sidenotes)
    ("-v" "Use alternate voice"                                 tlon-tts-menu-infix-toggle-alternate-voice)
    ""
    ("-e" "Engine"                                              tlon-tts-menu-infix-set-engine)
    ("-t" "Engine settings"                                     tlon-tts-menu-infix-set-engine-settings)]
   ["File processing"
    ("f" "Finalize audio processing"                            tlon-tts-finalize-audio-processing)
    ("m" "Move file to audio dir"                               tlon-tts-move-file-to-audio-server)
    ""
    ("d" "Delete file chunks"                                   tlon-tts-delete-chunks-of-file)
    ("n" "Normalize audio file(s)"                              tlon-tts-normalize-audio-files)
    ("x" "Truncate audio file"                                  tlon-tts-truncate-audio-file)
    ("o" "Open audio dir"                                       tlon-tts-open-audio-directory)
    ""
    "File processing options"
    ("-n" "Normalize audio during finalization"                 tlon-tts-menu-infix-toggle-normalize-audio)
    ("-d" "Delete file chunks after finalizing"                 tlon-tts-menu-infix-toggle-delete-file-chunks-after-finalizing)
    ("-k" "When narrating buffer, generate missing chunks only" tlon-tts-menu-infix-toggle-generate-missing-chunks-only)]
   ["Edit"
    "global"
    ("a" "Abbreviation"                                         tlon-tts-edit-global-abbreviations)
    ("r" "Replacement"                                          tlon-tts-edit-global-phonetic-replacements)
    ("t" "Transcription"                                        tlon-tts-edit-global-phonetic-transcriptions)
    ""
    "local"
    ("A" "Abbreviation"                                         tlon-add-local-abbreviation)
    ("R" "Replacement"                                          tlon-add-local-replacement)]])

(provide 'tlon-tts)
;;; tlon-tts.el ends here
