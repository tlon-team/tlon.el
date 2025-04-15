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

(defcustom tlon-tts-global-engine "Microsoft Azure"
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
    ("ko" . ("당신은 한국어 원어민입니다. 중립적인 한국어 악센트로 말합니다.")))
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
    ("audio-16khz-16bit-32kbps-mono-opus" . "opus")
    ("audio-16khz-32kbitrate-mono-mp3" . "mp3")
    ("audio-16khz-64kbitrate-mono-mp3" . "mp3")
    ("audio-16khz-128kbitrate-mono-mp3" . "mp3")
    ("audio-24khz-16bit-24kbps-mono-opus" . "opus")
    ("audio-24khz-16bit-48kbps-mono-opus" . "opus")
    ("audio-24khz-48kbitrate-mono-mp3" . "mp3")
    ("audio-24khz-96kbitrate-mono-mp3" . "mp3")
    ("audio-24khz-160kbitrate-mono-mp3" . "mp3")
    ("audio-48khz-96kbitrate-mono-mp3" . "mp3")
    ("audio-48khz-192kbitrate-mono-mp3" . "mp3")
    ("ogg-16khz-16bit-mono-opus" . "opus")
    ("ogg-24khz-16bit-mono-opus" . "opus")
    ("ogg-48khz-16bit-mono-opus" . "opus")
    ("raw-8khz-8bit-mono-alaw" . "alaw")
    ("raw-8khz-8bit-mono-mulaw" . "mulaw")
    ("raw-8khz-16bit-mono-pcm" . "pcm")
    ("raw-16khz-16bit-mono-pcm" . "pcm")
    ("raw-16khz-16bit-mono-truesilk" . "sil")
    ("raw-22050hz-16bit-mono-pcm" . "pcm")
    ("raw-24khz-16bit-mono-pcm" . "pcm")
    ("raw-24khz-16bit-mono-truesilk" . "sil")
    ("raw-44100hz-16bit-mono-pcm" . "pcm")
    ("raw-48khz-16bit-mono-pcm" . "pcm")
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
  '(("MP3" . "mp3")
    ("FLAC" . "flac")
    ("MULAW" . "mulaw")
    ("AMR" . "amr")
    ("AMR_WB" . "amr_wb")
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
    ("wav" . "wav")
    ("opus" . "opus"))
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
  '(("mp3_44100_32" . "mp3")
    ("mp3_44100_64" . "mp3")
    ("mp3_44100_96" . "mp3")
    ("mp3_44100_128" . "mp3")
    ("mp3_44100_192" . "mp3")
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
  '((:id "es-US-AlonsoNeural" :language "es" :gender "male" :role "main")
    (:id "es-US-PalomaNeural" :language "es" :gender "female" :role "main")
    (:id "es-CO-GonzaloNeural" :language "es" :gender "male")
    (:id "es-CO-SalomeNeural" :language "es" :gender "female")
    (:id "es-MX-DaliaNeural" :language "es" :gender "female" :role "alternate")
    (:id "es-MX-JorgeNeural" :language "es" :gender "male" :role "alternate")
    (:id "es-AR-TomasNeural" :language "es" :gender "male")
    (:id "es-AR-ElenaNeural" :language "es" :gender "female"))
  "Preferred Microsoft Azure voices for different languages.
All the voices in this property list are neural and multilingual, and are the
best male and female voices we were able to identify in each language.

A list of available voices may be found here:
<https://github.com/MicrosoftDocs/azure-docs/blob/main/articles/ai-services/speech-service/includes/language-support/tts.md>.")

(defconst tlon-microsoft-azure-char-limit (* 9 60 14)
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
  '((:id "en-US-Studio-Q" :language "en" :gender "male")
    (:id "en-US-Studio-O" :language "en" :gender "female")
    (:id "es-US-Studio-B" :language "es" :gender "male")
    (:id "es-US-Neural2-A" :language "es" :gender "female"))
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
    (:id "Matthew" :language "en" :gender "male")
    (:id "Lupe" :language "es" :gender "female")
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
    (:id "onyx" :language "es" :gender "male")
    (:id "nova" :language "es" :gender "female")
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
  '((:name "Brian" :id "rncjssM0aAEg1ApKehUP" :language "multilingual" :gender "male")
    (:name "Bruce" :id "qUqZ27WoGID6BUp35xTV" :language "multilingual" :gender "male" :role "main")
    (:name "Hades" :id "y3uxYtdWYpmzg8Wwx2k3" :language "multilingual" :gender "male")
    (:name "Michael" :id "8mLUlN9GCPCERe4bI7Wx" :language "multilingual" :gender "male" :role "alternate")
    (:name "Neal" :id "6JpiWMuXFTicEyWjwDLn" :language "multilingual" :gender "male")
    (:name "Amelia" :id "Lpn2A60EAsgGCWjFue20" :language "multilingual" :gender "female" :role "alternate")
    (:name "Victoria" :id "lm0dJr2LmYD4zn0kFH9E" :language "multilingual" :gender "female" :role "main")
    (:name "Mariluz" :id "m1VE7dnwBN0zMer3LcKv" :language "multilingual" :gender "female" :role "main")
    (:name "Ricardo" :id "CoAqFXxZEa3kpJmE7rDr" :language "multilingual" :gender "male" :role "main")
    (:name "Rob Wiblin" :id "t2fWaFXjl9r1VVHfyeK0" :language "multilingual" :gender "main"
	   :stability 0.40 :similarity_boost 0.80 :style 0 :use_speaker_boost t :speed 1)
    (:name "Nate Silver" :id "IT5b7X3vlpkwXPfi0xBA" :language "multilingual" :gender "main"
	   :stability 0.40 :similarity_boost 0.80 :style 0 :use_speaker_boost t :speed 1))
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
  `((:name "Microsoft Azure"
	   :voices-var tlon-microsoft-azure-voices
	   :audio-var tlon-microsoft-azure-audio-settings
	   :choices-var tlon-microsoft-azure-audio-choices
	   :request-fun tlon-tts-microsoft-azure-make-request
	   :char-limit ,tlon-microsoft-azure-char-limit
	   :property :azure)
    (:name "Google Cloud"
	   :voices-var tlon-google-cloud-voices
	   :audio-var tlon-google-cloud-audio-settings
	   :choices-var tlon-google-cloud-audio-choices
	   :request-fun tlon-tts-google-cloud-make-request
	   :char-limit ,tlon-google-cloud-char-limit
	   :property :google)
    (:name "Amazon Polly"
	   :voices-var tlon-amazon-polly-voices
	   :audio-var tlon-amazon-polly-audio-settings
	   :choices-var tlon-amazon-polly-audio-choices
	   :request-fun tlon-tts-amazon-polly-make-request
	   :char-limit ,tlon-amazon-polly-char-limit
	   :property :polly)
    (:name "OpenAI"
	   :voices-var tlon-openai-voices
	   :audio-var tlon-openai-audio-settings
	   :request-fun tlon-tts-openai-make-request
	   :char-limit ,tlon-openai-char-limit
	   :property :openai)
    (:name "ElevenLabs"
	   :voices-var tlon-elevenlabs-voices
	   :audio-var tlon-elevenlabs-audio-settings
	   :choices-var tlon-elevenlabs-audio-choices
	   :request-fun tlon-tts-elevenlabs-make-request
	   :char-limit ,tlon-elevenlabs-char-limit
	   :property :elevenlabs))
  "Text-to-speech engines and associated properties.")

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

;;;;; Authorhip

(defconst tlon-tts-authorship-pattern
  '((:language "en" :pattern "by %s.")
    (:language "es" :pattern "por %s.")
    (:language "fr" :pattern "par %s.")
    (:language "it" :pattern "di %s.")
    (:language "de" :pattern "von %s.")
    (:language "ar" :pattern "بواسطة %s.")
    (:language "ko" :pattern "%s에 의해.")
    (:language "ja" :pattern "%sによって".))
  "Pattern to use when listing the author(s) of a work.
For example, in English, the pattern is `by %s', where `%s' is replaced by the
author name(s).")

;;;;; Links

(defconst tlon-tts-self-referential-link
  '(("ar" . ("هنا" . "هنا (في النص هناك رابط يشير إلى صفحة وي"))
    ("de" . ("hier" . "hier (im Text gibt es einen Link, der auf eine Webseite zeigt)"))
    ("en" . ("here" . "here (in the text there is a link here pointing to a web page)"))
    ("es" . ("aquí" . "aquí (en el texto hay un enlace que apunta a una página web)"))
    ("fr" . ("ici" . "ici (dans le texte, il y a un lien qui pointe vers une page web)"))
    ("it" . ("qui" . "qui (nel testo c'è un link che punta a una pagina web)"))
    ("ja" . ("ここ" . "ここ (テキストには、ウェブページを指すリンクがあります)"))
    ("ko" . ("여기" . "여기 (텍스트에는 웹 페이지를 가리키는 링크가 있습니다)")))
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
  '(("es" . "a la %s")
    ("en" . "to the power of %s")
    ("fr" . "à la %s")
    ("de" . "zur %s")
    ("it" . "alla %s")
    ("pt" . "à %s")
    ("ar" . "إلى %s")
    ("ko" . "%s 제곱")
    ("ja" . "%s 乗"))
  "Pattern for regular exponents.")

(defconst tlon-tts-irregular-exponents
  '((2 . (("es" . "al cuadrado")
	  ("en" . "squared")
	  ("fr" . "au carré")
	  ("de" . "zum Quadrat")
	  ("it" . "al quadrato")
	  ("pt" . "ao quadrado")
	  ("ar" . "إلى السطح الثاني")
	  ("ko" . "제곱")
	  ("ja" . "二乗")))
    (3 . (("es" . "al cubo")
	  ("en" . "cubed")
	  ("fr" . "au cube")
	  ("de" . "zum Kubik")
	  ("it" . "al cubo")
	  ("pt" . "ao cubo")
	  ("ar" . "إلى السطح الثالث")
	  ("ko" . "세제곱")
	  ("ja" . "三乗")))
    (4 . (("es" . "a la cuarta")
	  ("en" . "to the fourth")
	  ("fr" . "à la quatrième")
	  ("de" . "zur vierten")
	  ("it" . "alla quarta")
	  ("pt" . "à quarta")
	  ("ar" . "إلى السطح الرابع")
	  ("ko" . "네제곱")
	  ("ja" . "四乗")))
    (5 . (("es" . "a la quinta")
	  ("en" . "to the fifth")
	  ("fr" . "à la cinquième")
	  ("de" . "zur fünften")
	  ("it" . "alla quinta")
	  ("pt" . "à quinta")
	  ("ar" . "إلى السطح الخامس")
	  ("ko" . "다섯제곱")
	  ("ja" . "五乗")))
    (6 . (("es" . "a la sexta")
	  ("en" . "to the sixth")
	  ("fr" . "à la sixième")
	  ("de" . "zur sechsten")
	  ("it" . "alla sesta")
	  ("pt" . "à sexta")
	  ("ar" . "إلى السطح السادس")
	  ("ko" . "여섯제곱")
	  ("ja" . "六乗")))
    (7 . (("es" . "a la séptima")
	  ("en" . "to the seventh")
	  ("fr" . "à la septième")
	  ("de" . "zur siebten")
	  ("it" . "alla settima")
	  ("pt" . "à sétima")
	  ("ar" . "إلى السطح السابع")
	  ("ko" . "일곱제곱")
	  ("ja" . "七乗")))
    (8 . (("es" . "a la octava")
	  ("en" . "to the eighth")
	  ("fr" . "à la huitième")
	  ("de" . "zur achten")
	  ("it" . "all'ottava")
	  ("pt" . "à oitava")
	  ("ar" . "إلى السطح الثامن")
	  ("ko" . "여덟제곱")
	  ("ja" . "八乗")))
    (9 . (("es" . "a la novena")
	  ("en" . "to the ninth")
	  ("fr" . "à la neuvième")
	  ("de" . "zur neunten")
	  ("it" . "alla nona")
	  ("pt" . "à nona")
	  ("ar" . "إلى السطح التاسع")
	  ("ko" . "아홉제곱")
	  ("ja" . "九乗"))))
  "List of exponents and their irregular verbal equivalents.")

(defconst tlon-tts-80000
  '(("en" . "Eighty thousand")
    ("es" . "Ochenta mil")
    ("pt" . "Oitenta mil")
    ("fr" . "Quatre-vingt mille")
    ("de" . "Achtzigtausend")
    ("it" . "Ottantamila")
    ("ar" . "ثمانون ألف")
    ("ko" . "팔만")
    ("ja" . "八万"))
  "The number 80000 in different languages.")

(defconst tlon-tts-thousands-separator
  '((:language "en" :separator ",")
    (:language "es" :separator ".")
    (:language "fr" :separator " ")
    (:language "it" :separator ".")
    (:language "de" :separator ".")
    (:language "ar" :separator ",")
    (:language "ko" :separator ",")
    (:language "ja" :separator ","))
  "List of language-specific thousands separators.
The specified separator will replace existing thousands separators, so that the
TTS engine pronounces the numbers correctly.")

(defconst tlon-tts-decimals-separator
  '((:language "en" :separator "")
    (:language "es" :separator "")
    (:language "fr" :separator "")
    (:language "it" :separator "")
    (:language "de" :separator "")
    (:language "ar" :separator "")
    (:language "ko" :separator "")
    (:language "ja" :separator ""))
  "List of language-specific decimals separators.
The specified separator will replace existing thousands separators, so that the
TTS engine pronounces the numbers correctly.")

;;;;; Currencies

(defconst tlon-tts-currencies
  '(("₿" . (("en" . ("bitcoin" . "bitcoins"))
	    ("es" . ("bitcoin" . "bitcoins"))))
    ("Ξ" . (("en" . ("ether" . "ether"))
	    ("es" . ("éter" . "éter"))))
    ("£" . (("en" . ("pound" . "pounds"))
	    ("es" . ("libra" . "libras"))))
    ("₪" . (("en" . ("shekel" . "shekels"))
	    ("es" . ("séquel" . "séqueles")))) ; https://www.fundeu.es/recomendacion/shekel-shequel-sekel-sequel/
    ("₹" . (("en" . ("rupee" . "rupees"))
	    ("es" . ("rupia" . "rupias"))))
    ("¥" . (("en" . ("yen" . "yens"))
	    ("es" . ("yen" . "yenes"))))
    ("$" . (("en" . ("dollar" . "dollars"))
	    ("es" . ("dólar" . "dólares")))))
  "Currency symbols and their associated three-letter codes.")

;;;;; Tag sections

(defconst tlon-tts-tag-section-names
  '(("es" . "\\(?:más información\\|entradas relacionadas\\|enlaces externos\\)")
    ("en" . "\\(?:further readong\\|related entries\\|external links\\)"))
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

;;;;;; Chunk processing

(defvar tlon-tts-chunks nil
  "List of chunks to be narrated.
Each element is a list: (TEXT VOICE-PARAMS FILENAME REQUEST-ID STATUS).
- TEXT: The text content of the chunk.
- VOICE-PARAMS: A cons cell like (tlon-tts-voice . VOICE-ID) or nil.
- FILENAME: The path to the generated audio file for this chunk.
- REQUEST-ID: The xi-request-id returned by ElevenLabs (nil otherwise/initially).
- STATUS: Processing status symbol ('pending, 'running, 'completed, 'failed).")

(defvar tlon-tts-chunks-to-process 0
  "Number of chunks left to process.")

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
     ("ko" "옆에." . "\n옆의 끝."))
    (blockquote
     ("ar" "اقتباس." . "\nنهاية الاقتباس.")
     ("de" "Zitat." . "\nEnde des Zitats.")
     ("en" "Quote." . "\nEnd of quote.")
     ("es" "Cita." . "\nFin de la cita.")
     ("fr" "Citation." . "\nFin de la citation.")
     ("it" "Citazione." . "\nFine della citazione.")
     ("ja" "引用." . "\n引用の終わり.")
     ("ko" "인용문." . "\n인용문의 끝."))
    (heading
     ("ar" "عنوان." . "")
     ("de" "Überschrift." . "")
     ("en" "Heading." . "")
     ("es" "Sección." ."")
     ("fr" "Titre." . "")
     ("it" "Titolo." . "")
     ("ko" "제목." . "")
     ("ja" "見出し." . ""))
    (image
     ("ar" "هناك صورة هنا." . "\nنهاية الصورة.")
     ("de" "Hier ist ein Bild." . "\nEnde des Bildes.")
     ("en" "There’s an image here." . "\nEnd of image.")
     ("es" "Aquí hay una imagen." . "\nFin de la imagen.")
     ("fr" "Il y a une image ici." . "\nFin de l'image.")
     ("it" "C'è un'immagine qui." . "\nFine dell'immagine.")
     ("ja" "ここに画像があります。" . "\n画像の終わり。")
     ("ko" "여기에 이미지가 있습니다." . "\n이미지의 끝."))
    (image-caption
     ("ar" . " تتبع الصورة تعليقًا يقول: ")
     ("de" . " Das Bild wird von einer Bildunterschrift gefolgt, die lautet: ")
     ("en" . " The image is followed by a caption that reads: ")
     ("es" . " A la imagen le sigue una leyenda que dice: ")
     ("fr" . " L'image est suivie d'une légende qui dit: ")
     ("it" . " L'immagine è seguita da una didascalia che recita: ")
     ("ja" . " 画像の後には次のようなキャプションが続きます: ")
     ("ko" . " 이미지 뒤에는 다음과 같은 캡션이 있습니다: "))
    (note
     ("ar" "ملاحظة." . "\nنهاية الملاحظة.")
     ("de" "Anmerkung." . "\nEnde der Anmerkung.")
     ("en" "Note." . "\nEnd of the note.")
     ("es" "Nota." . "\nFin de la nota.")
     ("fr" "Note." . "\nFin de la note.")
     ("it" "Nota." . "\nFine della nota.")
     ("ja" "ノート." . "\nノートの終わり.")
     ("ko" "노트." . "\n노트의 끝."))
    (embedded
     ("ar" "رسم بياني." . "\nنهاية الرسم البياني.")
     ("de" "Diagramm." . "\nEnde des Diagramms.")
     ("en" "Chart." . "\nEnd of chart.")
     ("es" "Cuadro." . "\nFin del cuadro.")
     ("fr" "Tableau." . "\nFin du tableau.")
     ("it" "Grafico." . "\nFine del grafico.")
     ("ja" "チャート." . "\nチャートの終わり.")
     ("ko" "차트." . "\n차트의 끝."))
    (quote
     ("ar" "(اقتباس)" . "(نهاية الاقتباس)")
     ("de" "(Zitat)" . "(Ende des Zitats)")
     ("en" "(quote)" . "(End of quote)")
     ("es" "(cita)" . "(Fin de la cita)")
     ("fr" "(citation)" . "(Fin de la citation)")
     ("it" "(citazione)" . "(Fine della citazione")
     ("ja" "(引用)" . "(引用の終わり")
     ("ko" "(인용문)" . "(인용문의 끝)"))
    (subheading
     ("ar" "عنوان فرعي." . "")
     ("de" "Unterüberschrift." . "")
     ("en" "Subheading." . "")
     ("es" "Subsección." . "")
     ("fr" "Sous-titre." . "")
     ("it" "Sottotitolo." . "")
     ("ko" "소제목." . "")
     ("ja" "サブタイトル." . ""))
    (table
     ("ar" "هناك جدول هنا.\n" . "\nنهاية الجدول.")
     ("de" "Hier ist eine Tabelle.\n" . "\nEnde der Tabelle.")
     ("en" "There’s a table here.\n" . "\nEnd of the table.")
     ("es" "Aquí hay una tabla.\n" . "\nFin de la tabla.")
     ("fr" "Il y a un tableau ici.\n" . "\nFin du tableau.")
     ("it" "C'è una tabella qui.\n" . "\nFine della tabella.")
     ("ja" "ここにテーブルがあります。\n" . "\nテーブルの終わり。")
     ("ko" "여기에 표가 있습니다.\n" . "\n표의 끝.")))
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
;;;###autoload
(defun tlon-tts-stage-content (&optional content file)
  "Stage the content to be narrated.
If CONTENT is nil, read the region if selected, FILE if non-nil, or the file
visited by the current buffer otherwise."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (content (or content (tlon-tts-get-content nil file))))
    (with-current-buffer (get-buffer-create (tlon-tts-get-staging-buffer-name file))
      (markdown-mode)
      (flycheck-mode -1)
      (erase-buffer)
      (insert content)
      (tlon-tts-set-file-local-vars file)
      (tlon-tts-prepare-staging-buffer))))

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

(declare-function tlon-tex-replace-keys-with-citations "tlon-tex")
(defun tlon-tts-prepare-staging-buffer ()
  "Prepare the current buffer for audio narration."
  (save-excursion
    (tlon-tts-ensure-all-images-have-alt-text)
    (tlon-tts-ensure-all-tables-have-alt-text)
    (tlon-tts-process-notes) ; should be before `tlon-tts-process-citations'?
    (tlon-tts-remove-tag-sections) ; should probably be before `tlon-tts-process-listener-cues'
    (tlon-tts-remove-horizontal-lines) ; should be before `tlon-tts-process-paragraphs'
    (tlon-tex-replace-keys-with-citations nil 'audio) ; should be before `tlon-tts-escape-xml-special-characters'
    (tlon-tts-process-listener-cues) ; should be before `tlon-tts-process-links', `tlon-tts-process-paragraphs'
    (tlon-tts-process-links) ; should probably be before `tlon-tts-process-formatting'
    (tlon-tts-process-formatting) ; should be before `tlon-tts-process-paragraphs'
    (tlon-tts-process-paragraphs)
    (tlon-tts-process-currencies) ; should be before `tlon-tts-process-numerals'
    (tlon-tts-process-numerals)
    (tlon-tts-remove-final-break-tag)
    (tlon-tts-process-local-abbreviations)
    (tlon-tts-process-global-abbreviations)
    (tlon-tts-process-local-phonetic-replacements)
    (tlon-tts-process-global-phonetic-replacements)
    (tlon-tts-process-global-phonetic-transcriptions)
    ;; FIXME: the below is not working properly.
    ;; `tlon-tts-get-replacement-for-unsupported-ssml-tags' is not returning the
    ;; correct value when run with the `phoneme' tag. Fixing it is not a
    ;; priority because Elevenlabs does not support this tag anyway.
    (tlon-tts-process-unsupported-ssml-tags)
    (tlon-tts-remove-extra-newlines)
    ;; FIXME: this is breaking the SSML tags
    ;; (tlon-tts-escape-xml-special-characters)
    (tlon-tts-generate-report)))

;;;;; Narrate

;;;###autoload
(defun tlon-tts-narrate-staged-content ()
  "Narrate the content staged in the current buffer."
  (interactive)
  (unless (tlon-tts-staging-buffer-p)
    (user-error "Not in a staging buffer"))
  (tlon-tts-prepare-chunks)
  (tlon-tts-process-chunks))

(defun tlon-tts-staging-buffer-p ()
  "Return t iff the current buffer is a staging buffer."
  (bound-and-true-p tlon-tts-staging-buffer-p))

;;;;;; Prepare chunks

(defun tlon-tts-prepare-chunks ()
  "Prepare the list of chunks.
For ElevenLabs, if `tlon-elevenlabs-char-limit' is nil, will chunk by paragraph
regardless of size to work around voice degradation issues."
  (let* ((char-limit (tlon-lookup tlon-tts-engines :char-limit :name tlon-tts-engine))
         (destination (tlon-tts-set-destination)) ; Get destination filename
         (chunks (tlon-tts-read-into-chunks char-limit destination))) ; Pass destination
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
    (save-excursion
      (goto-char (point-max))
      (insert local-vars))
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
paragraphs that fit within the size limit. DESTINATION is the base output filename.

If CHUNK-SIZE is nil (specifically for ElevenLabs paragraph mode), each
paragraph becomes a separate chunk, breaking before voice changes.

Voice changes specified in `tlon-tts-voice-chunks' always force a chunk break."
  (goto-char (point-min))
  (let* ((begin (point))
	 (voice-chunk-list tlon-tts-voice-chunks)
	 (use-paragraph-chunks (null chunk-size))
	 chunks current-voice next-voice-change-pos next-voice-id
	 ;; Determine initial voice and update voice-chunk-list
	 (initial-state (tlon-tts--determine-initial-voice voice-chunk-list)))
    (setq current-voice (car initial-state)
	  voice-chunk-list (cdr initial-state))
    (while (< begin (point-max))
      ;; Determine position of the next voice change, if any
      (setq next-voice-change-pos (if voice-chunk-list (marker-position (caar voice-chunk-list)) most-positive-fixnum)
	    next-voice-id (when voice-chunk-list (cdar voice-chunk-list)))
      ;; Calculate the end position for the current chunk
      (let ((end (tlon-tts--calculate-chunk-end begin chunk-size next-voice-change-pos use-paragraph-chunks)))
	;; Add the chunk if it's valid
	(setq chunks (tlon-tts--add-chunk begin end current-voice chunks destination)) ; Pass destination
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
		voice-chunk-list (cdr new-voice-state)))))
    (nreverse chunks)))

;;;;;; Chunking Helpers

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

(defun tlon-tts--add-chunk (begin end current-voice chunks destination)
  "Extract text between BEGIN and END, validate it, and add to CHUNKS.
Returns the updated CHUNKS list. CURRENT-VOICE is the voice to be used for
this chunk. DESTINATION is the base output filename."
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
        (let* ((chunk-index (length chunks)) ; 0-based index for the new chunk
               (filename (tlon-tts-get-chunk-name destination (1+ chunk-index))) ; 1-based for filename
               (voice-params (when current-voice (cons 'tlon-tts-voice current-voice)))
               (new-chunk (list trimmed-text voice-params filename nil 'pending))) ; text voice-params filename request-id status
          (push new-chunk chunks)))))
  chunks)

(defun tlon-tts--update-voice-state (begin next-voice-change-pos next-voice-id current-voice voice-chunk-list)
  "Update current-voice and voice-chunk-list if BEGIN is at a voice change point.
Returns (cons NEW-CURRENT-VOICE NEW-VOICE-CHUNK-LIST)."
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
  "Start processing the first chunk. Subsequent chunks are triggered by the sentinel."
  (setq tlon-tts-chunks-to-process (length tlon-tts-chunks))
  (if (>= tlon-tts-chunks-to-process 1)
      (tlon-tts-generate-audio 0) ; Start with the first chunk (index 0)
    (message "No TTS chunks to process.")))

(defun tlon-tts-generate-audio (chunk-index)
  "Generate audio for the chunk at CHUNK-INDEX.
Triggers the engine-specific request function and sets up the process sentinel."
  (let* ((chunk-data (nth chunk-index tlon-tts-chunks))
         (string (nth 0 chunk-data))
         (voice-params (nth 1 chunk-data))
         (file (nth 2 chunk-data))
         (fun (tlon-lookup tlon-tts-engines :request-fun :name tlon-tts-engine))
         ;; Pass chunk-index to the request function
         (request (funcall fun string file voice-params chunk-index)))

    ;; Mark chunk as running
    (setf (nth 4 chunk-data) 'running) ; Update status in the original list structure

    (when tlon-debug (message "Debug: Running command for chunk %d: %s" chunk-index request))
    (let ((process (start-process-shell-command (format "generate audio %d" chunk-index) nil request)))
      (set-process-sentinel process
                            (lambda (process event)
                              (tlon-tts-process-chunk-sentinel process event chunk-index))))))

(defun tlon-tts-get-chunk-name (file nth)
  "Return the name of the NTH chunk of FILE."
  (let ((extension (file-name-extension file))
	(file-name-sans-extension (file-name-sans-extension file)))
    (format "%s-%03d.%s" file-name-sans-extension nth extension)))

(defun tlon-tts-open-file (file)
  "Open generated TTS FILE."
  (shell-command (format "open %s" file)))

(defun tlon-tts-process-chunk-sentinel (process event chunk-index)
  "Process sentinel for TTS chunk generation at CHUNK-INDEX."
  (let* ((chunk-data (nth chunk-index tlon-tts-chunks))
         (file (nth 2 chunk-data)))
    (cond
     ((string-match "finished" event) ; Process finished successfully
      (let* ((process-buf (process-buffer process))
             ;; Safely get buffer content, default to "" if buffer is gone
             (output (if (buffer-live-p process-buf)
                         (with-current-buffer process-buf (buffer-string))
                       ""))
             (request-id (tlon-tts--parse-elevenlabs-request-id output)))

        (when tlon-debug (message "Debug: Chunk %d finished. Request ID: %s (Output Length: %d)"
                                  chunk-index request-id (length output)))

        ;; Store request ID and mark as completed
        (setf (nth 3 chunk-data) request-id)
        (setf (nth 4 chunk-data) 'completed)

        (setq tlon-tts-chunks-to-process (1- tlon-tts-chunks-to-process))
        (message "Chunk %d/%d processed successfully." (1+ chunk-index) (length tlon-tts-chunks))

        ;; Trigger next chunk if available
        (let ((next-chunk-index (1+ chunk-index)))
          (when (< next-chunk-index (length tlon-tts-chunks))
            (tlon-tts-generate-audio next-chunk-index)))

        ;; If all chunks are done, finalize
        (when (= tlon-tts-chunks-to-process 0)
          (tlon-tts-finish-processing file))))

     ((string-match "exited abnormally" event) ; Process failed
      (setf (nth 4 chunk-data) 'failed) ; Mark as failed
      (message "Error processing chunk %d (%s): %s" chunk-index file event)
      (when-let ((buffer (process-buffer process)))
        (with-current-buffer buffer
          (message "Error output for chunk %d:\n%s" chunk-index (buffer-string))))
      ;; Optionally stop processing further chunks on error
      ;; (setq tlon-tts-chunks-to-process 0)
      )

     (t ; Other events (e.g., signal)
      (message "Process %s (chunk %d): Event occurred - %s" (process-name process) chunk-index event)))))

(defun tlon-tts--parse-elevenlabs-request-id (output)
  "Parse the xi-request-id header from curl OUTPUT."
  (when (string-match-p "^xi-request-id: \\([a-zA-Z0-9]+\\)" output)
    (match-string 1 output)))

(defun tlon-tts-finish-processing (last-chunk-file)
  "Final steps after all chunks are processed: append silence, join, delete, open."
  (let ((file (tlon-tts-get-original-filename last-chunk-file))
        (dired-listing-switches "-alht")) ; Keep this local if only used here
    (message "All chunks processed. Finalizing...")
    (when (tlon-tts-append-silence-to-chunks-p file)
      (tlon-tts-append-silence-to-chunks file)
      (when tlon-debug (message "Debug: Appended silence to chunks.")))
    (tlon-tts-join-chunks file)
    (when tlon-tts-delete-file-chunks
      (tlon-tts-delete-chunks-of-file file))
    (dired (file-name-directory file))
    (shell-command (format "open '%s'" file))
    (message "TTS narration complete for %s" (file-name-nondirectory file))))

(defun tlon-tts-set-chunk-file (file)
  "Set chunk file based on FILE.
If FILE is nil, use the file visited by the current buffer, the file at point in
Dired, or prompt the user for a file (removing the chunk numbers if necessary)."
  (or file
      (buffer-file-name)
      (tlon-tts-get-original-filename (dired-get-filename))
      (tlon-tts-get-original-filename (read-file-name "File: "))))

(defun tlon-tts-join-chunks (&optional file)
  "Join chunks of FILE back into a single file.
If FILE is nil, use the file visited by the current buffer, the file at point in
Dired, or prompt the user for a file (removing the chunk numbers if necessary)."
  (interactive)
  (let* ((file (tlon-tts-set-chunk-file file))
	 (files (tlon-tts-get-list-of-chunks file))
	 (list-of-files (tlon-tts-create-list-of-chunks files)))
    (shell-command (format "ffmpeg -y -f concat -safe 0 -i %s -c copy %s"
			   list-of-files file)))
  (when (derived-mode-p 'dired-mode)
    (revert-buffer)))

(defun tlon-tts-get-list-of-chunks (file)
  "Return a list of the existing file chunks for FILE.
The file chunks are the files in the same directory as FILE that have the same
base name and extension as FILE, but with a number appended to the base name
before the extension. The list is sorted in ascending order by the appended
number. If no chunks are found, return nil."
  (let* ((dir (file-name-directory file))
	 (base-name (file-name-base file))
	 (extension (file-name-extension file))
	 (pattern (concat "^" (regexp-quote base-name) "-.*\\." (regexp-quote extension) "$"))
	 (files (directory-files dir t pattern)))
    (sort files #'string<)))

(defun tlon-tts-delete-chunks-of-file (&optional file)
  "Delete the chunks of FILE. Also delete the staging buffer."
  (interactive)
  (let* ((file (tlon-tts-set-chunk-file file))
	 (buffer-name (tlon-tts-get-staging-buffer-name file)))
    (dolist (chunk-file (tlon-tts-get-list-of-chunks file))
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
  "Return the filename from which chunk FILE derives."
  (let* ((base-name (file-name-sans-extension file))
	 (extension (file-name-extension file))
	 (original-base-name (replace-regexp-in-string "-[0-9]+\\'" "" base-name)))
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
  (shell-command (format "ffmpeg -f lavfi -i anullsrc=r=44100:cl=mono -t %s -b:a 128k %s"
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
    (shell-command (format "ffmpeg -f concat -safe 0 -i %s -c copy -f mpegts %s"
			   (shell-quote-argument concat-file)
			   (shell-quote-argument intermediate-file)))
    (shell-command (format "ffmpeg -i %s -c copy %s"
			   (shell-quote-argument intermediate-file)
			   (shell-quote-argument output-file)))
    (delete-file concat-file)
    (delete-file intermediate-file)))

(defun tlon-tts-append-silence-to-file (file duration)
  "Append silence of DURATION seconds to FILE."
  (let ((silence-file (concat (file-name-sans-extension file) "-silence.mp3"))
	(output-file (concat (file-name-sans-extension file) "-silent.mp3")))
    (tlon-tts-create-silence-file duration silence-file)
    (tlon-tts-concatenate-files file silence-file output-file)
    (rename-file output-file file t)
    (delete-file silence-file)))

(defun tlon-tts-append-silence-to-chunks (file)
  "Append a silence to each chunk of FILE."
  (let ((duration (replace-regexp-in-string "[[:alpha:]]" "" tlon-tts-paragraph-break-duration)))
    (message "Appending silence of %s to chunks of `%s'..."
	     tlon-tts-paragraph-break-duration (file-name-nondirectory file))
    (dolist (chunk (tlon-tts-get-list-of-chunks file))
      (tlon-tts-append-silence-to-file chunk duration))))

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

(defun tlon-tts-microsoft-azure-make-request (string destination &optional parameters)
  "Make a request to the Microsoft Azure text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a cons cell of parameters to use when generating the audio, where
the car is the name of the file-local variable the cdr is its overriding value."
  (let ((vars (tlon-tts-get-file-local-or-override
	       '(tlon-tts-audio
		 tlon-tts-locale
		 tlon-tts-voice)
	       parameters)))
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

(defun tlon-tts-google-cloud-make-request (string destination &optional parameters)
  "Make a request to the Google Cloud text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a cons cell of parameters to use when generating the audio, where
the car is the name of the file-local variable the cdr is its overriding value."
  (let ((vars (tlon-tts-get-file-local-or-override
	       '(tlon-tts-locale
		 tlon-tts-voice)
	       parameters)))
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

(defun tlon-tts-amazon-polly-make-request (string destination &optional parameters)
  "Construct the AWS CLI command to call Amazon Polly.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a cons cell of parameters to use when generating the audio, where
the car is the name of the file-local variable the cdr is its overriding value."
  (let ((vars (tlon-tts-get-file-local-or-override
	       '(tlon-tts-audio
		 tlon-tts-voice)
	       parameters)))
    (cl-destructuring-bind (audio voice) vars
      (format tlon-amazon-polly-request
	      (car audio) voice string tlon-amazon-polly-region destination))))

;;;;;;; OpenAI

(defun tlon-tts-openai-make-request (string destination &optional parameters)
  "Make a request to the OpenAI text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a cons cell of parameters to use when generating the audio, where
the car is the name of the file-local variable the cdr is its overriding value."
  (let ((vars (tlon-tts-get-file-local-or-override
	       '(tlon-tts-voice)
	       parameters)))
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
	    (auth-source-pass-get "emacs" (concat "tlon/core/openai.com/" tlon-email-shared)))))

;;;;;;; ElevenLabs

(defun tlon-tts-elevenlabs-make-request (string destination parameters chunk-index)
  "Make a request to the ElevenLabs text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a list of cons cells with parameters to use when generating the
audio. CHUNK-INDEX is the index of the current chunk."
  (let* (;; Ensure parameters is a list for assoc
         (parameters-alist (if parameters (list parameters) nil))
         (vars (tlon-tts-get-file-local-or-override
                '(tlon-tts-voice
                  tlon-tts-audio)
                parameters-alist))
         ;; Get before/after text from the main chunks list if needed (paragraph mode)
         (before-text (when (and (> chunk-index 0) (null tlon-elevenlabs-char-limit))
                        (nth 0 (nth (1- chunk-index) tlon-tts-chunks))))
         (after-text (when (and (< (1+ chunk-index) (length tlon-tts-chunks)) (null tlon-elevenlabs-char-limit))
                       (nth 0 (nth (1+ chunk-index) tlon-tts-chunks))))
         ;; Get previous request ID if available
         (previous-chunk-id (when (> chunk-index 0)
                              (let ((prev-chunk (nth (1- chunk-index) tlon-tts-chunks)))
                                (when (eq (nth 4 prev-chunk) 'completed) ; Check status
                                  (nth 3 prev-chunk))))) ; Get request-id
         (voice-settings-params '(:stability :similarity_boost :style :use_speaker_boost :speed))) ; Define params to extract
    (cl-destructuring-bind (voice audio) vars
      ;; Look up the full voice definition using the voice ID
      (let* ((voice-definition (cl-find-if (lambda (entry) (equal (plist-get entry :id) voice))
                                           tlon-elevenlabs-voices))
             ;; Extract voice settings if they exist in the definition
             (voice-settings
              ;; Build the voice_settings alist with STRING keys
              (delq nil
                    (mapcar (lambda (param)
                              (when-let ((value (plist-get voice-definition param)))
                                (cons (symbol-name param) ; Ensure string key, e.g., "stability"
                                      (if (eq param :use_speaker_boost)
                                          (if value t nil) ; Use standard Elisp booleans t/nil
                                        value))))
                            voice-settings-params)))
             ;; Define base payload parts as an alist with STRING keys
             (payload-parts
              `(("text" . ,string)
                ("model_id" . ,tlon-elevenlabs-model)
                ,@(when before-text `(("before_text" . ,before-text)))
                ,@(when after-text `(("after_text" . ,after-text)))
                ;; Add previous_request_ids if available
                ,@(when previous-chunk-id `(("previous_request_ids" . (,previous-chunk-id))))
                ;; Note: next_request_ids could be added similarly if needed/available
                ("stitch_audio" . ,(if (or before-text after-text previous-chunk-id) t nil)))) ; Use standard Elisp booleans t/nil
             ;; Add voice settings if they exist
             (final-payload-parts
              (if voice-settings
                  ;; Ensure the voice_settings alist itself is the value for the "voice_settings" key
                  (append payload-parts `(("voice_settings" . ,voice-settings)))
                payload-parts))
             ;; Encode the final payload alist (with nested alist) to JSON string
             ;; Explicitly set json-key-type to 'string to ensure correct key encoding
             (payload (let ((json-key-type 'string))
                        (json-encode final-payload-parts))))
        (mapconcat 'shell-quote-argument
                   (list "curl"
                         "--request" "POST"
			 "--url" (format tlon-elevenlabs-tts-url voice (car audio))
			 "--header" "Content-Type: application/json"
			 "--header" (format "xi-api-key: %s" (tlon-tts-elevenlabs-get-or-set-key))
                         ;; Include headers in output
                         "-i"
			 "--data" payload
			 "--output" destination)
		   " ")))))

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
		(pattern (tlon-lookup tlon-tts-authorship-pattern :pattern :language (tlon-tts-get-current-language)))
		(author-part (concat (format pattern author-string) separator)))
	  (concat title-part author-part)
	title-part))))

;;;;; Get SSML

(defun tlon-tts-get-ssml-break (time)
  "Return an SSML `break' tag with `time' attribute of TIME."
  (tlon-md-get-tag-filled "break" `(,time)))

;;;;; File uploading

;;;###autoload
(defun tlon-tts-move-file-to-audio-server (&optional file)
  "Upload audio FILE to the audio repo."
  (interactive)
  (let* ((file (files-extras-read-file file))
	 (lang (tlon-tts-get-current-language))
	 (destination (file-name-concat (tlon-tts-get-audio-directory lang)
					(file-name-nondirectory file))))
    (rename-file file destination 0)
    (message "Moved `%s' to `%s'." file destination)))

(defun tlon-tts-get-audio-directory (&optional lang)
  "Return the directory where audio files are stored for LANG.
If LANG is nil, get it from the current language process."
  (let ((dir (tlon-repo-lookup :dir :name "uqbar-audio"))
	(lang (tlon-tts-get-current-language lang)))
    (file-name-concat dir lang)))

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
`tlon-tts-process-citations'."
  (goto-char (point-min))
  (while (re-search-forward markdown-regex-footnote nil t)
    (let ((note (tlon-tts-get-note))
	  reposition)
      (markdown-footnote-kill)
      (when (not (string-empty-p note))
	(unless (looking-back (concat "\\.\\|" markdown-regex-footnote) (line-beginning-position))
	  (setq reposition t))
	(when (eq (tlon-get-note-type note) 'sidenote)
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

;;;;;;;; Formatting

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
  "Replace text enclosed in a `ReplaceAudio' MDX tag with its `text' attribute."
  (goto-char (point-min))
  (while (re-search-forward (tlon-md-get-tag-pattern "ReplaceAudio") nil t)
    (let* ((text (match-string 4))
	   (role (match-string 6))
	   replacement voice)
      (save-match-data
	(setq voice (tlon-tts-get-voice-of-role role))
	(setq replacement (if (string= voice (tlon-tts-get-voice-at-point))
			      text
			    (tlon-tts-enclose-in-voice-tag text voice))))
      (replace-match replacement t t))))

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
  (let ((case-fold-search nil))
    (dolist (entry (pcase type
		     ('local tlon-local-abbreviations-for-session)
		     ('global (tlon-tts-get-global-abbreviations))))
      (cl-destructuring-bind (abbrev . expansion) entry
	(let ((abbrev-introduced (format "%1$s (%2$s)\\|%1$s \\[%2$s\\]\\|%2$s (%1$s)\\|%2$s \\[%1$s\\]"
					 expansion abbrev)))
	  ;; we first replace the full abbrev introduction, then the abbrev itself
	  (dolist (cons (list (cons abbrev-introduced expansion) entry))
	    (goto-char (point-min))
	    (while (re-search-forward (car cons) nil t)
	      (tlon-tts-punctuate-abbrev-replacement (cdr cons)) t t)))))))

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
  "Add listener cues for text enclosed in tags of TYPE."
  (goto-char (point-min))
  (while (re-search-forward (tlon-md-get-tag-pattern "Figure") nil t)
    (if-let* ((alt (match-string-no-properties 6)))
	(let ((caption (match-string-no-properties 2))
	      caption-cue text)
	  (unless (string-empty-p caption)
	    (setq caption-cue (alist-get (tlon-tts-get-current-language)
					 (alist-get 'image-caption tlon-tts-listener-cues)
					 nil nil #'string=)))
	  (setq text (concat alt (when caption (concat caption-cue caption))))
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
      (when abbreviations
	(insert "***Missing abbreviations***\n\n")
	(dolist (abbreviation abbreviations)
	  (insert (format "%s\n" abbreviation))))
      (when chemical-symbols-p
	(insert (format "\n***Chemical symbols***\n\nSearch for ‘%s’\n"
			tlon-tts-maybe-chemical-symbol)))
      (when emphasis-p
	(insert (format "\n***Emphasis***\n\nRun ‘M-x tlon-manual-fixe-emphasis’\n")))
      (when en-dashes-p
	(insert (format "\n***En dashes***\n\nSearch for ‘–’")))
      (when numerals-sans-separator-p
	(insert (format "\n***Numerals sans separator***\n\nRun ‘M-x tlon-manual-fix-add-thousands-separators’\n"))))
    (switch-to-buffer report-buffer)
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
      (let ((match (match-string-no-properties 0)))
	(unless (or (string-match (mapconcat 'car abbrevs "\\|") match)
		    (tlon-tts-looking-at-excluded-tag-p))
	  (push match missing))))
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

(defvar tlon-fix-numerals-sans-separator)
(defun tlon-tts-get-numerals-sans-separator ()
  "Return it iff the current buffer has numerals without thousands separators.
We need to use separators to get some TTS engines (such as Elevenlabs) to
pronounce the numbers correctly. This requires manual processing since some
numbers, such as years, should not be separated."
  (tlon-tts-check-unprocessed tlon-fix-numerals-sans-separator))

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
    (dolist (cons tlon-tts-currencies)
      (let* ((pair (alist-get lang (cdr cons) nil nil #'string=))
	     (symbol (regexp-quote (car cons)))
	     (pattern (format "\\(?4:%s\\)%s" symbol
			      (tlon-get-number-separator-pattern lang tlon-default-thousands-separator))))
	(goto-char (point-min))
	(while (re-search-forward pattern nil t)
	  (let* ((amount (match-string-no-properties 1))
		 (number (tlon-string-to-number amount tlon-default-thousands-separator))
		 (words (if (= number 1) (car pair) (cdr pair)))
		 (replacement (format "%s %s" amount words)))
	    (replace-match replacement t t)))))))

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
This removes the tag but stores its position and the associated voice ID
\\=(looked up from the friendly name in the tag's `name' attribute) in
`tlon-tts-voice-chunks'."
  (tlon-tts-reposition-closing-voice-tag)
  (setq tlon-tts-voice-chunks '())
  (dolist (tag tags)
    (let ((cons (tlon-tts-get-cons-for-unsupported-ssml-tags tag))
	  voice-name voice-id replacement)
      (goto-char (point-min))
      (while (re-search-forward (car cons) nil t)
	;; Get friendly name from tag attribute
	(setq voice-name (match-string-no-properties 4))
	;; Look up the corresponding voice ID
	(setq voice-id (tlon-tts-get-voice-id-from-name voice-name))
	;; Get the replacement text (usually the content inside the tag)
	(setq replacement (tlon-tts-get-replacement-for-unsupported-ssml-tags cons))
	;; Replace the tag with its content
	(replace-match replacement t t)
	;; Store the position and the looked-up voice ID
	(goto-char (match-beginning 0)) ; Point is now at the start of the replaced content
	;; (tlon-tts-move-point-before-break-tag) ; This call was misplaced here
	(push (cons (point-marker) voice-id) tlon-tts-voice-chunks))))
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

;;;;; Global

;;;;;; Abbreviations

(declare-function tlon-edit-json-mapping "tlon-core")
;;;###autoload
(defun tlon-tts-edit-global-abbreviations ()
  "Add or edit a global abbreviation in `tlon-file-global-abbreviations'."
  (interactive)
  (tlon-edit-json-mapping tlon-file-global-abbreviations "Abbreviation: " "Spoken form: "))

;;;;;; Phonetic replacements

;;;###autoload
(defun tlon-tts-edit-global-phonetic-replacements ()
  "Add or edit a global phonetic replacement in `tlon-file-global-phonetic-replacements'."
  (interactive)
  (tlon-edit-json-mapping tlon-file-global-phonetic-replacements "Term to replace: " "Replacement: "))

;;;;;; Phonetic transcriptions

;;;###autoload
(defun tlon-tts-edit-global-phonetic-transcriptions ()
  "Add or edit a global phonetic transcription in `tlon-file-global-phonetic-transcriptions'."
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

(cl-defmethod transient-infix-read ((obj tlon-tts-global-engine-settings-infix))
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

(cl-defmethod transient-init-value ((object tlon-tts-global-engine-settings-infix))
  "Initialize the value of the infix OBJECT."
  (let* ((variable-name (tlon-lookup tlon-tts-engines :audio-var :name tlon-tts-global-engine))
	 (variable (and variable-name (intern-soft variable-name))))
    (when variable
      (setf (slot-value object 'variable) variable)
      (when (boundp variable)
	(setf (slot-value object 'custom-value) (symbol-value variable)))
      (symbol-value variable))))

(cl-defmethod transient-infix-set ((object tlon-tts-global-engine-settings-infix) value)
  "Set the value of the infix OBJECT to VALUE."
  (let* ((variable-name (tlon-lookup tlon-tts-engines :audio-var :name tlon-tts-global-engine))
	 (variable (and variable-name (intern-soft variable-name))))
    (when variable
      (set variable value)
      value)))

(cl-defmethod transient-format-value ((object tlon-tts-global-engine-settings-infix))
  "Format the value of the infix OBJECT."
  (let* ((variable-name (tlon-lookup tlon-tts-engines :audio-var :name tlon-tts-global-engine))
	 (variable (and variable-name (intern-soft variable-name)))
	 (value (when (and variable (boundp variable))
		  (symbol-value variable))))
    (if value
	(format "%s" value)
      "Not set")))

(defun tlon-tts-menu-infix-set-engine-settings-action ()
  "Set the engine settings."
  (interactive)
  (let* ((infix (transient-suffix-object 'tlon-tts-menu-infix-set-engine-settings))
	 (value (transient-infix-read infix)))
    (transient-infix-set infix value)
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

(transient-define-infix tlon-tts-menu-infix-toggle-delete-file-chunks ()
  "Toggle the value of `tlon-tts-delete-file-chunks' in `tts' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-tts-delete-file-chunks
  :reader 'tlon-tts-delete-file-chunks-reader)

(defun tlon-tts-delete-file-chunks-reader (_ _ _)
  "Reader for `tlon-tts-menu-infix-toggle-delete-file-chunks'."
  (tlon-transient-toggle-variable-value 'tlon-tts-delete-file-chunks))

;;;;;; Main menu

;;;###autoload (autoload 'tlon-tts-menu "tlon-tts" nil t)
(transient-define-prefix tlon-tts-menu ()
  "`tts' menu."
  [["Narration"
    ("s" "Stage content"                           tlon-tts-stage-content)
    ("n" "Narrate staged"                          tlon-tts-narrate-staged-content)
    ("e" "Generate report"                         tlon-tts-generate-report)
    ""
    "Narration options"
    ("-a" "Paragraph break duration"               tlon-tts-paragraph-break-duration-infix)
    ("-d" "Delete file chunks"                     tlon-tts-menu-infix-toggle-delete-file-chunks)
    ("-e" "Engine"                                 tlon-tts-menu-infix-set-engine)
    ("-s" "Settings"                               tlon-tts-menu-infix-set-engine-settings)
    ("-p" "Prompt"                                 tlon-tts-menu-infix-set-prompt)
    ("-v" "Use alternate voice"                    tlon-tts-menu-infix-toggle-alternate-voice)
    ""
    ("-D" "Debug"                                  tlon-menu-infix-toggle-debug)]
   ["File processing"
    ("j" "Join file chunks"                        tlon-tts-join-chunks)
    ("d" "Delete file chunks"                      tlon-tts-delete-chunks-of-file)
    ("x" "Truncate audio file"                     tlon-tts-truncate-audio-file)
    ""
    "Audio repo"
    ("o" "Open"                                    tlon-tts-open-audio-directory)
    ("m" "Move file to"                            tlon-tts-move-file-to-audio-server)]
   ["Edit"
    "global"
    ("a" "Abbreviation"                            tlon-tts-edit-global-abbreviations)
    ("r" "Replacement"                             tlon-tts-edit-global-phonetic-replacements)
    ("t" "Transcription"                           tlon-tts-edit-global-phonetic-transcriptions)
    ""
    "local"
    ("A" "Abbreviation"                            tlon-add-local-abbreviation)
    ("R" "Replacement"                             tlon-add-local-replacement)]])

(provide 'tlon-tts)

;;; tlon-tts.el ends here
