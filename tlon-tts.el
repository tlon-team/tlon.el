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
  '(("es" . ("You are a native Spanish speaker. You speak with a neutral Spanish accent."))
    ("en" . ("You are a native English speaker. You speak with a neutral English accent.")))
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
  "Output format and associated extension for the Google Cloud TTS service.")

;;;;; OpenAI

;; TODO: check if the OpenAI API allows for different output formats
(defcustom tlon-openai-audio-settings
  '("mp3" . "mp3")
  "Output format and associated extension for the OpenAI TTS service."
  :group 'tlon-tts
  :type '(cons (string :tag "Name") (string :tag "Extension")))

(defcustom tlon-openai-model
  "tts-1-hd"
  "Model to use for the OpenAI TTS.
Options are

- `\"tts-1\"': Standard model. Provides the lowest latency.

- `\"tts-1-hd\"': Higher quality model.

<https://platform.openai.com/docs/guides/text-to-speech/audio-quality>"
  :group 'tlon-tts
  :type 'string)

;;;;; ElevenLabs

(defcustom tlon-elevenlabs-audio-settings
  '("mp3_44100_128" . "mp3")
  "Output format and associated extension for the ElevenLabs TTS service.
The options are:

- `\"mp3_44100_32\"': mp3 with 44.1kHz sample rate at 32kbps.

- `\"mp3_44100_64\"': mp3 with 44.1kHz sample rate at 64kbps.

- `\"mp3_44100_96\"': mp3 with 44.1kHz sample rate at 96kbps.

- `\"mp3_44100_128\"': mp3 with 44.1kHz sample rate at 128kbps.

- `\"mp3_44100_192\"': mp3 with 44.1kHz sample rate at 192kbps. Requires you to
  be subscribed to Creator tier or above.

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
  stable.\ As of 2024-07-15, it does not support multilingual voices.

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
	  :if-unsupported remove
	  :replacement ,(tlon-md-get-tag-pattern "say-as"))
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
	  :azure t
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
tag.")

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
    (:id "es-CO-GonzaloNeural" :language "es" :gender "male" :role "alternate")
    (:id "es-CO-SalomeNeural" :language "es" :gender "female" :role "alternate")
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
  "curl https://api.openai.com/v1/audio/speech \
  -H \"Authorization: Bearer %s\" \
  -H \"Content-Type: application/json\" \
  -d '{
    \"model\": \"%s\",
    \"input\": \"%s\",
    \"voice\": \"%s\"
  }' \
  --output '%s'"
  "Curl command to send a request to the OpenAI text-to-speech engine.
The placeholders are the API key, the TTS model, the text to be synthesized, the
voice, and the file destination.")

(defconst tlon-openai-voices
  '((:id "echo" :language "es" :gender "male")
    (:id "nova" :language "es" :gender "female"))
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
    (:name "Victoria" :id "lm0dJr2LmYD4zn0kFH9E" :language "multilingual" :gender "female" :role "main"))
  "Preferred ElevenLabs voices for different languages.
A list of available voices may be found here:
<https://elevenlabs.io/app/voice-library>. To get information about the voices,
including the voice ID, run `tlon-tts-elevenlabs-get-voices'.")

(defconst tlon-elevenlabs-char-limit (* 5000 0.9)
  "Maximum number of characters that Elevenlabs can process per request.
Elevenlabs can process up to 5000 characters per request. We use a slightly
lower number to err on the safe side.

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
	   :choices-var ,tlon-microsoft-azure-audio-choices
	   :request-fun tlon-tts-microsoft-azure-make-request
	   :char-limit ,tlon-microsoft-azure-char-limit
	   :property :azure)
    (:name "Google Cloud"
	   :voices-var tlon-google-cloud-voices
	   :audio-var tlon-google-cloud-audio-settings
	   :choices-var ,tlon-google-cloud-audio-choices
	   :request-fun tlon-tts-google-cloud-make-request
	   :char-limit ,tlon-google-cloud-char-limit
	   :property :google)
    (:name "Amazon Polly"
	   :voices-var tlon-amazon-polly-voices
	   :audio-var tlon-amazon-polly-audio-settings
	   :choices-var ,tlon-amazon-polly-audio-choices
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
	   :choices-var ,tlon-elevenlabs-audio-choices
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

;;;;; Links

(defconst tlon-tts-self-referential-link
  '(("en" . ("here" . "here (in the text there is a link here pointing to a web page)"))
    ("es" . ("aquí" . "aquí (en el texto hay un enlace que apunta a una página web)"))
    ("fr" . ("ici" . "ici (dans le texte, il y a un lien qui pointe vers une page web)"))
    ("de" . ("hier" . "hier (im Text gibt es einen Link, der auf eine Webseite zeigt)"))
    ("it" . ("qui" . "qui (nel testo c'è un link che punta a una pagina web)"))
    ("pt" . ("aqui" . "aqui (no texto há um link que aponta para uma página web)")))
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
    ("en" . "to the power of %s"))
  "Pattern for regular exponents.")

(defconst tlon-tts-irregular-exponents
  '((2 . (("es" . "al cuadrado")
	  ("en" . "squared")))
    (3 . (("es" . "al cubo")
	  ("en" . "cubed")))
    (4 . (("es" . "a la cuarta")))
    (5 . (("es" . "a la quinta")))
    (6 . (("es" . "a la sexta")))
    (7 . (("es" . "a la séptima")))
    (8 . (("es" . "a la octava")))
    (9 . (("es" . "a la novena"))))
  "List of exponents and their irregular verbal equivalents.")

(defconst tlon-tts-80000
  '(("en" . "Eighty thousand")
    ("es" . "Ochenta mil")
    ("pt" . "Oitenta mil")
    ("fr" . "Quatre-vingt mille")
    ("de" . "Achtzigtausend")
    ("it" . "Ottantamila"))
  "The number 80000 in different languages.")

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
  (let ((format-string "^\\(?1:## %s\\(?:\\(.\\|\n\\)**\\)\\)"))
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

;; TODO: decide if they should be file-local variables
(defvar tlon-tts-chunks nil
  "Chunks of text to be narrated.
The value of this variable is used for debugging purposes. Hence it is not unset
at the end of the TTS process.")

(defvar tlon-tts-unprocessed-chunk-files nil
  "The chunks to process in the current TTS session.")

;;;;;; Local abbrevs

;; These are the variables that store the file-local variable values for a particular tts session
;; TODO: decide if they should also be file-local

(defvar tlon-local-abbreviations-for-session '()
  "Value of `tlon-local-abbreviations' for the file being processed.")

(defvar tlon-local-replacements-for-session '()
  "Value of `tlon-local-replacements' for the file being processed.")

;;;;; Abbreviations

(defvar tlon-global-abbreviations nil
  "Standard abbreviations and their spoken equivalent in each language.
Note that the replacements are performed in the order they appear in the list.
This may be relevant for certain types of abbreviations and languages. For
example, in Spanish certain abbreviations will be expanded in one form if the
term that precedes them is ‘1’ and in another form otherwise (e.g., 1 km vs. 2
km).")

;;;;; Phonetic replacements

(defvar tlon-tts-global-phonetic-replacements nil
  "Phonetic replacements for terms.")

;;;;; Phonetic transcriptions

(defvar tlon-tts-global-phonetic-transcriptions nil
  "Phonetic transcriptions for terms.")

;;;;; Listener cues

(defconst tlon-tts-cue-delimiter
  ""
  "Delimiter for listener cues.")

(defconst tlon-tts-listener-cues
  '((aside
     ("en" "Aside." . "\nEnd of the aside.")
     ("es" "Inciso." . "\nFin del inciso.")
     ("fr" "Incise." . "\nFin de l'incise.")
     ("de" "Beiseite." . "\nEnde des Beiseites.")
     ("it" "Parentesi." . "\nFine della parentesi."))
    (blockquote
     ("en" "Quote." . "\nEnd of quote.")
     ("es" "Cita." . "\nFin de la cita.")
     ("fr" "Citation." . "\nFin de la citation.")
     ("de" "Zitat." . "\nEnde des Zitats.")
     ("it" "Citazione." . "\nFine della citazione."))
    (heading
     ("en" "Heading." . "")
     ("es" "Sección." ."")
     ("fr" "Titre." . "")
     ("de" "Überschrift." . "")
     ("it" "Titolo." . ""))
    (image
     ("en" "There’s an image here." . "\nEnd of image.")
     ("es" "Aquí hay una imagen." . "\nFin de la imagen.")
     ("fr" "Il y a une image ici." . "\nFin de l'image.")
     ("de" "Hier ist ein Bild." . "\nEnde des Bildes.")
     ("it" "C'è un'immagine qui." . "\nFine dell'immagine."))
    (image-caption
     ("en" . " The image is followed by a caption that reads: ")
     ("es" . " A la imagen le sigue una leyenda que dice: ")
     ("fr" . " L'image est suivie d'une légende qui dit: ")
     ("de" . " Das Bild wird von einer Bildunterschrift gefolgt, die lautet: ")
     ("it" . " L'immagine è seguita da una didascalia che recita: "))
    (note
     ("en" "Note." . "\nEnd of the note.")
     ("es" "Nota." . "\nFin de la nota.")
     ("fr" "Note." . "\nFin de la note.")
     ("de" "Anmerkung." . "\nEnde der Anmerkung.")
     ("it" "Nota." . "\nFine della nota."))
    (owid
     ("en" "Chart." . "\nEnd of chart.")
     ("es" "Cuadro." . "\nFin del cuadro.")
     ("fr" "Tableau." . "\nFin du tableau.")
     ("de" "Diagramm." . "\nEnde des Diagramms.")
     ("it" "Grafico." . "\nFine del grafico."))
    (quote
     ("en" "(quote)" . "(End of quote)")
     ("es" "(cita)" . "(Fin de la cita)")
     ("fr" "(citation)" . "(Fin de la citation)")
     ("de" "(Zitat)" . "(Ende des Zitats)")
     ("it" "(citazione)" . "(Fine della citazione"))
    (subheading
     ("en" "Subheading." . "")
     ("es" "Subsección." . "")
     ("fr" "Sous-titre." . "")
     ("de" "Unterüberschrift." . "")
     ("it" "Sottotitolo." . ""))
    (table
     ("en" "There’s a table here.\n" . "\nEnd of the table.")
     ("es" "Aquí hay una tabla.\n" . "\nFin de la tabla.")
     ("fr" "Il y a un tableau ici.\n" . "\nFin du tableau.")
     ("de" "Hier ist eine Tabelle.\n" . "\nEnde der Tabelle.")
     ("it" "C'è una tabella qui.\n" . "\nFine della tabella.")))
  "Listener cues for different types of content.")

(defconst tlon-tts-listener-cue-patterns
  `((aside ,(tlon-md-get-tag-pattern "Aside") . 2)
    (blockquote ,tlon-md-blockquote . 1)
    (heading ,tlon-md-heading . 5)
    (owid ,(tlon-md-get-tag-pattern "OurWorldInData") . 6)
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
    (tlon-tts-generate-report)
    (tlon-tts-ensure-all-images-have-alt-text)
    (tlon-tts-ensure-all-tables-have-alt-text)
    (tlon-tts-process-notes) ; should be before `tlon-tts-process-citations'?
    (tlon-tts-remove-tag-sections) ; should probably be before `tlon-tts-process-listener-cues'
    (tlon-tts-remove-horizontal-lines) ; should be before `tlon-tts-process-paragraphs'
    (tlon-tex-replace-keys-with-citations nil 'audio)
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
    (tlon-tts-remove-extra-newlines)))

;;;;; Narrate

;;;###autoload
(defun tlon-tts-narrate-staged-content ()
  "Narrate the content staged in the current buffer."
  (interactive)
  (unless (tlon-tts-staging-buffer-p)
    (user-error "Not in a staging buffer"))
  (tlon-tts-set-chunks)
  (tlon-tts-process-chunks))

(defun tlon-tts-staging-buffer-p ()
  "Return t iff the current buffer is a staging buffer."
  (bound-and-true-p tlon-tts-staging-buffer-p))

;;;;;; Set chunks

(defun tlon-tts-set-chunks ()
  "Set chunks contents."
  (let* ((destination (tlon-tts-set-destination))
	 (char-limit (round (tlon-lookup tlon-tts-engines :char-limit :name tlon-tts-engine)))
	 (chunks (tlon-tts-read-into-chunks char-limit)))
    (setq tlon-tts-chunks chunks)
    (setq tlon-tts-unprocessed-chunk-files (tlon-tts-get-chunk-names destination (length tlon-tts-chunks)))))

(defun tlon-tts-set-destination ()
  "Set the destination for the audio file."
  (let* ((staged-file (file-name-base tlon-tts-source))
	 (extension (cdr tlon-tts-audio))
	 (file-name (file-name-with-extension staged-file extension)))
    (file-name-concat default-directory file-name)))

(defun tlon-tts-read-into-chunks (&optional chunk-size)
  "Read the current buffer and return its content as a list of chunks.
If CHUNK-SIZE is non-nil, split string into chunks no larger than that size."
  (let ((local-vars (tlon-tts-pop-file-local-vars))
	(chunks (tlon-tts-break-into-chunks chunk-size)))
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

(defun tlon-tts-break-into-chunks (chunk-size)
  "Break text in current buffer into chunks.
If CHUNK-SIZE is non-nil, break text into chunks no larger thank CHUNK-SIZE.
Each chunk will include the maximum number of paragraphs that fit in that size.
Breaking the text between paragraphs ensures that both the intonation and the
silences are preserved (breaking the text between sentences handles the
intonation, but not the silences, correctly)."
  (goto-char (point-min))
  (let* ((begin 1)
	 (chunk-size (or chunk-size most-positive-fixnum))
	 (voice-chunk tlon-tts-voice-chunks)
	 break chunks end voice voice-break)
    (while (not (eobp))
      (let ((voice-begin (if voice-chunk
			     (marker-position (caar voice-chunk))
			   most-positive-fixnum))
	    (next-voice (when voice-chunk (cdar voice-chunk))))
	(setq break (min (point-max) (+ begin chunk-size) voice-begin))
	(when (eq break voice-begin)
	  (setq voice-break t))
	(goto-char break)
	(unless (or voice-break (eobp))
	  (backward-paragraph))
	(tlon-tts-move-point-before-break-tag)
	(setq end (point))
	(let* ((string (string-trim (buffer-substring-no-properties begin end)))
	       (chunk (cons string (when voice (cons 'tlon-tts-voice voice)))))
	  (push chunk chunks))
	(setq voice next-voice)
	(setq begin end)
	(when voice-break
	  (setq voice-chunk (cdr voice-chunk)))))
    (nreverse chunks)))

(defun tlon-tts-move-point-before-break-tag ()
  "Move point before `break' tag if it immediately follows it.
This is to prevent Elevenlabs from inserting weird audio artifacts."
  (let ((break-tag (tlon-md-get-tag-pattern "break")))
    (while (looking-back (concat break-tag "[ \t\n]*") nil)
      (re-search-backward break-tag nil t)
      (goto-char (match-beginning 0)))))

;;;;;; Process chunks

(defun tlon-tts-process-chunks ()
  "Process chunks.
After processing the chunks, open the relevant Dired buffer."
  (let ((destination (tlon-tts-set-destination))
	(nth 1))
    (dolist (chunk tlon-tts-chunks)
      (let ((string (car chunk))
	    (voice-data (cdr chunk)))
	(tlon-tts-generate-audio string (tlon-tts-get-chunk-name destination nth)
				 (when voice-data
				   `(,voice-data)))
	(setq nth (1+ nth))))
    (let ((dired-listing-switches "-alht")) ; sort by date
      (dired (file-name-directory destination)))))

(defun tlon-tts-generate-audio (string file &optional parameters)
  "Generate audio FILE of STRING.
PARAMETERS is a list of cons cells of parameters to use when generating the
audio, where the car is the name of the file-local variable the cdr is its
overriding value."
  (let* ((fun (tlon-lookup tlon-tts-engines :request-fun :name tlon-tts-engine))
	 (request (funcall fun string file parameters)))
    (when tlon-debug (message "Debug: Running command: %s" request))
    (let ((process (start-process-shell-command "generate audio" nil request)))
      (set-process-sentinel process
			    (lambda (process event)
			      (if (string= event "finished\n")
				  (if (region-active-p)
				      (tlon-tts-open-file file)
				    (tlon-tts-process-chunk file))
				(message "Process %s: Event occurred - %s" (process-name process) event))
			      (kill-buffer "*Shell Command Output*")
			      (shell-command (format "open '%s'" (tlon-tts-get-original-name file))))))))

(defun tlon-tts-get-chunk-name (file nth)
  "Return the name of the NTH chunk of FILE."
  (let ((extension (file-name-extension file))
	(file-name-sans-extension (file-name-sans-extension file)))
    (format "%s-%03d.%s" file-name-sans-extension nth extension)))

(defun tlon-tts-open-file (file)
  "Open generated TTS FILE."
  (shell-command (format "open %s" file)))

(defun tlon-tts-process-chunk (file)
  "Process FILE chunk."
  (setq tlon-tts-unprocessed-chunk-files
	(remove file tlon-tts-unprocessed-chunk-files))
  (unless tlon-tts-unprocessed-chunk-files
    (let ((file (tlon-tts-get-original-name file)))
      (tlon-tts-join-chunks file)
      ;; (tlon-tts-delete-chunks-of-file file)
      )))

(defun tlon-tts-set-chunk-file (file)
  "Set chunk file based on FILE.
If FILE is nil, use the file visited by the current buffer, the file at point in
Dired, or prompt the user for a file (removing the chunk numbers if necessary)."
  (or file
      (buffer-file-name)
      (tlon-tts-get-original-name (dired-get-filename))
      (tlon-tts-get-original-name (read-file-name "File: "))))

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
  "Return a list of the file chunks for FILE."
  (let ((nth 1)
	file-chunk
	files)
    (while (file-exists-p (setq file-chunk (tlon-tts-get-chunk-name file nth)))
      (push file-chunk files)
      (setq nth (1+ nth)))
    (nreverse files)))

(defun tlon-tts-delete-chunks-of-file (file)
  "Delete the chunks of FILE.
Also delete the staging buffer."
  (interactive)
  (let* ((file (tlon-tts-set-chunk-file file))
	 (buffer-name (tlon-tts-get-staging-buffer-name file)))
    (dolist (file (tlon-tts-get-list-of-chunks file))
      (delete-file file 'trash))
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

(defun tlon-tts-get-chunk-names (file n)
  "Return a list of the first N chunk names of FILE."
  (let (names)
    (cl-loop for index from 1 to n
	     do (push (tlon-tts-get-chunk-name file index) names))
    names))

(defun tlon-tts-get-original-name (chunk-name)
  "Return the original file name before it was chunked, given CHUNK-NAME."
  (let* ((base-name (file-name-sans-extension chunk-name))
	 (extension (file-name-extension chunk-name))
	 (original-base-name (replace-regexp-in-string "-[0-9]+\\'" "" base-name)))
    (format "%s.%s" original-base-name extension)))

;;;;;; Set file-local variables

(defun tlon-tts-get-file-local-or-override (var-names parameters)
  "Return the values of VAR-NAMES if found in parameters, else the file-local var.
VAR-NAMES is a list of variable names. PARAMETERS is a cons cell of parameters
to use when generating the audio, where the car is the name of the file-local
variable the cdr is its overriding value."
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
If LANGUAGE is return it. Otherwise, get the value of
`tlon-tts-set-language', look up the language of the current file or
prompt the user to select it, in that order."
  (or language
      (when (boundp 'tlon-tts-language)
	tlon-tts-language)
      (when (boundp 'tlon-tts-source)
	(tlon-repo-lookup :language :dir (tlon-get-repo-from-file tlon-tts-source)))
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
	      (tlon-tts-openai-get-or-set-key) tlon-openai-model string voice destination))))

(defun tlon-tts-openai-get-or-set-key ()
  "Get or set the Microsoft Azure API key."
  (or tlon-openai-key
      (setq tlon-openai-key
	    (auth-source-pass-get "key" (concat "tlon/core/openai.com/" tlon-email-shared)))))

;;;;;;; ElevenLabs

(defun tlon-tts-elevenlabs-make-request (string destination &optional parameters)
  "Make a request to the ElevenLabs text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path.
PARAMETERS is a cons cell of parameters to use when generating the audio, where
the car is the name of the file-local variable the cdr is its overriding value."
  (let ((vars (tlon-tts-get-file-local-or-override
	       '(tlon-tts-voice
		 tlon-tts-audio)
	       parameters)))
    (cl-destructuring-bind (voice audio) vars
      (mapconcat 'shell-quote-argument
		 (list "curl"
		       "--request" "POST"
		       "--url" (format tlon-elevenlabs-tts-url voice (car audio))
		       "--header" "Content-Type: application/json"
		       "--header" (format "xi-api-key: %s" (tlon-tts-elevenlabs-get-or-set-key))
		       "--data" (json-encode `(("text" . ,string)
					       ("model_id" . ,tlon-elevenlabs-model)))
		       "--output" destination)
		 " "))))

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
  "Add title and author."
  (when-let* ((metadata (tlon-yaml-format-values-of-alist (tlon-yaml-get-metadata)))
	      (title (alist-get "title" metadata nil nil #'string=))
	      (title-part (format "%s.\n\n" title)))
    (if-let* ((authors (alist-get "authors" metadata nil nil #'string=))
	      (author-string (tlon-concatenate-list authors))
	      (author-part (format "Por %s.\n\n" author-string)))
	(concat title-part author-part)
      title-part)))

;;;;; Get SSML

(defun tlon-tts-get-ssml-break (time)
  "Return an SSML `break' tag with `time' attribute of TIME."
  (tlon-md-get-tag-filled "break" `(,time)))

;;;;; File uploading

(declare-function tlon-upload-file-to-server "tlon-api")
;;;###autoload
(defun tlon-tts-upload-audio-file-to-server (&optional file temp)
  "Upload audio FILE to the server and delete it locally.
If TEMP is non-nil, upload FILE to temporary server directory."
  (interactive)
  (let* ((file (or file (files-extras-read-file file)))
	 (lang (tlon-tts-get-current-language))
	 (destination (tlon-tts-get-audio-directory lang file nil temp)))
    (tlon-upload-file-to-server file destination 'delete-after-upload)))

;;;###autoload
(defun tlon-tts-upload-audio-file-to-server-temp-dir (&optional file)
  "Upload audio FILE to the server and delete it locally."
  (interactive)
  (let* ((file (or file (files-extras-read-file file))))
    (tlon-tts-upload-audio-file-to-server file 'temp)))

;;;###autoload
(defun tlon-tts-open-audio-directory (&optional lang temp)
  "Open the directory where the audio files for LANG are stored.
If TEMP is non-nil, open temporary server directory."
  (interactive)
  (let ((lang (tlon-tts-get-current-language lang)))
    (dired (tlon-tts-get-audio-directory lang nil 'tramp temp))))

;;;###autoload
(defun tlon-tts-open-temp-audio-directory (&optional lang)
  "Open the temporary directory where the audio files for LANG are stored."
  (interactive)
  (let ((lang (tlon-tts-get-current-language lang)))
    (tlon-tts-open-audio-directory lang 'temp)))

(defun tlon-tts-get-audio-directory (lang &optional file tramp temp)
  "Return the directory where audio files are stored for LANG.
If FILE is non-nil, get the bare directory from it; otherwise, prompt the user
to select it. By default, return the direct remote path. If TRAMP is non-nil,
return the TRAMP SSH path.

If TEMP is non-nil, return temporary server directory."
  (let ((bare-dir (if file
		      (or (tlon-get-bare-dir file)
			  (tlon-select-bare-dir lang))
		    (tlon-select-bare-dir lang)))
	(dirname (if temp (format "uqbar-%s-audio-temp" lang)
		   (format "uqbar-%s-audio" lang)))
	(path (if tramp
		  "/ssh:fede@tlon.team:/home/fede/%s/%s/"
		"fede@tlon.team:/home/fede/%s/%s/")))
    (format path dirname bare-dir)))

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

(defun tlon-tts-process-math ()
  "Replace math expressions with their alt text.
If no alt text is present, replace with the expression itself."
  (goto-char (point-min))
  (while (re-search-forward (tlon-md-get-tag-pattern "Math") nil t)
    (replace-match (or (match-string 4) (match-string 2)) t t)))

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
      (let ((pattern (alist-get lang tlon-tts-tag-section-patterns nil nil #'string=)))
	(when pattern
	  (while (re-search-forward pattern nil t)
	    (replace-match "" t t)))))))

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
  (or (tlon-tts-get-associated-terms tlon-global-abbreviations)
      (user-error "Warning: no global abbreviations found; check `tlon-global-abbreviations'")))

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
    (dolist (term var result)
      (when (member (tlon-tts-get-current-language) (car term))
	(setq result (append result (cadr term)))))
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
  (tlon-tts-get-associated-terms tlon-tts-global-phonetic-replacements))

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
  (tlon-tts-get-associated-terms tlon-tts-global-phonetic-transcriptions))

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
  (tlon-tts-process-owid))

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
  (dolist (tag '("Figure" "OurWorldInData"))
    (goto-char (point-min))
    (while (re-search-forward (tlon-md-get-tag-pattern tag) nil t)
      (unless (match-string 6)
	(user-error "Some images are missing alt text; run `M-x tlon-ai-set-image-alt-text-in-buffer'")))))

(defun tlon-tts-process-owid ()
  "Add listener cues for One World In Data charts."
  (tlon-tts-add-listener-cues 'owid))

;;;;;; Report

;;;###autoload
(defun tlon-tts-generate-report ()
  "Generate a report of TTS issues potentially worth addressing."
  (interactive)
  (let ((staging-buffer (current-buffer))
	(acronyms (tlon-tts-get-missing-acronyms))
	(chemical-symbols-p (tlon-tts-check-chemical-symols))
	(en-dashes-p (tlon-tts-check-en-dashes))
	(numerals-sans-separator-p (tlon-tts-get-numerals-sans-separator))
	report-buffer)
    (with-current-buffer (get-buffer-create tlon-tts-report-buffer-name)
      (setq report-buffer (current-buffer))
      (erase-buffer)
      (when acronyms
	(insert "***Missing acronyms***\n\n")
	(dolist (acronym acronyms)
	  (insert (format "%s\n" acronym))))
      (when chemical-symbols-p
	(insert (format "\n***Chemical symbols***\n\nSearch for ‘%s’"
			tlon-tts-maybe-chemical-symbol)))
      (when en-dashes-p
	(insert (format "\n***En dashes***\n\nSearch for ‘–’")))
      (when numerals-sans-separator-p
	(insert (format "\n***Numerals sans separator***\n\nRun ‘M-x tlon-manual-fix-add-thousands-separators’"))))
    (switch-to-buffer report-buffer)
    (other-window 1)
    (switch-to-buffer staging-buffer)))

;;;;;;; Missing acronyms

(defun tlon-tts-get-missing-acronyms ()
  "Return list of acronyms not found in the local or global list of abbreviations."
  (let ((abbrevs (append tlon-local-abbreviations-for-session
			 (tlon-tts-get-global-abbreviations)))
	(case-fold-search nil)
	missing)
    (goto-char (point-min))
    (while (re-search-forward "\\(?:[A-Z]\\{2,\\}\\(?:\\. ?\\)?\\)+\\.?" nil t)
      (let ((match (match-string-no-properties 0)))
	(unless (or (string-match (mapconcat 'car abbrevs "\\|") match)
		    (tlon-tts-looking-at-excluded-tag-p))
	  (push match missing))))
    (delete-dups missing)))

(defun tlon-tts-looking-at-excluded-tag-p ()
  "Return t iff point is looking at an excluded tag.
An excluded tag is one enclosing text that does not contain acronyms to be
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

(declare-function rst-roman-to-arabic "rst")
(defun tlon-tts-process-numerals-convert-roman ()
  "Replace Roman numerals with their Arabic equivalents."
  (goto-char (point-min))
  (while (re-search-forward (tlon-md-get-tag-pattern "Roman") nil t)
    (let* ((roman (match-string-no-properties 2))
	   (arabic (rst-roman-to-arabic roman)))
      (replace-match (number-to-string arabic) t t))))

;;;;;;;; Replace thousands separators

(defvar tlon-language-specific-thousands-separator)
(defun tlon-tts-process-numerals-set-thousands-separators ()
  "Replace narrow space with language-specific thousands separator.
Some TTS engines do not read numbers correctly when they are not separated by
periods or commas (depending on the language)."
  (goto-char (point-min))
  (let* ((default-separator (tlon-lookup tlon-language-specific-thousands-separator
					 :separator :language (tlon-tts-get-current-language)))
	 (separator (pcase tlon-tts-engine
		      ("ElevenLabs" default-separator)
		      ("Microsoft Azure" "")
		      (_ default-separator) ; fallback; for engines we haven’t yet tested
		      )))
    (while (re-search-forward (tlon-get-number-separator-pattern tlon-default-thousands-separator) nil t)
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
  (let ((language (tlon-tts-get-current-language)))
    (dolist (cons tlon-tts-currencies)
      (let* ((pair (alist-get language (cdr cons) nil nil #'string=))
	     (symbol (regexp-quote (car cons)))
	     (pattern (format "\\(?4:%s\\)%s" symbol
			      (tlon-get-number-separator-pattern tlon-default-thousands-separator))))
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
The car of the cons cell is the search pattern and its cdr is the number group
capturing the replacement text. If the cdr is nil, replace with an empty string.
See the end of the `tlon-tts-supported-tags' docstring for details."
  (let ((replacement (tlon-lookup tlon-tts-supported-tags :replacement :tag tag))
	(cdr 2)
	car)
    (if (listp replacement)
	(setq car (car replacement)
	      cdr (cdr replacement))
      (setq car replacement))
    (cons car cdr)))

;;;;;;; Chunkify

;; This assumes only one function will chunkify, since otherwise successive
;; functions will change positions of their predecessors
(defun tlon-tts-chunkify-unsupported-ssml-tags (tags)
  "Chunkify unsupported SSML TAGS."
  (tlon-tts-reposition-closing-voice-tag)
  (setq tlon-tts-voice-chunks '())
  (dolist (tag tags)
    (let ((cons (tlon-tts-get-cons-for-unsupported-ssml-tags tag))
	  voice replacement)
      (goto-char (point-min))
      (while (re-search-forward (car cons) nil t)
	(setq voice (match-string-no-properties 4)
	      replacement (tlon-tts-get-replacement-for-unsupported-ssml-tags cons))
	(replace-match replacement t t)
	(goto-char (match-beginning 0))
	(tlon-tts-move-point-before-break-tag)
	(push (cons (point-marker) voice) tlon-tts-voice-chunks))))
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

;;;;; Global

;;;;;; Common

;;;;;;; Variable setters

;;;###autoload
(defun tlon-tts-load-global-abbreviations ()
  "Load global abbreviations."
  (interactive)
  (if-let ((abbrevs (tlon-read-json tlon-file-global-abbreviations)))
      (setq tlon-global-abbreviations abbrevs)
    (user-error "Warning: no global abbreviations found; check `tlon-global-abbreviations'")))

(tlon-tts-load-global-abbreviations)

;;;###autoload
(defun tlon-tts-load-global-phonetic-replacements ()
  "Load global phonetic replacements."
  (interactive)
  (if-let ((replacements (tlon-read-json tlon-file-global-phonetic-replacements)))
      (setq tlon-tts-global-phonetic-replacements replacements)
    (user-error "Warning: no global phonetic replacements found; check `tlon-tts-global-phonetic-replacements'")))

(tlon-tts-load-global-phonetic-replacements)

;;;###autoload
(defun tlon-tts-load-global-phonetic-transcriptions ()
  "Load global phonetic transcriptions."
  (interactive)
  (if-let ((transcriptions (tlon-read-json tlon-file-global-phonetic-transcriptions)))
      (setq tlon-tts-global-phonetic-transcriptions transcriptions)
    (user-error "Warning: no global phonetic transcriptions found; check `tlon-tts-global-phonetic-transcriptions'")))

(tlon-tts-load-global-phonetic-transcriptions)

;;;;;;; Entry manipulation

(defun tlon-tts-edit-entry (variable file)
  "Add or revise an entry in VARIABLE and write it to FILE."
  (set variable (tlon-read-json file))
  (let* ((names (mapcan (lambda (group)
			  (mapcar #'car (cadr group)))
			(symbol-value variable)))
	 (term (completing-read "Term: " names nil nil))
	 (current-entry (catch 'current-entry
			  (dolist (group (symbol-value variable))
			    (dolist (pair (cadr group))
			      (when (string= (car pair) term)
				(throw 'current-entry (cdr pair))))))))
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
  (let* ((languages (tlon-select-language 'code 'babel nil nil nil 'multiple))
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
(defun tlon-tts-edit-global-abbreviations ()
  "Edit abbreviations."
  (interactive)
  (tlon-tts-edit-entry 'tlon-global-abbreviations tlon-file-global-abbreviations))

;;;;;; Phonetic replacements

;;;###autoload
(defun tlon-tts-edit-global-phonetic-replacements ()
  "Edit phonetic replacements."
  (interactive)
  (tlon-tts-edit-entry 'tlon-tts-global-phonetic-replacements tlon-file-global-phonetic-replacements))

;;;;;; Phonetic transcriptions

;;;###autoload
(defun tlon-tts-edit-global-phonetic-transcriptions ()
  "Edit phonetic transcriptions."
  (interactive)
  (tlon-tts-edit-entry 'tlon-tts-global-phonetic-transcriptions tlon-file-global-phonetic-transcriptions))

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

(declare-function modify-file-local-variable "files-x")
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

(transient-define-infix tlon-tts-menu-infix-set-engine ()
  "Set the engine."
  :class 'transient-lisp-variable
  :variable 'tlon-tts-global-engine
  :reader 'tlon-tts-set-engine-reader)

(defun tlon-tts-set-engine-reader (_ _ _)
  "Reader for `tlon-tts-menu-infix-set-engine'."
  (completing-read "Engine: " (tlon-lookup-all tlon-tts-engines :name)))

;;;;;;; Engine settings

(eval-and-compile
  (require 'eieio)
  (require 'transient))

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

(defun tlon-tts-global-engine-settings-reader (_ _ _)
  "Reader for `tlon-tts-menu-infix-set-engine-settings'."
  (let* ((choices (tlon-lookup tlon-tts-engines :choices-var :name tlon-tts-global-engine))
	 (selection (completing-read "Engine settings: " choices)))
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
  (let* ((variable (slot-value object 'variable)))
    (when variable
      (setf (slot-value object 'custom-value) value)
      (set variable value)
      value)))

(cl-defmethod transient-format-value ((object tlon-tts-global-engine-settings-infix))
  "Format the value of the infix OBJECT."
  (let ((value (slot-value object 'custom-value)))
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
   ["Files"
    ("j" "Join file chunks"                        tlon-tts-join-chunks)
    ("d" "Delete file chunks"                      tlon-tts-delete-chunks-of-file)
    ("x" "Truncate audio file"                     tlon-tts-truncate-audio-file)
    ""
    "Dirs"
    ("o" "Open dir"                                tlon-tts-open-audio-directory)
    ("O" "Open temp dir"                           tlon-tts-open-temp-audio-directory)
    ("u" "Upload to dir"                           tlon-tts-upload-audio-file-to-server)
    ("U" "Upload to temp dir"                      tlon-tts-upload-audio-file-to-server-temp-dir)]
   ["Edit"
    "global"
    ("a" "Abbreviation"                            tlon-tts-edit-global-abbreviations)
    ("r" "Replacement"                             tlon-tts-edit-global-phonetic-replacements)
    ("t" "Transcription"                           tlon-tts-edit-global-phonetic-transcriptions)
    ""
    "local"
    ("A" "Abbreviation"                            tlon-add-local-abbreviation)
    ("R" "Replacement"                             tlon-add-local-replacement)
    ""
    "Reload"
    "global"
    ("H-a" "Abbreviations"                         tlon-tts-load-global-abbreviations)
    ("H-r" "Replacements"                          tlon-tts-load-global-phonetic-replacements)
    ("H-t" "Transcriptions"                        tlon-tts-load-global-phonetic-transcriptions)]])

(provide 'tlon-tts)

;;; tlon-tts.el ends here
