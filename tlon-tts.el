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

(defcustom tlon-tts-engine "ElevenLabs"
  "The TTS engine."
  :group 'tlon-tts
  :type '(choice (const :tag "Microsoft Azure" :azure)
		 (const :tag "Google Cloud" :google)
		 (const :tag "Amazon Polly" :polly)
		 (const :tag "OpenAI" :openai)
		 (const :tag "ElevenLabs" :elevenlabs)))

(defcustom tlon-tts-use-alternate-voice nil
  "Whether to use an alternative voice for reading notes, asides, etc."
  :group 'tlon-tts
  :type 'boolean)

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

(defcustom tlon-tts-heading-break-duration "1s"
  "Duration of the break after a heading."
  :group 'tlon-tts
  :type 'string)

(defcustom tlon-tts-paragraph-break-duration "0.7s"
  "Duration of the break after a paragraph."
  :group 'tlon-tts
  :type 'string)

(defcustom tlon-tts-listener-cue-break-duration "0.5s"
  "Duration of the break for a listener cue."
  :group 'tlon-tts
  :type 'string)

;;;;; Microsoft Azure

(defcustom tlon-microsoft-azure-audio-settings
  '("audio-16khz-64kbitrate-mono-mp3" . "mp3")
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

For details, see <https://learn.microsoft.com/en-us/azure/ai-services/speech-service/rest-text-to-speech?tabs=streaming#audio-outputs>."
  :group 'tlon-tts
  :type '(cons (string :tag "Name") (string :tag "Extension")))

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

;;;;; Amazon Polly

(defcustom tlon-amazon-polly-audio-settings
  '("mp3" . "mp3")
  "Output format and associated extension for the Amazon Polly TTS service.
Admissible values are `\"ogg_vorbis\"', `\"json\"', `\"mp3\"' and `\""
  :group 'tlon-tts
  :type '(cons (string :tag "Name") (string :tag "Extension")))

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
  '("mp3_44100_192" . "mp3")
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
  stable.\"

<https://help.elevenlabs.io/hc/en-us/articles/17883183930129-What-models-do-you-offer-and-what-is-the-difference-between-them>"
  :group 'tlon-tts
  :type 'string)

;;;; Variables

;;;;; Paths

(defconst tlon-dir-tts
  (file-name-concat (tlon-repo-lookup :dir :name "babel-core") "tts/")
  "Directory for files related to text-to-speech functionality.")

(defconst tlon-file-global-phonetic-transcriptions
  (file-name-concat tlon-dir-tts "phonetic-transcriptions.json")
  "File with phonetic transcriptions.")

(defconst tlon-file-global-phonetic-replacements
  (file-name-concat tlon-dir-tts "phonetic-replacements.json")
  "File with replacements.")

(defconst tlon-file-global-abbreviations
  (file-name-concat tlon-dir-tts "abbreviations.json")
  "File with abbreviations.")

;;;;; Current values

(defvar tlon-tts-current-file-or-buffer nil
  "The file or buffer name with the content for the current TTS process.")

(defvar tlon-tts-current-content nil
  "The content to narrate in the current TTS process.")

(defvar tlon-tts-current-engine nil
  "The engine used in the current TTS process.")

(defvar tlon-tts-current-language nil
  "The language used in the current TTS process.")

(defvar tlon-tts-current-main-voice nil
  "The main voice used in the current TTS process.")

(defvar tlon-tts-current-alternative-voice nil
  "The alternative voice used in the current TTS process.")

(defvar tlon-tts-current-voice-locale nil
  "The locale of the main voice used in the current TTS process.")

;;;;; Chunk processing

(defvar tlon-tts-chunks nil
  "Chunks of text to be narrated.
The value of this variable is used for debugging purposes. Hence it is not unset
at the end of the TTS process.")

(defvar tlon-tts-unprocessed-chunk-files nil
  "The chunks to process in the current TTS session.")

;;;;; SSML tag pairs & patterns

;;;;;; `break'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#break-tag

(defconst tlon-tts-ssml-break
  "<break time=\"%s\" />"
  "Pattern for the `break' SSML tag.
It has one placeholder for the `time' attribute.
<https://learn.microsoft.com/en-us/previous-versions/office/developer/communication-server-2007/bb813930(v=office.12)>.")

;; FIXME: this is not working; `tlon-make-tag-pattern' needs to be adjusted to support self-closing tags
;; (defconst tlon-tts-ssml-break-pattern
;; (tlon-make-tag-pattern tlon-tts-ssml-break)
;; "Pattern to search for `break' tags.")

;;;;;; `emphasis'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#emphasis-tag

(defconst tlon-tts-ssml-emphasis
  '("<emphasis level=\"%s\">" . "</emphasis>")
  "Pattern for the `emphasis' SSML tag.
It has one placeholder for the `level' attribute.")

(defconst tlon-tts-ssml-emphasis-levels
  '("none" "reduced" "moderate" "strong")
  "Admissible values for the `level' attribute of the `emphasis' SSML tag.")

(defconst tlon-tts-ssml-default-emphasis-level
  "moderate"
  "Default value for the `level' attribute of the `emphasis' SSML tag.")

(defconst tlon-tts-ssml-emphasis-pattern
  (tlon-make-tag-pattern tlon-tts-ssml-emphasis)
  "Pattern to search for `emphasis' SSML tags.")

;;;;;; `lang'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#lang-tag

(defconst tlon-tts-ssml-lang
  '("<lang xml:lang=\"%s\">" . "</lang>")
  "Pattern for the SSML `lang' tag.
It has one placeholder for the `lang' attribute, which takes a five-letter
locale code.")

(defconst tlon-tts-ssml-lang-pattern
  (tlon-make-tag-pattern tlon-tts-ssml-lang)
  "Pattern to search for `lang' SSML tags.")

;;;;;; `phoneme'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#phoneme-tag

(defconst tlon-tts-ssml-phoneme
  '("<phoneme alphabet=\"%s\" ph=\"%s\">" . "</phoneme>")
  "Pattern for the `phoneme' SSML tag.
It has two placeholders: for `alphabet' and `ph' attributes, in that order.")

(defconst tlon-tts-ssml-phoneme-alphabets
  '("ipa" "x-sampa")
  "Admissible values for the `alphabet' attribute of the `phoneme' SSML tag.")

(defconst tlon-tts-ssml-phoneme-default-alphabet
  "ipa"
  "Default value for the `alphabet' attribute of the `phoneme' SSML tag.")

(defconst tlon-tts-ssml-phoneme-pattern
  (tlon-make-tag-pattern tlon-tts-ssml-phoneme)
  "Pattern to search for `phoneme' SSML tags.")

(defconst tlon-tts-ssml-phoneme-replace-pattern
  (tlon-make-tag-replace-pattern tlon-tts-ssml-phoneme)
  "Pattern to replace `phoneme' SSML tags.")

;;;;;; `prosody'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#prosody-tag

;; not currently supported

;;;;;; `say-as'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#say-as-tag

(defconst tlon-tts-ssml-say-as
  '("<say-as interpret-as=\"%s\">" . "</say-as>")
  "Pattern for the `say-as' SSML tag.
It has one placeholder for the `interpret-as' attribute.")

(defconst tlon-tts-ssml-interpret-as-values
  '("cardinal" "ordinal" "digits" "fraction" "unit" "date" "time" "telephone" "address" "interjection" "expletive" "spell-out" "characters" "verbatim")
  "Admissible values for the `interpret-as' attribute of the SSML tag.")

(defconst tlon-tts-ssml-say-as-pattern
  (tlon-make-tag-pattern tlon-tts-ssml-say-as)
  "Pattern to search for `say-as' SSML tags.")

;;;;;; `voice'

(defconst tlon-tts-ssml-voice
  '("<voice name=\"%s\">" . "</voice>")
  "SSML pair for voice tag, with voice name placeholder.")

(defconst tlon-tts-ssml-voice-pattern
  (tlon-make-tag-pattern tlon-tts-ssml-voice)
  "Pattern to search for voice tags.")

(defconst tlon-tts-ssml-voice-replace-pattern
  (tlon-make-tag-replace-pattern tlon-tts-ssml-voice)
  "SSML pattern for voice tag, with voice name and text placeholders.")

(defconst tlon-tts-ssml-double-voice-replace-pattern
  (concat (cdr tlon-tts-ssml-voice)
	  (tlon-make-tag-replace-pattern tlon-tts-ssml-voice)
	  (car tlon-tts-ssml-voice))
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
	  ;; :pattern ,tlon-tts-ssml-break-pattern ; not working; see const
	  )
    (:tag emphasis
	  :tlon t
	  :polly nil
	  :azure nil ; https://bit.ly/azure-ssml-emphasis
	  :google t
	  :openai nil
	  :elevenlabs nil ; content is read, but tag is ignored
	  :pattern ,tlon-tts-ssml-emphasis-pattern)
    (:tag lang
	  :tlon t
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :elevenlabs nil ; content is read, but tag is ignored
	  :pattern ,tlon-tts-ssml-lang-pattern)
    (:tag mark
	  :tlon nil
	  :polly t
	  :azure t
	  :google t
	  :openai nil)
    (:tag p
	  :tlon nil
	  :polly t
	  :azure t
	  :google t
	  :openai nil)
    (:tag phoneme
	  :tlon t
	  :polly t
	  :azure nil ; https://bit.ly/azure-ssml-phoneme
	  :google t
	  :openai nil
	  :elevenlabs nil ; only some models, otherwise not read (https://elevenlabs.io/docs/speech-synthesis/prompting#pronunciation)
	  :pattern ,tlon-tts-ssml-phoneme-pattern)
    (:tag prosody
	  :tlon nil
	  :polly t ; partial support
	  :azure t
	  :google t
	  :openai nil)
    (:tag s
	  :tlon nil
	  :polly t
	  :azure t
	  :google t
	  :openai nil)
    (:tag say-as
	  :tlon t
	  :polly t ; partial support
	  :azure t
	  :google t
	  :openai nil
	  :elevenlabs nil ; content is sometimes read, sometimes not read
	  :pattern ,tlon-tts-ssml-say-as-pattern)
    (:tag speak
	  :tlon t
	  :polly t
	  :azure t
	  :google t
	  :openai nil
	  :elevenlabs t ; I assume so?
	  ;; :pattern ; Do we need a pattern for this tag, given it's only used in the wrapper?
	  )
    (:tag sub
	  :tlon nil
	  :polly t
	  :azure t
	  :google t
	  :openai nil)
    (:tag voice
	  :tlon t
	  :polly nil
	  :azure t
	  :google t
	  :openai nil
	  :pattern ,tlon-tts-ssml-voice-pattern)
    (:tag w
	  :tlon nil
	  :polly t
	  :azure t
	  :google t
	  :openai nil))
  "SSML tags supported by this package and by various TTS engines.

- Amazon Polly:
  <https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html>.

- Microsoft Azure:

- Google Cloud: <https://cloud.google.com/text-to-speech/docs/ssml>.

- OpenAI:
  <https://community.openai.com/t/what-about-to-implement-ssml-on-the-new-tts-api-service/485686/5>.

- ElevenLabs: <https://elevenlabs.io/docs/speech-synthesis/prompting>. Only two
  tags are explicitly mentioned, so maybe none of the others are supported?")

;;;;; Engine settings

;;;;;; Microsoft Azure

(defconst tlon-microsoft-azure-request
  "curl -v --location --request POST 'https://eastus.tts.speech.microsoft.com/cognitiveservices/v1' \
--header 'Ocp-Apim-Subscription-Key: %s' \
--header 'Content-Type: application/ssml+xml' \
--header 'X-Microsoft-OutputFormat: %s' \
--header 'User-Agent: curl' \
--data-raw '%s' \
> '%4$s' 2> '%4$s.log'"
  "Curl command to send a request to the Microsoft Azure text-to-speech engine.
The placeholders are: API key, output format, SSML, destination for the audio
file, and destination for the log file.")

(defconst tlon-microsoft-azure-voices
  '((:voice "es-US-AlonsoNeural" :language "es" :gender "male")
    (:voice "es-US-PalomaNeural" :language "es" :gender "female"))
  "Preferred Microsoft Azure voices for different languages.
All the voices in this property list are neural and multilingual, and are the
best male and female voices we were able to identify in each language.

A list of available voices may be found here:
<https://github.com/MicrosoftDocs/azure-docs/blob/main/articles/ai-services/speech-service/includes/language-support/tts.md>.")

(defconst tlon-microsoft-azure-char-limit (* 9 60 14)
  "Maximum number of characters that Microsoft Azure can process per request.
Microsoft Azure can process up to 10 minutes of audio at a time. This estimate
assumes 14 characters per second, and uses nine minutes for safety.")

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

(defconst tlon-google-cloud-char-limit (* 5000 0.9)
  "Maximum number of characters that Google Cloud can process per request.
Google Cloud TTS can process up to 5000 bytes per request. We use a slightly
lower number for safety.

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
  '((:voice "Joanna" :language "en" :gender "female")
    (:voice "Matthew" :language "en" :gender "male")
    (:voice "Lupe" :language "es" :gender "female")
    (:voice "Pedro" :language "es" :gender "male"))
  "Preferred Amazon Polly voices for different languages.
Joanna and Matthew are some of the available Polly voices for English.")

(defconst tlon-amazon-polly-char-limit (* 1500 0.9)
  "Maximum number of characters that Amazon Polly can process per request.
The limit for Amazon Polly is 1500 characters but using a slightly lower number
for safety.")

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
  '((:voice "echo" :language "es" :gender "male")
    (:voice "nova" :language "es" :gender "female"))
  "Preferred OpenAI voices for different languages.
All the voices in this property list are neural and multilingual, and are the
best male and female voices we were able to identify in each language.

A list of available voices may be found here:
<https://platform.openai.com/docs/guides/text-to-speech>.")

(defconst tlon-openai-char-limit (* 4096 0.9)
  "Maximum number of characters that OpenAI can process per request.
OpenAI can process up to 4096 bytes per request. We use a slightly
lower number for safety.

See <https://help.openai.com/en/articles/8555505-tts-api#h_273e638099>.")

(defvar tlon-openai-key nil
  "API key for OpenAI TTS service.")

;;;;;; ElevenLabs

(defconst tlon-elevenlabs-voices
  '((:voice "Nigel" :id "jjz72sHaqmTCiPFqnw2N" :language "multilingual" :gender "male")
    (:voice "Victoria" :id "lm0dJr2LmYD4zn0kFH9E" :language "multilingual" :gender "female")
    (:voice "Knightley" :id "L0Nf9VH02lduEuhdmbJ5" :language "multilingual" :gender "male")
    (:voice "Brian" :id "rncjssM0aAEg1ApKehUP" :language "multilingual" :gender "male")
    (:voice "Valentino" :id "2BCou3n0mac8CZsumOUv" :language "multilingual" :gender "male")
    (:voice "Bruce" :id "qUqZ27WoGID6BUp35xTV" :language "multilingual" :gender "male")
    (:voice "Neal" :id "6JpiWMuXFTicEyWjwDLn" :language "multilingual" :gender "male")
    (:voice "Michael" :id "8mLUlN9GCPCERe4bI7Wx" :language "multilingual" :gender "male")
    (:voice "Readwell" :id "5ZrfTID2FK4q4M1dxIpc" :language "multilingual" :gender "male")
    (:voice "Hades" :id "y3uxYtdWYpmzg8Wwx2k3" :language "multilingual" :gender "male"))
  "Preferred ElevenLabs voices for different languages.
A list of available voices may be found here:
<https://elevenlabs.io/app/voice-library>. To get information about the voices,
including the voice ID, run `tlon-tts-elevenlabs-get-voices'.")

(defconst tlon-elevenlabs-char-limit (* 5000 0.9)
  "Maximum number of characters that OpenAI can process per request.
OpenAI can process up to 4096 bytes per request. We use a slightly
lower number for safety.

See <https://elevenlabs.io/app/subscription> (scroll down to \"Frequently asked
questions\").")

(defconst tlon-elevenlabs-tts-url
  "https://api.elevenlabs.io/v1/text-to-speech/%s/stream?output_format=%s"
  "Base URL for the ElevenLabs TTS API.")

(defvar tlon-elevenlabs-key nil
  "API key for the ElevelLabs TTS service.")

;;;;; Engines

(defconst tlon-tts-engines
  `((:name "Microsoft Azure"
	   :voices-var tlon-microsoft-azure-voices
	   :output-var tlon-microsoft-azure-audio-settings
	   :request-fun tlon-tts-microsoft-azure-make-request
	   :char-limit ,tlon-microsoft-azure-char-limit
	   :property :azure)
    (:name "Google Cloud"
	   :voices-var tlon-google-cloud-voices
	   :output-var tlon-google-cloud-audio-settings
	   :request-fun tlon-tts-google-cloud-make-request
	   :char-limit ,tlon-google-cloud-char-limit
	   :property :google)
    (:name "Amazon Polly"
	   :voices-var tlon-amazon-polly-voices
	   :output-var tlon-amazon-polly-audio-settings
	   :request-fun tlon-tts-amazon-polly-make-request
	   :char-limit ,tlon-amazon-polly-char-limit
	   :property :polly)
    (:name "OpenAI"
	   :voices-var tlon-openai-voices
	   :output-var tlon-openai-audio-settings
	   :request-fun tlon-tts-openai-make-request
	   :char-limit ,tlon-openai-char-limit
	   :property :openai)
    (:name "ElevenLabs"
	   :voices-var tlon-elevenlabs-voices
	   :output-var tlon-elevenlabs-audio-settings
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
			  tlon-tts-ssml-voice-replace-pattern)))
	  '("Microsoft Azure" "Google Cloud"))
  "SSML wrapper for the TTS request.")

;;;;; Numbers

(defconst tlon-tts-number-exponents
  '((1 . (("es" . "a la uno")
	  ("en" . "to the power of %s")))
    (2 . (("es" . "al cuadrado")
	  ("en" . "squared")))
    (3 . (("es" . "al cubo")
	  ("en" . "cubed")))
    (4 . (("es" . "a la cuarta")
	  ("en" . "to the power of %s")))
    (5 . (("es" . "a la quinta")
	  ("en" . "to the power of %s")))
    (6 . (("es" . "a la sexta")
	  ("en" . "to the power of %s")))
    (7 . (("es" . "a la séptima")
	  ("en" . "to the power of %s")))
    (8 . (("es" . "a la octava")
	  ("en" . "to the power of %s")))
    (9 . (("es" . "a la novena")
	  ("en" . "to the power of %s")))
    (10 . (("es" . "a la %s")
	   ("en" . "to the power of %s"))))
  "List of number exponents.")

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

;;;;; File-local variables

(defvar-local tlon-local-abbreviations '()
  "In-text abbreviations and their spoken equivalent.")

(defvar-local tlon-local-replacements '()
  "Local replacements.")

;;;;; Abbreviations

(defvar tlon-tts-abbreviations
  (tlon-parse-json tlon-file-global-abbreviations)
  "Standard abbreviations and their spoken equivalent in each language.")

;;;;; Phonetic replacements

(defvar tlon-tts-phonetic-replacements
  (tlon-parse-json tlon-file-global-phonetic-replacements)
  "Phonetic replacements for terms.")

;;;;; Phonetic transcriptions

(defvar tlon-tts-phonetic-transcriptions
  (tlon-parse-json tlon-file-global-phonetic-transcriptions)
  "Phonetic transcriptions for terms.")

;;;;; Listener cues

(defconst tlon-tts-cue-delimiter
  (concat "\n" (format tlon-tts-ssml-break "0.5s"))
  "Delimiter for listener cues.")

;;;;;; Notes

(defconst tlon-tts-note-cues
  '(("en" "A note starts here." . "End of the note.")
    ("es" "Aquí empieza una nota." . "Fin de la nota."))
  "Listener cues for notes.")

;;;;;; Quotes

(defconst tlon-tts-quote-cues
  '(("en" "A quote starts here." . "End of the quote.")
    ("es" "Aquí empieza una cita." . "Fin de la cita."))
  "Listener cues for quotes.")

;;;;;; Asides

(defconst tlon-tts-aside-cues
  '(("en" "An aside starts here." . "End of the aside.")
    ("es" "Aquí empieza un inciso." . "Fin del inciso."))
  "Listener cues for asides.")

;;;;;; Images

(defconst tlon-tts-image-cues
  '(("en" "Here’s an image." . "")
    ("es" "Aquí hay una imagen." .""))
  "Listener cues for images.")

;;;; Functions

;;;;; Narration

;;;###autoload
(defun tlon-tts-narrate-content (&optional content engine language voice file)
  "Narrate CONTENT in LANGUAGE with VOICE using text-to-speech ENGINE.
If region is active, read the region. Otherwise, read FILE.

Save the audio file in the downloads folder."
  (interactive)
  (tlon-tts-set-all-current-values file content engine language voice)
  (tlon-tts-process-chunks)
  (tlon-tts-unset-all-current-values))

(defun tlon-tts-generate-tts-buffer ()
  "Generate the TTS buffer without narrating its contents.
This command is used for debugging purposes."
  (interactive)
  (tlon-tts-set-all-current-values nil nil nil nil nil)
  (tlon-tts-read-content)
  (tlon-tts-unset-all-current-values))

(defun tlon-tts-process-chunks ()
  "Process unprocessed chunks."
  (let* ((destination (tlon-tts-set-audio-path))
	 (char-limit (round (tlon-lookup tlon-tts-engines :char-limit :name tlon-tts-engine)))
	 (nth 1))
    (setq tlon-tts-chunks (tlon-tts-read-content char-limit))
    (setq tlon-tts-unprocessed-chunk-files (tlon-tts-get-chunk-names destination (length tlon-tts-chunks)))
    (dolist (chunk tlon-tts-chunks)
      ;; TODO: see if this can be done in parallel
      (tlon-tts-generate-audio chunk (tlon-tts-get-chunk-name destination nth))
      (setq nth (1+ nth)))))

(defun tlon-tts-set-audio-path ()
  "Set the audio file path."
  ;; TODO: decide if files should always be saved in the downloads directory
  (let* ((file-name-sans-extension (file-name-sans-extension
				    (file-name-nondirectory tlon-tts-current-file-or-buffer)))
	 (extension (cdr (tlon-tts-get-output-format)))
	 (file-name (file-name-with-extension file-name-sans-extension extension)))
    (file-name-concat paths-dir-downloads file-name)))

(defun tlon-tts-get-output-format ()
  "Return the output format for the current TTS engine.
The output format is a cons cell with the format name and extension."
  (symbol-value (tlon-lookup tlon-tts-engines :output-var :name tlon-tts-engine)))

(defun tlon-tts-read-content (&optional chunk-size)
  "Read content and return it as a string ready for TTS processing.
If CHUNK-SIZE is non-nil, split string into chunks no larger than that size."
  (with-current-buffer (get-buffer-create (tlon-tts-get-temp-buffer-name))
    (erase-buffer)
    (insert (format "%s\n\n%s" (or tlon-tts-prompt "") tlon-tts-current-content))
    (tlon-tts-prepare-buffer)
    (let ((content (if chunk-size
		       (tlon-tts-break-into-chunks chunk-size)
		     (buffer-string))))
      ;; TODO: display buffer only if process failed
      (display-buffer (current-buffer))
      content)))

(defun tlon-tts-get-temp-buffer-name ()
  "Return the name of the temporary buffer for the current TTS process."
  (format "*TTS process: %s*" (file-name-nondirectory tlon-tts-current-file-or-buffer)))

(defun tlon-tts-generate-audio (string file)
  "Generate audio FILE of STRING."
  (let* ((fun (tlon-lookup tlon-tts-engines :request-fun :name tlon-tts-engine))
	 (request (funcall fun string file)))
    (when tlon-debug (message "Debug: Running command: %s" request))
    (let ((process (start-process-shell-command "generate audio" nil request)))
      (set-process-sentinel process
			    (lambda (process event)
			      (if (string= event "finished\n")
				  (if (region-active-p)
				      (tlon-tts-open-file file)
				    (tlon-tts-process-chunk file))
				(message "Process %s: Event occurred - %s" (process-name process) event)))))))

(defun tlon-tts-open-file (file)
  "Open generated TTS FILE."
  (shell-command (format "open %s" file)))

;;;;;; Set current values

;;;;;;; File or buffer

(defun tlon-tts-set-current-file-or-buffer (&optional file)
  "Set the file or buffer name with the content for the current TTS process.
If FILE is nil, use the name of the file visited by the current buffer, or the
buffer name if the buffer is not visiting a file."
  (setq tlon-tts-current-file-or-buffer
	(or file (buffer-file-name) (buffer-name))))

(defun tlon-tts-set-current-content (&optional content)
  "Set the current value of the content to be read.
If CONTENT is nil, read the region if selected or the current file or buffer
otherwise."
  (setq tlon-tts-current-content
	(or content (if (region-active-p)
			(buffer-substring-no-properties (region-beginning) (region-end))
		      (tlon-tts-read-file tlon-tts-current-file-or-buffer)))))

(defun tlon-tts-read-file (file)
  "Return the substantive content of FILE, handling in-text abbreviations."
  (unless (string= (file-name-extension file) "md")
    (user-error "File `%s' is not a Markdown file" file))
  (let ((tlon-local-abbreviations))
    (with-current-buffer (find-file-noselect file)
      ;; to make `tlon-tts-process-local-abbreviations' work, we
      ;; let-bound the variable above and now set its value to that of its
      ;; local counterpart
      (setq tlon-local-abbreviations tlon-local-abbreviations)
      (concat (tlon-tts-get-metadata) (tlon-md-read-content file)))))

;;;;;;; Language

;; TODO: decide if this getter function should also be used for the othe values (engine, voice, etc)
(defun tlon-tts-get-current-language (&optional language)
  "Return the value of the language of the current process.
If LANGUAGE is return it. Otherwise, get the value of
`tlon-tts-set-current-language', look up the language of the current file or
prompt the user to select it, in that order."
  (let ((language (or language tlon-tts-current-language)))
    (or language
	(tlon-repo-lookup :language :dir (tlon-get-repo-from-file tlon-tts-current-file-or-buffer))
	(tlon-select-language 'code 'babel))))

(defun tlon-tts-set-current-language (&optional language)
  "Set the value of the language of the current process.
If LANGUAGE is nil, look up the language of the current file or prompt the user
to select it."
  (setq tlon-tts-current-language (tlon-tts-get-current-language language)))

;;;;;;; Voice

(defun tlon-tts-set-current-main-voice (&optional voice)
  "Set the main voice for the current process.
If VOICE is nil, prompt the user to select a voice."
  (setq tlon-tts-current-main-voice (or voice (tlon-tts-get-voices)))
  (tlon-tts-set-voice-locale))

(defun tlon-tts-set-voice-locale ()
  "Set the locale of the current voice."
  (setq tlon-tts-current-voice-locale
	(catch 'found
	  (dolist (var (tlon-lookup-all tlon-tts-engines :voices-var))
	    (when-let* ((code (tlon-lookup (symbol-value var) :language :voice tlon-tts-current-main-voice))
			(locale (tlon-lookup tlon-languages-properties :locale :code code)))
	      (throw 'found locale))))))

(defun tlon-tts-get-voices ()
  "Get available voices in the current language for the current TTS engine."
  (let* ((voices (symbol-value (tlon-lookup tlon-tts-engines :voices-var :name tlon-tts-engine)))
	 (voice (completing-read "Voice: "
				 (apply #'append
					(mapcar (lambda (language)
						  "Return lists of monolingual and multilingual voices."
						  (tlon-lookup-all voices :voice :language language))
						`(,(tlon-tts-get-current-language) "multilingual"))))))
    ;; we use voice ID if available, for those engines that require it
    (if-let ((id (tlon-lookup voices :id :voice voice))) id voice)))

;;;;;;; Set/unset all values

(defun tlon-tts-set-all-current-values (file content engine language voice)
  "Set all current values.
FILE, CONTENT, ENGINE, LANGUAGE, and VOICE are the values to set."
  (tlon-tts-set-current-file-or-buffer file)
  (tlon-tts-set-current-content content)
  (tlon-tts-set-current-engine engine)
  (tlon-tts-set-current-language language)
  (tlon-tts-set-current-main-voice voice))

(defun tlon-tts-unset-all-current-values ()
  "Unset all current values."
  (setq tlon-tts-current-file-or-buffer nil
	tlon-tts-current-content nil
	tlon-tts-current-engine nil
	tlon-tts-current-language nil
	tlon-tts-current-main-voice nil
	tlon-tts-current-voice-locale nil))

;;;;;; Chunk processing

(defun tlon-tts-process-chunk (file)
  "Process FILE chunk."
  (setq tlon-tts-unprocessed-chunk-files
	(remove file tlon-tts-unprocessed-chunk-files))
  (unless tlon-tts-unprocessed-chunk-files
    (let ((file (tlon-tts-get-original-name file)))
      (tlon-tts-join-chunks file)
      ;; make conditional to no errors
      ;; (tlon-tts-delete-chunks file)
      )))

(defun tlon-tts-break-into-chunks (chunk-size)
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
			   list-of-files file))))

(defun tlon-tts-get-list-of-chunks (file)
  "Return a list of the file chunks for FILE."
  (let ((nth 1)
	file-chunk
	files)
    (while (file-exists-p (setq file-chunk (tlon-tts-get-chunk-name file nth)))
      (push file-chunk files)
      (setq nth (1+ nth)))
    (nreverse files)))

(defun tlon-tts-delete-chunks (&optional file)
  "Delete the chunks of FILE."
  (interactive)
  (let ((file (tlon-tts-set-chunk-file file)))
    (dolist (file (tlon-tts-get-list-of-chunks file))
      (delete-file file))))

(defun tlon-tts-create-list-of-chunks (files)
  "Create a temporary file with a list of audio FILES for use with `ffmpeg'."
  (let ((temp-file-list (make-temp-file "files" nil ".txt")))
    (with-temp-file temp-file-list
      (dolist (file files)
	(insert (format "file '%s'\n" (expand-file-name file)))))
    temp-file-list))

(defun tlon-tts-get-chunk-name (file nth)
  "Return the name of the NTH chunk of FILE."
  (let ((extension (file-name-extension file))
	(file-name-sans-extension (file-name-sans-extension file)))
    (format "%s-%03d.%s" file-name-sans-extension nth extension)))

(defun tlon-tts-get-chunk-names (file n)
  "Return a list of the first N chunk names of FILE."
  (let ((names '()))
    (dotimes (i n names)
      (push (tlon-tts-get-chunk-name file (1+ i)) names))))

(defun tlon-tts-get-original-name (chunk-name)
  "Return the original file name before it was chunked, given CHUNK-NAME."
  (let* ((base-name (file-name-sans-extension chunk-name))
	 (extension (file-name-extension chunk-name))
	 (original-base-name (replace-regexp-in-string "-[0-9]+\\'" "" base-name)))
    (format "%s.%s" original-base-name extension)))

;;;;;; TTS engines

;;;;;;; Microsoft Azure

(defun tlon-tts-microsoft-azure-make-request (string destination)
  "Make a request to the Microsoft Azure text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path."
  (let* ((ssml-wrapper (alist-get "Microsoft Azure" tlon-ssml-wrapper nil nil #'string=))
	 (wrapped-string (format ssml-wrapper
				 tlon-tts-current-voice-locale
				 tlon-tts-current-main-voice
				 string)))
    (format tlon-microsoft-azure-request
	    (tlon-tts-microsoft-azure-get-or-set-key) (car tlon-microsoft-azure-audio-settings)
	    wrapped-string destination)))

(defun tlon-tts-microsoft-azure-get-or-set-key ()
  "Get or set the Microsoft Azure key."
  (or tlon-microsoft-azure-key
      (setq tlon-microsoft-azure-key
	    (auth-source-pass-get "tts1" (concat "tlon/core/azure.com/" tlon-email-shared)))))

;;;;;;; Google Cloud

(defun tlon-tts-google-cloud-make-request (string destination)
  "Make a request to the Google Cloud text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path."
  (let* ((ssml-wrapper (alist-get "Google Cloud" tlon-ssml-wrapper nil nil #'string=))
	 (wrapped-string (format ssml-wrapper
				 tlon-tts-current-voice-locale
				 tlon-tts-current-main-voice
				 string)))
    (format tlon-google-cloud-request
	    (tlon-tts-google-cloud-get-token)
	    ;; note: `wrapped-string' already includes voice and locale info,
	    ;; but `tlon-tts-google-cloud-format-ssml' adds this info as part of the
	    ;; JSON payload. Is this correct?
	    (tlon-tts-google-cloud-format-ssml wrapped-string)
	    destination)))

;; If this keeps failing, consider using the approach in `tlon-tts-elevenlabs-make-request'
(defun tlon-tts-google-cloud-format-ssml (ssml)
  "Convert SSML string to JSON object for Google Cloud TTS."
  (let* ((gender (tlon-lookup tlon-google-cloud-voices :gender :voice tlon-tts-current-main-voice))
	 (payload (json-encode
		   `((input (ssml . ,ssml))
		     (voice (languageCode . ,tlon-tts-current-voice-locale)
			    (name . ,tlon-tts-current-main-voice)
			    (ssmlGender . ,(upcase gender)))
		     (audioConfig (audioEncoding . ,(car tlon-google-cloud-audio-settings)))))))
    payload))

(defun tlon-tts-google-cloud-get-token ()
  "Get or set the Google Cloud token key."
  (string-trim (shell-command-to-string "gcloud auth print-access-token")))

;;;;;;; Amazon Polly

(defun tlon-tts-amazon-polly-make-request (string destination)
  "Construct the AWS CLI command to call Amazon Polly.
STRING is the string of the request. DESTINATION is the output file path."
  (format tlon-amazon-polly-request
	  (car tlon-amazon-polly-audio-settings) tlon-tts-current-main-voice
	  string tlon-amazon-polly-region destination))

;;;;;;; OpenAI

(defun tlon-tts-openai-make-request (string destination)
  "Make a request to the OpenAI text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path."
  (format tlon-openai-tts-request
	  (tlon-tts-openai-get-or-set-key) tlon-openai-model string tlon-tts-current-main-voice destination))

(defun tlon-tts-openai-get-or-set-key ()
  "Get or set the Microsoft Azure API key."
  (or tlon-openai-key
      (setq tlon-openai-key
	    (auth-source-pass-get "key" (concat "tlon/core/openai.com/" tlon-email-shared)))))

;;;;;;; ElevenLabs

(defun tlon-tts-elevenlabs-make-request (string destination)
  "Make a request to the ElevenLabs text-to-speech service.
STRING is the string of the request. DESTINATION is the output file path."
  (let ((url (format tlon-elevenlabs-tts-url tlon-tts-current-main-voice
		     (car tlon-elevenlabs-audio-settings)))
	(api-key (tlon-tts-elevenlabs-get-or-set-key))
	(data (json-encode `(("text" . ,string)
			     ("model_id" . ,tlon-elevenlabs-model)))))
    (with-temp-buffer
      (call-process "curl" nil t nil
		    "--request" "POST"
		    "--url" url
		    "--header" (format "Content-Type: application/json")
		    "--header" (format "xi-api-key: %s" api-key)
		    "--data" data
		    "--output" destination)
      (buffer-string))))

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

;; Should also include summary?
;; Perhaps should be narrated by alternative voice?
(defun tlon-tts-get-metadata ()
  "Add title and author."
  (let* ((metadata (tlon-yaml-format-values-of-alist (tlon-yaml-get-metadata)))
	 (title (alist-get "title" metadata nil nil #'string=))
	 (authors (alist-get "authors" metadata nil nil #'string=))
	 (author-string (tlon-concatenate-list authors)))
    (format "%s.\n\nPor %s.\n\n" title author-string)))


;;;;; Get SSML

(defun tlon-tts-get-ssml-break (time)
  "Return an SSML `break' tag with `time' attribute of TIME."
  (format tlon-tts-ssml-break time))

(defun tlon-tts-get-ssml-emphasis (level)
  "Return an SSML `emphasis' tag with `level' attribute of LEVEL."
  (tlon-tag-element-with-attributes tlon-tts-ssml-emphasis level))

(defun tlon-tts-get-ssml-lang (language)
  "Return an SSML `lang' tag with `language' attribute of LANGUAGE."
  (tlon-tag-element-with-attributes tlon-tts-ssml-lang language))

(defun tlon-tts-get-ssml-phoneme (alphabet ph)
  "Return an SSML `phoneme' tag  with `alphabet' and `ph' set to ALPHABET and PH."
  (tlon-tag-element-with-attributes tlon-tts-ssml-phoneme alphabet ph))

;;;;; Insert SSML

;;;###autoload
(defun tlon-tts-insert-ssml-break (time)
  "Insert a `break' SSML tag pair at point or around the selected region.
If TIME is nil, prompt the user to enter a value for the `time' attribute."
  (interactive (list (concat (read-string "s`time' (seconds): ") "s")))
  (insert (tlon-tts-get-ssml-break time)))

;;;###autoload
(defun tlon-tts-insert-ssml-emphasis (level)
  "Insert a `emphasis' SSML tag pair at point or around the selected region.
If LEVEL is nil, prompt the user to select a value for the `level' attribute."
  (interactive (list (completing-read "`emphasis': "
				      tlon-tts-ssml-emphasis-levels nil t nil nil
				      tlon-tts-ssml-default-emphasis-level)))
  (tlon-md-insert-element-pair (tlon-tts-get-ssml-emphasis level)))

;;;###autoload
(defun tlon-tts-insert-ssml-lang (language)
  "Insert a `lang' SSML tag pair at point or around the selected region.
If LANGUAGE is nil. prompt the user to select a value for the `language'
attribute."
  (interactive (list (tlon-select-language 'locale)))
  (tlon-md-insert-element-pair
   (tlon-tag-element-with-attributes tlon-tts-ssml-lang language)))

;;;###autoload
(defun tlon-tts-insert-ssml-phoneme (alphabet ph)
  "Insert a `phoneme' SSML tag pair at point or around the selected region.
If ALPHABET or PH are nil, prompt the user to select a value for the `alphabet'
and `ph' attributes."
  (interactive (list (completing-read "`alphabet': "
				      tlon-tts-ssml-phoneme-alphabets nil t nil nil
				      tlon-tts-ssml-phoneme-default-alphabet)
		     (read-string "PH: "))) ; integrate with AI
  (tlon-md-insert-element-pair (tlon-tts-get-ssml-phoneme alphabet ph)))

;;;###autoload
(defun tlon-tts-insert-ssml-say-as (interpret-as)
  "Insert a `say-as' SSML tag pair at point or around the selected region.
If INTERPRET-AS is nil, prompt the user to select a value for the `interpret-as'
attribute."
  (interactive (list (completing-read "`interpret-as': "
				      tlon-tts-ssml-interpret-as-values)))
  (tlon-md-insert-element-pair (tlon-tag-element-with-attributes tlon-tts-ssml-say-as interpret-as)))

;;;;; Cleanup

(declare-function tlon-tex-replace-keys-with-citations "tlon-tex")
(defun tlon-tts-prepare-buffer ()
  "Prepare the current buffer for audio narration."
  (save-excursion
    (tlon-tts-process-notes) ; should be before `tlon-tts-process-citations'?
    (tlon-tex-replace-keys-with-citations nil 'mdx 'audio)
    (tlon-tts-process-formatting)
    (tlon-tts-process-headings)
    (tlon-tts-process-paragraphs)
    (tlon-tts-process-local-abbreviations)
    (tlon-tts-process-global-abbreviations)
    (tlon-tts-process-local-phonetic-replacements)
    (tlon-tts-process-globa-phonetic-replacements)
    (tlon-tts-process-alternative-voice)
    (tlon-tts-process-listener-cues) ; should be before `tlon-tts-process-links'
    (tlon-tts-process-links)
    (tlon-tts-process-currencies) ; should be before `tlon-tts-process-numerals'
    (tlon-tts-process-numerals)
    (tlon-tts-remove-unsupported-ssml-tags))
  (goto-char (point-min)))

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

;;;;;;;; Formatting

;;;;;;; General

;; TODO: should have more descriptive name
(defun tlon-tts-process-formatting ()
  "Remove formatting from text."
  (tlon-tts-process-boldface)
  (tlon-tts-process-italics)
  (tlon-tts-process-visually-hidden)
  (tlon-tts-process-visually-shown)
  (tlon-tts-process-small-caps))

(defun tlon-tts-remove-formatting (type)
  "Remove formatting TYPE from text."
  (cl-destructuring-bind (pattern . groups)
      (pcase type
	('boldface (cons markdown-regex-bold '(1 4)))
	('italics (cons markdown-regex-italic '(1 4)))
	('visually-hidden (cons tlon-mdx-visually-hidden-search-pattern '(2)))
	('visually-shown (cons tlon-mdx-visually-shown-search-pattern nil))
	('alternative-voice (cons tlon-mdx-alternative-voice-search-pattern '(2)))
	('small-caps (cons tlon-mdx-small-caps-search-pattern '(2)))
	;; add 'math type; should use the value of `alt' attribute
	(_ (user-error "Invalid formatting type: %s" type)))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (let ((replacement (if groups
			     (mapconcat (lambda (group)
					  (match-string group))
					groups)
			   "")))
	(replace-match replacement t nil)))))

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

;; TODO: replace with `ReplaceAudio'
(defun tlon-tts-process-visually-shown ()
  "Remove `VisuallyShown' MDX tag."
  (tlon-tts-remove-formatting 'visually-shown))

(defun tlon-tts-process-alternative-voice ()
  "Remove `AlternativeVoice' tag, or replace it with an SSML `voice' tag.
Whether the tag is removed or replaced depends on the value of
`tlon-tts-use-alternate-voice'. When the tag is replaced, the `voice' tag is set
to the alternative voice for the current language."
  (if tlon-tts-use-alternate-voice
      (progn
	(goto-char (point-min))
	(while (re-search-forward tlon-mdx-alternative-voice-search-pattern nil t)
	  (replace-match (tlon-tts-enclose-in-voice-tag (match-string 2)) t)))
    (tlon-tts-remove-formatting 'alternative-voice)))

(defun tlon-tts-process-small-caps ()
  "Replace small caps with their full form."
  (tlon-tts-remove-formatting 'small-caps))

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
    (unless (looking-back "^\\s-*$" (line-beginning-position))
      (insert (concat " " (tlon-tts-get-ssml-break tlon-tts-paragraph-break-duration))))
    (unless (eobp)
      (forward-char))))

;;;;;; Headings

(defun tlon-tts-process-headings ()
  "Remove heading markers from headings and add an optional pause.
The time length of the pause is determined by `tlon-tts-heading-break-duration'."
  (let ((initial-pause (tlon-tts-get-ssml-break tlon-tts-heading-break-duration)))
    (goto-char (point-min))
    (while (re-search-forward markdown-regex-header nil t)
      (let ((heading (match-string-no-properties 5)))
	(save-match-data
	  (unless (string-match "[\\.\\?!]$" heading)
	    (setq heading (concat heading "."))))
	(replace-match (format "%s %s" initial-pause heading) t t)))))

;;;;;; Abbreviations

(defun tlon-tts-process-abbreviations (type)
  "Replace abbreviations of TYPE with their spoken equivalent."
  (let ((case-fold-search nil))
    (dolist (entry (pcase type
		     ('local tlon-local-abbreviations)
		     ('global (tlon-tts-get-global-abbreviations))))
      (cl-destructuring-bind (abbrev . expansion) entry
	(let ((abbrev-introduced (format "%s (%s)" expansion abbrev)))
	  ;; we first replace the full abbrev introduction, then the abbrev itself
	  (dolist (cons (list (cons abbrev-introduced expansion) entry))
	    (goto-char (point-min))
	    (while (re-search-forward (car cons) nil t)
	      (replace-match (cdr cons) t t))))))))

;; TODO: the two functions below are not currently used
;; adapt code to handle cases when abbrev ends in sentence
(defun tlon-tts-replace-abbreviations (replacement)
  "When processing abbreviations, replace match with REPLACEMENT.
If the abbreviation occurs at the end of a sentence, do not remove the period."
  (let ((replacement (if nil
			 ;; FIXME: this throws an error when `tlon-tts-abbrev-ends-sentence-p' returns t
			 ;; (tlon-tts-abbrev-ends-sentence-p)
			 (concat replacement ".")
		       replacement)))
    (replace-match replacement t t)))

(defun tlon-tts-abbrev-ends-sentence-p ()
  "Return t iff the abbreviation at point ends the sentence."
  (save-excursion
    (let* ((case-fold-search nil)
	   (start (max (point-min) (- (point) 1)))
	   (end (min (point-max) (+ (point) 2)))
	   (substring (buffer-substring-no-properties start end)))
      (numberp (string-match "\\. [A-Z][[:alpha:]]*?" substring)))))

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
  "Get abbreviations."
  (tlon-tts-get-associated-terms tlon-tts-abbreviations))

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
  (dolist (pair tlon-local-replacements)
    (let ((find (car pair)))
      (goto-char (point-min))
      (while (re-search-forward find nil t)
	(replace-match (cdr pair) t t)))))

;;;;;;; Global

(defun tlon-tts-process-globa-phonetic-replacements ()
  "Replace terms with their counterparts."
  (tlon-tts-process-terms
   (tlon-tts-get-phonetic-replacements)
   'tlon-tts-replace-phonetic-replacements 'word-boundary))

(defun tlon-tts-get-phonetic-replacements ()
  "Get simple replacements."
  (tlon-tts-get-associated-terms tlon-tts-phonetic-replacements))

(defun tlon-tts-replace-phonetic-replacements (replacement)
  "When processing simple replacements, replace match with REPLACEMENT."
  (replace-match replacement t t))

;;;;;; Phonetic transcriptions

;; We are not supporting this currently.

(defun tlon-tts-process-phonetic-transcriptions ()
  "Replace terms with their pronunciations."
  (tlon-tts-process-terms
   (tlon-tts-get-phonetic-transcriptions)
   'tlon-tts-replace-phonetic-transcriptions 'word-boundary))

(defun tlon-tts-get-phonetic-transcriptions ()
  "Get the phonetic transcriptions."
  (tlon-tts-get-associated-terms tlon-tts-phonetic-transcriptions))

(defun tlon-tts-replace-phonetic-transcriptions (replacement)
  "When processing phonetic transcriptions, replace match with pattern.
REPLACEMENT is the cdr of the cons cell for the term being replaced."
  (replace-match (format tlon-tts-ssml-phoneme-replace-pattern
			 "ipa" replacement (match-string-no-properties 0)) t))

;;;;;; Listener cues

;;;;;;; General

(defun tlon-tts-process-listener-cues ()
  "Add listener cues to relevant elements."
  (tlon-tts-process-quotes)
  (tlon-tts-process-asides)
  (tlon-tts-process-images))

(defun tlon-tts-add-listener-cues (type)
  "Add listener cues for text enclosed in tags of TYPE."
  (cl-destructuring-bind (pattern cues group)
      (pcase type
	('aside (list tlon-mdx-aside-search-pattern tlon-tts-aside-cues 2))
	('quote (list tlon-md-blockquote tlon-tts-quote-cues 1))
	('image (list tlon-md-image tlon-tts-image-cues 1))
	;; TODO: determine if other types should be added
	(_ (user-error "Invalid formatting type: %s" type)))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (let* ((match (match-string-no-properties group))
	     (text (pcase type
		     ('quote (replace-regexp-in-string "^> ?" "" match))
		     (_ match))))
	(replace-match
	 (tlon-tts-listener-cue-full-enclose cues text))))))

(defun tlon-tts-enclose-in-listener-cues (type text)
  "Enclose TEXT in listener cues of TYPE."
  (cl-destructuring-bind (cue-begins . cue-ends)
      (alist-get (tlon-tts-get-current-language) type nil nil #'string=)
    (format "%s %s %s" cue-begins text cue-ends)))

(defun tlon-tts-enclose-in-voice-tag (string &optional voice)
  "Enclose STRING in `voice' SSML tags.
If VOICE is nil, default to the alternative voice.

Note that this function inserts two pairs of `voice' tags: the inner pair to set
the voice for the string that it encloses, and an outer pair of tags in reverse
order, to close the opening `voice' tag that wraps the entire document, and to
then reopen it."
  (let ((voice (or voice (tlon-tts-get-alternative-voice))))
    (format tlon-tts-ssml-double-voice-replace-pattern voice string tlon-tts-current-main-voice)))

(defun tlon-tts-get-alternative-voice ()
  "Return the voice in the current language that is not the current voice."
  (let ((voices (tlon-lookup-all tlon-microsoft-azure-voices
				 :voice :language (tlon-tts-get-current-language))))
    (car (delete tlon-tts-current-main-voice voices))))

(defun tlon-tts-enclose-in-cue-delimiter (string)
  "Enclose STRING in listener cue delimiter."
  (format "%1$s%s%1$s" tlon-tts-cue-delimiter string))

(defun tlon-tts-listener-cue-full-enclose (type text)
  "Enclose TEXT in listener cue of TYPE and, if appropriate, in `voice' SSML tags.
Whether TEXT is enclosed in `voice' tags is determined by the value of
`tlon-tts-use-alternate-voice'."
  (tlon-tts-enclose-in-cue-delimiter
   (if tlon-tts-use-alternate-voice
       (tlon-tts-enclose-in-voice-tag (tlon-tts-enclose-in-listener-cues type text))
     (tlon-tts-enclose-in-listener-cues type text))))

;;;;;;; Specific

(defun tlon-tts-process-quotes ()
  "Add listener cues for blockquotes."
  (tlon-tts-add-listener-cues 'quote))

(defun tlon-tts-process-asides ()
  "Add listener cues for asides."
  (tlon-tts-add-listener-cues 'aside))

(defun tlon-tts-process-images ()
  "Add listener cues for images."
  (tlon-tts-add-listener-cues 'image))

;;;;;; Links

(defun tlon-tts-process-links ()
  "Replace links with their text.
Note: this function should be run after `tlon-tts-process-images' because
image links are handled differently."
  (goto-char (point-min))
  (while (re-search-forward markdown-regex-link-inline nil t)
    (replace-match (match-string 3))))

;;;;;; Numbers

;;;;;;; General

(defun tlon-tts-process-numerals ()
  "Process numbers as appropriate."
  (tlon-tts-process-numerals-replace-powers)
  (tlon-tts-process-numerals-replace-roman)
  (tlon-tts-process-numerals-remove-thousands-separators))

;;;;;;; Specific

(defun tlon-tts-process-numerals-replace-powers ()
  "Replace numbers raised to a power with their verbal equivalents."
  (let ((language (tlon-tts-get-current-language)))
    (goto-char (point-min))
    (while (re-search-forward tlon-md-math-power nil t)
      (let* ((base (match-string-no-properties 1))
	     (exponent (match-string-no-properties 2))
	     (exponent-number (string-to-number exponent))
	     (exponent-bounded (if (> exponent-number 10) 10 exponent-number))
	     (inner-list (alist-get exponent-bounded tlon-tts-number-exponents))
	     (string (alist-get language inner-list nil nil #'string=))
	     (verbal-exponent (apply 'format string (list exponent))))
	(replace-match (format "%s %s" base verbal-exponent))))))

(declare-function rst-roman-to-arabic "rst")
(defun tlon-tts-process-numerals-replace-roman ()
  "Replace Roman numerals with their Arabic equivalents."
  (goto-char (point-min))
  (while (re-search-forward tlon-mdx-roman-search-pattern nil t)
    (let* ((roman (match-string-no-properties 2))
	   (arabic (rst-roman-to-arabic roman)))
      (replace-match (number-to-string arabic)))))

(defun tlon-tts-process-numerals-remove-thousands-separators ()
  "Remove default thousands separators in numerals."
  (goto-char (point-min))
  (while (re-search-forward (tlon-get-number-separator-pattern tlon-default-thousands-separator) nil t)
    (replace-match (replace-regexp-in-string tlon-default-thousands-separator "" (match-string 1)))))

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
	    (replace-match replacement)))))))

;;;;;; Remove unsupported SSML tags

(defun tlon-tts-remove-unsupported-ssml-tags ()
  "Remove SSML tags not supported by the current TTS engine."
  (let* ((property (tlon-lookup tlon-tts-engines :property :name tlon-tts-engine))
	 (unsupported-by-them (tlon-lookup-all tlon-tts-supported-tags :tag property nil))
	 (supported-by-us (tlon-lookup-all tlon-tts-supported-tags :tag :tlon t))
	 (tags (cl-intersection unsupported-by-them supported-by-us))
	 (patterns (mapcar (lambda (tag)
			     (tlon-lookup tlon-tts-supported-tags :pattern :tag tag))
			   tags)))
    (dolist (pattern patterns)
      (goto-char (point-min))
      (while (re-search-forward (tlon-make-pattern-searchable pattern) nil t)
	;; handle double backslashes
	(let ((replacement (replace-regexp-in-string "\\\\" "\\\\\\\\" (match-string 2))))
	  (replace-match replacement t))))))

(defun tlon-tts-count-format-placeholders (format)
  "Count the number of format placeholders in the format string FORMAT."
  (let ((count 0)
	(pos 0))
    (while (string-match "%\\([^%]\\)" format pos)
      (setq count (1+ count)
	    pos (match-end 0)))
    count))

(defun tlon-tts-fill-format-string (format string)
  "Fill all format specifiers in FORMAT with STRING."
  (let ((args-count (tlon-tts-count-format-placeholders format))
	args)
    (dotimes (_ args-count)
      (push string args))
    (apply 'format format (reverse args))))

(defun tlon-make-pattern-searchable (pattern &optional greedy)
  "Make an SSML tag PATTERN searchable.
The pattern is greedy if GREEDY is non-nil, and lazy otherwise."
  (let ((format (if greedy ".**" ".*?")))
    (tlon-tts-fill-format-string pattern format)))

;;;;; Project-wide

;;;;;; Common

(defun tlon-tts-edit-entry (variable file)
  "Add or revise an entry in VARIABLE and write it to FILE."
  (set variable (tlon-parse-json file))
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
(defun tlon-tts-edit-abbreviations ()
  "Edit abbreviations."
  (interactive)
  (tlon-tts-edit-entry 'tlon-tts-abbreviations tlon-file-global-abbreviations))

;;;;;; Phonetic replacements

;;;###autoload
(defun tlon-tts-edit-phonetic-replacements ()
  "Edit phonetic replacements."
  (interactive)
  (tlon-tts-edit-entry 'tlon-tts-phonetic-replacements tlon-file-global-phonetic-replacements))

;;;;;; Phonetic transcriptions

;;;###autoload
(defun tlon-tts-edit-phonetic-transcriptions ()
  "Edit phonetic transcriptions."
  (interactive)
  (tlon-tts-edit-entry 'tlon-tts-phonetic-transcriptions tlon-file-global-phonetic-transcriptions))

;;;;; Local

;;;;;; Common

(declare-function modify-file-local-variable "files-x")
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
    (modify-file-local-variable var (push cons-cell var-value) 'add-or-replace)
    (hack-local-variables)))

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

;;;;;; Prompt

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

;;;;;; Duration

(transient-define-infix tlon-tts-heading-break-duration-infix ()
  :class 'transient-lisp-variable
  :variable 'tlon-tts-heading-break-duration
  :reader (lambda (_ _ _) (read-string "Duration (seconds): " tlon-tts-heading-break-duration)))

(transient-define-infix tlon-tts-paragraph-break-duration-infix ()
  :class 'transient-lisp-variable
  :variable 'tlon-tts-paragraph-break-duration
  :reader (lambda (_ _ _) (read-string "Duration (seconds): " tlon-tts-heading-break-duration)))


(transient-define-infix tlon-tts-menu-infix-toggle-alternate-voice ()
  "Toggle the value of `tlon-tts-use-alternate-voice' in `tts' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-tts-use-alternate-voice
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-tts-use-alternate-voice))
  :key "-v")

;; TODO: define infixes to set audio settings
;; tlon-microsoft-azure-audio-settings
;; tlon-google-cloud-audio-settings
;; tlon-amazon-polly-audio-settings
;; tlon-openai-audio-settings
;; tlon-elevenlabs-audio-settings

;;;###autoload (autoload 'tlon-tts-menu "tlon-tts" nil t)
(transient-define-prefix tlon-tts-menu ()
  "`tts' menu."
  [["Insert SSML tag"
    ("b" "break"                            tlon-tts-insert-ssml-break)
    ("e" "emphasis"                         tlon-tts-insert-ssml-emphasis)
    ("l" "lang"                             tlon-tts-insert-ssml-lang)
    ("p" "phoneme"                          tlon-tts-insert-ssml-phoneme)
    ("s" "say-as"                           tlon-tts-insert-ssml-say-as)]
   ["Edit"
    "global"
    ("a" "Abbreviation"                     tlon-tts-edit-abbreviations)
    ("r" "Replacement"                      tlon-tts-edit-phonetic-replacements)
    ("t" "Transcription"                    tlon-tts-edit-phonetic-transcriptions)
    ""
    "file-local"
    ("A" "Abbreviation"                     tlon-add-file-local-abbreviation)
    ("R" "Replacement"                      tlon-add-file-local-replacement)]
   ["Create narration"
    ("z" "Narrate buffer or selection"      tlon-tts-narrate-content)
    ("j" "Join file chunks"                 tlon-tts-join-chunks)
    ("d" "Delete file chunks"               tlon-tts-delete-chunks)
    ""
    "Narration options"
    (tlon-tts-menu-infix-toggle-alternate-voice)]])
    ("-e" "Engine"                           tlon-tts-menu-infix-set-engine)
    ("-p" "Prompt"                           tlon-tts-menu-infix-set-prompt)
    ("-h" "Heading break duration"           tlon-tts-heading-break-duration-infix)
    ("-a" "Paragraph break duration"         tlon-tts-paragraph-break-duration-infix)

(provide 'tlon-tts)
;;; tlon-tts.el ends here
