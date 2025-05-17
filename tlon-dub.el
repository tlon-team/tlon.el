;;; tlon-dub.el --- Dubbing functionality using ElevenLabs -*- lexical-binding: t -*-

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

;; Provides functions to interact with the ElevenLabs Dubbing API.

;;; Code:

(require 'tlon-core)
(require 'tlon-tts)
(require 'json)
(require 'threads) ; For cond-> at runtime
(eval-when-compile (require 'threads))
(require 'tlon-ai)
(require 'transient)

(defgroup tlon-dub ()
  "Dubbing functionality using ElevenLabs."
  :group 'tlon)

;;;; User Options

(defcustom tlon-dub-propagation-model
  '("Gemini" . gemini-2.0-flash-thinking-exp-01-21)
  "Model to use for propagating timestamps.
The value is a cons cell whose car is the backend and whose cdr is the model
itself. See `gptel-extras-ai-models' for the available options. If nil, use the
default `gptel-model'."
  :type '(cons (string :tag "Backend") (symbol :tag "Model"))
  :group 'tlon-dub)

;;;; Constants

(defconst tlon-dub-api-base-url "https://api.elevenlabs.io/v1"
  "Base URL for the ElevenLabs API.")

(defconst tlon-dub-start-project-endpoint "/dubbing"
  "API endpoint for starting a dubbing project.")

(defconst tlon-dub-get-project-metadata-endpoint "/dubbing/%s"
  "API endpoint format for getting dubbing project metadata.
Requires dubbing_id.")

(defconst tlon-dub-get-dubbing-endpoint "/dubbing/%s/transcript/%s"
  "API endpoint format for getting the transcript for a dubbing project.
Requires `dubbing_id' and `language_code'.")

(defconst tlon-dub-get-resource-data-endpoint "/dubbing/resource/%s"
  "API endpoint format for getting dubbing resource data.
Requires dubbing_id.")

(defconst tlon-dub-share-resource-endpoint "/workspace/resources/%s/share"
  "API endpoint format for sharing a workspace resource.
Requires resource_id.")

(defconst tlon-dub-add-speaker-segment-endpoint "/dubbing/resource/%s/speaker/%s/segment"
  "API endpoint format for adding a speaker segment.
Requires dubbing_id and speaker_id.")

(defconst tlon-dub--vtt-timestamp-element
  "\\([[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\\.[[:digit:]]\\{3\\}\\)"
  "Regexp to match a VTT timestamp element.
Example: \"00:00:00.240\"")

(defconst tlon-dub--vtt-timestamp-marker
  (concat "^" tlon-dub--vtt-timestamp-element " -->")
  "Regexp to identify the beginning of a VTT timestamp line.
Example: \"00:00:00.240 -->\"")

(defconst tlon-dub--vtt-timestamp-line
  (concat tlon-dub--vtt-timestamp-marker tlon-dub--vtt-timestamp-element)
  "Regexp to match a VTT timestamp line and capture start and end times.
Example: \"00:00:00.240 --> 00:00:01.750\"")

(defconst tlon-dub--vtt-blank-line-regex
  "^\\s-*$"
  "Regexp to match a blank or whitespace-only line.")

(defconst tlon-dub-csv-header
  "speaker,start_time,end_time,transcription,translation"
  "Header for the ElevenLabs CSV file.")

(defconst tlon-dub-propagate-machine-timestamps-prompt
  "You will be given two inputs:

1. A machine-generated JSON transcript with timestamps.

2. A human-edited Markdown transcript of the same audio.

The human-edited transcript is more accurate in terms of wording, but has no timestamps.

Your task is to create a new SRT file that combines the timestamps in the WhisperX JSON file with the contents of the human-edited transcript. For each paragraph in the human-edited transcript, determine its start and end times based on the JSON timestamps, and then format this as an SRT segment.

Here is the Machine-generated JSON file:

```
%s
```

And here is the human-edited transcript file (plain text or Markdown):

```
%s
```

Return only the contents of the new SRT file, without any additional commentary. Do not enclose these contents in a code block such as \"```srt\". Here is an example of the beginning of an SRT file as it should look after timestamp propagation:

```
00:00:00,031  --> 00:00:06,360
People think of tilt as what happens, and it often does, when you're on a losing streak or you lose the game despite having a good hand. And you can have different reactions: you can try to recoup your losses, or often people become too hesitant and show risk aversion.

00:00:06,380 --> 00:00:10,740
But the winners' tilt can be just as negative, can't it? If you make a couple of bets in a row that come out right, especially if they're countertrend bets, as in the case of Elon Musk or Peter Thiel, for example... if you make a couple of countertrend bets and they come out right, it's really satisfying to get a financial return. Proving them all wrong gives you a lot of pleasure. And if you get both at the same time, it's like a drug cocktail, really. It produces a tremendous effect.
```"
  "Prompt for timestamp propagation.")

(defconst tlon-dub-propagate-english-timestamps-prompt
  "You will be given two inputs:

1. An English SRT file with timestamps.

2. A plain text or Markdown file with the translation of that transcript into another language. This translation does not have timestamps.

Your task is to create a new SRT file that combines the timestamps from the English SRT file with the content of the translated file. Each segment in the output SRT file should use the timestamps from the corresponding English segment and the text from the translated file.

English SRT file with timestamps:

```
%s
```

Translated plain text/Markdown file without timestamps:

```
%s
```

Return only the contents of the new timestamped translated SRT file, without any additional commentary. Do not enclose these contents in a code block such as \"```srt\". Here is an example of how a segment of the output SRT file should look:

```
00:00:00,031  --> 00:00:06,360
La gente piensa que el tilt es lo que ocurre, y suele ocurrir, cuando estás en una racha perdedora o pierdes la partida a pesar de tener una buena mano. Y puedes tener diferentes reacciones: puedes intentar recuperar tus pérdidas, o a menudo la gente se vuelve demasiado indecisa y muestra aversión al riesgo.

00:00:06,380 --> 00:00:10,740
Pero la inclinación de los ganadores puede ser igual de negativa, ¿no? Si haces un par de apuestas seguidas que salen bien, especialmente si son apuestas contra tendencia, como en el caso de Elon Musk o Peter Thiel, por ejemplo... si haces un par de apuestas contra tendencia y salen bien, es realmente satisfactorio obtener un rendimiento financiero. Demostrar que se equivocan todos te da mucho placer. Y si consigues ambas cosas a la vez, es como un cóctel de drogas, la verdad. Produce un efecto tremendo.
```"
  "Prompt for propagating timestamps from English to a translated file.")

;;;; Helper Functions

(defun tlon-dub--get-content-type (filename)
  "Return the MIME content type based on FILENAME's extension.
Returns nil if the extension is not recognized or unsupported for dubbing."
  (let ((extension (downcase (file-name-extension filename))))
    (cond
     ((string= extension "mp3") "audio/mpeg")
     ((string= extension "wav") "audio/wav")
     ((string= extension "mp4") "video/mp4")
     ((string= extension "webm") "video/webm")
     ((string= extension "flac") "audio/flac")
     ((string= extension "ogg") "audio/ogg") ; Common container for opus/vorbis
     ((string= extension "opus") "audio/opus")
     ;; Add other supported types as needed
     (t nil)))) ; Unsupported type

(defun tlon-dub--share-project-with-self (resource-id)
  "Share the workspace RESOURCE-ID with the user associated with the API key.
Shares with \"editor\" role using the top-level \"user_email\" field."
  (let* ((api-key (tlon-tts-elevenlabs-get-or-set-key))
	 (url (format (concat tlon-dub-api-base-url tlon-dub-share-resource-endpoint)
		      resource-id))
	 (user-email tlon-email-shared) ; Use the user email associated with the API key
	 ;; Revert to using user_email at the top level, matching successful curl structure
	 (payload-alist `(("resource_type" . "dubbing") ; Correct type for dubbing
			  ("user_email" . ,user-email)    ; Top-level user email
			  ("role" . "editor")))           ; Desired role
	 (payload (json-encode payload-alist))
	 ;; Build the argument list for curl
	 (args (list "curl" "-s"
		     "--request" "POST"
		     "--url" url
		     "--header" "accept: application/json"
		     "--header" "Content-Type: application/json"
		     "--header" (format "xi-api-key: %s" api-key)
		     "--data" payload))
	 (command (mapconcat #'shell-quote-argument args " ")))
    (message "Sharing resource %s with %s (role: editor)..." resource-id user-email) ; Revert message to use user-email
    (when tlon-debug (message "Debug: Running share command: %s" command))
    (let ((response (shell-command-to-string command)))
      ;; Check response - success might be empty or a simple confirmation
      (if (or (string-empty-p response)
	      (string-match-p "success" response)) ; Adjust if API returns specific success JSON
	  (progn
	    (message "Successfully shared resource %s." resource-id)
	    t) ; Indicate success
	(progn
	  (message "Failed to share resource %s. Response:\n%s" resource-id response)
	  ;; Try to parse potential error JSON
	  (condition-case _err
	      (json-parse-string response :object-type 'alist)
	    (error response)) ; Return raw response or parsed error
	  nil))))) ; Indicate failure

(defun tlon-dub--parse-vtt (vtt-string)
  "Parse VTT-STRING and return a list of segments.
Each segment is a plist with :start, :end, and :text keys."
  (let ((lines (split-string vtt-string "\n"))
	segments current-segment text-buffer)
    ;; Basic state machine: expecting timestamp or text
    (dolist (line lines)
      (cond
       ;; Skip WEBVTT header and empty lines
       ((or (string-prefix-p "WEBVTT" line) (string-blank-p line))
	nil) ; Do nothing
       ;; Skip NOTE comments
       ((string-prefix-p "NOTE" line)
	nil) ; Do nothing
       ;; Match timestamp line (e.g., 00:00:00.000 --> 00:00:05.000 ...)
       ((string-match "^\\([0-9:]+\\.[0-9]+\\)[ \t]+-->[ \t]+\\([0-9:]+\\.[0-9]+\\)" line)
	;; If we were collecting text, finalize the previous segment
	(when text-buffer
	  (setf (plist-put current-segment :text) (string-trim text-buffer))
	  (push current-segment segments)
	  (setq text-buffer nil))
	;; Start a new segment
	(setq current-segment (list :start (match-string 1 line) :end (match-string 2 line))
	      text-buffer "")) ; Initialize text buffer for the new segment
       ;; Assume it's text payload if a segment has been started
       (current-segment
	(setq text-buffer (concat text-buffer (if (string-empty-p text-buffer) "" "\n") line)))))
    ;; Add the last segment if text was collected
    (when text-buffer
      (setf (plist-put current-segment :text) (string-trim text-buffer))
      (push current-segment segments))
    (nreverse segments)))

(defun tlon-dub-convert-vtt-to-csv (source-file &optional output-file-path)
  "Convert VTT file to a CSV file in the format required by Elevenlabs.
SOURCE-FILE is the path to the VTT file in the complex format, which includes
headers, blank lines, tagged text lines, and duplicated timestamp/text entries.

OUTPUT-FILE-PATH is the optional path for the converted CSV file. If nil, it
defaults to SOURCE-FILE with its extension changed to \".csv\".

  The CSV file starts with the header line specified in `tlon-dub-csv-header'.
  Subsequent data lines have the format:
  \"\",\"HH:MM:SS,mmm\",\"HH:MM:SS,mmm\",\"Caption text\",\"\"

This format is intended for Elevenlabs, as detailed here:
https://elevenlabs.io/docs/product-guides/products/dubbing/dubbing-studio#example-csv-files

Returns the path to the OUTPUT-FILE-PATH."
  (unless output-file-path
    (setq output-file-path (concat (file-name-sans-extension source-file) ".csv")))

  (let ((collected-lines '()))
    (with-temp-buffer
      (insert-file-contents source-file)
      (goto-char (point-min))
      ;; Skip header lines (WEBVTT, Kind, Language, empty lines)
      (while (and (not (eobp)) (looking-at-p "^WEBVTT\\|^Kind:\\|^Language:\\|\\s-*$"))
	(forward-line 1))
      (while (not (eobp))
	(let ((current-line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
	  ;; 1. Expect Main Timestamp Line
	  (if (string-match tlon-dub--vtt-timestamp-line current-line-text)
	      (let ((formatted-timestamp ; Store the formatted timestamp
		     (format "\"%s\",\"%s\""
			     (replace-regexp-in-string "\\." "," (match-string 1 current-line-text))
			     (replace-regexp-in-string "\\." "," (match-string 2 current-line-text)))))
		(forward-line 1) ; Consume the timestamp line
		;; 2. Skip one empty line after MainTS (if present)
		(when (and (not (eobp)) (looking-at-p tlon-dub--vtt-blank-line-regex)) (forward-line 1))
		;; 3. Collect Intermediate Text lines (potential "Tagged Text") until SecondaryTS or EOF
		;;    and determine the final caption text.
		(let ((intermediate-text-parts '())
		      (caption-text-final "")) ; Initialize final caption text
		  (while (and (not (eobp)) (not (looking-at-p tlon-dub--vtt-timestamp-marker)))
		    (push (buffer-substring-no-properties (line-beginning-position) (line-end-position))
			  intermediate-text-parts)
		    (forward-line 1))
		  (if (looking-at-p tlon-dub--vtt-timestamp-marker) ; Found SecondaryTS
		      (progn
			;; 4. We are at SecondaryTS. Skip it.
			(forward-line 1)
			;; 5. Skip one empty line after SecondaryTS (if present)
			(when (and (not (eobp)) (looking-at-p tlon-dub--vtt-blank-line-regex)) (forward-line 1))
			;; 6. Collect Clean Text lines (this is the actual caption)
			(let ((clean-text-parts '()))
			  (while (and (not (eobp))
				      (not (looking-at-p tlon-dub--vtt-blank-line-regex))
				      (not (looking-at-p tlon-dub--vtt-timestamp-marker)))
			    (push (buffer-substring-no-properties (line-beginning-position) (line-end-position))
				  clean-text-parts)
			    (forward-line 1))
			  (when clean-text-parts
			    (setq caption-text-final (string-join (nreverse clean-text-parts) " "))))
			;; 7. Skip one empty line after CleanText (if present) - crucial for loop progression.
			(when (and (not (eobp)) (looking-at-p tlon-dub--vtt-blank-line-regex))
			  (forward-line 1)))
		    ;; Else (EOF reached, or non-TS line that breaks the pattern before a SecondaryTS)
		    ;; Use the collected intermediate-text-parts as the caption.
		    (when intermediate-text-parts
		      (setq caption-text-final (string-join (nreverse intermediate-text-parts) " "))))
		  ;; Combine stored timestamp and determined caption text, then push to collected-lines.
		  ;; Format: "", "start_time", "end_time", "transcription", ""
		  (push (format "\"\",%s,\"%s\",\"\""
				formatted-timestamp caption-text-final) collected-lines)))
	    (forward-line 1)))))
    (if collected-lines
	(let* ((data-string (string-join (nreverse collected-lines) "\n"))
	       (final-content (concat tlon-dub-csv-header "\n" data-string)))
	  (write-region final-content nil output-file-path nil 'silent)
	  output-file-path)
      (user-error "No VTT segments found or processed in %s. Output file not created" source-file))))

;;;; Functions

;;;###autoload
(defun tlon-dub-start-project (source-file source-lang target-lang project-name
					   &optional voice-id dubbing-studio num-speakers)
  "Start an ElevenLabs dubbing project for SOURCE-FILE.

- SOURCE-FILE is the path to the audio or video file to dub.


- SOURCE-LANG is the ISO code of the source language (e.g., \"en\").


- TARGET-LANG is the ISO code of the target language (e.g., \"es\").


- PROJECT-NAME is a name for the dubbing project.


- VOICE-ID is the optional ID of the ElevenLabs voice to use for the dubbing.

If VOICE-ID is nil, ElevenLabs might use a default or clone the original voice.
DUBBING-STUDIO, if non-nil, creates the project in Dubbing Studio
mode (adjustable). NUM-SPEAKERS, if > 0, specifies the expected number of
speakers.

Returns the JSON response from the API, typically containing the `dubbing_id'."
  (interactive
   (list (read-file-name "Source file: ")
	 (tlon-get-iso-code (tlon-read-language nil "Source language: " t nil)) ; Convert to ISO code
	 (tlon-get-iso-code (tlon-read-language nil "Target language: " t nil)) ; Convert to ISO code
	 (read-string "Project name: ")
	 (completing-read "Voice ID (optional, press RET for default): "
			  (mapcar (lambda (v) (plist-get v :id)) tlon-elevenlabs-voices)
			  nil nil nil nil "") ; Allow empty input for optional voice
	 (y-or-n-p "Create in Dubbing Studio mode (adjustable)? ")
	 (let ((num-str (read-string "Number of speakers (optional, press RET for auto): ")))
	   (if (string-empty-p num-str) 0 (string-to-number num-str)))))
  (let* ((content-type (tlon-dub--get-content-type source-file)))
    ;; Check for supported file type before proceeding
    (unless content-type
      (user-error "Unsupported file type for dubbing: %s. Please provide an audio or video file"
		  (file-name-extension source-file)))
    (let* ((api-key (tlon-tts-elevenlabs-get-or-set-key))
	   (url (concat tlon-dub-api-base-url tlon-dub-start-project-endpoint))
	   ;; Build the argument list for curl
	   (args (list "curl" "-s"
		       "--request" "POST"
		       "--url" url
		       "--header" "accept: application/json"
		       "--header" (format "xi-api-key: %s" api-key)
		       "--form" "mode=automatic"
		       ;; Use the determined content-type
		       "--form" (format "file=@%s;type=%s" source-file content-type)
		       "--form" (format "name=%s" project-name)
		       "--form" (format "source_lang=%s" source-lang)
		       "--form" (format "target_lang=%s" target-lang)))
	   ;; Conditionally add optional form parameters
	   (final-args (cond-> args
			       ;; Add voice_id if provided
			       (and voice-id (not (string-empty-p voice-id)))
			       (append (list "--form" (format "voice_id=%s" voice-id)))
			       ;; Add dubbing_studio if true
			       dubbing-studio
			       (append (list "--form" "dubbing_studio=true"))
			       ;; Add num_speakers if > 0
			       (and num-speakers (> num-speakers 0))
			       (append (list "--form" (format "num_speakers=%d" num-speakers)))))
	   (command (mapconcat #'shell-quote-argument final-args " ")))
      (message "Starting dubbing project '%s' for %s..." project-name (file-name-nondirectory source-file))
      (when tlon-debug (message "Debug: Running command: %s" command))
      (let ((response-string (shell-command-to-string command)))
	(message "Dubbing project started. Response:\n%s" response-string)
	;; Attempt to parse the JSON response
	(condition-case err
	    (let* ((parsed-response (json-parse-string response-string :object-type 'alist))
		   (dubbing-id (cdr (assoc 'dubbing_id parsed-response))))
	      ;; If parsing succeeded and we got a dubbing_id, try to share it
	      (if dubbing-id
		  (if (tlon-dub--share-project-with-self dubbing-id)
		      parsed-response ; Return original parsed response if sharing succeeds
		    ;; If sharing fails, maybe return the original response but log error?
		    (progn
		      (message "Warning: Project created (%s) but failed to share automatically." dubbing-id)
		      parsed-response)) ; Still return the creation response
		;; If no dubbing_id found in response
		(progn
		  (message "Could not find dubbing_id in response.")
		  parsed-response))) ; Return the parsed response anyway
	  (error (progn ; Handle JSON parsing error
		   (message "Error parsing project creation JSON response: %s" err)
		   response-string))))))) ; Return raw response string on error

;;;###autoload
(defun tlon-dub-get-project-metadata (dubbing-id)
  "Get metadata for the ElevenLabs dubbing project with DUBBING-ID."
  (interactive (list (read-string "Dubbing ID: ")))
  (let* ((api-key (tlon-tts-elevenlabs-get-or-set-key))
	 (url (format (concat tlon-dub-api-base-url tlon-dub-get-project-metadata-endpoint)
		      dubbing-id))
	 ;; Build the argument list for curl
	 (args (list "curl" "-s"
		     "--request" "GET"
		     "--url" url
		     "--header" "accept: application/json"
		     "--header" (format "xi-api-key: %s" api-key)))
	 (command (mapconcat #'shell-quote-argument args " ")))
    (message "Getting metadata for dubbing project %s..." dubbing-id)
    (when tlon-debug (message "Debug: Running command: %s" command))
    (let ((response (shell-command-to-string command)))
      (message "Metadata received. Response:\n%s" response)
      ;; Optionally parse the JSON response
      (condition-case err
	  (json-parse-string response :object-type 'alist)
	(error (progn
		 (message "Error parsing JSON response: %s" err)
		 response))))))

;;;###autoload
(defun tlon-dub-get-dubbing (dubbing-id language-code)
  "Get the dubbing for the project DUBBING-ID in LANGUAGE-CODE.
LANGUAGE-CODE should be the ISO code (e.g., \"en\", \"es\") for the desired
transcript."
  (interactive
   (list (read-string "Dubbing ID: ")
	 (tlon-get-iso-code (tlon-read-language nil "Language code for transcript: " t nil))))
  (let* ((api-key (tlon-tts-elevenlabs-get-or-set-key))
	 (url (format (concat tlon-dub-api-base-url tlon-dub-get-dubbing-endpoint)
		      dubbing-id language-code))
	 ;; Build the argument list for curl
	 ;; Use -L to follow redirects, as this endpoint might return a temporary URL for the transcript file
	 (args (list "curl" "-s" "-L"
		     "--request" "GET"
		     "--url" url
		     "--header" "accept: application/json" ; VTT is text-based, JSON accept might be ignored or fine
		     "--header" (format "xi-api-key: %s" api-key)))
	 (command (mapconcat #'shell-quote-argument args " ")))
    (message "Getting transcript for dubbing project %s (language: %s)..." dubbing-id language-code)
    (when tlon-debug (message "Debug: Running command: %s" command))
    (let ((response (shell-command-to-string command)))
      (message "Transcript received.")
      ;; Parse the VTT response
      (let ((segments (tlon-dub--parse-vtt response)))
	(if segments
	    (progn
	      (message "Parsed %d segments." (length segments))
	      ;; Display parsed segments in a new buffer
	      (with-current-buffer (get-buffer-create (format "*Dub Segments: %s (%s)*" dubbing-id language-code))
		(erase-buffer)
		(insert ";; Parsed Segments:\n")
		(pp segments (current-buffer)) ; Pretty-print the list
		(goto-char (point-min))
		(switch-to-buffer (current-buffer)))
	      segments) ; Return the parsed list
	  (progn
	    (message "Could not parse transcript. Displaying raw response.")
	    ;; Display raw response if parsing failed
	    (with-current-buffer (get-buffer-create (format "*Dub Transcript (Raw): %s (%s)*" dubbing-id language-code))
	      (erase-buffer)
	      (insert response)
	      (goto-char (point-min))
	      (switch-to-buffer (current-buffer)))
	    ;; Return raw response on parsing failure
	    response))))))

;;;###autoload
(defun tlon-dub-get-resource-data (dubbing-id)
  "Get resource data for the ElevenLabs dubbing project with DUBBING-ID.
This endpoint might provide detailed structure including resource, speaker, and
segment IDs."
  (interactive (list (read-string "Dubbing ID: ")))
  (let* ((api-key (tlon-tts-elevenlabs-get-or-set-key))
	 (url (format (concat tlon-dub-api-base-url tlon-dub-get-resource-data-endpoint)
		      dubbing-id))
	 ;; Build the argument list for curl
	 (args (list "curl" "-s"
		     "--request" "GET"
		     "--url" url
		     "--header" "accept: application/json"
		     "--header" (format "xi-api-key: %s" api-key)))
	 (command (mapconcat #'shell-quote-argument args " ")))
    (message "Getting resource data for dubbing project %s..." dubbing-id)
    (when tlon-debug (message "Debug: Running command: %s" command))
    (let ((response (shell-command-to-string command)))
      (message "Resource data received. Response:\n%s" response)
      ;; Attempt to parse the JSON response
      (condition-case err
	  (let ((parsed-json (json-parse-string response :object-type 'alist)))
	    ;; Display parsed JSON in a new buffer for inspection
	    (with-current-buffer (get-buffer-create (format "*Dub Resource Data: %s*" dubbing-id))
	      (erase-buffer)
	      (insert ";; Parsed Resource Data:\n")
	      (pp parsed-json (current-buffer)) ; Pretty-print the list
	      (goto-char (point-min))
	      (switch-to-buffer (current-buffer)))
	    parsed-json) ; Return the parsed alist
	(error (progn
		 (message "Error parsing JSON response: %s. Displaying raw." err)
		 ;; Display raw response if parsing failed
		 (with-current-buffer (get-buffer-create (format "*Dub Resource Data (Raw): %s*" dubbing-id))
		   (erase-buffer)
		   (insert response)
		   (goto-char (point-min))
		   (switch-to-buffer (current-buffer)))
		 response)))))) ; Return raw response on error

;;;###autoload
(defun tlon-dub-add-speaker-segment (dubbing-id speaker-id start-time end-time text)
  "Add a new segment for SPEAKER-ID within DUBBING-ID.

- DUBBING-ID is the ID of the dubbing project.
- SPEAKER-ID is the ID of the speaker within the project.
- START-TIME is the start time of the segment in seconds (float).
- END-TIME is the end time of the segment in seconds (float).
- TEXT is the text content of the segment."
  (interactive
   (list (read-string "Dubbing ID: ")
	 (read-string "Speaker ID: ")
	 (read-number "Start time (seconds): ")
	 (read-number "End time (seconds): ")
	 (read-string "Segment text: ")))
  (let* ((api-key (tlon-tts-elevenlabs-get-or-set-key))
	 (url (format (concat tlon-dub-api-base-url tlon-dub-add-speaker-segment-endpoint)
		      dubbing-id speaker-id))
	 ;; Construct the JSON payload
	 (payload-alist `(("start_time" . ,start-time)
			  ("end_time" . ,end-time)
			  ("text" . ,text)))
	 (payload (json-encode payload-alist))
	 ;; Build the argument list for curl
	 (args (list "curl" "-s"
		     "--request" "POST"
		     "--url" url
		     "--header" "accept: application/json"
		     "--header" "Content-Type: application/json"
		     "--header" (format "xi-api-key: %s" api-key)
		     "--data" payload))
	 (command (mapconcat #'shell-quote-argument args " ")))
    (message "Adding segment for speaker %s in project %s..." speaker-id dubbing-id)
    (when tlon-debug (message "Debug: Running command: %s" command))
    (let ((response (shell-command-to-string command)))
      (message "Segment addition response:\n%s" response)
      ;; Attempt to parse the JSON response
      (condition-case err
	  (json-parse-string response :object-type 'alist)
	(error (progn
		 (message "Error parsing JSON response: %s" err)
		 response))))))

;;;###autoload
(defun tlon-dub-transcribe-with-whisperx (audio-file &optional format)
  "Generate a transcript for AUDIO-FILE asynchronously using whisperx.
The transcript file will be saved in the same directory as AUDIO-FILE, in the
specified FORMAT. If FORMAT is nil, use \"json\"."
  (interactive (list (read-file-name "Audio/Video file to transcribe: ")))
  (let* ((expanded-audio-file (expand-file-name audio-file))
	 (output-dir (file-name-directory expanded-audio-file))
	 (process-name (format "whisperx-%s" (file-name-nondirectory expanded-audio-file)))
	 (output-buffer (format "*%s-output*" process-name))
	 (format (or format "json"))
	 (command-parts (list "whisperx"
			      expanded-audio-file
			      "--compute_type" "float32"
			      "--output_format" format
			      "--output_dir" output-dir)))
    (message "Starting whisperx %s transcription for %s..." format audio-file)
    (message "Output %s file will be in %s. Check %s for progress/errors." format output-dir output-buffer)
    (let ((process (apply #'start-process process-name output-buffer command-parts)))
      (set-process-sentinel
       process
       (lambda (proc event)
	 (let ((status (process-status proc)))
	   (with-current-buffer (process-buffer proc)
	     (message "Whisperx process '%s' for '%s' finished with status: %s. Output in %s and buffer %s"
		      (process-name proc) expanded-audio-file status output-dir (buffer-name (process-buffer proc))))
	   (when (memq status '(exit signal))
	     (cond
	      ((string-match "exited abnormally" event)
	       (message "Whisperx process for %s exited abnormally. Check buffer %s for details."
			expanded-audio-file (buffer-name (process-buffer proc)))
	       (display-buffer (process-buffer proc)))
	      ((string-match "finished" event)
	       (message "Whisperx transcription for %s completed successfully. Output in %s."
			expanded-audio-file output-dir))
	      (t
	       (message "Whisperx process for %s event: %s. Output in %s. Check buffer %s."
			expanded-audio-file event output-dir (buffer-name (process-buffer proc))))))))))))

;;;;; Timestamp propagation

;;;;;; Machine timestamps

;;;###autoload
(defun tlon-dub-propagate-machine-timestamps (machine-transcript human-transcript)
  "Propagate timestamps from MACHINE-TRANSCRIPT to HUMAN-TRANSCRIPT using AI.
The AI will attempt to align the timestamps from the (machine-generated)
WhisperX JSON output with the (human-edited) Markdown transcript. The result, a
Markdown file with the contents of the human-edited transcript and the
timestamps from the machine-generated transcript, is saved to a new file
specified by the user."
  (interactive
   (list (read-file-name "Machine-generated transcript: " nil nil t)
	 (read-file-name "Human-edited transcript: " nil nil t)))
  (let ((whisperx-content (with-temp-buffer
			    (insert-file-contents machine-transcript)
			    (buffer-string)))
	(human-text-content (with-temp-buffer
			      (insert-file-contents human-transcript)
			      (buffer-string))))
    (if (or (string-empty-p whisperx-content) (string-empty-p human-text-content))
	(user-error "One or both input files are empty")
      (let ((prompt (format tlon-dub-propagate-machine-timestamps-prompt
			    whisperx-content
			    human-text-content)))
	(message "Requesting AI to propagate timestamps...")
	(tlon-make-gptel-request prompt nil
				 (lambda (response info)
				   (tlon-dub--propagate-machine-timestamps-callback response info human-transcript))
				 tlon-dub-propagation-model
				 'no-context-check)))))

(defun tlon-dub--propagate-machine-timestamps-callback (response info human-transcript)
  "Callback for `tlon-dub-propagate-machine-timestamps'.
RESPONSE is the AI's response. INFO is the response info.
HUMAN-TRANSCRIPT is the path to the original human-edited transcript file."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let* ((output-file (file-name-with-extension
			 (concat (file-name-sans-extension human-transcript)
				 "-timestamped")
			 "srt"))) ; Output SRT file
      (with-temp-buffer
	(insert response)
	(write-file output-file))
      (message "Timestamped SRT transcript saved to \"%s\"" output-file))))

;;;;;; English timestamps

;;;###autoload
(defun tlon-dub-propagate-english-timestamps (english-timestamped-file translated-file)
  "Propagate timestamps from ENGLISH-TIMESTAMPED-FILE to TRANSLATED-FILE using AI.
The AI will attempt to align the timestamps from the (timestamped)
English Markdown file with the (non-timestamped) translated Markdown file.
The result, a Markdown file with the contents of the translated transcript and
the timestamps from the English file, is saved to a new file specified by the
user."
  (interactive
   (list (read-file-name "Timestamped English Markdown file: " nil nil t)
	 (read-file-name "Translated Markdown file (no timestamps): " nil nil t)))
  (let ((english-content (with-temp-buffer
			   (insert-file-contents english-timestamped-file)
			   (buffer-string)))
	(translated-content (with-temp-buffer
			      (insert-file-contents translated-file)
			      (buffer-string))))
    (if (or (string-empty-p english-content) (string-empty-p translated-content))
	(user-error "One or both input files are empty")
      (let ((prompt (format tlon-dub-propagate-english-timestamps-prompt
			    english-content
			    translated-content)))
	(message "Requesting AI to propagate English timestamps to translated file...")
	(tlon-make-gptel-request prompt nil
				 (lambda (response info)
				   (tlon-dub--propagate-english-timestamps-callback response info translated-file))
				 tlon-dub-propagation-model
				 'no-context-check)))))

(defun tlon-dub--propagate-english-timestamps-callback (response info translated-file)
  "Callback for `tlon-dub-propagate-english-timestamps'.
RESPONSE is the AI's response. INFO is the response info.
TRANSLATED-FILE is the path to the original non-timestamped translated file."
  (if (not response)
      (tlon-ai-callback-fail info)
    (let* ((output-file (file-name-with-extension
			 (concat (file-name-sans-extension translated-file)
				 "-timestamped")
			 "srt"))) ; Output SRT file
      (with-temp-buffer
	(insert response)
	(write-file output-file))
      (message "Timestamped translated SRT transcript saved to \"%s\"" output-file))))

;;;;; SRT to CSV Conversion

(defconst tlon-dub--srt-timestamp-regex
  "^\\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\},[0-9]\\{3\\}\\) --> \\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\},[0-9]\\{3\\}\\)$"
  "Regexp to match an SRT timestamp line and capture start and end times.
Example: 00:00:00,031 --> 00:00:06,360")

(defun tlon-dub--parse-srt-file (file)
  "Parse SRT FILE and return a list of segments.
Each segment is a plist with :start, :end, and :text keys.
Returns nil if parsing fails or file is empty."
  (condition-case err
      (with-temp-buffer
	(insert-file-contents file)
	(let ((content (replace-regexp-in-string "\r" "" (buffer-string))) ; Remove CR characters
	      segments)
	  (unless (string-blank-p content)
	    (let ((srt-blocks (split-string content "\n\\s-*\n+" t "[ \t\n]+")))
	      (dolist (block srt-blocks)
		(let (start-time end-time text-lines)
		  (with-temp-buffer
		    (insert block)
		    (goto-char (point-min))
		    ;; Skip optional segment number
		    (when (looking-at "^[0-9]+\\s-*$")
		      (forward-line 1))
		    ;; Expect timestamp line
		    (if (looking-at tlon-dub--srt-timestamp-regex)
			(progn
			  (setq start-time (match-string 1)
				end-time (match-string 2))
			  (forward-line 1)
			  ;; Collect remaining lines as text
			  (while (not (eobp))
			    (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) text-lines)
			    (forward-line 1))
			  (when text-lines
			    (push (list :start start-time :end end-time :text (string-join (nreverse text-lines) "\n"))
				  segments)))
		      (warn "Could not parse SRT block in %s: %s" file block))))))
	    (nreverse segments))))
    (error (progn (message "Error parsing SRT file %s: %s" file err) nil))))

(defun tlon-dub--csv-escape-string (str)
  "Escape STR for CSV by doubling quotes and enclosing in quotes."
  (format "\"%s\"" (replace-regexp-in-string "\"" "\"\"" str)))

;;;###autoload
(defun tlon-dub-convert-srt-to-csv (english-srt-file translated-srt-file)
  "Convert English and Translated SRT files to a CSV file for ElevenLabs.
ENGLISH-SRT-FILE is the path to the timestamped English SRT file.
TRANSLATED-SRT-FILE is the path to the timestamped translated SRT file.
OUTPUT-CSV-FILE is the optional path for the output CSV. If nil, it defaults
to a CSV file named after the ENGLISH-SRT-FILE in the same directory.

The CSV file starts with the header line specified in `tlon-dub-csv-header'.

The function will raise an error if the number of segments in the two SRT
files does not match, or if any corresponding segments have differing start or
end timestamps."
  (interactive
   (list (read-file-name "English SRT file: " nil nil t ".srt")
	 (read-file-name "Translated SRT file: " nil nil t ".srt")))
  (let ((english-segments (tlon-dub--parse-srt-file english-srt-file))
	(translated-segments (tlon-dub--parse-srt-file translated-srt-file)))
    (unless english-segments
      (user-error "Could not parse English SRT file, or it is empty: %s" english-srt-file))
    (unless translated-segments
      (user-error "Could not parse translated SRT file, or it is empty: %s" translated-srt-file))
    (unless (= (length english-segments) (length translated-segments))
      (user-error "Mismatch in number of segments: English SRT has %d, Translated SRT has %d"
		  (length english-segments) (length translated-segments)))
    (let ((csv-lines (list tlon-dub-csv-header))
	  (output-path (concat (file-name-sans-extension english-srt-file) ".csv")))
      (dotimes (i (length english-segments))
	(let* ((eng-seg (nth i english-segments))
	       (trans-seg (nth i translated-segments))
	       (eng-start-time (plist-get eng-seg :start))
	       (eng-end-time (plist-get eng-seg :end))
	       (trans-start-time (plist-get trans-seg :start))
	       (trans-end-time (plist-get trans-seg :end))
	       (eng-text (plist-get eng-seg :text))
	       (trans-text (plist-get trans-seg :text)))
	  ;; Verify that timestamps match for the current segment
	  (unless (and (string= eng-start-time trans-start-time)
		       (string= eng-end-time trans-end-time))
	    (user-error "Timestamp mismatch in segment %d:\nEnglish: %s --> %s\nTranslated: %s --> %s"
			(1+ i) eng-start-time eng-end-time trans-start-time trans-end-time))
	  ;; Timestamps match, proceed to format CSV line using English timestamps
	  (push (format "\"\",%s,%s,%s,%s" ; Speaker is empty
			(tlon-dub--csv-escape-string eng-start-time)
			(tlon-dub--csv-escape-string eng-end-time)
			(tlon-dub--csv-escape-string eng-text)
			(tlon-dub--csv-escape-string trans-text))
		csv-lines)))
      (write-region (string-join (nreverse csv-lines) "\n") nil output-path nil 'silent)
      (message "CSV file created at %s" output-path)
      output-path)))

;;;; Menu

(transient-define-infix tlon-dub-infix-select-propagation-model ()
  "AI model to use for propagating timestamps.
If nil, use the default `gptel-model'."
  :class 'tlon-ai-model-selection-infix
  :variable 'tlon-dub-propagation-model)

;;;###autoload (autoload 'tlon-dub-menu "tlon-dub" nil t)
(transient-define-prefix tlon-dub-menu ()
  "Menu for Tlön Dubbing (`tlon-dub`) functionality."
  :info-manual "(tlon) Dubbing"
  [["Transcription & Timestamps (SRT)"
    ("t" "Transcribe with WhisperX (Audio -> JSON)" tlon-dub-transcribe-with-whisperx)
    ("m" "Propagate Machine Timestamps (JSON + en.txt -> en.srt)" tlon-dub-propagate-machine-timestamps)
    ("e" "Propagate English Timestamps (en.srt + lang.txt -> lang.srt)" tlon-dub-propagate-english-timestamps)
    ("c" "Convert SRTs to CSV (en.srt + lang.srt -> .csv)" tlon-dub-convert-srt-to-csv)]
   ["ElevenLabs API"
    ("s" "Start New Dubbing Project" tlon-dub-start-project)
    ("d" "Get Project Metadata" tlon-dub-get-project-metadata) ; Changed key from "m" to "d" to avoid conflict
    ("g" "Get Dubbing Transcript (VTT)" tlon-dub-get-dubbing)
    ("r" "Get Resource Data" tlon-dub-get-resource-data)
    ("a" "Add Speaker Segment" tlon-dub-add-speaker-segment)]
   ["Options"
    ("-m" "Propagation Model" tlon-dub-infix-select-propagation-model)]])

(provide 'tlon-dub)
;;; tlon-dub.el ends here
