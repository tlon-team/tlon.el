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
(eval-when-compile (require 'threads)) ; For cond-> macro at compile time

(defgroup tlon-dub ()
  "Dubbing functionality using ElevenLabs."
  :group 'tlon)

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

(defun tlon-dub-convert-vtt-source-to-dest-format (source-file &optional output-file-path)
  "Convert VTT file from 'source.vtt' example format to 'dest.vtt' example format.

SOURCE-FILE is the path to the VTT file in the complex format,
which includes headers, blank lines, tagged text lines, and
duplicated timestamp/text entries.

OUTPUT-FILE-PATH is the optional path for the converted VTT file.
If nil, it defaults to SOURCE-FILE with \"-converted\" suffix and
the same extension (or \".vtt\" if none).

The output format consists of pairs of lines:
  HH:MM:SS.mmm --> HH:MM:SS.mmm
  Caption text

It does not include 'WEBVTT' headers or blank lines between entries.

Returns the path to the OUTPUT-FILE-PATH."
  (unless output-file-path
    (setq output-file-path (concat (file-name-sans-extension source-file)
                                   "-converted."
                                   (or (file-name-extension source-file) "vtt"))))

  (let ((collected-lines '())
        (timestamp-regex "^\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9][0-9][0-9]\\) --> \\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9][0-9][0-9]\\)")
        (timestamp-marker-regex "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9][0-9][0-9] -->")
        (blank-line-regex "^\\s-*$"))
    (with-temp-buffer
      (insert-file-contents source-file)
      (goto-char (point-min))
      ;; Skip header lines (WEBVTT, Kind, Language, empty lines)
      (while (and (not (eobp)) (looking-at-p "^WEBVTT\\|^Kind:\\|^Language:\\|\\s-*$"))
        (if (looking-at-p blank-line-regex)
            ;; If it's a blank line and there's non-blank content after, consume it.
            ;; Otherwise, if it's multiple blank lines before content, this handles them.
            ;; If it's blank lines at EOF, (eobp) will catch it.
            (let ((start (point)))
              (forward-line 1)
              (if (and (eobp) (string-blank-p (buffer-substring-no-properties start (point))))
                  ;; Reached EOF and it was all blank, break.
                  (goto-char (point-max))
                (goto-char start))) ; revert if not just blank lines or if content follows
          (forward-line 1)))

      (while (not (eobp))
        (let ((current-line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          ;; 1. Expect Main Timestamp Line
          (if (string-match timestamp-regex current-line-text)
              (progn
                (push (format "%s --> %s" (match-string 1 current-line-text) (match-string 2 current-line-text)) collected-lines)
                (forward-line 1)

                ;; 2. Skip one empty line after MainTS (if present)
                (when (and (not (eobp)) (looking-at-p blank-line-regex)) (forward-line 1))

                ;; 3. Skip Tagged Text lines until SecondaryTS
                (while (and (not (eobp)) (not (looking-at-p timestamp-marker-regex)))
                  (forward-line 1))

                (if (eobp)
                    (user-error "Unexpected EOF while looking for Secondary Timestamp in %s" source-file)
                  ;; 4. We are at SecondaryTS. Skip it.
                  (forward-line 1)

                  ;; 5. Skip one empty line after SecondaryTS (if present)
                  (when (and (not (eobp)) (looking-at-p blank-line-regex)) (forward-line 1))

                  ;; 6. Collect Clean Text lines
                  (let ((clean-text-parts '()))
                    (while (and (not (eobp))
                                (not (looking-at-p blank-line-regex))
                                (not (looking-at-p timestamp-marker-regex)))
                      (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) clean-text-parts)
                      (forward-line 1))
                    (when clean-text-parts
                      (push (string-join (nreverse clean-text-parts) " ") collected-lines)))

                  ;; 7. Skip one empty line after CleanText (if present)
                  (when (and (not (eobp)) (looking-at-p blank-line-regex))
                    (forward-line 1))))
            (progn ; Line was not a Main Timestamp as expected, skip it to find next block
              (forward-line 1))))))

    (if collected-lines
        (progn
          (write-region (string-join (nreverse collected-lines) "\n") nil output-file-path nil 'silent)
          output-file-path)
      (user-error "No VTT segments found or processed in %s. Output file not created." source-file))))

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

(provide 'tlon-dub)

;;; tlon-dub.el ends here
