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

(require 'subr-x)         ; for delete-dups, string-blank-p, etc.
(require 'tlon-core)
(require 'tlon-tts)
(require 'json)
(require 'threads) ; For cond-> at runtime
(eval-when-compile (require 'threads))
(require 'transient)
(require 'cl-lib)
(require 'tlon-whisperx)

(declare-function copy-list "cl-lib" (list))
(declare-function cond-> "threads" (val &rest clauses))

(defgroup tlon-dub ()
  "Dubbing functionality using ElevenLabs."
  :group 'tlon)

;;;; User Options




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

;;;;; Regexps

(defconst tlon-dub--diarized-speaker-regex
  "^\\[SPEAKER_\\([0-9]+\\)\\]:[[:space:]]*"
  "Regexp matching a WhisperX diarization speaker prefix inside a subtitle line.")

(defconst tlon-dub--speaker-prefix-regex
  "^\\([A-Z][a-z]*\\(?:[ \t]+[A-Z][a-z]*\\)\\{1,3\\}:\\)[ \t]*"
  "Regexp that matches a speaker name at the start of a line.
Capture group 1 includes the trailing colon (e.g., \"Rob Wiblin:\").
Case must remain significant – always bind `case-fold-search' to nil when
using this regexp.")

;;;;;; Common

(defconst tlon-dub--common-timestamp-element
  "\\([[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}%s[[:digit:]]\\{3\\}\\)"
  "Regexp to match a common timestamp element.
The placeholder %s is replaced with either \\. or , depending on the format.")

(defconst tlon-dub--blank-line-regex
  "^\\s-*$"
  "Regexp to match a blank or whitespace-only line.")

(defconst tlon-dub-csv-header
  "speaker,start_time,end_time,transcription,translation"
  "Header for the ElevenLabs CSV file.")

;;;;;; vtt

(defconst tlon-dub--vtt-timestamp-element
  (format tlon-dub--common-timestamp-element "\\.")
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

;;;;;; srt

(defconst tlon-dub--srt-timestamp-element
  (format tlon-dub--common-timestamp-element ",")
  "Regexp to match an SRT timestamp element.
Example: \"00:00:00,240\"")

(defconst tlon-dub--srt-timestamp-marker
  (concat "^" tlon-dub--srt-timestamp-element " --> ")
  "Regexp to identify the beginning of an SRT timestamp line.
Example: \"00:00:00,240 -->\"")

(defconst tlon-dub--srt-timestamp-line
  (concat tlon-dub--srt-timestamp-marker tlon-dub--srt-timestamp-element)
  "Regexp to match an SRT timestamp line and capture start and end times.
Example: \"00:00:00,240 --> 00:00:01,750\"")

;;;;; Propagation





;;;; Helper Functions

;; ------------------------------------------------------------------
;; Compatibility: Emacs < 28 does not have `plist-delete'.
(unless (fboundp 'plist-delete)
  (defun plist-delete (plist prop)
    "Return a copy of PLIST with every occurrence of PROP removed."
    (let ((res nil))
      (while plist
        (let ((key (pop plist))
              (val (pop plist)))
          (unless (eq key prop)
            (setq res (cons val (cons key res))))))
      (nreverse res)))
)
;; ------------------------------------------------------------------

(defun tlon-dub--timestamp-to-seconds (ts)
  "Convert an SRT timestamp TS (\"HH:MM:SS,mmm\") to float seconds."
  (when (string-match "\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\),\\([0-9][0-9][0-9]\\)" ts)
    (+ (* 3600 (string-to-number (match-string 1 ts)))
       (* 60   (string-to-number (match-string 2 ts)))
       (string-to-number (match-string 3 ts))
       (/ (string-to-number (match-string 4 ts)) 1000.0))))

(defun tlon-dub--get-speaker (text)
  "Return the speaker prefix (including the colon) found at start of TEXT or nil."
  (let ((case-fold-search nil))
    (when (string-match tlon-dub--speaker-prefix-regex text)
      (match-string 1 text))))

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
	  (setf (plist-get current-segment :text) (string-trim text-buffer))
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
      (setf (plist-get current-segment :text) (string-trim text-buffer))
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
		(when (and (not (eobp)) (looking-at-p tlon-dub--blank-line-regex)) (forward-line 1))
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
			(when (and (not (eobp)) (looking-at-p tlon-dub--blank-line-regex)) (forward-line 1))
			;; 6. Collect Clean Text lines (this is the actual caption)
			(let ((clean-text-parts '()))
			  (while (and (not (eobp))
				      (not (looking-at-p tlon-dub--blank-line-regex))
				      (not (looking-at-p tlon-dub--vtt-timestamp-marker)))
			    (push (buffer-substring-no-properties (line-beginning-position) (line-end-position))
				  clean-text-parts)
			    (forward-line 1))
			  (when clean-text-parts
			    (setq caption-text-final (string-join (nreverse clean-text-parts) " "))))
			;; 7. Skip one empty line after CleanText (if present) - crucial for loop progression.
			(when (and (not (eobp)) (looking-at-p tlon-dub--blank-line-regex))
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
(defun tlon-dub-resegment-srt (srt-file &optional min-duration)
  "Resegment SRT-FILE and write a new file with larger segments.
A new segment starts whenever the speaker changes and every segment is at
least MIN-DURATION seconds (default 30).  Subject to these constraints, the
segments are as short as possible.  The new file is written next to the
original with the suffix “-resegmented.srt” and the list of segments is
returned."
  (interactive (list (read-file-name "SRT file: " nil nil t ".srt")))
  (setq min-duration (or min-duration 5))
  (let* ((segments (tlon-dub--parse-srt srt-file))
         (merged '())
         (current nil)
         (current-speaker ""))   ;; NEW
    (dolist (seg segments)
      (let* ((text (plist-get seg :text))
             (detected (tlon-dub--get-speaker text)))
        ;; Update `current-speaker` whenever a speaker prefix is detected
        (when detected
          (setq current-speaker detected))
        (let ((speaker current-speaker))
          (if (null current)
              ;; ----- first block -----
              (setq current (list :start (plist-get seg :start)
                                  :end   (plist-get seg :end)
                                  :text  text
                                  :speaker speaker))
            ;; ----- we already have an open block -----
            (let* ((same-speaker (string= speaker (plist-get current :speaker)))
                   (cur-dur (- (tlon-dub--timestamp-to-seconds (plist-get current :end))
                               (tlon-dub--timestamp-to-seconds (plist-get current :start)))))
              (cond
               ;; 1. Speaker changed → always close current block
               ((not same-speaker)
                (let ((seg (copy-sequence current)))
                  (cl-remf seg :speaker)
                  (push seg merged))
                (setq current (list :start (plist-get seg :start)
                                    :end   (plist-get seg :end)
                                    :text  text
                                    :speaker speaker)))
               ;; 2. Same speaker, ≥ MIN-DURATION, and current text ends a sentence → close
               ((and (>= cur-dur min-duration)
                     (tlon-dub--ends-sentence-p (plist-get current :text)))
                (let ((seg (copy-sequence current)))
                  (cl-remf seg :speaker)
                  (push seg merged))
                (setq current (list :start (plist-get seg :start)
                                    :end   (plist-get seg :end)
                                    :text  text
                                    :speaker speaker)))
               ;; 3. Otherwise keep extending current block (same speaker, < MIN-DURATION)
               (t
                (setf (plist-get current :end) (plist-get seg :end))
                (setf (plist-get current :text)
                      (concat (plist-get current :text) "\n" text)))))))))
    ;; push the last open segment
    (when current
      (let ((seg (copy-sequence current)))
        (cl-remf seg :speaker)
        (push seg merged)))
    (setq merged (nreverse merged))
    (let ((out-file (concat (file-name-sans-extension srt-file) "-resegmented.srt")))
      (tlon-dub--write-srt-file merged out-file)
      (message "Resegmented file written to %s" out-file)
      merged)))

;;;###autoload
(defun tlon-dub-clean-diarized-srt (diarized-file &optional speaker-alist)
  "Convert DIARIZED-FILE into a cleaner SRT.

Interactively prompts for a name for every distinct SPEAKER_xx that appears.
SPEAKER-ALIST may be supplied programmatically as `((\"00\" . \"Alice\") (\"01\"
. \"Bob\"))`.

The first subtitle block of each *speaker run* is prefixed with \"Name: \",
subsequent contiguous blocks by the same speaker omit any prefix. Returns the
pathname of the newly written “-cleaned.srt” file."
  (interactive (list (read-file-name "Diarized SRT: " nil nil t ".srt")))
  (let* ((segments (tlon-dub--parse-srt diarized-file))
         ;; collect unique speaker ids present in file
         (ids (delete-dups
               (delq nil
                     (mapcar (lambda (seg)
                               (let ((text (plist-get seg :text)))
                                 (when (string-match tlon-dub--diarized-speaker-regex text)
                                   (match-string 1 text))))
                             segments))))
         ;; build alist id→name (prompt when needed)
         (mapping (or speaker-alist
                      (mapcar (lambda (id)
                                (cons id (read-string (format "Name for SPEAKER_%s: " id))))
                              ids)))
         (prev-id nil)
         (cleaned-segments
          (mapcar
           (lambda (seg)
             (let* ((raw-text (plist-get seg :text))
                    (cur-id  (when (string-match tlon-dub--diarized-speaker-regex raw-text)
                               (match-string 1 raw-text)))
                    ;; strip WhisperX prefix if present
                    (clean-text (replace-regexp-in-string
                                 tlon-dub--diarized-speaker-regex "" raw-text 1 t)))
               ;; prepend human name when speaker changed
               (when (and cur-id (not (equal cur-id prev-id)))
                 (setq clean-text
                       (concat (cdr (assoc cur-id mapping)) ": "
                               (string-trim-left clean-text))))
               (setq prev-id (or cur-id prev-id))
               (list :start (plist-get seg :start)
                     :end   (plist-get seg :end)
                     :text  clean-text)))
           segments))
         (out-file (concat (file-name-sans-extension diarized-file) "-cleaned.srt")))
    (tlon-dub--write-srt-file cleaned-segments out-file)
    (message "Cleaned SRT saved to %s" out-file)
    out-file))

(defun tlon-dub--ends-sentence-p (text)
  "Return non-nil when TEXT ends with obvious sentence punctuation."
  (string-match-p "[\\.\\!\\?…]\\s-*\\'" (string-trim text)))

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
	 (tlon-get-language-code-from-name (tlon-read-language nil "Source language: " t nil))
	 (tlon-get-language-code-from-name (tlon-read-language nil "Target language: " t nil))
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
	 (tlon-get-language-code-from-name (tlon-read-language nil "Language code for transcript: " t nil))))
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
FORMAT overrides `tlon-dub-transcription-format'."
  (interactive (list (read-file-name "Audio/Video file to transcribe: ")))
  (tlon-whisperx-transcribe audio-file (or format tlon-dub-transcription-format)))

;;;###autoload
(defun tlon-dub-diarize-with-whisperx (audio-file &optional language speakers)
  "Diarize AUDIO-FILE with whisperx.
Optional LANGUAGE defaults to \"en\".  If SPEAKERS > 0, whisperx is instructed
to use exactly that many speakers."
  (interactive
   (let* ((file (read-file-name "Audio/Video file to diarize: "))
          (n    (read-number "Number of speakers (0 = auto-detect): " 0)))
     (list file nil n)))
  (tlon-whisperx-diarize audio-file (or language "en") speakers))






(defun tlon-dub--write-srt-file (segments file)
  "Write SRT SEGMENTS to FILE.
Each segment in SEGMENTS should be a plist with :start, :end, and :text keys."
  (with-temp-buffer
    (let ((segment-num 1))
      (dolist (segment segments)
        ;; Write segment number
        (insert (format "%d\n" segment-num))
        ;; Write timestamp
        (insert (format "%s --> %s\n"
                        (plist-get segment :start)
                        (plist-get segment :end)))
        ;; Write text
        (insert (plist-get segment :text))
        ;; Add blank line between segments
        (insert "\n\n")
        (setq segment-num (1+ segment-num))))
    (write-file file)))

;;;;; SRT to CSV Conversion

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
  (let ((english-segments (tlon-dub--parse-srt english-srt-file))
	(translated-segments (tlon-dub--parse-srt translated-srt-file)))
    (tlon-dub--validate-srt-segments
     english-segments translated-segments
     english-srt-file translated-srt-file)
    (let ((csv-lines (list tlon-dub-csv-header))
	  (output-path (concat (file-name-sans-extension english-srt-file) ".csv"))
          (previous-speaker ""))
      (dotimes (i (length english-segments))
        (let* ((eng-seg          (nth i english-segments))
               (trans-seg        (nth i translated-segments))
               (eng-start-time   (plist-get eng-seg  :start))
               (eng-end-time     (plist-get eng-seg  :end))
               (trans-start-time (plist-get trans-seg :start))
               (trans-end-time   (plist-get trans-seg :end))
               (eng-text-raw     (plist-get eng-seg  :text))
               (trans-text-raw   (plist-get trans-seg :text))
               (speaker-prefix   (or (tlon-dub--get-speaker eng-text-raw)
                                     (tlon-dub--get-speaker trans-text-raw)))
               (speaker          (when speaker-prefix
                                   (string-trim (substring speaker-prefix 0 -1))))
               (eng-text-clean   (tlon-dub--clean-segment-text eng-text-raw))
               (trans-text-clean (tlon-dub--clean-segment-text trans-text-raw)))
          (setq speaker (or speaker previous-speaker))
          (tlon-dub--ensure-matching-timestamps eng-start-time eng-end-time trans-start-time trans-end-time i)
          (push (tlon-dub--make-csv-line
                 speaker eng-start-time eng-end-time
                 eng-text-clean trans-text-clean)
                csv-lines)
          (unless (string-empty-p speaker)
            (setq previous-speaker speaker))))
      (write-region (string-join (nreverse csv-lines) "\n") nil output-path nil 'silent)
      (message "CSV file created at %s" output-path)
      output-path)))

(defun tlon-dub--validate-srt-segments (en-segs tr-segs en-file tr-file)
  "Signal user errors when either SRT list is nil or their lengths differ.
EN-SEGS and TR-SEGS are the parsed segment lists.
EN-FILE and TR-FILE are the originating filenames, used only in
the error messages."
  (unless en-segs
    (user-error "Could not parse English SRT file, or it is empty: %s" en-file))
  (unless tr-segs
    (user-error "Could not parse translated SRT file, or it is empty: %s" tr-file))
  (unless (= (length en-segs) (length tr-segs))
    (user-error "Mismatch in number of segments: English SRT has %d, Translated SRT has %d"
                (length en-segs) (length tr-segs))))

(defun tlon-dub--ensure-matching-timestamps (en-start en-end tr-start tr-end idx)
  "Ensure timestamps in segment IDX match between English and translation.
EN-START isthe start timestamp in the English segment. EN-END is the end
timestamp. TR-START is the start timestamp in the translated segment. TR-END is
the end timestamp."
  (unless (and (string= en-start tr-start) (string= en-end tr-end))
    (user-error "Timestamp mismatch in segment %d:\nEnglish: %s → %s\nTranslated: %s → %s"
                (1+ idx) en-start en-end tr-start tr-end)))

(defun tlon-dub--make-csv-line (speaker start end en-text tr-text)
  "Return a CSV line for ElevenLabs.
SPEAKER, START, END, EN-TEXT and TR-TEXT are inserted after proper escaping."
  (format "%s,%s,%s,%s,%s"
          (tlon-dub--csv-escape-string (or speaker ""))
          (tlon-dub--csv-escape-string start)
          (tlon-dub--csv-escape-string end)
          (tlon-dub--csv-escape-string en-text)
          (tlon-dub--csv-escape-string tr-text)))

(defun tlon-dub--parse-srt (file)
  "Parse SRT FILE and return a list of segments.
Each segment is a plist with :start, :end, and :text keys.
Handles optional segment numbers, optional paragraph numbers preceding
the timestamp, and CR/LF line endings. Returns =nil= if parsing fails or
file is empty."
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
		    ;; Skip optional segment number line (first numeric line)
		    (when (and (not (eobp)) (looking-at "^[0-9]+\\s-*$"))
		      (forward-line 1))
		    ;; Expect timestamp line (allow optional leading whitespace)
		    (if (looking-at tlon-dub--srt-timestamp-line)
			(progn
			  (setq start-time (match-string 1)
				end-time (match-string 2))
			  (forward-line 1)
			  ;; Collect remaining lines as text
			  (while (not (eobp))
			    (push (buffer-substring-no-properties (line-beginning-position) (line-end-position))
				  text-lines)
			    (forward-line 1))
			  (when text-lines
			    (push (list :start start-time
					:end end-time
					:text (string-join (nreverse text-lines) "\n"))
				  segments)))
		      (warn "Could not parse SRT block in %s: %s" file block))))))
	    (nreverse segments))))
    (user-error (progn (message "Error parsing SRT file %s: %s" file err) nil))))

(defun tlon-dub--clean-segment-text (text)
  "Remove speaker prefix and line-breaks from TEXT, then collapse spaces."
  (let ((case-fold-search nil))
    (setq text (replace-regexp-in-string tlon-dub--speaker-prefix-regex "" text 1 t)))
  (setq text (replace-regexp-in-string "[[:space:]\n\r]+" " " (string-trim text)))
  text)

(defun tlon-dub--csv-escape-string (str)
  "Escape STR for CSV by doubling quotes and enclosing in quotes."
  (format "\"%s\"" (replace-regexp-in-string "\"" "\"\"" str)))


;;;;; video splitting

;;;###autoload
(defun tlon-dub-split-video-at-timestamps (video-file timestamps-file &optional quick output-dir)
  "Split VIDEO-FILE using markers from TIMESTAMPS-FILE (one HH:MM:SS.mmm per line).
Each part starts at its timestamp and ends 1 ms before the next timestamp, or at
the end of the video for the last part. Parts are saved as
<BASENAME>-partN.<ext> (where <ext> is the original file extension) in
OUTPUT-DIR, defaulting to the directory of VIDEO-FILE. If QUICK is non-nil, do
not re-encode the video, just copy it. This generates the files much more
quickly but it can only cut at keyframes, so the video boundaries may not
exactly coincide with those specified in the timestamp file."
  (interactive
   (list (read-file-name "Video file: " nil nil t)
	 (read-file-name "Timestamps file: " nil nil t)
	 current-prefix-arg
	 nil))
  (setq video-file (expand-file-name video-file)
        timestamps-file (expand-file-name timestamps-file))
  (let* ((output-dir (or output-dir (file-name-directory video-file)))
         (base (file-name-base video-file))
         (timestamps (with-temp-buffer
                       (insert-file-contents timestamps-file)
                       (split-string (buffer-string) "[\n\r]+" t "[ \t]+")))
         (duration (tlon-dub--get-video-duration video-file)))
    (tlon-dub--validate-timestamps timestamps timestamps-file)
    (let ((final-part (tlon-dub--process-all-segments video-file base output-dir timestamps duration quick)))
      (message "Finished splitting video into %d parts" (1- final-part)))))

;;;###autoload
(defun tlon-dub-split-video-at-timestamps-quick ()
  "Call `tlon-dub-split-video-at-timestamps' with QUICK argument."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'tlon-dub-split-video-at-timestamps)))

(defun tlon-dub--validate-timestamps (timestamps timestamps-file)
  "Validate that TIMESTAMPS are in HH:MM:SS.mmm format from TIMESTAMPS-FILE."
  (unless timestamps
    (user-error "No timestamps found in %s" timestamps-file))
  (dolist (ts timestamps)
    (unless (tlon-dub--dot-timestamp-to-seconds ts)
      (user-error "Invalid timestamp \"%s\" in %s. Expected format HH:MM:SS.mmm"
                  ts timestamps-file))))

(defun tlon-dub--split-video-segment (video-file base output-dir timestamps duration i part &optional quick)
  "Split a single video segment and return the next part number.
VIDEO-FILE is the path to the input video file to split. BASE is the base name
for the output files (without extension). OUTPUT-DIR is the directory where the
split video segments will be saved. TIMESTAMPS is a list of timestamp strings
marking segment boundaries. DURATION is the total duration of the video in
seconds. I is the index of the current timestamp in the TIMESTAMPS list. PART is
the current part number for naming the output file. If QUICK is non-nil, do not
re-encode the video, just copy it. This generates the files much more quickly
but it can only cut at keyframes, so the video boundaries may not exactly
coincide with those specified in the timestamp file."
  (let* ((start (nth i timestamps))
         (end-secs (if (< i (1- (length timestamps)))
                       (- (tlon-dub--dot-timestamp-to-seconds
                           (nth (1+ i) timestamps))
			  0.040)
                     duration))
         (end-str (when end-secs (tlon-dub--seconds-to-dot-timestamp end-secs)))
         (input-ext (downcase (or (file-name-extension video-file) "mp4")))
         (outfile (expand-file-name (format "%s-part%02d.%s" base part input-ext) output-dir))
         (args (append '("-y" "-i")
		       (list video-file)
		       '("-ss")
		       (list start)
                       (when end-str (list "-to" end-str))
                       (when quick '("-c" "copy"))
		       (list outfile))))
    (message "Creating %s..." (file-name-nondirectory outfile))
    (unless (= 0 (apply #'call-process "ffmpeg" nil "*tlon-dub-ffmpeg*" t args))
      (error "Command `ffmpeg' failed to create %s" outfile))
    (1+ part)))

(defun tlon-dub--process-all-segments (video-file base output-dir timestamps duration &optional quick)
  "Process all video segments and return the final part number.
VIDEO-FILE is the path to the input video file to be processed. BASE is the base
name used for naming output segments. OUTPUT-DIR is the directory where
processed segments will be saved. TIMESTAMPS is a list of timestamp values
defining segment boundaries. DURATION is the total duration of the video file.
If QUICK is non-nil, do not re-encode the video, just copy it. This generates
the files much more quickly but it can only cut at keyframes, so the video
boundaries may not exactly coincide with those specified in the timestamp file."
  (let ((part 1))
    (dotimes (i (length timestamps))
      (setq part (tlon-dub--split-video-segment video-file base output-dir timestamps duration i part quick)))
    part))

(defun tlon-dub--dot-timestamp-to-seconds (ts)
  "Convert TS in \"H:MM:SS.mmm\" format to float seconds."
  (when (string-match
         "\\`\\([0-9]+\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\.\\([0-9][0-9][0-9]\\)\\'" ts)
    (+ (* 3600 (string-to-number (match-string 1 ts)))
       (* 60   (string-to-number (match-string 2 ts)))
       (string-to-number (match-string 3 ts))
       (/ (string-to-number (match-string 4 ts)) 1000.0))))

(defun tlon-dub--seconds-to-dot-timestamp (secs)
  "Convert SECS (float) to \"HH:MM:SS.mmm\"."
  (let* ((total (floor secs))
         (ms (round (* (- secs total) 1000)))
         (h (/ total 3600))
         (m (/ (% total 3600) 60))
         (s (% total 60)))
    (format "%02d:%02d:%02d.%03d" h m s ms)))

(defun tlon-dub--get-video-duration (file)
  "Return FILE duration in seconds as float using ffprobe, or nil."
  (let ((cmd '("ffprobe" "-v" "error" "-show_entries" "format=duration"
               "-of" "default=noprint_wrappers=1:nokey=1")))
    (with-temp-buffer
      (if (= 0 (apply #'call-process (car cmd) nil (current-buffer) nil
                      (append (cdr cmd) (list file))))
          (let ((out (string-trim (buffer-string))))
            (when (string-match "\\`[0-9.]+\\'" out)
              (string-to-number out)))
        nil))))

;;;;; audio extraction

;;;###autoload
(defun tlon-dub-extract-audio-from-parts (video-file)
  "Extract audio (wav) from all <base>-partN video files derived from VIDEO-FILE.
The command prompts for the master VIDEO-FILE, derives its base name, looks for
all files named <base>-partN.<ext> in the same directory (where <ext> is any
extension, typically mp4), and runs

  ffmpeg -i \"<base>-partN.<ext>\" -vn -c:a copy \"<base>-partN.wav\"

for each matching part.  Returns a list of generated WAV filenames or signals
an error if no part files are found."
  (interactive (list (read-file-name "Master video file: " nil nil t)))
  (let* ((video-file (expand-file-name video-file))
         (dir        (file-name-directory video-file))
         (base       (file-name-base video-file))
         (regex      (format "^%s-part[0-9]+\\.[A-Za-z0-9]+$" (regexp-quote base)))
         (part-files (directory-files dir t regex))
         (created    '()))
    (unless part-files
      (user-error "No part files matching %s found in %s" regex dir))
    (dolist (part part-files)
      (let* ((out (concat (file-name-sans-extension part) ".wav"))
             (args (list "-y" "-i" part "-vn" "-c:a" "copy" out)))
        (message "Extracting audio to %s..." (file-name-nondirectory out))
        (unless (= 0 (apply #'call-process "ffmpeg" nil "*tlon-dub-ffmpeg*" t args))
          (error "Application `ffmpeg' failed to create %s" out))
        (push out created)))
    (message "Created %d wav files in %s" (length created) dir)
    (nreverse created)))

;;;###autoload
(defun tlon-dub-extract-audio (video-file &optional output-file)
  "Extract the audio track from VIDEO-FILE and save it as OUTPUT-FILE.

If OUTPUT-FILE is nil, a WAV file with the same basename as VIDEO-FILE is
created in the same directory. Uses ffmpeg with uncompressed pcm_s16le
\\=(-vn -acodec pcm_s16le).  Returns the pathname of the created file."
  (interactive (list (read-file-name "Video file: " nil nil t)))
  (setq video-file (expand-file-name video-file))
  (unless output-file
    (setq output-file (concat (file-name-sans-extension video-file) ".wav")))
  (let ((args (list "-y" "-i" video-file "-vn" "-acodec" "pcm_s16le" output-file)))
    (message "Extracting audio to %s..." (file-name-nondirectory output-file))
    (unless (= 0 (apply #'call-process "ffmpeg" nil "*tlon-dub-ffmpeg*" t args))
      (error "The program `ffmpeg' failed to create %s" output-file))
    (message "Saved WAV audio to %s" output-file)
    output-file))

;;;;; audio replacement

;;;###autoload
(defun tlon-dub-replace-audio (video-file audio-file &optional output-file)
  "Replace the audio track in VIDEO-FILE with AUDIO-FILE and write OUTPUT-FILE.
VIDEO-FILE and AUDIO-FILE must be readable paths. If OUTPUT-FILE is nil,
create <BASE>-replaced.<EXT> next to VIDEO-FILE, where <BASE> and <EXT> come
from VIDEO-FILE. The video stream is copied without re-encoding; the audio
is re-encoded as AAC for broad compatibility. Return OUTPUT-FILE or signal
an error if ffmpeg fails."
  (interactive (list (read-file-name "Video file: " nil nil t)
                     (read-file-name "Audio file: " nil nil t)
                     nil))
  (setq video-file (expand-file-name video-file))
  (setq audio-file (expand-file-name audio-file))
  (unless (file-readable-p video-file)
    (user-error "Cannot read video file: %s" video-file))
  (unless (file-readable-p audio-file)
    (user-error "Cannot read audio file: %s" audio-file))
  (let* ((ext (or (file-name-extension video-file) "mp4"))
         (out (or output-file
                  (expand-file-name
                   (format "%s-replaced.%s"
                           (file-name-sans-extension video-file) ext)))))
    (message "Replacing audio in %s using %s..."
             (file-name-nondirectory video-file)
             (file-name-nondirectory audio-file))
    (let ((args (list "-y" "-i" video-file "-i" audio-file
                      "-map" "0:v:0" "-map" "1:a:0"
                      "-c:v" "copy" "-c:a" "aac"
                      "-shortest" out)))
      (unless (= 0 (apply #'call-process "ffmpeg" nil "*tlon-dub-ffmpeg*" t args))
        (error "The program `ffmpeg' failed to produce %s" out))
      (message "Saved video with replaced audio to %s" out)
      out)))

;;;;; audio splitting

;;;###autoload
(defun tlon-dub-split-audio-at-timestamps (audio-file timestamps-file &optional output-dir)
  "Split AUDIO-FILE using markers from TIMESTAMPS-FILE (one HH:MM:SS.mmm per line).
Each part starts at its timestamp and ends 1 ms before the next timestamp, or at
the end of the audio for the last part. Parts are saved as
<BASENAME>-partN.<ext> in OUTPUT-DIR, defaulting to the directory of AUDIO-FILE."
  (interactive
   (list (read-file-name "Audio file: " nil nil t)
         (read-file-name "Timestamps file: " nil nil t)
         nil))
  (setq audio-file (expand-file-name audio-file)
        timestamps-file (expand-file-name timestamps-file))
  (let* ((output-dir (or output-dir (file-name-directory audio-file)))
         (base       (file-name-base audio-file))
         (ext        (file-name-extension audio-file))
         (timestamps (with-temp-buffer
                       (insert-file-contents timestamps-file)
                       (split-string (buffer-string) "[\n\r]+" t "[ \t]+")))
         (duration   (tlon-dub--get-video-duration audio-file))) ; works for audio too
    (tlon-dub--validate-timestamps timestamps timestamps-file)
    (let ((part 1))
      (dotimes (i (length timestamps))
        (let* ((start (nth i timestamps))
               (end-secs (if (< i (1- (length timestamps)))
                             (- (tlon-dub--dot-timestamp-to-seconds
                                 (nth (1+ i) timestamps))
                                0.040)
                           duration))
               (end-str (when end-secs (tlon-dub--seconds-to-dot-timestamp end-secs)))
               (outfile (expand-file-name
                         (format "%s-part%02d.%s" base part ext)
                         output-dir))
               (args (append '("-y" "-i")
                             (list audio-file)
                             '("-ss") (list start)
                             (when end-str (list "-to" end-str))
                             '("-c" "copy")
                             (list outfile))))
          (message "Creating %s..." (file-name-nondirectory outfile))
          (unless (= 0 (apply #'call-process "ffmpeg" nil "*tlon-dub-ffmpeg*" t args))
            (error "Command `ffmpeg` failed to create %s" outfile))
          (setq part (1+ part))))
      (message "Finished splitting audio into %d parts" (1- part)))))

;;;###autoload
(defun tlon-dub-join-audio-files (list-file &optional output-file)
  "Join audio files whose paths are listed in LIST-FILE into OUTPUT-FILE.
This is a thin wrapper around `tlon-dub-join-files`."
  (interactive (list (read-file-name "List of audio files: " nil nil t ".txt")))
  (tlon-dub-join-files list-file output-file))

;;;###autoload
(defun tlon-dub-join-video-files (list-file &optional output-file)
  "Join video files whose paths are listed in LIST-FILE into OUTPUT-FILE.
This is a convenience wrapper around `tlon-dub-join-files` intended for video
segments created by `tlon-dub-split-video-at-timestamps`."
  (interactive (list (read-file-name "List of video files: " nil nil t ".txt")))
  (tlon-dub-join-files list-file output-file))

;;;;; video merging

;;;###autoload
(defun tlon-dub-join-files (list-file &optional output-file)
  "Join files whose paths are listed line-by-line in LIST-FILE into OUTPUT-FILE.
If every listed file has a recognised media extension (e.g. mp4, webm, mp3,
wav) the concatenation is performed with ffmpeg so the resulting container has
correct metadata.  Otherwise the files are treated as plain text and their
contents concatenated with a single newline inserted between them.

LIST-FILE is a text file containing one file path per line.  Paths may be
absolute or relative to the directory of LIST-FILE.  Blank lines and lines
starting with \"#\" are ignored.

Interactively, OUTPUT-FILE is prompted for and defaults to the same directory
as LIST-FILE with the basename \"joined\" and the same extension as the first
valid path listed (or \"txt\" if that cannot be determined).

Signals an error if any listed file cannot be read or if ffmpeg fails.  Returns
the pathname of OUTPUT-FILE."
  (interactive (list (read-file-name "List of files: " nil nil t ".txt")))
  ;; Expand and collect the file paths
  (setq list-file (expand-file-name list-file))
  (let* ((dir (file-name-directory list-file))
         (paths (with-temp-buffer
                  (insert-file-contents list-file)
                  (split-string (buffer-string) "[\n\r]+" t "[ \t]+")))
         (clean-paths
          (delq nil
                (mapcar (lambda (p)
                          (setq p (string-trim p))
                          (unless (or (string-empty-p p)
                                      (string-prefix-p "#" p))
                            (expand-file-name p dir)))
                        paths))))
    (unless clean-paths
      (user-error "No valid file paths found in %s" list-file))
    ;; Determine default output name
    (unless output-file
      (let* ((first-ext (file-name-extension (car clean-paths) t))
             (default-name (concat (file-name-sans-extension list-file)
                                   "-joined"
                                   (or first-ext ".txt"))))
        (setq output-file (read-file-name "Output file: " dir default-name))))
    (setq output-file (expand-file-name output-file))
    ;; Decide whether to use ffmpeg
    (let* ((media-exts '("mp4" "mov" "mkv" "webm" "mp3" "wav" "flac" "ogg" "opus" "m4a"))
           (first-ext (downcase (file-name-extension (car clean-paths))))
           (use-ffmpeg (and first-ext (member first-ext media-exts)
                            ;; all files must share the same extension
                            (cl-every (lambda (f)
                                        (string= first-ext (downcase (file-name-extension f))))
                                      clean-paths))))
      (if use-ffmpeg
          ;; concatenate with ffmpeg concat demuxer
          (let ((tmp-list (make-temp-file "tlon-dub-concat" nil ".txt")))
            (unwind-protect
                (progn
                  ;; build list file
                  (with-temp-buffer
                    (dolist (f clean-paths)
                      (insert "file '" (replace-regexp-in-string "'" "'\\''" f) "'\n"))
                    (write-region (point-min) (point-max) tmp-list nil 'silent))
                  ;; run ffmpeg
                  (let ((args (list "-y" "-f" "concat" "-safe" "0" "-i" tmp-list "-c" "copy" output-file)))
                    (message "Concatenating %d media files with ffmpeg..." (length clean-paths))
                    (unless (= 0 (apply #'call-process "ffmpeg" nil "*tlon-dub-ffmpeg*" t args))
                      (error "The program `ffmpeg' failed to concatenate files"))))
              (delete-file tmp-list)))
        ;; fallback to plain-text concatenation
        (with-temp-buffer
          (dolist (file clean-paths)
            (unless (file-readable-p file)
              (user-error "Cannot read %s" file))
            (insert-file-contents file)
            ;; Ensure a trailing newline except for last file
            (goto-char (point-max))
            (unless (bolp) (insert "\n")))
          (write-region (point-min) (point-max) output-file nil 'silent))))
    (message "Joined %d files into %s" (length clean-paths) output-file)
    output-file))

;;;; Menu

(transient-define-infix tlon-dub-infix-select-transcription-format ()
  "Default WhisperX transcription format."
  :class 'transient-lisp-variable
  :variable 'tlon-dub-transcription-format
  :reader (lambda (prompt _ _)
            (tlon-transient-read-string-choice
             prompt '("all" "srt" "vtt" "txt" "tsv" "json" "aud")))
  :prompt "Transcription format: ")



;;;###autoload (autoload 'tlon-dub-menu "tlon-dub" nil t)
(transient-define-prefix tlon-dub-menu ()
  "Menu for Tlön Dubbing (`tlon-dub`) functionality."
  :info-manual "(tlon) Dubbing"
  [["Transcription & Timestamps (srt)"
    ("t t" "Transcribe with WhisperX (Audio -> srt)" tlon-dub-transcribe-with-whisperx)
    ("t i" "Diarize with WhisperX (Audio -> diarized srt/txt)" tlon-dub-diarize-with-whisperx)
    ("t x" "Clean diarized SRT (→ -cleaned.srt)" tlon-dub-clean-diarized-srt)
    ;; ("m" "Propagate Machine Timestamps (srt + en.md -> en.srt)" tlon-dub-propagate-machine-timestamps)
    ;; ("e" "Propagate English Timestamps (en.srt + lang.md -> lang.srt)" tlon-dub-propagate-english-timestamps)
    ;; ("a" "Align Punctuation (txt + md -> aligned.md)" tlon-dub-align-punctuation)
    ;; ("o" "Optimize Translation Length (en.srt + lang.srt)" tlon-dub-optimize-translation-length)
    ("t c" "Convert SRTs to CSV (en.srt + lang.srt -> .csv)" tlon-dub-convert-srt-to-csv)
    ("t r" "Resegment SRT (speaker/min-30s)" tlon-dub-resegment-srt)
    ""
    "Options"
    ("t -f" "Transcription format" tlon-dub-infix-select-transcription-format)]
   ["Audio & video manipulation"
    "Split"
    ("s v" "Split video by timestamps (quick, imprecise, lossless)" tlon-dub-split-video-at-timestamps-quick)
    ("s V" "Split video by timestamps (slow, precise, lossy)" tlon-dub-split-video-at-timestamps)
    ("s a" "Split audio by timestamps" tlon-dub-split-audio-at-timestamps)
    ""
    "Join"
    ("j v" "Join video files from list" tlon-dub-join-video-files)
    ("j a" "Join audio files from list" tlon-dub-join-audio-files)
    ""
    "Extract"
    ("e a" "Extract audio from video (video -> wav)" tlon-dub-extract-audio)
    ("e A" "Extract audio from video parts (parts -> wavs)" tlon-dub-extract-audio-from-parts)
    ""
    "Replace"
    ("r a" "Replace video audio (video + audio -> new video)" tlon-dub-replace-audio)]
   ["ElevenLabs API"
    ("a s" "Start New Dubbing Project" tlon-dub-start-project)
    ("a d" "Get Project Metadata" tlon-dub-get-project-metadata)
    ("a g" "Get Dubbing Transcript (VTT)" tlon-dub-get-dubbing)
    ("a r" "Get Resource Data" tlon-dub-get-resource-data)
    ("a k" "Add Speaker Segment" tlon-dub-add-speaker-segment)]])

(provide 'tlon-dub)
;;; tlon-dub.el ends here
