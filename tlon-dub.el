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

(defgroup tlon-dub ()
  "Dubbing functionality using ElevenLabs."
  :group 'tlon)

;;;; Constants

(defconst tlon-dub-api-base-url "https://api.elevenlabs.io/v1"
  "Base URL for the ElevenLabs API.")

(defconst tlon-dub-start-project-endpoint "/dubbing"
  "API endpoint for starting a dubbing project.")

(defconst tlon-dub-get-project-metadata-endpoint "/dubbing/%s"
  "API endpoint format for getting dubbing project metadata. Requires dubbing_id.")

(defconst tlon-dub-get-transcript-endpoint "/dubbing/%s/transcript/%s"
  "API endpoint format for getting the transcript for a dubbing project. Requires dubbing_id and language_code.")

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

;;;; Functions

;;;###autoload
(defun tlon-dub-start-project (source-file source-lang target-lang project-name
                               &optional voice-id dubbing-studio num-speakers)
  "Start an ElevenLabs dubbing project for SOURCE-FILE.
SOURCE-FILE is the path to the audio or video file to dub.
SOURCE-LANG is the ISO code of the source language (e.g., \"en\").
TARGET-LANG is the ISO code of the target language (e.g., \"es\").
PROJECT-NAME is a name for the dubbing project.
VOICE-ID is the optional ID of the ElevenLabs voice to use for the dubbing.
If VOICE-ID is nil, ElevenLabs might use a default or clone the original voice.
DUBBING-STUDIO, if non-nil, creates the project in Dubbing Studio mode (adjustable).
NUM-SPEAKERS, if > 0, specifies the expected number of speakers.

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
      (user-error "Unsupported file type for dubbing: %s. Please provide an audio or video file."
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
    (let ((response (shell-command-to-string command)))
      (message "Dubbing project started. Response:\n%s" response)
      ;; Optionally parse the JSON response
      (condition-case err
          (json-parse-string response :object-type 'alist)
        (error (progn
                 (message "Error parsing JSON response: %s" err)
                 response))))))) ; Return raw response on error

;;;###autoload
(defun tlon-dub-get-project-metadata (dubbing-id)
  "Get metadata for the ElevenLabs dubbing project with DUBBING-ID."
  (interactive (list (read-string "Dubbing ID: ")))
  (let* ((api-key (tlon-tts-elevenlabs-get-or-set-key))
         (url (format (concat tlon-dub-api-base-url tlon-dub-get-project-metadata-endpoint)
                      dubbing-id))
         (command (format "curl -s --request GET '%s' \
--header 'accept: application/json' \
--header 'xi-api-key: %s'"
                          url
                          api-key)))
    (message "Getting metadata for dubbing project %s..." dubbing-id)
    (when tlon-debug (message "Debug: Running command: %s" command))
    (let ((response (shell-command-to-string command)))
      (message "Metadata received. Response:\n%s" response)
      ;; Optionally parse the JSON response
      (condition-case err
          (json-parse-string response :object-type 'alist)
        (error (progn
                 (message "Error parsing JSON response: %s" err)
                 response)))))) ; Return raw response on error

;;;###autoload
(defun tlon-dub-get-transcript (dubbing-id language-code)
  "Get the transcript for the ElevenLabs dubbing project DUBBING-ID in LANGUAGE-CODE.
LANGUAGE-CODE should be the ISO code (e.g., \"en\", \"es\") for the desired transcript."
  (interactive
   (list (read-string "Dubbing ID: ")
         (tlon-get-iso-code (tlon-read-language nil "Language code for transcript: " t nil))))
  (let* ((api-key (tlon-tts-elevenlabs-get-or-set-key))
         (url (format (concat tlon-dub-api-base-url tlon-dub-get-transcript-endpoint)
                      dubbing-id language-code))
         ;; Use -L to follow redirects, as this endpoint might return a temporary URL for the transcript file
         (command (format "curl -s -L --request GET '%s' \
--header 'accept: application/json' \
--header 'xi-api-key: %s'"
                          url
                          api-key)))
    (message "Getting transcript for dubbing project %s (language: %s)..." dubbing-id language-code)
    (when tlon-debug (message "Debug: Running command: %s" command))
    (let ((response (shell-command-to-string command)))
      ;; The response is expected to be the transcript file content itself (e.g., VTT or SRT)
      ;; It's not typically JSON, so we don't parse it here.
      (message "Transcript received.")
      ;; Display in a new buffer for inspection
      (with-current-buffer (get-buffer-create (format "*Dub Transcript: %s (%s)*" dubbing-id language-code))
        (erase-buffer)
        (insert response)
        (goto-char (point-min))
        (switch-to-buffer (current-buffer)))
      response))) ; Return the raw transcript string

(provide 'tlon-dub)

;;; tlon-dub.el ends here
