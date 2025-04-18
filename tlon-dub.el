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

;;;; Functions

;;;###autoload
(defun tlon-dub-start-project (source-file source-lang target-lang project-name &optional voice-id)
  "Start an ElevenLabs dubbing project for SOURCE-FILE.
SOURCE-FILE is the path to the audio or video file to dub.
SOURCE-LANG is the ISO code of the source language (e.g., \"en\").
TARGET-LANG is the ISO code of the target language (e.g., \"es\").
PROJECT-NAME is a name for the dubbing project.
VOICE-ID is the optional ID of the ElevenLabs voice to use for the dubbing.
If VOICE-ID is nil, ElevenLabs might use a default or clone the original voice.

Returns the JSON response from the API, typically containing the `dubbing_id'."
  (interactive
   (list (read-file-name "Source file: ")
         (tlon-read-language nil "Source language: " t t)
         (tlon-read-language nil "Target language: " t t)
         (read-string "Project name: ")
         (completing-read "Voice ID (optional, press RET for default): "
                          (mapcar (lambda (v) (plist-get v :id)) tlon-elevenlabs-voices)
                          nil nil nil nil ""))) ; Allow empty input for optional voice
  (let* ((api-key (tlon-tts-elevenlabs-get-or-set-key))
         (url (concat tlon-dub-api-base-url tlon-dub-start-project-endpoint))
         ;; Construct the multipart/form-data command
         (command (format "curl -s --request POST '%s' \
--header 'accept: application/json' \
--header 'xi-api-key: %s' \
--form 'mode=\"automatic\"' \
--form 'file=@\"%s\"' \
--form 'name=\"%s\"' \
--form 'source_lang=\"%s\"' \
--form 'target_lang=\"%s\"%s"
                          url
                          api-key
                          source-file
                          project-name
                          source-lang
                          target-lang
                          (if (and voice-id (not (string-empty-p voice-id)))
                              (format " --form 'voice_id=\"%s\"'" voice-id)
                            ""))))
    (message "Starting dubbing project '%s' for %s..." project-name (file-name-nondirectory source-file))
    (when tlon-debug (message "Debug: Running command: %s" command))
    (let ((response (shell-command-to-string command)))
      (message "Dubbing project started. Response:\n%s" response)
      ;; Optionally parse the JSON response
      (condition-case err
          (json-parse-string response :object-type 'alist)
        (error (progn
                 (message "Error parsing JSON response: %s" err)
                 response)))))) ; Return raw response on error

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

(provide 'tlon-dub)

;;; tlon-dub.el ends here
