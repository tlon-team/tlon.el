;;; tlon-whisperx.el --- Shared whisperx helpers for Tlön -*- lexical-binding: t -*-

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

;; Shared helpers for running whisperx for transcription and diarization.

;;; Code:

(require 'tlon-core)
(require 'auth-source)

;;;; User options

(defgroup tlon-whisperx nil
  "Shared helpers for running whisperx."
  :group 'tlon)

(defcustom tlon-dub-transcription-format "all"
  "Output format produced by WhisperX transcription.
Allowed values are \"all\", \"srt\", \"vtt\", \"txt\", \"tsv\", \"json\",
and \"aud\"."
  :type '(choice (const "all") (const "srt") (const "vtt")
                 (const "txt") (const "tsv") (const "json")
                 (const "aud"))
  :group 'tlon-whisperx)

;;;; Functions

(defun tlon-whisperx--start (audio-file args buffer-name callback)
  "Run whisperx on AUDIO-FILE with ARGS, log to BUFFER-NAME, call CALLBACK.
CALLBACK is called as (CALLBACK transcript-path t) on success, or (CALLBACK nil
nil) on failure. The transcript extension is determined from the
--output_format argument in ARGS (defaulting to \"txt\")."
  (let* ((buffer (get-buffer-create buffer-name))
         (default-directory (file-name-directory (expand-file-name audio-file)))
         (basename (file-name-sans-extension (file-name-nondirectory audio-file)))
         (process-name (format "whisperx-%s-process" basename))
         (cmd (cons "whisperx" args))
         ;; Determine expected output extension from --output_format arg
         (fmt-pos (cl-position "--output_format" args :test #'string=))
         (output-ext (if (and fmt-pos (< (1+ fmt-pos) (length args)))
                         (nth (1+ fmt-pos) args)
                       "txt")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((display-cmd (copy-sequence cmd))
	    (pos (cl-position "--hf_token" cmd :test #'string=)))
	(when (and pos (< (1+ pos) (length display-cmd)))
	  (setf (nth (1+ pos) display-cmd) "[REDACTED]"))
	(insert (format "Running: %s\n\n" (string-join display-cmd " "))))
)
    (make-process
     :name process-name
     :buffer buffer
     :command cmd
     :sentinel
     (lambda (_proc event)
       (let* ((expected-file (expand-file-name (concat basename "." output-ext)))
              (success (and (string= event "finished\n")
			    (file-exists-p expected-file)))
	      (transcript-path (when success expected-file)))
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert (format "Process %s event: %s\n" process-name event)))
         (if success
             (progn
               (with-current-buffer buffer
                 (insert (format "Transcript file created: %s\n" transcript-path)))
               (funcall callback transcript-path t))
           (progn
             (with-current-buffer buffer
               (insert "Error: Transcript file not created.\n"))
             (funcall callback nil nil))))))))

(defun tlon-whisperx-transcribe (audio-file &optional format callback)
  "Transcribe AUDIO-FILE with whisperx, optionally specifying FORMAT and CALLBACK.
FORMAT defaults to \"json\". CALLBACK is called as (CALLBACK transcript-path t)
on success."
  (let* ((format (or format "json"))
         (args (list (expand-file-name audio-file)
                     "--compute_type" "float32"
                     "--output_format" format
                     "--output_dir" (file-name-directory (expand-file-name audio-file))))
         (buffer-name (format "*whisperx-%s-output*" (file-name-nondirectory audio-file))))
    (tlon-whisperx--start audio-file args buffer-name (or callback (lambda (&rest _) nil)))))

(defun tlon-whisperx-diarize (audio-file &optional language speakers hf-token callback)
  "Diarize AUDIO-FILE with whisperx.
LANGUAGE defaults to \"es\".  If SPEAKERS is a positive integer, whisperx is
called with “--min_speakers SPEAKERS --max_speakers SPEAKERS”; if nil or 0 the
tool auto-detects speakers.  HF-TOKEN and CALLBACK behave as before."
  (let* ((language (or language "es"))
         (hf-token (or hf-token
                       (auth-source-pass-get "whisperX"
                                             (concat "chrome/huggingface.co/" (getenv "PERSONAL_EMAIL")))))
         (speaker-flags
          (when (and speakers (> speakers 0))
            (list "--min_speakers" (number-to-string speakers)
                  "--max_speakers" (number-to-string speakers))))
         (args (append
                (list (expand-file-name audio-file)
                      "--diarize"
                      "--language" language
                      "--hf_token" hf-token
                      "--compute_type" "float32"
                      "--output_dir" ".")
                speaker-flags))
         (buffer-name "*Diarization Output*"))
    (tlon-whisperx--start audio-file args buffer-name (or callback (lambda (&rest _) nil)))))

(provide 'tlon-whisperx)
;;; tlon-whisperx.el ends here
