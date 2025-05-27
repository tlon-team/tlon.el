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

(defun tlon-whisperx--start (audio-file args buffer-name callback)
  "Run whisperx on AUDIO-FILE with ARGS, log to BUFFER-NAME, call CALLBACK.
CALLBACK is called as (CALLBACK transcript-path t) on success, or (CALLBACK nil
nil) on failure. The transcript path is <basename>.txt for diarization, or
<basename>.json for transcription."
  (let* ((buffer (get-buffer-create buffer-name))
         (default-directory (file-name-directory (expand-file-name audio-file)))
         (basename (file-name-sans-extension (file-name-nondirectory audio-file)))
         (process-name (format "whisperx-%s-process" basename))
         (cmd (cons "whisperx" args)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "Running: %s\n\n" (string-join cmd " "))))
    (make-process
     :name process-name
     :buffer buffer
     :command cmd
     :sentinel
     (lambda (_proc event)
       (let* ((success (and (string= event "finished\n")
			    (file-exists-p (expand-file-name (concat basename ".txt")))))
	      (transcript-path (when success (expand-file-name (concat basename ".txt")))))
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

(defun tlon-whisperx-diarize (audio-file &optional language hf-token callback)
  "Diarize AUDIO-FILE with whisperx.
LANGUAGE defaults to \"es\". HF-TOKEN is retrieved if nil. CALLBACK is called
as (CALLBACK transcript-path t) on success."
  (let* ((language (or language "es"))
         (hf-token (or hf-token
                       (auth-source-pass-get "whisperX"
                                             (concat "chrome/huggingface.co/" (getenv "PERSONAL_EMAIL")))))
         (args (list (expand-file-name audio-file)
                     "--diarize"
                     "--language" language
                     "--hf_token" hf-token
                     "--compute_type" "float32"
                     "--output_dir" "."))
         (buffer-name "*Diarization Output*"))
    (tlon-whisperx--start audio-file args buffer-name (or callback (lambda (&rest _) nil)))))

(provide 'tlon-whisperx)
;;; tlon-whisperx.el ends here
