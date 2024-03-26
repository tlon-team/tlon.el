;;; tlon-babel-tts.el --- Text-to-speech functionality -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon-babel
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

(require 'markdown-mode)
(require 'tlon-babel-core)
(require 'unfill)

;;;; User options

(defgroup tlon-babel-tts ()
  "Text-to-speech functionality."
  :group 'tlon-babel)

(defcustom tlon-babel-azure-audio-settings
  "audio-24khz-160kbitrate-mono-mp3"
  "Settings for the Azure text-to-speech service.
Here's a description of the main options:

- `audio-24khz-160kbitrate-mono-mp3': Offers higher quality due to a higher
  bitrate and sample rate. This means the audio will sound clearer, especially
  for more complex sounds or music. However, the file size will also be larger.

- `audio-16khz-64kbitrate-mono-mp3': Reduces the bitrate, which will result in a
  smaller file at the cost of lower audio quality. Useful when network bandwidth
  or storage is limited.

- `raw-16khz-16bit-mono-pcm': Provides raw audio data without compression. This
  is useful if you plan to further process the audio yourself or need lossless
  quality. Note that the files will be significantly larger.

- `riff-16khz-16bit-mono-pcm': Similar to the RAW format but wrapped in the
  Waveform Audio File Format, which includes headers making it compatible with
  more playback devices and software.

- `riff-24khz-16bit-mono-pcm': Offers a higher sample rate compared to the 16kHz
  versions, which can provide better audio quality at the expense of larger file
  sizes."
  :type 'string
  :group 'tlon-babel-tts)

;;;; Variables

(defvar tlon-babel-azure-voices
  '((:voice "es-US-AlonsoNeural" :language "es" :gender "male")
    (:voice "es-US-PalomaNeural" :language "es" :gender "female"))
  "Preferred Azure voices for different languages.
All the voices in this property list are neural and multilingual, and are the
best male and female voices we were able to identify in each language.")

(defvar tlon-babel-azure-key nil
  "Azure subscription key for the text-to-speech service.")

(defconst tlon-babel-azure-char-limit (* 9 60 14)
  "Maximum number of characters that Azure can process per request.
Azure can process up to 10 minutes of audio at a time. This estimate assumes 14
characters per second, and uses nine minutes.")

;;;;; Citations

;; Develop multilanguage support
(defconst tlon-babel-tts-standard-abbreviations
  '(("a\\. de C\\." . "antes de Cristo")
    ("ca\\." . "alrededor de")
    ("d\\. de C\\." . "después de Cristo")
    ("e\\. g\\." . "por ejemplo")
    ("EE\\. UU\\." . "Estados Unidos")
    ("et al\\." . "y otros")
    ("etc\\." . "etcétera")
    ("i\\.e\\." . "esto es")
    ("N\\.B\\." . "obsérvese bien")
    ("p\\. ej\\." . "por ejemplo")
    ("vs\\." . "versus"))
  "Standard Spanish abbreviations and their spoken equivalent.")

;;;;; Currencies

(defconst tlon-babel-tts-currencies
  '(("$" . "dólar")
    ("₿" . "bitcoin")
    ("¢" . "centavo")
    ("Ξ" . "éter")
    ("£" . "libra esterlina")
    ("₹" . "rupia")
    ("₹" . "₪ shekel")
    ("¥" . "yen"))
  "Currencies and their spoken equivalent.")

;;;;; Listener cues

(defconst tlon-babel-tts-note-begins
  '(("en" . "\n\nA note starts here. ")
    ("es" . "\n\nAquí empieza una nota. "))
  "Text to insert at the beginning of a note.")

(defconst tlon-babel-tts-note-ends
  '(("en" . " The note ends here.\n\n")
    ("es" . " Aquí termina la nota.\n\n"))
  "Text to insert at the end of a note.")

;;;; Functions

(defun tlon-babel-get-or-set-azure-key ()
  "Get or set the Azure key."
  (or tlon-babel-azure-key
      (setq tlon-babel-azure-key
	    (auth-source-pass-get "key1" "tlon/core/live.com/tlon.shared@gmail.com [Azure]"))))

(defun tlon-babel-read-file (file)
  ""
  (let ((file-name (file-name-sans-extension file))
	(tlon-babel-in-text-abbreviations)
	article chunks)
    (with-current-buffer (find-file-noselect file)
      ;; to make `tlon-babel-tts-process-in-text-abbreviations' work, we
      ;; let-bound the variable above and now set its value to that of its
      ;; file-local counterpart
      (setq tlon-babel-in-text-abbreviations tlon-babel-in-text-abbreviations)
      (let ((begin (or (cdr (tlon-babel-get-delimited-region-pos
			     tlon-babel-yaml-delimiter))
		       (point-min)))
	    (end (or (string-match tlon-babel-md-local-variables-line-start (buffer-string))
		     (point-max))))
	(setq article (buffer-substring-no-properties begin end))))
    (unless (string= (file-name-extension file) "md")
      (user-error "File `%s' is not a Markdown file" file-name))
    (with-temp-buffer
      ;; exclude metadata and local vars
      (insert-file-contents article)
      (tlon-babel-tts-prepare-buffer)
      ;; TODO: create a preview from the prepared buffer (to remove links etc)
      (buffer-string)
      (setq chunks (tlon-babel-break-into-chunks))
      (kill-buffer))
    chunks))

;;;;; Cleanup

(defun tlon-babel-tts-prepare-buffer ()
  "Prepare the current buffer for audio narration."
  (tlon-babel-tts-position-notes)
  (tlon-babel-tts-process-bibtex-keys)
  (tlon-babel-tts-process-currencies)
  ;; handle text formatting: replace e.g. italics with SSML
  ;; replace small caps
  ;; check all other elements in markdown-menu
  ;; quotes
  (tlon-babel-tts-process-standard-abbreviations)
  (tlon-babel-tts-process-in-text-abbreviations)
  ;; images
  )

;;;;;; Formatting

;; This has to be done in the Markdown file itself, because some italics are not
;; for emphasis. Prospectively, we should revise the MD function to insert
;; italics to also prompt the user if they want to insert emphasis.
;; Retrospectively, we should revise all existing MD files and add emphasis tags
;; where appropriate.
(defun tlon-babel-handle-italics ()
  "Search for text in italics and prompt user to add enclosing `emphasis' tag."
  (interactive)
  (query-replace-regexp markdown-regex-italic " <emphasis>\\3</emphasis>" nil))


;;;;;; Notes

(defun tlon-babel-tts-position-notes ()
  "Replace citation with its note if sidenote, else delete it.
Move sidenote to the end of the sentence if necessary.

Note: the function assumes that the citation is in MDX, rather than Pandoc
citation key, format. Hence, it must be run *before*
`tlon-babel-tts-process-bibtex-keys'."
  (goto-char (point-min))
  (while (re-search-forward markdown-regex-footnote nil t)
    (let (reposition)
      (markdown-footnote-kill)
      (unless (looking-back (concat "\\.\\|" markdown-regex-footnote) (line-beginning-position))
	(setq reposition t))
      (when (eq (tlon-babel-get-note-type (current-kill 0)) 'sidenote)
	(when reposition
	  (forward-sentence))
	(insert (tlon-babel-tts-handle-note (string-trim (current-kill 0))))))))

(defun tlon-babel-tts-handle-note (note)
  "Handle NOTE for audio narration."
  (let ((language (tlon-babel-repo-lookup :language :dir (tlon-babel-get-repo))))
    (with-temp-buffer
      (insert note)
      (goto-char (point-min))
      (insert (alist-get language tlon-babel-tts-note-begins nil nil #'string=))
      (while (re-search-forward tlon-babel-sidenote-marker nil t)
	(replace-match ""))
      (goto-char (point-max))
      (insert (alist-get language tlon-babel-tts-note-ends nil nil #'string=))
      (buffer-string))))

(defun tlon-babel-count-regexp-occurrences (regexp &optional start end)
  "Count the number of occurrences of REGEXP between START and END.
If START and END are nil, the whole buffer is searched."
  (save-excursion
    (goto-char (or start (point-min)))
    (let ((count 0))
      (while (re-search-forward regexp (or end (point-max)) t)
	(setq count (1+ count)))
      count)))

;;;;;; Citations

(defun tlon-babel-tts-process-bibtex-keys ()
  "Replace our custom MDX cite tags with a pandoc-style citation.
For example `<Cite bibKey={\"Clark2015SonAlsoRises\"} />' will be replaced with
`[@Clark2015SonAlsoRises]'."
  (goto-char (point-min))
  (while (re-search-forward tlon-babel-cite-pattern nil t)
    (replace-match (format "[@%s]"(match-string 1)) nil nil)))

;;;;;; Abbreviations

(defun tlon-babel-tts-process-standard-abbreviations ()
  "Replace standard abbreviations with their spoken equivalent."
  (dolist (abbreviation tlon-babel-tts-standard-abbreviations)
    (goto-char (point-min))
    (while (re-search-forward (car abbreviation) nil t)
      (replace-match (cdr abbreviation) t nil))))

(defun tlon-babel-tts-process-in-text-abbreviations ()
  "Replace in-text abbreviations with their spoken equivalent.
In-text abbreviations are those that are introduced in the text itself,
typically in parenthesis after the first occurrence of the phrase they
abbreviate. We store these abbreviations on a per file basis, in the file-local
variable `tlon-babel-in-text-abbreviations'"
  (let ((case-fold-search nil))
    (dolist (partial tlon-babel-in-text-abbreviations)
      (dolist (full (list (cons
			   (format "%s (%s)" (cdr partial) (car partial))
			   (cdr partial))
			  partial))
	(goto-char (point-min))
	(while (re-search-forward (car full) nil t)
	  (replace-match (cdr full) t nil))))))

;; Things to fix:
;; - year numbers
;; - https://docs.google.com/document/d/1m1k57PbKkkVy0eLwrHKjZiu9XOcOD74WhX4jKKQbwRU/
;; math expressions

;;;;;; Currencies

(defun tlon-babel-tts-process-currencies ()
  "Replace currency symbols with their spoken equivalent."
  (dolist (currency tlon-babel-tts-currencies)
    (goto-char (point-min))
    (while (re-search-forward (format "%s\\([0-9,.]+\\)\\b" (regexp-quote (car currency))) nil t)
      (replace-match (format  "\\1 %s" (cdr currency)) nil nil))))

;;;;; Chunk processing

(defun tlon-babel-break-into-chunks ()
  "Break text in current buffer into chunks.
Each chunk will be at most `tlon-babel-azure-char-limit' words."
  (let ((chunks '())
	(begin 1)
	end)
    (while (not (eobp))
      (goto-char (min
		  (point-max)
		  (+ begin tlon-babel-azure-char-limit)))
      (setq end (if (eobp)
		    (point)
		  (backward-sentence))) ; avoid mid-sentence breaks
      (push (string-trim (buffer-substring-no-properties begin end)) chunks)
      (setq begin end))
    (nreverse chunks)))

(defun tlon-babel-join-audio-files (file)
  "Join chunks of FILE back into a single file."
  (let* ((files (tlon-babel-get-file-chunks file))
	 (list-of-files (tlon-babel-create-ffmpeg-list-of-audio-files files)))
    (shell-command (format "ffmpeg -f concat -safe 0 -i %s -c copy %s"
			   list-of-files file))))

(defun tlon-babel-create-ffmpeg-list-of-audio-files (files)
  "Create a temporary file with a list of audio FILES for use with `ffmpeg'."
  (let ((temp-file-list (make-temp-file "files" nil ".txt")))
    (with-temp-file temp-file-list
      (dolist (file files)
	(insert (format "file '%s'\n" (expand-file-name file)))))
    temp-file-list))

(defun tlon-babel-get-file-chunks (file)
  "Return a list of the file chunks for FILE."
  (let ((nth 1)
	file-chunk
	files)
    (while (file-exists-p (setq file-chunk (tlon-babel-name-chunk file nth)))
      (push file-chunk files)
      (setq nth (1+ nth)))
    (nreverse files)))

(defun tlon-babel-name-chunk (file nth)
  "Return the name of the NTH chunk of FILE."
  (let ((extension (file-name-extension file))
	(file-name-sans-extension (file-name-sans-extension file)))
    (format "%s-%03d.%s" file-name-sans-extension nth extension)))

;;;;; Azure

(defun tlon-babel-azure-narrate-file (&optional file voice)
  "Narrate FILE with VOICE using Azure's text-to-speech service."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (chunks (tlon-babel-read-file file))
	 (repo (tlon-babel-get-repo-from-file file))
	 (language (tlon-babel-repo-lookup :language :dir repo))
	 (voice (or voice (completing-read "Voice: " (tlon-babel-lookup-all
						      tlon-babel-azure-voices :voice
						      :language language))))
	 (file-name-sans-extension (file-name-sans-extension
				    (file-name-nondirectory file)))
	 (file-name (file-name-with-extension file-name-sans-extension "mp3"))
	 (output (file-name-concat repo "audio" file-name))
	 (nth 1))
    (dolist (chunk chunks)
      (tlon-babel-azure-generate-audio
       chunk voice (tlon-babel-name-chunk output nth))
      (setq nth (1+ nth)))))

(defun tlon-babel-azure-generate-audio (text voice output)
  "Generate an audio file from TEXT using VOICE and save it to OUTPUT."
  (let* ((ssml (shell-quote-argument
		(format "<speak version='1.0' xml:lang='en-US'><voice name='%s'>%s</voice></speak>"
			voice text)))
	 (command (format "curl --location --request POST 'https://eastus.tts.speech.microsoft.com/cognitiveservices/v1' \
--header 'Ocp-Apim-Subscription-Key: %s' \
--header 'Content-Type: application/ssml+xml' \
--header 'X-Microsoft-OutputFormat: %s' \
--header 'User-Agent: curl' \
--data-raw %s > '%s'" (tlon-babel-get-or-set-azure-key) tlon-babel-azure-audio-settings ssml output)))
    (async-shell-command command)))

(provide 'tlon-babel-tts)
;;; tlon-babel-tts.el ends here
