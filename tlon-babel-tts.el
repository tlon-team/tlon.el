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
(require 'tlon-babel-md)

;;;; User options

(defgroup tlon-babel-tts ()
  "Text-to-speech functionality."
  :group 'tlon-babel)

(defcustom tlon-babel-azure-audio-settings
  "audio-16khz-64kbitrate-mono-mp3"
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

;;;;; Current values

(defvar tlon-babel-tts-current-voice ""
  "The voice used in the current text-to-speech process.")

(defvar tlon-babel-tts-current-language ""
  "The language used in the current text-to-speech process.")

;;;;; SSML patterns

(defconst tlon-babel-tts-ssml-lang
  '("<lang xml:lang=\"%s\">" . "</lang>")
  "SSML pattern for language tag, with locale code placeholder.")

(defconst tlon-babel-tts-ssml-voice
  "<voice name=\"%s\">%s</voice>"
  "SSML pattern for voice tag, with voice name and text placeholders.")

(defconst tlon-babel-tts-ssml-break
  "<break time=\"%s\" />"
  "SSML pattern for break tag, with time placeholder.
<https://learn.microsoft.com/en-us/previous-versions/office/developer/communication-server-2007/bb813930(v=office.12)>")

(defconst tlon-babel-tts-ssml-phoneme
  "<phoneme alphabet=\"%s\" ph=\"%s\">%s</phoneme>"
  "SSML pattern for phoneme tag, with phoneme and text placeholders.
<https://learn.microsoft.com/en-us/azure/ai-services/speech-service/speech-synthesis-markup-pronunciation>")

(defconst tlon-babel-tts-ssml-emphasis
  "<emphasis level=\"%s\">%s</emphasis>"
  "SSML pattern for emphasis tag, with level and text placeholders.
Note that this tag is not supported by Azure TTS except for a handful of voices:
<https://learn.microsoft.com/en-us/azure/ai-services/speech-service/speech-synthesis-markup-voice#adjust-emphasis>")

;;;;; Azure

(defconst tlon-babel-azure-ssml-template
  (format "<speak version='1.0' xml:lang='%%s'>%s</speak>"
	  tlon-babel-tts-ssml-voice)
  "SSML template for Azure TTS.")

(defconst tlon-babel-azure-request
  "curl --location --request POST 'https://eastus.tts.speech.microsoft.com/cognitiveservices/v1' \
--header 'Ocp-Apim-Subscription-Key: %s' \
--header 'Content-Type: application/ssml+xml' \
--header 'X-Microsoft-OutputFormat: %s' \
--header 'User-Agent: curl' \
--data-raw %s > '%s'"
  "Curl command to send a request to the Azure text-to-speech.")

(defconst tlon-babel-azure-voices
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

(defconst tlon-babel-tts-standard-abbreviations
  '(("es"
     (" a\\. de C\\." . " antes de Cristo")
     (" ca\\." . " alrededor de")
     (" d\\. de C\\." . " después de Cristo")
     (" e\\.[[:space:] ]?g\\." . " por ejemplo")
     (" EE\\.[[:space:] ]UU\\." . " Estados Unidos")
     (" et al\\." . " y otros")
     (" etc\\." . " etcétera")
     (" i\\.[[:space:] ]e\\." . " esto es")
     ("N\\.[[:space:] ]B\\." . " obsérvese bien")
     (" p\\.[[:space:] ]ej\\." . " por ejemplo")
     (" vs\\." . " versus")
     ("y/o" . "y o"))
    ("it"
     (()))
    ("fr"
     (())))
  "Standard abbreviations and their spoken equivalent in each language.")

;;;;; Currencies

(defconst tlon-babel-tts-currencies
  '(("₿" . "BTC")
    ("Ξ" . "ETH")
    ("£" . "GBP")
    ("₪" . "ILS")
    ("₹" . "INR")
    ("¥" . "JPY")
    ("$" . "USD"))
  "Currency symbols and their associated three-letter codes.")

(defconst tlon-babel-tts-currency-ssml
  "<say-as interpret-as=\"currency\">%s %s</say-as>"
  "SSML pattern for currency symbols.
The first placeholder is the currency amount, and the second is the currency
code.

For more information, see <https://learn.microsoft.com/en-us/azure/ai-services/speech-service/speech-synthesis-markup-pronunciation#say-as-element>.")

;;;;; Terms

(defconst tlon-babel-tts-terms
  `((,(mapcar 'cdr tlon-babel-languages)
     (("Nate Soares" . "neɪt soˈa.ɾis")
      ("GiveWell" . "ˈɡɪv.wɛl")))
    (("es")
     (("transhumanos" . "tɾansumanos"))))
  "Terms and their pronunciations.")

;;;;; Listener cues

(defconst tlon-babel-tts-cue-delimiter
  (concat "\n" (format tlon-babel-tts-ssml-break "0.5s"))
  "Delimiter for listener cues.")

;;;;;; Notes

(defconst tlon-babel-tts-note-cues
  '(("en" "A note starts here." . "The note ends here.")
    ("es" "Aquí empieza una nota." . "Aquí termina la nota."))
  "Listener cues for notes.")

;;;;;; Quotes

(defconst tlon-babel-tts-quote-cues
  '(("en" "A quote starts here." . "The quote ends here.")
    ("es" "Aquí empieza una cita." . "Aquí termina la cita."))
  "Listener cues for asides.")

;;;;;; Asides

(defconst tlon-babel-tts-aside-cues
  '(("en" "An aside starts here." . "The aside ends here.")
    ("es" "Aquí empieza un inciso." . "Aquí termina el inciso."))
  "Listener cues for asides.")

;;;;;; Images

(defconst tlon-babel-tts-image-cues
  '(("en" "Here’s an image." . "The image ends here.")
    ("es" "Aquí hay una imagen." ."Aquí termina la imagen."))
  "Listener cues for images.")

;;;; Functions

(defun tlon-babel-tts-get-alternative-voice ()
  "Return the voice in the current language that is not the current voice."
  (let ((voices (tlon-babel-lookup-all tlon-babel-azure-voices
				       :voice :language tlon-babel-tts-current-language)))
    (car (delete tlon-babel-tts-current-voice voices))))

(defun tlon-babel-tts-get-voice-locale ()
  "Return the locale of the current voice."
  (substring tlon-babel-tts-current-voice 0 5))

;;;;; Reading

(defun tlon-babel-tts-read-content (content &optional chunk-size)
  "Read CONTENT and return it as a string ready for TTS processing.
If CHUNK-SIZE is non-nil, split string into chunks no larger than that size."
  (with-temp-buffer
    (insert content)
    (tlon-babel-tts-prepare-buffer)
    (if chunk-size
	(tlon-babel-break-into-chunks chunk-size)
      (buffer-string))))

(defun tlon-babel-tts-read-file (file)
  "Return substantive content of FILE, handling in-text abbreviations."
  (unless (string= (file-name-extension file) "md")
    (user-error "File `%s' is not a Markdown file" file))
  (let ((tlon-babel-in-text-abbreviations))
    (with-current-buffer (find-file-noselect file)
      ;; to make `tlon-babel-tts-process-in-text-abbreviations' work, we
      ;; let-bound the variable above and now set its value to that of its
      ;; file-local counterpart
      (setq tlon-babel-in-text-abbreviations tlon-babel-in-text-abbreviations)
      (concat (tlon-babel-tts-get-metadata) (tlon-babel-md-read-content file)))))

;;;;; Azure

;;;###autoload
(defun tlon-babel-azure-narrate-content (&optional voice file)
  "Narrate content with VOICE using Azure's text-to-speech service.
If region is active, read the region. Otherwise, read FILE.

Save the narration to the `audio' directory in the same repository as FILE or,
if region is active, save it to the downloads directory."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-get-repo-from-file file))
	 (language (setq tlon-babel-tts-current-language
			 (tlon-babel-repo-lookup :language :dir repo)))
	 (voice (setq tlon-babel-tts-current-voice
		      (or voice (completing-read "Voice: " (tlon-babel-lookup-all
							    tlon-babel-azure-voices :voice
							    :language language)))))
	 (content (if (region-active-p)
		      (buffer-substring-no-properties (region-beginning) (region-end))
		    (tlon-babel-tts-read-file file)))
	 (chunks (tlon-babel-tts-read-content content tlon-babel-azure-char-limit))
	 (file-name-sans-extension (file-name-sans-extension
				    (file-name-nondirectory file)))
	 ;; FIXME: extension should be set based on `tlon-babel-azure-audio-settings'
	 (file-name (file-name-with-extension file-name-sans-extension "mp3"))
	 (destination (if (region-active-p)
			  (file-name-concat paths-dir-downloads file-name)
			(file-name-concat repo "audio" file-name)))
	 (nth 1))
    (dolist (chunk chunks)
      (tlon-babel-azure-generate-audio
       chunk voice (tlon-babel-name-chunk destination nth))
      (setq nth (1+ nth)))))

(defun tlon-babel-azure-generate-audio (text voice destination)
  "Generate an audio file from TEXT using VOICE and save it to DESTINATION."
  (let* ((locale (tlon-babel-tts-get-voice-locale))
	 (ssml (shell-quote-argument
		(format tlon-babel-azure-ssml-template locale voice text)))
	 (command (format tlon-babel-azure-request
			  (tlon-babel-get-or-set-azure-key)
			  tlon-babel-azure-audio-settings ssml destination)))
    (async-shell-command command)))

(defun tlon-babel-get-or-set-azure-key ()
  "Get or set the Azure key."
  (or tlon-babel-azure-key
      (setq tlon-babel-azure-key
	    (auth-source-pass-get "key1" "tlon/core/live.com/tlon.shared@gmail.com [Azure]"))))

;;;;; Chunk processing

(defun tlon-babel-break-into-chunks (chunk-size)
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

(defun tlon-babel-join-audio-files (file)
  "Join chunks of FILE back into a single file."
  (let* ((files (tlon-babel-get-file-chunks file))
	 (list-of-files (tlon-babel-create-ffmpeg-list-of-audio-files files)))
    (shell-command (format "ffmpeg -f concat -safe 0 -i %s -c copy %s"
			   list-of-files file))))

(defun tlon-babel-get-file-chunks (file)
  "Return a list of the file chunks for FILE."
  (let ((nth 1)
	file-chunk
	files)
    (while (file-exists-p (setq file-chunk (tlon-babel-name-chunk file nth)))
      (push file-chunk files)
      (setq nth (1+ nth)))
    (nreverse files)))

(defun tlon-babel-create-ffmpeg-list-of-audio-files (files)
  "Create a temporary file with a list of audio FILES for use with `ffmpeg'."
  (let ((temp-file-list (make-temp-file "files" nil ".txt")))
    (with-temp-file temp-file-list
      (dolist (file files)
	(insert (format "file '%s'\n" (expand-file-name file)))))
    temp-file-list))

(defun tlon-babel-name-chunk (file nth)
  "Return the name of the NTH chunk of FILE."
  (let ((extension (file-name-extension file))
	(file-name-sans-extension (file-name-sans-extension file)))
    (format "%s-%03d.%s" file-name-sans-extension nth extension)))

;;;;; Metadata

;; Should also include summary?
(defun tlon-babel-tts-get-metadata ()
  "Add title and author."
  (let* ((metadata (tlon-babel-yaml-format-values-of-alist (tlon-babel-yaml-get-metadata)))
	 (title (alist-get "title" metadata nil nil #'string=))
	 (authors (alist-get "authors" metadata nil nil #'string=))
	 (author-string (tlon-babel-concatenate-list authors)))
    (format "%s.\n\nPor %s.\n\n" title author-string)))

;;;;; Cleanup

(defun tlon-babel-tts-prepare-buffer ()
  "Prepare the current buffer for audio narration."
  (save-excursion
    (tlon-babel-tts-process-notes)
    (tlon-babel-tts-process-citations)
    (tlon-babel-tts-process-boldface)
    (tlon-babel-tts-process-italics)
    (tlon-babel-tts-process-headings)
    ;; replace small caps
    ;; check all other elements in markdown-menu
    (tlon-babel-tts-process-standard-abbreviations)
    (tlon-babel-tts-process-in-text-abbreviations)
    (tlon-babel-tts-process-terms)
    (tlon-babel-tts-process-asides)
    (tlon-babel-tts-process-quotes)
    (tlon-babel-tts-process-links)
    (tlon-babel-tts-process-images) ; should be after links
    (tlon-babel-tts-process-numbers)
    (tlon-babel-tts-process-currencies)
    (tlon-babel-tts-process-math-expressions))
  (goto-char (point-min)))

;;;;;; Notes

(defun tlon-babel-tts-process-notes ()
  "Replace note reference with its content, if it is a sidenote, else delete it.
Move the note to the end of the sentence if necessary.

Note: the function assumes that the citation is in MDX, rather than Pandoc
citation key, format. Hence, it must be run *before*
`tlon-babel-tts-process-citations'."
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
  (let ((clean-note (replace-regexp-in-string tlon-babel-sidenote-marker "" note)))
    (tlon-babel-tts-listener-cue-full-enclose tlon-babel-tts-note-cues clean-note)))

;;;;;; Citations

(defun tlon-babel-tts-process-citations ()
  "Replace our custom MDX cite tags with a pandoc-style citation.
For example `<Cite bibKey={\"Clark2015SonAlsoRises\"} />' will be replaced with
`[@Clark2015SonAlsoRises]'."
  (goto-char (point-min))
  (while (re-search-forward tlon-babel-cite-pattern nil t)
    (replace-match (format "[@%s]"(match-string 1)) nil nil)))

;;;;;; Formatting

(defun tlon-babel-tts-process-formatting (type)
  "Remove formatting TYPE from text."
  (cl-destructuring-bind (pattern . group)
      (pcase type
	('boldface (cons markdown-regex-bold 4))
	('italics (cons markdown-regex-italic 3))
	;; TODO: determine if other types should be added
	(_ (user-error "Invalid formatting type: %s" type)))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match (format " %s" (match-string group)) t nil))))

(defun tlon-babel-tts-process-boldface ()
  "Remove boldface from text."
  (tlon-babel-tts-process-formatting 'boldface))

(defun tlon-babel-tts-process-italics ()
  "Remove italics from text."
  (tlon-babel-tts-process-formatting 'italics))

;;;;;; Headings

(defun tlon-babel-tts-process-headings ()
  "Remove heading markers from headings."
  (let ((insert-pause (format tlon-babel-tts-ssml-break "1s")))
    (goto-char (point-min))
    (while (re-search-forward markdown-regex-header nil t)
      (replace-match (format "%s %s" insert-pause (match-string 5))))))

;;;;;; Abbreviations

(defun tlon-babel-tts-process-standard-abbreviations ()
  "Replace standard abbreviations with their spoken equivalent."
  (let* ((abbreviations (alist-get tlon-babel-tts-current-language
				   tlon-babel-tts-standard-abbreviations nil nil #'string=)))
    (dolist (abbreviation abbreviations)
      (goto-char (point-min))
      (while (re-search-forward (car abbreviation) nil t)
	(replace-match (cdr abbreviation) t nil)))))

(defun tlon-babel-tts-process-in-text-abbreviations ()
  "Replace in-text abbreviations with their spoken equivalent.
In-text abbreviations are those that are introduced in the text itself,
typically in parenthesis after the first occurrence of the phrase they
abbreviate. We store these abbreviations on a per file basis, in the file-local
variable `tlon-babel-in-text-abbreviations'"
  (let ((case-fold-search nil))
    (dolist (entry tlon-babel-in-text-abbreviations)
      (cl-destructuring-bind (abbrev . expansion) entry
	(let ((abbrev-introduced (format "%s (%s)" expansion abbrev)))
	  ;; we first replace the full abbrev introduction, then the abbrev itself
	  (dolist (cons (list (cons abbrev-introduced expansion) entry))
	    (goto-char (point-min))
	    (while (re-search-forward (car cons) nil t)
	      (replace-match (cdr cons) t nil))))))))

;; Things to fix:
;; - https://docs.google.com/document/d/1m1k57PbKkkVy0eLwrHKjZiu9XOcOD74WhX4jKKQbwRU/

;;;;;; Terms

(defun tlon-babel-tts-process-terms ()
  "Replace terms with their pronunciations."
  (let* ((case-fold-search nil)
	 (language (tlon-babel-repo-lookup :language :dir (tlon-babel-get-repo)))
	 (terms (tlon-babel-get-terms-for-language language)))
    (dolist (term terms)
      (let ((find (format "\\b%s\\b" (car term)))
	    (replace (cdr term)))
	(goto-char (point-min))
	(while (re-search-forward find nil t)
	  (replace-match (format tlon-babel-tts-ssml-phoneme
				 "ipa" replace (match-string-no-properties 0)) t))))))

(defun tlon-babel-get-terms-for-language (language)
  "Get the terms associated with LANGUAGE."
  (let ((result '()))
    (dolist (item tlon-babel-tts-terms result)
      (when (member language (car item))
        (setq result (append result (cadr item)))))
    result))

;;;;;; Listener cues

;;;;;;; General functions

(defun tlon-babel-tts-enclose-in-listener-cues (type text)
  "Enclose TEXT in listener cues of TYPE."
  (cl-destructuring-bind (cue-begins . cue-ends)
      (alist-get tlon-babel-tts-current-language type nil nil #'string=)
    (format "%s %s %s" cue-begins text cue-ends)))

(defun tlon-babel-tts-enclose-in-voice-tag (string)
  "Enclose STRING in `voice' SSML tags."
  (format tlon-babel-tts-ssml-voice (tlon-babel-tts-get-alternative-voice) string))

(defun tlon-babel-tts-enclose-in-cue-delimiter (string)
  "Enclose STRING in listener cue delimiter."
  (format "%1$s%s%1$s" tlon-babel-tts-cue-delimiter string))

(defun tlon-babel-tts-listener-cue-full-enclose (type text)
  "Enclose TEXT in listener cue of TYPE, and in turn in `voice' SSML tags."
  (tlon-babel-tts-enclose-in-cue-delimiter
   (tlon-babel-tts-enclose-in-voice-tag
    (tlon-babel-tts-enclose-in-listener-cues type text))))

(defun tlon-babel-tts-process-element (type)
  "Add listener cues for text enclosed in tags of TYPE."
  (cl-destructuring-bind (pattern cues group)
      (pcase type
	('aside (list tlon-babel-mdx-aside-pattern tlon-babel-tts-aside-cues 2))
	('quote (list markdown-regex-blockquote tlon-babel-tts-quote-cues 3))
	('image (list markdown-regex-link-inline tlon-babel-tts-image-cues 3))
	;; TODO: determine if other types should be added
	(_ (user-error "Invalid formatting type: %s" type)))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match
       (tlon-babel-tts-listener-cue-full-enclose cues (match-string-no-properties group))))))

;;;;;;; Specific elements

(defun tlon-babel-tts-process-quotes ()
  "Add listener cues for blockquotes."
  (tlon-babel-tts-process-element 'quote))

(defun tlon-babel-tts-process-asides ()
  "Add listener cues for asides."
  (tlon-babel-tts-process-element 'aside))

(defun tlon-babel-tts-process-images ()
  "Add listener cues for images."
  (tlon-babel-tts-process-element 'image))

;;;;;; Links

(defun tlon-babel-tts-process-links ()
  "Replace links with their text.
Note: this function should be run after `tlon-babel-tts-process-images' because
image links are handled differently."
  (goto-char (point-min))
  (while (re-search-forward markdown-regex-link-inline nil t)
    (replace-match (match-string 3))))

;;;;;; Numbers

(defun tlon-babel-tts-process-numbers ()
  "Remove number separators."
  (let* ((separator (alist-get tlon-babel-tts-current-language
			       tlon-babel-md-number-separators nil nil #'string=)))
    (goto-char (point-min))
    (while (re-search-forward (format tlon-babel-md-number-separator-pattern separator) nil t)
      (replace-match "\\1\\3"))))

;;;;;; Currencies

(defun tlon-babel-tts-process-currencies ()
  "Format currency with appropriate SSML tags."
  (dolist (cons tlon-babel-tts-currencies)
    (let ((symbol (car cons))
	  (code (cdr cons)))
      (goto-char (point-min))
      (while (re-search-forward (format "%s.?\\([0-9,.]+\\)\\b" (regexp-quote symbol)) nil t)
	(replace-match (format tlon-babel-tts-currency-ssml
			       (match-string-no-properties 1) code))))))

;;;;;; Math expressions

(declare-function tlon-babel-ai-translate-math "tlon-babel-ai")
(defun tlon-babel-tts-process-math-expressions ()
  "Replace math expressions with their spoken equivalent."
  (dolist (pattern (list tlon-babel-math-inline-pattern
			 tlon-babel-math-display-pattern))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (let ((math (match-string-no-properties 2)))
	(replace-match "" nil nil)
	(tlon-babel-ai-translate-math math tlon-babel-tts-current-language)))))

(provide 'tlon-babel-tts)
;;; tlon-babel-tts.el ends here
