;;; tlon-md.el --- Markdown functionality for Tlön -*- lexical-binding: t -*-

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

;; Markdown functionality for Tlön.

;;; Code:

(require 'markdown-mode-extras)
(require 'tlon-core)
(require 'tlon-yaml)

(declare-function tlon-deepl-translate "tlon-deepl")
(declare-function tlon-deepl-print-translation "tlon-deepl")

;;;; User options

(defgroup tlon-md ()
  "Markdown functionality."
  :group 'tlon)

(defcustom tlon-md-special-characters
  '(("bullet" . "•")
    ("en dash" . "–")
    ("em dash" . "—")
    ("opening single quote" . "‘")
    ("closing single quote" . "’")
    ("opening double quote" . "“")
    ("closing double quote" . "”")
    ("narrow non-break space" . " ")
    ("soft hyphen" . "­")
    ("ellipsis" . "…"))
  "Alist of special characters to insert in a Markdown file."
  :group 'tlon-md
  :type '(alist :key-type string :value-type string))

;;;; Variables

;;;;; Local variables

(defconst tlon-md-local-variables-line-start
  "<!-- Local Variables:"
  "Start of the first line that contains file local variables.")

(defconst tlon-md-local-variables-line-end
  "End: -->"
  "End of the last line that contains file local variables.")

;;;;; Tag sections

(defconst tlon-md-canonical-tag-sections
  '((:sections ("Further reading" "External links" "Related entries") :language "en")
    (:sections ("Más información" "Enlaces externos" "Entradas relacionadas") :language "es")))

;;;;; Markdown

;;;;;; Italics

;; We tweak the default value of `markdown-regex-italic' to make it analogous
;; to `markdown-regex-bold': the group that captures the leading character is a
;; proper capturing group, allowing us to prepend this character to the
;; replacement.
(setq markdown-regex-italic
      "\\(?1:^\\|[^\\]\\)\\(?2:\\(?3:[*_]\\)\\(?4:[^ \n	\\]\\|[^ \n	*]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(?5:\\3\\)\\)")

;;;;;; Links

(defconst tlon-md-regexp-link-formatter
  "\\(?%s:!\\)?\\(?%s:\\[\\)\\(?%s:\\^?\\(?:\\\\\\]\\|[^]]\\)*\\|\\)\\(?%s:\\]\\)\\(?%s:(\\)\\s-*\\(?%s:[^)]*?\\)\\(?:\\s-+\\(?%s:\"[^\"]*\"\\)\\)?\\s-*\\(?%s:)\\)"
  "Formatter for a regexp pattern to match a Markdown link.
This copies `markdown-regex-link-inline' but replaces the digits in each capture
group with placeholders.")

;;;;;; Images

(defconst tlon-md-image
  "!\\[\\(?1:.*?\\)\\](\\(?2:.*?\\)\\(?:\\s-*\"\\(?3:.*?\\)\"\\)?)"
  "Pattern to match an image in a Markdown file.
The first group captures the alt text. The second group captures the image URL.
The third group captures the title.")

(defconst tlon-md-image-sans-alt
  "!\\[\\](.*?\\(?:\\s-*\".*?\"\\)?)"
  "Pattern to match an image without alt text in a Markdown file.")

;;;;;; Blockquote

;; TODO: make sure it works even when the blockquote does *not* span more than one line
(defconst tlon-md-blockquote
  "\\(?1:\\(?:^[[:blank:]]*?>.*\n\\)+\\)"
  "Pattern to match a blockquote in a Markdown file.
The pattern captures the entire blockquote, including the quote markers. This is
to allow for capturing multi-line quotes. To get the actual quote only, remove
them from the captured string as part of the post-processing.")

;;;;;; Math

(defconst tlon-md-math-power
  "\\(?1:[[:digit:]]+\\)<sup>\\(?2:[n[:digit:]]+\\)</sup>"
  "Regexp pattern for matching a number raised to a power.
The first group captures the base, and the second group captures
the exponent.")

(defconst tlon-md-math-big-number
  "\\b\\(?1:[[:digit:]]\\{4,\\}\\(?:\\.[[:digit:]]+\\)?\\)\\b"
  "Regexp pattern for matching a “big” number.
Currently, we define it as a number no smaller than 1000.")

;;;;;; Footnotes

(defconst tlon-md-footnote-start
  "\\[\\^%s\\]:[[:space:]]"
  "Regexp pattern to match the start of a footnote.
The placeholder is the footnote number.")

;;;;;; Headings

;; adapted from `markdown-regex-header'
(defconst tlon-md-heading-template
  "^\\(?:\\(?1:[^
\n	 -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\\(?4:%s[ 	]+\\)\\(?5:.*?\\)\\(?6:[ 	]+#+\\)?\\)$"
  "Template to construct constants to match headings and subheadings.")

(defconst tlon-md-heading
  (format tlon-md-heading-template "##")
  "Regexp patern to match a top-level heading.")

(defconst tlon-md-subheading
  (format tlon-md-heading-template "###+")
  "Regexp patern to match a subheading.
A subheading is defined as any heading that is not top-level.")

;;;;; SSML

;;;;;; `emphasis'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#emphasis-tag

(defconst tlon-md-ssml-emphasis-levels
  '("none" "reduced" "moderate" "strong")
  "Admissible values for the `level' attribute of the `emphasis' SSML tag.")

(defconst tlon-tts-ssml-default-emphasis-level
  "moderate"
  "Default value for the `level' attribute of the `emphasis' SSML tag.")

;;;;;; `phoneme'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#phoneme-tag

(defconst tlon-md-ssml-phoneme-alphabets
  '("ipa" "x-sampa")
  "Admissible values for the `alphabet' attribute of the `phoneme' SSML tag.")

(defconst tlon-tts-ssml-phoneme-default-alphabet
  "ipa"
  "Default value for the `alphabet' attribute of the `phoneme' SSML tag.")

;;;;;; `prosody'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#prosody-tag

;; not currently supported

;;;;;; `say-as'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#say-as-tag

(defconst tlon-md-ssml-interpret-as-values
  '("cardinal" "ordinal" "digits" "fraction" "unit" "date" "time" "telephone" "address" "interjection" "expletive" "spell-out" "characters" "verbatim")
  "Admissible values for the `interpret-as' attribute of the SSML tag.")

;;;;; MDX

;;;;;; `Table'

;; TODO: develop
;; https://github.com/tlon-team/uqbar-issues#mdx-tag-syntax

;;;;; Common constants

(defconst tlon-tag-specs
  `((:tag "AlternativeVoice"
	  :type mdx
	  :self-closing nil
	  :doc "Encloses text to be narrated in the alternative voice, as opposed to the main voice.")
    (:tag "Aside"
	  :type mdx
	  :self-closing nil
	  :doc "Encloses text in an aside section.")
    (:tag "break"
	  :attributes ((:name "time" :required t :valued t :group 2 :prompt "Time (e.g. \"1s\"): "))
	  :type ssml
	  :self-closing t
	  :doc "")
    (:tag "Cite"
	  :attributes ((:name "bibKey" :required t :valued t :group 2 :reader tlon-md-cite-bibkey-reader)
		       (:name "locator" :required nil :valued t :group 4 :reader tlon-md-cite-locator-reader)
		       (:name "short" :required nil :valued nil :group 6 :reader tlon-md-cite-length-reader))
	  :type mdx
	  :self-closing t)
    (:tag "emphasis"
	  :attributes ((:name "level" :required t :valued t :group 3 :reader tlon-md-emphasis-level-reader))
	  :type ssml
	  :self-closing nil
	  :doc "")
    (:tag "Figure"
	  :attributes ((:name "src" :required t :valued t  :group 3 :prompt "Image URL: ")
		       (:name "alt" :required nil :valued t :group 5 :prompt "Alt text: ")
		       (:name "ignore-content" :required nil :valued nil :group 7 :reader tlon-md-figure-ignore-content-reader))
	  :type mdx
	  :self-closing nil)
    (:tag "Footnote"
	  :type mdx
	  :self-closing t
	  :doc "")
    (:tag "lang"
	  :attributes ((:name "xml:lang" :required t :valued t :group 3 :reader tlon-md-lang-reader))
	  :type ssml
	  :self-closing nil
	  :doc "")
    (:tag "Language"
	  :attributes ((:name "id" :required t :valued t :group 3 :prompt "Language: "))
	  :type mdx
	  :self-closing nil
	  :doc "Encloses text treated as belonging to that language (e.g. for the purposed of hyphenating it).")
    (:tag "LiteralLink"
	  :attributes ((:name "src" :required t :valued t :group 3 :prompt "Link: "))
	  :type mdx
	  :self-closing nil
	  :doc "The URL of a regular link may be replaced by our scripts according to various
rules. To prevent these replacements with a particular link, enclose its texts
in this tag and pass the URL as the value of the `src' attribute.")
    (:tag "Math"
	  :attributes ((:name "alt" :required nil :valued t :group 3 :prompt "Alt text: ")
		       (:name "display" :required nil :valued nil :group 5 :reader tlon-md-math-display-reader))
	  :type mdx
	  :self-closing nil)
    (:tag "Embedded"
	  :attributes ((:name "src" :required t :valued t :group 3 :prompt "Chart URL: ")
		       (:name "alt" :required nil :valued t :group 5 :prompt "Alt text: ")
		       (:name "height" :required nil :valued t :group 7 :prompt "Height: "))
	  :type mdx
	  :self-closing t
	  :doc "")
    (:tag "phoneme"
	  :attributes ((:name "alphabet" :required t :valued t :group 3 :reader tlon-md-phoneme-alphabet-reader)
		       (:name "ph" :required t :valued t :group 5 :prompt "Phonetic symbols: "))
	  :type ssml
	  :self-closing nil
	  :doc "")
    (:tag "ReplaceAudio"
	  :attributes ((:name "text" :required nil :valued t :group 3 :prompt "Audio text: ") ; Allow empty string
		       (:name "role" :required nil :valued t :group 5 :prompt "Role: " :reader tlon-md-replace-audio-voice-reader))
	  :type mdx
	  :self-closing nil
	  :doc "Encloses text to be read based on the value of `text'.")
    (:tag "Roman"
	  :type mdx
	  :self-closing nil
	  :doc "Encloses text to be displayed in small caps. It will also be read correctly in TTS narration.")
    (:tag "say-as"
	  :attributes ((:name "interpret-as" :required t :valued t :group 3 :reader tlon-md-say-as-interpret-as-reader))
	  :type ssml
	  :self-closing nil
	  :doc "")
    (:tag "Sidenote"
	  :type mdx
	  :self-closing t
	  :doc "")
    (:tag "SimpleTable"
	  :attributes ((:name "alt" :required t :valued t :group 3 :prompt "Alt text: ")
		       (:name "include" :required nil :valued nil :group 5 :reader tlon-md-simple-table-include-reader))
	  :type mdx
	  :self-closing nil
	  :doc "Encloses a Markdown table.")
    (:tag "SmallCaps"
	  :type mdx
	  :self-closing nil
	  :doc "Encloses text to be displayed in small caps.")
    (:tag "sub"
	  :type html
	  :self-closing nil)
    (:tag "sup"
	  :type html
	  :self-closing nil)
    (:tag "q"
	  :type html
	  :self-closing nil)
    (:tag "VisuallyHidden"
	  :type mdx
	  :self-closing nil
	  :doc "Text to be narrated, but not displayed.")
    (:tag "voice"
	  :attributes ((:name "name" :required t :valued t :group 3 :reader tlon-md-voice-name-reader)) ; Added reader
	  :type ssml
	  :self-closing nil
	  :doc "Specifies the voice to use. The `name` attribute should contain the friendly voice name (e.g., \"Brian\", \"Victoria\").")
    (:tag "VoiceRole"
	  :attributes ((:name "role" :required t :valued t :group 3 :prompt "Role: " :reader tlon-md-replace-audio-voice-reader))
	  :type mdx
	  :self-closing nil
	  :doc "Encloses text to be read based on the value of `text'."))
  "Property list of MDX tag attributes and patterns.
- `:group': the group number that captures the attribute. The value of the
  attribute is captured in a separate group, numbered `:group' + 1. The entire
  tag expression is captured in group number 1, and the text enclosed by the
  tags, when the tag is not self-closing, is captured in group number 2.")

;;;; Functions

;;;;; Tag sections

;;;###autoload
(defun tlon-md-get-tag-section (section target-language)
  "Translate tag SECTION to TARGET-LANGUAGE."
  (let ((result nil))
    (dolist (entry tlon-md-canonical-tag-sections)
      (let ((sections (plist-get entry :sections)))
        (when (member section sections)
          (let ((index (cl-position section sections :test 'equal))
                (target-entry (tlon-lookup tlon-md-canonical-tag-sections :sections :language target-language)))
            (when (and target-entry index)
              (setq result (nth index target-entry)))))))
    result))

;;;;; Insertion

;;;;;; metadata

;;;;###autoload
(defun tlon-edit-yaml-field ()
  "Edit the YAML field at point."
  (interactive)
  (cl-destructuring-bind (key value) (tlon-yaml-get-field-at-point)
    (tlon-yaml-insert-field key value)))

;;;;;; Tags

;; TODO: revise to support multiple langs, including en
;;;###autoload
(defun tlon-insert-internal-link ()
  "Insert a link to an entity at point.
The entity can be a tag or an author."
  (interactive)
  (tlon-ensure-markdown-mode)
  (let* ((selection (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))
	 (current-link (markdown-link-at-pos (point)))
	 (current-desc (nth 2 current-link))
	 (current-target (nth 3 current-link))
	 current-element-title)
    (when current-target
      (setq current-element-title
	    (tlon-md-get-title-in-link-target
	     current-target)))
    (let* ((candidates (append
			(tlon-metadata-lookup-all (tlon-metadata-in-repo) "title" "type" "article")
			(tlon-metadata-lookup-all (tlon-metadata-in-repo) "title" "type" "author")
			(tlon-metadata-lookup-all (tlon-metadata-in-repo) "title" "type" "tag")))
	   (new-element-title (completing-read "Selection: " candidates nil t
					       (or current-element-title selection)))
	   (new-target-file (tlon-metadata-lookup (tlon-metadata-in-repo) "file" "title" new-element-title))
	   (new-target-dir (file-relative-name
			    (file-name-directory new-target-file) (file-name-directory (buffer-file-name))))
	   (new-target (file-name-concat new-target-dir (file-name-nondirectory new-target-file)))
	   (new-desc (if (and current-desc (string= new-target current-target))
			 current-desc
		       (or selection new-element-title)))
	   (link (format "[%s](%s)" new-desc new-target)))
      (when current-target
	(markdown-mode-extras-delete-link))
      (when selection
	(delete-region (region-beginning) (region-end)))
      (insert link))))

(defun tlon-md-get-title-in-link-target (target)
  "Return the title of the tag to which the TARGET of a Markdown link points."
  (let* ((file (expand-file-name target default-directory))
	 (title (tlon-metadata-lookup (tlon-metadata-in-repo) "title" "file" file)))
    title))

(defun tlon-md-sort-elements-in-paragraph (separator)
  "Sort the elements separated by SEPARATOR in the current paragraph."
  (save-excursion
    ;; Get paragraph boundaries
    (let* ((para-start (progn (backward-paragraph)
			      (skip-chars-forward "\n\t ")
			      (point)))
	   (para-end (progn (end-of-paragraph-text)
			    (point)))
	   ;; Get paragraph text, separate the links
	   (para-text (buffer-substring-no-properties para-start para-end))
	   (link-list (mapcar 'ucs-normalize-NFD-string (split-string para-text separator)))
	   ;; Trim and sort the links
	   (sorted-links (seq-sort-by 'downcase
				      (lambda (s1 s2)
					(string-collate-lessp s1 s2 nil t))
				      (mapcar 'string-trim link-list))))
      ;; Clear the current paragraph
      (delete-region para-start para-end)
      ;; Replace it with sorted links
      (goto-char para-start)
      (insert (mapconcat 'identity sorted-links separator)))))

;;;###autoload
(defun tlon-md-sort-related-entries ()
  "Sort the links in the `related entries' section in current buffer.
If no section is found, do nothing."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^## Entradas relacionadas" nil t)
      (forward-paragraph)
      (tlon-md-sort-elements-in-paragraph " • "))))

;;;###autoload
(defun tlon-md-open-src-file ()
  "If point is on a tag with a `src' attribute, open the file it points to."
  (interactive)
  (tlon-ensure-markdown-mode)
  (if-let ((tag (tlon-get-tag-at-point)))
      (let* ((names (tlon-get-tag-attribute-names tag))
             (src-pos (cl-position "src" names :test #'string=)))
        (if src-pos
            (let* ((values (tlon-get-tag-attribute-values tag))
                   (src-value (nth src-pos values)))
              (if (and src-value (not (string-empty-p src-value)))
                  (find-file (expand-file-name src-value (file-name-directory (buffer-file-name))))
                (user-error "Tag `%s' has an empty `src' attribute" tag)))
          (user-error "Tag `%s' does not have a `src' attribute" tag)))
    (user-error "Point is not on a known tag")))

;;;;;; Tag handling

(defun tlon-md-insert-or-edit-tag (tag)
  "Insert or edit an MDX TAG pair at point or around the selected region."
  (if (tlon-looking-at-tag-p tag)
      (tlon-md-edit-tag)
    (tlon-md-return-tag tag)))

(defun tlon-looking-at-tag-p (tag)
  "Return t iff point is looking at TAG."
  (let ((pattern (tlon-md-format-tag tag nil 'get-match-string)))
    (thing-at-point-looking-at pattern)))

(defun tlon-md-edit-tag (&optional values content format)
  "Edit tag at point.
Optionally, set the attribute VALUES and the tag CONTENT. FORMAT is one of
`get-match-string', `get-placeholders', `get-values', `insert-values', or
`insert-prompt'. If `get-match-string', return a regex pattern to match the tag
and capture its attribute values. Otherwise, return a cons cell with the tag
pair or the opening tag if it is self-closing. If `get-placeholders', return it
filled with placeholders. If `get-values', return it filled with VALUES. If
`insert-values', insert it filled with VALUES. If `insert-prompt', prompt the
user to insert VALUES."
  (let* ((tag (tlon-get-tag-at-point))
         (pattern (tlon-md-get-tag-pattern tag)))
    ;; Re-scan to get fresh match data
    (goto-char (line-beginning-position))
    (when (re-search-forward pattern (line-end-position) t)
      (let* ((existing-values (or values (tlon-get-tag-attribute-values tag)))
             (content (or content (match-string-no-properties 2)))
             (beg (match-beginning 0))
             (end (match-end 0)))
        ;; Store positions before we lose match data
        (delete-region beg end)
        (goto-char beg)
        (tlon-md-return-tag tag existing-values content format)))))

(defun tlon-md-set-tag-attribute-value (tag attribute value)
  "Set the VALUE of ATTRIBUTE for TAG."
  (let ((values (tlon-get-tag-attribute-values tag))
	(pos (cl-position attribute (tlon-get-tag-attribute-names tag) :test #'string=)))
    (setcar (nthcdr pos values) value)
    values))

;;;###autoload
(defun tlon-md-insert-attribute-value (attribute value &optional content)
  "Insert an ATTRIBUTE VALUE in the tag at point.
If the tag already contains an attribute with the same name, replace its value.
If CONTENT is provided, use it as the tag content."
  (let ((tag (tlon-get-tag-at-point)))
    (tlon-md-edit-tag (tlon-md-set-tag-attribute-value tag attribute value)
		      content 'insert-values)))

(defun tlon-md-return-tag (tag &optional values content format)
  "Return a TAG or TAG pair at point or around the selected region.
VALUES is a list of attribute values. When non-nil, offer each value as the
initial input when prompting the user for the attribute value. CONTENT is the
text the tag pair encloses. FORMAT is one of `get-match-string',
`get-placeholders', `get-values', `insert-values' or `insert-prompt'. If
`get-match-string', return a regex pattern to match the tag and capture its
attribute values. Otherwise, return a cons cell with the tag pair or the opening
tag if it is self-closing. If `get-placeholders', return it filled with
placeholders. If `get-values', return it filled with VALUES. If `insert-values',
insert it filled with VALUES. If `insert-prompt', prompt the user to insert
VALUES."
  ;; TODO: add check that the length of the VALUES list matches the number of
  ;; tag attributes, and is nil when it takes no attributes. Similarly for
  ;; CONTENT and self-closing tag.
  (let ((format (or format 'insert-prompt)))
    (cl-destructuring-bind (open . close) (tlon-md-format-tag tag values format)
      (if close
          (let ((text (if content
                          content
                        (when (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))))))
            (when (use-region-p)
              (delete-region (region-beginning) (region-end)))
            (tlon-md-act-on-returned-tag (concat open (or text "") close) format))
        (tlon-md-act-on-returned-tag open format)))))

(defun tlon-md-act-on-returned-tag (string format)
  "Act on the returned STRING based on FORMAT.
FORMAT is one of `get-match-string', `get-placeholders', `get-values',
`insert-values' or `insert-prompt'. If `get-match-string', return a regex
pattern to match the tag and capture its attribute values. Otherwise, return a
cons cell with the tag pair or the opening tag if it is self-closing. If
`get-placeholders', return it filled with placeholders. If `get-values', return
it filled with VALUES. If `insert-values', insert it filled with VALUES. If
`insert-prompt', prompt the user to insert VALUES."
  (pcase format
    ((or 'insert-prompt 'insert-values) (insert string))
    ((or 'get-match-string 'get-placeholders 'get-values) string)))

(defun tlon-md-format-tag (tag &optional values format)
  "Return TAG with attribute VALUES in the appropriate format.
FORMAT is one of `get-match-string', `get-placeholders', `get-values',
`insert-values' or `insert-prompt'. If `get-match-string', return a regex
pattern to match the tag and capture its attribute values. Otherwise, return a
cons cell with the tag pair or the opening tag if it is self-closing. If
`get-placeholders', return it filled with placeholders. If `get-values', return
it filled with VALUES. If `insert-values', insert it filled with VALUES. If
`insert-prompt', prompt the user to insert VALUES."
  (let* ((attributes (tlon-md-format-tag-with-attributes tag values format))
	 (self-closing-p (tlon-lookup tlon-tag-specs :self-closing :tag tag))
	 (format-string (if self-closing-p "\\(?1:%s\\)" "\\(?1:%s\\(?2:\\(.\\|\n\\)*?\\)%s\\)"))
	 (open-ending (if self-closing-p " />" ">"))
	 (open (concat "<" tag attributes open-ending))
	 (close (unless self-closing-p (concat "</" tag ">"))))
    (pcase format
      ('get-match-string (format format-string open close))
      ((or 'get-placeholders 'get-values 'insert-values 'insert-prompt)
       (if self-closing-p (list open) (cons open close))))))

(defun tlon-md-get-tag-filled (tag &optional values content)
  "Return a TAG string with VALUES and CONTENT.
VALUES is a list of attribute values. When non-nil, offer each value as the
initial input when prompting the user for the attribute value. CONTENT is the
text the tag pair encloses."
  (cl-destructuring-bind (open . close)
      (tlon-md-format-tag tag values 'get-values)
    (if close (concat open (or content "") close) open)))

(defun tlon-md-get-tag-to-fill (tag)
  "Return a TAG string with placeholders for content and attribute values."
  (cl-destructuring-bind (open . close)
      (tlon-md-format-tag tag nil 'get-placeholders)
    (if close (concat open "%s" close) open)))

(defun tlon-get-tag-at-point ()
  "Return the name of the tag at point."
  (let ((tags (mapcar (lambda (plist)
			(plist-get plist :tag))
		      tlon-tag-specs)))
    (catch 'found
      (dolist (tag tags)
	(when (tlon-looking-at-tag-p tag)
	  (throw 'found tag))))))

(defun tlon-get-tag-groups (tag &optional values)
  "Return the attribute capture group numbers for TAG.
If VALUES is non-nil, return instead the capture group numbers for the attribute
values."
  (let* ((attributes (tlon-lookup tlon-tag-specs :attributes :tag tag))
	 (groups (mapcar (lambda (attribute)
			   (plist-get attribute :group))
                         (if values
                             (seq-filter (lambda (attr)
                                           (plist-get attr :valued))
					 attributes)
                           attributes))))
    (if values (mapcar #'1+ groups) groups)))

(defun tlon-get-tag-attribute-names (tag)
  "Return a list of all attribute names of TAG."
  (let* ((attributes (tlon-lookup tlon-tag-specs :attributes :tag tag))
	 names)
    (dolist (attribute attributes (reverse names))
      (push (plist-get attribute :name) names))))

;;;###autoload
(defun tlon-get-tag-attribute-values (tag)
  "Return a list of all attribute values of TAG at point."
  (when (tlon-looking-at-tag-p tag)
    (let* ((attributes (tlon-lookup tlon-tag-specs :attributes :tag tag))
           values)
      (dolist (attr attributes values)
        (let* ((group (plist-get attr :group))
               (valued (plist-get attr :valued))
               (value (if valued
                          (match-string-no-properties (1+ group))  ; value group
			(match-string-no-properties group))))      ; attribute group
          (push value values)))
      (nreverse values))))

;; Not currently used, but might come handy
(defun tlon-md-goto-tag-attribute-value (tag attribute)
  "When point is on TAG, move to beginning of ATTRIBUTE value and return it."
  (interactive)
  (when (tlon-looking-at-tag-p tag)
    (let* ((group (1+ (tlon-md-lookup-tag-attribute-property tag attribute :group))))
      (when-let* ((values (tlon-get-tag-attribute-values tag))
                  (value (car values))
                  (pattern (tlon-md-format-attribute-with-placeholder tag attribute 'capture)))
        (re-search-backward (format pattern value) nil t)
        (goto-char (match-beginning group))
        value))))

(defun tlon-md-lookup-tag-attribute-property (tag name property)
  "For the attribute named NAME in TAG, return the value of the PROPERTY."
  (let ((attributes (tlon-lookup tlon-tag-specs :attributes :tag tag)))
    (tlon-lookup attributes property :name name)))

(defun tlon-md-format-tag-with-attributes (tag &optional values format)
  "Format TAG attributes.
FORMAT is one of `get-match-string', `get-placeholders', `get-values' or
`insert-prompt'. If `get-match-string', return a regex string pattern to match
the tag attributes and capture its values. If `get-values', return string with
the attributes filled with VALUES. If `insert-prompt', prompt the user to insert
VALUES. If `get-placeholders', return it filled with placeholders."
  (let* ((names (tlon-get-tag-attribute-names tag))
	 (list (if values
		   (cl-mapcar #'cons names values)
		 (mapcar #'list names)))
	 formatted)
    (dolist (cons list formatted)
      (let* ((name (car cons))
	     (value (cdr cons))
	     (format-string (tlon-md-format-attribute-with-placeholder
			     tag name (eq format 'get-match-string)))
	     (required-p (tlon-md-lookup-tag-attribute-property tag name :required))
	     (group (tlon-md-lookup-tag-attribute-property tag name :group))
	     (fun (or (tlon-md-lookup-tag-attribute-property tag name :reader)
		      (lambda ()
			(read-string (tlon-md-lookup-tag-attribute-property tag name :prompt) value))))
	     (string (pcase format
		       ('insert-prompt (tlon-md-format-tag-with-attributes-from-user required-p fun))
		       ((or 'get-values 'insert-values) value))))
	(setq formatted
	      (concat formatted
		      (pcase format
			('get-match-string (tlon-make-attribute-pattern-searchable format-string group required-p))
			((or 'get-values 'insert-values 'insert-prompt)
			 ;; Don't insert nil or empty string values
			 (when (and string (not (string-empty-p string)))
			   (format format-string string)))
			('get-placeholders format-string))))))))

(defun tlon-md-format-attribute-with-placeholder (tag name &optional capture)
  "Get a formatted string for the attribute NAME of TAG.
Include a placeholder for the attribute value, when it is a valued attribute. If
CAPTURE is non-nil, enclose the placeholder in a named capture group."
  (let* ((valued-p (tlon-md-lookup-tag-attribute-property tag name :valued))
	 (group (tlon-md-lookup-tag-attribute-property tag name :group))
	 (attribute-template (if valued-p
				 (concat " " name)
			       "%s"))
	 (value-template (if capture
			     (format "=\"\\(?%s:%%s\\)\"" (1+ group))
			   "=\"%s\"")))
    (concat attribute-template (when valued-p value-template))))

(defun tlon-make-attribute-pattern-searchable (pattern group &optional required greedy)
  "Make attribute PATTERN with placeholders searchable.
Enclose the PATTERN in a capture group numbered GROUP. Replace the placeholders
with a greedy pattern if GREEDY is non-nil, and a lazy pattern otherwise.
Enclose each pattern in a capture group numbered consecutively starting after
the highest numbered group in the pattern. Unless REQUIRED is non-nil, make the
capture group optional."
  (let* ((string (if greedy ".**" ".*?"))
	 (group-content (format pattern string))
	 (pattern (format "\\(?%s:%s\\)" group group-content)))
    (if required pattern (concat pattern "?"))))

(defun tlon-md-format-tag-with-attributes-from-user (required-p fun)
  "Prompt the user to set the value of tag attributes.
FUN is the function to read the attribute value. If REQUIRED-P is non-nil, keep
prompting the user until a non-empty string is entered."
  (let ((string (funcall fun)))
    (while (and required-p (string-empty-p string))
      (setq string (funcall fun)))
    string))

(defun tlon-md-get-tag-pattern (tag)
  "Return a regexp pattern that matches a TAG expression."
  (tlon-md-format-tag tag nil 'get-match-string))

;;;;;;; HTML

;;;###autoload
(defun tlon-html-insert-subscript ()
  "Insert an HTML `sub' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "sub"))

;;;###autoload
(defun tlon-html-insert-superscript ()
  "Insert an HTML `sup' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "sup"))

;;;###autoload
(defun tlon-html-insert-quote ()
  "Insert an HTML `q' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "q"))

;;;;;;; MDX

;;;###autoload
(defun tlon-mdx-insert-aside ()
  "Insert an MDX `Aside' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "Aside"))

(defun tlon-mdx-insert-cite ()
  "Insert a `Cite' tag pair of TYPE at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "Cite"))

(defun tlon-md-cite-bibkey-reader ()
  "Prompt the user to select the `bibKey' attribute value of the `Cite' tag."
  (car (citar-select-refs)))

(defvar tlon-locators)
(defun tlon-md-cite-locator-reader ()
  "Prompt the user to select the `locator' attribute value of the `Cite' tag."
  (let* ((selection (completing-read "Locator: " (push "" tlon-locators) nil t)))
    (alist-get selection tlon-locators nil nil 'string=)))

(defun tlon-md-cite-length-reader ()
  "Prompt the user to set the length of the `Cite' tag."
  (let ((length (completing-read "Type? " '("short" "long") nil t)))
    (if (string= length "short") " short" "")))

(defun tlon-md-figure-ignore-content-reader ()
  "Prompt the user whether to ignore the content of the `Figure' tag for TTS.
Returns \" ignore-content\" if yes, nil otherwise."
  (if (y-or-n-p "Ignore content for TTS? ")
      " ignore-content"
    nil))

(defun tlon-md--get-tag-attributes-by-src (tag file src)
  "In FILE, get attributes of TAG with matching SRC."
  (cl-block nil
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (let ((pattern (tlon-md-get-tag-pattern tag)))
          (while (re-search-forward pattern nil t)
            (let* ((attrs (tlon-get-tag-attribute-values tag))
		   (target-src (nth 0 attrs))
		   (match (pcase tag
			    ;; For Figure tags, we ignore the extension since some images
			    ;; have extensions different from their counterparts
			    ("Figure" (string-match (file-name-sans-extension src) target-src))
			    (_ (string= src target-src)))))
              (when match
		(cl-return attrs)))))))))

;;;###autoload
(defun tlon-mdx-insert-figure ()
  "Insert a `Figure' tag pair at point or around the selected region.
If the file is a translation in the `uqbar' subproject, this function attempts
to find the corresponding figure in the original file, translate its alt text,
and insert a `Figure' tag with the original `src' and translated `alt'.

Otherwise, it prompts the user to enter values for SRC and ALT. SRC is the
image URL, and ALT is the alt text."
  (interactive)
  (let* ((repo (tlon-get-repo-from-file (buffer-file-name)))
         (subproject (tlon-repo-lookup :subproject :dir repo))
         (subtype (tlon-repo-lookup :subtype :dir repo)))
    (if (and (string= subproject "uqbar")
             (eq subtype 'translations))
        (tlon-mdx-insert-translated-figure)
      (tlon-md-insert-or-edit-tag "Figure"))))

(declare-function tlon-get-counterpart "tlon-counterpart")
(declare-function tlon-get-image-counterpart "tlon-counterpart")
(declare-function tlon-deepl-translate "tlon-deepl")
(declare-function tlon-deepl-print-translation "tlon-deepl")
(declare-function tlon-images-get-dir "tlon-images")
(defun tlon-mdx-insert-translated-figure ()
  "Insert a translated Figure tag.
This function is for internal use. It is called by `tlon-mdx-insert-figure'
when the current buffer is a translation in the `uqbar' subproject."
  (interactive)
  (let* ((point (point-marker))
	 (action (if (tlon-looking-at-tag-p "Figure") 'edit 'insert))
	 (translated-src (pcase action
			   ('edit (expand-file-name (car (tlon-get-tag-attribute-values "Figure"))))
			   ('insert (file-truename (read-file-name "Image URL: " (tlon-images-get-dir) nil t)))))
	 (counterpart-file (tlon-get-counterpart))
	 (target-lang (tlon-get-language-in-file))
	 (source-lang "en")
	 (original-src (tlon-get-image-counterpart translated-src))
	 (relative-original-src (file-relative-name original-src (file-name-directory counterpart-file))))
    (if-let ((original-attrs (tlon-md--get-tag-attributes-by-src "Figure" counterpart-file relative-original-src)))
	(let* ((original-alt (nth 1 original-attrs)))
          (if original-alt
              (tlon-deepl-translate
	       original-alt target-lang source-lang
	       (lambda () (tlon-md--insert-translated-figure-callback translated-src point action))
	       t)
            (user-error "Could not find alt text for figure with src %s in %s" relative-original-src counterpart-file)))
      (user-error "Could not find figure with src %s in %s" relative-original-src counterpart-file))))

(defun tlon-md--insert-translated-figure-callback (src point action)
  "Callback function for `tlon-mdx-insert-translated-figure'.
SRC is the URL of the translated image. POINT is the marker position where the
`Figure' tag should be inserted. ACTION is one of `edit' or `insert', depending
on whether the tag should be edited or inserted."
  (let* ((translated-alt (tlon-deepl-print-translation))
         (content (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    "")))
    (with-current-buffer (marker-buffer point)
      (goto-char (marker-position point))
      (let* ((relative-src (file-relative-name
			    src (file-name-directory (buffer-file-name))))
             (values (list relative-src translated-alt))
	     (fun (pcase action
		    ('edit 'tlon-md-edit-tag)
		    ('insert 'tlon-md-return-tag)))
	     (common-args (list values content 'insert-values))
	     (all-args (pcase action
			 ('edit common-args)
			 ('insert (cons "Figure" common-args)))))
	(apply fun all-args)))))

;; TODO: offer language candidates
;;;###autoload
(defun tlon-mdx-insert-language ()
  "Insert an MDX `Language' tag pair at point or around the selected region.
Prompt the user to select a LANGUAGE. The enclosed text will be interpreted as
written in that language."
  (interactive)
  (tlon-md-return-tag "Language"))

;;;###autoload
(defun tlon-mdx-insert-literal-link ()
  "Insert an MDX `LiteralLink' tag pair at point or around the selected region.
Prompt the user to select a URL."
  (interactive)
  (tlon-md-insert-or-edit-tag "LiteralLink"))

;;;###autoload
(defun tlon-mdx-insert-math ()
  "Insert an `Math' tag pair of TYPE at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "Math")
  (message "To set the “alt” text with AI, call `tlon-ai-translate-math' (`H-r a m')."))

(defun tlon-md-math-display-reader ()
  "Prompt the user to set the display attribute for a `Math' tag."
  (let ((display (completing-read "Type? " '("inline" "display"))))
    (concat " " display)))

;;;###autoload
(defun tlon-mdx-insert-embedded ()
  "Insert an `Embedded' tag pair at point or around the selected region.
Prompt the user to enter a SRC value."
  (interactive)
  (tlon-md-insert-or-edit-tag "Embedded"))

;;;###autoload
(defun tlon-mdx-insert-replace-audio ()
  "Insert an MDX `ReplaceAudio' tag pair at point or around selected region.
Text enclosed by an `ReplaceAudio' tag pair will be displayed, but when narrated
the value of TEXT will be used instead."
  (interactive)
  (tlon-md-insert-or-edit-tag "ReplaceAudio"))

(defun tlon-md-replace-audio-voice-reader ()
  "Prompt user to select \"role\" attribute value for `ReplaceAudio', `VoiceRole'.
Allows an empty selection, which defaults to inheriting the surrounding voice
context.

Available roles:

- `\"\"` (empty string): Inherit the surrounding voice context (default).

- `\"inherit\"': Explicitly inherit the voice of the surrounding text.

- `\"main\"': Use the main text body voice.

- `\"alternate\"': Use the alternate voice (same gender as main) for elements
  like notes, quotes, asides, and listener cues.

- `\"male\"' / `\"female\"': Use a specific gender voice different from the main
  voice (matches the alternate voice of that gender). Useful for attributed
  quotes.

- `\"alternate gender\"': Use a voice of the opposite gender to the main voice."
  (completing-read "Role (leave empty for default): "
		   '("" "inherit" "main" "alternate" "male" "female" "alternate gender")
		   nil nil)) ; Allow empty input (require-match is nil)

;;;###autoload
(defun tlon-mdx-insert-romantlon-insert-mdx-roman ()
  "Insert an MDX `Roman' tag pair at point or around the selected region.
Text enclosed by an `Roman' tag pair will be displayed in small caps."
  (interactive)
  (tlon-md-insert-or-edit-tag "Roman"))

;;;###autoload
(defun tlon-mdx-insert-small-caps ()
  "Insert an MDX `SmallCaps' tag pair at point or around the selected region.
Text enclosed by an `SmallCaps' tag pair will be displayed in small caps.

Note: for Roman numerals, use `tlon-mdx-insert-romantlon-insert-mdx-roman':
`Roman' has exactly the same visual effects as `SmallCaps', but it also makes
the TTS engine read the numbers correctly."
  (interactive)
  (tlon-md-insert-or-edit-tag "SmallCaps"))

;; TODO: develop
;;;###autoload
(defun tlon-mdx-insert-table ()
  "Insert an MDX `Table' tag pair at point or around the selected region."
  (interactive)
  (message "This command is not yet developed."))

;;;###autoload
(defun tlon-mdx-insert-simple-table ()
  "Insert an MDX `SimpleTable' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "SimpleTable"))

(defun tlon-md-simple-table-include-reader ()
  "Prompt the user to set the `include' attribute for a `SimpleTable' tag."
  (let ((selection (completing-read "Include: " '("nothing" "everything" "body") nil t)))
    (format " include=\"%s\"" selection)))

;;;###autoload
(defun tlon-mdx-insert-visually-hidden ()
  "Insert an MDX `VisuallyHidden' tag pair at point or around selected region.
Text enclosed by a `VisuallyHidden' tag pair will be narrated, but not
displayed."
  (interactive)
  (tlon-md-insert-or-edit-tag "VisuallyHidden"))

;;;###autoload
(defun tlon-mdx-insert-voice-role ()
  "Insert an MDX `VoiceRole' tag pair at point or around region.
Text enclosed by a `VoiceRole' tag pair will be narrated in the
alternative voice, as opposed to the main voice."
  (interactive)
  (tlon-md-insert-or-edit-tag "VoiceRole"))

;;;;;;;; Notes

(defun tlon-insert-note-marker (marker &optional overwrite)
  "Insert note MARKER in the footnote at point.
If OVERWRITE is non-nil, replace the existing marker when present."
  (if-let ((fn-data (tlon-md-get-note-bounds nil 'content-only)))
      (let ((start (car fn-data)))
	(goto-char start)
	(let ((other-marker (car (remove marker (list (car (tlon-md-format-tag "Footnote" nil 'get-values))
						      (car (tlon-md-format-tag "Sidenote" nil 'get-values)))))))
	  (cond ((thing-at-point-looking-at (regexp-quote marker))
		 nil)
		((thing-at-point-looking-at (regexp-quote other-marker))
		 (when overwrite
		   (replace-match marker)))
		(t
		 (insert marker)))))
    (user-error "Not in a footnote")))

;;;###autoload
(defun tlon-insert-footnote-marker (&optional overwrite)
  "Insert a `Footnote' marker in the footnote at point.
Text enclosed by a `Footnote' tag pair will be displayed as a footnote, as
opposed to a sidenote.

If OVERWRITE is non-nil, or called interactively, replace the existing marker
when present."
  (interactive)
  (let ((overwrite (or overwrite (called-interactively-p 'any))))
    transient-current-command
    (tlon-insert-note-marker (car (tlon-md-format-tag "Footnote" nil 'insert-prompt)) overwrite)))

;;;###autoload
(defun tlon-insert-sidenote-marker (&optional overwrite)
  "Insert a `Sidenote' marker in the footnote at point.
Text enclosed by a `Sidenote' tag pair will be displayed as a sidenote, as
opposed to a footnote.

If OVERWRITE is non-nil, or called interactively, replace the existing marker
when present."
  (interactive)
  (tlon-insert-note-marker (car (tlon-md-format-tag "Sidenote" nil 'insert-prompt)) overwrite))

;;;;;;; SSML

;;;###autoload
(defun tlon-tts-insert-ssml-break ()
  "Insert a `break' SSML tag pair at point or around the selected region.
If TIME is nil, prompt the user to enter a value for the `time' attribute."
  (interactive)
  (tlon-md-insert-or-edit-tag "break"))

;;;###autoload
(defun tlon-tts-insert-ssml-emphasis ()
  "Insert a `emphasis' SSML tag pair at point or around the selected region.
If LEVEL is nil, prompt the user to select a value for the `level' attribute."
  (interactive)
  (tlon-md-insert-or-edit-tag "emphasis"))

(defun tlon-md-emphasis-level-reader ()
  "Prompt the user to set the `level' attribute value for a `emphasis' tag."
  (completing-read "`emphasis': "
		   tlon-md-ssml-emphasis-levels nil t nil nil
		   tlon-tts-ssml-default-emphasis-level))

;;;###autoload
(defun tlon-tts-insert-ssml-lang ()
  "Insert a `lang' SSML tag pair at point or around the selected region.
If LANGUAGE is nil. prompt the user to select a value for the `language'
attribute."
  (interactive)
  (tlon-md-insert-or-edit-tag "lang"))

;; TODO: check whether the two-letter code pairs should be separated by a hyphen or an underscore
(defun tlon-md-lang-reader ()
  "Prompt the user to select the `lang' attribute value of the `lang' tag."
  (tlon-select-language 'locale))

;;;###autoload
(defun tlon-tts-insert-ssml-phoneme ()
  "Insert a `phoneme' SSML tag pair at point or around the selected region.
If ALPHABET or PH are nil, prompt the user to select a value for the `alphabet'
and `ph' attributes."
  (interactive)
  (tlon-md-insert-or-edit-tag "phoneme"))

(defun tlon-md-phoneme-alphabet-reader ()
  "Prompt the user to select the `alphabet' attribute value for a `phoneme' tag."
  (completing-read "`alphabet': "
		   tlon-md-ssml-phoneme-alphabets nil t nil nil
		   tlon-tts-ssml-phoneme-default-alphabet))

;;;###autoload
(defun tlon-tts-insert-ssml-say-as ()
  "Insert a `say-as' SSML tag pair at point or around the selected region.
If INTERPRET-AS is nil, prompt the user to select a value for the `interpret-as'
attribute."
  (interactive)
  (tlon-md-insert-or-edit-tag "say-as"))

(defun tlon-md-say-as-interpret-as-reader ()
  "Prompt the user to select the `interpret-as' attribute value for a `say-as' tag."
  (completing-read "`interpret-as': " tlon-md-ssml-interpret-as-values))

;;;###autoload
(defun tlon-tts-insert-ssml-voice ()
  "Insert a `voice' SSML tag pair at point or around the selected region.
Prompts the user to select a value for the `name' attribute based on the
voices available for the `tlon-tts-global-engine'."
  (interactive)
  (tlon-md-insert-or-edit-tag "voice"))

;;;;;;;; Common functions

;;;;;;; Attribute readers

(defvar tlon-tts-global-engine)
(defvar tlon-tts-engines)
(defun tlon-md-voice-name-reader ()
  "Prompt the user to select the friendly `name` attribute value for a `voice` tag.
Uses the voices defined for the `tlon-tts-global-engine'. Returns the selected
friendly name (e.g., \"Brian\"), not the voice ID or the full display string."
  (let* ((engine-name tlon-tts-global-engine)
	 (voices-var (tlon-lookup tlon-tts-engines :voices-var :name engine-name))
	 (voices (when voices-var (symbol-value voices-var)))
	 (voices-cons (mapcar (lambda (voice-plist)
				(let* ((id (plist-get voice-plist :id))
				       (name (plist-get voice-plist :name))
				       (gender (plist-get voice-plist :gender))
				       (language (plist-get voice-plist :language))
				       (display-name (or name id))
				       (metadata (format "%-20.20s %-10.10s (%s)"
							 display-name gender language)))
				  (cons metadata id)))
			      voices)))
    (unless voices-cons
      (user-error "No voices configured for engine: %s" engine-name))
    (let* ((selected-display (completing-read (format "Voice for %s: " engine-name) voices-cons))
           (selected-id (cdr (assoc selected-display voices-cons)))
           (friendly-name (tlon-lookup voices :name :id selected-id)))
      friendly-name)))

;;;;;;; Misc

(defun tlon-md-tag-list ()
  "Return a list of all tag names in `tlon-tag-specs'."
  (let ((tags (mapcar (lambda (plist) (plist-get plist :tag)) tlon-tag-specs)))
    (when (called-interactively-p 'any)
      (message "%S" tags))
    tags))

;;;;; Notes

(defun tlon-md-get-note (&optional n content-only)
  "Return the note N.
If N is nil, return the note at point.

If CONTENT-ONLY is non-nil, return the beginning of the note content. The note
content is the note minus the marker that precedes it."
  (when-let* ((bounds (tlon-md-get-note-bounds n content-only))
	      (begin (car bounds))
	      (end (cdr bounds)))
    (string-trim (buffer-substring-no-properties begin end))))

(defun tlon-md-get-note-bounds (&optional n content-only)
  "Return the start and end positions of note N.
If N is nil, return the note at point.
If CONTENT-ONLY is non-nil, return the beginning of the note content. The note
content is the note minus the marker that precedes it."
  (save-excursion
    (when n
      (if-let ((id (concat "^" (number-to-string n))))
	  (goto-char (markdown-footnote-find-text id))
	(user-error "Note %d not found" n)))
    (when-let* ((begin (tlon-md-get-note-beginning content-only))
		(end (tlon-md-get-note-end)))
      (cons begin end))))

(defun tlon-md-get-note-beginning (&optional content-only)
  "Return the beginning of the note at point.
If CONTENT-ONLY is non-nil, return the beginning of the note content. The note
content is the note minus the marker that precedes it."
  (if-let ((fn-pos (markdown-footnote-text-positions))
	   (id (nth 0 fn-pos))
	   (start (nth 1 fn-pos)))
      (if content-only
	  (markdown-footnote-find-text id)
	start)
    (save-excursion
      (re-search-backward (format tlon-md-footnote-start "[[:digit:]]+") nil t)
      (if content-only
	  (match-end 0)
	(match-beginning 0)))))

(defun tlon-md-get-note-end ()
  "Return the end of the note at point."
  (save-excursion
    ;; Find the start of the current footnote definition.
    (goto-char (tlon-md-get-note-beginning))
    (let ((next-boundary most-positive-fixnum))
      ;; Find start of the next footnote definition
      (save-excursion
        ;; Search from end of current footnote line to avoid matching current footnote
        (end-of-line)
        (when (re-search-forward (format tlon-md-footnote-start "[[:digit:]]+") nil t)
          (setq next-boundary (min next-boundary (match-beginning 0)))))
      ;; Find start of local variables section
      (when-let ((lv-start (tlon-md-beginning-of-local-variables)))
        (setq next-boundary (min next-boundary lv-start)))
      ;; Consider buffer end
      (setq next-boundary (min next-boundary (point-max)))
      ;; The end is right before the next boundary, skipping trailing whitespace.
      (goto-char next-boundary)
      ;; If the boundary is the start of another section (footnote/local vars),
      ;; move back one char unless already at buffer start.
      (when (and (/= next-boundary (point-max)) (> (point) (point-min)))
        (backward-char 1))
      ;; Skip backward over whitespace characters (space, tab, newline)
      (skip-chars-backward " \t\n")
      ;; Ensure point is not before the start of the note
      (max (point) (tlon-md-get-note-beginning)))))

;;;;; Note classification

(defun tlon-auto-classify-note-at-point ()
  "Automatically classify note at point as either a footnote or a sidenote."
  (interactive)
  (let* ((note (tlon-md-get-note))
	 (type (tlon-note-automatic-type note)))
    (tlon-classify-note-at-point type 'overwrite)))

(defun tlon-auto-classify-notes-in-file (&optional file)
  "Automatically classify all notes in FILE.
If FILE is nil, use the current buffer."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward markdown-regex-footnote-definition nil t)
	  (tlon-auto-classify-note-at-point))))))

(defun tlon-auto-classify-notes-in-directory (&optional dir)
  "Automatically classify all notes in DIR.
If REPO is nil, use the current directory."
  (interactive)
  (let ((dir (or dir (file-name-directory default-directory))))
    (dolist (file (directory-files dir t "^[^.][^/]*$"))
      (when (string-match-p "\\.md$" file)
	(message "Classifying notes in %s" file)
	(tlon-auto-classify-notes-in-file file)))))

(defun tlon-get-note-type (&optional note)
  "Return the type of NOTE.
If NOTE is nil, use the note at point. A note type may be either `footnote' or
`sidenote'."
  (when-let ((note (or note (tlon-md-get-note))))
    (cond ((string-match (tlon-md-get-tag-pattern "Footnote") note)
	   'footnote)
	  ((string-match (tlon-md-get-tag-pattern "Sidenote") note)
	   'sidenote))))

(defun tlon-note-automatic-type (note)
  "Return the type into which NOTE is automatically classified.
The function implements the following classification criterion: if the note
contains at least one citation and no more than four words excluding citations,
it is classified as a footnote; otherwise, it is classified as a sidenote."
  (with-temp-buffer
    (let ((is-footnote-p)
	  (has-citation-p))
      (insert note)
      (goto-char (point-min))
      (while (re-search-forward (tlon-md-get-tag-pattern "Cite") nil t)
	(replace-match "")
	(setq has-citation-p t))
      (when has-citation-p
	(let ((words (count-words-region (point-min) (point-max))))
	  (when (<= words 4)
	    (setq is-footnote-p t))))
      (if is-footnote-p 'footnote 'sidenote))))

(defun tlon-classify-note-at-point (&optional type overwrite)
  "Classify note at point as a note of TYPE.
TYPE can be either `footnote' o `sidenote'. If TYPE is nil, prompt the user for
a type.

If OVERWRITE is non, replace the existing marker when present."
  (interactive)
  (let ((type (or type (completing-read "Type: " '("footnote" "sidenote") nil t))))
    (pcase type
      ('footnote (tlon-insert-footnote-marker overwrite))
      ('sidenote (tlon-insert-sidenote-marker overwrite))
      (_ (user-error "Invalid type")))))

;;;;; Images

(defun tlon-offset-timestamps (offset)
  "Increase all MM:SS timestamps in the current buffer by OFFSET."
  (interactive "sEnter time offset (MM:SS): ")
  (save-excursion
    (goto-char (point-min))
    (let ((minutes (string-to-number (substring offset 0 2)))
	  (seconds (string-to-number (substring offset 3 5))))
      (while (re-search-forward "\\[\\([0-9]+\\):\\([0-9]+\\)\\]" nil t)
	(let* ((original-minutes (string-to-number (match-string 1)))
	       (original-seconds (string-to-number (match-string 2)))
	       (total-seconds (+ (* original-minutes 60) original-seconds
				 (* minutes 60) seconds))
	       (final-minutes (floor (/ total-seconds 60)))
	       (final-seconds (% total-seconds 60))
	       (new-timestamp (format "[%02d:%02d]" final-minutes final-seconds)))
	  (replace-match new-timestamp t nil))))))

;;;;; Misc

;;;###autoload
(defun tlon-ensure-markdown-mode ()
  "Check if the current buffer is in a Markdown-derived mode."
  (unless (derived-mode-p 'markdown-mode)
    (user-error "Not in a Markdown buffer")))

(defun tlon-insert-special-character (char)
  "Insert a special CHAR at point.
The list of completion candidates can be customized via the user option
`tlon-md-special-characters'."
  (interactive (list (completing-read "Character: " tlon-md-special-characters nil t)))
  (insert (alist-get char tlon-md-special-characters nil nil #'string= )))

;;;;; Actual content

;; TODO: make it work twice consecutively
;;;###autoload
(defun tlon-md-beginning-of-buffer-dwim ()
  "Move point to the beginning of the relevant part of the buffer.
The relevant part of the buffer is the part of the buffer that excludes the
metadata section.

If this function is called twice consecutively, it will move the point to the
end of the buffer unconditionally."
  (interactive)
  (goto-char (tlon-md-beginning-of-content)))

;; TODO: make it work twice consecutively
;;;###autoload
(defun tlon-md-end-of-buffer-dwim ()
  "Move point to the end of the relevant part of the buffer.
The relevant part of the buffer is the part of the buffer that excludes the
\"local variables\" section.

If this function is called twice consecutively, it will move the point to the
end of the buffer unconditionally."
  (interactive)
  (goto-char (tlon-md-end-of-content)))

(defun tlon-md-beginning-of-content ()
  "Return the position of the beginning of the content in the current buffer."
  (or (tlon-md-end-of-metadata) (point-min)))

(defun tlon-md-end-of-content ()
  "Return the position of the end of the content in the current buffer."
  (or (tlon-md-beginning-of-local-variables) (point-max)))

(defun tlon-md-beginning-of-local-variables ()
  "Return the position of the beginning of the local variables section."
  (when-let ((cons (tlon-get-delimited-region-pos
		    tlon-md-local-variables-line-start
		    tlon-md-local-variables-line-end)))
    (car cons)))

(defun tlon-md-end-of-metadata ()
  "Return the position of the end of the metadata section."
  (when-let ((cons (tlon-get-delimited-region-pos
		    (concat "^" tlon-yaml-delimiter))))
    (cdr cons)))

(defun tlon-md-get-local-variables ()
  "Get the text in the \"local variables\" section of the current buffer."
  (when-let ((beg (tlon-md-beginning-of-local-variables)))
    (string-trim (buffer-substring-no-properties beg (point-max)))))

(defun tlon-md-get-metadata ()
  "Get the text in the metadata section of the current buffer."
  (when-let ((end (tlon-md-end-of-metadata)))
    (string-trim (buffer-substring-no-properties (point-min) end))))

;;;###autoload
(defun tlon-md-read-content (&optional file)
  "Read the substantive content of FILE.
The substantive content of a file is the file minus the metadata and the local
variables section. If FILE is nil, read the file visited by the current buffer."
  (let ((file (or file (buffer-file-name)))
	(begin (tlon-md-beginning-of-content))
	(end (tlon-md-end-of-content)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties begin end))))

;;;;; Menu

;;;###autoload (autoload 'tlon-md-menu "tlon-md" nil t)
(transient-define-prefix tlon-md-menu ()
  "Dispatch a `tlon' command for Markdown insertion."
  :info-manual "(tlon) Editing Markdown"
  [["YAML"
    ("y" "field"                tlon-edit-yaml-field)]
   ["TTS"
    ("t b" "break"              tlon-tts-insert-ssml-break)
    ("t e" "emphasis"           tlon-tts-insert-ssml-emphasis)
    ("t l" "lang"               tlon-tts-insert-ssml-lang)
    ("t o" "voice role"         tlon-mdx-insert-voice-role)
    ("t p" "phoneme"            tlon-tts-insert-ssml-phoneme)
    ("t r" "replace audio"      tlon-mdx-insert-replace-audio)
    ("t s" "say-as"             tlon-tts-insert-ssml-say-as)
    ("t v" "voice"              tlon-tts-insert-ssml-voice) ; Added voice tag command
    ("t V" "visually hidden"    tlon-mdx-insert-visually-hidden)] ; Changed shortcut for visually hidden
   ["Note markers"
    ("f" "footnote"             (lambda () (interactive) (tlon-insert-footnote-marker 'overwrite)))
    ("s" "sidenote"             (lambda () (interactive) (tlon-insert-sidenote-marker 'overwrite)))
    ("n" "auto: at point"       tlon-auto-classify-note-at-point)
    ("N" "auto: in file"        tlon-auto-classify-notes-in-file)
    ""
    "Citations"
    ("c" "cite"                 tlon-mdx-insert-cite)
    ""
    "Quotes"
    ("i" "inline"               tlon-html-insert-quote)
    ("b" "blockquote"           markdown-insert-blockquote)
    ]
   ["Images"
    ("g" "figure"               tlon-mdx-insert-figure)
    ("o" "Embedded"             tlon-mdx-insert-embedded)
    ""
    "Link"
    ("k" "internal"             tlon-insert-internal-link)
    ("e" "literal"              tlon-mdx-insert-literal-link)
    ""
    "Subscripts and superscripts"
    ("," "subscript"            tlon-html-insert-subscript)
    ("/" "superscript"          tlon-html-insert-superscript)]
   ["Misc"
    ("a" "aside"                tlon-mdx-insert-aside)
    ("l" "language"             tlon-mdx-insert-language)
    ("m" "Math"                 tlon-mdx-insert-math)
    ("." "special character"    tlon-insert-special-character)
    ("RET" "open src"           tlon-md-open-src-file)
    ""
    "Caps"
    ("p" "small caps"           tlon-mdx-insert-small-caps)
    ("r" "roman"                tlon-mdx-insert-romantlon-insert-mdx-roman)
    ""
    "Table"
    ("A" "table"                tlon-mdx-insert-table)
    ("T" "simple table"         tlon-mdx-insert-simple-table)]])

(provide 'tlon-md)
;;; tlon-md.el ends here
