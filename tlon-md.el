;;; tlon-md.el --- Markdown functionality for the Babel project -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon
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

;; Markdown functionality for the Babel project.

;;; Code:

(require 'markdown-mode-extras)
(require 'tlon-core)
(require 'tlon-yaml)

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
  "\\(?1:\\(?:^>.*\n\\)+\\)"
  "Pattern to match a blockquote in a Markdown file.
The pattern captures the entire blockquote, including the quote markers. This is
to allow for capturing multi-line quotes. To get the actual quote only, remove
them from the captured string as part of the post-processing.")

;;;;;; Math

(defconst tlon-md-math-power
  "\\b\\(?1:[[:digit:]]+\\)<sup>\\(?2:[[:digit:]]+\\)</sup>\\b"
  "Regexp pattern for matching a number raised to a power.
The first group captures the base, and the second group captures
the exponent.")

;;;;;; SSML

;;;;;;; `emphasis'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#emphasis-tag

(defconst tlon-md-ssml-emphasis-levels
  '("none" "reduced" "moderate" "strong")
  "Admissible values for the `level' attribute of the `emphasis' SSML tag.")

(defconst tlon-tts-ssml-default-emphasis-level
  "moderate"
  "Default value for the `level' attribute of the `emphasis' SSML tag.")

;;;;;;; `phoneme'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#phoneme-tag

(defconst tlon-md-ssml-phoneme-alphabets
  '("ipa" "x-sampa")
  "Admissible values for the `alphabet' attribute of the `phoneme' SSML tag.")

(defconst tlon-tts-ssml-phoneme-default-alphabet
  "ipa"
  "Default value for the `alphabet' attribute of the `phoneme' SSML tag.")

;;;;;;; `prosody'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#prosody-tag

;; not currently supported

;;;;;;; `say-as'
;; https://docs.aws.amazon.com/polly/latest/dg/supportedtags.html#say-as-tag

(defconst tlon-md-ssml-interpret-as-values
  '("cardinal" "ordinal" "digits" "fraction" "unit" "date" "time" "telephone" "address" "interjection" "expletive" "spell-out" "characters" "verbatim")
  "Admissible values for the `interpret-as' attribute of the SSML tag.")

;;;;;; MDX

;;;;;;; `Cite'

(defconst tlon-mdx-cite-pattern-formatter
  "<Cite bibKey=\"\\(?1:.*?\\)\\(?:, \\(?2:.*?\\)\\)?\"\\(?:%s />\\|>.*?</Cite>\\)"
  "Formatter for `Cite' patterns.")

(defconst tlon-mdx-cite-pattern-short-element
  "\\(?3: short\\)"
  "Pattern that matches a short citation flag in the third capture group.
To use in conjunction with `tlon-mdx-cite-pattern-formatter'.")

(defconst tlon-mdx-cite-pattern
  (format tlon-mdx-cite-pattern-formatter (concat tlon-mdx-cite-pattern-short-element "?"))
  "Pattern to match a citation in a Markdown file.
The first group captures the BibTeX key, the second group captures the
locator(s), if present, and the third group captures the short citation flag, if
present.")

(defconst tlon-mdx-cite-pattern-long
  (format tlon-mdx-cite-pattern-formatter "")
  "Pattern to match a long citation in a Markdown file.
The first group captures the BibTeX key, the second group captures the
locator(s), if present.")

(defconst tlon-mdx-cite-pattern-short
  (format tlon-mdx-cite-pattern-formatter tlon-mdx-cite-pattern-short-element)
  "Pattern to match a short citation in a Markdown file.
The first group captures the BibTeX key, the second group captures the
locator(s), if present, and the third group captures the short citation flag.")

;;;;;;; `Table'

;; TODO: develop
;; https://github.com/tlon-team/uqbar-issues#mdx-tag-syntax

;;;;;; Common constants

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
		       (:name "short" :required nil :valued nil :group 4 :reader tlon-md-cite-length-reader)
		       ;; TODO: currently, the locator attribute is not
		       ;; introduced explicitly as the value of a named
		       ;; attribute. Fix this.
		       )
	  :type mdx
	  :self-closing t)
    (:tag "emphasis"
	  :attributes ((:name "level" :required t :valued t :group 3 :reader tlon-md-emphasis-level-reader))
	  :type ssml
	  :self-closing nil
	  :doc "")
    (:tag "Figure"
	  :attributes ((:name "src" :required t :valued t  :group 3 :prompt "Image URL: ")
		       (:name "alt" :required nil :valued t :group 5 :prompt "Alt text: "))
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
    (:tag "OurWorldInData"
	  :attributes ((:name "src" :required t :valued t :group 3 :prompt "Chart URL: ")
		       (:name "alt" :required nil :valued t :group 5 :prompt "Alt text: "))
	  :type mdx
	  :self-closing t
	  :doc "")
    (:tag "phoneme"
	  :attributes ((:name "alphabet" :required t :valued t :group 3 :reader tlon-md-phoneme-alphabet-reader)
		       ;; TODO: integrate with AI
		       (:name "ph" :required t :valued t :group 5 :prompt "Phonetic symbols: "))
	  :type ssml
	  :self-closing nil
	  :doc "")
    (:tag "ReplaceAudio"
	  :attributes ((:name "text" :required t :valued t :group 3 :prompt "Audio text: "))
	  :type mdx
	  :self-closing nil
	  :doc "Encloses text be read based on the value of `text'.")
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
		       (:name "header" :required nil :valued nil :group 5 :reader tlon-md-simple-table-header-reader))
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
    (:tag "VisuallyHidden"
	  :type mdx
	  :self-closing nil
	  :doc "Text to be narrated, but not displayed.")
    (:tag "voice"
	  :attributes ((:name "name" :required t :valued t :group 3 :prompt "Voice name: "))
	  :type ssml
	  :self-closing nil
	  :doc ""))
  "Property list of MDX tag attributes and patterns.
- `:group': the group number that captures the attribute. The value of the
  attribute is captured in a separate group, numbered `:group' + 1. The entire
  tag expression is captured in group number 1, and the text enclosed by the
  tags, when the tag is not self-closing, is captured in group number 2.")

;;;; Functions

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
	 (language (tlon-get-language))
	 current-element-title)
    (when current-target
      (setq current-element-title
	    (tlon-md-get-title-in-link-target
	     current-target)))
    (let* ((candidates (tlon-metadata-get-values-of-all-types language 'current-repo))
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

;;;;;; Tag handling

(defun tlon-md-insert-or-edit-tag (tag)
  "Insert or edit an MDX TAG pair at point or around the selected region."
  (if (tlon-looking-at-tag-p tag)
      (tlon-md-edit-tag)
    (tlon-md-insert-tag tag)))

(defun tlon-looking-at-tag-p (tag)
  "Return t iff point is looking at TAG."
  (let ((pattern (tlon-md-format-tag tag nil 'match-string)))
    (thing-at-point-looking-at pattern)))

(defun tlon-md-edit-tag (&optional values content format)
  "Edit tag at point.
Optionally, set the attribute VALUES and the tag CONTENT. FORMAT is one of
`match-string', `to-fill', `filled' or `inserted'. If `match-string', return a
regex pattern to match the tag and capture its attribute values. Otherwise,
return a cons cell with the tag pair or the opening tag if it is self-closing.
If `filled', return it filled with VALUES. If `inserted', prompt the user to
insert VALUES. If `to-fill', return it filled with placeholders."
  (let* ((tag (tlon-get-tag-at-point))
	 (values (or values (tlon-get-tag-attribute-values tag)))
	 (content (or content (match-string 2))))
    (replace-match "")
    (tlon-md-insert-tag tag values content format)))

(defun tlon-md-set-tag-attribute-value (tag attribute value)
  "Set the VALUE of ATTRIBUTE for TAG."
  (let ((values (tlon-get-tag-attribute-values tag))
	(pos (cl-position attribute (tlon-get-tag-attribute-names tag) :test #'string=)))
    (setcar (nthcdr pos values) value)
    values))

(defun tlon-md-insert-attribute-value (attribute value)
  "Insert an ATTRIBUTE VALUE in the tag at point.
If the tag already contains an attribute with the same name, replace its value."
  (let ((tag (tlon-get-tag-at-point)))
    (tlon-md-edit-tag (tlon-md-set-tag-attribute-value tag attribute value) nil 'filled)))

(defun tlon-md-insert-tag (tag &optional values content format)
  "Insert a TAG or TAG pair at point or around the selected region.
VALUES is a list of attribute values. When non-nil, offer each value as the
initial input when prompting the user for the attribute value. CONTENT is the
text the tag pair encloses. FORMAT is one of `match-string', `to-fill', `filled'
or `inserted'. If `match-string', return a regex pattern to match the tag and
capture its attribute values. Otherwise, return a cons cell with the tag pair or
the opening tag if it is self-closing. If `filled', return it filled with
VALUES. If `inserted', prompt the user to insert VALUES. If `to-fill', return it
filled with placeholders."
  ;; TODO: add check that the length of the VALUES list matches the number of
  ;; tag attributes, and is nil when it takes no attributes. Similarly for
  ;; CONTENT and self-closing tag.
  (let ((format (or format 'inserted)))
    (cl-destructuring-bind (open . close) (tlon-md-format-tag tag values format)
      (if close
	  (if (use-region-p)
	      (let ((begin (region-beginning)))
		(goto-char (region-end))
		(insert close)
		(goto-char begin)
		(insert open))
	    (insert (concat open (or content "") close)))
	(insert open)))))

(defun tlon-md-format-tag (tag &optional values format)
  "Return TAG with attribute VALUES in the appropriate format.
FORMAT is one of `match-string', `to-fill', `filled' or `inserted'. If
`match-string', return a regex pattern to match the tag and capture its
attribute values. Otherwise, return a cons cell with the tag pair or the opening
tag if it is self-closing. If `filled', return it filled with VALUES. If
`inserted', prompt the user to insert VALUES. If `to-fill', return it filled
with placeholders."
  (let* ((attributes (tlon-md-format-tag-with-attributes tag values format))
	 (self-closing-p (tlon-lookup tlon-tag-specs :self-closing :tag tag))
	 (format-string (if self-closing-p "\\(?1:%s\\)" "\\(?1:%s\\(?2:\\(.\\|\n\\)*?\\)%s\\)"))
	 (open-ending (if self-closing-p " />" ">"))
	 (open (concat "<" tag attributes open-ending))
	 (close (unless self-closing-p (concat "</" tag ">"))))
    (pcase format
      ('match-string (format format-string open close))
      ((or 'to-fill 'filled 'inserted) (if self-closing-p (list open) (cons open close))))))

(defun tlon-md-get-tag-filled (tag &optional values content)
  "Return a TAG string with VALUES and CONTENT.
VALUES is a list of attribute values. When non-nil, offer each value as the
initial input when prompting the user for the attribute value. CONTENT is the
text the tag pair encloses."
  (cl-destructuring-bind (open . close)
      (tlon-md-format-tag tag values 'filled)
    (if close (concat open (or content "") close) open)))

(defun tlon-md-get-tag-to-fill (tag)
  "Return a TAG string with placeholders for content and attribute values."
  (cl-destructuring-bind (open . close)
      (tlon-md-format-tag tag nil 'to-fill)
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
			 attributes)))
    (if values (mapcar #'1+ groups) groups)))

(defun tlon-get-tag-attribute-names (tag)
  "Return a list of all attribute names of TAG."
  (let* ((attributes (tlon-lookup tlon-tag-specs :attributes :tag tag))
	 names)
    (dolist (attribute attributes (reverse names))
      (push (plist-get attribute :name) names))))

(defun tlon-get-tag-attribute-values (tag)
  "Return a list of all attribute values of TAG at point."
  (when (tlon-looking-at-tag-p tag)
    (mapcar (lambda (group)
	      (match-string-no-properties group))
	    (tlon-get-tag-groups tag 'values))))

(defun tlon-md-lookup-tag-attribute-property (tag name property)
  "For the attribute named NAME in TAG, return the value of the PROPERTY."
  (let ((attributes (tlon-lookup tlon-tag-specs :attributes :tag tag)))
    (tlon-lookup attributes property :name name)))

(defun tlon-tag-pair-with-attributes (element &rest attributes)
  "Construct a tag ELEMENT with one or more ATTRIBUTES."
  (cons (apply 'format (car element) attributes)
	(cdr element)))

(defun tlon-md-format-tag-with-attributes (tag &optional values format)
  "Format TAG attributes.
FORMAT is one of `match-string', `to-fill', `filled' or `inserted'. If
`match-string', return a regex string pattern to match the tag attributes and
capture its values. If `filled', return string with the attributes filled with
VALUES. If `inserted', prompt the user to insert VALUES. If `to-fill', return it
filled with placeholders."
  (let* ((names (tlon-get-tag-attribute-names tag))
	 (list (if values
		   (cl-mapcar #'cons names values)
		 (mapcar #'list names)))
	 formatted)
    (dolist (cons list formatted)
      (let* ((name (car cons))
	     (value (cdr cons))
	     (format-string (tlon-md-format-attribute-with-placeholder
			     tag name (eq format 'match-string)))
	     (required-p (tlon-md-lookup-tag-attribute-property tag name :required))
	     (group (tlon-md-lookup-tag-attribute-property tag name :group))
	     (fun (or (tlon-md-lookup-tag-attribute-property tag name :reader)
		      (lambda ()
			(read-string (tlon-md-lookup-tag-attribute-property tag name :prompt) value))))
	     (string (pcase format
		       ('inserted (tlon-md-format-tag-with-attributes-from-user required-p fun))
		       ('filled value))))
	(setq formatted
	      (concat formatted
		      (pcase format
			('match-string (tlon-make-attribute-pattern-searchable format-string group required-p))
			((or 'filled 'inserted) (format format-string string))
			('to-fill format-string))))))))

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
  (tlon-md-format-tag tag nil 'match-string))

;;;;;;; HTML

;;;###autoload
(defun tlon-insert-html-subscript ()
  "Insert an HTML `sub' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "sub"))

;;;###autoload
(defun tlon-insert-html-superscript ()
  "Insert an HTML `sup' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "sup"))

;;;;;;; MDX

;;;###autoload
(defun tlon-insert-mdx-alternative-voice ()
  "Insert an MDX `AlternativeVoice' tag pair at point or around region.
Text enclosed by a `AlternativeVoice' tag pair will be narrated in the
alternative voice, as opposed to the main voice."
  (interactive)
  (tlon-md-insert-or-edit-tag "AlternativeVoice"))

;;;###autoload
(defun tlon-insert-mdx-aside ()
  "Insert an MDX `Aside' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "Aside"))

(defun tlon-insert-mdx-cite ()
  "Insert a `Cite' tag pair of TYPE at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "Cite"))

(defun tlon-md-cite-bibkey-reader ()
  "Prompt the user to select the `bibKey' attribute value of the `Cite' tag."
  (car (citar-select-refs)))

(defun tlon-md-cite-length-reader ()
  "Prompt the user to set the length of the `Cite' tag."
  (let ((length (completing-read "Type? " '("short" "long") nil t)))
    (if (string= length "short") " short" "")))

;;;###autoload
(defun tlon-insert-mdx-figure ()
  "Insert a `Figure' tag pair at point or around the selected region.
Prompt the user to enter values for SRC and ALT. SRC is the image URL, and ALT
is the alt text."
  (interactive)
  (tlon-md-insert-or-edit-tag "Figure"))

;; TODO: offer language candidates
;;;###autoload
(defun tlon-insert-mdx-language ()
  "Insert an MDX `Language' tag pair at point or around the selected region.
Prompt the user to select a LANGUAGE. The enclosed text will be interpreted as
written in that language."
  (interactive)
  (tlon-md-insert-tag "Language"))

;;;###autoload
(defun tlon-insert-mdx-literal-link ()
  "Insert an MDX `LiteralLink' tag pair at point or around the selected region.
Prompt the user to select a URL."
  (interactive)
  (tlon-md-insert-or-edit-tag "LiteralLink"))

;; TODO: create command to set alt text; integrate with AI
(defun tlon-insert-mdx-math ()
  "Insert an `Math' tag pair of TYPE at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "Math"))

(defun tlon-md-math-display-reader ()
  "Prompt the user to set the display attribute for a `Math' tag."
  (let ((display (completing-read "Type? " '("inline" "display"))))
    (if (string= display "display") " display" "")))

;;;###autoload
(defun tlon-insert-mdx-our-world-in-data ()
  "Insert an `OurWorldInData' tag pair at point or around the selected region.
Prompt the user to enter a SRC value."
  (interactive)
  (tlon-md-insert-or-edit-tag "OurWorldInData"))

;;;###autoload
(defun tlon-insert-mdx-replace-audio ()
  "Insert an MDX `ReplaceAudio' tag pair at point or around selected region.
Text enclosed by an `ReplaceAudio' tag pair will be displayed, but when narrated
the value of TEXT will be used instead."
  (interactive)
  (tlon-md-insert-or-edit-tag "ReplaceAudio"))

;;;###autoload
(defun tlon-insert-mdx-roman ()
  "Insert an MDX `Roman' tag pair at point or around the selected region.
Text enclosed by an `Roman' tag pair will be displayed in small caps."
  (interactive)
  (tlon-md-insert-or-edit-tag "Roman"))

;;;###autoload
(defun tlon-insert-mdx-small-caps ()
  "Insert an MDX `SmallCaps' tag pair at point or around the selected region.
Text enclosed by an `SmallCaps' tag pair will be displayed in small caps.

Note: for Roman numerals, use `tlon-insert-mdx-roman': `Roman' has exactly the
same visual effects as `SmallCaps', but it also makes the TTS engine read the
numbers correctly."
  (interactive)
  (tlon-md-insert-or-edit-tag "SmallCaps"))

;;;###autoload
(defun tlon-insert-mdx-simple-table ()
  "Insert an MDX `SimpleTable' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-or-edit-tag "SimpleTable"))

(defun tlon-md-simple-table-header-reader ()
  "Prompt the user to set the header attribute for a `SimpleTable' tag."
  (if (y-or-n-p "Omit header? ")
      " omit-header" ""))

;;;###autoload
(defun tlon-insert-mdx-visually-hidden ()
  "Insert an MDX `VisuallyHidden' tag pair at point or around selected region.
Text enclosed by a `VisuallyHidden' tag pair will be narrated, but not
displayed."
  (interactive)
  (tlon-md-insert-or-edit-tag "VisuallyHidden"))

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

;;;;;;;; To revise

;;;;;;;;;; Locators

(defvar tlon-locators)
(defun tlon-insert-locator ()
  "Insert locator in citation at point.
If point is on a locator, it will be replaced by the new one. Otherwise, the new
locator will be inserted after the key, if there are no locators, or at the end
of the existing locators."
  (interactive)
  (unless (tlon-looking-at-tag-p "Cite")
    (user-error "Not in a citation"))
  (let* ((selection (completing-read "Locator: " tlon-locators nil t))
	 (locator (alist-get selection tlon-locators nil nil 'string=)))
    (let ((key (car (tlon-get-tag-attribute-values "Cite"))))
      (forward-char)
      (re-search-backward "<" nil t)
      (re-search-forward key nil t)
      (if-let ((existing (tlon-get-locator-at-point)))
	  (replace-match locator)
	(insert (format ", %s " locator))))))

(defun tlon-get-locator-at-point ()
  "Return the locator at point, if present."
  (let ((locators (mapcar 'cdr tlon-locators)))
    (when (thing-at-point-looking-at (regexp-opt locators))
      (match-string-no-properties 0))))

;;;;;;;;; `Footnote'

(defun tlon-insert-footnote-marker (&optional overwrite)
  "Insert a `Footnote' marker in the footnote at point.
Text enclosed by a `Footnote' tag pair will be displayed as a footnote, as
opposed to a sidenote.

If OVERWRITE is non-nil, or called interactively, replace the existing marker
when present."
  (interactive)
  (let ((overwrite (or overwrite (called-interactively-p 'any))))
    transient-current-command
    (tlon-insert-note-marker (car (tlon-md-format-tag "Footnote" nil 'inserted)) overwrite)))

;;;###autoload
(defun tlon-insert-note-marker (marker &optional overwrite)
  "Insert note MARKER in the footnote at point.
If OVERWRITE is non-nil, replace the existing marker when present."
  (if-let ((fn-data (tlon-note-content-bounds)))
      (let ((start (car fn-data)))
	(goto-char start)
	(let ((other-marker (car (remove marker (list (car (tlon-md-format-tag "Footnote" nil 'inserted))
						      (car (tlon-md-format-tag "Sidenote" nil 'inserted)))))))
	  (cond ((thing-at-point-looking-at (regexp-quote marker))
		 nil)
		((thing-at-point-looking-at (regexp-quote other-marker))
		 (when overwrite
		   (replace-match marker)))
		(t
		 (insert marker)))))
    (user-error "Not in a footnote")))

;;;;;;;;; `Sidenote'

;;;###autoload
(defun tlon-insert-sidenote-marker (&optional overwrite)
  "Insert a `Sidenote' marker in the footnote at point.
Text enclosed by a `Sidenote' tag pair will be displayed as a sidenote, as
opposed to a footnote.

If OVERWRITE is non-nil, or called interactively, replace the existing marker
when present."
  (interactive)
  (tlon-insert-note-marker (car (tlon-md-format-tag "Sidenote" nil 'inserted)) overwrite))

;;;;;;;;; `Table'

;; TODO: develop
;; https://github.com/tlon-team/uqbar-issues#mdx-tag-syntax

;;;;;;;; Common functions

;;;;; Note classification

(defun tlon-auto-classify-note-at-point ()
  "Automatically classify note at point as either a footnote or a sidenote."
  (interactive)
  (let* ((note (tlon-get-note-at-point))
	 (type (tlon-note-automatic-type note)))
    (tlon-classify-note-at-point type 'overwrite)))

(defun tlon-note-content-bounds ()
  "Return the start and end positions of the content of the note at point.
The content of a note is its substantive part of the note, i.e. the note minus
the marker that precedes it."
  (when-let* ((fn-pos (markdown-footnote-text-positions))
	      (id (nth 0 fn-pos))
	      (begin (nth 1 fn-pos))
	      (end (nth 2 fn-pos))
	      (fn (buffer-substring-no-properties begin end)))
    ;; regexp pattern copied from `markdown-footnote-kill-text'
    (string-match (concat "\\[\\" id "\\]:[[:space:]]") fn)
    (cons (+ begin (match-end 0)) end)))

;; FIXME: fails in multi-paragraph notes
(defun tlon-get-note-at-point ()
  "Get the note at point, if any."
  (when-let* ((bounds (tlon-note-content-bounds))
	      (begin (car bounds))
	      (end (cdr bounds)))
    (string-trim (buffer-substring-no-properties begin end))))

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
  (when-let ((note (or note (tlon-get-note-at-point))))
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
      (while (re-search-forward tlon-mdx-cite-pattern nil t)
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
		    tlon-yaml-delimiter)))
    (cdr cons)))

(defun tlon-md-get-local-variables ()
  "Get the text in the \"local variables\" section of the current buffer."
  (when-let ((beg (tlon-md-beginning-of-local-variables)))
    (string-trim (buffer-substring-no-properties beg (point-max)))))

(defun tlon-md-get-metadata ()
  "Get the text in the metadata section of the current buffer."
  (when-let ((end (tlon-md-end-of-metadata)))
    (string-trim (buffer-substring-no-properties (point-min) end))))

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
   ["Link"
    ("k" "internal"             tlon-insert-internal-link)
    ("t" "literal"              tlon-insert-mdx-literal-link)]
   ["Note markers"
    ("f" "footnote"             (lambda () (interactive) (tlon-insert-footnote-marker 'overwrite)))
    ("s" "sidenote"             (lambda () (interactive) (tlon-insert-sidenote-marker 'overwrite)))
    ("n" "auto: at point"       tlon-auto-classify-note-at-point)
    ("N" "auto: in file"        tlon-auto-classify-notes-in-file)]
   ["Citations"
    ("c" "cite"                 tlon-insert-mdx-cite)
    ;; ("c" "cite long"            tlon-insert-mdx-cite-long)
    ;; ("C" "cite short"           tlon-insert-mdx-cite-short)
    ;; ("l" "locator"              tlon-insert-locator)
    ]
   ["TTS"
    ("V" "alternative voice"    tlon-insert-mdx-alternative-voice)
    ("K" "break"                tlon-tts-insert-ssml-break)
    ("E" "emphasis"             tlon-tts-insert-ssml-emphasis)
    ("L" "lang"                 tlon-tts-insert-ssml-lang)
    ("P" "phoneme"              tlon-tts-insert-ssml-phoneme)
    ("A" "replace audio"        tlon-insert-mdx-replace-audio)
    ("S" "say-as"               tlon-tts-insert-ssml-say-as)
    ("H" "visually hidden"      tlon-insert-mdx-visually-hidden)
    ("T" "simple table"         tlon-insert-mdx-simple-table)]
   ["Misc"
    ("b" "subscript"            tlon-insert-html-subscript)
    ("p" "superscript"          tlon-insert-html-superscript)
    ("." "special character"    tlon-insert-special-character)
    ""
    ("F" "figure"               tlon-insert-mdx-figure)
    ("o" "Our World In Data"    tlon-insert-mdx-our-world-in-data)]
   [""
    ("a" "aside"                tlon-insert-mdx-aside)
    ;; ("t" "table"                ) ; placeholder while I develop the command
    ("g" "language"             tlon-insert-mdx-language)
    ("m" "Math"                 tlon-insert-mdx-math)
    ""
    ("M" "small caps"           tlon-insert-mdx-small-caps)
    ("r" "roman"                tlon-insert-mdx-roman)]])

(provide 'tlon-md)
;;; tlon-md.el ends here
