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

;;;;; Cite

(defconst tlon-cite-pattern
  "<Cite bibKey={\"\\(.*?\\)\\(, .*?\\)?\"}\\(\\( short\\)? />\\|>.*?</Cite>\\)"
  "Pattern to match a citation in a Markdown file.
The first group captures the bibTeX key, the second group captures the locators,
and the third group captures the short citation flag.")

;;;;; Note markers

(defconst tlon-footnote-marker "<Footnote />"
  "Marker for a footnote in a `Cite' MDX element.")

(defconst tlon-sidenote-marker "<Sidenote />"
  "Marker for a sidenote in a `Cite' MDX element.")

;;;;; Numbers

(defconst tlon-md-number-separators
  '(("en" . ",")
    ("es" . " "))
  "Number separators for different languages.")

(defconst tlon-md-number-separator-pattern
  "\\([[:digit:]]+?\\)%s\\([[:digit:]]+?\\)"
  "Pattern to match numbers separated by language-specific separator.")

;;;;; Math

;;;;;; inline

(defconst tlon-math-inline
  '("$`" . "`$")
  "Delimiter pair for an inline math expression.")

(defconst tlon-math-inline-search-pattern
  (tlon-make-tag-pattern tlon-math-inline)
  "Regexp pattern for matching an inline mathematical expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the delimiters.")

;;;;;; display

(defconst tlon-math-display
  '("$$\n" . "\n$$")
  "Delimiter pair for a display math expression.")

(defconst tlon-math-display-search-pattern
  (tlon-make-tag-pattern tlon-math-display)
  "Regexp pattern for matching a display mathematical expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the delimiters.")

;;;;; MDX

;;;;;; Aside

(defconst tlon-mdx-aside
  '("<Aside>" . "</Aside>")
  "Pair of MDX `Aside' tags.
Text enclosed in an `Aside' tag pair will be treated like an aside section.")

(defconst tlon-mdx-aside-search-pattern
  (tlon-make-tag-pattern tlon-mdx-aside)
  "Regexp pattern for matching an MDX `Aside' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;;; Lang

(defconst tlon-mdx-lang
  '("<Lang id={\"%s\"}>" . "</Lang>")
  "Pair of MDX `Lang' tags.
Text enclosed by a `Lang' tag pair will be treated as belonging to that
language (e.g. for the purposed of hyphenating it).")

(defconst tlon-mdx-lang-search-pattern
  (tlon-make-tag-pattern tlon-mdx-lang)
  "Regexp pattern for matching an MDX `Lang' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;;; LiteralLink

(defconst tlon-mdx-literal-link
  '("<LiteralLink src={\"%s\"}>" . "</LiteralLink>")
  "Pair of MDX `LiteralLink' tags.
Links enclosed by a `LiteralLink' tag pair will be treated as literal links.")

(defconst tlon-mdx-literal-link-search-pattern
  (tlon-make-tag-pattern tlon-mdx-literal-link)
  "Regexp pattern for matching an MDX `LiteralLink' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;;; SmallCaps

(defconst tlon-mdx-small-caps
  '("<SmallCaps>" . "</SmallCaps>")
  "Pair of MDX `SmallCaps' tags.
Text enclosed by a `SmallCaps' tag pair will be displayed in small caps.")

(defconst tlon-mdx-small-caps-search-pattern
  (tlon-make-tag-pattern tlon-mdx-small-caps)
  "Regexp pattern for matching an MDX `SmallCaps' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;;; VisuallyHidden

(defconst tlon-mdx-visually-hidden
  '("<VisuallyHidden>" . "</VisuallyHidden>")
  "Pair of MDX `VisuallyHidden' tags.
Text enclosed by a `VisuallyHidden' tag pair will be narrated, but not
displayed.")

(defconst tlon-mdx-visually-hidden-search-pattern
  (tlon-make-tag-pattern tlon-mdx-visually-hidden)
  "Regexp pattern for matching an MDX `VisuallyHidden' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;;; VisuallyShown

(defconst tlon-mdx-visually-shown
  '("<VisuallyShown>" . "</VisuallyShown>")
  "Pair of MDX `VisuallyShown' tags.
Text enclosed by a `VisuallyShown' tag pair will be displayed, but not narrated.")

(defconst tlon-mdx-visually-shown-search-pattern
  (tlon-make-tag-pattern tlon-mdx-visually-shown)
  "Regexp pattern for matching an MDX `VisuallyShown' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;;; AlternativeVoice

(defconst tlon-mdx-alternative-voice
  '("<AlternativeVoice>" . "</AlternativeVoice>")
  "Pair of MDX `AlternativeVoice' tags.
Text enclosed by a `AlternativeVoice' tag pair will be narrated in the
alternative voice, as opposed to the main voice.")

(defconst tlon-mdx-alternative-voice-search-pattern
  (tlon-make-tag-pattern tlon-mdx-alternative-voice)
  "Regexp pattern for matching an MDX `AlternativeVoice' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;; Images

(defconst tlon-md-image-with-alt
  "!\\[\\(.+?\\)\\](\\(.*?\\))"
  "Pattern to match an image with alt text in a Markdown file.
The first group captures the alt text. The second group captures the image URL.")

(defconst tlon-md-image-sans-alt
  "!\\[\\(\\)\\](\\(.*?\\))"
  "Pattern to match an image without alt text in a Markdown file.
The first group captures the empty alt text. The second group captures the image
URL.")

;;;; Functions

;;;;; Insertion

;;;;;; metadata

;;;;###autoload
(defun tlon-edit-yaml-field ()
  "Edit the YAML field at point."
  (interactive)
  (cl-destructuring-bind (key value) (tlon-yaml-get-field-at-point)
    (tlon-yaml-get-completions key value)))

;;;;;; entities
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
	 (language (tlon-repo-lookup :language :dir (tlon-get-repo)))
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

;;;;;; Insert elements

(declare-function s-chop-right "s")
;;;###autoload
(defun tlon-md-insert-element-pair (pair &optional self-closing-p)
  "Insert an element PAIR at point or around the selected region.
PAIR is a cons cell whose car is the opening element and whose cdr is the
closing element. If SELF-CLOSING-P is non-nil, the opening element will be
self-closing."
  (interactive)
  (tlon-ensure-markdown-mode)
  (cl-destructuring-bind (open . close) pair
    (if (use-region-p)
	(let ((begin (region-beginning)))
	  (goto-char (region-end))
	  (insert close)
	  (goto-char begin)
	  (insert open))
      (if self-closing-p
	  (let ((open (concat (s-chop-right 1 open) " />")))
	    (insert open))
	(insert (concat open close)))
      (backward-char (length close)))))

;;;;;;; HTML

(defun tlon-insert-html-subscript ()
  "Insert an HTML `sub' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-element-pair "<sub>" "</sub>"))

(defun tlon-insert-html-superscript ()
  "Insert an HTML `sup' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-element-pair "<sup>" "</sup>"))

;;;;;;; MDX

;;;###autoload
(defun tlon-insert-mdx-aside ()
  "Insert an MDX `Aside' tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-element-pair tlon-mdx-aside))

;;;###autoload
(defun tlon-insert-mdx-lang (language)
  "Insert an MDX `Lang' tag pair at point or around the selected region.
Prompt the user to select a LANGUAGE. The enclosed text will be interpreted as
written in that language."
  (interactive (list (tlon-select-language 'code)))
  (tlon-md-insert-element-pair
   (tlon-tag-element-with-attributes tlon-mdx-lang language)))

;; TODO: revise to offer the url at point as default completion candidate
;;;###autoload
(defun tlon-insert-mdx-literal-link (url)
  "Insert an MDX `LiteralLink' tag pair at point or around the selected region.
Prompt the user to select a URL."
  (interactive (list (read-string "URL: ")))
  (tlon-md-insert-element-pair
   (tlon-tag-element-with-attributes tlon-mdx-literal-link url)))

;;;###autoload
(defun tlon-insert-mdx-small-caps ()
  "Insert an MDX `SmallCaps' tag pair at point or around the selected region.
Text enclosed by an `SmallCaps' tag pair will be displayed in small caps."
  (interactive)
  (tlon-md-insert-element-pair tlon-mdx-small-caps))

;;;###autoload
(defun tlon-insert-mdx-visually-hidden ()
  "Insert an MDX `VisuallyHidden' tag pair at point or around selected region.
Text enclosed by a `VisuallyHidden' tag pair will be narrated, but not
displayed."
  (interactive)
  (tlon-md-insert-element-pair tlon-mdx-visually-hidden))

;;;###autoload
(defun tlon-insert-mdx-visually-shown ()
  "Insert an MDX `VisuallyShown' tag pair at point or around selected region.
Text enclosed by an `VisuallyShown' tag pair will be displayed, but not
narrated."
  (interactive)
  (tlon-md-insert-element-pair tlon-mdx-visually-shown))

;;;###autoload
(defun tlon-insert-mdx-alternative-voice ()
  "Insert an MDX `AlternativeVoice' tag pair at point or around region.
Text enclosed by a `AlternativeVoice' tag pair will be narrated in the
alternative voice, as opposed to the main voice."
  (interactive)
  (tlon-md-insert-element-pair tlon-mdx-alternative-voice))

(defun tlon-tag-element-with-attributes (element &rest attributes)
  "Construct an tag ELEMENT with one or more ATTRIBUTES."
  (cons (apply 'format (car element) attributes)
	(cdr element)))

;;;;;;;; Notes

(defun tlon-insert-note-marker (marker &optional overwrite)
  "Insert note MARKER in the footnote at point.
If OVERWRITE is non-nil, replace the existing marker when present."
  (if-let ((fn-data (tlon-note-content-bounds)))
      (let ((start (car fn-data)))
	(goto-char start)
	(let ((other-marker (car (remove marker (list tlon-footnote-marker
						      tlon-sidenote-marker)))))
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
    (tlon-insert-note-marker tlon-footnote-marker overwrite)))

;;;###autoload
(defun tlon-insert-sidenote-marker (&optional overwrite)
  "Insert a `Sidenote' marker in the footnote at point.
Text enclosed by a `Sidenote' tag pair will be displayed as a sidenote, as
opposed to a footnote.

If OVERWRITE is non-nil, or called interactively, replace the existing marker
when present."
  (interactive)
  (tlon-insert-note-marker tlon-sidenote-marker overwrite))

;;;;;;;; Citations

;;;###autoload
(defun tlon-insert-mdx-cite (arg)
  "Insert an MDX `Cite' element at point or around the selected region.
Prompt the user to select a BibTeX KEY. If point is already on a `Cite' element,
the KEY will replace the existing key.

By default, it will insert a \"long\" citation. To insert a \"short\" citation,
call the function preceded by the universal ARG or invoke
`tlon-insert-mdx-cite-short'."
  (interactive "P")
  (let ((key (car (citar-select-refs))))
    (if-let ((data (tlon-get-key-in-citation)))
	(cl-destructuring-bind (_ (begin . end)) data
	  (tlon-replace-bibtex-element-in-citation key begin end))
      (tlon-md-insert-element-pair
       (cons (format "<Cite bibKey={\"%s\"}%s>" key (if arg " short" ""))
	     "</Cite>")
       t))))

;;;###autoload
(defun tlon-insert-mdx-cite-short ()
  "Insert a short MDX `Cite' element at point or around the selected region."
  (interactive)
  (tlon-insert-mdx-cite '(4)))

;;;;;;;;; Citation elements

(defun tlon-get-bibtex-element-in-citation (type)
  "Return the BibTeX element of TYPE and its position in `Cite' element at point.
TYPE can be either `key' or `locators'."
  (when (thing-at-point-looking-at tlon-cite-pattern)
    (when-let* ((num (pcase type ('key 1) ('locators 2)
			    (_ (user-error "Invalid type"))))
		(match (match-string-no-properties num))
		(begin (match-beginning num))
		(end (match-end num)))
      (list match (cons begin end)))))

(defun tlon-replace-bibtex-element-in-citation (element begin end)
  "Delete bibtex ELEMENT between BEGIN and END."
  (save-excursion
    (set-buffer-modified-p t)
    (goto-char begin)
    (delete-region begin end)
    (insert element)))

;;;;;;;;; Locators

;; TODO: make it work in org-mode
(defun tlon-get-key-in-citation ()
  "Return the BibTeX key and its position in `Cite' element at point."
  (tlon-get-bibtex-element-in-citation 'key))

(defun tlon-get-locators-in-citation ()
  "Return the BibTeX locators and its position in `Cite' element at point."
  (tlon-get-bibtex-element-in-citation 'locators))

(defvar tlon-locators)
(defun tlon-insert-locator ()
  "Insert locator in citation at point.
If point is on a locator, it will be replaced by the new one. Otherwise, the new
locator will be inserted after the key, if there are no locators, or at the end
of the existing locators."
  (interactive)
  (unless (thing-at-point-looking-at tlon-cite-pattern)
    (user-error "Not in a citation"))
  (let* ((selection (completing-read "Locator: " tlon-locators nil t))
	 (locator (alist-get selection tlon-locators "" "" 'string=)))
    (if-let ((existing (tlon-get-locator-at-point)))
	(replace-match locator)
      (let ((end (cdadr (or (tlon-get-locators-in-citation)
			    (tlon-get-key-in-citation)))))
	(goto-char end)
	(insert (format ", %s " locator))))))

(defun tlon-get-locator-at-point ()
  "Return the locator at point, if present."
  (let ((locators (mapcar 'cdr tlon-locators)))
    (when (thing-at-point-looking-at (regexp-opt locators))
      (match-string-no-properties 0))))

;;;;;;; SSML

(defvar tlon-tts-ssml-lang)
;;;###autoload
(defun tlon-insert-ssml-lang (language)
  "Insert an SSML `lang' tag pair at point or around the selected region.
Prompt the user to select a LANGUAGE. The enclosed text will be interpreted as
written in that language."
  (interactive (list (tlon-select-language 'locale)))
  (tlon-md-insert-element-pair
   (tlon-tag-element-with-attribute tlon-tts-ssml-lang language)))

(defvar tlon-tts-ssml-emphasis-levels)
(defvar tlon-tts-ssml-emphasis-default-level)
(defvar tlon-tts-ssml-emphasis)
;;;###autoload
(defun tlon-insert-ssml-emphasis (level)
  "Insert an SSML `emphasis' tag pair at point or around the selected region.
Prompt the user to select a LEVEL."
  (interactive (list (completing-read "Emphasis: "
				      tlon-tts-ssml-emphasis-levels nil t
				      tlon-tts-ssml-emphasis-default-level)))
  (tlon-md-insert-element-pair
   (tlon-tag-element-with-attribute tlon-tts-ssml-emphasis level)))

(defvar tlon-tts-ssml-break)
;;;###autoload
(defun tlon-insert-ssml-break (time)
  "Insert an SSML `break' tag pair at point or around the selected region.
TIME is the duration of the break n seconds."
  (interactive (list (concat (read-string "sTime (seconds): ") "s")))
  (insert (format tlon-tts-ssml-break time)))

;;;;;;; Math

;;;###autoload
(defun tlon-insert-math-inline ()
  "Insert an inline math tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-element-pair tlon-math-inline))

;;;###autoload
(defun tlon-insert-math-display ()
  "Insert a display math tag pair at point or around the selected region."
  (interactive)
  (tlon-md-insert-element-pair tlon-math-display))

;;;;; Note classification

(defun tlon-auto-classify-note-at-point ()
  "Automatically classify note at point as a note of TYPE."
  (interactive)
  (let* ((note (tlon-get-note-at-point))
	 (type (tlon-note-automatic-type note)))
    (tlon-classify-note-at-point type)))

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
    (cond ((string-match tlon-footnote-marker note)
	   'footnote)
	  ((string-match tlon-sidenote-marker note)
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
      (while (re-search-forward tlon-cite-pattern nil t)
	(replace-match "")
	(setq has-citation-p t))
      (when has-citation-p
	(let ((words (count-words-region (point-min) (point-max))))
	  (when (<= words 4)
	    (setq is-footnote-p t))))
      (if is-footnote-p 'footnote 'sidenote))))

(defun tlon-classify-note-at-point (&optional type)
  "Classify note at point as a note of TYPE.
TYPE can be either `footnote' o `sidenote'. If TYPE is nil, prompt the user for
a type."
  (interactive)
  (let ((type (or type (completing-read "Type: " '("footnote" "sidenote") nil t))))
    (pcase type
      ('footnote (tlon-insert-footnote-marker))
      ('sidenote (tlon-insert-sidenote-marker))
      (_ (user-error "Invalid type")))))

;;;;; Images

;; Maybe move to AI

(declare-function tlon-ai-describe-image "tlon-ai")
;;;###autoload
(defun tlon-add-alt-text ()
  "Add alt text to the image at point."
  (interactive)
  (if (thing-at-point-looking-at tlon-md-image-sans-alt)
      (let* ((alt-text-marker (make-marker))
	     (image-file (expand-file-name (match-string-no-properties 2))))
	(set-marker alt-text-marker (match-beginning 1))
	(tlon-ai-describe-image image-file
				      (lambda (description)
					(goto-char (marker-position alt-text-marker))
					(insert description))))
    (message "Not on an image link with missing alt text")))

;; TODO: create function to add alt text to all images in a file

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

(declare-function ffap-url-p "ffap")
(defun tlon-get-md-links-in-file (&optional file)
  "Return a list of all the URLs in the Markdown links present in FILE.
If FILE is nil, use the file visited by the current buffer."
  (let ((file (or file (buffer-file-name))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((links))
	(while (re-search-forward markdown-regex-link-inline nil t)
	  (when-let ((url (ffap-url-p (match-string-no-properties 6))))
	    (unless (member url links)
	      (push url links))))
	(reverse links)))))

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
    ("C" "cite short"           tlon-insert-mdx-cite-short)
    ("l" "locator"              tlon-insert-locator)]
   ["Math"
    ("i" "inline"               tlon-insert-math-inline)
    ("d" "display"              tlon-insert-math-display)]
   ["TTS"
    ("L" "lang"                 tlon-insert-ssml-lang)
    ("e" "emphasis"             tlon-insert-ssml-emphasis)
    ("v" "alternative voice"    tlon-insert-mdx-alternative-voice)
    ("H" "visually hidden"      tlon-insert-mdx-visually-hidden)
    ("S" "visually shown"       tlon-insert-mdx-visually-shown)]
   ["Misc"
    ("b" "subscript"            tlon-insert-html-subscript)
    ("p" "superscript"          tlon-insert-html-superscript)
    ("m" "small caps"           tlon-insert-mdx-small-caps)
    ("a" "aside"                tlon-insert-mdx-aside)
    ("g" "lang"                 tlon-insert-mdx-lang)
    ("." "special character"    tlon-insert-special-character)]])

(provide 'tlon-md)
;;; tlon-md.el ends here
