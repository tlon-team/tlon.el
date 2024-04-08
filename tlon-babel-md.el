;;; tlon-babel-md.el --- Markdown functionality for the Babel project -*- lexical-binding: t -*-

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

;; Markdown functionality for the Babel project.

;;; Code:

(require 'files-x)
(require 'markdown-mode-extras)
(require 'tlon-babel-core)
(require 'tlon-babel-yaml)

;;;; User options

(defgroup tlon-babel-md ()
  "Markdown functionality."
  :group 'tlon-babel)

(defcustom tlon-babel-md-special-characters
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
  :group 'tlon-babel-md
  :type '(alist :key-type string :value-type string))

;;;; Variables

;;;;; Local variables

(defconst tlon-babel-md-local-variables-line-start
  "<!-- Local Variables:"
  "Start of the first line that contains file local variables.")

(defconst tlon-babel-md-local-variables-line-end
  "End: -->"
  "End of the last line that contains file local variables.")

;;;;; Cite

(defconst tlon-babel-cite-pattern
  "<Cite bibKey={\"\\(.*?\\)\\(, .*?\\)?\"}\\(\\( short\\)? />\\|>.*?</Cite>\\)"
  "Pattern to match a citation in a Markdown file.
The first group captures the bibTeX key, the second group captures the locators,
and the third group captures the short citation flag.")

;;;;; Note markers

(defconst tlon-babel-footnote-marker "<Footnote />"
  "Marker for a footnote in a `Cite' MDX element.")

(defconst tlon-babel-sidenote-marker "<Sidenote />"
  "Marker for a sidenote in a `Cite' MDX element.")

;;;;; Numbers

(defconst tlon-babel-md-number-separators
  '(("en" . ",")
    ("es" . " "))
  "Number separators for different languages.")

(defconst tlon-babel-md-number-separator-pattern
  "\\([[:digit:]]+?\\)%s\\([[:digit:]]+?\\)"
  "Pattern to match numbers separated by language-specific separator.")

;;;;; Math

;;;;;; inline

(defconst tlon-babel-math-inline
  '("$`" . "`$")
  "Delimiter pair for an inline math expression.")

(defconst tlon-babel-math-inline-search-pattern
  (tlon-babel-make-tag-search-pattern tlon-babel-math-inline)
  "Regexp pattern for matching inline mathematical expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the delimiters.")

;;;;;; display

(defconst tlon-babel-math-display
  '("$$\n" . "\n$$")
  "Delimiter pair for a display math expression.")

(defconst tlon-babel-math-display-search-pattern
  (tlon-babel-make-tag-search-pattern tlon-babel-math-display)
  "Regexp pattern for matching display mathematical expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the delimiters.")

;;;;; MDX

;;;;;; Aside

(defconst tlon-babel-mdx-aside
  '("<Aside>" . "</Aside>")
  "Pair of MDX `Aside' tags.")

(defconst tlon-babel-mdx-aside-search-pattern
  (tlon-babel-make-tag-search-pattern tlon-babel-mdx-aside)
  "Regexp pattern for matching an MDX `Aside' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;;; Lang

(defconst tlon-babel-mdx-lang
  '("<Lang id={\"%s\"}>" . "</Lang>")
  "Pair of MDX `Lang' tags.")

(defconst tlon-babel-mdx-lang-search-pattern
  (tlon-babel-make-tag-search-pattern tlon-babel-mdx-lang)
  "Regexp pattern for matching an MDX `Lang' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;;; LiteralLink

(defconst tlon-babel-mdx-literal-link
  '("<LiteralLink src={\"%s\"}>" . "</LiteralLink>")
  "Pair of MDX `LiteralLink' tags.")

(defconst tlon-babel-mdx-literal-link-search-pattern
  (tlon-babel-make-tag-search-pattern tlon-babel-mdx-literal-link)
  "Regexp pattern for matching an MDX `LiteralLink' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;;; SmallCaps

(defconst tlon-babel-mdx-small-caps
  '("<SmallCaps>" . "</SmallCaps>")
  "Pair of MDX `SmallCaps' tags.")

(defconst tlon-babel-mdx-small-caps-search-pattern
  (tlon-babel-make-tag-search-pattern tlon-babel-mdx-small-caps)
  "Regexp pattern for matching an MDX `SmallCaps' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;;; VisuallyHidden

(defconst tlon-babel-mdx-visually-hidden
  '("<VisuallyHidden>" . "</VisuallyHidden>")
  "Pair of MDX `VisuallyHidden' tags.")

(defconst tlon-babel-mdx-visually-hidden-search-pattern
  (tlon-babel-make-tag-search-pattern tlon-babel-mdx-visually-hidden)
  "Regexp pattern for matching an MDX `VisuallyHidden' expression.
The first capture group captures the entire expression. The second capture group
captures the expression without the tags.")

;;;;; Images

(defconst tlon-babel-md-image-with-alt
  "!\\[\\(.+?\\)\\](\\(.*?\\))"
  "Pattern to match an image with alt text in a Markdown file.
The first group captures the alt text. The second group captures the image URL.")

(defconst tlon-babel-md-image-sans-alt
  "!\\[\\(\\)\\](\\(.*?\\))"
  "Pattern to match an image without alt text in a Markdown file.
The first group captures the empty alt text. The second group captures the image
URL.")

;;;; Functions

;;;;; Insertion

;;;;;; metadata

;;;;###autoload
(defun tlon-babel-edit-yaml-field ()
  "Edit the YAML field at point."
  (interactive)
  (cl-destructuring-bind (key value) (tlon-babel-yaml-get-field-at-point)
    (tlon-babel-yaml-get-completions key value)))

;;;;;; entities
;; TODO: revise to support multiple langs, including en
;;;###autoload
(defun tlon-babel-insert-internal-link ()
  "Insert a link to an entity at point.
The entity can be a tag or an author."
  (interactive)
  (tlon-babel-md-check-in-markdown-mode)
  (let* ((selection (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))
	 (current-link (markdown-link-at-pos (point)))
	 (current-desc (nth 2 current-link))
	 (current-target (nth 3 current-link))
	 (language (tlon-babel-repo-lookup :language :dir (tlon-babel-get-repo)))
	 current-element-title)
    (when current-target
      (setq current-element-title
	    (tlon-babel-md-get-title-in-link-target
	     current-target)))
    (let* ((candidates (tlon-babel-metadata-get-values-of-all-types language 'current-repo))
	   (new-element-title (completing-read "Selection: " candidates nil t
					       (or current-element-title selection)))
	   (new-target-file (tlon-babel-metadata-lookup (tlon-babel-metadata-in-repo) "file" "title" new-element-title))
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

(defun tlon-babel-md-get-title-in-link-target (target)
  "Return the title of the tag to which the TARGET of a Markdown link points."
  (let* ((file (expand-file-name target default-directory))
	 (title (tlon-babel-metadata-lookup (tlon-babel-metadata-in-repo) "title" "file" file)))
    title))

(defun tlon-babel-md-sort-elements-in-paragraph (separator)
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
(defun tlon-babel-md-sort-related-entries ()
  "Sort the links in the `related entries' section in current buffer.
If no section is found, do nothing."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^## Entradas relacionadas" nil t)
      (forward-paragraph)
      (tlon-babel-md-sort-elements-in-paragraph " • "))))

;;;;;; Insert elements

;;;###autoload
(defun tlon-babel-md-insert-element-pair (pair &optional self-closing-p)
  "Insert an element PAIR at point or around the selected region.
PAIR is a cons cell whose car is the opening element and whose cdr is the
closing element. If SELF-CLOSING-P is non-nil, the opening element will be
self-closing."
  (interactive)
  (tlon-babel-md-check-in-markdown-mode)
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

(defun tlon-babel-insert-html-subscript ()
  "Insert an HTML `sub' element pair at point or around the selected region."
  (interactive)
  (tlon-babel-md-insert-element-pair "<sub>" "</sub>"))

(defun tlon-babel-insert-html-superscript ()
  "Insert an HTML `sup' element pair at point or around the selected region."
  (interactive)
  (tlon-babel-md-insert-element-pair "<sup>" "</sup>"))

;;;;;;; MDX

;;;###autoload
(defun tlon-babel-insert-mdx-aside ()
  "Insert an MDX `Aside' element pair at point or around the selected region."
  (interactive)
  (tlon-babel-md-insert-element-pair tlon-babel-mdx-aside))

;;;###autoload
(defun tlon-babel-insert-mdx-lang (language)
  "Insert an MDX `Lang' element pair at point or around the selected region.
Prompt the user to select a LANGUAGE. The enclosed text will be interpreted as
written in that language."
  (interactive (list (tlon-babel-select-language 'two-letter)))
  (tlon-babel-md-insert-element-pair
   (tlon-babel-tag-element-with-attribute tlon-babel-mdx-lang language)))

;; TODO: revise to offer the url at point as default completion candidate
;;;###autoload
(defun tlon-babel-insert-mdx-literal-link (url)
  "Insert an MDX `LiteralLink' element pair at point or around the selected region.
Prompt the user to select a URL."
  (interactive (list (read-string "URL: ")))
  (tlon-babel-md-insert-element-pair
   (tlon-babel-tag-element-with-attribute tlon-babel-mdx-literal-link url)))

;;;###autoload
(defun tlon-babel-insert-mdx-small-caps ()
  "Insert an MDX `SmallCaps' element pair at point or around the selected region.
Text enclosed by an `SmallCaps' element pair will be displayed in small caps."
  (interactive)
  (tlon-babel-md-insert-element-pair tlon-babel-mdx-small-caps))

;;;###autoload
(defun tlon-babel-insert-mdx-visually-hidden ()
  "Insert an MDX `VisuallyHidden' element pair at point or around selected region.
Text enclosed by an ``VisuallyHidden'' element pair will be displayed in small
caps."
  (interactive)
  (tlon-babel-md-insert-element-pair tlon-babel-mdx-visually-hidden))

(defun tlon-babel-tag-element-with-attribute (element attribute)
  "Construct an tag ELEMENT with an ATTRIBUTE."
  (cons (format (car element) attribute)
	(cdr element)))

;;;;;;;; Notes

(defun tlon-babel-insert-note-marker (marker &optional overwrite)
  "Insert note MARKER in the footnote at point.
If OVERWRITE is non-nil, replace the existing marker when present."
  (if-let ((fn-data (tlon-babel-note-content-bounds)))
      (let ((start (car fn-data)))
	(goto-char start)
	(let ((other-marker (car (remove marker (list tlon-babel-footnote-marker
						      tlon-babel-sidenote-marker)))))
	  (cond ((thing-at-point-looking-at (regexp-quote marker))
		 nil)
		((thing-at-point-looking-at (regexp-quote other-marker))
		 (when overwrite
		   (replace-match marker)))
		(t
		 (insert marker)))))
    (user-error "Not in a footnote")))

;;;###autoload
(defun tlon-babel-insert-footnote-marker (&optional overwrite)
  "Insert a `Footnote' marker in the footnote at point.
Text enclosed by a `Footnote' element pair will be displayed as a footnote, as
opposed to a sidenote.

If OVERWRITE is non-nil, or called interactively, replace the existing marker
when present."
  (interactive)
  (let ((overwrite (or overwrite (called-interactively-p 'any))))
    transient-current-command
    (tlon-babel-insert-note-marker tlon-babel-footnote-marker overwrite)))

;;;###autoload
(defun tlon-babel-insert-sidenote-marker (&optional overwrite)
  "Insert a `Sidenote' marker in the footnote at point.
Text enclosed by a `Sidenote' element pair will be displayed as a sidenote, as
opposed to a footnote.

If OVERWRITE is non-nil, or called interactively, replace the existing marker
when present."
  (interactive)
  (tlon-babel-insert-note-marker tlon-babel-sidenote-marker overwrite))

;;;;;;;; Citations

;;;###autoload
(defun tlon-babel-insert-mdx-cite (arg)
  "Insert an MDX `Cite' element at point or around the selected region.
Prompt the user to select a BibTeX KEY. If point is already on a `Cite' element,
the KEY will replace the existing key.

By default, it will insert a \"long\" citation. To insert a \"short\" citation,
call the function preceded by the universal ARG or invoke
`tlon-babel-insert-mdx-cite-short'."
  (interactive "P")
  (let ((key (car (citar-select-refs))))
    (if-let ((data (tlon-babel-get-key-in-citation)))
	(cl-destructuring-bind (_ (begin . end)) data
	  (tlon-babel-replace-bibtex-element-in-citation key begin end))
      (tlon-babel-md-insert-element-pair
       (cons (format "<Cite bibKey={\"%s\"}%s>" key (if arg " short" ""))
	     "</Cite>")
       t))))

;;;###autoload
(defun tlon-babel-insert-mdx-cite-short ()
  "Insert a short MDX `Cite' element at point or around the selected region."
  (interactive)
  (tlon-babel-insert-mdx-cite '(4)))

;;;;;;;;; Citation elements

(defun tlon-babel-get-bibtex-element-in-citation (type)
  "Return the BibTeX element of TYPE and its position in `Cite' element at point.
TYPE can be either `key' or `locators'."
  (when (thing-at-point-looking-at tlon-babel-cite-pattern)
    (let* ((num (pcase type ('key 1) ('locators 2)
		       (_ (user-error "Invalid type"))))
	   (match (match-string-no-properties num))
	   (begin (match-beginning num))
	   (end (match-end num)))
      (list match (cons begin end)))))

(defun tlon-babel-replace-bibtex-element-in-citation (element begin end)
  "Delete bibtex ELEMENT between BEGIN and END."
  (save-excursion
    (set-buffer-modified-p t)
    (goto-char begin)
    (delete-region begin end)
    (insert element)))

;;;;;;;;; Locators

;; TODO: make it work in org-mode
(defun tlon-babel-get-key-in-citation ()
  "Return the BibTeX key and its position in `Cite' element at point."
  (tlon-babel-get-bibtex-element-in-citation 'key))

(defun tlon-babel-get-locators-in-citation ()
  "Return the BibTeX locators and its position in `Cite' element at point."
  (tlon-babel-get-bibtex-element-in-citation 'locators))

(defun tlon-babel-insert-locator ()
  "Insert locator in citation at point.
If point is on a locator, it will be replaced by the new one. Otherwise, the new
locator will be inserted after the key, if there are no locators, or at the end
of the existing locators."
  (interactive)
  (unless (thing-at-point-looking-at tlon-babel-cite-pattern)
    (user-error "Not in a citation"))
  (let* ((selection (completing-read "Locator: " tlon-babel-locators nil t))
	 (locator (alist-get selection tlon-babel-locators "" "" 'string=)))
    (if-let ((existing (tlon-babel-get-locator-at-point)))
	(replace-match locator)
      (let ((end (cdadr (or (tlon-babel-get-locators-in-citation)
			    (tlon-babel-get-key-in-citation)))))
	(goto-char end)
	(insert (format ", %s " locator))))))

(defun tlon-babel-get-locator-at-point ()
  "Return the locator at point, if present."
  (let ((locators (mapcar 'cdr tlon-babel-locators)))
    (when (thing-at-point-looking-at (regexp-opt locators))
      (match-string-no-properties 0))))

;;;;;;; SSML

;;;###autoload
(defun tlon-babel-insert-ssml-lang (language)
  "Insert an SSML `lang' element pair at point or around the selected region.
Prompt the user to select a LANGUAGE. The enclosed text will be interpreted as
written in that language."
  (interactive (list (tlon-babel-select-language 'locale)))
  (tlon-babel-md-insert-element-pair
   (tlon-babel-tag-element-with-attribute tlon-babel-tts-ssml-lang language)))

;;;###autoload
(defun tlon-babel-insert-ssml-emphasis (level)
  "Insert an SSML `emphasis' element pair at point or around the selected region.
Prompt the user to select a LEVEL."
  (interactive (list (completing-read "Emphasis: "
				      tlon-babel-tts-ssml-emphasis-levels nil t
				      tlon-babel-tts-ssml-emphasis-default-level)))
  (tlon-babel-md-insert-element-pair
   (tlon-babel-tag-element-with-attribute tlon-babel-tts-ssml-emphasis level)))

;;;###autoload
(defun tlon-babel-insert-ssml-break (time)
  "Insert an SSML `break' element pair at point or around the selected region.
TIME is the duration of the break n seconds."
  (interactive (list (concat (read-string "sTime (seconds): ") "s")))
  (insert (format tlon-babel-tts-ssml-break time)))

;;;;;;; Math

;;;###autoload
(defun tlon-babel-insert-math-inline ()
  "Insert an inline math element pair at point or around the selected region."
  (interactive)
  (tlon-babel-md-insert-element-pair tlon-babel-math-inline))

;;;###autoload
(defun tlon-babel-insert-math-display ()
  "Insert a display math element pair at point or around the selected region."
  (interactive)
  (tlon-babel-md-insert-element-pair tlon-babel-math-display))

;;;;; Note classification

(defun tlon-babel-auto-classify-note-at-point ()
  "Automatically classify note at point as a note of TYPE."
  (interactive)
  (let* ((note (tlon-babel-get-note-at-point))
	 (type (tlon-babel-note-automatic-type note)))
    (tlon-babel-classify-note-at-point type)))

(defun tlon-babel-note-content-bounds ()
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

(defun tlon-babel-get-note-at-point ()
  "Get the note at point, if any."
  (when-let* ((bounds (tlon-babel-note-content-bounds))
	      (begin (car bounds))
	      (end (cdr bounds)))
    (string-trim (buffer-substring-no-properties begin end))))

(defun tlon-babel-auto-classify-notes-in-file (&optional file)
  "Automatically classify all notes in FILE.
If FILE is nil, use the current buffer."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward markdown-regex-footnote-definition nil t)
	  (tlon-babel-auto-classify-note-at-point))))))

(defun tlon-babel-auto-classify-notes-in-directory (&optional dir)
  "Automatically classify all notes in DIR.
If REPO is nil, use the current directory."
  (interactive)
  (let ((dir (or dir (file-name-directory default-directory))))
    (dolist (file (directory-files dir t "^[^.][^/]*$"))
      (when (string-match-p "\\.md$" file)
	(message "Classifying notes in %s" file)
	(tlon-babel-auto-classify-notes-in-file file)))))

(defun tlon-babel-get-note-type (&optional note)
  "Return the type of NOTE.
If NOTE is nil, use the note at point. A note type may be either `footnote' or
`sidenote'."
  (when-let ((note (or note (tlon-babel-get-note-at-point))))
    (cond ((string-match tlon-babel-footnote-marker note)
	   'footnote)
	  ((string-match tlon-babel-sidenote-marker note)
	   'sidenote))))

(defun tlon-babel-note-automatic-type (note)
  "Return the type into which NOTE is automatically classified.
The function implements the following classification criterion: if the note
contains at least one citation and no more than four words excluding citations,
it is classified as a footnote; otherwise, it is classified as a sidenote."
  (with-temp-buffer
    (let ((is-footnote-p)
	  (has-citation-p))
      (insert note)
      (goto-char (point-min))
      (while (re-search-forward tlon-babel-cite-pattern nil t)
	(replace-match "")
	(setq has-citation-p t))
      (when has-citation-p
	(let ((words (count-words-region (point-min) (point-max))))
	  (when (<= words 4)
	    (setq is-footnote-p t))))
      (if is-footnote-p 'footnote 'sidenote))))

(defun tlon-babel-classify-note-at-point (&optional type)
  "Classify note at point as a note of TYPE.
TYPE can be either `footnote' o `sidenote'. If TYPE is nil, prompt the user for
a type."
  (interactive)
  (let ((type (or type (completing-read "Type: " '("footnote" "sidenote") nil t))))
    (pcase type
      ('footnote (tlon-babel-insert-footnote-marker))
      ('sidenote (tlon-babel-insert-sidenote-marker))
      (_ (user-error "Invalid type")))))

;;;;; Images

;; Maybe move to AI

;;;###autoload
(defun tlon-babel-add-alt-text ()
  "Add alt text to the image at point."
  (interactive)
  (if (thing-at-point-looking-at tlon-babel-md-image-sans-alt)
      (let* ((alt-text-marker (make-marker))
	     (image-file (expand-file-name (match-string-no-properties 2))))
	(set-marker alt-text-marker (match-beginning 1))
	(tlon-babel-ai-describe-image image-file
				      (lambda (description)
					(goto-char (marker-position alt-text-marker))
					(insert description))))
    (message "Not on an image link with missing alt text")))

;; TODO: create function to add alt text to all images in a file

;;;;; Misc

(defun tlon-babel-md-check-in-markdown-mode ()
  "Check if the current buffer is in a Markdown-derived mode."
  (unless (derived-mode-p 'markdown-mode)
    (user-error "Not in a Markdown buffer")))

(defun tlon-babel-insert-special-character (char)
  "Insert a special CHAR at point.
The list of completion candidates can be customized via the user option
`tlon-babel-md-special-characters'."
  (interactive (list (completing-read "Character: " tlon-babel-md-special-characters nil t)))
  (insert (alist-get char tlon-babel-md-special-characters nil nil #'string= )))

;;;;; Actual content

;; TODO: make it work twice consecutively
;;;###autoload
(defun tlon-babel-md-beginning-of-buffer-dwim ()
  "Move point to the beginning of the relevant part of the buffer.
The relevant part of the buffer is the part of the buffer that excludes the
metadata section.

If this function is called twice consecutively, it will move the point to the
end of the buffer unconditionally."
  (interactive)
  (goto-char (tlon-babel-md-beginning-of-content)))

;; TODO: make it work twice consecutively
;;;###autoload
(defun tlon-babel-md-end-of-buffer-dwim ()
  "Move point to the end of the relevant part of the buffer.
The relevant part of the buffer is the part of the buffer that excludes the
\"local variables\" section.

If this function is called twice consecutively, it will move the point to the
end of the buffer unconditionally."
  (interactive)
  (goto-char (tlon-babel-md-end-of-content)))

(defun tlon-babel-md-beginning-of-content ()
  "Return the position of the beginning of the content in the current buffer."
  (or (tlon-babel-md-end-of-metadata) (point-min)))

(defun tlon-babel-md-end-of-content ()
  "Return the position of the end of the content in the current buffer."
  (or (tlon-babel-md-beginning-of-local-variables) (point-max)))

(defun tlon-babel-md-beginning-of-local-variables ()
  "Return the position of the beginning of the local variables section."
  (when-let ((cons (tlon-babel-get-delimited-region-pos
		    tlon-babel-md-local-variables-line-start
		    tlon-babel-md-local-variables-line-end)))
    (car cons)))

(defun tlon-babel-md-end-of-metadata ()
  "Return the position of the end of the metadata section."
  (when-let ((cons (tlon-babel-get-delimited-region-pos
		    tlon-babel-yaml-delimiter)))
    (cdr cons)))

(defun tlon-babel-md-get-local-variables ()
  "Get the text in the \"local variables\" section of the current buffer."
  (when-let ((beg (tlon-babel-md-beginning-of-local-variables)))
    (string-trim (buffer-substring-no-properties beg (point-max)))))

(defun tlon-babel-md-get-metadata ()
  "Get the text in the metadata section of the current buffer."
  (when-let ((end (tlon-babel-md-end-of-metadata)))
    (string-trim (buffer-substring-no-properties (point-min) end))))

(defun tlon-babel-md-read-content (&optional file)
  "Read the substantive content of FILE.
The substantive content of a file is the file minus the metadata and the local
variables section. If FILE is nil, read the file visited by the current buffer."
  (let ((file (or file (buffer-file-name)))
	(begin (tlon-babel-md-beginning-of-content))
	(end (tlon-babel-md-end-of-content)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties begin end))))

;;;;; Menu

;;;###autoload (autoload 'tlon-babel-md-menu "tlon-babel-md" nil t)
(transient-define-prefix tlon-babel-md-menu ()
  "Dispatch a `tlon-babel' command for Markdown insertion."
  :info-manual "(tlon-babel) Editing Markdown"
  [["YAML"
    ("y" "field"                tlon-babel-edit-yaml-field)]
   ["Link"
    ("k" "internal"             tlon-babel-insert-internal-link)
    ("t" "literal"              tlon-babel-insert-mdx-literal-link)]
   ["Note markers"
    ("f" "footnote"             (lambda () (interactive) (tlon-babel-insert-footnote-marker 'overwrite)))
    ("s" "sidenote"             (lambda () (interactive) (tlon-babel-insert-sidenote-marker 'overwrite)))
    ("n" "auto: at point"       tlon-babel-auto-classify-note-at-point)
    ("N" "auto: in file"        tlon-babel-auto-classify-notes-in-file)]
   ["Citations"
    ("c" "cite"                 tlon-babel-insert-mdx-cite)
    ("C" "cite short"           tlon-babel-insert-mdx-cite-short)
    ("l" "locator"              tlon-babel-insert-locator)]
   ["Math"
    ("i" "inline"               tlon-babel-insert-math-inline)
    ("d" "display"              tlon-babel-insert-math-display)]
   ["SSML"
    ("L" "lang"                 tlon-babel-insert-ssml-lang)
    ("e" "emphasis"             tlon-babel-insert-ssml-emphasis)]
   ["Misc"
    ("b" "subscript"            tlon-babel-insert-html-subscript)
    ("p" "superscript"          tlon-babel-insert-html-superscript)
    ("m" "small caps"           tlon-babel-insert-mdx-small-caps)
    ("h" "visually hidden"      tlon-babel-insert-mdx-visually-hidden)
    ("a" "aside"                tlon-babel-insert-mdx-aside)
    ("g" "lang"                 tlon-babel-insert-mdx-lang)
    ("." "special character"    tlon-babel-insert-special-character)]])

(provide 'tlon-babel-md)
;;; tlon-babel-md.el ends here
