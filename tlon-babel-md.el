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

;; Markdown functionality for the Babel project

;;; Code:

(require 'markdown-mode-extras)
(require 'tlon-babel-yaml)

;;;; Variables

(defconst tlon-babel-md-local-variables-line-start
  "<!-- Local Variables: -->"
  "Start of the line that contains file local variables.")

(defconst tlon-babel-md-local-variables-line-end
  "<!-- End: -->"
  "End of the line that contains file local variables.")

(defconst tlon-babel-cite-pattern
  "<Cite id={\"\\(.*?\\)\"}\\(\\( short\\)? />\\|>.*?</Cite>\\)"
  "Pattern to match a citation in a Markdown file.")

;;;; Functions

;;;;; Insertion

;; TODO: revise to support multiple langs, including en
;;;###autoload
(defun tlon-babel-md-insert-entity ()
  "Insert a link to an entity at point.
The entity can be a tag or an author."
  (interactive)
  (tlon-babel-md-check-in-markdown-mode)
  (let* ((selection (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))
	 (current-link (markdown-link-at-pos (point)))
	 (current-desc (nth 2 current-link))
	 (current-target (nth 3 current-link))
	 current-element-title)
    (when current-target
      (setq current-element-title
	    (tlon-babel-md-get-title-in-link-target
	     current-target)))
    (let* ((new-element-title (completing-read "Selection: " (tlon-babel-metadata-get-all-uqbar-entities)
					       nil t
					       (or current-element-title
						   selection)))
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
(defun tlon-babel-md-insert-element-pair (open close &optional self-closing-p)
  "Insert an element pair at point or around the selected region.
OPEN is the opening element and CLOSE is the closing element. If SELF-CLOSING-P
is non-nil, the opening element will be self-closing."
  (interactive)
  (tlon-babel-md-check-in-markdown-mode)
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
    (backward-char (length close))))

;;;;;;; HTML

(defun tlon-babel-md-insert-html-subscript ()
  "Insert an HTML `sub' element pair at point or around the selected region."
  (interactive)
  (tlon-babel-md-insert-element-pair "<sub>" "</sub>"))

(defun tlon-babel-md-insert-html-superscript ()
  "Insert an HTML `sup' element pair at point or around the selected region."
  (interactive)
  (tlon-babel-md-insert-element-pair "<sup>" "</sup>"))

;;;;;;; MDX

;;;###autoload
(defun tlon-babel-md-insert-mdx-cite (arg &optional key)
  "Insert an MDX `Cite' element at point or around the selected region.
Prompt the user to select a BibTeX KEY. If point is already on a `Cite' element,
the KEY will replace the existing key.

By default, it will insert a \"long\" citation. To insert a \"short\" citation,
call the function preceded by the universal ARG."
  (interactive "P")
  (let ((key (car (citar-select-refs))))
    (if-let ((data (tlon-babel-get-bibtex-key-in-citation)))
	(cl-destructuring-bind (_ (begin . end)) data
	  (tlon-babel-replace-bibtex-key-in-citation key begin end))
      (tlon-babel-md-insert-element-pair (format "<Cite id={\"%s\"}%s>"
						 key (if arg " short" ""))
					 "</Cite>" t))))

(defun tlon-babel-get-bibtex-key-in-citation ()
  "Return the BibTeX key and its position in `Cite' element at point."
  (when (thing-at-point-looking-at tlon-babel-cite-pattern)
    (let ((match (match-string-no-properties 1))
	  (begin (match-beginning 1))
          (end (match-end 1)))
      (list match (cons begin end)))))

(defun tlon-babel-replace-bibtex-key-in-citation (key begin end)
  ""
  (save-excursion
    (set-buffer-modified-p t)
    (goto-char begin)
    (delete-region begin end)
    (insert key)))

;;;###autoload
(defun tlon-babel-md-insert-mdx-aside ()
  "Insert an MDX `Aside' element pair at point or around the selected region."
  (interactive)
  (tlon-babel-md-insert-element-pair "<Aside>" "</Aside>"))

;;;###autoload
(defun tlon-babel-md-insert-mdx-lang (language)
  "Insert an MDX `Lang' element pair at point or around the selected region.
Prompt the user to select a LANGUAGE. The enclosed text will be interpreted as
written in that language."
  (interactive (list (completing-read "Language: " (mapcar #'car tlon-babel-core-languages))))
  (tlon-babel-md-insert-element-pair (format "<Lang id={\"%s\"}>"
					     language)
				     "</Lang>"))

;; TODO: revise to offer the url at point as default completion candidate
;;;###autoload
(defun tlon-babel-md-insert-mdx-literal-link (url)
  "Insert an MDX `LiteralLink' element pair at point or around the selected region.
Prompt the user to select a URL."
  (interactive (list (read-string "URL: ")))
  (tlon-babel-md-insert-element-pair (format "<LiteralLink src={\"%s\"}>"
					     url)
				     "</LiteralLink>"))

;;;###autoload
(defun tlon-babel-md-insert-mdx-small-caps ()
  "Insert an MDX `SmallCaps' element pair at point or around the selected region.
Text enclosed by an `SmallCaps' element pair will be displayed in small caps."
  (interactive)
  (tlon-babel-md-insert-element-pair "<SmallCaps>" "</SmallCaps>"))

;;;###autoload
(defun tlon-babel-md-insert-mdx-footnote ()
  "Insert an MDX `Footnote' element pair at point or around the selected region.
Text enclosed by a `Footnote' element pair will be displayed as a footnote, as
opposed to a sidenote."
  (interactive)
  (tlon-babel-md-insert-element-pair "<Footnote>" "</Footnote>"))

;;;###autoload
(defun tlon-babel-md-insert-mdx-sidenote ()
  "Insert an MDX `Sidenote' element pair at point or around the selected region.
Text enclosed by a `Sidenote' element pair will be displayed as a sidenote, as
opposed to a footnote."
  (interactive)
  (tlon-babel-md-insert-element-pair "<Sidenote>" "</Sidenote>"))

;;;;;;; Math

;;;###autoload
(defun tlon-babel-md-insert-math-inline ()
  "Insert an inline math element pair at point or around the selected region."
  (interactive)
  (tlon-babel-md-insert-element-pair "$`" "`$"))

;;;###autoload
(defun tlon-babel-md-insert-math-display ()
  "Insert a display math element pair at point or around the selected region."
  (interactive)
  (tlon-babel-md-insert-element-pair "$$\n" "\n$$"))


;;;;;;; Citations

(defun tlon-babel-md-insert-locator ()
  "Insert locator in citation at point."
  (interactive)
  (unless (thing-at-point-looking-at citar-markdown-citation-key-regexp)
    (user-error "Not in a citation"))
  (let* ((locators '(("book" . "bk.")
		     ("chapter ". "chap.")
		     ("column" . "col.")
		     ("figure" . "fig.")
		     ("folio" . "fol.")
		     ("number" . "no.")
		     ("line" . "l.")
		     ("note" . "n.")
		     ("opus" . "op.")
		     ("page" . "p.")
		     ("paragraph" . "para.")
		     ("part" . "pt.")
		     ("section" . "sec.")
		     ("sub verbo" . "s.v")
		     ("verse" . "v.")
		     ("volumes" . "vol.")
		     ("books" . "bks.")
		     ("chapter ". "chaps.")
		     ("columns" . "cols.")
		     ("figures" . "figs.")
		     ("folios" . "fols.")
		     ("numbers" . "nos.")
		     ("lines" . "ll.")
		     ("notes" . "nn.")
		     ("opera" . "opp.")
		     ("pages" . "pp.")
		     ("paragraphs" . "paras.")
		     ("parts" . "pts.")
		     ("sections" . "secs.")
		     ("sub  verbis" . "s.vv.")
		     ("verses" . "vv.")
		     ("volumes" . "vols.")))
	 (selection (completing-read "Locator: " locators nil t)))
    (goto-char (cdr (bounds-of-thing-at-point 'symbol)))
    (when (string= "," (thing-at-point 'char))
      (re-search-forward ", [[:alpha:]]*?\\. " nil t)
      (replace-match ""))
    (insert (format ", %s " (alist-get selection locators "" "" 'string=)))))

;;;;; Misc

(defun tlon-babel-md-check-in-markdown-mode ()
  "Check if the current buffer is in a Markdown-derived mode."
  (unless (derived-mode-p 'markdown-mode)
    (user-error "Not in a Markdown buffer")))

(defun tlon-babel-md-end-of-buffer-dwim ()
  "Move point to the end of the relevant part of the buffer.
The relevant part of the buffer is the part of the buffer that excludes the
\"local variables\" section.

If this function is called twice consecutively, it will move the point to the
end of the buffer unconditionally."
  (interactive)
  (if (tlon-babel-md-get-local-variables)
      (progn
	(re-search-forward tlon-babel-md-local-variables-line-start nil t)
	(goto-char (- (match-beginning 0) 1)))
    (goto-char (point-max))))

(defun tlon-babel-md-beginning-of-buffer-dwim ()
  "Move point to the beginning of the relevant part of the buffer.
The relevant part of the buffer is the part of the buffer that excludes the
metadata section.

If this function is called twice consecutively, it will move the point to the
end of the buffer unconditionally."
  (interactive)
  (if (tlon-babel-md-get-metadata)
      (progn
	(re-search-backward tlon-babel-yaml-delimiter nil t)
	(goto-char (match-end 0)))
    (goto-char (point-min))))

;;;###autoload
(defun tlon-babel-md-get-local-variables ()
  "Get the text in the \"local variables\" section of the current buffer."
  (when-let ((range (tlon-babel-md-get-delimiter-region-position
                     tlon-babel-md-local-variables-line-start
                     tlon-babel-md-local-variables-line-end)))
    (cl-destructuring-bind (start . end) range
      (buffer-substring-no-properties start end))))

(defun tlon-babel-md-get-metadata ()
  "Get the text in the metadata section of the current buffer."
  (when-let ((range (tlon-babel-md-get-delimiter-region-position
                     tlon-babel-yaml-delimiter)))
    (cl-destructuring-bind (start . end) range
      (buffer-substring-no-properties start end))))

(defun tlon-babel-md-get-delimiter-region-position (start-delimiter &optional end-delimiter)
  "Get the position of the region between START-DELIMITER and END-DELIMITER.
If END-DELIMITER is nil, use START-DELIMITER as the end delimiter."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward start-delimiter nil t)
	(let* ((start (match-beginning 0))
	       (end (when (re-search-forward (or end-delimiter start-delimiter) nil t)
		      (match-end 0))))
	  (when (and start end)
	    (cons start end)))))))

;;;;; Dispatcher

(transient-define-prefix tlon-babel-md-insert-dispatch ()
  "Dispatch a `tlon-babel' command for Markdown insertion."
  [["HTML"
    ("b" "subscript"            tlon-babel-md-insert-html-subscript)
    ("p" "superscript"          tlon-babel-md-insert-html-superscript)
    ]
   ["MDX: notes"
    ("f" "footnote"             tlon-babel-md-insert-mdx-footnote)
    ("s" "sidenote"             tlon-babel-md-insert-mdx-sidenote)
    ]
   ["MDX: other"
    ("a" "aside"                tlon-babel-md-insert-mdx-aside)
    ("c" "cite"                 tlon-babel-md-insert-mdx-cite)
    ("g" "lang"                 tlon-babel-md-insert-mdx-lang)
    ("k" "literal link"         tlon-babel-md-insert-mdx-literal-link)
    ("m" "small caps"           tlon-babel-md-insert-mdx-small-caps)
    ]
   ["Citations"
    ("l" "locator"              tlon-babel-md-insert-locator)]
   ["Math"
    ("i" "inline"               tlon-babel-md-insert-math-inline)
    ("d" "display"              tlon-babel-md-insert-math-display)
    ]])

(provide 'tlon-babel-md)
;;; tlon-babel-md.el ends here
