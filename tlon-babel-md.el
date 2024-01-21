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

(require 'tlon-babel)

;;;; Functions

;;;;; Insertion

;; TODO: revise to support multiple langs, including en
;;;###autoload
(defun tlon-babel-markdown-insert-element ()
  "Insert a link to an element at point.
The element can be a tag or an author."
  (interactive)
  (tlon-babel-check-in-markdown-mode)
  (let* ((selection (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))
	 (current-link (markdown-link-at-pos (point)))
	 (current-desc (nth 2 current-link))
	 (current-target (nth 3 current-link))
	 current-element-title)
    (when current-target
      (setq current-element-title
	    (tlon-babel-markdown-get-title-in-link-target
	     current-target)))
    (let* ((new-element-title (completing-read "Selection: " (tlon-babel-get-all-uqbar-entities)
					       nil t
					       (or current-element-title
						   selection)))
	   (new-target-file (tlon-babel-metadata-lookup "file" "title" new-element-title (tlon-babel-get-metadata-in-repo)))
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

(defun tlon-babel-markdown-get-title-in-link-target (target)
  "Return the title of the tag to which the TARGET of a Markdown link points."
  (let* ((file (expand-file-name target default-directory))
	 (title (tlon-babel-metadata-lookup "title" "file" file (tlon-babel-get-metadata-in-repo))))
    title))

(defun tlon-babel-markdown-sort-elements-in-paragraph (separator)
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
(defun tlon-babel-markdown-sort-related-entries ()
  "Sort the links in the `related entries' section in current buffer.
If no section is found, do nothing."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^## Entradas relacionadas" nil t)
      (forward-paragraph)
      (tlon-babel-markdown-sort-elements-in-paragraph " • "))))

;;;;;; Insert elements

;;;###autoload
(defun tlon-babel-markdown-insert-element-pair (open close)
  "Insert an element pair at point or around the selected region.
OPEN is the opening element and CLOSE is the closing element."
  (interactive)
  (tlon-babel-check-in-markdown-mode)
  (if (use-region-p)
      (let ((begin (region-beginning)))
	(goto-char (region-end))
	(insert close)
	(goto-char begin)
	(insert open))
    (insert (concat open close))
    (backward-char (length close))))

;; TODO: revise to offer the key at point as default completion candidate
(defun tlon-babel-markdown-insert-mdx-cite (key)
  "Insert an MDX `Cite' element pair at point or around the selected region.
Prompt the user to select a BibTeX KEY. When a key is enclosed in a `Cite'
element pair, only its title will be displayed in the exported web page."
  (interactive (list (read-string "Key: ")))
  (tlon-babel-markdown-insert-element-pair (format "<Cite id={\"%s\"}>"
						   key)
					   "</Cite>"))

;;;###autoload
(defun tlon-babel-markdown-insert-mdx-aside ()
  "Insert an MDX `Aside' element pair at point or around the selected region."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "<Aside>" "</Aside>"))

(defun tlon-babel-markdown-insert-mdx-lang (language)
  "Insert an MDX `Lang' element pair at point or around the selected region.
Prompt the user to select a LANGUAGE. The enclosed text will be interpreted as
written in that language."
  (interactive (list (completing-read "Language: " (mapcar #'car tlon-babel-languages))))
  (tlon-babel-markdown-insert-element-pair (format "<Lang id={\"%s\"}>"
						   language)
					   "</Lang>"))

;; TODO: revise to offer the url at point as default completion candidate
(defun tlon-babel-markdown-insert-mdx-literal-link (url)
  "Insert an MDX `LiteralLink' element pair at point or around the selected region.
Prompt the user to select a URL."
  (interactive (list (read-string "URL: ")))
  (tlon-babel-markdown-insert-element-pair (format "<LiteralLink src={\"%s\"}>"
						   url)
					   "</LiteralLink>"))

;;;###autoload
(defun tlon-babel-markdown-insert-mdx-small-caps ()
  "Insert an MDX `SmallCaps' element pair at point or around the selected region.
Text enclosed by an `SmallCaps' element pair will be displayed in small caps."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "<SmallCaps>" "</SmallCaps>"))

;;;###autoload
(defun tlon-babel-markdown-insert-mdx-footnote ()
  "Insert an MDX `Footnote' element pair at point or around the selected region.
Text enclosed by a `Footnote' element pair will be displayed as a footnote, as
opposed to a sidenote."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "<Footnote>" "</Footnote>"))

;;;###autoload
(defun tlon-babel-markdown-insert-mdx-sidenote ()
  "Insert an MDX `Sidenote' element pair at point or around the selected region.
Text enclosed by a `Sidenote' element pair will be displayed as a sidenote, as
opposed to a footnote."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "<Sidenote>" "</Sidenote>"))

;;;###autoload
(defun tlon-babel-markdown-insert-math-inline ()
  "Insert an inline math element pair at point or around the selected region."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "$`" "`$"))

;;;###autoload
(defun tlon-babel-markdown-insert-math-display ()
  "Insert a display math element pair at point or around the selected region."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "$$\n" "\n$$"))

;;;###autoload
(defun tlon-babel-markdown-end-of-buffer-dwim ()
  "Move point to the end of the relevant part of the buffer.
The relevant part of the buffer is the part of the buffer that excludes the
\"local variables\" section.

If this function is called twice consecutively, it will move the point to the
end of the buffer unconditionally."
  (interactive)
  (let ((match (re-search-forward tlon-babel-local-variables-line-start nil t)))
    (if (or (not match) (eq this-command last-command))
	(goto-char (point-max))
      (goto-char (- (match-beginning 0) 1)))))

(transient-define-prefix tlon-babel-markdown-insert-dispatch ()
  "Dispatch a `tlon-babel' command for Markdown insertion."
  [["MDX: notes"
    ("f" "footnote"             tlon-babel-markdown-insert-mdx-footnote)
    ("s" "sidenote"             tlon-babel-markdown-insert-mdx-sidenote)
    ]
   ["MDX: other"
    ("a" "aside"                tlon-babel-markdown-insert-mdx-aside)
    ("c" "cite"                 tlon-babel-markdown-insert-mdx-cite)
    ("g" "lang"                 tlon-babel-markdown-insert-mdx-lang)
    ("l" "literal link"         tlon-babel-markdown-insert-mdx-literal-link)
    ("m" "small caps"           tlon-babel-markdown-insert-mdx-small-caps)
    ]
   ["Math"
    ("i" "inline"               tlon-babel-markdown-insert-math-inline)
    ("d" "display"              tlon-babel-markdown-insert-math-display)
    ]]
  )

(provide 'tlon-babel-md)
;;; tlon-babel-md.el ends here

