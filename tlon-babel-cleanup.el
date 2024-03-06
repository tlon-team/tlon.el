;;; tlon-babel-cleanup.el --- Cleanup Markdown buffers after import  -*- lexical-binding: t -*-

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

;; Cleanup Markdown buffers after import

;;; Code:

(require 'markdown-mode)
(require 'substitute)
(require 'unfill)

;;;; Functions

;;;;; Common

(defun tlon-babel-cleanup-common ()
  "Cleanup a buffer visiting an imported document.
These functions are to be called for all imported documents: both EAF and
non-EAF."
  (interactive)
  (tlon-babel-cleanup-unescape-chars)
  (tlon-babel-cleanup-unescape-lines)
  (tlon-babel-cleanup-remove-linebreaks)
  (tlon-babel-cleanup-convert-hyphens)
  (tlon-babel-cleanup-format-heading)
  (tlon-babel-cleanup-set-heading-levels)
  (tlon-babel-cleanup-remove-double-brackets)
  (tlon-babel-cleanup-remove-nonbreaking-spaces)
  (unfill-region (point-min) (point-max)))

(defun tlon-babel-cleanup-unescape-chars ()
  "Unescape relevant characters in the current buffer."
  ;; characters that need to be escaped
  (dolist (char '("." "[" "]" "$"))
    (let ((regexp (concat "\\\\\\" char)))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match char))))
  ;; characters that do not need to be escaped
  (dolist (char '("@" "\"" "'" "|" ">" "<" "~"))
    (let ((regexp (concat (regexp-quote "\\") char)))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match char)))))

(defun tlon-babel-cleanup-unescape-lines ()
  "Unescape consecutive empty lines."
  (goto-char (point-min))
  (while (re-search-forward "\\\\\n\\\\\n" nil t)
    (replace-match "\n\n")))

(defun tlon-babel-cleanup-remove-linebreaks ()
  "Remove extra line breaks in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n" nil t)
    (replace-match "\n\n")))

(defun tlon-babel-cleanup-format-heading ()
  "Remove boldfacing in headline elements."
  (goto-char (point-min))
  (while (re-search-forward "^\\(#\\{1,6\\} \\)\\*\\*\\(.*\\)\\*\\*$" nil t)
    (replace-match "\\1\\2")))

(defun tlon-babel-cleanup-convert-hyphens ()
  "Convert double and triple hyphens into en and em dashes, respectively."
  (dolist (cons '(("---" . "—")
		  ("--" . "–")))
    (goto-char (point-min))
    (while (re-search-forward (car cons) nil t)
      (replace-match (cdr cons)))))

(defun tlon-babel-cleanup-set-heading-levels ()
  "Promote or demote headings in the current buffer when appropriate.
Specifically, when the buffer contains at least one heading, demote all headings
if there is at least one level 1 heading, and promote all headings while there
are no level 2 headings and some headings level 3 or higher."
  (save-excursion
    (goto-char (point-min))
    (when (> (point-max) (markdown-next-heading)) ; buffer has at least one heading
      (goto-char (point-min))
      (when (re-search-forward "^# " nil t)
	(substitute-target-in-buffer "^#" "##"))
      (goto-char (point-min))
      (while (not (re-search-forward "^## " nil t))
	(substitute-target-in-buffer "^###" "##")))))

;; Not sure what the cause of these double brackets is; for now, just remove them
(defun tlon-babel-cleanup-remove-double-brackets ()
  "Remove consecutive double brackets."
  (dolist (string '("\\(\\]\\)\\]" "\\(\\[\\)\\["))
    (goto-char (point-min))
    (while (re-search-forward string nil t)
      (replace-match "\\1"))))

(defun tlon-babel-cleanup-remove-nonbreaking-spaces ()
  "Remove selected nonbreaking spaces."
  (goto-char (point-min))
  (while (re-search-forward "\\. \\([ \\[]\\)" nil t)
    (replace-match ".\\1")))

;;;;; EA Forum

(defun tlon-babel-cleanup-eaf ()
  "Cleanup a buffer visiting an imported document from the EA Forum.
Please note that the order in which these functions are called is relevant. Do
not alter it unless you know what you are doing."
  (interactive)
  (tlon-babel-cleanup-fix-footnote-refs)
  (tlon-babel-cleanup-remove-text))

;; If problems arise, test against documents imported from these URLs:
;; https://forum.effectivealtruism.org/s/vSAFjmWsfbMrTonpq/p/u5JesqQ3jdLENXBtB
(defun tlon-babel-cleanup-fix-footnote-refs ()
  "Convert footnote references to valid Markdown syntax."
  (let* ((ref-number "[[:digit:]]\\{1,3\\}")
	 (ref-source (format "\\^\\[\\(%s\\)\\](#fn.*?){.*?}\\^" ref-number))
	 (ref-target (format "\\(%s\\)\\.  \\(::: \\)?{#fn.*?} " ref-number))
	 (find-replace `((,ref-source . "[^\\1]")
			 (,ref-target . "[^\\1]: "))))
    (dolist (elt find-replace)
      (goto-char (point-min))
      (while (re-search-forward (car elt) nil t)
	(replace-match (cdr elt))))))

(defun tlon-babel-cleanup-remove-text ()
  "Remove various strings of text."
  (dolist (string '("::: footnotes\n"
		    "{rev=\"footnote\"} :::"
		    "(#fnref[[:digit:]]\\{1,3\\})"
		    " \\[↩︎](#fnref-.*?){\\.footnote-backref}"
		    "\\[↩]"
		    " :::"
		    "————————————————————————

  ::: {.section .footnotes}"
		    "{.underline}"
		    "\\*This work is licensed under a \\[Creative Commons Attribution 4.0 International License.\\](https://creativecommons.org/licenses/by/4.0/)\\*\n\n"))
    (goto-char (point-min))
    (while (re-search-forward string nil t)
      (replace-match ""))))

;;;;;; Footnotes

(defun tlon-babel-cleanup-split-footnotes-into-paragraphs ()
  "Split footnotes into separate paragraphs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[\\^[[:digit:]]\\{1,3\\}\\]:\\)" nil t)
      (replace-match "\n\n\\1"))))

(defun tlon-babel-cleanup-consolidate-all-footnotes (dir)
  "Consolidate all footnotes in DIR."
  (interactive "D")
  (dolist (file (directory-files dir nil "\\.md$"))
    (with-current-buffer (find-file-noselect file)
      (message "Consolidating footnotes in %s" (buffer-name))
      (tlon-babel-cleanup-consolidate-footnotes)
      (save-buffer))))

(defun tlon-babel-cleanup-consolidate-footnotes ()
  "Consolidate consecutive footnotes."
  (interactive)
  (goto-char (point-min))
  (let ((regex "\\[\\^\\([[:digit:]]\\{1,3\\}\\)\\]\\ ?\\[\\^\\([[:digit:]]\\{1,3\\}\\)\\]"))
    (while (re-search-forward regex nil t)
      (let* ((n1 (string-to-number (match-string-no-properties 1)))
	     (n2 (string-to-number (match-string-no-properties 2))))
	(replace-match "" nil nil)
	(let* ((fn1 (tlon-babel-cleanup-get-footnote n1 'delete))
	       (fn2 (tlon-babel-cleanup-get-footnote n2 'delete))
	       (consolidated (tlon-babel-cleanup-consolidate-bibtex-keys (format "%s; %s" fn1 fn2))))
	  (markdown-insert-footnote)
	  (insert (format "%s." consolidated))
	  (goto-char (point-min)))))))

(defun tlon-babel-cleanup-get-footnote (n &optional delete)
  "Get the content of footnote number N.
If DELETE is non-nil, delete the footnote."
  (save-excursion
    (goto-char (point-min))
    (let ((footnote-start)
	  (footnote-end)
	  (footnote-content))
      ;; Locate the footnote
      (unless (re-search-forward (format "\\[\\^%d\\]:\\ " n) nil t)
	(error (format "Footnote %d not found" n)))
      (setq footnote-start (point))
      ;; Locate end of footnote content
      (if (re-search-forward "\\[\\^[[:digit:]]\\{1,3\\}\\]:\\ " nil t)
	  (goto-char (match-beginning 0))
	(goto-char (point-max)))
      (setq footnote-end (if (bolp) (- (point) 1) (point)))
      ;; Extract footnote content
      (setq footnote-content (buffer-substring-no-properties footnote-start footnote-end))
      (when delete
	(tlon-babel-cleanup-delete-footnote n))
      footnote-content)))

(defun tlon-babel-cleanup-delete-footnote (n)
  "Delete footnote number N."
  (save-excursion                         ; Preserve initial position
    (goto-char (point-min))               ; Go to beginning of buffer
    (let (footnote-start)
      ;; Locate the footnote
      (unless (re-search-forward (format "\\[\\^%d\\]:\\ " n) nil t)
	(error (format "Footnote ^%d not found" n)))
      (setq footnote-start (match-beginning 0))
      ;; Locate end of footnote content
      (if (re-search-forward "\\[\\^[[:digit:]]\\{1,3\\}\\]:\\ " nil t)
	  (goto-char (match-beginning 0))
	(goto-char (point-max)))
      (delete-region footnote-start (point)))))

(defun tlon-babel-cleanup-consolidate-bibtex-keys (string)
  "Consolidate Bibtex keys in STRING."
  (let ((start 0)
	matches)
    (while (string-match "\\[\\(@.*?\\)\\]" string start)
      (push (match-string 1 string) matches)
      (setq start (match-end 0)))
    (format "[%s]" (mapconcat 'identity (nreverse matches) "; "))))

(provide 'tlon-babel-cleanup)
;;; tlon-babel-cleanup.el ends here

