;;; tlon-cleanup.el --- Cleanup Markdown buffers after import  -*- lexical-binding: t -*-

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

;; Cleanup Markdown buffers after import

;;; Code:

(require 'tlon-core)
(require 'transient)
(require 'subr-x)

(defvar-local tlon-cleanup--footnote-map nil
  "Hash table mapping EA Forum footnote ids to sequential numbers.
It is populated by `tlon-cleanup-fix-eaf-footnotes' and consumed by
`tlon-cleanup-fix-eaf-footnote-references'.")

;;;; Functions

;;;;; Common

(defun tlon-cleanup-common ()
  "Cleanup a buffer visiting an imported document.
These functions are to be called for all imported documents: both EAF and
non-EAF."
  (interactive)
  (tlon-cleanup-unescape-chars)
  (tlon-cleanup-unescape-lines)
  (tlon-cleanup-remove-linebreaks)
  (tlon-cleanup-convert-hyphens)
  (tlon-cleanup-format-heading)
  (tlon-cleanup-set-heading-levels)
  (tlon-cleanup-remove-double-brackets)
  (tlon-cleanup-remove-nonbreaking-spaces)
  (tlon-cleanup-remove-span-elements)
  (let ((fill-column (point-max)))
    (fill-region (point) (point-max))))

(defun tlon-cleanup-unescape-chars ()
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

(defun tlon-cleanup-unescape-lines ()
  "Unescape consecutive empty lines."
  (goto-char (point-min))
  (while (re-search-forward "\\\\\n\\\\\n" nil t)
    (replace-match "\n\n")))

(defun tlon-cleanup-remove-linebreaks ()
  "Remove extra line breaks in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n" nil t)
    (replace-match "\n\n")))

(defun tlon-cleanup-format-heading ()
  "Remove boldfacing in headline elements."
  (goto-char (point-min))
  (while (re-search-forward "^\\(#\\{1,6\\} \\)\\*\\*\\(.*\\)\\*\\*$" nil t)
    (replace-match "\\1\\2")))

(defun tlon-cleanup-convert-hyphens ()
  "Convert double and triple hyphens into en and em dashes, respectively."
  (dolist (cons '(("---" . "—")
		  ("--" . "–")))
    (goto-char (point-min))
    (while (re-search-forward (car cons) nil t)
      (replace-match (cdr cons)))))

(declare-function markdown-next-heading "markdown-mode")
(declare-function substitute-target-in-buffer "substitute")
(defun tlon-cleanup-set-heading-levels ()
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
(defun tlon-cleanup-remove-double-brackets ()
  "Remove consecutive double brackets."
  (dolist (string '("\\(\\]\\)\\]" "\\(\\[\\)\\["))
    (goto-char (point-min))
    (while (re-search-forward string nil t)
      (replace-match "\\1"))))

(defun tlon-cleanup-remove-nonbreaking-spaces ()
  "Replace non-breaking spaces (U+00A0) that appear in common EA-Forum artefacts.

The function normalises three patterns:
1. After a period:      \". X\" → \". X\"
2. After a footnote ref \"[^1] X\" → \"[^1] X\"
3. At a footnote start  \"[^1]: X\" → \"[^1]: X\""
  (goto-char (point-min))
  ;; 1. After periods.
  (while (re-search-forward "\\. " nil t)
    (replace-match ". "))
  ;; 2. After in-text footnote references.
  (goto-char (point-min))
  (while (re-search-forward "\\(\\[\\^[[:digit:]]\\{1,3\\}\\]\\) " nil t)
    (replace-match "\\1 "))
  ;; 3. Immediately after the colon in footnote definitions.
  (goto-char (point-min))
  (while (re-search-forward "^\\(\\[\\^[[:digit:]]\\{1,3\\}\\]:\\) " nil t)
    (replace-match "\\1 ")))

(defun tlon-cleanup-remove-span-elements ()
  "Remove span elements spaces."
  (goto-char (point-min))
  (while (re-search-forward "{.*?}" nil t)
    (replace-match "")))

;;;;; EA Forum

(defun tlon-cleanup-eaf ()
  "Cleanup a buffer visiting an imported document from the EA Forum.
Please note that the order in which these functions are called is relevant. Do
not alter it unless you know what you are doing."
  (interactive)
  (tlon-cleanup-eaf-replace-urls)
  (tlon-cleanup-fix-eaf-footnotes)
  (tlon-cleanup-fix-eaf-footnote-references)
  (tlon-cleanup-remove-eaf-text))

;;;###autoload
(defun tlon-cleanup-eaf-replace-urls ()
  "Replace EA Forum URLs in the current buffer with their \"canonical\" forms."
  (interactive)
  (save-excursion
    (dolist (pattern (list tlon-eaf-url-post-canonical
			   tlon-eaf-url-post-collection))
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
	(let* ((base-url (replace-regexp-in-string "\\\\" "" tlon-eaf-base-regexp))
	       (post-id (match-string-no-properties 2))
	       (comment-id (match-string-no-properties 3))
	       (replacement (format "%s/posts/%s%s" base-url post-id (or comment-id ""))))
	  (replace-match replacement t t))))))

(defun tlon-cleanup-eaf-replace-urls-in-repo ()
  "Replace EA Forum URLs in all Markdown files in the current repository.
This command should only be used once in a while to ensure that all URLs in the
repository are in their \"canonical\" form. But this shouldn’t normally happen,
since `tlon-cleanup-eaf-replace-urls' is run automatically every time a new
entry is added."
  (interactive)
  (let ((repo (tlon-get-repo)))
    (dolist (file (directory-files-recursively repo "\\.md"))
      (with-current-buffer (find-file-noselect file)
	(message "Cleaning up `%s'" file)
	(tlon-cleanup-eaf-replace-urls)
	(save-buffer)))))

;; If problems arise, test against documents imported from these URLs:
;; https://forum.effectivealtruism.org/s/vSAFjmWsfbMrTonpq/p/u5JesqQ3jdLENXBtB
(defun tlon-cleanup-fix-eaf-footnotes ()
  "Convert EAF in-text footnote markers to `[^N]' and record the mapping.
Each EA Forum footnote marker looks like:
  ^[\\[3\\]](#fn<id>)^
We sequentially number them in order of appearance, replace the
marker with `[^N]', and store an id→N mapping in the buffer-local
variable `tlon-cleanup--footnote-map', ready for
`tlon-cleanup-fix-eaf-footnote-references'."
  (setq-local tlon-cleanup--footnote-map (make-hash-table :test #'equal))
  (let ((pattern "\\^\\[\\\\?\\[?\\([[:digit:]]+\\)\\\\?\\]?\\](#fn\\([^)]*\\))\\^")
        (counter 1))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (let* ((id (match-string-no-properties 2))
             (n  (or (gethash id tlon-cleanup--footnote-map)
                     (prog1 counter
                       (puthash id counter tlon-cleanup--footnote-map)
                       (setq counter (1+ counter))))))
        (replace-match (format "[^%d]" n))))))

(defun tlon-cleanup-fix-eaf-footnote-references ()
  "Convert EA Forum footnote reference blocks to standard Markdown.

The function handles two EA Forum formats:

1. Classic numeric list items, e.g.
     3. Footnote text [↩︎](#fnref-abc123-3)

2. Caret-style blocks:
     ^**[^](#fnrefabc123)**^
     Footnote text …

It relies on the buffer-local `tlon-cleanup--footnote-map' populated by
`tlon-cleanup-fix-eaf-footnotes' so that numbers match the in-text markers."
  (let ((caret-pattern  "^[[:blank:]]*\\^\\*\\*\\[\\\\?\\^\\](#fnref\\([^)]*\\))\\*\\*\\^")
        (classic-pattern "^\\([[:digit:]]+\\)\\.\\s-*\\([^[:space:]].*?\\)\\s-*\\[↩\\(?:︎\\)?\\](#fnref-[[:alnum:]]*-[[:digit:]]+)"))
    ;; ── caret-style blocks ────────────────────────────────────────────────
    (goto-char (point-min))
    (while (re-search-forward caret-pattern nil t)
      (let* ((id (match-string-no-properties 1))
             (num (or (and (hash-table-p tlon-cleanup--footnote-map)
                           (gethash id tlon-cleanup--footnote-map))
                      (let ((n (1+ (hash-table-count tlon-cleanup--footnote-map))))
                        (puthash id n tlon-cleanup--footnote-map) n)))
             (block-start (match-beginning 0)))
        ;; Determine the bounds of the footnote body
        (end-of-line)
        (let* ((text-beg (progn (forward-line 1) (point)))
               (text-end (or (save-excursion
                               (when (re-search-forward caret-pattern nil t)
                                 (match-beginning 0)))
                             (point-max)))
               (body (string-trim (buffer-substring-no-properties text-beg text-end))))
          (delete-region block-start text-end)
          (goto-char block-start)
          (insert (format "[^%d]: %s\n\n" num body)))))
    ;; ── classic numeric list items ────────────────────────────────────────
    (goto-char (point-min))
    (while (re-search-forward classic-pattern nil t)
      (replace-match (format "[^%s]: %s"
                             (match-string-no-properties 1)
                             (match-string-no-properties 2))
                     nil t))))

(defun tlon-cleanup-remove-eaf-text ()
  "Remove various strings of text."
  (dolist (string '("::: footnotes\n"
		    "{rev=\"footnote\"} :::"
		    "(#fnref[[:digit:]]\\{1,3\\})"
		    " \\[↩︎](#fnref-.*?){\\.footnote-backref}"
		    "\\[↩]"
		    " :::"
		    "————————————————————————\n\n"
		    "\n\n::: {.section .footnotes}"
		    "\\*This work is licensed under a \\[Creative Commons Attribution 4.0 International License.\\](https://creativecommons.org/licenses/by/4.0/)\\*\n\n"))
    (goto-char (point-min))
    (while (re-search-forward string nil t)
      (replace-match ""))))

;;;;; 80,000 Hours

(defun tlon-cleanup-80k ()
  "Cleanup 80,000 Hours footnotes in the current buffer."
  (interactive)
  (tlon-cleanup-fix-80k-footnotes)
  (tlon-cleanup-fix-80k-footnote-references))

(defun tlon-cleanup-fix-80k-footnotes ()
  "Convert 80,000 Hours in-text footnote markers to `[^N]'.

This replaces occurrences of:
  [<sup>N</sup>](#fn-N \"...\")
with:
  [^N]
and removes the whole trailing link block."
  (goto-char (point-min))
  (let ((pattern "\\[<sup>\\([[:digit:]]\\{1,3\\}\\)</sup>\\]"))
    (while (re-search-forward pattern nil t)
      (let* ((n (match-string-no-properties 1))
             (end (match-end 0)))
        (when (save-excursion
                (goto-char end)
                (looking-at (format "(#fn-%s" (regexp-quote n))))
          ;; Replace the superscript with a Markdown footnote marker.
          (replace-match (format "[^%s]" n) t t nil 0)
          ;; Point is just after the replacement. Remove the parenthesized link.
          (when (eq (char-after) ?\()
            (tlon-cleanup--80k-delete-paren-block-at-point)))))))

(defun tlon-cleanup-fix-80k-footnote-references ()
  "Convert 80,000 Hours footnote list items to standard Markdown footnotes.

Transforms blocks like:
  1. First paragraph

     Continuation paragraph…

     [↩](#fn-ref-1)

into:
  [^1]: First paragraph

  Continuation paragraph…"
  (let ((item-re "^\\([[:digit:]]\\{1,3\\}\\)\\.\\s-+"))
    (goto-char (point-min))
    (while (re-search-forward item-re nil t)
      (let* ((n (match-string-no-properties 1))
             (block-beg (match-beginning 0))
             (content-beg (match-end 0))
             (next (or (save-excursion
                         (when (re-search-forward item-re nil t)
                           (match-beginning 0)))
                       (point-max)))
             (raw (buffer-substring-no-properties content-beg next)))
        ;; Only process if this block appears to be a footnote (has backref).
        (when (string-match (format "#fn-\\(?:ref-\\)?%s\\b" (regexp-quote n)) raw)
          (let* ((no-backref (tlon-cleanup--80k-strip-backrefs raw))
                 (deindented (tlon-cleanup--80k-strip-indentation no-backref))
                 (body (string-trim deindented))
                 (definition (format "[^%s]: %s\n\n" n body)))
            (delete-region block-beg next)
            (insert definition)
            ;; Restart scanning from beginning to keep anchors valid.
            (goto-char (point-min))))))))

(defun tlon-cleanup--80k-strip-backrefs (text)
  "Remove 80,000 Hours backreference links from TEXT."
  (let ((re "\\s-*\\[↩\\(?:︎\\)?\\](#fn-\\(?:ref-\\)?[[:digit:]]+)\\s-*"))
    (replace-regexp-in-string re "" text t t)))

(defun tlon-cleanup--80k-strip-indentation (text)
  "Remove list indentation from multi-paragraph TEXT."
  (setq text (replace-regexp-in-string "\\`[[:space:]]\\{4\\}" "" text t t))
  (replace-regexp-in-string "\n[[:space:]]\\{4\\}" "\n" text t t))

(defun tlon-cleanup--80k-delete-paren-block-at-point ()
  "Delete the balanced parenthesized block starting at point.

Point must be at an opening parenthesis."
  (when (eq (char-after) ?\()
    (let ((start (point))
          (depth 1))
      (forward-char 1)
      (while (and (> depth 0) (not (eobp)))
        (pcase (char-after)
          (?\( (setq depth (1+ depth)))
          (?\) (setq depth (1- depth))))
        (forward-char 1))
      (delete-region start (point)))))

;;;;; Non-EAF

(defun tlon-cleanup-non-eaf ()
  "Cleanup a buffer visiting an imported document from a non-EAF source.
Please note that the order in which these functions are called is relevant. Do
not alter it unless you know what you are doing."
  (interactive)
  (tlon-cleanup-fix-non-eaf-footnotes)
  (tlon-cleanup-fix-non-eaf-footnote-references))

(defun tlon-cleanup-fix-non-eaf-footnotes ()
  "Convert non-EAF footnotes to valid Markdown syntax."
  (let ((ref-source "\\[<sup>\\([[:digit:]]\\{1,3\\}\\)</sup>\\](#fn-\\1[^)]*)"))
    (goto-char (point-min))
    (while (re-search-forward ref-source nil t)
      (replace-match (format "[^%s]" (match-string-no-properties 1))))))

(defun tlon-cleanup-fix-non-eaf-footnote-references ()
  "Convert non-EAF footnote references to valid Markdown syntax."
  (let ((ref-target "^\\([[:digit:]]\\{1,3\\}\\)\\.\\s-*\\([[:print:][:space:]]*?\\)\\s-*\\[↩\\](#fn-ref-\\1)"))
    (goto-char (point-min))
    (while (re-search-forward ref-target nil t)
      (replace-match (format "[^%s]: %s"
                             (match-string-no-properties 1)
                             (match-string-no-properties 2))
                     nil t))))

;;;;;; Footnotes

(defun tlon-cleanup-split-footnotes-into-paragraphs ()
  "Split footnotes into separate paragraphs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[\\^[[:digit:]]\\{1,3\\}\\]:\\)" nil t)
      (replace-match "\n\n\\1"))))

(defun tlon-cleanup-consolidate-all-footnotes (dir)
  "Consolidate all footnotes in DIR."
  (interactive "D")
  (dolist (file (directory-files dir nil "\\.md$"))
    (with-current-buffer (find-file-noselect file)
      (message "Consolidating footnotes in %s" (buffer-name))
      (tlon-cleanup-consolidate-footnotes)
      (save-buffer))))

(declare-function markdown-insert-footnote "markdown-mode")
(defun tlon-cleanup-consolidate-footnotes ()
"Consolidate consecutive footnotes."
(interactive)
(goto-char (point-min))
(let ((regex "\\[\\^\\([[:digit:]]\\{1,3\\}\\)\\]\\ ?\\[\\^\\([[:digit:]]\\{1,3\\}\\)\\]"))
  (while (re-search-forward regex nil t)
    (let* ((n1 (string-to-number (match-string-no-properties 1)))
	   (n2 (string-to-number (match-string-no-properties 2))))
      (replace-match "" nil nil)
      (let* ((fn1 (tlon-cleanup-get-footnote n1 'delete))
	     (fn2 (tlon-cleanup-get-footnote n2 'delete))
	     (consolidated (tlon-cleanup-consolidate-bibtex-keys (format "%s; %s" fn1 fn2))))
	(markdown-insert-footnote)
	(insert (format "%s." consolidated))
	(goto-char (point-min)))))))

(defun tlon-cleanup-get-footnote (n &optional delete)
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
	(tlon-cleanup-delete-footnote n))
      footnote-content)))

(defun tlon-cleanup-delete-footnote (n)
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

(defun tlon-cleanup-consolidate-bibtex-keys (string)
  "Consolidate Bibtex keys in STRING."
  (let ((start 0)
	matches)
    (while (string-match "\\[\\(@.*?\\)\\]" string start)
      (push (match-string 1 string) matches)
      (setq start (match-end 0)))
    (format "[%s]" (mapconcat 'identity (nreverse matches) "; "))))

;;;; Transient commands

;;;###autoload (autoload 'tlon-cleanup-menu "tlon-cleanup" nil t)
(transient-define-prefix tlon-cleanup-menu ()
  "`tlon-cleanup' menu."
  ["Cleanup"
   ("c" "Common" tlon-cleanup-common)
   ("e" "EA Forum" tlon-cleanup-eaf)
   ("k" "80,000 Hours" tlon-cleanup-80k)
   ("n" "Non-EAF" tlon-cleanup-non-eaf)]
  ["Footnotes"
   ("s" "Split into paragraphs" tlon-cleanup-split-footnotes-into-paragraphs)
   ("f" "Consolidate" tlon-cleanup-consolidate-footnotes)
   ("F" "Consolidate all" tlon-cleanup-consolidate-all-footnotes)]
  ["URLs"
   ("u" "Replace EAF URLs" tlon-cleanup-eaf-replace-urls)
   ("U" "Replace EAF URLs in repo" tlon-cleanup-eaf-replace-urls-in-repo)])

(provide 'tlon-cleanup)
;;; tlon-cleanup.el ends here
