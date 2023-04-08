;;; tlon.el --- A collection of convenience functions to be used by the Tlön team. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.0
;; Package-Requires: ((org "9.1") (dired "1.0") (ox-hugo "1.2.0"))
;; Homepage: https://tlon.team
;; Keywords: convenience tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(defun ps/tlon-meeting-with (person tareas-id meetings-id pending-id)
  "TODO: docstring"
  ;; "[person] > Meetings > Pending for next meeting" org heading in `work-dashboard.org'
  (ps/org-id-goto pending-id)
  (let ((contents (ps/org-get-heading-contents)))
    (ps/org-clear-heading-contents)
    ;; "[person] > Meetings" org heading in `work-dashboard.org'
    (ps/org-id-goto meetings-id)
    (org-narrow-to-subtree)
    (goto-char (point-max))
    (org-insert-heading)
    (insert "Meeting on ")
    (ps/org-time-stamp-inactive-current-time)
    (unless (string= contents "")
      (insert (concat "\nTo discuss:\n" contents "\n"))))
  (forward-line)
  (ps/org-narrow-to-entry-and-children)
  (ps/window-split-if-unsplit)
  (ps/switch-to-last-window)
  ;; "Tareas" org heading in `tareas.org'
  (ps/org-id-goto tareas-id)
  (ps/org-narrow-to-entry-and-children)
  (ps/switch-to-last-window))

(defun ps/tlon-bae-format-file (&optional extension)
  "Return a file name based on user supplied information.
If EXTENSION is not provided, markdown is used."
  (let* ((lastname (read-string "Last name(s) [separated by spaces if more than one author]: "))
	 (title (read-string "Title: "))
	 (slug-lastname (ps/bibtex-asciify-string (org-hugo-slug lastname)))
	 (slug-title (ps/bibtex-asciify-string (org-hugo-slug title)))
	 (extension (or extension "md")))
    (file-name-with-extension (concat slug-lastname "--" slug-title) extension)))

(defun ps/tlon-bae-rename-file (&optional extension)
  "Rename file at point based on user-supplied information.
If EXTENSION is not provided, markdown is used."
  (interactive)
  (let* ((source-file-path (dired-get-filename)))
    (rename-file
     source-file-path
     (file-name-concat
      (file-name-directory source-file-path)
      (ps/tlon-bae-format-file extension))))
  (revert-buffer))

(defun ps/tlon-bae-create-file (&optional extension)
  "Create a new file based on user-supplied information.
Prompt the user for bibliographic information and create a new
 file based on it in the current directory. If EXTENSION is not
 provided, markdown is used."
  (interactive)
  (find-file (ps/tlon-bae-format-file extension)))

(defvar ps/tlon-markdown-eawiki-footnote-source
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\{1,2\\}\\)\\\\\\]\\](#.+?)\\^\\]{#.+? .footnote-reference role=\"doc-noteref\"}"
  "Regexp to match footnotes in the main body.")

(defvar ps/tlon-markdown-eawiki-footnote-source2
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\)\\\\\\]\\](#.+?)\\^\\]{#\\\\\\\\\\\\\".+?\\\\\\\\\\\\\" .\\\\\\\\\\\\\\\"footnote-reference\\\\\\\\\\\\\" role=\"\\\\\\\\\\\\\"doc-noteref\\\\\\\\\\\\\"\"}"
  "Regexp to match footnotes in the main body.")

(defvar ps/tlon-markdown-eawiki-footnote-target
  "\\([[:digit:]]\\{1,2\\}\\). *?\\[\\[^\\*\\*\\[\\\\^\\](#.+?)\\*\\*^\\]{\\.footnote-back-link}\\]{#.+?}

    ::: footnote-content"

  "Regexp to match footnotes in the footnote section.")

(defvar ps/tlon-markdown-eawiki-footnote-target2
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[^\\*\\*\\[\\\\^\\](#.+?)\\*\\*^\\]{\..+?}\\]{#.+?}

    ::: footnote-content "
  "Regexp to match footnotes in the footnote section.")

(defvar ps/tlon-markdown-eawiki-footnote-target3
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[\\^\\*\\*\\[\\\\\\^\\](\\\\%22#.+?\\\\%22)\\*\\*\\^\\]{\\.\\\\\\\\\\\\\"footnote-back-link\\\\\\\\\\\\\"}\\]{#\\\\\\\\\\\\\\\".+?\\\\\\\\\\\\\\\"}

    ::: \\\\\"footnote-content\\\\\" "
  "Regexp to match footnotes in the footnote section.")

(defvar ps/tlon-markdown-eawiki-footnote-target4
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[^\\*\\*\\[\\\\^\\](.+?)\\*\\*^\\]\\]{#.+?}

    footnote-content "
  "Regexp to match footnotes in the footnote section.")

(defvar ps/tlon-markdown-eawiki-footnote-target5  
  "\\([[:digit:]]\\{1,2\\}\\)\\. *?\\[\\[^\\*\\*\\[\\\\^\\](#.+?)\\*\\*^\\]\\]{#.+?}

    footnote-content "
  "Regexp to match footnotes in the footnote section.")

(defvar ps/tlon-markdown-eawiki-links
  "\\[\\(.+?\\)\\](\\\\%22\\(.+?\\)\\\\%22)"
  "Regexp to match links.")

(defvar ps/tlon-markdown-eawiki-escaped-quotes
  "\\\\\\\\\\\\\""
  "Regexp to match escaped quotes.")

(defun ps/tlon-markdown-eawiki-cleanup (&optional buffer)
  "Cleanup the buffer visiting an EA Wiki entry.
Assumes that the entry was imported using the GraphQL query below
and converted to Markdown with Pandoc using `pandoc -s
[source.html] -t markdown -o [destination.md]'.

`{
  tag(input:{selector:{slug:\"[slug]\"}}) {
    result {
      name
      description {
	html
      }
      parentTag {
	name
      }
    }
  }
}'"
  (interactive)
  (when (not (eq major-mode 'markdown-mode))
    (user-error "Not in a Markdown buffer"))
  (save-excursion
    (unfill-region (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "{.underline}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "{.footnote-back-link}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-source nil t)
      (replace-match (format "[^%s] " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-source2 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-target nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-target2 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-target3 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-target4 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-footnote-target5 nil t)
      (replace-match (format "[^%s]: " (match-string 1))))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-links nil t)
      (replace-match (format "[%s](%s)" (match-string 1) (match-string 2)) nil t))
    (goto-char (point-min))
    (while (re-search-forward ps/tlon-markdown-eawiki-escaped-quotes nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (re-search-forward " " nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward " :::" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "{.underline}" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "{.footnote-back-link}" nil t)
      (replace-match ""))
    (fill-region (point-min) (point-max))
    (save-buffer)
    ))

(defun ps/tlon-convert-to-markdown ()
  "Convert a file from EA Wiki to Markdown."
  (interactive)
  (dolist (file (directory-files "." nil "\\.md$"))
    ;; (shell-command (format "pandoc -s '%s' -t markdown -o '%s'"
    ;; file
    ;; (file-name-with-extension file "md")))
    (with-current-buffer (find-file-noselect (file-name-with-extension file "md"))
      (message "Cleaning up %s" (buffer-name))
      (ps/tlon-markdown-eawiki-cleanup))))

  (provide 'tlon)
;;; tlon.el ends here
