;;; tlon-babel-fix.el --- Manual & auto fix functionality -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon-babel
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

;; Manual & auto fix functionality

;;; Code:

(require 'tlon-babel-core)

;;;; Functions

;;;;; autofix

(defun tlon-babel-autofix (regexp-list newtext)
  "Replace matches in REGEXP-LIST with NEWTEXT."
  (widen)
  (save-excursion
    (dolist (regexp regexp-list)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match newtext)))))

(defun tlon-babel-autofix-curly-quotes ()
  "Replace straight quotes with curly quotes when appropriate."
  (tlon-babel-autofix '("\\([^\\.\\?]\"\\)\\[")
		      "\\1["))

(defun tlon-babel-autofix-footnote-punctuation ()
  "Place footnotes after punctuation mark."
  (tlon-babel-autofix '("\\(.\\)\\(\\[\\^[[:digit:]]\\{1,3\\}\\]\\)\\([[:punct:]]\\)")
		      "\\1\\3\\2")
  (tlon-babel-autofix-footnote-punctuation-amend))

(defun tlon-babel-autofix-footnote-punctuation-amend ()
  "Reverse undesired effects of `tlon-babel-autofix-footnote-punctuation'.
Ideally the function should be amended so that it doesn’t introduce these
effects to begin with."
  (tlon-babel-autofix '("\\[\\[\\^\\([0-9]+\\)\\]\\^\\([0-9]+\\)\\]"  ; fixes `[[^1]^2]'
			"\\[\\^\\[\\^\\([0-9]+\\)\\]\\([0-9]+\\)\\]") ; fixes `[^[^1]2]'
		      "[^\\1][^\\2]"))

(defun tlon-babel-autofix-periods-in-headings ()
  "Remove periods at the end of headings."
  (tlon-babel-autofix '("^\\(#\\{2,6\\}.*\\)\\.$")
		      "\\1"))

(defun tlon-babel-autofix-percent-signs ()
  "Add non-breaking space before percent sign."
  (tlon-babel-autofix '("\\([[:digit:],()]+\\)%\\([^\";[:alnum:]]\\)"
			"\\([[:digit:],()]+\\) %\\([^\";[:alnum:]]\\)")
		      "\\1 %\\2"))

;;;###autoload
(defun tlon-babel-autofix-all ()
  "Run all the `tlon-babel-autofix' commands."
  (interactive)
  (tlon-babel-autofix-curly-quotes)
  (tlon-babel-autofix-footnote-punctuation)
  (tlon-babel-autofix-periods-in-headings)
  (tlon-babel-autofix-percent-signs)
  (let ((after-save-hook (remove #'tlon-babel-autofix-all after-save-hook)))
    (save-buffer)
    (add-hook 'after-save-hook #'tlon-babel-autofix-all nil t)))

;;;;; manual-fix

(defun tlon-babel-manual-fix (regexp-list newtext &optional keep-case)
  "Prompt user to replace matches in REGEXP-LIST with NEWTEXT.
If KEEP-CASE is non-nil, keep the case of the matched text."
  (widen)
  (save-excursion
    (point-min)
    (dolist (regexp regexp-list)
      (goto-char (point-min))
      (let ((case-replace keep-case))
	(query-replace-regexp regexp newtext nil (point-min) (point-max))))))

(defun tlon-babel-manual-fix-em-dashes ()
  "Prompt the user to replace hyphens with em dashes, when appropriate."
  (tlon-babel-manual-fix '("\\([^ ][ ,)]\\)-\\([(\"[:alnum:]]\\)" ; opening dash
			   "\\([)\\.%\"[:alnum:]]\\)-\\([ ,(]\\)" ; closing dash
			   "\\([^ >)] \\)-\\( \\)")
			 "\\1—\\2"))

(defun tlon-babel-manual-fix-number-ranges ()
  "Prompt the user to replace hyphens with em dashes, when appropriate."
  (tlon-babel-manual-fix '("\\([ \\[]\\)\\([[:digit:]]\\{1,12\\}\\)-\\([[:digit:]]\\{1,12\\}\\)\\([,.:;?!   ]\\)")
			 "\\1\\2–\\3\\4"))

(defun tlon-babel-manual-fix-roman-numerals ()
  "Prompt the user to add small caps tags to roman numerals."
  (tlon-babel-manual-fix '(" \\b\\([IVXLCDM]+\\)\\b")
			 " <abbr>\\1</abbr>"))

(defun tlon-babel-manual-fix-thin-spaces ()
  "Prompt the user to add a thin space between abbreviations followed by a period."
  (tlon-babel-manual-fix '("\\([A-Z]\\.\\)\\([A-Z]\\)")
			 "\\1 \\2"))

(defun tlon-babel-manual-fix-solo ()
  "Prompt the user to replace `sólo' with `solo'."
  (tlon-babel-manual-fix '("sólo")
			 "solo"
			 'keep-case))

(defun tlon-babel-manual-fix-podcast ()
  "Prompt the user to replace `podcast' with `pódcast'.
Enchant/Aspell do not make the correct suggestion, so it's easier to use a
dedicated function."
  (tlon-babel-manual-fix '(" podcast")
			 " pódcast"
			 'keep-case))

(defun tlon-babel-manual-fix-all ()
  "Run all the `tlon-babel-manual-fix' commands."
  (interactive)
  (tlon-babel-manual-fix-em-dashes)
  (tlon-babel-manual-fix-number-ranges)
  (tlon-babel-manual-fix-roman-numerals)
  (tlon-babel-manual-fix-thin-spaces)
  (tlon-babel-manual-fix-solo)
  (tlon-babel-manual-fix-podcast))

(defun tlon-babel-fix-internet-archive-links ()
  "Fix Internet Archive links in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((cnt 0))
      (while (re-search-forward "https://web\.archive\.org/web/[[:digit:]]*?/" nil t)
	(replace-match "")
	(setq cnt (1+ cnt)))
      (message "Done. %d URLs were fixed." cnt))))

(provide 'tlon-babel-fix)
;;; tlon-babel-fix.el ends here

