;;; tlon-fix.el --- Manual & auto fix functionality -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon
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

(require 'tlon-core)

;;;;;; Variables

(defconst tlon-fix-french-translation
  '((") - \\[" . ") • [")
    (" - " . " — ")
    ("  — " . "  - ")
    ("## Pour savoir plus" . "## Pour en savoir plus")
    ("type :" . "type:")
    ("title :" . "title:")
    ("\ntitre :" . "\ntitle:")
    ("\ntitre:" . "\ntitle:")
    ("\\*\\*L’" . "L’**")
    ("\\*\\*L'" . "L’**")
    ("\\*\\*La " . "La **")
    ("\\*\\*Le " . "Le **")
    ("\\*\\*Les " . "Les **")
    ("\\*\\*Un " . "Un **")
    ("\\*\\*Une " . "Une **")
    ("## Entrées associées \\." . "## Entrées associées")
    ("## Entrées associées\\." . "## Entrées associées")
    (" \"" . " « ")
    ;; ("\" " . " » ")
    (" » />" . "\" />")
    (" » />" . "\" />")
    (" » short />" . "\" short />")
    (" » court />" . "\" short />")
    ("\" court />" . "\" short />")
    ("=« " . "=\"")
    (" » />" . "\" />")
    ("\"\\." . " ».")
    ("\"\\?" . " »?")
    ("\"!" . " »!")
    ("\":" . " »:")
    ("\";" . " »;")
    ("\"," . " »,")
    ("\")" . " »)")
    ("(\"" . "(« ")
    ("(\"" . "(« ")
    ("] : <Footnote />" . "]: <Footnote />")
    ("] : <Sidenote />" . "]: <Sidenote />")
    ("] (\\." . "](.")
    ("] (htt" . "](htt")
    ("(\\. /" . "(./")
    ("\\. md)" . ".md)")
    ("\\. \\./" . "../")
    ("\\.\\. /" . "../")
    (" :" . " :")
    (" ;" . " ;")
    (" \\?" . " ?")
    (" !" . " !")
    (" %" . " %")
    ("\\[\\[" . "[")
    ("\\]\\]" . "]")
    ("{“" . "\"")
    ("{« " . "\"")
    ("{«" . "\"")
    ("”}" . "\"")
    (" »}" . "\"")
    ("»}" . "\"")
    (">\\. />" . " />")
    ("\"})\\. />". "\" />")
    ("\"} })\\. />". "\" />")
    ("={" . "=")
    ("} />" . " />")
    ("<Note />" . "<Footnote />")
    ("\"}\\. />" . "\" />")
    ("\"\\[\\^" . " »[^")
    ("« " . "« ")
    (" »" . " »")
    ("«" . "« ")
    ("»" . " »")
    ("  " . " "))
  "Search and replace pairs for fixing common issues in French translations.")

(defconst tlon-fix-italian-translation
  '((") - \\[" . ") • [")
    (" - " . " — ")
    ("  — " . "  - ")
    ;; ("tipo:" . "type:")
    ;; ("titolo:" . "title:")
    ;; ("\ntitre:" . "\ntitle:")
    ;; ("\\*\\*Il " . "Il **")
    ;; ("\\*\\*L’" . "L’**")
    ;; ("\\*\\*L'" . "L’**")
    ;; ("\\*\\*Un’" . "Un’**")
    ;; ("\\*\\*Un'" . "Un’**")
    ;; ("\\*\\*La " . "La **")
    ;; ("\\*\\*Le " . "Le **")
    ;; ("\\*\\*Gli " . "Gli **")
    ;; ("\\*\\*Un " . "Un **")
    ;; ("\\*\\*Una " . "Una **")
    ("## Voci correlate \\." . "## Voci correlate")
    ("## Voci correlate\\." . "## Voci correlate")
    ("] (\\." . "](.")
    ("] (htt" . "](htt")
    ("\\. md)" . ".md)")
    ("\\. \\./" . "../")
    ("\\.\\. /" . "../")
    ("(\\. /" . "(./")
    ("\\[\\[" . "[")
    ("\\]\\]" . "]")
    ("{“" . "{\"")
    ("\”}" . "\"}")
    (">\\. />" . " />")
    ("\"})\\. />". "\"} />")
    ("\"} })\\. />". "\"} />")
    ("<Nota />" . "<Footnote />")
    ("\"}\\. />" . "\"} />"))
  "Search and replace pairs for fixing common issues in Italian translations.")

(defconst tlon-fix-translations
  `(("fr" . ,tlon-fix-french-translation)
    ("it" . ,tlon-fix-italian-translation)
    ;; ("es" . ,tlon-fix-spanish-translation)
    ;; ("de" . ,tlon-fix-german-translation)
    )
  "Alist of language codes and their corresponding fix variable.")

;;;; Functions

;;;;; autofix

(defun tlon-autofix (regexp-list newtext)
  "Replace matches in REGEXP-LIST with NEWTEXT."
  (widen)
  (save-excursion
    (dolist (regexp regexp-list)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match newtext)))))

(defun tlon-autofix-curly-quotes ()
  "Replace straight quotes with curly quotes when appropriate."
  (tlon-autofix '("\\([^\\.\\?]\"\\)\\[")
		"\\1["))

(defun tlon-autofix-footnote-punctuation ()
  "Place footnotes after punctuation mark."
  (let ((language (tlon-repo-lookup :language :dir (tlon-get-repo 'no-prompt))))
    (when (member language '("en" "es")) ; usage varies across languages
      (tlon-autofix '("\\(.\\)\\(\\[\\^[[:digit:]]\\{1,3\\}\\]\\)\\([,\\.:;\\?!]\\)")
		    "\\1\\3\\2")
      (tlon-autofix-footnote-punctuation-amend))))

(defun tlon-autofix-footnote-punctuation-amend ()
  "Reverse undesired effects of `tlon-autofix-footnote-punctuation'.
Ideally the function should be amended so that it doesn’t introduce these
effects to begin with."
  (tlon-autofix '("\\[\\[\\^\\([0-9]+\\)\\]\\^\\([0-9]+\\)\\]"  ; fixes `[[^1]^2]'
		  "\\[\\^\\[\\^\\([0-9]+\\)\\]\\([0-9]+\\)\\]") ; fixes `[^[^1]2]'
		"[^\\1][^\\2]"))

(defun tlon-autofix-periods-in-headings ()
  "Remove periods at the end of headings."
  (tlon-autofix '("^\\(#\\{2,6\\}.*\\)\\.$")
		"\\1"))

(defun tlon-autofix-percent-signs ()
  "Add non-breaking space before percent sign.
The character used is U+202F NARROW NO-BREAK SPACE."
  (tlon-autofix '("\\([[:digit:],()]+\\)%\\([^\";[:alnum:]]\\)"
		  "\\([[:digit:],()]+\\) %\\([^\";[:alnum:]]\\)")
		"\\1 %\\2"))

(defun tlon-autofix-thin-spaces ()
  "Replace thin spaces with narrow spaces.
This replaces the character U+2009 THIN SPACE with U+202F NARROW NO-BREAK SPACE.
Thin and narrow spaces have exactly the same width, but only the latter are
non-breaking."
  (tlon-autofix '(" ")
		" "))

(defun tlon-autofix-superscripts ()
  "Replace carets with `<sup'> tags."
  (tlon-autofix '("\\^\\(?1:[[:digit:]]+\\)\\^")
		"<sup>\\1</sup>"))

(defvar markdown-regex-link-inline)
(defvar ffap-url-regexp)
(declare-function tlon-md-get-tag-pattern "tlon-md")
(defun tlon-autofix-thousands-separators (&optional separator)
  "Replace thousands SEPARATOR (typically, a comma or a period) with narrow spaces.
Do not perform these replacements if the terms occur in math formulae, links, or
match certain words that should not be altered, such as \"80,000 Hours\"."
  (interactive)
  (let* ((exclusions `(,(tlon-md-get-tag-pattern "Math")
		       ,markdown-regex-link-inline
		       ,ffap-url-regexp
		       "80,000 Hours"))
	 (exclusion-patterns (mapconcat #'identity exclusions "\\|"))
	 protected-ranges)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward exclusion-patterns nil t)
	(push (cons (match-beginning 0) (match-end 0)) protected-ranges))
      (let ((separator (or separator (tlon-get-thousands-separator))))
	(tlon-number-separators-perform-replacements separator protected-ranges)))))

(defun tlon-number-separators-perform-replacements (separator protected-ranges)
  "Replace thousands SEPARATOR with default separator, except in PROTECTED-RANGES."
  (goto-char (point-min))
  (let* ((digit-pattern (tlon-get-number-separator-pattern separator nil 'bounded)))
    (while (re-search-forward digit-pattern nil t)
      (unless (tlon-is-in-protected-range-p (match-beginning 0) (match-end 0) protected-ranges)
	(replace-match (replace-regexp-in-string (regexp-quote separator)
						 tlon-default-thousands-separator
						 (match-string-no-properties 1)))))))

(defun tlon-is-in-protected-range-p (start end protected-ranges)
  "Check if range from START to END overlaps with any PROTECTED-RANGES."
  (cl-loop for (pstart . pend) in protected-ranges
	   thereis (not (or (< end pstart) (> start pend)))))

;;;###autoload
(defun tlon-autofix-all ()
  "Run all the `tlon-autofix' commands."
  (interactive)
  (tlon-autofix-curly-quotes)
  (tlon-autofix-footnote-punctuation)
  (tlon-autofix-periods-in-headings)
  (tlon-autofix-percent-signs)
  (tlon-autofix-thousands-separators)
  (tlon-autofix-thin-spaces)
  (tlon-autofix-superscripts)
  (let ((after-save-hook (remove #'tlon-autofix-all after-save-hook)))
    (save-buffer)
    (add-hook 'after-save-hook #'tlon-autofix-all nil t)))

;;;;; manual-fix

(defun tlon-manual-fix (regexp-list newtext &optional keep-case)
  "Prompt user to replace matches in REGEXP-LIST with NEWTEXT.
If KEEP-CASE is non-nil, keep the case of the matched text."
  (widen)
  (save-excursion
    (point-min)
    (dolist (regexp regexp-list)
      (goto-char (point-min))
      (let ((case-replace keep-case))
	(query-replace-regexp regexp newtext nil (point-min) (point-max))))))

(defun tlon-manual-fix-em-dashes ()
  "Prompt the user to replace hyphens with em dashes, when appropriate."
  (tlon-manual-fix '("\\([^ ][ ,)]\\)-\\([(\"[:alnum:]]\\)" ; opening dash
		     "\\([)\\.%\"[:alnum:]]\\)-\\([ ,(]\\)" ; closing dash
		     "\\([^ >)] \\)-\\( \\)")
		   "\\1—\\2"))

(defun tlon-manual-fix-number-ranges ()
  "Prompt the user to replace hyphens with em dashes, when appropriate."
  (tlon-manual-fix '("\\([ \\[]\\)\\([[:digit:]]\\{1,12\\}\\)-\\([[:digit:]]\\{1,12\\}\\)\\([,.:;?!   ]\\)")
		   "\\1\\2–\\3\\4"))

(defun tlon-manual-fix-roman-numerals ()
  "Prompt the user to add small caps tags to roman numerals."
  (tlon-manual-fix '(" \\b\\([IVXLCDM]+\\)\\b")
		   " <abbr>\\1</abbr>"))

(defun tlon-manual-fix-narrow-spaces ()
  "Prompt user to add a narrow space between abbreviations followed by a period.
The character used is U+202F NARROW NO-BREAK SPACE."
  (tlon-manual-fix '("\\([A-Z]\\.\\)\\([A-Z]\\)")
		   "\\1 \\2"))

(defun tlon-manual-fix-solo ()
  "Prompt the user to replace `sólo' with `solo'."
  (tlon-manual-fix '("sólo")
		   "solo"
		   'keep-case))

(defun tlon-manual-fix-podcast ()
  "Prompt the user to replace `podcast' with `pódcast'.
Enchant/Aspell do not make the correct suggestion, so it's easier to use a
dedicated function."
  (tlon-manual-fix '(" podcast")
		   " pódcast"
		   'keep-case))

(defun tlon-manual-fix-all ()
  "Run all the `tlon-manual-fix' commands."
  (interactive)
  (tlon-manual-fix-em-dashes)
  (tlon-manual-fix-number-ranges)
  (tlon-manual-fix-roman-numerals)
  (tlon-manual-fix-narrow-spaces)
  (tlon-manual-fix-solo)
  (tlon-manual-fix-podcast))

(defun tlon-fix-internet-archive-links ()
  "Fix Internet Archive links in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((cnt 0))
      (while (re-search-forward "https://web\.archive\.org/web/[[:digit:]]*?/" nil t)
	(replace-match "")
	(setq cnt (1+ cnt)))
      (message "Done. %d URLs were fixed." cnt))))

;;;;;; Language-specific

(declare-function dired-get-marked-files "dired")
(defun tlon-fix-translation (lang)
  "Fix common issues in translations in LANG."
  (if-let ((files (or (dired-get-marked-files) (list (buffer-file-name)))))
      (dolist (file files)
	(with-current-buffer (find-file-noselect file)
	  (tlon-fix-translation-in-file lang)
	  (message "Fixed translation in %s" file)))
    (user-error "Buffer is not visiting a file")))

(defun tlon-fix-translation-in-file (lang)
  "Fix common issues in translations in LANG."
  (let ((var (alist-get lang tlon-fix-translations nil nil #'string=))
        (case-fold-search nil)) ;; Make sure searches are case-sensitive
    (dolist (cons var)
      (let ((search (car cons))
	    (replace (cdr cons)))
	(tlon-autofix (list search) replace)))))

;;;;;;; French

(defun tlon-fix-french-translation ()
  "Fix common issues in French translations."
  (interactive)
  (tlon-fix-translation "fr"))

;;;;;;; Italian

(defun tlon-fix-italian-translation ()
  "Fix common issues in Italian translations."
  (interactive)
  (tlon-fix-translation "it"))

(provide 'tlon-fix)
;;; tlon-fix.el ends here
