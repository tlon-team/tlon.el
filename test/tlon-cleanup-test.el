;;; tlon-cleanup-test.el --- Tests for tlon-cleanup -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for text cleanup and transformation functions in
;; tlon-cleanup.el.  These functions process imported Markdown
;; documents, so incorrect behavior silently corrupts content.

;;; Code:

(require 'ert)
(require 'tlon-cleanup)

;;;; tlon-cleanup-consolidate-bibtex-keys

(ert-deftest tlon-cleanup-consolidate-bibtex-keys-single ()
  "Consolidate a single BibTeX reference."
  (should (equal "[@Smith2020]"
                 (tlon-cleanup-consolidate-bibtex-keys "[@Smith2020]"))))

(ert-deftest tlon-cleanup-consolidate-bibtex-keys-multiple ()
  "Consolidate multiple adjacent BibTeX references into one."
  (should (equal "[@Smith2020; @Jones2021]"
                 (tlon-cleanup-consolidate-bibtex-keys
                  "[@Smith2020][@Jones2021]"))))

(ert-deftest tlon-cleanup-consolidate-bibtex-keys-three ()
  "Consolidate three references."
  (should (equal "[@A; @B; @C]"
                 (tlon-cleanup-consolidate-bibtex-keys "[@A][@B][@C]"))))

(ert-deftest tlon-cleanup-consolidate-bibtex-keys-with-pages ()
  "Consolidate references that include page numbers."
  (should (equal "[@Smith2020, p. 42; @Jones2021, ch. 3]"
                 (tlon-cleanup-consolidate-bibtex-keys
                  "[@Smith2020, p. 42][@Jones2021, ch. 3]"))))

;;;; Buffer-based cleanup functions

(ert-deftest tlon-cleanup-unescape-chars-dots ()
  "Unescape escaped dots."
  (with-temp-buffer
    (insert "e\\.g\\.")
    (tlon-cleanup-unescape-chars)
    (should (equal "e.g." (buffer-string)))))

(ert-deftest tlon-cleanup-unescape-chars-brackets ()
  "Unescape escaped brackets."
  (with-temp-buffer
    (insert "\\[1\\]")
    (tlon-cleanup-unescape-chars)
    (should (equal "[1]" (buffer-string)))))

(ert-deftest tlon-cleanup-unescape-chars-dollar ()
  "Unescape escaped dollar signs."
  (with-temp-buffer
    (insert "\\$100")
    (tlon-cleanup-unescape-chars)
    (should (equal "$100" (buffer-string)))))

(ert-deftest tlon-cleanup-unescape-chars-unnecessary-escapes ()
  "Remove unnecessary escapes from characters like @ and quotes."
  (with-temp-buffer
    (insert "\\@user \\'quote\\' \\\"double\\\"")
    (tlon-cleanup-unescape-chars)
    (should (equal "@user 'quote' \"double\"" (buffer-string)))))

(ert-deftest tlon-cleanup-convert-hyphens-em-dash ()
  "Convert triple hyphens to em dashes."
  (with-temp-buffer
    (insert "hello---world")
    (tlon-cleanup-convert-hyphens)
    (should (equal "hello\u2014world" (buffer-string)))))

(ert-deftest tlon-cleanup-convert-hyphens-en-dash ()
  "Convert double hyphens to en dashes."
  (with-temp-buffer
    (insert "1990--2000")
    (tlon-cleanup-convert-hyphens)
    (should (equal "1990\u20132000" (buffer-string)))))

(ert-deftest tlon-cleanup-convert-hyphens-order-matters ()
  "Triple hyphens become em dashes, not en dash + hyphen."
  (with-temp-buffer
    (insert "a---b--c")
    (tlon-cleanup-convert-hyphens)
    (should (equal "a\u2014b\u2013c" (buffer-string)))))

(ert-deftest tlon-cleanup-remove-linebreaks-triples ()
  "Collapse triple newlines to double."
  (with-temp-buffer
    (insert "a\n\n\nb")
    (tlon-cleanup-remove-linebreaks)
    (should (equal "a\n\nb" (buffer-string)))))

(ert-deftest tlon-cleanup-format-heading-strips-bold ()
  "Remove bold markers from headings."
  (with-temp-buffer
    (insert "## **Bold Heading**\n")
    (tlon-cleanup-format-heading)
    (should (equal "## Bold Heading\n" (buffer-string)))))

(ert-deftest tlon-cleanup-format-heading-leaves-non-bold ()
  "Leave headings without bold markers untouched."
  (with-temp-buffer
    (insert "## Normal Heading\n")
    (tlon-cleanup-format-heading)
    (should (equal "## Normal Heading\n" (buffer-string)))))

;;;; tlon-cleanup--80k-strip-backrefs

(ert-deftest tlon-cleanup-80k-strip-backrefs-basic ()
  "Remove backreference links from footnote text."
  (should (equal "Footnote text"
                 (string-trim (tlon-cleanup--80k-strip-backrefs
                               "Footnote text [↩](#fn-ref-1)")))))

(ert-deftest tlon-cleanup-80k-strip-backrefs-no-match ()
  "Leave text unchanged when no backreference."
  (should (equal "Normal text" (tlon-cleanup--80k-strip-backrefs "Normal text"))))

(ert-deftest tlon-cleanup-80k-strip-backrefs-variant ()
  "Strip variant with ︎ character."
  (let ((result (tlon-cleanup--80k-strip-backrefs "Text [↩︎](#fn-42)")))
    (should (equal "Text" (string-trim result)))))

;;;; tlon-cleanup--80k-strip-indentation

(ert-deftest tlon-cleanup-80k-strip-indentation-basic ()
  "Remove 4-space indentation from start of text."
  (should (equal "First line" (tlon-cleanup--80k-strip-indentation "    First line"))))

(ert-deftest tlon-cleanup-80k-strip-indentation-multiline ()
  "Remove 4-space indentation from multiple lines."
  (should (equal "First\nSecond"
                 (tlon-cleanup--80k-strip-indentation "    First\n    Second"))))

(ert-deftest tlon-cleanup-80k-strip-indentation-no-indent ()
  "Leave unindented text unchanged."
  (should (equal "No indent" (tlon-cleanup--80k-strip-indentation "No indent"))))

(provide 'tlon-cleanup-test)
;;; tlon-cleanup-test.el ends here
