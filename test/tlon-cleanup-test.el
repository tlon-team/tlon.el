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

;;;; tlon-cleanup-unescape-lines

(ert-deftest tlon-cleanup-unescape-lines-basic ()
  "Replace backslash-escaped empty lines with plain newlines."
  (with-temp-buffer
    (insert "before\\\n\\\nafter")
    (tlon-cleanup-unescape-lines)
    (should (equal "before\n\nafter" (buffer-string)))))

(ert-deftest tlon-cleanup-unescape-lines-no-match ()
  "Leave buffer unchanged when no escaped lines are present."
  (with-temp-buffer
    (insert "plain\n\ntext")
    (tlon-cleanup-unescape-lines)
    (should (equal "plain\n\ntext" (buffer-string)))))

;;;; tlon-cleanup-remove-double-brackets

(ert-deftest tlon-cleanup-remove-double-brackets-closing ()
  "Reduce ]] to ]."
  (with-temp-buffer
    (insert "text]] more")
    (tlon-cleanup-remove-double-brackets)
    (should (equal "text] more" (buffer-string)))))

(ert-deftest tlon-cleanup-remove-double-brackets-opening ()
  "Reduce [[ to [."
  (with-temp-buffer
    (insert "text [[ more")
    (tlon-cleanup-remove-double-brackets)
    (should (equal "text [ more" (buffer-string)))))

(ert-deftest tlon-cleanup-remove-double-brackets-both ()
  "Reduce both [[ and ]] in the same buffer."
  (with-temp-buffer
    (insert "[[link]]")
    (tlon-cleanup-remove-double-brackets)
    (should (equal "[link]" (buffer-string)))))

(ert-deftest tlon-cleanup-remove-double-brackets-no-match ()
  "Leave single brackets unchanged."
  (with-temp-buffer
    (insert "[single]")
    (tlon-cleanup-remove-double-brackets)
    (should (equal "[single]" (buffer-string)))))

;;;; tlon-cleanup-remove-span-elements

(ert-deftest tlon-cleanup-remove-span-elements-basic ()
  "Remove a Pandoc span element."
  (with-temp-buffer
    (insert "text{.underline} more")
    (tlon-cleanup-remove-span-elements)
    (should (equal "text more" (buffer-string)))))

(ert-deftest tlon-cleanup-remove-span-elements-multiple-classes ()
  "Remove a span element with multiple classes."
  (with-temp-buffer
    (insert "word{.class1 .class2}")
    (tlon-cleanup-remove-span-elements)
    (should (equal "word" (buffer-string)))))

(ert-deftest tlon-cleanup-remove-span-elements-no-match ()
  "Leave text without span elements unchanged."
  (with-temp-buffer
    (insert "no spans here")
    (tlon-cleanup-remove-span-elements)
    (should (equal "no spans here" (buffer-string)))))

;;;; tlon-cleanup-fix-non-eaf-footnotes

(ert-deftest tlon-cleanup-fix-non-eaf-footnotes-basic ()
  "Convert sup-style footnote markers to standard Markdown."
  (with-temp-buffer
    (insert "Text[<sup>1</sup>](#fn-1) here.")
    (tlon-cleanup-fix-non-eaf-footnotes)
    (should (equal "Text[^1] here." (buffer-string)))))

(ert-deftest tlon-cleanup-fix-non-eaf-footnotes-multiple ()
  "Convert multiple sup-style footnote markers."
  (with-temp-buffer
    (insert "A[<sup>1</sup>](#fn-1) B[<sup>2</sup>](#fn-2)")
    (tlon-cleanup-fix-non-eaf-footnotes)
    (should (equal "A[^1] B[^2]" (buffer-string)))))

(ert-deftest tlon-cleanup-fix-non-eaf-footnotes-no-match ()
  "Leave text without sup-style markers unchanged."
  (with-temp-buffer
    (insert "Normal [^1] footnote.")
    (tlon-cleanup-fix-non-eaf-footnotes)
    (should (equal "Normal [^1] footnote." (buffer-string)))))

;;;; tlon-cleanup-split-footnotes-into-paragraphs

(ert-deftest tlon-cleanup-split-footnotes-into-paragraphs-basic ()
  "Insert blank lines before footnote definitions."
  (with-temp-buffer
    (insert "[^1]: First note\n[^2]: Second note\n")
    (tlon-cleanup-split-footnotes-into-paragraphs)
    (should (string-match-p "\n\n\\[\\^1\\]:" (buffer-string)))
    (should (string-match-p "\n\n\\[\\^2\\]:" (buffer-string)))))

;;;; tlon-cleanup--80k-delete-paren-block-at-point

(ert-deftest tlon-cleanup-80k-delete-paren-block-basic ()
  "Delete a simple balanced parenthesized block."
  (with-temp-buffer
    (insert "before(inner)after")
    (goto-char 7)  ; on the opening paren
    (tlon-cleanup--80k-delete-paren-block-at-point)
    (should (equal "beforeafter" (buffer-string)))))

(ert-deftest tlon-cleanup-80k-delete-paren-block-nested ()
  "Delete a nested parenthesized block."
  (with-temp-buffer
    (insert "x(a(b)c)y")
    (goto-char 2)  ; on the outer opening paren
    (tlon-cleanup--80k-delete-paren-block-at-point)
    (should (equal "xy" (buffer-string)))))

(ert-deftest tlon-cleanup-80k-delete-paren-block-quoted-parens ()
  "Parentheses inside double quotes are ignored."
  (with-temp-buffer
    (insert "x(a \"b)c\" d)y")
    (goto-char 2)  ; on the opening paren
    (tlon-cleanup--80k-delete-paren-block-at-point)
    (should (equal "xy" (buffer-string)))))

(ert-deftest tlon-cleanup-80k-delete-paren-block-not-on-paren ()
  "Do nothing when point is not on an opening paren."
  (with-temp-buffer
    (insert "no parens here")
    (goto-char 1)
    (tlon-cleanup--80k-delete-paren-block-at-point)
    (should (equal "no parens here" (buffer-string)))))

;;;; tlon-cleanup-fix-eaf-footnotes

(ert-deftest tlon-cleanup-fix-eaf-footnotes-caret-variant ()
  "Convert caret-wrapped EAF footnote markers."
  (with-temp-buffer
    (insert "Text ^[\\[1\\]](#fnabc123)^ more text.")
    (tlon-cleanup-fix-eaf-footnotes)
    (should (equal "Text [^1] more text." (buffer-string)))))

(ert-deftest tlon-cleanup-fix-eaf-footnotes-sequential-numbering ()
  "Footnotes are numbered sequentially in order of appearance."
  (with-temp-buffer
    (insert "A ^[\\[5\\]](#fn-id1)^ B ^[\\[3\\]](#fn-id2)^")
    (tlon-cleanup-fix-eaf-footnotes)
    (should (equal "A [^1] B [^2]" (buffer-string)))))

(ert-deftest tlon-cleanup-fix-eaf-footnotes-same-id-reuses-number ()
  "Duplicate references to the same footnote id reuse the number."
  (with-temp-buffer
    (insert "A ^[\\[1\\]](#fn-sameID)^ B ^[\\[1\\]](#fn-sameID)^")
    (tlon-cleanup-fix-eaf-footnotes)
    (should (equal "A [^1] B [^1]" (buffer-string)))))

(provide 'tlon-cleanup-test)
;;; tlon-cleanup-test.el ends here
