;;; tlon-bib-test.el --- Tests for tlon-bib -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for bibliography functions: abstract cleanup, name formatting,
;; date parsing, BibTeX key generation, and brace removal.

;;; Code:

(require 'ert)
(require 'tlon-bib)

;;;; tlon-abstract-cleanup

(ert-deftest tlon-abstract-cleanup-removes-xml-tags ()
  "Strip XML tags from abstract text."
  (should (equal "Hello world."
                 (tlon-abstract-cleanup "<p>Hello world</p>"))))

(ert-deftest tlon-abstract-cleanup-removes-latex-tags ()
  "Strip LaTeX-style tags."
  (should (equal "Hello."
                 (tlon-abstract-cleanup "{\\textless}.p{\\textgreater}Hello"))))

(ert-deftest tlon-abstract-cleanup-removes-leading-words ()
  "Strip 'abstract' or 'summary' prefix including trailing space."
  (should (equal "This is the text."
                 (tlon-abstract-cleanup "abstract This is the text.")))
  (should (equal "This is the text."
                 (tlon-abstract-cleanup "summary This is the text."))))

(ert-deftest tlon-abstract-cleanup-adds-trailing-period ()
  "Add a period at the end if missing."
  (should (equal "No period at end."
                 (tlon-abstract-cleanup "No period at end"))))

(ert-deftest tlon-abstract-cleanup-keeps-existing-period ()
  "Do not double a trailing period."
  (should (equal "Already has period."
                 (tlon-abstract-cleanup "Already has period."))))

;;;; tlon-bib-remove-braces

(ert-deftest tlon-bib-remove-braces-basic ()
  "Remove curly braces from a string."
  (should (equal "Hello World" (tlon-bib-remove-braces "{Hello} {World}"))))

(ert-deftest tlon-bib-remove-braces-nested ()
  "Remove nested braces."
  (should (equal "AB" (tlon-bib-remove-braces "{{A}{B}}"))))

(ert-deftest tlon-bib-remove-braces-nil ()
  "Return nil for nil input."
  (should (null (tlon-bib-remove-braces nil))))

(ert-deftest tlon-bib-remove-braces-no-braces ()
  "Return unchanged string when no braces present."
  (should (equal "plain" (tlon-bib-remove-braces "plain"))))

;;;; tlon-bib--join-names

(ert-deftest tlon-bib-join-names-two ()
  "Join two author names with 'and'."
  (should (equal "Alice and Bob"
                 (tlon-bib--join-names '("Alice" "Bob")))))

(ert-deftest tlon-bib-join-names-single ()
  "Single name returns as-is."
  (should (equal "Alice"
                 (tlon-bib--join-names '("Alice")))))

(ert-deftest tlon-bib-join-names-three ()
  "Join three names."
  (should (equal "Alice and Bob and Carol"
                 (tlon-bib--join-names '("Alice" "Bob" "Carol")))))

(ert-deftest tlon-bib-join-names-nil ()
  "Return nil for nil input."
  (should (null (tlon-bib--join-names nil))))

(ert-deftest tlon-bib-join-names-empty-strings ()
  "Return nil when all names are empty strings."
  (should (null (tlon-bib--join-names '("" "  ")))))

(ert-deftest tlon-bib-join-names-trims-whitespace ()
  "Trim whitespace from names."
  (should (equal "Alice and Bob"
                 (tlon-bib--join-names '("  Alice  " " Bob ")))))

;;;; tlon-bib--reverse-name

(ert-deftest tlon-bib-reverse-name-two-parts ()
  "Reverse 'First Last' to 'Last, First'."
  (should (equal "Doe, John" (tlon-bib--reverse-name "John Doe"))))

(ert-deftest tlon-bib-reverse-name-three-parts ()
  "Move only the last token to front."
  (should (equal "Smith, John Michael"
                 (tlon-bib--reverse-name "John Michael Smith"))))

(ert-deftest tlon-bib-reverse-name-already-reversed ()
  "Leave alone if already contains a comma."
  (should (equal "Doe, John" (tlon-bib--reverse-name "Doe, John"))))

(ert-deftest tlon-bib-reverse-name-single-token ()
  "Single-token name returned as-is."
  (should (equal "Aristotle" (tlon-bib--reverse-name "Aristotle"))))

(ert-deftest tlon-bib-reverse-name-empty ()
  "Empty string returned as-is."
  (should (equal "" (tlon-bib--reverse-name ""))))

(ert-deftest tlon-bib-reverse-name-nil ()
  "Nil input returns empty string."
  (should (equal "" (tlon-bib--reverse-name nil))))

;;;; tlon-bib--date-year

(ert-deftest tlon-bib-date-year-iso ()
  "Extract year from ISO date."
  (should (equal "2024" (tlon-bib--date-year "2024-03-15"))))

(ert-deftest tlon-bib-date-year-datetime ()
  "Extract year from datetime string."
  (should (equal "2023" (tlon-bib--date-year "2023-12-01T10:30:00"))))

(ert-deftest tlon-bib-date-year-only ()
  "Extract year from year-only string."
  (should (equal "2020" (tlon-bib--date-year "2020"))))

(ert-deftest tlon-bib-date-year-nil ()
  "Return nil for nil input."
  (should (null (tlon-bib--date-year nil))))

(ert-deftest tlon-bib-date-year-invalid ()
  "Return nil for non-date string."
  (should (null (tlon-bib--date-year "not a date"))))

;;;; tlon-bib--date-iso

(ert-deftest tlon-bib-date-iso-full ()
  "Normalize full ISO date."
  (should (equal "2024-03-15" (tlon-bib--date-iso "2024-03-15"))))

(ert-deftest tlon-bib-date-iso-strips-time ()
  "Strip time component from datetime."
  (should (equal "2024-03-15" (tlon-bib--date-iso "2024-03-15T10:30:00"))))

(ert-deftest tlon-bib-date-iso-year-only ()
  "Pad year-only to January 1st."
  (should (equal "2024-01-01" (tlon-bib--date-iso "2024"))))

(ert-deftest tlon-bib-date-iso-nil ()
  "Return nil for nil input."
  (should (null (tlon-bib--date-iso nil))))

(ert-deftest tlon-bib-date-iso-invalid ()
  "Return nil for invalid input."
  (should (null (tlon-bib--date-iso "not a date"))))

;;;; tlon-get-abstract-translation

(ert-deftest tlon-get-abstract-translation-found ()
  "Return translation when present."
  (let ((translations '(("key1" . "Translated abstract"))))
    (should (equal "Translated abstract"
                   (tlon-get-abstract-translation "key1" "es" translations)))))

(ert-deftest tlon-get-abstract-translation-not-found ()
  "Return nil when key not in translations."
  (let ((translations '(("key1" . "text"))))
    (should (null (tlon-get-abstract-translation "key2" "es" translations)))))

;;;; tlon-autokey-get-year

(ert-deftest tlon-autokey-get-year-four-digit ()
  "Extract last N digits from year."
  (let ((bibtex-autokey-year-length 4))
    (should (equal "2024" (tlon-autokey-get-year "2024")))))

(ert-deftest tlon-autokey-get-year-two-digit ()
  "Extract last 2 digits."
  (let ((bibtex-autokey-year-length 2))
    (should (equal "24" (tlon-autokey-get-year "2024")))))

(ert-deftest tlon-autokey-get-year-empty ()
  "Return empty string for empty year."
  (let ((bibtex-autokey-year-length 4))
    (should (equal "" (tlon-autokey-get-year "")))))

;;;; tlon-autokey-get-names

(ert-deftest tlon-autokey-get-names-single ()
  "Format a single author name."
  (let ((result (tlon-autokey-get-names "John Doe")))
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest tlon-autokey-get-names-empty ()
  "Return empty string for empty input."
  (should (equal "" (tlon-autokey-get-names ""))))

(ert-deftest tlon-autokey-get-names-multiple ()
  "Format multiple authors separated by 'and'."
  (let ((result (tlon-autokey-get-names "John Doe and Jane Smith")))
    (should (stringp result))
    (should (> (length result) 0))))

;;;; tlon-autokey-get-title

(ert-deftest tlon-autokey-get-title-basic ()
  "Format a simple title."
  (let ((result (tlon-autokey-get-title "The Meaning of Life")))
    (should (stringp result))))

(ert-deftest tlon-autokey-get-title-empty ()
  "Return empty string for empty title."
  (should (equal "" (tlon-autokey-get-title ""))))

;;;; tlon-generate-autokey

(ert-deftest tlon-generate-autokey-basic ()
  "Generate a BibTeX key from author, year, title."
  (let ((key (tlon-generate-autokey "John Doe" "2024" "The Meaning of Life")))
    (should (stringp key))
    (should (> (length key) 0))
    ;; bibtex-autokey-year-length defaults to 2, so "2024" -> "24"
    (should (string-match-p "24" key))))

(ert-deftest tlon-generate-autokey-empty-author ()
  "Generate a key with empty author."
  (let ((key (tlon-generate-autokey "" "2024" "Title")))
    (should (stringp key))
    (should (string-match-p "24" key))))

(ert-deftest tlon-generate-autokey-empty-all ()
  "Generate an empty key when all fields are empty."
  (let ((key (tlon-generate-autokey "" "" "")))
    (should (stringp key))))

(provide 'tlon-bib-test)
;;; tlon-bib-test.el ends here
