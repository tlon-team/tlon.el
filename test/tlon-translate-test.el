;;; tlon-translate-test.el --- Tests for tlon-translate -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for translation helpers: chunk range building, fuzzy regex
;; creation, and response paragraph splitting.

;;; Code:

(require 'ert)
(require 'tlon-translate)

;;;; tlon-translate--build-chunk-ranges

(ert-deftest tlon-translate-build-chunk-ranges-exact ()
  "Exact division into chunks."
  (should (equal '((0 . 5) (5 . 10))
                 (tlon-translate--build-chunk-ranges 10 5))))

(ert-deftest tlon-translate-build-chunk-ranges-remainder ()
  "Last chunk is smaller when total is not divisible."
  (should (equal '((0 . 3) (3 . 6) (6 . 7))
                 (tlon-translate--build-chunk-ranges 7 3))))

(ert-deftest tlon-translate-build-chunk-ranges-single ()
  "Single chunk when total <= chunk-size."
  (should (equal '((0 . 3))
                 (tlon-translate--build-chunk-ranges 3 10))))

(ert-deftest tlon-translate-build-chunk-ranges-zero ()
  "Zero total gives empty list."
  (should (null (tlon-translate--build-chunk-ranges 0 5))))

;;;; tlon-translate--create-fuzzy-regex

(ert-deftest tlon-translate-create-fuzzy-regex-basic ()
  "Plain spaces are not escaped by `regexp-quote', so the replacement
pattern (which targets backslash-escaped whitespace) does not apply.
The function only makes whitespace flexible when the input already
contains backslash-escaped whitespace sequences."
  (let ((re (tlon-translate--create-fuzzy-regex "hello world")))
    ;; With plain spaces, the regex is just the literal string
    (should (string-match-p re "hello world"))
    ;; Multiple spaces and newlines do NOT match because plain spaces
    ;; are not converted to flexible whitespace by this function
    (should-not (string-match-p re "hello  world"))
    (should-not (string-match-p re "hello\nworld"))))

(ert-deftest tlon-translate-create-fuzzy-regex-special-chars ()
  "Special regex characters are escaped."
  (let ((re (tlon-translate--create-fuzzy-regex "price: $10.00")))
    (should (string-match-p re "price: $10.00"))))

(ert-deftest tlon-translate-create-fuzzy-regex-no-whitespace ()
  "String without whitespace stays unchanged."
  (let ((re (tlon-translate--create-fuzzy-regex "hello")))
    (should (string-match-p re "hello"))))

;;;; tlon-translate--split-response-into-paragraphs

(ert-deftest tlon-translate-split-response-basic ()
  "Split response into paragraphs by double newline."
  (should (equal '("para one" "para two")
                 (tlon-translate--split-response-into-paragraphs
                  "para one\n\npara two" 2))))

(ert-deftest tlon-translate-split-response-strips-fences ()
  "Strip code fences from the response."
  (should (equal '("content here")
                 (tlon-translate--split-response-into-paragraphs
                  "```markdown\ncontent here\n```" 1))))

(ert-deftest tlon-translate-split-response-nil ()
  "Return nil for nil input."
  (should (null (tlon-translate--split-response-into-paragraphs nil 1))))

(ert-deftest tlon-translate-split-response-empty ()
  "Return nil for empty string."
  (should (null (tlon-translate--split-response-into-paragraphs "" 1))))

(ert-deftest tlon-translate-split-response-trims ()
  "Trim whitespace from individual paragraphs."
  (should (equal '("hello" "world")
                 (tlon-translate--split-response-into-paragraphs
                  "  hello  \n\n  world  " 2))))

(provide 'tlon-translate-test)
;;; tlon-translate-test.el ends here
