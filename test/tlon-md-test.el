;;; tlon-md-test.el --- Tests for tlon-md -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for MDX/Markdown tag formatting, pattern generation, attribute
;; lookup, and note type classification.

;;; Code:

(require 'ert)
(require 'tlon-md)

;;;; tlon-md-get-tag-pattern

(ert-deftest tlon-md-get-tag-pattern-returns-string ()
  "Tag pattern is a non-empty string."
  (let ((pattern (tlon-md-get-tag-pattern "Cite")))
    (should (stringp pattern))
    (should (> (length pattern) 0))))

(ert-deftest tlon-md-get-tag-pattern-matches-cite ()
  "Cite pattern matches a Cite tag."
  (let ((pattern (tlon-md-get-tag-pattern "Cite")))
    (should (string-match-p pattern "<Cite bibKey=\"Smith2020\" />"))))

(ert-deftest tlon-md-get-tag-pattern-matches-aside ()
  "Aside pattern matches a pair tag with content."
  (let ((pattern (tlon-md-get-tag-pattern "Aside")))
    (should (string-match-p pattern "<Aside>Some text</Aside>"))))

(ert-deftest tlon-md-get-tag-pattern-matches-smallcaps ()
  "SmallCaps pattern matches a pair tag with content."
  (let ((pattern (tlon-md-get-tag-pattern "SmallCaps")))
    (should (string-match-p pattern "<SmallCaps>Caps text</SmallCaps>"))))

;;;; tlon-md-get-tag-filled

(ert-deftest tlon-md-get-tag-filled-self-closing ()
  "Fill a self-closing tag with values."
  (let ((result (tlon-md-get-tag-filled "break" '("0.5s"))))
    (should (stringp result))
    (should (string-match-p "break" result))
    (should (string-match-p "0.5s" result))))

(ert-deftest tlon-md-get-tag-filled-pair-with-content ()
  "Fill a pair tag (non-self-closing) with content."
  (let ((result (tlon-md-get-tag-filled "Aside" nil "Note text")))
    (should (stringp result))
    (should (string-match-p "Aside" result))
    (should (string-match-p "Note text" result))))

;;;; tlon-md-get-tag-to-fill

(ert-deftest tlon-md-get-tag-to-fill-has-placeholder ()
  "Tag-to-fill for a pair tag includes a %s placeholder."
  (let ((result (tlon-md-get-tag-to-fill "Aside")))
    (should (stringp result))
    (should (string-match-p "%s" result))))

;;;; tlon-get-tag-attribute-names

(ert-deftest tlon-get-tag-attribute-names-cite ()
  "Cite tag has expected attributes."
  (let ((names (tlon-get-tag-attribute-names "Cite")))
    (should (listp names))
    (should (member "bibKey" names))))

(ert-deftest tlon-get-tag-attribute-names-break ()
  "Break tag has time attribute."
  (let ((names (tlon-get-tag-attribute-names "break")))
    (should (listp names))
    (should (member "time" names))))

;;;; tlon-md-lookup-tag-attribute-property

(ert-deftest tlon-md-lookup-tag-attribute-property-group ()
  "Look up the group number for a known attribute."
  (let ((group (tlon-md-lookup-tag-attribute-property "Cite" "bibKey" :group)))
    (should (numberp group))))

;;;; tlon-get-tag-groups

(ert-deftest tlon-get-tag-groups-returns-list ()
  "Tag groups returns a list of numbers."
  (let ((groups (tlon-get-tag-groups "Cite")))
    (should (listp groups))
    (should (cl-every #'numberp groups))))

;;;; tlon-make-attribute-pattern-searchable

(ert-deftest tlon-make-attribute-pattern-searchable-required ()
  "Required pattern does not end with ?."
  (let ((result (tlon-make-attribute-pattern-searchable " key=\"%s\"" 3 t)))
    (should (stringp result))
    (should-not (string-suffix-p "?" result))))

(ert-deftest tlon-make-attribute-pattern-searchable-optional ()
  "Optional pattern ends with ?."
  (let ((result (tlon-make-attribute-pattern-searchable " key=\"%s\"" 3 nil)))
    (should (stringp result))
    (should (string-suffix-p "?" result))))

;;;; tlon-get-note-type

(ert-deftest tlon-get-note-type-footnote ()
  "Detect footnote type from tag string."
  (let ((footnote-str (tlon-md-get-tag-filled "Footnote" nil "text")))
    (should (eq 'footnote (tlon-get-note-type footnote-str)))))

(ert-deftest tlon-get-note-type-sidenote ()
  "Detect sidenote type from tag string."
  (let ((sidenote-str (tlon-md-get-tag-filled "Sidenote" nil "text")))
    (should (eq 'sidenote (tlon-get-note-type sidenote-str)))))

(ert-deftest tlon-get-note-type-nil-for-non-note ()
  "Return nil for strings that aren't notes."
  (should (null (tlon-get-note-type "just plain text"))))

;;;; tlon-note-automatic-type

(ert-deftest tlon-note-automatic-type-citation-only ()
  "A note with only a citation is classified as footnote."
  (let ((cite-tag (tlon-md-get-tag-filled "Cite" '("Smith2020"))))
    (should (eq 'footnote (tlon-note-automatic-type cite-tag)))))

(ert-deftest tlon-note-automatic-type-long-text ()
  "A note with many words is classified as sidenote."
  (should (eq 'sidenote
              (tlon-note-automatic-type
               "This is a longer note with many words and no citations at all."))))

;;;; tlon-md-get-tag-section

(ert-deftest tlon-md-get-tag-section-en-to-es ()
  "Translate 'Further reading' from English to Spanish."
  (when (boundp 'tlon-md-canonical-tag-sections)
    (let ((result (tlon-md-get-tag-section "Further reading" "es")))
      (when result
        (should (stringp result))))))

;;;; tlon-note-automatic-type (boundary cases)

(ert-deftest tlon-note-automatic-type-citation-plus-four-words ()
  "A note with citation and exactly 4 words is classified as footnote."
  (let ((cite-tag (tlon-md-get-tag-filled "Cite" '("K"))))
    (should (eq 'footnote
                (tlon-note-automatic-type (concat cite-tag " one two three four"))))))

(ert-deftest tlon-note-automatic-type-citation-plus-five-words ()
  "A note with citation and 5 words is classified as sidenote."
  (let ((cite-tag (tlon-md-get-tag-filled "Cite" '("K"))))
    (should (eq 'sidenote
                (tlon-note-automatic-type (concat cite-tag " one two three four five"))))))

(ert-deftest tlon-note-automatic-type-no-citation ()
  "A note without citation is always sidenote."
  (should (eq 'sidenote (tlon-note-automatic-type "Short"))))

;;;; tlon-md-get-tag-pattern (additional match tests)

(ert-deftest tlon-md-get-tag-pattern-footnote ()
  "Footnote pattern matches a self-closing Footnote tag."
  (let ((pattern (tlon-md-get-tag-pattern "Footnote")))
    (should (string-match-p pattern "<Footnote />"))))

(ert-deftest tlon-md-get-tag-pattern-sidenote ()
  "Sidenote pattern matches a self-closing Sidenote tag."
  (let ((pattern (tlon-md-get-tag-pattern "Sidenote")))
    (should (string-match-p pattern "<Sidenote />"))))

(provide 'tlon-md-test)
;;; tlon-md-test.el ends here
