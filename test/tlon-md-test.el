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

;;;; tlon-md-tag-list

(ert-deftest tlon-md-tag-list-returns-list ()
  "Tag list returns a non-empty list of strings."
  (let ((tags (tlon-md-tag-list)))
    (should (listp tags))
    (should (> (length tags) 0))
    (should (cl-every #'stringp tags))))

(ert-deftest tlon-md-tag-list-contains-known-tags ()
  "Tag list contains known tag names."
  (let ((tags (tlon-md-tag-list)))
    (should (member "Cite" tags))
    (should (member "Aside" tags))))

;;;; tlon-md-format-tag

(ert-deftest tlon-md-format-tag-self-closing-match-string ()
  "Self-closing tag produces a regex pattern in match-string mode."
  (let ((pattern (tlon-md-format-tag "Cite" nil 'get-match-string)))
    (should (stringp pattern))
    (should (string-match-p pattern "<Cite bibKey=\"test\" />"))))

(ert-deftest tlon-md-format-tag-pair-match-string ()
  "Pair tag produces a regex pattern matching open+content+close."
  (let ((pattern (tlon-md-format-tag "Aside" nil 'get-match-string)))
    (should (stringp pattern))
    (should (string-match-p pattern "<Aside>hello world</Aside>"))))

(ert-deftest tlon-md-format-tag-self-closing-values ()
  "Self-closing tag with values returns a list with one element."
  (let ((result (tlon-md-format-tag "Cite" '("Smith2020") 'get-values)))
    (should (listp result))
    (should (= 1 (length result)))
    (should (string-match-p "Smith2020" (car result)))))

(ert-deftest tlon-md-format-tag-pair-placeholders ()
  "Pair tag with placeholders returns a cons cell."
  (let ((result (tlon-md-format-tag "Aside" nil 'get-placeholders)))
    (should (consp result))
    (should (stringp (car result)))
    (should (stringp (cdr result)))
    (should (string-match-p "Aside" (car result)))
    (should (string-match-p "Aside" (cdr result)))))

;;;; tlon-offset-timestamps

(ert-deftest tlon-md-offset-timestamps-basic ()
  "Offset a single timestamp by a fixed amount."
  (with-temp-buffer
    (insert "[01:30] Some text")
    (tlon-offset-timestamps "00:30")
    (should (equal "[02:00] Some text" (buffer-string)))))

(ert-deftest tlon-md-offset-timestamps-multiple ()
  "Offset multiple timestamps."
  (with-temp-buffer
    (insert "[00:00] Start\n[01:30] Middle\n[10:00] End")
    (tlon-offset-timestamps "05:00")
    (should (equal "[05:00] Start\n[06:30] Middle\n[15:00] End" (buffer-string)))))

(ert-deftest tlon-md-offset-timestamps-seconds-overflow ()
  "Seconds overflow correctly rolls into minutes."
  (with-temp-buffer
    (insert "[00:45] Text")
    (tlon-offset-timestamps "00:30")
    (should (equal "[01:15] Text" (buffer-string)))))

(ert-deftest tlon-md-offset-timestamps-no-timestamps ()
  "Buffer without timestamps is unchanged."
  (with-temp-buffer
    (insert "No timestamps here.")
    (tlon-offset-timestamps "01:00")
    (should (equal "No timestamps here." (buffer-string)))))

(ert-deftest tlon-md-offset-timestamps-invalid-format-errors ()
  "Invalid offset format signals an error."
  (with-temp-buffer
    (insert "[01:00] Text")
    (should-error (tlon-offset-timestamps "bad") :type 'user-error)))

;;;; tlon-looking-at-tag-p

(ert-deftest tlon-looking-at-tag-p-on-tag ()
  "Return non-nil when point is on a matching tag."
  (with-temp-buffer
    (insert "<Aside>content</Aside>")
    (goto-char 1)
    (should (tlon-looking-at-tag-p "Aside"))))

(ert-deftest tlon-looking-at-tag-p-not-on-tag ()
  "Return nil when point is not on a matching tag."
  (with-temp-buffer
    (insert "plain text")
    (goto-char 1)
    (should-not (tlon-looking-at-tag-p "Aside"))))

(provide 'tlon-md-test)
;;; tlon-md-test.el ends here
