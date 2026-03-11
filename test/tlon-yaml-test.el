;;; tlon-yaml-test.el --- Tests for tlon-yaml -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for YAML parsing functions in tlon-yaml.el.  YAML metadata is
;; used throughout the codebase for article frontmatter, so these
;; parsers must handle edge cases correctly.

;;; Code:

(require 'ert)
(require 'tlon-yaml)

;;;; tlon-yaml-to-alist

(ert-deftest tlon-yaml-to-alist-basic ()
  "Parse simple key-value YAML lines."
  (should (equal '(("title" . "Hello World")
                   ("author" . "Alice"))
                 (tlon-yaml-to-alist '("title: Hello World"
                                       "author: Alice")))))

(ert-deftest tlon-yaml-to-alist-trims-whitespace ()
  "Trim leading/trailing whitespace from keys and values."
  (should (equal '(("title" . "Hello"))
                 (tlon-yaml-to-alist '("  title:   Hello  ")))))

(ert-deftest tlon-yaml-to-alist-preserves-colons-in-value ()
  "Colons inside the value portion are preserved."
  (should (equal '(("url" . "https://example.com"))
                 (tlon-yaml-to-alist '("url: https://example.com")))))

(ert-deftest tlon-yaml-to-alist-empty-input ()
  "Return nil for an empty list of strings."
  (should (null (tlon-yaml-to-alist nil)))
  (should (null (tlon-yaml-to-alist '()))))

(ert-deftest tlon-yaml-to-alist-skips-malformed-lines ()
  "Lines without a colon-space separator are skipped."
  (should (equal '(("good" . "yes"))
                 (tlon-yaml-to-alist '("no-colon-here"
                                       "good: yes")))))

(ert-deftest tlon-yaml-to-alist-list-value ()
  "A bracketed list value is kept as a raw string."
  (let ((result (tlon-yaml-to-alist '("tags: [\"a\", \"b\"]"))))
    (should (equal '(("tags" . "[\"a\", \"b\"]")) result))))

;;;; tlon-yaml-format-value

(ert-deftest tlon-yaml-format-value-plain-string ()
  "A plain string is returned as-is."
  (should (equal "hello" (tlon-yaml-format-value "hello"))))

(ert-deftest tlon-yaml-format-value-quoted-string ()
  "A double-quoted string is unquoted."
  (should (equal "hello world" (tlon-yaml-format-value "\"hello world\""))))

(ert-deftest tlon-yaml-format-value-list ()
  "A bracketed list is parsed into a list of strings."
  (should (equal '("a" "b" "c")
                 (tlon-yaml-format-value "[\"a\", \"b\", \"c\"]"))))

(ert-deftest tlon-yaml-format-value-list-unquoted ()
  "A bracketed list with unquoted items keeps them as strings."
  (should (equal '("a" "b")
                 (tlon-yaml-format-value "[a, b]"))))

(ert-deftest tlon-yaml-format-value-empty-list ()
  "An empty bracketed list parses to a list with one empty string."
  ;; The implementation splits \"\" by comma, yielding (\"\").
  (let ((result (tlon-yaml-format-value "[]")))
    (should (listp result))))

(ert-deftest tlon-yaml-format-value-single-element-list ()
  "A single-element bracketed list returns a one-element list."
  (should (equal '("only")
                 (tlon-yaml-format-value "[\"only\"]"))))

;;;; tlon-yaml-format-values-of-alist

(ert-deftest tlon-yaml-format-values-of-alist-converts-all ()
  "Convert all values in an alist from YAML to Elisp format."
  (let ((input '(("title" . "\"Hello\"")
                 ("tags" . "[\"a\", \"b\"]")
                 ("count" . "42"))))
    (should (equal '(("title" . "Hello")
                     ("tags" "a" "b")
                     ("count" . "42"))
                   (tlon-yaml-format-values-of-alist input)))))

;;;; tlon-yaml-convert-list

(ert-deftest tlon-yaml-convert-list-basic ()
  "Convert an Elisp list to a YAML list string."
  (should (equal "[\"a\", \"b\", \"c\"]"
                 (tlon-yaml-convert-list '("a" "b" "c")))))

(ert-deftest tlon-yaml-convert-list-single ()
  "Convert a single-element list."
  (should (equal "[\"only\"]"
                 (tlon-yaml-convert-list '("only")))))

(ert-deftest tlon-yaml-convert-list-round-trip ()
  "Round-trip: convert to YAML and back should yield the original list."
  (let* ((original '("alpha" "beta" "gamma"))
         (yaml-str (tlon-yaml-convert-list original))
         (parsed (tlon-yaml-format-value yaml-str)))
    (should (equal original parsed))))

;;;; tlon-yaml-sort-fields

(ert-deftest tlon-yaml-sort-fields-basic ()
  "Sort alist fields by key order."
  (let ((fields '(("b" . "2") ("a" . "1") ("c" . "3")))
        (keys '("a" "b" "c")))
    (should (equal '(("a" . "1") ("b" . "2") ("c" . "3"))
                   (tlon-yaml-sort-fields fields keys)))))

(ert-deftest tlon-yaml-sort-fields-subset ()
  "Return only keys present in the keys list, in order."
  (let ((fields '(("title" . "X") ("author" . "A") ("date" . "2024")))
        (keys '("date" "title")))
    (should (equal '(("date" . "2024") ("title" . "X"))
                   (tlon-yaml-sort-fields fields keys)))))

(ert-deftest tlon-yaml-sort-fields-missing-key-no-error ()
  "Silently skip missing keys when NO-ERROR is non-nil."
  (let ((fields '(("a" . "1")))
        (keys '("a" "b")))
    (should (equal '(("a" . "1"))
                   (tlon-yaml-sort-fields fields keys t)))))

(ert-deftest tlon-yaml-sort-fields-missing-key-signals-error ()
  "Signal an error for a missing key when NO-ERROR is nil."
  (let ((fields '(("a" . "1")))
        (keys '("a" "b")))
    (should-error (tlon-yaml-sort-fields fields keys)
                  :type 'user-error)))

;;;; tlon-yaml-read-until-match

(ert-deftest tlon-yaml-read-until-match-basic ()
  "Read lines from a buffer until the delimiter is found."
  (with-temp-buffer
    (insert "title: Hello\nauthor: Alice\n---\nbody text\n")
    (goto-char (point-min))
    (let ((result (tlon-yaml-read-until-match "---")))
      (should (equal '("title: Hello" "author: Alice") result)))))

(ert-deftest tlon-yaml-read-until-match-delimiter-at-start ()
  "Return empty list when delimiter is on the first line."
  (with-temp-buffer
    (insert "---\nstuff\n")
    (goto-char (point-min))
    (should (null (tlon-yaml-read-until-match "---")))))

(ert-deftest tlon-yaml-read-until-match-missing-delimiter ()
  "Signal an error when the delimiter is never found."
  (with-temp-buffer
    (insert "no delimiter here\n")
    (goto-char (point-min))
    (should-error (tlon-yaml-read-until-match "---")
                  :type 'user-error)))

;;;; tlon-yaml-get-metadata (integration, requires buffer with YAML)

(ert-deftest tlon-yaml-get-metadata-from-buffer ()
  "Parse YAML frontmatter from a buffer."
  (with-temp-buffer
    (insert "---\ntitle: Test\nauthor: Me\n---\nBody text\n")
    (let ((metadata (tlon-yaml-get-metadata (current-buffer))))
      (should (equal "Test" (alist-get "title" metadata nil nil #'string=)))
      (should (equal "Me" (alist-get "author" metadata nil nil #'string=))))))

(ert-deftest tlon-yaml-get-metadata-no-frontmatter ()
  "Return nil when there is no YAML frontmatter."
  (with-temp-buffer
    (insert "Just plain text, no YAML.\n")
    (should (null (tlon-yaml-get-metadata (current-buffer))))))

(ert-deftest tlon-yaml-get-metadata-raw-mode ()
  "In raw mode, return a list of raw YAML strings."
  (with-temp-buffer
    (insert "---\ntitle: Test\n---\n")
    (let ((raw (tlon-yaml-get-metadata (current-buffer) t)))
      (should (equal '("title: Test") raw)))))

;;;; tlon-yaml--parse-tags-from-response

(ert-deftest tlon-yaml-parse-tags-json-array ()
  "Parse a raw JSON array response."
  (should (equal '("Alpha" "Beta" "Gamma")
                 (tlon-yaml--parse-tags-from-response "[\"Alpha\", \"Beta\", \"Gamma\"]"))))

(ert-deftest tlon-yaml-parse-tags-fenced-json ()
  "Parse a fenced code block containing a JSON array."
  (should (equal '("Tag A" "Tag B")
                 (tlon-yaml--parse-tags-from-response
                  "```json\n[\"Tag A\", \"Tag B\"]\n```"))))

(ert-deftest tlon-yaml-parse-tags-comma-separated ()
  "Parse a comma-separated plain text response."
  (should (equal '("Alpha" "Beta" "Gamma")
                 (tlon-yaml--parse-tags-from-response "Alpha, Beta, Gamma"))))

(ert-deftest tlon-yaml-parse-tags-newline-separated ()
  "Parse a newline-separated plain text response."
  (should (equal '("Alpha" "Beta")
                 (tlon-yaml--parse-tags-from-response "Alpha\nBeta"))))

(ert-deftest tlon-yaml-parse-tags-empty-response ()
  "Return nil for an empty response."
  (should (null (tlon-yaml--parse-tags-from-response ""))))

(ert-deftest tlon-yaml-parse-tags-nil-response ()
  "Return nil for a nil response."
  (should (null (tlon-yaml--parse-tags-from-response nil))))

(ert-deftest tlon-yaml-parse-tags-quoted-items-without-brackets ()
  "Quoted items without brackets: JSON greedily parses the first item.
Note: this is a limitation — json-read-from-string returns the first
JSON value without erroring, so the fallback splitter is never reached."
  (let ((result (tlon-yaml--parse-tags-from-response "\"Tag A\", \"Tag B\"")))
    ;; JSON parser returns just the first string, not a list
    (should (equal "Tag A" result))))

;;;; tlon-yaml-get-valid-keys

(ert-deftest tlon-yaml-get-valid-keys-article ()
  "Return article keys for type article."
  (let ((keys (tlon-yaml-get-valid-keys nil "article")))
    (should (member "title" keys))
    (should (member "authors" keys))
    (should (member "tags" keys))))

(ert-deftest tlon-yaml-get-valid-keys-tag ()
  "Return tag keys for type tag."
  (let ((keys (tlon-yaml-get-valid-keys nil "tag")))
    (should (member "title" keys))
    (should (member "brief_title" keys))
    (should-not (member "authors" keys))))

(ert-deftest tlon-yaml-get-valid-keys-author ()
  "Return author keys for type author."
  (let ((keys (tlon-yaml-get-valid-keys nil "author")))
    (should (member "title" keys))
    (should-not (member "authors" keys))
    (should-not (member "tags" keys))))

(ert-deftest tlon-yaml-get-valid-keys-unknown-returns-nil ()
  "Return nil for an unknown type."
  (should (null (tlon-yaml-get-valid-keys nil "unknown_type"))))

(ert-deftest tlon-yaml-get-valid-keys-no-core ()
  "Exclude core keys when no-core is non-nil."
  (let ((keys (tlon-yaml-get-valid-keys nil "article" t)))
    (should (member "title" keys))
    (should-not (member "original_path" keys))))

(provide 'tlon-yaml-test)
;;; tlon-yaml-test.el ends here
