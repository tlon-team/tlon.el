;;; tlon-core-test.el --- Tests for tlon-core -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the core lookup functions and language/path utilities in
;; tlon-core.el.  These are foundational functions used throughout the
;; entire tlon codebase, so correctness here is critical.

;;; Code:

(require 'ert)
(require 'tlon-core)

;;;; tlon-get-value-in-entry

(ert-deftest tlon-get-value-in-entry-from-alist ()
  "Retrieve a value from an alist entry by string key."
  (let ((entry '(("title" . "Hello") ("author" . "Alice"))))
    (should (equal "Hello" (tlon-get-value-in-entry "title" entry)))
    (should (equal "Alice" (tlon-get-value-in-entry "author" entry)))))

(ert-deftest tlon-get-value-in-entry-from-plist ()
  "Retrieve a value from a plist entry by keyword key."
  (let ((entry '(:name "uqbar-es" :language "es" :type content)))
    (should (equal "uqbar-es" (tlon-get-value-in-entry :name entry)))
    (should (equal "es" (tlon-get-value-in-entry :language entry)))
    (should (equal 'content (tlon-get-value-in-entry :type entry)))))

(ert-deftest tlon-get-value-in-entry-missing-key-returns-nil ()
  "Return nil when the key is not present."
  (should (null (tlon-get-value-in-entry "missing" '(("a" . "1")))))
  (should (null (tlon-get-value-in-entry :missing '(:a "1")))))

(ert-deftest tlon-get-value-in-entry-empty-entry ()
  "Return nil for an empty entry."
  (should (null (tlon-get-value-in-entry "x" nil)))
  (should (null (tlon-get-value-in-entry :x nil))))

;;;; tlon-all-pairs-in-entry-p

(ert-deftest tlon-all-pairs-in-entry-p-single-match ()
  "Match a single key-value pair in a plist entry."
  (let ((entry '(:name "spanish" :code "es")))
    (should (tlon-all-pairs-in-entry-p '(:code "es") entry nil))))

(ert-deftest tlon-all-pairs-in-entry-p-multiple-match ()
  "Match multiple key-value pairs simultaneously."
  (let ((entry '(:name "spanish" :code "es" :locale "es_ES")))
    (should (tlon-all-pairs-in-entry-p '(:code "es" :name "spanish") entry nil))))

(ert-deftest tlon-all-pairs-in-entry-p-mismatch ()
  "Return nil when a pair does not match."
  (let ((entry '(:name "spanish" :code "es")))
    (should-not (tlon-all-pairs-in-entry-p '(:code "fr") entry nil))))

(ert-deftest tlon-all-pairs-in-entry-p-partial-mismatch ()
  "Return nil when one of several pairs does not match."
  (let ((entry '(:name "spanish" :code "es")))
    (should-not (tlon-all-pairs-in-entry-p '(:code "es" :name "french") entry nil))))

(ert-deftest tlon-all-pairs-in-entry-p-case-insensitive ()
  "Case-insensitive matching when requested."
  (let ((entry '(:name "Spanish" :code "ES")))
    (should (tlon-all-pairs-in-entry-p '(:name "spanish") entry t))
    (should (tlon-all-pairs-in-entry-p '(:code "es") entry t))))

(ert-deftest tlon-all-pairs-in-entry-p-case-sensitive-rejects ()
  "Case-sensitive matching rejects case differences."
  (let ((entry '(:name "Spanish" :code "ES")))
    (should-not (tlon-all-pairs-in-entry-p '(:name "spanish") entry nil))))

(ert-deftest tlon-all-pairs-in-entry-p-empty-pairs ()
  "Empty pairs list matches any entry (vacuous truth)."
  (should (tlon-all-pairs-in-entry-p nil '(:a 1) nil)))

;;;; tlon-lookup

(ert-deftest tlon-lookup-basic ()
  "Look up a value by matching a single pair."
  (let ((data '((:name "alice" :role "dev")
                (:name "bob" :role "pm"))))
    (should (equal "dev" (tlon-lookup data :role :name "alice")))
    (should (equal "pm" (tlon-lookup data :role :name "bob")))))

(ert-deftest tlon-lookup-returns-first-match ()
  "When multiple entries match, return the first."
  (let ((data '((:lang "es" :greeting "hola")
                (:lang "es" :greeting "buenos días"))))
    (should (equal "hola" (tlon-lookup data :greeting :lang "es")))))

(ert-deftest tlon-lookup-no-match-returns-nil ()
  "Return nil when no entry matches."
  (let ((data '((:name "alice" :role "dev"))))
    (should (null (tlon-lookup data :role :name "nobody")))))

(ert-deftest tlon-lookup-multiple-pairs ()
  "Look up with multiple filter pairs."
  (let ((data '((:name "alice" :dept "eng" :level 3)
                (:name "bob" :dept "eng" :level 5)
                (:name "carol" :dept "pm" :level 3))))
    (should (equal "bob" (tlon-lookup data :name :dept "eng" :level 5)))))

(ert-deftest tlon-lookup-with-alist-entries ()
  "Look up in a list of alist entries using string keys."
  (let ((data '((("title" . "Foo") ("author" . "A"))
                (("title" . "Bar") ("author" . "B")))))
    (should (equal "A" (tlon-lookup data "author" "title" "Foo")))))

;;;; tlon-lookup-all

(ert-deftest tlon-lookup-all-collects-all-matches ()
  "Collect all matching values, preserving order."
  (let ((data '((:lang "es" :name "alice")
                (:lang "en" :name "bob")
                (:lang "es" :name "carol"))))
    (should (equal '("alice" "carol") (tlon-lookup-all data :name :lang "es")))))

(ert-deftest tlon-lookup-all-deduplicates ()
  "Remove duplicate values from results."
  (let ((data '((:lang "es" :tag "a")
                (:lang "es" :tag "a")
                (:lang "es" :tag "b"))))
    (should (equal '("a" "b") (tlon-lookup-all data :tag :lang "es")))))

(ert-deftest tlon-lookup-all-flattens-lists ()
  "Flatten list-valued results into the output."
  (let ((data '((:lang "es" :tags ("a" "b"))
                (:lang "es" :tags ("c")))))
    (should (equal '("a" "b" "c") (tlon-lookup-all data :tags :lang "es")))))

(ert-deftest tlon-lookup-all-no-match ()
  "Return nil when no entries match."
  (let ((data '((:lang "es" :name "alice"))))
    (should (null (tlon-lookup-all data :name :lang "fr")))))

;;;; tlon-lookup-case-insensitive

(ert-deftest tlon-lookup-case-insensitive-matches ()
  "Case-insensitive lookup finds entries regardless of case."
  (let ((data '((:name "Alice" :role "Dev"))))
    (should (equal "Dev" (tlon-lookup-case-insensitive data :role :name "alice")))))

;;;; tlon-validate-language

(ert-deftest tlon-validate-language-valid-name ()
  "Validate a known language name."
  (should (equal "spanish" (tlon-validate-language "spanish")))
  (should (equal "spanish" (tlon-validate-language "Spanish"))))

(ert-deftest tlon-validate-language-valid-code ()
  "Validate a known ISO 639-1 code."
  (should (equal "es" (tlon-validate-language "es" 'code)))
  (should (equal "en" (tlon-validate-language "EN" 'code))))

(ert-deftest tlon-validate-language-invalid ()
  "Return nil for an invalid language."
  (should (null (tlon-validate-language "klingon")))
  (should (null (tlon-validate-language "zz" 'code))))

;;;; tlon-get-iso-code

(ert-deftest tlon-get-iso-code-from-name ()
  "Convert a language name to its two-letter code."
  (should (equal "es" (tlon-get-iso-code "spanish")))
  (should (equal "en" (tlon-get-iso-code "american"))))

(ert-deftest tlon-get-iso-code-passthrough-when-already-code ()
  "Return the input unchanged if it is already a two-letter code."
  (should (equal "es" (tlon-get-iso-code "es")))
  (should (equal "fr" (tlon-get-iso-code "fr"))))

(ert-deftest tlon-get-iso-code-invalid-returns-nil ()
  "Return nil for an unknown language name."
  (should (null (tlon-get-iso-code "klingon"))))

;;;; tlon-get-bare-dir-translation

(ert-deftest tlon-get-bare-dir-translation-es-to-en ()
  "Translate Spanish bare dir to English."
  (should (equal "articles" (tlon-get-bare-dir-translation "en" "es" "articulos")))
  (should (equal "tags" (tlon-get-bare-dir-translation "en" "es" "temas")))
  (should (equal "authors" (tlon-get-bare-dir-translation "en" "es" "autores")))
  (should (equal "collections" (tlon-get-bare-dir-translation "en" "es" "colecciones"))))

(ert-deftest tlon-get-bare-dir-translation-en-to-es ()
  "Translate English bare dir to Spanish."
  (should (equal "articulos" (tlon-get-bare-dir-translation "es" "en" "articles")))
  (should (equal "temas" (tlon-get-bare-dir-translation "es" "en" "tags"))))

(ert-deftest tlon-get-bare-dir-translation-en-to-ja ()
  "Translate English bare dir to Japanese."
  (should (equal "記事" (tlon-get-bare-dir-translation "ja" "en" "articles")))
  (should (equal "タグ" (tlon-get-bare-dir-translation "ja" "en" "tags"))))

(ert-deftest tlon-get-bare-dir-translation-unknown-dir-returns-nil ()
  "Return nil for an unrecognized bare dir."
  (should (null (tlon-get-bare-dir-translation "en" "es" "nonexistent"))))

(ert-deftest tlon-get-bare-dir-translation-unknown-lang-returns-nil ()
  "Return nil for an unrecognized language code."
  (should (null (tlon-get-bare-dir-translation "xx" "en" "articles"))))

;;;; tlon-repo-lookup (integration with real data)

(ert-deftest tlon-repo-lookup-finds-known-repo ()
  "Look up a known repo by name."
  (should (tlon-repo-lookup :name :name "babel-core")))

(ert-deftest tlon-repo-lookup-language-by-name ()
  "Look up a repo's language by its name."
  (let ((lang (tlon-repo-lookup :language :name "uqbar-es")))
    (should (equal "es" lang))))

(ert-deftest tlon-repo-lookup-all-content-repos ()
  "Look up all content repos."
  (let ((names (tlon-repo-lookup-all :name :type 'content)))
    (should (listp names))
    (should (> (length names) 0))))

;;;; tlon-string-to-number

(ert-deftest tlon-string-to-number-english ()
  "Parse English-formatted number with comma thousands separator."
  (should (= 1234567.89
             (tlon-string-to-number "1,234,567.89" "," "."))))

(ert-deftest tlon-string-to-number-german ()
  "Parse German-formatted number with dot thousands and comma decimal."
  (should (= 1234567.89
             (tlon-string-to-number "1.234.567,89" "." ","))))

(ert-deftest tlon-string-to-number-spanish ()
  "Parse Spanish-formatted number with space thousands and comma decimal."
  (should (= 1234.56
             (tlon-string-to-number "1 234,56" " " ","))))

(ert-deftest tlon-string-to-number-no-thousands ()
  "Parse a number without thousands separators."
  (should (= 42.5
             (tlon-string-to-number "42.5" "," "."))))

(ert-deftest tlon-string-to-number-integer ()
  "Parse an integer (no decimal part)."
  (should (= 1000
             (tlon-string-to-number "1,000" "," "."))))

(ert-deftest tlon-string-to-number-plain-integer ()
  "Parse a plain integer with no separators."
  (should (= 42 (tlon-string-to-number "42" "," "."))))

;;;; tlon-concatenate-list

(ert-deftest tlon-concatenate-list-empty ()
  "Empty list returns empty string."
  (should (equal "" (tlon-concatenate-list nil))))

(ert-deftest tlon-concatenate-list-single ()
  "Single-element list returns that element."
  (should (equal "apple" (tlon-concatenate-list '("apple")))))

(ert-deftest tlon-concatenate-list-two-elements ()
  "Two elements are joined with the conjunct."
  (let ((conjuncts '((:language "en" :conjunct "and"))))
    (should (equal "apple and banana"
                   (tlon-concatenate-list '("apple" "banana") conjuncts)))))

(ert-deftest tlon-concatenate-list-three-elements ()
  "Three elements use commas and a conjunct."
  (let ((conjuncts '((:language "en" :conjunct "and"))))
    (should (equal "apple, banana and cherry"
                   (tlon-concatenate-list '("apple" "banana" "cherry") conjuncts)))))

(ert-deftest tlon-concatenate-list-fallback-without-file-context ()
  "Without a file context, falls back to and regardless of conjuncts data."
  ;; tlon-concatenate-list looks up language from the current file, not from
  ;; the conjuncts list, so in batch mode it always falls back to "and".
  (let ((conjuncts '((:language "es" :conjunct "y"))))
    (should (equal "manzana and naranja"
                   (tlon-concatenate-list '("manzana" "naranja") conjuncts)))))

(ert-deftest tlon-concatenate-list-fallback-conjunct ()
  "Falls back to and when language lookup fails."
  (let ((conjuncts '((:language "xx" :conjunct "UND"))))
    ;; No matching language, so falls back to "and"
    (should (equal "a and b"
                   (tlon-concatenate-list '("a" "b") conjuncts)))))

;;;; tlon-get-delimited-region-pos

(ert-deftest tlon-get-delimited-region-pos-symmetric-delimiters ()
  "Find region between two identical delimiters (e.g. YAML front matter)."
  (with-temp-buffer
    (insert "---\ntitle: foo\n---\nbody text\n")
    (let ((pos (tlon-get-delimited-region-pos "---")))
      (should pos)
      (should (= 1 (car pos)))
      (should (= 19 (cdr pos))))))

(ert-deftest tlon-get-delimited-region-pos-exclude-delimiters ()
  "Exclude the delimiters from the returned region."
  (with-temp-buffer
    (insert "---\ntitle: foo\n---\nbody text\n")
    (let ((pos (tlon-get-delimited-region-pos "---" nil t)))
      (should pos)
      ;; begin-pos is match-end of first ---, end-pos is match-beginning of second ---
      (should (= 4 (car pos)))
      (should (= 16 (cdr pos))))))

(ert-deftest tlon-get-delimited-region-pos-different-delimiters ()
  "Find region between two different delimiters."
  (with-temp-buffer
    (insert "BEGIN\ncontent here\nEND\n")
    (let ((pos (tlon-get-delimited-region-pos "BEGIN" "END")))
      (should pos)
      (should (= 1 (car pos)))
      ;; match-end returns position AFTER the match, so END at 20-22 → 23
      (should (= 23 (cdr pos))))))

(ert-deftest tlon-get-delimited-region-pos-no-begin-returns-nil ()
  "Return nil when the begin delimiter is not found."
  (with-temp-buffer
    (insert "nothing here\n")
    (should (null (tlon-get-delimited-region-pos "---")))))

(ert-deftest tlon-get-delimited-region-pos-no-end-returns-nil ()
  "Return nil when only the begin delimiter is found."
  (with-temp-buffer
    (insert "---\ntitle: foo\n")
    (should (null (tlon-get-delimited-region-pos "---")))))

;;;; tlon-read-json

(ert-deftest tlon-read-json-from-buffer ()
  "Parse JSON from a buffer."
  (with-temp-buffer
    (insert "{\"name\": \"Alice\", \"age\": 30}")
    (let ((result (tlon-read-json)))
      (should (equal "Alice" (alist-get "name" result nil nil #'equal)))
      (should (= 30 (alist-get "age" result nil nil #'equal))))))

(ert-deftest tlon-read-json-with-leading-text ()
  "Parse JSON from a buffer with text before the opening brace."
  (with-temp-buffer
    (insert "some header\n{\"key\": \"value\"}")
    (let ((result (tlon-read-json)))
      (should (equal "value" (alist-get "key" result nil nil #'equal))))))

(ert-deftest tlon-read-json-empty-buffer-returns-nil ()
  "Return nil for an empty buffer."
  (with-temp-buffer
    (should (null (tlon-read-json)))))

(ert-deftest tlon-read-json-no-brace-returns-nil ()
  "Return nil when buffer contains no JSON object."
  (with-temp-buffer
    (insert "just plain text, no braces")
    (should (null (tlon-read-json)))))

(ert-deftest tlon-read-json-malformed-returns-nil ()
  "Return nil for malformed JSON."
  (with-temp-buffer
    (insert "{invalid json!!!}")
    (should (null (tlon-read-json)))))

(ert-deftest tlon-read-json-nested-object ()
  "Parse nested JSON objects."
  (with-temp-buffer
    (insert "{\"a\": {\"b\": \"nested\"}}")
    (let* ((result (tlon-read-json))
           (inner (alist-get "a" result nil nil #'equal)))
      (should (equal "nested" (alist-get "b" inner nil nil #'equal))))))

(ert-deftest tlon-read-json-custom-types ()
  "Custom object-type and array-type are respected."
  (with-temp-buffer
    (insert "{\"items\": [1, 2, 3]}")
    (let ((result (tlon-read-json nil 'alist 'vector)))
      (should (vectorp (alist-get "items" result nil nil #'equal))))))

;;;; tlon-set-dir

(ert-deftest tlon-set-dir-sets-directory ()
  "Set :dir from :name and paths-dir-tlon-repos."
  (let ((repo (list :name "test-repo" :abbrev "test-repo"))
        (paths-dir-tlon-repos "/tmp/repos/"))
    (tlon-set-dir repo)
    (should (equal "/tmp/repos/test-repo/" (plist-get repo :dir)))))

(ert-deftest tlon-set-dir-skips-when-already-set ()
  "Do not overwrite :dir when already set."
  (let ((repo (list :name "test-repo" :abbrev "test-repo" :dir "/custom/path/"))
        (paths-dir-tlon-repos "/tmp/repos/"))
    (tlon-set-dir repo)
    (should (equal "/custom/path/" (plist-get repo :dir)))))

;;;; tlon-get-number-separator-pattern

(ert-deftest tlon-get-number-separator-pattern-english ()
  "English pattern matches comma-separated numbers."
  (let ((pat (tlon-get-number-separator-pattern nil "," ".")))
    (should pat)
    (should (string-match-p pat "1,234,567.89"))
    (should (string-match-p pat "42"))
    (should (string-match-p pat "1,000"))))

(ert-deftest tlon-get-number-separator-pattern-german ()
  "German pattern matches dot-separated numbers with comma decimal."
  (let ((pat (tlon-get-number-separator-pattern nil "." ",")))
    (should pat)
    (should (string-match-p pat "1.234.567,89"))))

(ert-deftest tlon-get-number-separator-pattern-bounded ()
  "Bounded pattern adds word boundaries."
  (let ((pat (tlon-get-number-separator-pattern nil "," "." t)))
    (should (string-match-p "\\\\b" pat))))

(ert-deftest tlon-get-number-separator-pattern-by-lang ()
  "Pattern can be derived from a language code."
  (let ((pat (tlon-get-number-separator-pattern "en")))
    (should pat)
    (should (string-match-p pat "1,234.56"))))

(ert-deftest tlon-get-number-separator-pattern-unknown-lang ()
  "Return nil for an unknown language code."
  (should (null (tlon-get-number-separator-pattern "xx"))))

(provide 'tlon-core-test)
;;; tlon-core-test.el ends here
