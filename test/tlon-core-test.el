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

(provide 'tlon-core-test)
;;; tlon-core-test.el ends here
