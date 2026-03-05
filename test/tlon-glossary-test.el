;;; tlon-glossary-test.el --- Tests for tlon-glossary -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for glossary management: entry creation, lookup, update, term
;; normalization, CSV escaping, and AI translation pipeline helpers.

;;; Code:

(require 'ert)
(require 'tlon-glossary)

;;;; tlon-get-terms

(ert-deftest tlon-get-terms-basic ()
  "Extract terms for a given language."
  (let ((glossary '(((en . "hello") (es . "hola"))
                    ((en . "world") (es . "mundo")))))
    (should (equal '("hello" "world") (tlon-get-terms glossary "en")))
    (should (equal '("hola" "mundo") (tlon-get-terms glossary "es")))))

;;;; tlon-find-entry-by-term-in-lang

(ert-deftest tlon-find-entry-by-term-basic ()
  "Find an entry by term in a specific language."
  (let ((glossary '(((en . "hello") (es . "hola"))
                    ((en . "world") (es . "mundo")))))
    (should (equal '((en . "hello") (es . "hola"))
                   (tlon-find-entry-by-term-in-lang glossary "hello" "en")))))

(ert-deftest tlon-find-entry-by-term-not-found ()
  "Return nil when term not found."
  (let ((glossary '(((en . "hello")))))
    (should (null (tlon-find-entry-by-term-in-lang glossary "missing" "en")))))

;;;; tlon-create-entry

(ert-deftest tlon-create-entry-variable ()
  "Create a variable-type entry."
  (let ((entry (tlon-create-entry "hello" "variable" "en")))
    (should (equal "hello" (alist-get 'en entry)))
    (should (equal "variable" (alist-get 'type entry)))))

(ert-deftest tlon-create-entry-invariant ()
  "Create an invariant-type entry, which copies the term to all target languages."
  (let ((tlon-project-target-languages '("es" "fr")))
    (let ((entry (tlon-create-entry "DNA" "invariant" "en")))
      (should (equal "DNA" (alist-get 'en entry)))
      (should (equal "DNA" (alist-get 'es entry)))
      (should (equal "DNA" (alist-get 'fr entry)))
      (should (equal "invariant" (alist-get 'type entry))))))

;;;; tlon-update-glossary

(ert-deftest tlon-update-glossary-replaces-existing ()
  "Replace an existing entry."
  (let* ((glossary '(((en . "hello") (es . "hola"))))
         (new-entry '((en . "hello") (es . "hola!") (type . "variable")))
         (result (tlon-update-glossary glossary new-entry "hello" "en")))
    (should (= 1 (length result)))
    (should (equal "hola!" (alist-get 'es (car result))))))

(ert-deftest tlon-update-glossary-appends-new ()
  "Append when term not found."
  (let* ((glossary '(((en . "hello"))))
         (new-entry '((en . "world")))
         (result (tlon-update-glossary glossary new-entry "world" "en")))
    (should (= 2 (length result)))))

;;;; tlon--glossary-normalize-term

(ert-deftest tlon-glossary-normalize-term-basic ()
  "Trim whitespace."
  (should (equal "hello" (tlon--glossary-normalize-term "  hello  "))))

(ert-deftest tlon-glossary-normalize-term-empty ()
  "Return nil for empty string."
  (should (null (tlon--glossary-normalize-term "")))
  (should (null (tlon--glossary-normalize-term "   "))))

(ert-deftest tlon-glossary-normalize-term-nil ()
  "Return nil for nil."
  (should (null (tlon--glossary-normalize-term nil))))

;;;; tlon--glossary-escape

(ert-deftest tlon-glossary-escape-quotes ()
  "Escape double quotes for CSV."
  (should (equal "say \"\"hello\"\"" (tlon--glossary-escape "say \"hello\""))))

(ert-deftest tlon-glossary-escape-no-quotes ()
  "No change when no quotes."
  (should (equal "plain" (tlon--glossary-escape "plain"))))

;;;; tlon--glossary-make-csv-row

(ert-deftest tlon-glossary-make-csv-row-basic ()
  "Format fields as a quoted CSV row."
  (should (equal "\"hello\",\"world\"\n"
                 (tlon--glossary-make-csv-row '("hello" "world")))))

(ert-deftest tlon-glossary-make-csv-row-with-quotes ()
  "Escape quotes inside CSV fields."
  (should (equal "\"say \"\"hi\"\"\"\n"
                 (tlon--glossary-make-csv-row '("say \"hi\"")))))

;;;; tlon--glossary-terms-present

(ert-deftest tlon-glossary-terms-present-basic ()
  "Find terms present in text."
  (should (equal '("hello")
                 (tlon--glossary-terms-present "I said hello to you" '("hello" "goodbye")))))

(ert-deftest tlon-glossary-terms-present-case-insensitive ()
  "Match is case-insensitive."
  (should (equal '("hello")
                 (tlon--glossary-terms-present "I said HELLO" '("hello")))))

(ert-deftest tlon-glossary-terms-present-word-boundary ()
  "Respect word boundaries."
  (should (null (tlon--glossary-terms-present "caterpillar" '("cat")))))

(ert-deftest tlon-glossary-terms-present-none ()
  "Return nil when no terms match."
  (should (null (tlon--glossary-terms-present "nothing here" '("apple" "banana")))))

;;;; tlon-ai--clean-verified-response

(ert-deftest tlon-ai-clean-verified-response-strips-fences ()
  "Remove markdown code fences."
  (should (equal "hello\nworld"
                 (tlon-ai--clean-verified-response "```\nhello\nworld\n```"))))

(ert-deftest tlon-ai-clean-verified-response-trims ()
  "Trim leading/trailing whitespace."
  (should (equal "hello"
                 (tlon-ai--clean-verified-response "  hello  "))))

;;;; tlon-ai--validate-translation-list

(ert-deftest tlon-ai-validate-translation-list-matching ()
  "Return full list when counts match."
  (should (equal '("a" "b")
                 (tlon-ai--validate-translation-list '("a" "b") '("x" "y")))))

(ert-deftest tlon-ai-validate-translation-list-truncates ()
  "Truncate to the shorter list."
  (should (equal '("a")
                 (tlon-ai--validate-translation-list '("a" "b" "c") '("x")))))

;;;; tlon-ai--create-translation-pairs

(ert-deftest tlon-ai-create-translation-pairs-basic ()
  "Create pairs from translations and terms."
  (should (equal '(("cat" . "gato") ("dog" . "perro"))
                 (tlon-ai--create-translation-pairs
                  '("gato" "perro") '("cat" "dog")))))

(ert-deftest tlon-ai-create-translation-pairs-filters-unavailable ()
  "Filter out [TRANSLATION_UNAVAILABLE]."
  (should (equal '(("cat" . "gato"))
                 (tlon-ai--create-translation-pairs
                  '("gato" "[TRANSLATION_UNAVAILABLE]") '("cat" "dog")))))

;;;; tlon--glossary-detect-languages

(ert-deftest tlon-glossary-detect-languages-basic ()
  "Detect languages from JSON glossary entries."
  (let ((tlon-project-target-languages '("es" "fr"))
        (json '(((en . "hello") (es . "hola")))))
    (let ((result (tlon--glossary-detect-languages json)))
      (should (memq 'en result))
      (should (memq 'es result)))))

(ert-deftest tlon-glossary-detect-languages-excludes-type ()
  "Exclude 'type' key from detected languages."
  (let ((tlon-project-target-languages '("es"))
        (json '(((en . "hello") (es . "hola") (type . "variable")))))
    (let ((result (tlon--glossary-detect-languages json)))
      (should-not (memq 'type result)))))

(ert-deftest tlon-glossary-detect-languages-en-first ()
  "English appears first in result."
  (let ((tlon-project-target-languages '("es" "fr"))
        (json '(((en . "a") (fr . "b") (es . "c")))))
    (let ((result (tlon--glossary-detect-languages json)))
      (should (eq 'en (car result))))))

(provide 'tlon-glossary-test)
;;; tlon-glossary-test.el ends here
