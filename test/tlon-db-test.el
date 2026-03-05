;;; tlon-db-test.el --- Tests for tlon-db -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for database helpers: name normalization, whitespace collapsing,
;; author string splitting, BibTeX key extraction, diff parsing, and
;; entry text normalization.

;;; Code:

(require 'ert)
;; tlon-db calls citar-extras-refresh-bibliography at load time
;; without requiring the package (relies on elpaca autoloads).
(unless (fboundp 'citar-extras-refresh-bibliography)
  (defun citar-extras-refresh-bibliography (&rest _) nil))
(require 'tlon-db)

;;;; tlon-db--normalize-name

(ert-deftest tlon-db-normalize-name-basic ()
  "Normalize a plain name."
  (should (equal "John Doe" (tlon-db--normalize-name "John Doe"))))

(ert-deftest tlon-db-normalize-name-nbsp ()
  "Convert non-breaking space to regular space."
  (should (equal "John Doe" (tlon-db--normalize-name "John\u00A0Doe"))))

(ert-deftest tlon-db-normalize-name-trims ()
  "Trim leading and trailing whitespace."
  (should (equal "John" (tlon-db--normalize-name "  John  "))))

(ert-deftest tlon-db-normalize-name-collapses-spaces ()
  "Collapse multiple spaces to one."
  (should (equal "John Doe" (tlon-db--normalize-name "John   Doe"))))

;;;; tlon-db--collapse-whitespace

(ert-deftest tlon-db-collapse-whitespace-tabs ()
  "Convert tabs to space and collapse."
  (should (equal "John Doe" (tlon-db--collapse-whitespace "John\t\tDoe"))))

(ert-deftest tlon-db-collapse-whitespace-newlines ()
  "Convert newlines to spaces."
  (should (equal "John Doe" (tlon-db--collapse-whitespace "John\nDoe"))))

(ert-deftest tlon-db-collapse-whitespace-preserves-nbsp-type ()
  "Collapse runs but preserve the leading space type."
  (let ((result (tlon-db--collapse-whitespace "A\u00A0\u00A0B")))
    (should (equal 3 (length result)))
    ;; The middle character should be the original nbsp
    (should (= ?\u00A0 (aref result 1)))))

(ert-deftest tlon-db-collapse-whitespace-mixed ()
  "Collapse mixed tabs, newlines, and spaces."
  (should (equal "a b c" (tlon-db--collapse-whitespace "a\t \n b\t\tc"))))

;;;; tlon-db--split-author-string

(ert-deftest tlon-db-split-author-string-two ()
  "Split two authors."
  (should (equal '("Doe, John" "Roe, Jane")
                 (tlon-db--split-author-string "Doe, John and Roe, Jane"))))

(ert-deftest tlon-db-split-author-string-single ()
  "Single author returned as singleton list."
  (should (equal '("Doe, John")
                 (tlon-db--split-author-string "Doe, John"))))

(ert-deftest tlon-db-split-author-string-three ()
  "Split three authors."
  (should (equal '("Alice" "Bob" "Carol")
                 (tlon-db--split-author-string "Alice and Bob and Carol"))))

(ert-deftest tlon-db-split-author-string-normalizes ()
  "Normalize whitespace in each name."
  (should (equal '("Doe, John" "Roe, Jane")
                 (tlon-db--split-author-string "Doe,  John  and  Roe,  Jane"))))

;;;; tlon-db--author-value-from-entry

(ert-deftest tlon-db-author-value-from-entry-basic ()
  "Extract author from BibTeX entry."
  (should (equal "John Doe"
                 (tlon-db--author-value-from-entry
                  "@article{key, author={John Doe}, title={T}}"))))

(ert-deftest tlon-db-author-value-from-entry-missing ()
  "Return <none> when no author field."
  (should (equal "<none>"
                 (tlon-db--author-value-from-entry
                  "@article{key, title={Test}}"))))

(ert-deftest tlon-db-author-value-from-entry-normalizes ()
  "Normalize whitespace in extracted author."
  (should (equal "John Doe"
                 (tlon-db--author-value-from-entry
                  "@article{key, author={John  Doe}}"))))

;;;; tlon-db--normalize-author-field

(ert-deftest tlon-db-normalize-author-field-collapses ()
  "Collapse newlines in author field."
  (let ((entry "@article{key,\n  author={John\nDoe},\n  title={T}\n}"))
    (should (string-match-p "author={John Doe}" (tlon-db--normalize-author-field entry)))))

(ert-deftest tlon-db-normalize-author-field-no-author ()
  "Return entry unchanged when no author field."
  (let ((entry "@article{key, title={Test}}"))
    (should (equal entry (tlon-db--normalize-author-field entry)))))

;;;; tlon-db--normalize-entry-text

(ert-deftest tlon-db-normalize-entry-text-adds-blank-line ()
  "Add trailing blank line when missing."
  (should (equal "@article{key}\n\n"
                 (tlon-db--normalize-entry-text "@article{key}"))))

(ert-deftest tlon-db-normalize-entry-text-keeps-blank-line ()
  "Keep exactly one blank line when already present."
  (should (equal "@article{key}\n\n"
                 (tlon-db--normalize-entry-text "@article{key}\n\n"))))

(ert-deftest tlon-db-normalize-entry-text-strips-extra ()
  "Strip extra trailing blank lines."
  (should (equal "@article{key}\n\n"
                 (tlon-db--normalize-entry-text "@article{key}\n\n\n\n"))))

;;;; tlon-db--get-key-from-bibtex-line

(ert-deftest tlon-db-get-key-from-bibtex-line-article ()
  "Extract key from @article entry."
  (should (equal "smith2023" (tlon-db--get-key-from-bibtex-line "@article{smith2023,"))))

(ert-deftest tlon-db-get-key-from-bibtex-line-book ()
  "Extract key from @book entry."
  (should (equal "jones2020" (tlon-db--get-key-from-bibtex-line "@book{jones2020,"))))

(ert-deftest tlon-db-get-key-from-bibtex-line-diff-add ()
  "Extract key from diff added line."
  (should (equal "newkey" (tlon-db--get-key-from-bibtex-line "+@article{newkey,"))))

(ert-deftest tlon-db-get-key-from-bibtex-line-diff-del ()
  "Extract key from diff deleted line."
  (should (equal "oldkey" (tlon-db--get-key-from-bibtex-line "-@article{oldkey,"))))

(ert-deftest tlon-db-get-key-from-bibtex-line-not-entry ()
  "Return nil for non-entry lines."
  (should (null (tlon-db--get-key-from-bibtex-line "  title = {Test},"))))

;;;; tlon-db--get-response-body

(ert-deftest tlon-db-get-response-body-basic ()
  "Extract body after blank line."
  (should (equal "Body content"
                 (tlon-db--get-response-body "HTTP/1.1 200 OK\nHeader: value\n\nBody content"))))

(ert-deftest tlon-db-get-response-body-crlf ()
  "Handle CRLF line endings."
  (should (equal "Body"
                 (tlon-db--get-response-body "HTTP/1.1 200\r\nH: v\r\n\r\nBody"))))

(ert-deftest tlon-db-get-response-body-nil ()
  "Return nil for nil input."
  (should (null (tlon-db--get-response-body nil))))

(ert-deftest tlon-db-get-response-body-no-separator ()
  "Return full text when no blank line separator."
  (should (equal "just text" (tlon-db--get-response-body "just text"))))

;;;; tlon-db--current-base-url

(ert-deftest tlon-db-current-base-url-production ()
  "Return production URL when local env disabled."
  (let ((tlon-db-use-local-environment nil))
    (should (stringp (tlon-db--current-base-url)))
    (should-not (string-match-p "local" (tlon-db--current-base-url)))))

(ert-deftest tlon-db-current-base-url-local ()
  "Return local URL when local env enabled."
  (let ((tlon-db-use-local-environment t))
    (should (string-match-p "local" (tlon-db--current-base-url)))))

;;;; tlon-db--files-have-same-content-p

(ert-deftest tlon-db-files-same-content-identical ()
  "Two files with same content."
  (let ((f1 (make-temp-file "tlon-test-"))
        (f2 (make-temp-file "tlon-test-")))
    (unwind-protect
        (progn
          (with-temp-file f1 (insert "same"))
          (with-temp-file f2 (insert "same"))
          (should (tlon-db--files-have-same-content-p f1 f2)))
      (delete-file f1)
      (delete-file f2))))

(ert-deftest tlon-db-files-same-content-different ()
  "Two files with different content."
  (let ((f1 (make-temp-file "tlon-test-"))
        (f2 (make-temp-file "tlon-test-")))
    (unwind-protect
        (progn
          (with-temp-file f1 (insert "foo"))
          (with-temp-file f2 (insert "bar"))
          (should-not (tlon-db--files-have-same-content-p f1 f2)))
      (delete-file f1)
      (delete-file f2))))

(ert-deftest tlon-db-files-same-content-one-missing ()
  "One file exists, one doesn't."
  (let ((f1 (make-temp-file "tlon-test-")))
    (unwind-protect
        (should-not (tlon-db--files-have-same-content-p f1 "/tmp/nonexistent-tlon-test"))
      (delete-file f1))))

;;;; tlon-db--get-changed-keys-from-diff

(ert-deftest tlon-db-changed-keys-added ()
  "Detect newly added entry in unified diff."
  (let* ((diff "@@ -0,0 +1,3 @@\n+@article{newkey,\n+  title={Test}\n+}\n")
         (result (tlon-db--get-changed-keys-from-diff diff)))
    (should (member "newkey" (plist-get result :added)))))

(ert-deftest tlon-db-changed-keys-deleted ()
  "Detect removed entry in unified diff."
  (let* ((diff "@@ -1,3 +0,0 @@\n-@article{oldkey,\n-  title={Test}\n-}\n")
         (result (tlon-db--get-changed-keys-from-diff diff)))
    (should (member "oldkey" (plist-get result :deleted)))))

(ert-deftest tlon-db-changed-keys-modified ()
  "Detect modified entry in unified diff."
  (let* ((diff "@@ -1,3 +1,3 @@\n-@article{modkey,\n-  title={Old}\n-}\n+@article{modkey,\n+  title={New}\n+}\n")
         (result (tlon-db--get-changed-keys-from-diff diff)))
    (should (member "modkey" (plist-get result :modified)))))

(provide 'tlon-db-test)
;;; tlon-db-test.el ends here
