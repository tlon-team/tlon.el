;;; tlon-counterpart-test.el --- Tests for tlon-counterpart -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for relative link translation helpers in tlon-counterpart.el.
;; These functions parse Markdown relative links and extract their
;; components, which is critical for the translation workflow.

;;; Code:

(require 'ert)
(require 'tlon-counterpart)

;;;; tlon-translate-relative-links--extract-file-part-and-anchor

(ert-deftest tlon-counterpart-extract-cur-dir-link ()
  "Extract file part and anchor from a current-directory relative link."
  (let ((result (tlon-translate-relative-links--extract-file-part-and-anchor
                 "./some-file.md"
                 tlon-translate-relative-links--cur-dir-pattern)))
    (should result)
    (should (equal "./some-file.md" (nth 0 result)))
    (should (equal "" (nth 1 result)))))

(ert-deftest tlon-counterpart-extract-cur-dir-link-with-anchor ()
  "Extract file part and anchor from a link with a fragment."
  (let ((result (tlon-translate-relative-links--extract-file-part-and-anchor
                 "./some-file.md#section-1"
                 tlon-translate-relative-links--cur-dir-pattern)))
    (should result)
    (should (equal "./some-file.md" (nth 0 result)))
    (should (equal "#section-1" (nth 1 result)))))

(ert-deftest tlon-counterpart-extract-parent-dir-link ()
  "Extract file part and anchor from a parent-directory relative link."
  (let ((result (tlon-translate-relative-links--extract-file-part-and-anchor
                 "../authors/john-doe.md"
                 tlon-translate-relative-links--parent-dir-pattern)))
    (should result)
    (should (equal "../authors/john-doe.md" (nth 0 result)))
    (should (equal "" (nth 1 result)))))

(ert-deftest tlon-counterpart-extract-parent-dir-link-with-anchor ()
  "Extract file and anchor from a parent-directory link with fragment."
  (let ((result (tlon-translate-relative-links--extract-file-part-and-anchor
                 "../tags/effective-altruism.md#intro"
                 tlon-translate-relative-links--parent-dir-pattern)))
    (should result)
    (should (equal "../tags/effective-altruism.md" (nth 0 result)))
    (should (equal "#intro" (nth 1 result)))))

(ert-deftest tlon-counterpart-extract-non-matching-returns-nil ()
  "Return nil when the URL does not match the pattern."
  (should (null (tlon-translate-relative-links--extract-file-part-and-anchor
                 "https://example.com"
                 tlon-translate-relative-links--cur-dir-pattern)))
  (should (null (tlon-translate-relative-links--extract-file-part-and-anchor
                 "just-a-filename.md"
                 tlon-translate-relative-links--cur-dir-pattern))))

;;;; Pattern constants

(ert-deftest tlon-counterpart-cur-dir-pattern-matches ()
  "The current-dir pattern matches expected link formats."
  (should (string-match-p tlon-translate-relative-links--cur-dir-pattern
                          "./file.md"))
  (should (string-match-p tlon-translate-relative-links--cur-dir-pattern
                          "./file.md#anchor"))
  (should-not (string-match-p tlon-translate-relative-links--cur-dir-pattern
                              "../parent/file.md")))

(ert-deftest tlon-counterpart-parent-dir-pattern-matches ()
  "The parent-dir pattern matches expected link formats."
  (should (string-match-p tlon-translate-relative-links--parent-dir-pattern
                          "../dir/file.md"))
  (should (string-match-p tlon-translate-relative-links--parent-dir-pattern
                          "../dir/file.md#anchor"))
  (should-not (string-match-p tlon-translate-relative-links--parent-dir-pattern
                              "./file.md")))

(provide 'tlon-counterpart-test)
;;; tlon-counterpart-test.el ends here
