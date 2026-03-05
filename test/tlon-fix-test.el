;;; tlon-fix-test.el --- Tests for tlon-fix -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for error correction helpers: protected range checking and
;; unbalanced character detection.

;;; Code:

(require 'ert)
(require 'tlon-fix)

;;;; tlon-is-in-protected-range-p

(ert-deftest tlon-fix-protected-range-overlap ()
  "Detect overlap with a protected range."
  (should (tlon-is-in-protected-range-p 5 10 '((3 . 7)))))

(ert-deftest tlon-fix-protected-range-contained ()
  "Detect when range is fully contained in protected range."
  (should (tlon-is-in-protected-range-p 4 6 '((3 . 10)))))

(ert-deftest tlon-fix-protected-range-no-overlap ()
  "Return nil when range does not overlap."
  (should-not (tlon-is-in-protected-range-p 1 3 '((5 . 10)))))

(ert-deftest tlon-fix-protected-range-adjacent ()
  "Adjacent ranges do not overlap."
  (should-not (tlon-is-in-protected-range-p 1 4 '((5 . 10)))))

(ert-deftest tlon-fix-protected-range-empty-list ()
  "No overlap with empty protected ranges."
  (should-not (tlon-is-in-protected-range-p 1 10 nil)))

(ert-deftest tlon-fix-protected-range-multiple ()
  "Check against multiple protected ranges."
  (should (tlon-is-in-protected-range-p 15 20 '((1 . 5) (10 . 25)))))

;;;; tlon--check-unbalanced--pair

(ert-deftest tlon-fix-pair-parens ()
  "Look up parenthesis pair, both from open and close char."
  (should (equal '("(" . ")") (tlon--check-unbalanced--pair "(")))
  (should (equal '("(" . ")") (tlon--check-unbalanced--pair ")"))))

(ert-deftest tlon-fix-pair-brackets ()
  "Look up bracket pair, both from open and close char."
  (should (equal '("[" . "]") (tlon--check-unbalanced--pair "[")))
  (should (equal '("[" . "]") (tlon--check-unbalanced--pair "]"))))

(ert-deftest tlon-fix-pair-guillemets ()
  "Look up guillemet pair."
  (should (equal '("«" . "»") (tlon--check-unbalanced--pair "«"))))

(ert-deftest tlon-fix-pair-self ()
  "Self-pairing character."
  (should (equal '("\"" . "\"") (tlon--check-unbalanced--pair "\""))))

(ert-deftest tlon-fix-pair-unknown ()
  "Unknown character pairs with itself."
  (should (equal '("X" . "X") (tlon--check-unbalanced--pair "X"))))

;;;; tlon--check-unbalanced--scan

(ert-deftest tlon-fix-scan-balanced-parens ()
  "Balanced parentheses return nil."
  (with-temp-buffer
    (insert "(hello (world))")
    (should (null (tlon--check-unbalanced--scan "(" ")")))))

(ert-deftest tlon-fix-scan-missing-close ()
  "Detect missing closing delimiter."
  (with-temp-buffer
    (insert "(hello")
    (let ((result (tlon--check-unbalanced--scan "(" ")")))
      (should result)
      (should (eq 'missing-close (car result))))))

(ert-deftest tlon-fix-scan-missing-open ()
  "Detect missing opening delimiter."
  (with-temp-buffer
    (insert "hello)")
    (let ((result (tlon--check-unbalanced--scan "(" ")")))
      (should result)
      (should (eq 'missing-open (car result))))))

(ert-deftest tlon-fix-scan-symmetric-balanced ()
  "Balanced double quotes return nil."
  (with-temp-buffer
    (insert "say \"hello\" to me")
    (should (null (tlon--check-unbalanced--scan "\"" "\"")))))

(ert-deftest tlon-fix-scan-symmetric-unbalanced ()
  "Detect unbalanced double quotes."
  (with-temp-buffer
    (insert "say \"hello to me")
    (let ((result (tlon--check-unbalanced--scan "\"" "\"")))
      (should result)
      (should (eq 'missing-close (car result))))))

(provide 'tlon-fix-test)
;;; tlon-fix-test.el ends here
