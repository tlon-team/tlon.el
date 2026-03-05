;;; tlon-clock-test.el --- Tests for tlon-clock/tlon-core utilities -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the `tlon-next-value' utility used in clock/workflow rotation.

;;; Code:

(require 'ert)
(require 'tlon-core)

;;;; tlon-next-value

(ert-deftest tlon-next-value-basic ()
  "Get the next value in a rotation."
  (let ((alist '((:name "alice") (:name "bob") (:name "carol"))))
    (should (equal "bob" (tlon-next-value :name "alice" alist)))))

(ert-deftest tlon-next-value-last-returns-nil ()
  "Return nil when at the last element."
  (let ((alist '((:name "alice") (:name "bob"))))
    (should (null (tlon-next-value :name "bob" alist)))))

(ert-deftest tlon-next-value-not-found ()
  "Return nil when value not found."
  (let ((alist '((:name "alice"))))
    (should (null (tlon-next-value :name "nobody" alist)))))

(ert-deftest tlon-next-value-skips-duplicates ()
  "Skip consecutive entries with the same value."
  (let ((alist '((:name "alice") (:name "alice") (:name "bob"))))
    (should (equal "bob" (tlon-next-value :name "alice" alist)))))

(provide 'tlon-clock-test)
;;; tlon-clock-test.el ends here
