;;; tlon-count-test.el --- Tests for tlon-count -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for word/key counting utilities: sorting and merging hash tables.

;;; Code:

(require 'ert)
(require 'tlon-count)

;;;; tlon-sort-key-counts

(ert-deftest tlon-count-sort-key-counts-descending ()
  "Sort by count descending."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "a" 1 ht)
    (puthash "b" 3 ht)
    (puthash "c" 2 ht)
    (let ((result (tlon-sort-key-counts ht)))
      (should (equal '(("b" . 3) ("c" . 2) ("a" . 1)) result)))))

(ert-deftest tlon-count-sort-key-counts-empty ()
  "Empty hash table returns empty list."
  (let ((ht (make-hash-table :test 'equal)))
    (should (null (tlon-sort-key-counts ht)))))

;;;; tlon-merge-key-counts

(ert-deftest tlon-count-merge-key-counts-basic ()
  "Merge two hash tables by summing counts."
  (let ((ht1 (make-hash-table :test 'equal))
        (ht2 (make-hash-table :test 'equal)))
    (puthash "a" 1 ht1)
    (puthash "a" 2 ht2)
    (puthash "b" 3 ht2)
    (tlon-merge-key-counts ht1 ht2)
    (should (equal 3 (gethash "a" ht1)))
    (should (equal 3 (gethash "b" ht1)))))

(ert-deftest tlon-count-merge-key-counts-disjoint ()
  "Merge disjoint hash tables."
  (let ((ht1 (make-hash-table :test 'equal))
        (ht2 (make-hash-table :test 'equal)))
    (puthash "a" 1 ht1)
    (puthash "b" 2 ht2)
    (tlon-merge-key-counts ht1 ht2)
    (should (equal 1 (gethash "a" ht1)))
    (should (equal 2 (gethash "b" ht1)))))

(provide 'tlon-count-test)
;;; tlon-count-test.el ends here
