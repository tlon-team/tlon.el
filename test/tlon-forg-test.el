;;; tlon-forg-test.el --- Tests for tlon-forg -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for Forge/GitHub integration helpers: tag normalization,
;; effort conversion, diff reporting, and pandoc-based format conversion.

;;; Code:

(require 'ert)
(require 'tlon-forg)

;;;; tlon-forg--normalize-tags

(ert-deftest tlon-forg-normalize-tags-basic ()
  "Normalize a list of tags to lowercase sorted unique strings."
  (should (equal '("alpha" "beta")
                 (tlon-forg--normalize-tags '("Beta" "Alpha")))))

(ert-deftest tlon-forg-normalize-tags-deduplicates ()
  "Remove duplicate tags."
  (should (equal '("alpha")
                 (tlon-forg--normalize-tags '("Alpha" "alpha" "ALPHA")))))

(ert-deftest tlon-forg-normalize-tags-trims ()
  "Trim whitespace from tags."
  (should (equal '("alpha" "beta")
                 (tlon-forg--normalize-tags '("  alpha  " "beta ")))))

(ert-deftest tlon-forg-normalize-tags-filters-empty ()
  "Remove empty strings from the result."
  (should (equal '("alpha")
                 (tlon-forg--normalize-tags '("alpha" "" "  ")))))

(ert-deftest tlon-forg-normalize-tags-nil ()
  "Return nil for nil input."
  (should (null (tlon-forg--normalize-tags nil))))

(ert-deftest tlon-forg-normalize-tags-all-empty ()
  "Return nil when all tags are empty."
  (should (null (tlon-forg--normalize-tags '("" " ")))))

;;;; tlon-forg--valid-tags

(ert-deftest tlon-forg-valid-tags-filters ()
  "Keep only tags listed in `tlon-todo-tags'."
  (let ((tlon-todo-tags '("bug" "feature" "docs")))
    (should (equal '("bug" "feature")
                   (tlon-forg--valid-tags '("Bug" "Feature" "random"))))))

(ert-deftest tlon-forg-valid-tags-empty ()
  "Return nil when no tags match."
  (let ((tlon-todo-tags '("bug")))
    (should (null (tlon-forg--valid-tags '("unrelated"))))))

;;;; tlon-forg--report-diff

(ert-deftest tlon-forg-report-diff-basic ()
  "Format a diff list as a comma-separated string."
  (should (equal "title, tags"
                 (tlon-forg--report-diff '(title tags)))))

(ert-deftest tlon-forg-report-diff-single ()
  "Format a single-element diff."
  (should (equal "title"
                 (tlon-forg--report-diff '(title)))))

;;;; tlon-forg--org-effort-to-hours

(ert-deftest tlon-forg-effort-to-hours-plain-number ()
  "A plain number is interpreted as hours."
  (should (= 2.5 (tlon-forg--org-effort-to-hours "2.5"))))

(ert-deftest tlon-forg-effort-to-hours-duration ()
  "An Org duration string is converted to hours."
  (should (= 1.5 (tlon-forg--org-effort-to-hours "1:30"))))

(ert-deftest tlon-forg-effort-to-hours-nil ()
  "Return nil for nil input."
  (should (null (tlon-forg--org-effort-to-hours nil))))

(ert-deftest tlon-forg-effort-to-hours-trims ()
  "Trim whitespace before parsing."
  (should (= 3.0 (tlon-forg--org-effort-to-hours "  3  "))))

;;;; tlon-forg--prompt-element-diff

(ert-deftest tlon-forg-prompt-element-diff-issue-mode ()
  "When `tlon-forg-when-syncing' is 'issue, return ?i."
  (let ((tlon-forg-when-syncing 'issue))
    (should (equal ?i (tlon-forg--prompt-element-diff "Title" "A" "B" "ctx")))))

(ert-deftest tlon-forg-prompt-element-diff-todo-mode ()
  "When `tlon-forg-when-syncing' is 'todo, return ?t."
  (let ((tlon-forg-when-syncing 'todo))
    (should (equal ?t (tlon-forg--prompt-element-diff "Title" "A" "B" "ctx")))))

;;;; Pandoc conversion (requires pandoc)

(ert-deftest tlon-forg-md-to-org-basic ()
  "Convert simple Markdown to Org format."
  (when (executable-find "pandoc")
    (should (equal "hello" (tlon-forg-md->org "hello")))))

(ert-deftest tlon-forg-md-to-org-bold ()
  "Convert Markdown bold to Org bold."
  (when (executable-find "pandoc")
    (let ((result (tlon-forg-md->org "**bold**")))
      (should (string-match-p "\\*bold\\*" result)))))

(ert-deftest tlon-forg-org-to-md-empty ()
  "Convert empty string."
  (should (equal "" (tlon-forg-org->md ""))))

(ert-deftest tlon-forg-org-to-md-nil ()
  "Convert nil returns empty string."
  (should (equal "" (tlon-forg-org->md nil))))

(provide 'tlon-forg-test)
;;; tlon-forg-test.el ends here
