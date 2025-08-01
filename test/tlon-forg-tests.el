;;; tlon-forg-tests.el --- unit tests for tlon-forg.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; These ERT tests cover a few small, side-effect-free helper functions in
;; tlon-forg.el.  They do not require Forge or any network connectivity and can
;; therefore run quickly in batch mode.

;;; Code:

(require 'ert)

;; ensure we load the updated source files when they are newer than any .elc
(setq load-prefer-newer t)

;; Ensure the main library under test is loaded
(require 'tlon-forg)
(require 'org)

;;;; tlon-forg--normalize-tags

(ert-deftest tlon-forg--normalize-tags/basic ()
  "Whitespace trimming, case folding, duplicate removal and sorting."
  (should (equal (tlon-forg--normalize-tags '("Later" "later" " PendingReview "))
                 '("later" "pendingreview"))))

(ert-deftest tlon-forg--normalize-tags/nil ()
  "Return nil gracefully on nil or empty input."
  (should (equal (tlon-forg--normalize-tags nil) nil))
  (should (equal (tlon-forg--normalize-tags '("")) nil)))

;;;; tlon-forg--valid-tags

(ert-deftest tlon-forg--valid-tags/selection ()
  "Keep only tags that are listed in `tlon-todo-tags'."
  (let ((tlon-todo-tags '("PendingReview" "Later")))
    (should (equal (tlon-forg--valid-tags '("later" "unknown" "PendingReview" "Later"))
                   '("later" "pendingreview")))))

;;;; tlon-forg--org-effort-to-hours

(ert-deftest tlon-forg--org-effort-to-hours/numeric ()
  "Plain numeric effort strings are treated as hours."
  (should (= (tlon-forg--org-effort-to-hours "2") 2.0)))

(ert-deftest tlon-forg--org-effort-to-hours/duration ()
  "Duration strings like \"1h\" are converted to hours."
  ;; org-duration-to-minutes understands \"1h\" → 60
  (should (= (tlon-forg--org-effort-to-hours "1h") 1.0)))

(ert-deftest tlon-forg--org-effort-to-hours/nil ()
  "Nil input yields nil."
  (should (equal (tlon-forg--org-effort-to-hours nil) nil)))

(provide 'tlon-forg-tests)
;;; tlon-forg-tests.el ends here
