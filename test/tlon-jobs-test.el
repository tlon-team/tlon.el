;;; tlon-jobs-test.el --- Tests for tlon-jobs -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for job workflow helpers.

;;; Code:

(require 'ert)
(require 'tlon-jobs)

;;;; tlon-jobs-get-action-in-phase

(ert-deftest tlon-jobs-get-action-basic ()
  "Extract action from a phase string."
  (should (equal "Translate" (tlon-jobs-get-action-in-phase "Job: Translate"))))

(ert-deftest tlon-jobs-get-action-single-word ()
  "Signal error for single-word phase."
  (should-error (tlon-jobs-get-action-in-phase "Review") :type 'user-error))

(provide 'tlon-jobs-test)
;;; tlon-jobs-test.el ends here
