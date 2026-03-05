;;; tlon-ai-test.el --- Tests for tlon-ai -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for AI helpers: prompt editing, callback handling.

;;; Code:

(require 'ert)
(require 'tlon-ai)

;;;; tlon-ai-maybe-edit-prompt

(ert-deftest tlon-ai-maybe-edit-prompt-passthrough ()
  "With editing disabled, return prompt as-is."
  (let ((tlon-ai-edit-prompt nil))
    (should (equal "test prompt" (tlon-ai-maybe-edit-prompt "test prompt")))))

;;;; tlon-ai-callback-return

(ert-deftest tlon-ai-callback-return-success ()
  "Return response when non-nil."
  (should (equal "result" (tlon-ai-callback-return "result" nil))))

(ert-deftest tlon-ai-callback-return-nil ()
  "Return a failure message for nil response."
  ;; callback-fail returns a status message string, not nil
  (let ((result (tlon-ai-callback-return nil '(:status "error"))))
    (should (stringp result))))

(provide 'tlon-ai-test)
;;; tlon-ai-test.el ends here
