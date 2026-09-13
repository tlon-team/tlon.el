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

(ert-deftest tlon-ai-abstract-entrypoints-load-bibliography-dependency ()
  "Abstract commands load bibliography helpers before calling them."
  (dolist (command '(tlon-get-abstract-with-or-without-ai
                     tlon-get-abstract-with-ai))
    (let ((original-require (symbol-function 'require)))
      (cl-letf (((symbol-function 'tlon-bib--should-dispatch-to-batch-p)
                 (lambda (&rest _) (ert-fail "Dependency was not required first")))
                ((symbol-function 'require)
                 (lambda (feature &rest args)
                   (prog1 (apply original-require feature args)
                     (when (eq feature 'tlon-bib)
                       (should (featurep 'tlon-bib))
                       ;; Stop before any network request or bibliography write.
                       (throw 'dependency-loaded t))))))
        (should (catch 'dependency-loaded (funcall command) nil))))))

(provide 'tlon-ai-test)
;;; tlon-ai-test.el ends here
