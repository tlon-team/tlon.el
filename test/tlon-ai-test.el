;;; tlon-ai-test.el --- Tests for tlon-ai -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for AI helpers: prompt editing, callback handling.

;;; Code:

(require 'ert)
(require 'tlon-ai)
(require 'tlon-bib)
(require 'doi-utils)

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

(ert-deftest tlon-ai-preserving-abstract-skips-existing-without-prompts ()
  "Explicit preservation skips existing abstracts and fetches missing ones."
  (require 'tlon-bib)
  (with-temp-buffer
    (bibtex-mode)
    (insert "@article{Test2026Paper,\n  abstract = {Existing},\n  doi = {10.1/test},\n}\n")
    (goto-char (point-min))
    (let ((tlon-abstract-overwrite 'always)
          (tlon-ai-batch-fun nil)
          fetched)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (ert-fail "Unexpected overwrite prompt")))
                ((symbol-function 'tlon-fetch-abstract-from-crossref)
                 (lambda (_) (setq fetched t) "Fetched"))
                ((symbol-function 'tlon-ai-batch-continue) #'ignore)
                ((symbol-function 'run-with-idle-timer) #'ignore))
        (tlon-get-abstract-with-or-without-ai nil t)
        (should-not fetched)
        (should (equal (bibtex-extras-get-field "abstract") "Existing"))
        (bibtex-extras-set-field "abstract" "")
        (tlon-get-abstract-with-or-without-ai nil t)
        (should fetched)
        (should (equal (bibtex-extras-get-field "abstract") "Fetched."))))))

(ert-deftest tlon-ai-preserving-abstract-rechecks-after-fetch ()
  "A synchronous fetch must preserve an abstract added while it yields."
  (require 'tlon-bib)
  (with-temp-buffer
    (bibtex-mode)
    (insert "@article{Test2026Paper,\n  abstract = {},\n  doi = {10.1/test},\n}\n")
    (goto-char (point-min))
    (let ((tlon-ai-batch-fun nil))
      (cl-letf (((symbol-function 'tlon-fetch-abstract-from-crossref)
                 (lambda (_)
                   (bibtex-extras-set-field "abstract" "Arrived meanwhile")
                   "Fetched")))
        (should (tlon-fetch-and-set-abstract nil t))
        (should (equal (bibtex-extras-get-field "abstract") "Arrived meanwhile"))))))

(ert-deftest tlon-ai-preserving-abstract-late-callback-keeps-existing ()
  "A callback retains preservation after the original invocation returns."
  (require 'tlon-bib)
  (let* ((file (make-temp-file "tlon-abstract-race-" nil ".bib"))
         (db (ebib-db-new-database))
         (ebib--databases (list db))
         (ebib--cur-db db)
         (key "Test2026Paper"))
    (unwind-protect
        (progn
          (with-temp-file file (insert "@article{Test2026Paper,\n  abstract = {},\n  title = {Paper},\n}\n"))
          (ebib-db-set-filename file db)
          (ebib-db-set-entry key '(("=type=" . "article") ("title" . "Paper")) db)
          (let ((buffer (find-file-noselect file)))
            (with-current-buffer buffer
              (bibtex-mode)
              (let ((callback (tlon-get-abstract-callback key nil buffer t)))
                (cl-letf (((symbol-function 'ebib-extras-get-file-of-key) (lambda (_) file))
                          ((symbol-function 'tlon-ai-batch-continue) #'ignore)
                          ((symbol-function 'y-or-n-p)
                           (lambda (&rest _) (ert-fail "Unexpected callback prompt"))))
                  (ebib-db-set-field-value "abstract" "Live database abstract" key db 'overwrite)
                  (funcall callback "Late response" nil)
                  (should (equal (ebib-db-get-field-value "abstract" key db)
                                 "Live database abstract"))
                  (should-not (bibtex-extras-get-field "abstract"))
                  (ebib-db-set-field-value "abstract" nil key db 'overwrite)
                  (bibtex-extras-set-field "abstract" "Unsaved buffer abstract")
                  (funcall callback "Late response" nil)
                  (should (equal (bibtex-extras-get-field "abstract")
                                 "Unsaved buffer abstract")))))))
      (when-let ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest tlon-ai-preserving-fetch-refuses-different-database ()
  "A network yield cannot redirect the result to the same key in another DB."
  (let* ((db (ebib-db-new-database))
         (other (ebib-db-new-database))
         (ebib--cur-db db)
         (tlon-ai-batch-fun nil)
         written)
    (with-temp-buffer
      (setq major-mode 'ebib-entry-mode)
      (cl-letf (((symbol-function 'tlon-bib--field-accessors)
                 (lambda () (list (lambda (_) nil)
                                  (lambda (&rest _) (setq written t)))))
                ((symbol-function 'tlon-abstract-may-proceed-p) (lambda (&rest _) t))
                ((symbol-function 'tlon-get-key-at-point) (lambda () "SameKey"))
                ((symbol-function 'tlon-fetch-abstract-from-crossref)
                 (lambda (_) (setq ebib--cur-db other) "Fetched")))
        (should-error (tlon-fetch-and-set-abstract--single t) :type 'user-error)
        (should-not written)))))

(ert-deftest tlon-ai-manual-fetch-confirms-only-once ()
  "Preservation support must not add another manual overwrite confirmation."
  (with-temp-buffer
    (bibtex-mode)
    (insert "@article{Test2026Paper,\n abstract = {Existing},\n doi = {10.1/test},\n}\n")
    (goto-char (point-min))
    (let ((tlon-abstract-overwrite 'ask)
          (tlon-ai-batch-fun nil)
          (prompts 0))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) (cl-incf prompts) t))
                ((symbol-function 'tlon-fetch-abstract-from-crossref)
                 (lambda (_) "Fetched")))
        (tlon-fetch-and-set-abstract)
        (should (= prompts 1))
        (should (equal (bibtex-extras-get-field "abstract") "Fetched."))))))

(ert-deftest tlon-ai-language-detection-refuses-changed-source ()
  "A delayed language response cannot read a different selected entry."
  (with-temp-buffer
    (let ((key "Original")
          continued)
      (cl-letf (((symbol-function 'tlon-get-key-at-point) (lambda () key))
                ((symbol-function 'tlon-ai-get-abstract-in-language)
                 (lambda (&rest _) (setq continued t))))
        (let ((callback (tlon-ai-get-abstract-from-detected-language nil t)))
          (setq key "Different")
          (should-error (funcall callback "English" nil) :type 'user-error)
          (should-not continued))))))

(provide 'tlon-ai-test)
;;; tlon-ai-test.el ends here
