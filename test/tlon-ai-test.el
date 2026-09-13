;;; tlon-ai-test.el --- Tests for tlon-ai -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for AI helpers: prompt editing, callback handling.

;;; Code:

(require 'ert)
(require 'tlon-ai)
(require 'tlon-bib)
(require 'doi-utils)

;;;; tlon-ai-maybe-edit-prompt

(defmacro tlon-test-with-abstract-target (&rest body)
  "Run BODY with two databases containing the same key and a captured target."
  (declare (indent 0))
  `(let* ((db (ebib-db-new-database))
          (other (ebib-db-new-database))
          (ebib--databases (list db other))
          (ebib--cur-db db)
          (key "Same2026Key")
          (file (make-temp-file "tlon-target-" nil ".bib"))
          events target)
     (unwind-protect
         (progn
           (ebib-db-set-filename file db)
           (ebib-db-set-entry key (copy-tree '(("=type=" . "article") ("doi" . "10.1/target"))) db)
           (ebib-db-set-entry key (copy-tree '(("=type=" . "article") ("abstract" . "Other database"))) other)
           (setq target (list :key key :db db :entry (ebib-db-get-entry key db)
                              :file "/tmp/captured-source.pdf" :language "english"
                              :callback (lambda (status &optional error)
                                          (push (list status error) events))))
           ,@body)
       (when-let ((buffer (find-buffer-visiting file)))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-file file))))

(ert-deftest tlon-ai-target-fetch-survives-database-switch ()
  "A yielding metadata fetch writes only the captured same-key entry."
  (tlon-test-with-abstract-target
    (cl-letf (((symbol-function 'tlon-fetch-abstract-from-crossref)
               (lambda (_doi) (setq ebib--cur-db other) "Fetched abstract"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Prompt"))))
      (with-temp-buffer (tlon-get-abstract-with-or-without-ai nil t target))
      (should (equal (ebib-unbrace (ebib-db-get-field-value "abstract" key db)) "Fetched abstract."))
      (should (equal (ebib-db-get-field-value "abstract" key other) "Other database"))
      (should (eq (caar events) 'complete))
      (should (= (length events) 1)))))

(ert-deftest tlon-ai-target-abstract-round-trips-through-bibtex-file ()
  "A comma-containing abstract survives Ebib serialization and BibTeX parsing."
  (tlon-test-with-abstract-target
    (let ((abstract "Choice {A}, choice B."))
      (ebib-set-field-value "doi" "10.1/target" key db 'overwrite)
      (cl-letf (((symbol-function 'tlon-fetch-abstract-from-crossref)
                 (lambda (_) abstract)))
        (tlon-get-abstract-with-or-without-ai nil t target))
      (should (eq (caar events) 'complete))
      (ebib-db-set-backup nil db)
      (with-temp-buffer
        (let ((ebib--cur-db db)
              (ebib--buffer-alist (list (cons 'index (current-buffer)))))
          (ebib--save-database db)))
      (with-temp-buffer
        (insert-file-contents file)
        (should (re-search-forward "abstract = {Choice {A}, choice B[.]}" nil t))
        (bibtex-mode)
        (goto-char (point-min))
        (should (bibtex-search-entry key))
        (should (equal (cdr (assoc "abstract" (bibtex-parse-entry t))) abstract))))))

(ert-deftest tlon-ai-target-ai-captures-source-and-preserves-late-abstract ()
  "A delayed AI response retains its target and finishes only once."
  (dolist (late '(nil "Added while waiting"))
    (tlon-test-with-abstract-target
      (let (callback source)
        (cl-letf (((symbol-function 'tlon-fetch-abstract-from-crossref) #'ignore)
                  ((symbol-function 'tlon-fetch-abstract-from-google-books) #'ignore)
                  ((symbol-function 'tlon-fetch-abstract-with-zotra) #'ignore)
                  ((symbol-function 'tlon-get-string-dwim)
                   (lambda (file) (setq source file) "Captured text"))
                  ((symbol-function 'tlon-ai-get-abstract-common)
                   (lambda (_prompt _text _language cb) (setq callback cb)))
                  ((symbol-function 'read-string) (lambda (&rest _) (ert-fail "Prompt"))))
          (with-temp-buffer (tlon-get-abstract-with-or-without-ai nil t target))
          (should (equal source "/tmp/captured-source.pdf"))
          (setq ebib--cur-db other)
          (when late (ebib-db-set-field-value "abstract" late key db 'overwrite))
          (with-temp-buffer (funcall callback "AI abstract" nil))
          (funcall callback "Duplicate delivery" nil)
          (should (equal (ebib-unbrace (ebib-db-get-field-value "abstract" key db)) (or late "AI abstract")))
          (should (equal (ebib-db-get-field-value "abstract" key other) "Other database"))
          (should (eq (caar events) (if late 'preserved 'complete)))
          (should (= (length events) 1)))))))

(ert-deftest tlon-ai-target-rejects-replaced-entry-and-failed-response ()
  "A delayed response never writes a replacement entry or reports twice."
  (dolist (failure '(replaced no-response dirty-buffer dirty-db buffer-abstract))
    (tlon-test-with-abstract-target
      (let (callback)
        (cl-letf (((symbol-function 'tlon-fetch-abstract-from-crossref) #'ignore)
                  ((symbol-function 'tlon-fetch-abstract-from-google-books) #'ignore)
                  ((symbol-function 'tlon-fetch-abstract-with-zotra) #'ignore)
                  ((symbol-function 'tlon-get-string-dwim) (lambda (_) "Source"))
                  ((symbol-function 'tlon-ai-get-abstract-common)
                   (lambda (_prompt _text _language cb) (setq callback cb))))
          (tlon-get-abstract-with-or-without-ai nil t target)
          (pcase failure
            ('replaced (ebib-db-set-entry key '(("=type=" . "article")) db 'overwrite))
            ('dirty-db (ebib-db-set-modified t db))
            ('dirty-buffer (with-current-buffer (find-file-noselect file)
                             (insert "@article{Same2026Key, title={Unsaved}}")))
            ('buffer-abstract (with-current-buffer (find-file-noselect file)
                                (insert "@article{Same2026Key, abstract={User abstract}}"))))
          (funcall callback (unless (eq failure 'no-response) "Late AI") nil)
          (funcall callback "Repeated AI" nil)
          (should-not (ebib-db-get-field-value "abstract" key db 'noerror))
          (should (eq (caar events) (if (eq failure 'buffer-abstract) 'preserved 'failed)))
          (should (= (length events) 1)))))))

(ert-deftest tlon-ai-target-missing-local-key-preserves-database-abstract ()
  "A stale visiting buffer must not hide an abstract in the target database."
  (tlon-test-with-abstract-target
    (ebib-db-set-field-value "abstract" "Unsaved database abstract" key db 'overwrite)
    (ebib-db-set-modified t db)
    (with-current-buffer (find-file-noselect file) (insert "@article{DifferentKey, title={Stale}}"))
    (cl-letf (((symbol-function 'tlon-bib--fetch-abstract)
               (lambda (&rest _) (ert-fail "Existing abstract was ignored"))))
      (tlon-get-abstract-with-or-without-ai nil t target))
    (should (eq (caar events) 'preserved))
    (should (= (length events) 1))
    (should (ebib-db-modified-p db))))

(ert-deftest tlon-ai-target-missing-source-or-prompt-finishes-failed ()
  "Unavailable text or a missing language prompt must not leave work pending."
  (dolist (text '(nil "Source text"))
    (tlon-test-with-abstract-target
      (let ((tlon-ai-get-abstract-prompts nil))
        (cl-letf (((symbol-function 'tlon-bib--fetch-abstract) #'ignore)
                  ((symbol-function 'tlon-get-string-dwim) (lambda (_) text))
                  ((symbol-function 'tlon-make-gptel-request)
                   (lambda (&rest _) (ert-fail "Missing prompt sent a request"))))
          (tlon-get-abstract-with-or-without-ai nil t target))
        (should (eq (caar events) 'failed))
        (should (= (length events) 1))))))

(ert-deftest tlon-ai-target-isolates-ambient-context-during-request-copy ()
  "A captured source excludes ambient context without altering the user's context."
  (tlon-test-with-abstract-target
    (with-temp-buffer
      (setq-local gptel-context '(("unrelated-user-document")))
      (let ((original-context gptel-context)
            (tlon-ai-summarization-model '(nil . test-model))
            copied-context copied-use-context sent-prompt callback)
        (cl-letf (((symbol-function 'tlon-bib--fetch-abstract) #'ignore)
                  ((symbol-function 'tlon-get-string-dwim) (lambda (_) "CAPTURED SOURCE"))
                  ((symbol-function 'tlon--resolve-backend+model) #'identity)
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Prompt")))
                  ((symbol-function 'gptel-request)
                   (lambda (prompt &rest args)
                     (setq sent-prompt prompt callback (plist-get args :callback))
                     (gptel--with-buffer-copy (plist-get args :buffer) nil nil
                       (setq copied-context gptel-context copied-use-context gptel-use-context)
                       (kill-buffer (current-buffer))))))
          (tlon-get-abstract-with-or-without-ai nil t target))
        (should (string-match-p "CAPTURED SOURCE" sent-prompt))
        (should-not copied-context)
        (should-not copied-use-context)
        (should (eq gptel-context original-context))
        (funcall callback "Target abstract" nil)
        (should (eq (caar events) 'complete))))))

(ert-deftest tlon-ai-abstract-no-translator-reaches-ai ()
  "An unsupported metadata page permits the intended AI abstract step."
  (with-temp-buffer
    (bibtex-mode)
    (insert "@book{Test2026Book,\n title = {Book},\n url = {https://example.org/book},\n}\n")
    (goto-char (point-min))
    (let ((tlon-ai-batch-fun nil)
          generated)
      (cl-letf (((symbol-function 'tlon-fetch-abstract-from-crossref) #'ignore)
                ((symbol-function 'tlon-fetch-abstract-from-google-books) #'ignore)
                ((symbol-function 'tlon-fetch-url-from-doi) #'ignore)
                ((symbol-function 'zotra-extras-fetch-field)
                 (lambda (&rest _)
                   (user-error "JSON parse error: No items returned from any translator")))
                ((symbol-function 'tlon-get-abstract-with-ai)
                 (lambda (&rest _) (setq generated t)))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (ert-fail "Unexpected prompt"))))
        (tlon-get-abstract-with-or-without-ai nil t)
        (should generated)))))

(ert-deftest tlon-ai-abstract-zotra-receives-doi ()
  "A DOI-only entry reaches Zotra using its DOI rather than a missing URL."
  (with-temp-buffer
    (bibtex-mode)
    (insert "@book{Test2026Book,\n title = {Book},\n doi = {10.1234/book},\n}\n")
    (goto-char (point-min))
    (let ((tlon-ai-batch-fun nil)
          requested-doi)
      (cl-letf (((symbol-function 'tlon-fetch-abstract-from-crossref) #'ignore)
                ((symbol-function 'tlon-fetch-abstract-from-google-books) #'ignore)
                ((symbol-function 'tlon-fetch-url-from-doi)
                 (lambda (doi) (setq requested-doi doi) "https://example.org/book"))
                ((symbol-function 'zotra-extras-fetch-field)
                 (lambda (&rest _) "Found abstract")))
        (should (tlon-fetch-and-set-abstract nil t))
        (should (equal requested-doi "10.1234/book"))
        (should (equal (bibtex-extras-get-field "abstract") "Found abstract."))))))

(ert-deftest tlon-ai-abstract-zotra-preserves-unrelated-errors ()
  "The no-translator case must not hide authentication or programming errors."
  (dolist (batch '(nil tlon-fetch-and-set-abstract))
    (let ((tlon-ai-batch-fun batch))
      (dolist (failure '((user-error "JSON parse error: Unauthorized")
                         (user-error "Request timed out")
                         (wrong-type-argument stringp nil)))
        (cl-letf (((symbol-function 'zotra-extras-fetch-field)
                   (lambda (_field _url ignore-errors &rest _)
                     (unless ignore-errors (signal (car failure) (cdr failure))))))
          (should-error (tlon-fetch-abstract-with-zotra "https://example.org/book" nil)
                        :type (car failure)))))))

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
                                 "Unsaved buffer abstract"))
                  ;; Empty BibTeX delimiters are not an existing abstract.
                  (bibtex-extras-set-field "abstract" "")
                  (set-buffer-modified-p nil)
                  (ebib-db-set-field-value "abstract" "{}" key db 'overwrite)
                  (cl-letf (((symbol-function 'ebib-extras-reload-database-no-confirm)
                             #'ignore))
                    (funcall callback "First actual abstract" nil))
                  (should (equal (bibtex-extras-get-field "abstract")
                                 "First actual abstract")))))))
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
