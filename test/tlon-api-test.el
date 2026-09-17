;;; tlon-api-test.el --- Tests for tlon-api -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for API route construction, citation URL building and citation JSON
;; retrieval.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tlon-api)

;;;; tlon-api-get-citation-url

(ert-deftest tlon-api-citation-url-text-long ()
  "Citation URL for long text style."
  (let ((url (tlon-api-get-citation-url "smith2023" 'long)))
    (should (stringp url))
    (should (string-match-p "smith2023" url))
    (should (string-match-p "/text$" url))))

(ert-deftest tlon-api-citation-url-text-short ()
  "Citation URL for short text style."
  (let ((url (tlon-api-get-citation-url "smith2023" 'short)))
    (should (string-match-p "/text$" url))))

(ert-deftest tlon-api-citation-url-audio ()
  "Citation URL for audio style."
  (let ((url (tlon-api-get-citation-url "smith2023" 'long-audio)))
    (should (string-match-p "/audio$" url))))

;;;; tlon-api-get-citation-json

(defun tlon-api-test-stub-curl (stdout stderr exit-code)
  "Return a `call-process' stub writing STDOUT and STDERR, exiting EXIT-CODE.
The stub honors the (BUFFER STDERR-FILE) destination form the production code
uses, so the two streams stay separate exactly as they would with real `curl'."
  (lambda (_program _infile destination _display &rest _args)
    (with-current-buffer (car destination)
      (insert stdout))
    (with-temp-file (cadr destination)
      (insert stderr))
    exit-code))

(ert-deftest tlon-api-get-citation-json-ignores-stderr-noise ()
  "Shell-style noise on stderr does not corrupt the parsed JSON."
  (cl-letf (((symbol-function 'call-process)
	     (tlon-api-test-stub-curl
	      "{\"long\": \"Smith (2023)\"}\n"
	      "/Users/someone/.zshrc:12: no such file or directory: /opt/missing\n"
	      0)))
    (should (equal (tlon-api-get-citation-json "http://localhost/api/citations/smith2023/text")
		   '((long . "Smith (2023)"))))))

(ert-deftest tlon-api-get-citation-json-reports-nonzero-exit ()
  "A failing `curl' surfaces its exit status and stderr in a `user-error'."
  (cl-letf (((symbol-function 'call-process)
	     (tlon-api-test-stub-curl "" "curl: (7) Failed to connect to localhost port 80\n" 7)))
    (let ((err (should-error (tlon-api-get-citation-json "http://localhost/x") :type 'user-error)))
      (should (string-match-p "status 7" (cadr err)))
      (should (string-match-p "Failed to connect" (cadr err))))))

(ert-deftest tlon-api-get-citation-json-reports-unresolved-host ()
  "A host resolution failure keeps its dedicated error message."
  (cl-letf (((symbol-function 'call-process)
	     (tlon-api-test-stub-curl "" "curl: (6) Could not resolve host: example.invalid\n" 6)))
    (let ((err (should-error (tlon-api-get-citation-json "http://example.invalid/x") :type 'user-error)))
      (should (string-match-p "could not resolve host" (cadr err))))))

(ert-deftest tlon-api-get-citation-json-rejects-non-json ()
  "A successful request whose body is not JSON signals a `user-error'."
  (cl-letf (((symbol-function 'call-process)
	     (tlon-api-test-stub-curl "<html>oops</html>" "" 0)))
    (should-error (tlon-api-get-citation-json "http://localhost/x") :type 'user-error)))

;;;; tlon-api-get-routes

(ert-deftest tlon-api-get-routes-returns-list ()
  "Routes returns a non-empty list."
  (let ((tlon-translation-language "es"))
    (should (listp (tlon-api-get-routes)))))

(ert-deftest tlon-api-get-routes-substitutes-language ()
  "Routes with %s get the current translation language."
  (let ((tlon-translation-language "fr"))
    (let ((routes (tlon-api-get-routes)))
      (dolist (route routes)
        (when (and (listp route)
                   (stringp (plist-get route :route)))
          (should-not (string-match-p "%s" (plist-get route :route))))))))

(provide 'tlon-api-test)
;;; tlon-api-test.el ends here
