;;; tlon-local-test.el --- Tests for tlon-local -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for local repository helpers: RFC3339 formatting, container
;; naming, and log line parsing.

;;; Code:

(require 'ert)
(require 'tlon-local)

;;;; tlon-local--rfc3339

(ert-deftest tlon-local-rfc3339-format ()
  "Format a time as RFC3339 in UTC."
  (let ((time (encode-time 30 45 12 15 3 2024 nil nil 0)))
    (should (equal "2024-03-15T12:45:30Z" (tlon-local--rfc3339 time)))))

(ert-deftest tlon-local-rfc3339-midnight ()
  "Format midnight correctly."
  (let ((time (encode-time 0 0 0 1 1 2024 nil nil 0)))
    (should (equal "2024-01-01T00:00:00Z" (tlon-local--rfc3339 time)))))

;;;; tlon-local--check-pages-container

(ert-deftest tlon-local-check-pages-container-basic ()
  "Generate container name from language code."
  (should (equal "uqbar-es-check-pages-1"
                 (tlon-local--check-pages-container "es"))))

(ert-deftest tlon-local-check-pages-container-en ()
  "Generate container name for English."
  (should (equal "uqbar-en-check-pages-1"
                 (tlon-local--check-pages-container "en"))))

;;;; tlon-local--parse-log-line

(ert-deftest tlon-local-parse-log-line-json ()
  "Parse a JSON log line."
  (let ((result (tlon-local--parse-log-line
                 "{\"message\":\"All good\",\"source_filename\":\"test.md\"}")))
    (should (equal "All good" (car result)))
    (should (equal "test.md" (cdr result)))))

(ert-deftest tlon-local-parse-log-line-json-msg-key ()
  "Parse a JSON log line with 'msg' instead of 'message'."
  (let ((result (tlon-local--parse-log-line "{\"msg\":\"Hello\"}")))
    (should (equal "Hello" (car result)))
    (should (null (cdr result)))))

(ert-deftest tlon-local-parse-log-line-plain ()
  "Parse a plain (non-JSON) log line."
  (let ((result (tlon-local--parse-log-line "just a plain line")))
    (should (equal "just a plain line" (car result)))
    (should (null (cdr result)))))

(ert-deftest tlon-local-parse-log-line-empty ()
  "Parse an empty log line."
  (let ((result (tlon-local--parse-log-line "")))
    (should (equal "" (car result)))
    (should (null (cdr result)))))

(provide 'tlon-local-test)
;;; tlon-local-test.el ends here
