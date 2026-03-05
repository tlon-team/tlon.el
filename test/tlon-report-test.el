;;; tlon-report-test.el --- Tests for tlon-report -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for time tracking report helpers: time string conversion.

;;; Code:

(require 'ert)
(require 'tlon-report)

;;;; tlon-clock-time-string-to-minutes

(ert-deftest tlon-report-time-to-minutes-basic ()
  "Convert H:MM to minutes."
  (should (equal 90 (tlon-clock-time-string-to-minutes "1:30"))))

(ert-deftest tlon-report-time-to-minutes-zero ()
  "Convert 0:00 to 0."
  (should (equal 0 (tlon-clock-time-string-to-minutes "0:00"))))

(ert-deftest tlon-report-time-to-minutes-large ()
  "Convert large hour values."
  (should (equal 600 (tlon-clock-time-string-to-minutes "10:00"))))

(ert-deftest tlon-report-time-to-minutes-nil ()
  "Return nil for nil input."
  (should (null (tlon-clock-time-string-to-minutes nil))))

(ert-deftest tlon-report-time-to-minutes-invalid ()
  "Return 0 for invalid format."
  (should (equal 0 (tlon-clock-time-string-to-minutes "not a time"))))

;;;; tlon-clock-minutes-to-time-string

(ert-deftest tlon-report-minutes-to-string-basic ()
  "Convert 90 minutes to 1:30."
  (should (equal "1:30" (tlon-clock-minutes-to-time-string 90))))

(ert-deftest tlon-report-minutes-to-string-zero ()
  "Convert 0 minutes to 0:00."
  (should (equal "0:00" (tlon-clock-minutes-to-time-string 0))))

(ert-deftest tlon-report-minutes-to-string-round-trip ()
  "Round-trip: minutes -> string -> minutes."
  (should (equal 150
                 (tlon-clock-time-string-to-minutes
                  (tlon-clock-minutes-to-time-string 150)))))

(provide 'tlon-report-test)
;;; tlon-report-test.el ends here
