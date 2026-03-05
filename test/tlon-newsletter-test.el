;;; tlon-newsletter-test.el --- Tests for tlon-newsletter -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for newsletter date handling: validation, month extraction,
;; content year calculation, and previous month lookup.

;;; Code:

(require 'ert)
(require 'tlon-newsletter)

;;;; tlon-newsletter--validate-date-format

(ert-deftest tlon-newsletter-validate-date-valid ()
  "Accept a valid yyyy-mm date."
  (let ((result (tlon-newsletter--validate-date-format "2025-03")))
    (should (equal 2025 (car result)))
    (should (equal 3 (cdr result)))))

(ert-deftest tlon-newsletter-validate-date-december ()
  "Accept December."
  (let ((result (tlon-newsletter--validate-date-format "2024-12")))
    (should (equal 12 (cdr result)))))

(ert-deftest tlon-newsletter-validate-date-invalid-format ()
  "Reject non-yyyy-mm strings."
  (should-error (tlon-newsletter--validate-date-format "2025-3"))
  (should-error (tlon-newsletter--validate-date-format "not-a-date")))

(ert-deftest tlon-newsletter-validate-date-invalid-month ()
  "Reject month 13."
  (should-error (tlon-newsletter--validate-date-format "2025-13")))

(ert-deftest tlon-newsletter-validate-date-month-zero ()
  "Reject month 00."
  (should-error (tlon-newsletter--validate-date-format "2025-00")))

;;;; tlon-newsletter-get-content-year

(ert-deftest tlon-newsletter-content-year-same-year ()
  "Content year for March is same year."
  (should (equal 2025 (tlon-newsletter-get-content-year "2025-03"))))

(ert-deftest tlon-newsletter-content-year-january ()
  "Content year for January is previous year (Dec content)."
  (should (equal 2024 (tlon-newsletter-get-content-year "2025-01"))))

;;;; tlon-newsletter-get-previous-month

(ert-deftest tlon-newsletter-previous-month-basic ()
  "Previous month of marzo is febrero."
  (should (equal "febrero" (tlon-newsletter-get-previous-month "marzo"))))

(ert-deftest tlon-newsletter-previous-month-january ()
  "Previous month of enero is diciembre."
  (should (equal "diciembre" (tlon-newsletter-get-previous-month "enero"))))

(ert-deftest tlon-newsletter-previous-month-case-insensitive ()
  "Case-insensitive matching."
  (should (equal "febrero" (tlon-newsletter-get-previous-month "Marzo"))))

;;;; tlon-newsletter--next-year-month

(ert-deftest tlon-newsletter-next-year-month-mid-year ()
  "Next month from March is April of same year."
  (let ((time (encode-time 0 0 0 1 3 2025)))
    (should (equal '(2025 . 4) (tlon-newsletter--next-year-month time)))))

(ert-deftest tlon-newsletter-next-year-month-december ()
  "Next month from December is January of next year."
  (let ((time (encode-time 0 0 0 1 12 2024)))
    (should (equal '(2025 . 1) (tlon-newsletter--next-year-month time)))))

(provide 'tlon-newsletter-test)
;;; tlon-newsletter-test.el ends here
