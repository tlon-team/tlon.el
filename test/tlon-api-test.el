;;; tlon-api-test.el --- Tests for tlon-api -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for API route construction and citation URL building.

;;; Code:

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
