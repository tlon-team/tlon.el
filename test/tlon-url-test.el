;;; tlon-url-test.el --- Tests for tlon-url -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for URL normalization, validation, and utility functions in
;; tlon-url.el.  URL handling is critical for link checking and
;; archiving workflows.

;;; Code:

(require 'ert)
(require 'tlon-url)

;;;; tlon--normalize-wayback-url

(ert-deftest tlon-normalize-wayback-url-removes-nested-prefix ()
  "Remove a nested Wayback Machine prefix, keeping the outermost."
  (should (equal "https://web.archive.org/web/2024/https://example.com"
                 (tlon--normalize-wayback-url
                  "https://web.archive.org/web/2024/https://web.archive.org/web/2023/https://example.com"))))

(ert-deftest tlon-normalize-wayback-url-removes-trailing-double-slashes ()
  "Collapse multiple trailing slashes to a single one."
  (should (equal "https://web.archive.org/web/2024/https://example.com/"
                 (tlon--normalize-wayback-url
                  "https://web.archive.org/web/2024/https://example.com//"))))

(ert-deftest tlon-normalize-wayback-url-no-change-needed ()
  "Return the URL unchanged when already normalized."
  (let ((url "https://web.archive.org/web/2024/https://example.com"))
    (should (equal url (tlon--normalize-wayback-url url)))))

(ert-deftest tlon-normalize-wayback-url-nil-input ()
  "Return nil for nil input."
  (should (null (tlon--normalize-wayback-url nil))))

(ert-deftest tlon-normalize-wayback-url-http-variant ()
  "Handle http:// Wayback URLs too."
  (should (equal "http://web.archive.org/web/2024/http://example.com"
                 (tlon--normalize-wayback-url
                  "http://web.archive.org/web/2024/http://web.archive.org/web/2023/http://example.com"))))

;;;; tlon-lychee--is-wayback-url-p

(ert-deftest tlon-is-wayback-url-https ()
  "Recognize an https Wayback URL."
  (should (tlon-lychee--is-wayback-url-p "https://web.archive.org/web/2024/foo")))

(ert-deftest tlon-is-wayback-url-http ()
  "Recognize an http Wayback URL."
  (should (tlon-lychee--is-wayback-url-p "http://web.archive.org/web/2024/foo")))

(ert-deftest tlon-is-wayback-url-archive-org-without-web ()
  "Recognize archive.org URLs without the web. subdomain."
  (should (tlon-lychee--is-wayback-url-p "https://archive.org/details/something")))

(ert-deftest tlon-is-wayback-url-rejects-non-archive ()
  "Reject a non-archive URL."
  (should-not (tlon-lychee--is-wayback-url-p "https://example.com"))
  (should-not (tlon-lychee--is-wayback-url-p "https://notarchive.org/web/foo")))

;;;; tlon--url-domain-root-core

(ert-deftest tlon-url-domain-root-core-basic ()
  "Generate a regex matching http/https, optional www, and optional trailing /."
  (let ((re (tlon--url-domain-root-core "https://example.com")))
    (should (string-match-p re "https://example.com"))
    (should (string-match-p re "http://example.com"))
    (should (string-match-p re "https://www.example.com"))
    (should (string-match-p re "https://example.com/"))))

(ert-deftest tlon-url-domain-root-core-rejects-different-domain ()
  "The regex should not match a different domain."
  (let ((re (tlon--url-domain-root-core "https://example.com")))
    (should-not (string-match-p re "https://other.com"))))

(ert-deftest tlon-url-domain-root-core-with-trailing-slash ()
  "Accept a URL that has a trailing slash."
  (let ((re (tlon--url-domain-root-core "https://example.com/")))
    (should (string-match-p re "https://example.com"))
    (should (string-match-p re "https://example.com/"))))

(ert-deftest tlon-url-domain-root-core-rejects-path ()
  "Signal an error when the URL contains a path beyond /."
  (should-error (tlon--url-domain-root-core "https://example.com/path/to/page")))

(ert-deftest tlon-url-domain-root-core-rejects-empty-host ()
  "Signal an error for a URL with no parseable host."
  (should-error (tlon--url-domain-root-core "not-a-url")))

;;;; tlon--zip-url-lists

(ert-deftest tlon-zip-url-lists-equal-length ()
  "Zip two lists of equal length."
  (should (equal '(("a" . "1") ("b" . "2"))
                 (tlon--zip-url-lists '("a" "b") '("1" "2")))))

(ert-deftest tlon-zip-url-lists-first-shorter ()
  "Stop at the shorter list (first is shorter)."
  (should (equal '(("a" . "1"))
                 (tlon--zip-url-lists '("a") '("1" "2" "3")))))

(ert-deftest tlon-zip-url-lists-second-shorter ()
  "Stop at the shorter list (second is shorter)."
  (should (equal '(("a" . "1"))
                 (tlon--zip-url-lists '("a" "b" "c") '("1")))))

(ert-deftest tlon-zip-url-lists-empty ()
  "Return nil when either list is empty."
  (should (null (tlon--zip-url-lists nil '("1"))))
  (should (null (tlon--zip-url-lists '("a") nil)))
  (should (null (tlon--zip-url-lists nil nil))))

;;;; tlon--extract-urls-from-string

(ert-deftest tlon-extract-urls-single ()
  "Extract a single URL from text."
  (let ((result (tlon--extract-urls-from-string "Visit https://example.com for info.")))
    (should (member "https://example.com" result))))

(ert-deftest tlon-extract-urls-multiple ()
  "Extract multiple URLs from text."
  (let ((result (tlon--extract-urls-from-string
                 "See https://a.com and http://b.com for details.")))
    (should (>= (length result) 2))))

(ert-deftest tlon-extract-urls-deduplicates ()
  "Duplicate URLs are removed."
  (let ((result (tlon--extract-urls-from-string
                 "https://example.com https://example.com")))
    (should (= 1 (length result)))))

(ert-deftest tlon-extract-urls-no-urls ()
  "Return nil when there are no URLs."
  (should (null (tlon--extract-urls-from-string "No URLs here."))))

(ert-deftest tlon-extract-urls-nil-input ()
  "Return nil for nil input."
  (should (null (tlon--extract-urls-from-string nil))))

(provide 'tlon-url-test)
;;; tlon-url-test.el ends here
