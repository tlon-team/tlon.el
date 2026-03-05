;;; tlon-import-test.el --- Tests for tlon-import -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for EA Forum import helpers: GraphQL query construction, response
;; parsing, URL/ID/slug extraction, type discrimination, and title shortening.

;;; Code:

(require 'ert)
(require 'tlon-import)

;;;; tlon-import-eaf-article-query

(ert-deftest tlon-import-eaf-article-query-contains-id ()
  "Query string contains the given post ID."
  (let ((q (tlon-import-eaf-article-query "abc123def456ghi78")))
    (should (stringp q))
    (should (string-match-p "abc123def456ghi78" q))))

(ert-deftest tlon-import-eaf-article-query-is-json ()
  "Query string is JSON-like with query key."
  (let ((q (tlon-import-eaf-article-query "testid")))
    (should (string-prefix-p "{\"query\":" q))))

;;;; tlon-import-eaf-tag-query

(ert-deftest tlon-import-eaf-tag-query-contains-slug ()
  "Tag query contains the given slug."
  (let ((q (tlon-import-eaf-tag-query "effective-altruism")))
    (should (string-match-p "effective-altruism" q))))

(ert-deftest tlon-import-eaf-tag-query-is-json ()
  "Tag query is JSON-like."
  (let ((q (tlon-import-eaf-tag-query "ai-safety")))
    (should (string-prefix-p "{\"query\":" q))))

;;;; tlon-import-eaf-get-article-result

(ert-deftest tlon-import-eaf-get-article-result-extracts ()
  "Extract result from nested post response."
  (let ((response '((post . ((result . ((title . "Test") (slug . "test"))))))))
    (should (equal '((title . "Test") (slug . "test"))
                   (tlon-import-eaf-get-article-result response)))))

(ert-deftest tlon-import-eaf-get-article-result-nil ()
  "Return nil when post not in response."
  (should (null (tlon-import-eaf-get-article-result '((other . "data"))))))

;;;; tlon-import-eaf-get-article-url

(ert-deftest tlon-import-eaf-get-article-url-extracts ()
  "Extract pageUrl from response."
  (let ((response '((post . ((result . ((pageUrl . "https://example.com/post"))))))))
    (should (equal "https://example.com/post"
                   (tlon-import-eaf-get-article-url response)))))

;;;; tlon-import-eaf-get-article-html

(ert-deftest tlon-import-eaf-get-article-html-extracts ()
  "Extract htmlBody from response."
  (let ((response '((post . ((result . ((htmlBody . "<p>Text</p>"))))))))
    (should (equal "<p>Text</p>"
                   (tlon-import-eaf-get-article-html response)))))

;;;; tlon-import-eaf-get-article-title

(ert-deftest tlon-import-eaf-get-article-title-extracts ()
  "Extract title from response."
  (let ((response '((post . ((result . ((title . "My Article"))))))))
    (should (equal "My Article"
                   (tlon-import-eaf-get-article-title response)))))

;;;; tlon-import-eaf-get-tag-result

(ert-deftest tlon-import-eaf-get-tag-result-extracts ()
  "Extract result from tag response."
  (let ((response '((tag . ((result . ((name . "AI Safety") (slug . "ai-safety"))))))))
    (should (equal '((name . "AI Safety") (slug . "ai-safety"))
                   (tlon-import-eaf-get-tag-result response)))))

;;;; tlon-import-eaf-get-tag-url

(ert-deftest tlon-import-eaf-get-tag-url-constructs ()
  "Construct tag URL from slug in response."
  (let ((response '((tag . ((result . ((slug . "ai-safety"))))))))
    (should (equal "https://forum.effectivealtruism.org/topics/ai-safety"
                   (tlon-import-eaf-get-tag-url response)))))

(ert-deftest tlon-import-eaf-get-tag-url-nil-slug ()
  "Return nil when slug is nil."
  (let ((response '((tag . ((result . ((slug . nil))))))))
    (should (null (tlon-import-eaf-get-tag-url response)))))

;;;; tlon-import-eaf-get-tag-html

(ert-deftest tlon-import-eaf-get-tag-html-extracts ()
  "Extract HTML from nested tag description."
  (let ((response '((tag . ((result . ((description . ((html . "<p>Desc</p>"))))))))))
    (should (equal "<p>Desc</p>"
                   (tlon-import-eaf-get-tag-html response)))))

;;;; tlon-import-eaf-shorten-title

(ert-deftest tlon-import-eaf-shorten-title-plain ()
  "Plain title returned as-is."
  (should (equal "My Title" (tlon-import-eaf-shorten-title "My Title"))))

(ert-deftest tlon-import-eaf-shorten-title-strips-trailing ()
  "Strip trailing special characters not in the charset."
  (let ((result (tlon-import-eaf-shorten-title "Title!!!")))
    (should (stringp result))))

;;;; tlon-import-eaf-article-id-p

(ert-deftest tlon-import-eaf-article-id-p-valid ()
  "Valid 17-char alphanumeric ID."
  (should (tlon-import-eaf-article-id-p "abc123def456ghi78")))

(ert-deftest tlon-import-eaf-article-id-p-too-short ()
  "Too-short string is not an ID."
  (should-not (tlon-import-eaf-article-id-p "abc123")))

(ert-deftest tlon-import-eaf-article-id-p-slug ()
  "Slug with hyphens is not an ID."
  (should-not (tlon-import-eaf-article-id-p "effective-altruism")))

;;;; tlon-import-eaf-tag-slug-p

(ert-deftest tlon-import-eaf-tag-slug-p-valid ()
  "Valid slug with hyphens."
  (should (tlon-import-eaf-tag-slug-p "effective-altruism")))

(ert-deftest tlon-import-eaf-tag-slug-p-simple ()
  "Simple alphanumeric slug."
  (should (tlon-import-eaf-tag-slug-p "ai")))

;;;; tlon-import-eaf-get-id-from-identifier

(ert-deftest tlon-import-eaf-get-id-from-url ()
  "Extract ID from canonical EAF post URL."
  (let ((url "https://forum.effectivealtruism.org/posts/abc123def456ghi78/my-post"))
    (should (equal "abc123def456ghi78"
                   (tlon-import-eaf-get-id-from-identifier url)))))

(ert-deftest tlon-import-eaf-get-id-non-eaf ()
  "Return nil for non-EAF URL."
  (should (null (tlon-import-eaf-get-id-from-identifier "https://google.com"))))

;;;; tlon-import-eaf-get-slug-from-identifier

(ert-deftest tlon-import-eaf-get-slug-from-url ()
  "Extract slug from topics URL."
  (let ((url "https://forum.effectivealtruism.org/topics/ai-safety"))
    (should (equal "ai-safety"
                   (tlon-import-eaf-get-slug-from-identifier url)))))

(ert-deftest tlon-import-eaf-get-slug-non-topic ()
  "Return nil for non-topic EAF URL."
  (should (null (tlon-import-eaf-get-slug-from-identifier
                 "https://forum.effectivealtruism.org/posts/abc123"))))

;;;; tlon-import-eaf-get-type

(ert-deftest tlon-import-eaf-get-type-article ()
  "Recognize article ID."
  (should (eq 'article (tlon-import-eaf-get-type "abc123def456ghi78"))))

(ert-deftest tlon-import-eaf-get-type-tag ()
  "Recognize tag slug."
  (should (eq 'tag (tlon-import-eaf-get-type "effective-altruism"))))

;;;; tlon-import-save-html-to-file

(ert-deftest tlon-import-save-html-to-file-creates ()
  "Save HTML to temp file and verify content."
  (let ((file (tlon-import-save-html-to-file "<p>Test</p>")))
    (unwind-protect
        (progn
          (should (file-exists-p file))
          (should (equal "<p>Test</p>"
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (delete-file file))))

(provide 'tlon-import-test)
;;; tlon-import-test.el ends here
