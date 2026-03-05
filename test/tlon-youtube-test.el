;;; tlon-youtube-test.el --- Tests for tlon-youtube -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for YouTube helpers: string sanitization and URL extraction.

;;; Code:

(require 'ert)
(require 'tlon-youtube)

;;;; tlon-youtube--sanitize-draw-string

(ert-deftest tlon-youtube-sanitize-draw-string-quotes ()
  "Escape single quotes for ImageMagick draw commands."
  (should (equal "it\\\\'s" (tlon-youtube--sanitize-draw-string "it's"))))

(ert-deftest tlon-youtube-sanitize-draw-string-no-quotes ()
  "Leave strings without quotes unchanged."
  (should (equal "hello world" (tlon-youtube--sanitize-draw-string "hello world"))))

(ert-deftest tlon-youtube-sanitize-draw-string-multiple ()
  "Escape multiple single quotes."
  (should (equal "don\\\\'t won\\\\'t"
                 (tlon-youtube--sanitize-draw-string "don't won't"))))

;;;; tlon-youtube--extract-upload-url

(ert-deftest tlon-youtube-extract-upload-url-found ()
  "Extract Location header URL."
  (let ((output "HTTP/2 200\r\nLocation: https://upload.googleapis.com/abc\r\n\r\n"))
    (should (equal "https://upload.googleapis.com/abc"
                   (tlon-youtube--extract-upload-url output)))))

(ert-deftest tlon-youtube-extract-upload-url-not-found ()
  "Return nil when no Location header."
  (should (null (tlon-youtube--extract-upload-url "HTTP/2 200\r\n\r\n"))))

(provide 'tlon-youtube-test)
;;; tlon-youtube-test.el ends here
