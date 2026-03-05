;;; tlon-deepl-test.el --- Tests for tlon-deepl -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for DeepL integration helpers: text pre/post-processing for
;; the newline workaround and translation text extraction.

;;; Code:

(require 'ert)
(require 'tlon-deepl)

;;;; tlon-deepl--preprocess-text / tlon-deepl--postprocess-text

(ert-deftest tlon-deepl-preprocess-quality-optimized ()
  "Replace newlines with tokens in quality_optimized mode."
  (let ((tlon-deepl-model-type "quality_optimized"))
    (let ((result (tlon-deepl--preprocess-text "line1\nline2")))
      (should (not (string-match-p "\n" result)))
      (should (string-match-p (regexp-quote tlon-deepl-newline-token) result)))))

(ert-deftest tlon-deepl-preprocess-other-model ()
  "Leave text unchanged in non-quality_optimized mode."
  (let ((tlon-deepl-model-type "latency_optimized"))
    (should (equal "line1\nline2"
                   (tlon-deepl--preprocess-text "line1\nline2")))))

(ert-deftest tlon-deepl-postprocess-quality-optimized ()
  "Restore newlines from tokens in quality_optimized mode."
  (let ((tlon-deepl-model-type "quality_optimized"))
    (let ((preprocessed (tlon-deepl--preprocess-text "line1\nline2")))
      (should (equal "line1\nline2"
                     (tlon-deepl--postprocess-text preprocessed))))))

(ert-deftest tlon-deepl-postprocess-other-model ()
  "Leave text unchanged in non-quality_optimized mode."
  (let ((tlon-deepl-model-type "latency_optimized"))
    (should (equal "some text"
                   (tlon-deepl--postprocess-text "some text")))))

(ert-deftest tlon-deepl-round-trip ()
  "Round-trip: preprocess then postprocess should return original."
  (let ((tlon-deepl-model-type "quality_optimized"))
    (let* ((original "first\nsecond\nthird")
           (processed (tlon-deepl--preprocess-text original))
           (restored (tlon-deepl--postprocess-text processed)))
      (should (equal original restored)))))

;;;; tlon-deepl--translation-text

(ert-deftest tlon-deepl-translation-text-basic ()
  "Extract text from a translation alist."
  (let ((tlon-deepl-model-type "latency_optimized"))
    (should (equal "hello"
                   (tlon-deepl--translation-text '(("text" . "hello")))))))

(ert-deftest tlon-deepl-translation-text-nil ()
  "Return nil for nil input."
  (should (null (tlon-deepl--translation-text nil))))

;;;; tlon-deepl-model-type-formatter

(ert-deftest tlon-deepl-model-type-formatter-known ()
  "Format a known model type."
  (when (and (boundp 'tlon-deepl--model-choices)
             tlon-deepl--model-choices)
    (let* ((first-choice (car tlon-deepl--model-choices))
           (label (car first-choice))
           (value (cdr first-choice)))
      (should (equal label (tlon-deepl-model-type-formatter value))))))

(ert-deftest tlon-deepl-model-type-formatter-unknown ()
  "Format an unknown model type."
  (should (equal "unknown_model (Unknown)"
                 (tlon-deepl-model-type-formatter "unknown_model"))))

;;;; tlon-deepl--model-uses-newline-workaround-p

(ert-deftest tlon-deepl-model-uses-newline-workaround-true ()
  "Return non-nil for quality_optimized."
  (let ((tlon-deepl-model-type "quality_optimized"))
    (should (tlon-deepl--model-uses-newline-workaround-p))))

(ert-deftest tlon-deepl-model-uses-newline-workaround-false ()
  "Return nil for other model types."
  (let ((tlon-deepl-model-type "latency_optimized"))
    (should-not (tlon-deepl--model-uses-newline-workaround-p))))

(provide 'tlon-deepl-test)
;;; tlon-deepl-test.el ends here
