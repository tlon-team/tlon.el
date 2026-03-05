;;; tlon-color-test.el --- Tests for tlon-color -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for color palette encoding into CSS and JS formats.

;;; Code:

(require 'ert)
(require 'tlon-color)

;;;; tlon-color-encode-frontend-variables

(ert-deftest tlon-color-encode-frontend-variables-css ()
  "Encode palette variables as CSS custom properties."
  (when (and (boundp 'tlon-color-palettes) tlon-color-palettes)
    (let* ((palette-name (caar tlon-color-palettes))
           (result (tlon-color-encode-frontend-variables palette-name 'light 'css)))
      (should (stringp result))
      (should (string-match-p ":" result)))))

(ert-deftest tlon-color-encode-frontend-variables-js ()
  "Encode palette variables as JS key-value pairs."
  (when (and (boundp 'tlon-color-palettes) tlon-color-palettes)
    (let* ((palette-name (caar tlon-color-palettes))
           (result (tlon-color-encode-frontend-variables palette-name 'light 'js)))
      (should (stringp result))
      (should (string-match-p "\"" result)))))

;;;; tlon-color-encode

(ert-deftest tlon-color-encode-css ()
  "Encode a full palette as CSS."
  (when (and (boundp 'tlon-color-palettes) tlon-color-palettes)
    (let* ((palette-name (caar tlon-color-palettes))
           (result (tlon-color-encode palette-name 'css)))
      (should (stringp result))
      (should (> (length result) 0)))))

(ert-deftest tlon-color-encode-js ()
  "Encode a full palette as JS."
  (when (and (boundp 'tlon-color-palettes) tlon-color-palettes)
    (let* ((palette-name (caar tlon-color-palettes))
           (result (tlon-color-encode palette-name 'js)))
      (should (stringp result))
      (should (> (length result) 0)))))

(provide 'tlon-color-test)
;;; tlon-color-test.el ends here
