;;; tlon-images-test.el --- Tests for tlon-images -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for image handling: themed file naming, extension normalization,
;; and newline trimming.

;;; Code:

(require 'ert)
(require 'tlon-images)

;;;; tlon-images-get-themed-file-name

(ert-deftest tlon-images-themed-file-name-light ()
  "Generate light theme filename."
  (should (equal "chart-light.png"
                 (tlon-images-get-themed-file-name "chart.png" 'light))))

(ert-deftest tlon-images-themed-file-name-dark ()
  "Generate dark theme filename."
  (should (equal "chart-dark.png"
                 (tlon-images-get-themed-file-name "chart.png" 'dark))))

(ert-deftest tlon-images-themed-file-name-with-path ()
  "Preserve path in themed filename."
  (should (equal "/img/chart-light.svg"
                 (tlon-images-get-themed-file-name "/img/chart.svg" 'light))))

(ert-deftest tlon-images-themed-file-name-invalid-theme ()
  "Signal error for invalid theme."
  (should-error (tlon-images-get-themed-file-name "chart.png" 'sepia)
                :type 'user-error))

;;;; tlon-images--normalize-image-extension

(ert-deftest tlon-images-normalize-extension-jpeg ()
  "Normalize jpeg to jpg."
  (should (equal "jpg" (tlon-images--normalize-image-extension "jpeg"))))

(ert-deftest tlon-images-normalize-extension-png ()
  "Pass through png."
  (should (equal "png" (tlon-images--normalize-image-extension "png"))))

(ert-deftest tlon-images-normalize-extension-uppercase ()
  "Downcase extension."
  (should (equal "jpg" (tlon-images--normalize-image-extension "JPEG"))))

(ert-deftest tlon-images-normalize-extension-x-icon ()
  "Normalize x-icon to ico."
  (should (equal "ico" (tlon-images--normalize-image-extension "x-icon"))))

(ert-deftest tlon-images-normalize-extension-nil ()
  "Return nil for nil."
  (should (null (tlon-images--normalize-image-extension nil))))

(ert-deftest tlon-images-normalize-extension-with-plus ()
  "Handle MIME type with + separator."
  (should (equal "svg" (tlon-images--normalize-image-extension "svg+xml"))))

(provide 'tlon-images-test)
;;; tlon-images-test.el ends here
