;;; tlon-images.el --- Image processing functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Image processing functionality.

(require 'tlon)
(require 'transient)

;;; Code:

(defgroup tlon-images ()
  "`tlon' images functionality."
  :group 'emacs)

(defcustom tlon-images-default-brightness-reduction 20
  "Default percentage by which to reduce the brightness of an image."
  :type 'integer
  :group 'tlon-images)

(defcustom tlon-images-open-after-processing t
  "Whether to open the original and processed image after processing."
  :type 'boolean
  :group 'tlon-images)

(defcustom tlon-images-process-without-asking nil
  "Whether to process the image at point without asking for confirmation."
  :type 'boolean
  :group 'tlon-images)

;;;; Variables

(defconst tlon-invertornot-generic-endpoint
  "https://invertornot.com/api/"
  "Generic endpoint for the invertornot.com API.")

(defconst tlon-imagemagick-reduce-brightness
  "magick %1$s -modulate %3$s %2$s"
  "Command to reduce the brightness of an image using `imagemagic'.
The first placeholder is the input image, the second placeholder is the
percentage by which to reduce the brightness, and the third placeholder is the
output image.")

(defconst tlon-imagemagick-invert-colors
  "magick %s -channel RGB -negate %s"
  "Command to invert the colors of an image using `imagemagic'.
The first placeholder is the input image and the second placeholder is the
output image.")

(defconst tlon-imagemagick-make-nontransparent
  "magick %1$s -background %3$s -flatten %2$s"
  "Command to remove the transparent background of an image using `imagemagic'.
The first placeholder is the input image, the second placeholder is the
background color, and the third placeholder is the output image.")

;;;; Functions

(declare-function dired-get-filename "dired")
;;;###autoload
(defun tlon-images-auto-process (&optional image)
  "Darken or invert the colors of IMAGE."
  (interactive)
  (let* ((image (tlon-images-read-image image))
	 (output (tlon-images-get-themed-file-name image 'dark)))
    (if (tlon-images-can-invert-p image)
	(tlon-images-invert-colors image output)
      (tlon-images-reduce-brightnesss image output))))

(defun tlon-images-read-image (&optional image)
  "Prompt the user for an IMAGE."
  (let ((insert-default-directory nil))
    (if-let ((file (or image
		       (when (derived-mode-p 'dired-mode)
			 (dired-get-filename))
		       (buffer-file-name))))
	(if tlon-images-process-without-asking
	    file
	  (read-file-name "Image: " nil nil nil file))
      (read-file-name "Image: "))))

;;;;; Process images

;;;###autoload
(defun tlon-images-reduce-brightnesss (&optional source target percent)
  "Reduce the brightness of SOURCE image by PERCENT and save it to TARGET.
Percent defaults to `tlon-images-default-brightness-reduction' if nil."
  (interactive)
  (let* ((source (tlon-images-read-image source))
         (target (or target (tlon-images-get-themed-file-name source 'dark)))
         (percent (- 100 (or percent tlon-images-default-brightness-reduction)))
         (command (format tlon-imagemagick-reduce-brightness source target percent)))
    (tlon-images-process-image source target command "Reduced brightness of `%s'.")))

;;;###autoload
(defun tlon-images-invert-colors (&optional source target)
  "Invert the colors of SOURCE image and save it to TARGET."
  (interactive)
  (let* ((source (tlon-images-read-image source))
         (target (or target (tlon-images-get-themed-file-name source 'dark)))
         (command (format tlon-imagemagick-invert-colors source target)))
    (tlon-images-process-image source target command "Inverted colors of `%s'.")))

;;;###autoload
(defun tlon-images-make-nontransparent (&optional source target background)
  "Make the BACKGROUND of SOURCE image non-transparent and save it to TARGET.
BACKGROUND defaults to \"white\" if nil."
  (interactive)
  (let* ((source (tlon-images-read-image source))
         (target (or target source))
         (background (or background "white"))
         (command (format tlon-imagemagick-make-nontransparent source target background))
	 (output (shell-command-to-string command)))
    (tlon-images-handle-output output (format "Made `%s' nontransparent." source))))

;;;###autoload
(defun tlon-images-download-from-markdown (&optional file)
  "Scan Markdown FILE for all image URLs, download them, and store them locally.
The images are stored in a directory structure derived from the FILE's path
relative to the repository root. For example, if FILE is
`.../repo/articles/my-post.md`, images will be saved in
`.../repo/images/articles/my-post/`."
  (interactive)
  (let* ((file (or file (read-file-name "Download images from Markdown file: " nil nil t
					(file-relative-name (buffer-file-name) default-directory))))
	 (repo-root (tlon-get-repo))
         (relative-path (file-relative-name file repo-root))
         (target-dir (expand-file-name (file-name-sans-extension relative-path)
                                       (expand-file-name "images" repo-root)))
         (image-urls (tlon-images--get-image-urls-from-markdown file))
         (counter 1))
    (unless (file-directory-p target-dir)
      (make-directory target-dir t))
    (dolist (url image-urls)
      (message "Downloading %s..." url)
      (let ((image-data-buffer (url-retrieve-synchronously url)))
        (with-current-buffer image-data-buffer
          (goto-char (point-min))
          (re-search-forward "^$" nil t)
          (let* ((headers (buffer-substring-no-properties (point-min) (match-beginning 0)))
                 (content-type (when (string-match "Content-Type: image/\\([a-zA-Z0-9-]+\\)" headers)
                                 (match-string 1 headers)))
                 (extension (or content-type "jpg"))
                 (image-file-name (format "figure-%02d.%s" counter extension))
                 (image-path (expand-file-name image-file-name target-dir)))
            (write-region (point) (point-max) image-path nil 0)
            (message "Saved to %s" image-path)
            (kill-buffer image-data-buffer)
            (setq counter (1+ counter))))))
    (message "Downloaded %d images to %s" (length image-urls) (file-relative-name target-dir repo-root))))

(defun tlon-images--get-image-urls-from-markdown (file)
  "Return a list of all image URLs in Markdown FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((urls '()))
      (goto-char (point-min))
      (while (re-search-forward "!\\[.*?\\](\\(.+?\\))" nil t)
        (push (match-string 1) urls))
      (nreverse urls))))

(defun tlon-images-process-image (source target command message-fmt)
  "Utility to process an image from SOURCE to TARGET with COMMAND.
Display MESSAGE-FMT with the name of the source image."
  (let* ((output (shell-command-to-string command))
	 (white (tlon-images-get-themed-file-name source 'light)))
    (copy-file source white t)
    (tlon-images-maybe-open-after-processing white target)
    (tlon-images-handle-output output (format message-fmt source))))

(defun tlon-images-handle-output (output message)
  "Handle OUTPUT of `imagemagic' command and display MESSAGE."
  (if (string-empty-p output)
      (message message)
    (user-error "Error: %s" output)))

(defun tlon-images-get-themed-file-name (file theme)
  "Get the name of image FILE based on THEME.
THEME is either `light' or `dark'."
  (let ((sans-extension (file-name-sans-extension file))
	(extension (file-name-extension file))
	(theme-name (pcase theme
		      ('light "light")
		      ('dark "dark")
		      (_ (user-error "Invalid theme")))))
    (format "%s-%s.%s" sans-extension theme-name extension)))

(declare-function window-extras-split-if-unsplit "window-extras")
(declare-function window-extras-select-side-window "window-extras")
(defun tlon-images-maybe-open-after-processing (original processed)
  "Open the ORIGINAL and PROCESSED image conditionally.
The images are opened conditional on the value of
`tlon-images-open-after-processing'."
  (when tlon-images-open-after-processing
    (window-extras-split-if-unsplit)
    (select-window (window-extras-select-side-window "left"))
    (find-file original)
    (find-file-other-window processed)))

;;;;; `invertornot.com' request

(defun tlon-images-post-file-to-invertornot (file)
  "Post FILE to the `invertornot.com' API and return the response."
  (let* ((boundary "BoundaryString")
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . ,(concat "multipart/form-data; boundary=" boundary))))
         (file-content (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally file)
                         (buffer-string)))
         (url-request-data (encode-coding-string
                            (concat "--" boundary "\r\n"
                                    "Content-Disposition: form-data; name=\"files\"; filename=\""
                                    (file-name-nondirectory file) "\"\r\n"
                                    "Content-Type: image/png\r\n\r\n"
                                    file-content "\r\n"
                                    "--" boundary "--\r\n")
                            'binary))
         (url "https://invertornot.com/api/file"))
    (message "Connecting to `invertornot.com'...")
    (with-current-buffer (url-retrieve-synchronously url t)
      (tlon-images-handle-synchronous-response))))

(defun tlon-images-post-url-to-invertornot (image-url)
  "Post IMAGE-URL to the `invertornot.com' API and return the response."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")
                                     ("Accept" . "application/json")))
        (url-request-data (json-encode-array (vector image-url)))
        (url (format tlon-invertornot-generic-endpoint "url")))
    (with-current-buffer (url-retrieve-synchronously url t)
      (tlon-images-handle-synchronous-response))))

(defun tlon-images-handle-synchronous-response ()
  "Handle the HTTP response from a synchronous request and return JSON."
  (goto-char (point-min))
  (when (search-forward-regexp "^$" nil t)
    (delete-region (point-min) (point))
    (forward-char)
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'string)
           (json-response (json-read)))
      (kill-buffer)
      json-response)))

(defun tlon-images-can-invert-p (image)
  "Return t iff the colors of IMAGE can be safely inverted and nil otherwise."
  (let* ((json (tlon-images-post-file-to-invertornot image))
	 (invert (alist-get "invert" (car json) nil nil #'string=)))
    (pcase invert
      (0 nil)
      (1 t)
      (_ (user-error "Invalid response from server")))))

;;;;; Menu

(transient-define-infix tlon-images-brightness-reduction-infix ()
  "Change the local value of the `tlon-images-default-brightness-reduction'
variable."
  :class 'transient-lisp-variable
  :variable 'tlon-images-default-brightness-reduction
  :reader (lambda (_ _ _) (read-number "Brightness reduction: " tlon-images-default-brightness-reduction))
  :prompt "Disconnect after: ")

(transient-define-infix tlon-images-toggle-open-after-processing ()
  "Toggle the value of `tlon-images-open-after-processing' in `images' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-images-open-after-processing
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-images-open-after-processing)))

(transient-define-infix tlon-images-toggle-process-without-asking ()
  "Toggle the value of `'tlon-images-process-without-asking' in `images' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-images-process-without-asking
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-images-process-without-asking)))

;;;###autoload (autoload 'tlon-images-menu "tlon-images" nil t)
(transient-define-prefix tlon-images-menu ()
  "Images menu."
  [["Commands"
    ("a" "auto"                                  tlon-images-auto-process)
    ("d" "download from markdown"                tlon-images-download-from-markdown)
    ""
    ("r" "reduce brightness"                     tlon-images-reduce-brightnesss)
    ("i" "invert colors"                         tlon-images-invert-colors)
    ("n" "make nontransparent"                   tlon-images-make-nontransparent)]
   ["Options"
    ("-o" "open after processing"                tlon-images-toggle-open-after-processing)
    ("-p" "process without asking"               tlon-images-toggle-process-without-asking)
    ("-r" "percent brightness reduction"         tlon-images-brightness-reduction-infix)]])

(provide 'tlon-images)
;;; tlon-images.el ends here

