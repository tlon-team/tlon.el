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
(require 'tlon-core)
(require 'transient)
(require 'url-parse)
(require 'tlon-ai)
(require 'tlon-md)
(require 'dired)

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

(defcustom tlon-images-read-file-without-asking nil
  "Whether to process the image at point without asking for confirmation."
  :type 'boolean
  :group 'tlon-images)

(defcustom tlon-images-overwrite-alt-text nil
  "Whether to overwrite existing alt text in images.
This variable only affects the behavior of
`tlon-images-set-image-alt-text-in-buffer'; it is ignored by
`tlon-images-set-image-alt-text-in-file', which always overwrites."
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

(defconst tlon-images-describe-image-prompt
  `((:prompt "Please provide a concise description of the attached image. The description should consist of one or two sentences and must never exceed 50 words. If you need to use quotes, please use single quotes."
	     :language "en")
    (:prompt "Por favor, describe brevemente la imagen adjunta. La descripción debe consistir de una o dos oraciones y en ningún caso debe exceder las 50 palabras. Si necesitas usar comillas, por favor utiliza comillas simples."
	     :language "es")
    (:prompt "Veuillez fournir une description concise de l'image ci-jointe. La description doit consister en une ou deux phrases et ne doit pas dépasser 50 mots. Si vous devez utiliser des guillemets, veuillez utiliser des guillemets simples."
	     :language "fr")
    (:prompt "Si prega di fornire una descrizione concisa dell'immagine allegata. La descrizione deve consistere in una o due frasi e non deve mai superare le 50 parole. Se è necessario utilizzare le virgolette, si prega di utilizzare le virgolette singole."
	     :language "it")
    (:prompt "Bitte geben Sie eine kurze Beschreibung des beigefügten Bildes. Die Beschreibung sollte aus ein oder zwei Sätzen bestehen und darf 50 Wörter nicht überschreiten. Wenn Sie Anführungszeichen verwenden müssen, verwenden Sie bitte einfache Anführungszeichen."
	     :language "de")))

;;;; Functions

(declare-function dired-get-filename "dired")
(declare-function tlon-get-language-in-file "tlon-core")
(declare-function tlon-get-repo-from-file "tlon-core")
(declare-function tlon-lookup "tlon-core")
;;;###autoload
(defun tlon-images-get-dir (&optional file)
  "Return the images directory of FILE.
This directory is constructed as <repo-root>/<image-dir-name>/<slug>, where
<image-dir-name> is language-specific and <slug> is the relative path of FILE
inside the repo, without extension."
  (let* ((file (or file (buffer-file-name)))
         (repo-root (tlon-get-repo-from-file file))
         (relative-path (file-relative-name file repo-root))
         (lang (tlon-get-language-in-file file))
         (image-dir-name (tlon-lookup tlon-image-dirs :name :language lang)))
    (expand-file-name (file-name-as-directory (file-name-sans-extension relative-path))
                      (expand-file-name image-dir-name repo-root))))

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
  (or image
      (let ((insert-default-directory nil)
	    (image-candidate (or (when (derived-mode-p 'dired-mode)
				   (dired-get-filename))
				 (buffer-file-name))))
	(or (and tlon-images-read-file-without-asking image-candidate)
	    (read-file-name "Image: " nil nil nil
			    (when image-candidate
			      (file-relative-name image-candidate default-directory)))))))

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

(defun tlon-images--get-image-format-from-content (buffer)
  "Determine image format from BUFFER content using magic numbers."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\r?\n\r?\n" nil t) ; Move to start of body
        (let ((magic (buffer-substring-no-properties (point) (min (point-max) (+ (point) 12)))))
          (cond
           ((string-prefix-p "\xFF\xD8\xFF" magic) "jpg")
           ((string-prefix-p "\x89PNG\r\n\x1a\n" magic) "png")
           ((string-prefix-p "GIF8" magic) "gif")
           ((and (>= (length magic) 12)
                 (string-prefix-p "RIFF" magic)
                 (string-equal "WEBP" (substring magic 8 12)))
            "webp")
           (t nil)))))))

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

;;;;; invertornot.com request

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

;;;;; Image description

;;;###autoload
(defun tlon-images-describe-image (&optional file callback)
  "Describe the contents of the image in FILE.
By default, print the description in the minibuffer. If CALLBACK is non-nil, use
it instead."
  (interactive)
  ;; we warn here because this command adds files to the context, so the usual
  ;; check downstream must be bypassed via `no-context-check'
  (gptel-extras-warn-when-context)
  (let* ((previous-context gptel-context--alist)
	 (file (tlon-images-read-image-file file))
	 (language (tlon-get-language-in-file file))
	 (default-prompt (tlon-lookup tlon-images-describe-image-prompt :prompt :language language))
	 (custom-callback (lambda (response info)
			    (tlon-images-describe-image-callback response info callback previous-context))))
    (gptel-context-remove-all)
    (gptel-context-add-file file)
    (tlon-make-gptel-request default-prompt nil custom-callback nil t)))

(defun tlon-images-describe-image-callback (response info original-callback previous-context)
  "Handle the response from `tlon-images-describe-image'.
If ORIGINAL-CALLBACK is non-nil, call it with RESPONSE and INFO.
Otherwise, message RESPONSE or an error based on INFO.
Finally, restore `gptel-context--alist' to PREVIOUS-CONTEXT."
  (when original-callback
    (funcall original-callback response info))
  (unless original-callback
    (if response
	(message response)
      (user-error "Error: %s" (plist-get info :status))))
  (setq gptel-context--alist previous-context))

(declare-function tlon-get-tag-attribute-values "tlon-md")
(declare-function tlon-md-insert-attribute-value "tlon-md")
(defun tlon-images-set-image-alt-text-in-file ()
  "Insert a description of the image in the image tag at point.
The image tags are \"Figure\" or \"OurWorldInData\"."
  (interactive)
  (save-excursion
    (if-let* ((src (car (or (tlon-get-tag-attribute-values "Figure")
			    (tlon-get-tag-attribute-values "OurWorldInData"))))
	      (file (tlon-images-get-image-file-from-src src))
	      (pos (point-marker)))
	(tlon-images-describe-image file (lambda (response info)
					   (if response
					       (with-current-buffer (marker-buffer pos)
						 (goto-char pos)
						 (tlon-md-insert-attribute-value "alt" response))
					     (user-error "Error: %s" (plist-get info :status)))))
      (user-error "No \"Figure\" or \"OurWorldInData\" tag at point"))))

(declare-function tlon-md-get-tag-pattern "tlon-md")
(defun tlon-images-set-image-alt-text-in-buffer ()
  "Insert a description of all the images in the current buffer.
If the image already contains a non-empty `alt' field, overwrite it when
`tlon-images-overwrite-alt-text' is non-nil."
  (interactive)
  (save-excursion
    (dolist (tag '("Figure" "OurWorldInData"))
      (goto-char (point-min))
      (while (re-search-forward (tlon-md-get-tag-pattern tag) nil t)
	(when (or tlon-images-overwrite-alt-text
		  (not (match-string 6))
		  (string-empty-p (match-string 6)))
	  (tlon-images-set-image-alt-text-in-file))))))

(declare-function dired-get-filename "dired")
(defun tlon-images-read-image-file (&optional file)
  "Read an image FILE from multiple sources.
In order, the sources are: the value of FILE, the value of `src' attribute in a
`Figure' MDX tag, the image in the current buffer, the image at point in Dired
and the file selected by the user."
  (or file
      (when-let ((name (car (tlon-get-tag-attribute-values "Figure"))))
	(file-name-concat (file-name-as-directory (tlon-get-repo 'no-prompt))
			  (replace-regexp-in-string "^\\.\\./" "" name)))
      (member (buffer-file-name) image-file-name-extensions)
      (when (derived-mode-p 'dired-mode)
	(dired-get-filename))
      (read-file-name "Image file: ")))

(defun tlon-images-get-image-file-from-src (src)
  "Get the image file from the SRC attribute.
If SRC is a One World in Data URL, download the image and return the local file.
Otherwise, construct a local file path from SRC and return it."
  (if (string-match-p "ourworldindata.org" src)
      (let* ((extension ".png")
	     (url (format "https://ourworldindata.org/grapher/thumbnail/%s%s"
			  (car (last (split-string src "/"))) extension))
	     (file (make-temp-file nil nil extension)))
	(url-copy-file url file t)
	file)
    (file-name-concat (file-name-as-directory (tlon-get-repo 'no-prompt))
		      (replace-regexp-in-string "^\\.\\./" "" src))))

;;;;; Download images in Markdown file

;;;###autoload
(defun tlon-images-download-from-markdown (&optional file)
  "Scan Markdown FILE for all image URLs, download them, and store them locally.
The images are stored in a directory structure derived from the FILE's path
relative to the repository root. For example, if FILE is
\".../repo/articles/my-post.md\", images will be saved in
\".../repo/images/articles/my-post/\"."
  (interactive)
  (let* ((file (or file (read-file-name "Download images from Markdown file: " nil nil t
					(file-relative-name (buffer-file-name) default-directory))))
	 (repo-root (tlon-get-repo))
         (target-dir (tlon-images-get-dir file))
         (image-urls (tlon-images--get-image-urls-from-markdown file))
         (counter 1)
         (tag-replacements '()))
    (when (and (file-directory-p target-dir)
               (directory-files target-dir nil "^[^.]"))
      (if (y-or-n-p (format "Directory `%s' is not empty. Delete contents and proceed? "
                            (file-relative-name target-dir repo-root)))
          (delete-directory target-dir t)
        (user-error "Aborted")))
    (unless (file-directory-p target-dir)
      (make-directory target-dir t))
    (dolist (url image-urls)
      (message "Downloading %s..." url)
      (let ((image-data-buffer (url-retrieve-synchronously url)))
        (with-current-buffer image-data-buffer
          (goto-char (point-min))
          (if (re-search-forward "\r?\n\r?\n" nil t)
              (let* ((header-end (match-end 0))
                     (headers (buffer-substring-no-properties (point-min) header-end))
                     (extension
                      (or (tlon-images--get-image-format-from-content (current-buffer))
			  (let ((from-header
				 (let ((case-fold-search t))
				   (when (string-match "Content-Type: image/\\([a-zA-Z0-9-+]+\\)" headers)
                                     (match-string 1 headers)))))
                            (if (string-equal from-header "jpeg")
				"jpg"
                              from-header))
			  (let ((path (url-filename (url-generic-parse-url url))))
                            (when path (file-name-extension path))))))
                (if (not extension)
                    (progn
                      (message "Skipping %s (unknown or unsupported image type)" url)
                      (kill-buffer image-data-buffer))
                  (let* ((lang (tlon-get-language-in-file file))
			 (figure-name (tlon-lookup tlon-figure-names :name :language lang))
			 (image-file-name (format "%s-%02d.%s" figure-name counter extension))
			 (image-path (expand-file-name image-file-name target-dir)))
		    (write-region header-end (point-max) image-path nil 0)
		    (message "Saved to %s" image-path)
                    (let* ((relative-src (file-relative-name image-path (file-name-directory file))))
                      (push (cons url relative-src) tag-replacements))
		    (kill-buffer image-data-buffer)
		    (setq counter (1+ counter))))))))
      (tlon-images--replace-markdown-images-with-figure-tags file tag-replacements)
      (when tlon-images-open-after-processing
	(dired target-dir))
      (message "Downloaded %d images to %s" (length image-urls) (file-relative-name target-dir repo-root)))))

(defun tlon-images--get-image-urls-from-markdown (file)
  "Return a list of all image URLs in Markdown FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((urls '()))
      (goto-char (point-min))
      (while (re-search-forward "!\\[.*?\\](\\(.+?\\))" nil t)
        (push (match-string 1) urls))
      (nreverse urls))))

(defun tlon-images--replace-markdown-images-with-figure-tags (file tag-replacements)
  "Replace Markdown images in FILE using TAG-REPLACEMENTS alist.
Trim leading and trailing newlines from replacement tags before insertion."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward tlon-md-image nil t)
        (let* ((alt (string-trim (or (match-string 1) "")))
               (url (match-string 2))
               (relative-src (assoc-default url tag-replacements #'string=))
               (alt-val (unless (string-empty-p alt) alt))
               (values (list relative-src alt-val nil))
               (figure-tag (tlon-md-get-tag-filled "Figure" values "")))
          (when relative-src
            (replace-match (tlon-images--trim-leading-trailing-newlines figure-tag) t t))))
      (save-buffer))))

(defun tlon-images--trim-leading-trailing-newlines (s)
  "Return S without leading or trailing newlines and surrounding spaces."
  (let ((s1 (replace-regexp-in-string "\\`[ \t]*\n+" "" s)))
    (replace-regexp-in-string "\n+[ \t]*\\'" "" s1)))

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

(transient-define-infix tlon-images-toggle-overwrite-alt-text ()
  "Toggle the value of `tlon-images-overwrite-alt-text' in `images' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-images-overwrite-alt-text
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-images-overwrite-alt-text)))

(transient-define-infix tlon-images-toggle-process-without-asking ()
  "Toggle the value of `'tlon-images-read-file-without-asking' in `images' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-images-read-file-without-asking
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-images-read-file-without-asking)))

;;;###autoload (autoload 'tlon-images-menu "tlon-images" nil t)
(transient-define-prefix tlon-images-menu ()
  "Images menu."
  [["Light/Dark"
    ("a" "auto"                                  tlon-images-auto-process)
    ("r" "reduce brightness"                     tlon-images-reduce-brightnesss)
    ("i" "invert colors"                         tlon-images-invert-colors)
    ("n" "make nontransparent"                   tlon-images-make-nontransparent)
    ""
    "Options"
    ("-o" "open after processing"                tlon-images-toggle-open-after-processing)
    ("-r" "percent brightness reduction"         tlon-images-brightness-reduction-infix)]
   ["Alt text"
    ("d" "display alt text"                      tlon-images-describe-image)
    ("t" "set alt text in tag"                   tlon-images-set-image-alt-text-in-file)
    ("b" "set alt text in buffer"                tlon-images-set-image-alt-text-in-buffer)
    ""
    "Options"
    ("-l" "overwrite alt text"                   tlon-images-toggle-overwrite-alt-text)]
   ["Other"
    ("w" "download images in file"               tlon-images-download-from-markdown)]
   ["General options"
    ("-f" "read file without asking"             tlon-images-toggle-process-without-asking)]])

(provide 'tlon-images)
;;; tlon-images.el ends here

