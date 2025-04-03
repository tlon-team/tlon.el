;;; tlon-color.el --- Color palette handling -*- lexical-binding: t; eval: (rainbow-mode 1); -*-

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

;; This library provides functions to manage the color palettes of the different
;; websites, to assist the design process.

;;; Code:

(require 'tlon-core)
(require 'transient)

;;;; User options

(defgroup tlon-color ()
  "`tlon-color' group."
  :group 'tlon)

(defcustom tlon-color-format-in-frontend-files
  "hex"
  "Color format to use in the frontend files when setting a color palette.
Allowed values are \"hex\" and \"hsl\"."
  :group 'tlon-color
  :type '(choice (const :tag "Hex" "hex")
		 (const :tag "HSL" "hsl")))

(defcustom tlon-colors-change-step
  1
  "Step to use when changing the hue, saturation or lightness of a color."
  :group 'tlon-color
  :type 'integer)

;;;; Variables

(defconst tlon-color-palettes '()
  "List of color palettes and their light and dark values.")

;;;;; Files

(defconst tlon-color-palette-file
  (file-name-concat tlon-package-dir "tlon-color-palettes.el")
  "File where custom color palettes are persisted.")

(defvar tlon-color-globals-css-file
  (file-name-concat (tlon-repo-lookup :dir :name "uqbar-front")
		    "src/styles/globals.css")
  "Path to the `globals.css' file.")

(defvar tlon-color-theme-colors-js-file
  (file-name-concat (tlon-repo-lookup :dir :name "uqbar-front")
		    "src/app/theme-colors.js")
  "Path to the `theme-colors.js' file.")

;;;;; Frontend elements

;;;;;; CSS

(defconst tlon-color-globals-css-header
  "@media (prefers-color-scheme: %s) {\n"
  "Header for each theme section in the `globals.css' file.
The placeholder %s is replaced with either \"light\" or \"dark\".")

(defconst tlon-color-globals-css-footer
  "\n    }\n}\n\n"
  "Footer for each theme section in the `globals.css' file.")

(defconst tlon-color-globals-css-remove-pattern
  (concat (format tlon-color-globals-css-header ".*")
	  "\\(.\\|\n\\)*?"
	  tlon-color-globals-css-footer)
  "Pattern to remove the colors section in the `globals.css' file.")

;;;;;; JS

(defconst tlon-color-theme-colors-js-header
  "export const %s = {\n"
  "Header for each theme section in the `theme-colors.js' file.")

(defconst tlon-color-theme-colors-js-footer
  "\n};\n\n"
  "Footer for each theme section in the `theme-colors.js' file.")

;;;; Functions

(defun tlon-color-read-palette ()
  "Prompt the user to select an existing palette."
  (list (completing-read "Palette: "
			 (mapcar #'car tlon-color-palettes) nil t)))

;;;###autoload
(defun tlon-color-open-globals-file ()
  "Open the global CSS file."
  (interactive)
  (find-file tlon-color-globals-css-file))

;;;;; Load palettes

;;;###autoload
(defun tlon-color-load-palette (palette &optional language)
  "Load color PALETTE for file in LANGUAGE.
If LANGUAGE is nil, load it for both CSS and JS files."
  (interactive (tlon-color-read-palette))
  (let ((languages (if language (list language) '(css js))))
    (dolist (language languages)
      (tlon-color-remove-colors-section language)
      (tlon-color-insert-colors-section palette language))))

;;;;;; Encoding

(defun tlon-color-insert-colors-section (palette language)
  "Insert PALETTE into the LANGUAGE file."
  (let ((encoded (tlon-color-encode palette language))
	(file (pcase language
		('css tlon-color-globals-css-file)
		('js tlon-color-theme-colors-js-file))))
    (save-excursion
      (with-current-buffer (find-file-noselect file)
	(goto-char (point-min))
	(insert encoded)
	;; FIXME: for the time being we always use `hsl' format for the
	;; `theme-colors.js' file, since the site will otherwise break.
	(if (string= (buffer-file-name) tlon-color-globals-css-file)
	    (color-extras-convert-all tlon-color-format-in-frontend-files)
	  (color-extras-convert-all "hsl"))
	(save-buffer)))))

;; TODO: add name of palette as a comment line in the CSS file
(defun tlon-color-encode (palette language)
  "Encode PALETTE in LANGUAGE."
  (let ((fun (pcase language
	       ('css #'tlon-color-encode-globals-css)
	       ('js #'tlon-color-encode-theme-colors-js)))
	(encoded))
    (dolist (theme '(dark light))
      (push (funcall fun palette theme) encoded))
    (mapconcat #'identity encoded)))

(defun tlon-color-encode-globals-css (palette theme)
  "Generate `globals.css' section for THEME in PALETTE.
THEME is either `light' or `dark'."
  (concat
   (format tlon-color-globals-css-header theme)
   "    :root {\n"
   (tlon-color-encode-frontend-variables palette theme 'css)
   tlon-color-globals-css-footer))

(defun tlon-color-encode-theme-colors-js (palette theme)
  "Generate `theme-colors.js' section for THEME in PALETTE.
THEME is either `light' or `dark'."
  (concat
   (format tlon-color-theme-colors-js-header (pcase theme
					       ('light "LIGHT_COLORS")
					       ('dark "DARK_COLORS")))
   (tlon-color-encode-frontend-variables palette theme 'js)
   tlon-color-theme-colors-js-footer))

(defun tlon-color-encode-frontend-variables (palette theme language)
  "Encode frontend variables for PALETTE of THEME.
THEME is either `light' or `dark'. LANGUAGE is either `css' or `js'."
  (mapconcat (lambda (line)
	       (let* ((variable (car line))
		      (cons (cdr line))
		      (value (pcase theme
			       ('light (car cons))
			       ('dark (cdr cons))))
		      (format-string (pcase language
				       ('css "        %s: %s;")
				       ('js "  \"%s\": \"%s\","))))
		 (format format-string variable value)))
	     (alist-get palette tlon-color-palettes nil nil #'string=) "\n"))

(defun tlon-color-remove-colors-section (language)
  "Remove the colors section in the LANGUAGE file."
  (save-excursion
    (with-current-buffer (find-file-noselect (pcase language
					       ('css tlon-color-globals-css-file)
					       ('js tlon-color-theme-colors-js-file)))
      (pcase language
	('css
	 (goto-char (point-min))
	 (while (re-search-forward tlon-color-globals-css-remove-pattern nil t)
	   (replace-match "")))
	('js
	 (erase-buffer))
	(save-buffer)))))

;;;###autoload
(defun tlon-color-save-frontend-files ()
  "Save `globals.css' and propagate its state to `theme-colors.js'."
  (interactive)
  (save-excursion
    (with-current-buffer (find-file-noselect tlon-color-globals-css-file))
    (save-buffer))
  (tlon-color-store-palette "temp-palette" 'overwrite)
  (tlon-color-load-palette "temp-palette" 'js))

;;;;; Store & delete palettes

;;;###autoload
(defun tlon-color-store-palette (palette-name &optional overwrite)
  "Store the parsed PALETTE-NAME in `tlon-color-palettes'.
If PALETTE-NAME already exists, asks for confirmation to overwrite it, unless
OVERWRITE is non-nil."
  (interactive "MPalette name: ")
  (let* ((parsed-palette (cons palette-name (tlon-color-parse-css)))
         (existing-palette (assoc palette-name tlon-color-palettes))
	 (format-string "Palette %s %s.")
	 action)
    (if existing-palette
        (when (or overwrite (yes-or-no-p "Palette already exists. Overwrite? "))
          (setcdr existing-palette (cdr parsed-palette))
	  (setq action "updated"))
      (push parsed-palette tlon-color-palettes)
      (setq action "added"))
    (tlon-color-save-palettes-to-file)
    (message format-string palette-name action)))

;;;###autoload
(defun tlon-color-delete-palette (palette)
  "Delete stored color PALETTE from `tlon-color-palettes'."
  (interactive (tlon-color-read-palette))
  (setq tlon-color-palettes
	(assoc-delete-all palette tlon-color-palettes nil))
  (tlon-color-save-palettes-to-file)  ;; Call save function if you want changes to persist.
  (message "Palette `%s` removed successfully." palette))

;;;;;; CSS parsing

(defun tlon-color-parse-css ()
  "Parse the CSS file and extract color variables and their values for each theme."
  (let* ((css-buffer (find-file-noselect tlon-color-globals-css-file))
	 (light-collect (tlon-color-parse-css-theme css-buffer 'light))
	 (dark-collect (tlon-color-parse-css-theme css-buffer 'dark)))
    (mapcar (lambda (light-var)
	      (cons (car light-var)
		    (cons (cdr light-var)
			  (cdr (assoc (car light-var) dark-collect)))))
	    light-collect)))

(declare-function color-extras-convert-all "color-extras")
(defun tlon-color-parse-css-theme (css-buffer theme)
  "Parse the CSS-BUFFER for a THEME and return its color definitions as an alist.
To ensure that colors are stored in a consistent format, they are returned in
hex format irrespective of their original format."
  (let ((temp-file (make-temp-file "tlon-color-parse-css-theme"))
	(theme-collect)
	(css-buffer-contents))
    (with-current-buffer css-buffer
      (setq css-buffer-contents (buffer-string)))
    (let ((temp-buf (find-file-noselect temp-file)))
      (unwind-protect
	  (with-current-buffer temp-buf
	    (insert css-buffer-contents)
	    (color-extras-convert-all "hex" temp-file)
	    (goto-char (point-min))
	    (re-search-forward (format tlon-color-globals-css-header theme))
	    (while (re-search-forward "--\\(.*?\\): \\(.*?\\);$"
				      (save-excursion
					(re-search-forward tlon-color-globals-css-footer) (point)) t)
	      (push (cons (intern (concat "--" (match-string-no-properties 1)))
			  (match-string-no-properties 2))
		    theme-collect))
	    (save-buffer)
	    (kill-buffer temp-buf))
	(delete-file temp-file))
      (reverse theme-collect))))

;;;;; Sessions

(defun tlon-color-save-palettes-to-file ()
  "Save the current color palettes to the file."
  (with-temp-file tlon-color-palette-file
    (insert ";;; tlon Color Palettes -*- lexical-binding: t -*-\n\n")
    (insert ";; This file contains saved color palettes for tlon color configurations.\n\n")
    (insert "(setq tlon-color-palettes '")
    (prin1 tlon-color-palettes (current-buffer))
    (insert ")\n\n(provide 'tlon-color-palettes)")))

(defun tlon-color-load-palettes ()
  "Load the color palettes from the file."
  (interactive)
  (let ((file tlon-color-palette-file))
    (when (file-exists-p file)
      (load file))))

(tlon-color-load-palettes)

;;;;; Color manipulation

(declare-function color-extras-looking-at-color "color-extras")
(defun tlon-color-change-value-at-point (component direction &optional step)
  "Increase or decrease the value of color COMPONENT at point by a constant amount.
COMPONENT can be either \"h\", \"s\" or \"l\". DIRECTION can be either `+' or
`-'. STEP is the amount to change the value by. If STEP is nil, use the value of
`tlon-colors-change-step'."
  (let ((color (color-extras-looking-at-color))
	(fun (pcase direction
	       ("+" (intern (format "ct-edit-hsluv-%s-inc" component)))
	       ("-" (intern (format "ct-edit-hsluv-%s-dec" component))))))
    (replace-match "")
    (insert (funcall fun color (or step tlon-colors-change-step)))
    (tlon-color-save-frontend-files)))

;;;;;; Hue

(defun tlon-color-increase-hue-at-point ()
  "Increase the hue of the color at point by a constant amount."
  (interactive)
  (tlon-color-change-value-at-point "h" "+"))

(defun tlon-color-decrease-hue-at-point ()
  "Decrease the hue of the color at point by a constant amount."
  (interactive)
  (tlon-color-change-value-at-point "h" "-"))

(defun tlon-color-change-hue-at-point (change)
  "Decrease the hue of the color at point by CHANGE."
  (interactive "sChange (+ or - follow by step): ")
  (let ((direction (substring change 0 1))
	(step (string-to-number (substring change 1))))
    (tlon-color-change-value-at-point "h" direction step)))

;;;;;; Saturation

(defun tlon-color-increase-saturation-at-point ()
  "Increase the saturation of the color at point by a constant amount."
  (interactive)
  (tlon-color-change-value-at-point "s" "+"))

(defun tlon-color-decrease-saturation-at-point ()
  "Decrease the saturation of the color at point by a constant amount."
  (interactive)
  (tlon-color-change-value-at-point "s" "-"))

(defun tlon-color-change-saturation-at-point (change)
  "Decrease the saturation of the color at point by CHANGE."
  (interactive "sChange (+ or - follow by step): ")
  (let ((direction (substring change 0 1))
	(step (string-to-number (substring change 1))))
    (tlon-color-change-value-at-point "s" direction step)))

;;;;;; Lightness

(defun tlon-color-increase-lightness-at-point ()
  "Increase the lightness of the color at point by a constant amount."
  (interactive)
  (tlon-color-change-value-at-point "l" "+"))

(defun tlon-color-decrease-lightness-at-point ()
  "Decrease the lightness of the color at point by a constant amount."
  (interactive)
  (tlon-color-change-value-at-point "l" "-"))

(defun tlon-color-change-lightness-at-point (change)
  "Decrease the lightness of the color at point by CHANGE."
  (interactive "sChange (+ or - follow by step): ")
  (let ((direction (substring change 0 1))
	(step (string-to-number (substring change 1))))
    (tlon-color-change-value-at-point "l" direction step)))

;;;;; Menu

(transient-define-infix tlon-color-format-in-frontend-files-infix ()
  :class 'transient-lisp-variable
  :variable 'tlon-color-format-in-frontend-files
  :reader (lambda (_ _ _) (completing-read ": " '("hex" "hsl"))))

(transient-define-infix tlon-color-change-step-infix ()
  :class 'transient-lisp-variable
  :variable 'tlon-colors-change-step
  :reader (lambda (_ _ _)  (string-to-number (read-string "Step: "))))

;;;###autoload (autoload 'tlon-color-menu "tlon-color" nil t)
(transient-define-prefix tlon-color-menu ()
  "`tlon-color' menu."
  [["Change colors"
    ""
    "hue (H)"
    ("h" "Increase"                        tlon-color-increase-hue-at-point :transient t)
    ("H" "Decrease"                        tlon-color-decrease-hue-at-point :transient t)
    ("A-H-M-s-h" "Change"                  tlon-color-change-hue-at-point)
    ""
    "saturation (S)"
    ("s" "Increase"                        tlon-color-increase-saturation-at-point :transient t)
    ("S" "Decrease"                        tlon-color-decrease-saturation-at-point :transient t)
    ("H-s" "Change"                        tlon-color-change-saturation-at-point)
    ""
    "lightness (L)"
    ("l" "Increase"                        tlon-color-increase-lightness-at-point :transient t)
    ("L" "Decrease"                        tlon-color-decrease-lightness-at-point :transient t)
    ("H-l" "Change"                        tlon-color-change-lightness-at-point)
    ""
    "Options"
    ("-s" "Change step"                    tlon-color-change-step-infix)]
   ["Palette"
    ""
    ("o" "Load"                            tlon-color-load-palette)
    ("t" "Store"                           tlon-color-store-palette)
    ("d" "Delete"                          tlon-color-delete-palette)
    ""
    ("g" "Open ‘globals.css’ file"         tlon-color-open-globals-file)
    ("f" "Save frontend files"             tlon-color-save-frontend-files)
    ("c" "Convert colors"                  color-extras-convert-all)
    ("C" "Calculate contrast"              color-extras-contrast)
    ""
    "Options"
    ("-f" "Format in frontend files"       tlon-color-format-in-frontend-files-infix)]])

(provide 'tlon-color)
;;; tlon-color.el ends here
