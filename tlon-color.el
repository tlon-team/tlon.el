;;; tlon-color.el --- Color functionality -*- lexical-binding: t; eval: (rainbow-mode 1); -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon
;; Version: 0.1

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

;; Color functionality.

;;; Code:

(require 'tlon-core)

;;;; User options

(defgroup tlon-color ()
  "`tlon-color' group."
  :group 'tlon)

(defcustom tlon-color-css-format "HEX"
  "Format for the color values in the css file."
  :type '(choice (const "HEX")
		 (const "HSL"))
  :group 'tlon-color)

;;;; Variables

;;;;; Files

(defvar tlon-color-palette-file "tlon-color-palettes.el"
  "File where custom color palettes are persisted.")

(defvar tlon-color-globals-file
  (file-name-concat (tlon-repo-lookup :dir :name "uqbar-front")
		    "src/styles/globals.css")
  "Path to the `globals.css' file.")

;;;;; Color variables

(defconst tlon-color-palettes '()
  "List of color palettes and their light and dark values.")

;;;;; CSS elements

(defconst tlon-color-css-header
  "@media (prefers-color-scheme: %s) {\n"
  "CSS header for each theme section.
The placeholder %s is replaced with either \"light\" or \"dark\".")

(defconst tlon-color-css-footer
  "\n    }\n}\n"
  "CSS footer for each theme section.")

;;;; Functions

(defun tlon-color-read-palette ()
  "Prompt the user to select an existing palette."
  (list (completing-read "Palette: "
			 (mapcar #'car tlon-color-palettes) nil t)))

;;;;; Set palette

;;;###autoload
(defun tlon-color-set-palette (palette)
  "Set the color PALETTE."
  (interactive (tlon-color-read-palette))
  (tlon-color-remove-colors-section)
  (tlon-color-insert-palette palette))

;;;;;; CSS encoding

(defun tlon-color-encode-css (palette)
  "Generate CSS for PALETTE."
  (let (css)
    (dolist (theme '(light dark))
      (push (tlon-color-encode-css-theme palette theme) css))
    (mapconcat #'identity (reverse css) "\n")))

(defun tlon-color-encode-css-theme (palette theme)
  "Generate css for PALETTE THEME.
THEME is either `light' or `dark'."
  (string-chop-newline
   (concat
    (format tlon-color-css-header theme)
    "    :root {\n"
    (tlon-tlon-encode-css-variables palette theme)
    tlon-color-css-footer)))

(defun tlon-tlon-encode-css-variables (palette theme)
  "Encode CSS variables for PALETTE THEME."
  (mapconcat (lambda (line)
	       (let* ((variable (car line))
		      (cons (cdr line))
		      (value (pcase theme
			       ('light (car cons))
			       ('dark (cdr cons)))))
		 (format "        %s: %s;" variable value)))
	     (alist-get palette tlon-color-palettes nil nil #'string=) "\n"))

(defun tlon-color-remove-colors-section ()
  "Remove the colors section in the CSS file."
  (let ((pattern (concat (format tlon-color-css-header ".*")
			 "\\(.\\|\n\\)*?"
			 tlon-color-css-footer)))
    (save-excursion
      (with-current-buffer (find-file-noselect tlon-color-globals-file)
	(goto-char (point-min))
	(while (re-search-forward pattern nil t)
	  (replace-match ""))
	(save-buffer)))))

(defun tlon-color-insert-palette (palette)
  "Insert PALETTE into the CSS file."
  (let ((css (tlon-color-encode-css palette)))
    (save-excursion
      (with-current-buffer (find-file-noselect tlon-color-globals-file)
	(goto-char (point-min))
	(insert css)
	(save-buffer)))))

;;;;; Add & remove palettes

;;;###autoload
(defun tlon-color-add-palette (name)
  "Add parsed palette with NAME to `tlon-color-palettes'."
  (interactive "MPalette Name: ")
  (let ((new-palette (cons name (tlon-color-parse-css))))
    (unless (assoc name tlon-color-palettes)
      (push new-palette tlon-color-palettes)
      (tlon-color-save-palettes)
      (message "Palette %s added and saved successfully." name))))

;;;###autoload
(defun tlon-color-remove-palette (name)
  "Remove the palette with the given NAME from `tlon-color-palettes'."
  (interactive (tlon-color-read-palette))
  (setq tlon-color-palettes
        (assoc-delete-all name tlon-color-palettes nil))
  (tlon-color-save-palettes)  ;; Call save function if you want changes to persist.
  (message "Palette `%s` removed successfully." name))

;;;;;; CSS parsing

(defun tlon-color-parse-css ()
  "Parse the CSS file and extract color variables and their values for each theme."
  (let ((css-buffer (find-file-noselect tlon-color-globals-file)))
    (let ((light-collect (tlon-color-parse-css-theme css-buffer 'light))
          (dark-collect (tlon-color-parse-css-theme css-buffer 'dark)))
      (kill-buffer css-buffer)
      (mapcar (lambda (light-var)
                (cons (car light-var)
                      (cons (cdr light-var)
                            (cdr (assoc (car light-var) dark-collect)))))
              light-collect))))

(defun tlon-color-parse-css-theme (css-buffer theme)
  "Parse the CSS-BUFFER for a THEME and return its color definitions as an alist."
  (let (theme-collect)
    (with-current-buffer css-buffer
      (goto-char (point-min))
      (re-search-forward (format tlon-color-css-header theme))
      (while (re-search-forward "--\\(.*?\\): \\(.*?\\);"
				(save-excursion
				  (re-search-forward tlon-color-css-footer) (point)) t)
        (push (cons (intern (concat "--" (match-string-no-properties 1)))
                    (match-string-no-properties 2))
              theme-collect)))
    theme-collect))

;;;;; Sessions

(defun tlon-color-save-palettes ()
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

;;;;; Menu

;;;###autoload (autoload 'tlon-color-menu "tlon-color" nil t)
(transient-define-prefix tlon-color-menu ()
  "`tlon-color' menu."
  ["Palette"
   ("a" "Add"               tlon-color-add-palette)
   ("r" "Remove"            tlon-color-remove-palette)
   ("s" "Set"               tlon-color-set-palette)])

(provide 'tlon-color)
;;; tlon-color.el ends here
