;;; tlon-babel-words.el --- Functionality for counting words -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon-babel
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

;; Functionality for counting words.

;;; Code:

(require 'tlon-babel-md)
(require 'tlon-babel-yaml)

;;;; Functions

;;;;; Commands

(defun tlon-babel-count-words-in-repo (&optional repo-name regexp)
  "Return the number of words in REPO-NAME whose file names match REGEXP.
If REPO is nil, prompt the user for one. If REGEXP is nil, restrict return the
number of words in Markdown files."
  (interactive)
  (let* ((repo-name (or repo-name
			(completing-read "Repo: "
					 (tlon-babel-repo-lookup-all :abbrev :subtype 'translations))))
	 (dir (tlon-babel-repo-lookup :dir :name repo-name)))
    (tlon-babel-count-words-in-dir dir regexp)))

(defun tlon-babel-count-words-in-dir (&optional dir regexp)
  "Return the number of words files in DIR whose file names match REGEXP.
If DIR is nil, use the directory at point, else prompt the user for one. If
REGEXP is nil, restrict return the number of words in Markdown files."
  (interactive)
  (if-let ((dir (or dir
		    (let ((file-at-point (thing-at-point 'filename)))
		      (when (file-directory-p file-at-point)
			file-at-point)))))
      (let* ((regexp (or regexp "\\.md$"))
	     (files (directory-files-recursively dir regexp)))
	(tlon-babel-count-words-in-files files))
    (user-error "No directory found")))

(defun tlon-babel-count-words-in-files (&optional files)
  "Return the number of words in the list of FILES.
If FILES is nil, use the selected files in Dired, if possible."
  (interactive)
  (when-let ((files (or files (dired-get-marked-files))))
    (let ((initial-buffers (buffer-list))
	  (in-total 0))
      (dolist (file files in-total)
	(with-temp-buffer
	  (insert-file-contents file)
	  (let ((in-file (tlon-babel-count-substantive-words)))
	    (setq in-total (+ in-total in-file)))
	  (unless (member (current-buffer) initial-buffers)
	    (kill-buffer (current-buffer)))))
      (message (number-to-string in-total)))))

;;;;; Filter count

(defun tlon-babel-count-extraneous-words ()
  "Return the number of extraneous words in the current buffer.
Extraneous words are words in the front matter section and words in the local
variables section."
  (let ((metadata (mapconcat 'identity (tlon-babel-yaml-get-metadata nil 'raw) " ")))
    (with-temp-buffer
      (insert metadata)
      (when-let ((vars (tlon-babel-md-get-local-variables)))
	(insert vars))
      (goto-char (point-min))
      (count-words-region (point-min) (point-max)))))

(defun tlon-babel-count-substantive-words ()
  "Return the number of substantive words in the current buffer.
Substantive words are all words minus extraneous words, as that expression is
defined in `tlon-babel-count-extraneous-words'."
  (save-restriction
    (widen)
    (let ((raw (count-words (point-min) (point-max))))
      (- raw (tlon-babel-count-extraneous-words)))))

(provide 'tlon-babel-words)
;;; tlon-babel-words.el ends here

