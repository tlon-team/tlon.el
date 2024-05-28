;;; tlon-words.el --- Functionality for counting words -*- lexical-binding: t -*-

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

;; Functionality for counting words.

;;; Code:

(require 'tlon-md)
(require 'tlon-yaml)

;;;; Functions

;;;;; Commands

;;;###autoload
(defun tlon-count-words-in-repo (&optional repo-name regexp)
  "Return the number of words in REPO-NAME whose file names match REGEXP.
If REPO is nil, prompt the user for one. If REGEXP is nil, restrict return the
number of words in Markdown files."
  (interactive)
  (let* ((repo-name (or repo-name
			(completing-read "Repo: "
					 (tlon-repo-lookup-all :abbrev :subtype 'translations))))
	 (dir (tlon-repo-lookup :dir :name repo-name)))
    (tlon-count-words-in-dir dir regexp)))

;;;###autoload
(defun tlon-count-words-in-dir (&optional dir regexp)
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
	(tlon-count-words-in-files files))
    (user-error "No directory found")))

;;;###autoload
(defun tlon-count-words-in-files (&optional files)
  "Return the number of words in the list of FILES.
If FILES is nil, use the selected files in Dired, if possible."
  (interactive)
  (when-let ((files (or files (dired-get-marked-files))))
    (let ((initial-buffers (buffer-list))
	  (in-total 0))
      (dolist (file files in-total)
	(with-temp-buffer
	  (insert-file-contents file)
	  (let ((in-file (tlon-count-substantive-words)))
	    (setq in-total (+ in-total in-file)))
	  (unless (member (current-buffer) initial-buffers)
	    (kill-buffer (current-buffer)))))
      (message (number-to-string in-total)))))

;;;;; Filter count

(defun tlon-count-extraneous-words ()
  "Return the number of extraneous words in the current buffer.
Extraneous words are words in the front matter section and words in the local
variables section."
  (let ((metadata (mapconcat 'identity (tlon-yaml-get-metadata nil 'raw) " ")))
    (with-temp-buffer
      (insert metadata)
      (when-let ((vars (tlon-md-get-local-variables)))
	(insert vars))
      (goto-char (point-min))
      (count-words-region (point-min) (point-max)))))

(defun tlon-count-substantive-words ()
  "Return the number of substantive words in the current buffer.
Substantive words are all words minus extraneous words, as that expression is
defined in `tlon-count-extraneous-words'."
  (save-restriction
    (widen)
    (let ((raw (count-words (point-min) (point-max))))
      (- raw (tlon-count-extraneous-words)))))


;;;;; Misc

;; this may be obsolete; revise
    ;;;###autoload
(defun tlon-historic-word-count (&optional repo-name days chars-per-word)
  "Compute the historic word count of REPO-NAME for past DAYS.
CHARS-PER-WORD is the average number of characters per word. The word count is
computed by dividing the file size by CHARS-PER-WORD."
  (interactive)
  (unless (executable-find "gdu")
    (user-error "Please install `gdu' (e.g. `brew install gdu')"))
  (unless (executable-find "gnuplot")
    (user-error "Please install `gnuplot' (e.g. `brew install gnuplot')"))
  (let* ((repo-name (or repo-name
			(completing-read "Repo: "
					 (tlon-repo-lookup-all :name :subtype 'translations))))
	 (dir (tlon-repo-lookup :dir :name repo-name))
	 (days (or days (read-number "How many days into the past? ")))
	 (chars-per-word (or chars-per-word 5.5))
	 (buffer (get-buffer-create "*Directory Size*"))
	 (script (file-name-concat (tlon-repo-lookup :dir :name "babel")
				   "count/historic-word-count")))
    (shell-command (format "sh %s %s %s %s" script dir days chars-per-word) buffer)))
(provide 'tlon-words)

;;;;; Menu

;;;###autoload (autoload 'tlon-words-menu "tlon-words" nil t)
(transient-define-prefix tlon-words-menu ()
  "`words' menu."
  ["Count words"
   ("f" "in file(s)"           tlon-count-words-in-files)
   ("d" "in dir"               tlon-count-words-in-dir)
   ("r" "in repo"              tlon-count-words-in-repo)
   ""
   "Create table"
   ("t" "for dir"              tlon-words-create-table-for-dir)])

(provide 'tlon-words)
;;; tlon-words.el ends here

