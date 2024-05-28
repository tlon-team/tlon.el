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

;;;; Variables

(defconst tlon-words-gdrive-directory-ids
  '(("de" . "1K9OuZSP1yB-2m7G5pib9DkNqyE17bwJ-")
    ("en" . "13WXtjz18x3jSz5NI080SfTJAdWMN-zvC")
    ("es" . "1uwu3CyVdOa3o6NgohQTbMwqQ451fKP8p")
    ("fr" . "12HCj4y9qkA-Mlr4O_pv1AunPzJr76ata")
    ("it" . "1NW7DZIqBqsZIlWAgyxM5VuSJzirwPvOs")
    ("pt" . "1rsSiyWLFY9HbcIeyML0mxM7FiWSLw7u0"))
  "Alist of Google Drive directories and associated IDs.")

;;;; Functions

;;;;; Commands

;;;###autoload
(defun tlon-count-words-in-repo (&optional repo-name format regexp)
  "Return the number of words in REPO-NAME whose file names match REGEXP.
If REPO is nil, prompt the user for one.

FORMAT is the format in which to return the results. If `list', return a list of
cons cells. If `count', return the word count as a number. If `summary', or any
other value, return a message with the word count and the file count.

If REGEXP is nil, restrict return the number of words in Markdown files."
  (interactive)
  (let* ((repo-name (or repo-name
			(completing-read "Repo: "
					 (tlon-repo-lookup-all :abbrev :subtype 'translations))))
	 (dir (tlon-repo-lookup :dir :name repo-name)))
    (tlon-count-words-in-dir dir format regexp 'recursively)))

;;;###autoload
(defun tlon-count-words-in-dir (&optional dir format regexp recursively)
  "Return the number of words files in DIR whose file names match REGEXP.
If DIR is nil, use the directory at point, else prompt the user for one.

FORMAT is the format in which to return the results. If `list', return a list of
cons cells. If `count', return the word count as a number. If `summary', or any
other value, return a message with the word count and the file count.

If REGEXP is nil, restrict return the number of words in Markdown files."
  (interactive)
  (if-let ((dir (or dir
		    (let ((file-at-point (thing-at-point 'filename)))
		      (when (file-directory-p file-at-point)
			file-at-point)))))
      (let* ((regexp (or regexp "\\.md$"))
	     (files (if recursively
			(directory-files-recursively dir regexp)
		      (directory-files dir t regexp))))
	(tlon-count-words-in-files files format))
    (user-error "No directory found")))

;;;###autoload
(defun tlon-count-words-in-files (&optional files format)
  "Return the number of words in the list of FILES.
If FILES is nil, use the selected files in Dired, if possible.

FORMAT is the format in which to return the results. If `list', return a list of
cons cells. If `count', return the word count as a number. If `summary', or any
other value, return a message with the word count and the file count."
  (interactive)
  (when-let ((files (or files (dired-get-marked-files))))
    (let ((initial-buffers (buffer-list))
	  list)
      (dolist (file files list)
	(with-temp-buffer
	  (insert-file-contents file)
	  (push (cons (tlon-count-substantive-words) (file-name-nondirectory file)) list))
	(unless (member (current-buffer) initial-buffers)
	  (kill-buffer (current-buffer))))
      (let ((word-count (apply '+ (mapcar 'car list)))
	    (file-count (length list)))
	(pcase format
	  ('list (reverse list))
	  ('count word-count)
	  (_ (message "%s words in %s files." word-count file-count)))))))

;;;;; Org export

(declare-function dired-filename-at-point "dired-x")
;;;###autoload
(defun tlon-words-create-table-for-dir (&optional dir)
  "Create an `org-mode' table with the word counts for all Markdown files in DIR.
If DIR is nil, use the directory at point, else prompt the user for one."
  (interactive)
  (let* ((dir (or dir (dired-filename-at-point)))
	 (list (tlon-count-words-in-dir dir 'list))
	 (destination (tlon-words-get-table-output-path dir)))
    (tlon-words-insert-table-from-list list destination)
    (tlon-words-export-org-table destination)
    (tlon-words-export-to-google-drive (tlon-words-get-tsv-file destination))))

(declare-function org-table-align "org-table")
(defun tlon-words-insert-table-from-list (list &optional destination)
  "Insert an `org-mode' table into a new buffer from LIST.
LIST is a list of cons cells. If DESTINATION is non-nil, save the table to it."
  (let ((buffer (generate-new-buffer "*Tlön stats*")))
    (switch-to-buffer buffer)
    (dolist (pair list)
      (insert (format "| %s | %s |\n" (cdr pair) (car pair))))
    (org-mode)
    ;; (org-table-align) ; extremely slow!
    (goto-char (point-min))
    (when destination
      (write-file destination))))

(defun tlon-words-get-table-output-path (dir)
  "Return the path of the `org-mode' table file for DIR."
  (let* ((target-dir (file-name-concat (tlon-get-repo 'error) "stats/"))
	 (filename-base (file-name-nondirectory (directory-file-name dir)))
	 (filename (file-name-with-extension filename-base "org")))
    (file-name-concat target-dir filename)))

;;;;; Google Drive export

(declare-function org-table-export "org-table")
(defun tlon-words-export-org-table (&optional file)
  "Export the `org-mode' table in FILE to a `tsv' file.
If FILE is nil, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (destination (tlon-words-get-tsv-file file)))
    (with-current-buffer (find-file-noselect file)
      (org-table-export destination "orgtbl-to-tsv"))))

(defun tlon-words-get-tsv-file (file)
  ""
  (let* ((target-dir (file-name-directory file))
	 (target-file-base (file-name-base (file-name-nondirectory file)))
	 (target-file (file-name-with-extension target-file-base "tsv")))
    (file-name-concat target-dir target-file)))

(defun tlon-words-export-to-google-drive (&optional file)
  "Export FILE to Google Drive.
If FILE is nil, export the file visited by the current buffer."
  (interactive)
  (let* ((dir-id (alist-get (tlon-repo-lookup :language :dir (tlon-get-repo 'error))
			    tlon-words-gdrive-directory-ids nil nil 'string=))
	 (output (shell-command (format "gdrive files import %s --parent %s" file dir-id)))
	 (url (format "https://drive.google.com/drive/folders/%s" dir-id)))
    (pcase output
      (0 (message "File exported to Google Drive."))
      (_ (user-error "Google Drive export error")))
    (browse-url url)))

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

