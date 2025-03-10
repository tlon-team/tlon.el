;;; tlon-count.el --- Functionality for counting words -*- lexical-binding: t; fill-column: 80 -*-

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

;; Functionality for counting words.

;;; Code:

(require 'tlon-md)
(require 'tlon-yaml)

;;;; Variables

(defconst tlon-count-gdrive-directory-ids
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

(autoload 'dired-get-marked-files "dired")
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
	(kill-new (number-to-string word-count))
	(pcase format
	  ('list (reverse list))
	  ('count word-count)
	  (_ (message "%s words in %s files." word-count file-count)))))))

;;;;; Org export

(declare-function dired-filename-at-point "dired-x")
;;;###autoload
(defun tlon-count-create-table-for-dir (&optional dir)
  "Create an `org-mode' table with the word counts for all Markdown files in DIR.
If DIR is nil, use the directory at point, else prompt the user for one."
  (interactive)
  (let* ((dir (or dir (dired-filename-at-point)))
	 (list (tlon-count-words-in-dir dir 'list))
	 (destination (tlon-count-get-table-output-path dir)))
    (tlon-count-insert-table-from-list list destination)
    (tlon-count-export-org-table destination)
    (tlon-count-export-to-google-drive (tlon-count-get-tsv-file destination))))

(declare-function org-table-align "org-table")
(defun tlon-count-insert-table-from-list (list &optional destination)
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

(defun tlon-count-get-table-output-path (dir)
  "Return the path of the `org-mode' table file for DIR."
  (let* ((target-dir (file-name-concat (tlon-get-repo 'error) "stats/"))
	 (filename-base (file-name-nondirectory (directory-file-name dir)))
	 (filename (file-name-with-extension filename-base "org")))
    (file-name-concat target-dir filename)))

;;;;; Google Drive export

(declare-function org-table-export "org-table")
(defun tlon-count-export-org-table (&optional file)
  "Export the `org-mode' table in FILE to a `tsv' file.
If FILE is nil, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (destination (tlon-count-get-tsv-file file)))
    (with-current-buffer (find-file-noselect file)
      (org-table-export destination "orgtbl-to-tsv"))))

(defun tlon-count-get-tsv-file (file)
  "Return the path of the `tsv' FILE."
  (let* ((target-dir (file-name-directory file))
	 (target-file-base (file-name-base (file-name-nondirectory file)))
	 (target-file (file-name-with-extension target-file-base "tsv")))
    (file-name-concat target-dir target-file)))

(defun tlon-count-export-to-google-drive (&optional file)
  "Export FILE to Google Drive.
If FILE is nil, export the file visited by the current buffer."
  (interactive)
  (let* ((dir-id (alist-get (tlon-get-language-in-file file 'error)
			    tlon-count-gdrive-directory-ids nil nil 'string=))
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

;;;;; BibTeX

;;;###autoload
(defun tlon-count-bibtex-keys-in-dir (recursive &optional dir)
  "Count occurrences of each BibTeX key in every file in DIR.
If RECURSIVE is non-nil, include files in subdirectories."
  (interactive "P")
  (let* ((dir (let ((file (files-extras-read-file dir)))
                (if (file-directory-p file)
		    file
		  (user-error "Not a directory: %s" file))))
	 (bibkey-pattern (tlon-md-get-tag-pattern "Cite"))
         (files (tlon-get-files-in-dir dir recursive))
         (total-key-counts (make-hash-table :test #'equal)))
    (dolist (file files)
      (let ((file-key-counts (tlon-extract-bibkeys-from-file file bibkey-pattern)))
        (tlon-merge-key-counts total-key-counts file-key-counts)))
    (let ((sorted-keys (tlon-sort-key-counts total-key-counts)))
      (tlon-display-key-counts sorted-keys))))

(defun tlon-get-files-in-dir (dir &optional recursive)
  "Get a list of all files in DIR, excluding hidden files.
If RECURSIVE is non-nil, include files in subdirectories."
  (let ((files (if recursive
		   (directory-files-recursively dir "^[^.][^/]*$")
		 (directory-files dir t "^[^.][^/]*$"))))
    (cl-remove-if #'file-directory-p files)))

(defun tlon-extract-bibkeys-from-file (file bibkey-pattern)
  "Extract and count BibTeX keys in FILE using BIBKEY-PATTERN.
Returns a hash table with BibTeX keys and their counts."
  (let ((key-counts (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert-file-contents file nil nil nil t)
      (while (re-search-forward bibkey-pattern nil t)
        (let ((key (match-string 3)))
          (puthash key (1+ (gethash key key-counts 0)) key-counts))))
    key-counts))

(defun tlon-merge-key-counts (hash-table1 hash-table2)
  "Merge two hash tables HASH-TABLE1 and HASH-TABLE2 containing key counts."
  (maphash (lambda (key count)
             (puthash key (+ count (gethash key hash-table1 0)) hash-table1))
           hash-table2)
  hash-table1)

(defun tlon-sort-key-counts (key-counts)
  "Sort the KEY-COUNTS hash table and return a sorted list."
  (sort (cl-loop for key being the hash-keys of key-counts
                 using (hash-values count)
                 collect (cons key count))
        (lambda (a b) (> (cdr a) (cdr b)))))

(defun tlon-display-key-counts (sorted-keys)
  "Display the sorted key counts SORTED-KEYS in a new buffer."
  (with-current-buffer (get-buffer-create "/BibTeX Key Counts/")
    (erase-buffer)
    (dolist (pair sorted-keys)
      (let ((count (cdr pair)))
        (insert (format "%-4d %s\n" count (car pair)))))
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))))

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
(provide 'tlon-count)

;;;;; Menu

;;;###autoload (autoload 'tlon-count-menu "tlon-count" nil t)
(transient-define-prefix tlon-count-menu ()
  "`count' menu."
  [["Count words"
    ("w f" "in file(s)"                tlon-count-words-in-files)
    ("w d" "in dir"                    tlon-count-words-in-dir)
    ("w r" "in repo"                   tlon-count-words-in-repo)
    ""
    "Create table with word count"
    ("w t" "for dir"                   tlon-count-create-table-for-dir)]
   ["Count BibTeX keys"
    ("k d" "in dir"                    tlon-count-bibtex-keys-in-dir)]])

(provide 'tlon-count)
;;; tlon-count.el ends here

