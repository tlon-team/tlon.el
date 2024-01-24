;;; tlon-babel-counterpart.el --- File, dir & repo counterparts -*- lexical-binding: t -*-

;; Copyright (C) 2024l

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

;; File, dir & repo counterparts

;;; Code:

(require 'dired)
(require 'tlon-babel-core)

;;;; Functions

(defun tlon-babel-get-content-subtype (&optional file)
  "For repo of FILE, get the value of its `:subtype' property.
If FILE is nil, return the counterpart of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-core-get-repo-from-file file))
	 (type (tlon-babel-core-repo-lookup :type :dir repo)))
    (unless (eq type 'content)
      (user-error "Repo of file `%s' is not of type `content'" file))
    (tlon-babel-core-repo-lookup :subtype :dir repo)))

(defun tlon-babel-get-counterpart (&optional file)
  "Get the counterpart file of FILE.
A file's counterpart is its translation if it is an original, and vice versa.
The translation language is defined by `tlon-babel-core-translation-language'.

If FILE is nil, return the counterpart of the file visited by the current
buffer."
  (let* ((file (or file (tlon-babel-core-buffer-file-name)))
	 (repo (tlon-babel-core-get-repo-from-file file)))
    (pcase (tlon-babel-core-repo-lookup :subtype :dir repo)
      ('translations (tlon-babel-get-counterpart-in-translations file))
      ('originals (tlon-babel-get-counterpart-in-originals file))
      (_ (user-error "Subtype of repo `%s' is neither `originals' nor `translations'" repo)))))

(defun tlon-babel-get-counterpart-in-translations (file)
  "Get the counterpart of FILE, when FILE is in `translations'."
  (if-let ((dir (tlon-babel-get-counterpart-dir file))
	   (locator (tlon-babel-metadata-get-field-value-in-file "original_path" file)))
      (file-name-concat dir locator)
    (user-error "Couldn’t find relevant metadata")))

(defun tlon-babel-get-counterpart-in-originals (file)
  "Get the counterpart of FILE, when FILE is in `originals'."
  (let ((translations-repo (tlon-babel-get-counterpart-repo file)))
    (tlon-babel-metadata-lookup (tlon-babel-metadata-in-repo translations-repo)
				"file"
				"original_path"
				(file-name-nondirectory file))))

(defun tlon-babel-get-counterpart-repo (&optional file)
  "Get the counterpart repo of FILE.
A file's counterpart repo is the repo of that file's counterpart.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (tlon-babel-core-buffer-file-name)))
	 (repo (tlon-babel-core-get-repo-from-file file))
	 (subproject (tlon-babel-core-repo-lookup :subproject :dir repo))
	 (language (tlon-babel-get-counterpart-language repo))
	 (counterpart-repo
	  (tlon-babel-core-repo-lookup :dir
				       :subproject subproject
				       :language language)))
    counterpart-repo))

(defun tlon-babel-get-counterpart-language (&optional repo)
  "Return the language of the counterpart of REPO."
  (let* ((repo (or repo (tlon-babel-core-get-repo)))
	 (language (tlon-babel-core-repo-lookup :language :dir repo)))
    (pcase language
      ("en" tlon-babel-core-translation-language)
      ((pred (lambda (lang) (member lang (mapcar #'car tlon-babel-core-languages)))) "en")
      (_ (user-error "Language not recognized")))))

(defun tlon-babel-get-counterpart-dir (&optional file)
  "Get the counterpart directory of FILE.
A file's counterpart directory is the directory of that file's counterpart. For
example, the counterpart directory of `~/Dropbox/repos/uqbar-es/autores/' is
`~/Dropbox/repos/uqbar-en/authors/'.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-core-get-repo-from-file file))
	 (counterpart-repo (tlon-babel-get-counterpart-repo file))
	 (bare-dir (tlon-babel-get-bare-dir file))
	 (source-lang (tlon-babel-core-repo-lookup :language :dir repo))
	 (target-lang (tlon-babel-get-counterpart-language repo))
	 (counterpart-bare-dir (tlon-babel-core-get-bare-dir-translation target-lang source-lang bare-dir)))
    (file-name-concat counterpart-repo counterpart-bare-dir)))

(defun tlon-babel-get-bare-dir (&optional file)
  "Get the bare directory of FILE.
A file’s bare directory is its directory minus its repository. For example, the
bare directory of `~/Dropbox/repos/uqbar-es/autores/' is `autores'.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-core-get-repo-from-file file)))
    (directory-file-name (file-name-directory (file-relative-name file repo)))))

(defun tlon-babel-open-counterpart (&optional arg file)
  "Open the counterpart of file in FILE and move point to matching position.
If FILE is nil, open the counterpart of the file visited by the current buffer.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (unless file
    (save-buffer))
  (let* ((fun (if arg #'find-file-other-window #'find-file))
	 (counterpart (tlon-babel-get-counterpart
		       (or file (buffer-file-name))))
	 (paragraphs (- (tlon-babel-count-paragraphs
			 file (point-min) (min (point-max) (+ (point) 2)))
			1)))
    (funcall fun counterpart)
    (goto-char (point-min))
    (forward-paragraph paragraphs)))

(defun tlon-babel-open-dired-counterpart (&optional arg file)
  "Open the counterpart of file in FILE in Dired.
If FILE is nil, open the counterpart of the file at point.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (let* ((counterpart (tlon-babel-get-counterpart
		       (or file (dired-get-file-for-visit)))))
    (dired-jump arg counterpart)))

(defun tlon-babel-open-counterpart-dwim (&optional arg file)
  "Open the counterpart of file in FILE as appropriate.
If called in `markdown-mode', open FILE’s counterpart. If called in
`dired-mode', jump to its counterpart’s Dired buffer.

If FILE is nil, act on the file at point or visited in the current buffer.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (pcase major-mode
    ('markdown-mode (tlon-babel-open-counterpart arg file))
    ('dired-mode (tlon-babel-open-dired-counterpart arg file))))

(defun tlon-babel-open-counterpart-other-window-dwim (&optional file)
  "Open the counterpart of file in FILE as appropriate.
If called in `markdown-mode', open FILE’s counterpart. If called in
`dired-mode', jump to its counterpart’s Dired buffer.

If FILE is nil, act on the file at point or visited in the current buffer.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (tlon-babel-open-counterpart-dwim t file))

(defun tlon-babel-count-paragraphs (&optional file start end)
  "Return number of paragraphs between START and END in FILE.
If either START or END is nil, default to the beginning and end of the buffer.
If FILE is nil, count paragraphs in the current buffer."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((start (or start (point-min)))
	    (end (min (or end (point-max)))))
	(narrow-to-region start end)
	(goto-char (point-min))
	(- (buffer-size) (forward-paragraph (buffer-size)))))))

(defun tlon-babel-check-paragraph-number-match (&optional file)
  "Check that FILE and its counterpart have the same number of paragraphs.
If FILE is not provided, use the current buffer."
  (interactive)
  (let* ((part (or file (buffer-file-name)))
	 (counterpart (tlon-babel-get-counterpart part))
	 (paras-in-part (tlon-babel-count-paragraphs part))
	 (paras-in-counterpart (tlon-babel-count-paragraphs counterpart)))
    (if (= paras-in-part paras-in-counterpart)
	t
      (message "Paragraph number mismatch: \n%s has %s paragraphs\n%s has %s paragraphs"
	       (file-name-nondirectory part) paras-in-part
	       (file-name-nondirectory counterpart) paras-in-counterpart))))

(defun tlon-babel-check-paragraph-number-match-in-dir (dir &optional extension)
  "Check that files in DIR and counterparts have the same number of paragraphs.
If EXTENSION is provided, only check files with that extension. Otherwise,
default to \".md\"."
  (let* ((extension (or extension ".md"))
	 (files (directory-files dir t (concat ".*\\" extension "$"))))
    (cl-loop for file in files
	     do (tlon-babel-check-paragraph-number-match file))))

(provide 'tlon-babel-counterpart)
;;; tlon-babel-counterpart.el ends here
