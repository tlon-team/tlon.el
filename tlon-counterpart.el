;;; tlon-counterpart.el --- File, dir & repo counterparts -*- lexical-binding: t -*-

;; Copyright (C) 2024l

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

;; File, dir & repo counterparts

;;; Code:

(require 'tlon-core)
(require 'tlon-md)
(require 'tlon-yaml)

;;;; Functions

(defun tlon-get-content-subtype (&optional file)
  "For repo of FILE, get the value of its `:subtype' property.
If FILE is nil, return the counterpart of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-get-repo-from-file file))
	 (type (tlon-repo-lookup :type :dir repo)))
    (unless (eq type 'content)
      (user-error "Repo of file `%s' is not of type `content'" file))
    (tlon-repo-lookup :subtype :dir repo)))

(defun tlon-get-counterpart (&optional file)
  "Get the counterpart file of FILE.
A file's counterpart is its translation if it is an original, and vice versa.
The translation language is defined by `tlon-translation-language'.

If FILE is nil, return the counterpart of the file visited by the current
buffer."
  (let* ((file (or file (tlon-core-buffer-file-name)))
	 (repo (tlon-get-repo-from-file file)))
    (pcase (tlon-repo-lookup :subtype :dir repo)
      ('translations (tlon-get-counterpart-in-translations file))
      ('originals (tlon-get-counterpart-in-originals file))
      (_ (user-error "Subtype of repo `%s' is neither `originals' nor `translations'" repo)))))

(defun tlon-get-counterpart-in-translations (file)
  "Get the counterpart of FILE, when FILE is in `translations'."
  (if-let ((dir (tlon-get-counterpart-dir file))
	   (locator (tlon-yaml-get-key "original_path" file)))
      (file-name-concat dir locator)
    (user-error "Couldn’t find relevant metadata")))

(defun tlon-get-counterpart-in-originals (file)
  "Get the counterpart of FILE, when FILE is in `originals'."
  (let ((translations-repo (tlon-get-counterpart-repo file)))
    (tlon-metadata-lookup (tlon-metadata-in-repo translations-repo)
				"file"
				"original_path"
				(file-name-nondirectory file))))

(defun tlon-get-counterpart-repo (&optional file)
  "Get the counterpart repo of FILE.
A file's counterpart repo is the repo of that file's counterpart.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (tlon-core-buffer-file-name)))
	 (repo (tlon-get-repo-from-file file))
	 (subproject (tlon-repo-lookup :subproject :dir repo))
	 (language (tlon-get-counterpart-language repo))
	 (counterpart-repo
	  (tlon-repo-lookup :dir
				  :subproject subproject
				  :language language)))
    counterpart-repo))

(defun tlon-get-counterpart-language (&optional repo)
  "Return the language of the counterpart of REPO."
  (let* ((repo (or repo (tlon-get-repo)))
	 (language (tlon-repo-lookup :language :dir repo)))
    (pcase language
      ("en" tlon-translation-language)
      ((pred (lambda (lang)
	       (member lang (tlon-get-valid-language-codes))))
       "en")
      (_ (user-error "Language not recognized")))))

(defun tlon-get-counterpart-dir (&optional file)
  "Get the counterpart directory of FILE.
A file's counterpart directory is the directory of that file's counterpart. For
example, the counterpart directory of `~/Dropbox/repos/uqbar-es/autores/' is
`~/Dropbox/repos/uqbar-en/authors/'.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-get-repo-from-file file))
	 (counterpart-repo (tlon-get-counterpart-repo file))
	 (bare-dir (tlon-get-bare-dir file))
	 (source-lang (tlon-repo-lookup :language :dir repo))
	 (target-lang (tlon-get-counterpart-language repo))
	 (counterpart-bare-dir (tlon-get-bare-dir-translation target-lang source-lang bare-dir)))
    (file-name-concat counterpart-repo counterpart-bare-dir)))

(defun tlon-get-bare-dir (&optional file)
  "Get the bare directory of FILE.
A file’s bare directory is its directory minus its repository. For example, the
bare directory of `~/Dropbox/repos/uqbar-es/autores/' is `autores'.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-get-repo-from-file file)))
    (directory-file-name (file-name-directory (file-relative-name file repo)))))

(defun tlon-open-counterpart (&optional arg file)
  "Open the counterpart of file in FILE and move point to matching position.
If FILE is nil, open the counterpart of the file visited by the current buffer.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (unless file
    (save-buffer))
  (when-let* ((fun (if arg #'find-file-other-window #'find-file))
	      (counterpart (tlon-get-counterpart
			    (or file (buffer-file-name))))
	      (paragraphs (tlon-count-paragraphs
			   (point-min)
			   (point)))
	      (offset (if (tlon-is-between-paragraphs-p) 0 1)))
    (funcall fun counterpart)
    (goto-char (or (cdr (tlon-get-delimited-region-pos
			 tlon-yaml-delimiter))
		   (point-min)))
    (markdown-forward-paragraph (- paragraphs offset))
    (goto-char (1+ (point)))))

(defun tlon-open-counterpart-in-dired (&optional arg file)
  "Open the counterpart of file in FILE in Dired.
If FILE is nil, open the counterpart of the file at point.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (let* ((counterpart (tlon-get-counterpart
		       (or file (dired-get-file-for-visit)))))
    (dired-jump arg counterpart)))

(defun tlon-open-counterpart-dwim (&optional arg file)
  "Open the counterpart of file in FILE as appropriate.
If called in `markdown-mode', open FILE’s counterpart. If called in
`dired-mode', jump to its counterpart’s Dired buffer.

If FILE is nil, act on the file at point or visited in the current buffer.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (pcase major-mode
    ('markdown-mode (tlon-open-counterpart arg file))
    ('dired-mode (tlon-open-counterpart-in-dired arg file))))

(defun tlon-open-counterpart-in-other-window-dwim (&optional file)
  "Open the counterpart of file in FILE as appropriate.
If called in `markdown-mode', open FILE’s counterpart. If called in
`dired-mode', jump to its counterpart’s Dired buffer.

If FILE is nil, act on the file at point or visited in the current buffer.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (tlon-open-counterpart-dwim t file))

(defun tlon-is-between-paragraphs-p ()
  "Return t iff point is right between to paragraphs."
  (not (= (tlon-count-paragraphs nil (point))
	  (tlon-count-paragraphs nil (min (point-max) (1+ (point)))))))

(defun tlon-count-paragraphs (&optional start end)
  "Count the number of paragraphs in a Markdown buffer between START and END."
  (interactive)
  (save-excursion
    (goto-char (or start
		   (cdr (tlon-get-delimited-region-pos
			 tlon-yaml-delimiter))
		   (point-min)))
    (let ((count 0))
      (while (< (point) (or end
			    (cdr (tlon-get-delimited-region-pos
				  tlon-md-local-variables-line-start
				  tlon-md-local-variables-line-end))
			    (point-max)))
	(let ((pos (point)))
	  (markdown-forward-paragraph)
	  (when (> (point) pos)
	    (setq count (1+ count)))))
      (if (called-interactively-p t)
	  (message "%d" count)
	count))))

;; TODO: make it inform the user where the discrepancies arise, e.g. by coloring
;; the relevant paragraphs
(defun tlon-check-counterpart-paragraph-number-match (&optional file)
  "Check that FILE and its counterpart have the same number of paragraphs.
If FILE is not provided, use the current buffer."
  (interactive)
  (let* ((part (or file (buffer-file-name)))
	 (counterpart (tlon-get-counterpart part))
	 (paras-in-part (tlon-count-paragraphs))
	 paras-in-counterpart)
    (with-current-buffer (find-file-noselect counterpart)
      (setq paras-in-counterpart (tlon-count-paragraphs)))
    (if (= paras-in-part paras-in-counterpart)
	t
      (message "Paragraph number mismatch: \n%s has %s paragraphs\n%s has %s paragraphs"
	       (file-name-nondirectory part) paras-in-part
	       (file-name-nondirectory counterpart) paras-in-counterpart))))

(defun tlon-check-counterpart-paragraph-number-match-in-dir (dir &optional extension)
  "Check that files in DIR and counterparts have the same number of paragraphs.
If EXTENSION is provided, only check files with that extension. Otherwise,
default to \".md\"."
  (let* ((extension (or extension ".md"))
	 (files (directory-files dir t (concat ".*\\" extension "$"))))
    (cl-loop for file in files
	     do (tlon-check-counterpart-paragraph-number-match file))))

(provide 'tlon-counterpart)
;;; tlon-counterpart.el ends here
