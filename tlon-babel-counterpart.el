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
(require 'markdown-mode)
(require 'tlon-babel-core)
(require 'tlon-babel-yaml)

;;;; Variables

(defconst tlon-babel-counterpart-local-variables-line-start
  "<!-- Local Variables: -->"
  "Start of the line that contains file local variables.")

(defconst tlon-babel-counterpart-local-variables-line-end
  "<!-- End: -->"
  "End of the line that contains file local variables.")

;;;; Functions

(defun tlon-babel-counterpart-get-content-subtype (&optional file)
  "For repo of FILE, get the value of its `:subtype' property.
If FILE is nil, return the counterpart of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-core-get-repo-from-file file))
	 (type (tlon-babel-core-repo-lookup :type :dir repo)))
    (unless (eq type 'content)
      (user-error "Repo of file `%s' is not of type `content'" file))
    (tlon-babel-core-repo-lookup :subtype :dir repo)))

(defun tlon-babel-counterpart-get (&optional file)
  "Get the counterpart file of FILE.
A file's counterpart is its translation if it is an original, and vice versa.
The translation language is defined by `tlon-babel-core-translation-language'.

If FILE is nil, return the counterpart of the file visited by the current
buffer."
  (let* ((file (or file (tlon-babel-core-buffer-file-name)))
	 (repo (tlon-babel-core-get-repo-from-file file)))
    (pcase (tlon-babel-core-repo-lookup :subtype :dir repo)
      ('translations (tlon-babel-counterpart-get-in-translations file))
      ('originals (tlon-babel-counterpart-get-in-originals file))
      (_ (user-error "Subtype of repo `%s' is neither `originals' nor `translations'" repo)))))

(defun tlon-babel-counterpart-get-in-translations (file)
  "Get the counterpart of FILE, when FILE is in `translations'."
  (if-let ((dir (tlon-babel-counterpart-get-dir file))
	   (locator (tlon-babel-metadata-get-field-value-in-file "original_path" file)))
      (file-name-concat dir locator)
    (user-error "Couldn’t find relevant metadata")))

(defun tlon-babel-counterpart-get-in-originals (file)
  "Get the counterpart of FILE, when FILE is in `originals'."
  (let ((translations-repo (tlon-babel-counterpart-get-repo file)))
    (tlon-babel-metadata-lookup (tlon-babel-metadata-in-repo translations-repo)
				"file"
				"original_path"
				(file-name-nondirectory file))))

(defun tlon-babel-counterpart-get-repo (&optional file)
  "Get the counterpart repo of FILE.
A file's counterpart repo is the repo of that file's counterpart.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (tlon-babel-core-buffer-file-name)))
	 (repo (tlon-babel-core-get-repo-from-file file))
	 (subproject (tlon-babel-core-repo-lookup :subproject :dir repo))
	 (language (tlon-babel-counterpart-get-language repo))
	 (counterpart-repo
	  (tlon-babel-core-repo-lookup :dir
				       :subproject subproject
				       :language language)))
    counterpart-repo))

(defun tlon-babel-counterpart-get-language (&optional repo)
  "Return the language of the counterpart of REPO."
  (let* ((repo (or repo (tlon-babel-core-get-repo)))
	 (language (tlon-babel-core-repo-lookup :language :dir repo)))
    (pcase language
      ("en" tlon-babel-core-translation-language)
      ((pred (lambda (lang) (member lang (mapcar #'car tlon-babel-core-languages)))) "en")
      (_ (user-error "Language not recognized")))))

(defun tlon-babel-counterpart-get-dir (&optional file)
  "Get the counterpart directory of FILE.
A file's counterpart directory is the directory of that file's counterpart. For
example, the counterpart directory of `~/Dropbox/repos/uqbar-es/autores/' is
`~/Dropbox/repos/uqbar-en/authors/'.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-core-get-repo-from-file file))
	 (counterpart-repo (tlon-babel-counterpart-get-repo file))
	 (bare-dir (tlon-babel-counterpart-get-bare-dir file))
	 (source-lang (tlon-babel-core-repo-lookup :language :dir repo))
	 (target-lang (tlon-babel-counterpart-get-language repo))
	 (counterpart-bare-dir (tlon-babel-core-get-bare-dir-translation target-lang source-lang bare-dir)))
    (file-name-concat counterpart-repo counterpart-bare-dir)))

(defun tlon-babel-counterpart-get-bare-dir (&optional file)
  "Get the bare directory of FILE.
A file’s bare directory is its directory minus its repository. For example, the
bare directory of `~/Dropbox/repos/uqbar-es/autores/' is `autores'.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-core-get-repo-from-file file)))
    (directory-file-name (file-name-directory (file-relative-name file repo)))))

(defun tlon-babel-counterpart-open (&optional arg file)
  "Open the counterpart of file in FILE and move point to matching position.
If FILE is nil, open the counterpart of the file visited by the current buffer.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (unless file
    (save-buffer))
  (let* ((fun (if arg #'find-file-other-window #'find-file))
	 (counterpart (tlon-babel-counterpart-get
		       (or file (buffer-file-name))))
	 (paragraphs (tlon-babel-counterpart-count-paragraphs
		      (point-min)
		      (point)))
	 (offset (if (tlon-babel-counterpart-between-paragraphs-p) 0 1)))
    (funcall fun counterpart)
    (goto-char (or (cdr (tlon-babel-counterpart-get-delimiter-region-position
			 tlon-babel-yaml-delimiter))
		   (point-min)))
    (markdown-forward-paragraph (- paragraphs offset))
    (goto-char (1+ (point)))))

(defun tlon-babel-counterpart-open-in-dired (&optional arg file)
  "Open the counterpart of file in FILE in Dired.
If FILE is nil, open the counterpart of the file at point.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (let* ((counterpart (tlon-babel-counterpart-get
		       (or file (dired-get-file-for-visit)))))
    (dired-jump arg counterpart)))

(defun tlon-babel-counterpart-open-dwim (&optional arg file)
  "Open the counterpart of file in FILE as appropriate.
If called in `markdown-mode', open FILE’s counterpart. If called in
`dired-mode', jump to its counterpart’s Dired buffer.

If FILE is nil, act on the file at point or visited in the current buffer.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (pcase major-mode
    ('markdown-mode (tlon-babel-counterpart-open arg file))
    ('dired-mode (tlon-babel-counterpart-open-in-dired arg file))))

(defun tlon-babel-counterpart-open-in-other-window-dwim (&optional file)
  "Open the counterpart of file in FILE as appropriate.
If called in `markdown-mode', open FILE’s counterpart. If called in
`dired-mode', jump to its counterpart’s Dired buffer.

If FILE is nil, act on the file at point or visited in the current buffer.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (tlon-babel-counterpart-open-dwim t file))

(defun tlon-babel-counterpart-between-paragraphs-p ()
  "Return t iff point is right between to paragraphs."
  (not (= (tlon-babel-counterpart-count-paragraphs nil (point))
	  (tlon-babel-counterpart-count-paragraphs nil (1+ (point))))))

(defun tlon-babel-counterpart-get-delimiter-region-position (start-delimiter &optional end-delimiter)
  "Get the position of the region between START-DELIMITER and END-DELIMITER.
If END-DELIMITER is nil, use START-DELIMITER as the end delimiter."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward start-delimiter nil t)
	(let* ((start (match-beginning 0))
	       (end (when (re-search-forward (or end-delimiter start-delimiter) nil t)
		      (match-end 0))))
	  (when (and start end)
	    (cons start end)))))))

(defun tlon-babel-counterpart-count-paragraphs (&optional start end)
  "Count the number of paragraphs in a Markdown buffer between START and END."
  (interactive)
  (save-excursion
    (goto-char (or start
		   (cdr (tlon-babel-counterpart-get-delimiter-region-position
			 tlon-babel-yaml-delimiter))
		   (point-min)))
    (let ((count 0))
      (while (< (point) (or end
			    (cdr (tlon-babel-counterpart-get-delimiter-region-position
				  tlon-babel-counterpart-local-variables-line-start
				  tlon-babel-counterpart-local-variables-line-end))
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
(defun tlon-babel-counterpart-check-paragraph-number-match (&optional file)
  "Check that FILE and its counterpart have the same number of paragraphs.
If FILE is not provided, use the current buffer."
  (interactive)
  (let* ((part (or file (buffer-file-name)))
	 (counterpart (tlon-babel-counterpart-get part))
	 (paras-in-part (tlon-babel-counterpart-count-paragraphs))
	 paras-in-counterpart)
    (with-current-buffer (find-file-noselect counterpart)
      (setq paras-in-counterpart (tlon-babel-counterpart-count-paragraphs)))
    (if (= paras-in-part paras-in-counterpart)
	t
      (message "Paragraph number mismatch: \n%s has %s paragraphs\n%s has %s paragraphs"
	       (file-name-nondirectory part) paras-in-part
	       (file-name-nondirectory counterpart) paras-in-counterpart))))

(defun tlon-babel-counterpart-check-paragraph-number-match-in-dir (dir &optional extension)
  "Check that files in DIR and counterparts have the same number of paragraphs.
If EXTENSION is provided, only check files with that extension. Otherwise,
default to \".md\"."
  (let* ((extension (or extension ".md"))
	 (files (directory-files dir t (concat ".*\\" extension "$"))))
    (cl-loop for file in files
	     do (tlon-babel-counterpart-check-paragraph-number-match file))))

;;;;; Word count

(defun tlon-babel-counterpart-get-local-variables ()
  "Get the text in the \"local variables\" section of the current buffer."
  (cl-destructuring-bind (start . end)
      (tlon-babel-counterpart-get-delimiter-region-position
       tlon-babel-counterpart-local-variables-line-start
       tlon-babel-counterpart-local-variables-line-end)
    (buffer-substring-no-properties start end)))

(defun tlon-babel-counterpart-count-words-extra ()
  "Count extraneous words in current buffer."
  (let ((metadata (mapconcat 'identity (tlon-babel-yaml-get-metadata nil 'raw) " ")))
    (with-temp-buffer
      (insert metadata)
      (when-let ((vars (tlon-babel-counterpart-get-local-variables)))
	(insert vars))
      (goto-char (point-min))
      (count-words-region (point-min) (point-max)))))

(defun tlon-babel-counterpart-count-words-substance ()
  "Count substantive words in current buffer."
  (save-restriction
    (widen)
    (let ((raw (count-words (point-min) (point-max))))
      (- raw (tlon-babel-counterpart-count-words-extra)))))

(defun tlon-babel-counterpart-count-words-in-repo (&optional repo)
  "Count words in Markdown files in REPO.
If REPO is nil, prompt the user for one."
  (interactive)
  (let* ((repo (or repo
		   (intern (completing-read
			    "Repo: "
			    (tlon-babel-core-repo-lookup-all :abbrev :subtype 'translations)))))
	 (initial-buffers (buffer-list))
	 (files (directory-files-recursively
		 (tlon-babel-core-repo-lookup :dir :name repo) "\\.md$"))
	 (total-words 0))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
	(let ((words-in-file (tlon-babel-counterpart-count-words-substance)))
	  (setq total-words (+ total-words words-in-file)))
	(unless (member (current-buffer) initial-buffers)
	  (kill-buffer (current-buffer)))))
    (message (number-to-string total-words))))

(provide 'tlon-babel-counterpart)
;;; tlon-babel-counterpart.el ends here
