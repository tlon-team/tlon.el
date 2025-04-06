;;; tlon-counterpart.el --- File, dir & repo counterparts -*- lexical-binding: t -*-

;; Copyright (C) 2025l

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

;;;###autoload
(defun tlon-get-counterpart (&optional file)
  "Get the counterpart file of FILE.
If FILE is nil, return the counterpart of the file visited by the current
buffer.

A file's counterpart is the original if it is a translation, and a translation
into some language if it is the original. If the latter, prompt the user for a
language."
  (let* ((file (or file (files-extras-buffer-file-name)))
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
  (let* ((file (or file (files-extras-buffer-file-name)))
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
	 (language (tlon-repo-lookup :language :dir repo))
	 (languages (mapcar (lambda (language)
			      (tlon-get-formatted-languages language 'code))
			    tlon-project-target-languages)))
    (pcase language
      ("en" (completing-read "Language: " languages))
      ((pred (lambda (lang)
	       "Return t if LANG is the code of one of the Babel languages."
	       (member lang languages)))
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

;;;###autoload
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
	      (paragraphs (tlon-get-number-of-paragraphs
			   (point-min)
			   (point)))
	      (offset (if (tlon-is-between-paragraphs-p) -1 0)))
    (funcall fun counterpart)
    (goto-char (or (cdr (tlon-get-delimited-region-pos
			 tlon-yaml-delimiter))
		   (point-min)))
    (markdown-forward-paragraph (- paragraphs offset))
    (goto-char (1+ (point)))))

(autoload 'dired-get-file-for-visit "dired")
(defun tlon-open-counterpart-in-dired (&optional arg file)
  "Open the counterpart of file in FILE in Dired.
If FILE is nil, open the counterpart of the file at point.

If called with a prefix ARG, open the counterpart in the other window."
  (interactive "P")
  (let* ((counterpart (tlon-get-counterpart
		       (or file (dired-get-file-for-visit)))))
    (dired-jump arg counterpart)))

;;;###autoload
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

;;;###autoload
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
  (not (= (tlon-get-number-of-paragraphs nil (point))
	  (tlon-get-number-of-paragraphs nil (min (point-max) (1+ (point)))))))

(defun tlon-with-paragraphs (file fn &optional return-positions)
  "Execute FN for each paragraph in FILE.
If RETURN-POSITIONS is non-nil, return list of (start . end) positions.
Otherwise, return list of FN's results for each paragraph.
If FILE is nil, use the current buffer's file."
  (with-current-buffer (find-file-noselect (if (stringp file) file (buffer-file-name)))
    (save-excursion
      (goto-char (or (cdr (tlon-get-delimited-region-pos
                           tlon-yaml-delimiter))
                     (point-min)))
      (let ((content-end (or (car (tlon-get-delimited-region-pos
                                   tlon-md-local-variables-line-start
                                   tlon-md-local-variables-line-end))
                             (point-max)))
            result)
        (while (and (< (point) content-end)
                    (not (looking-at-p tlon-md-local-variables-line-start)))
          (let ((start (point)))
            (markdown-forward-paragraph)
            (let ((end (min (point) content-end)))
              (when (and (> end start)
                         (string-match-p "[^\s\n]"
					 (buffer-substring-no-properties start end)))
                (push (if return-positions
                          (cons start end)
			(funcall fn start end))
                      result)))))
        (nreverse result)))))

(defun tlon-get-number-of-paragraphs (&optional start end)
  "Return the number of paragraphs between START and END.
START and END are buffer positions. If START is nil, use `point-min'.
If END is nil, use `point-max'."
  (let ((positions (tlon-with-paragraphs nil #'ignore t)))
    (cl-count-if (lambda (pos)
		   (and (>= (car pos) (or start (point-min)))
			(< (cdr pos) (or end (point-max)))))
		 positions)))

(defun tlon-count-paragraphs (&optional start end)
  "Count the number of paragraphs in the active region.
If the region is not active, count the number of paragraphs between START and
END."
  (interactive)
  (unless (or (region-active-p)
	      (and start end))
    (user-error "No region selected and no START and END specified"))
  (message "Number of paragraphs: %d"
	   (tlon-get-number-of-paragraphs (cl-destructuring-bind (start . end)
					      (if (region-active-p)
						  (cons (region-beginning) (region-end))
						(cons start end))
					    start end))))

(defun tlon-get-corresponding-paragraphs (&optional file counterpart)
  "Return pairs of paragraphs between FILE and its COUNTERPART.
Signals an error if files have different number of paragraphs, and displays the
paragraphs in a buffer only in that case. If COUNTERPART is nil, infer it from
FILE."
  (let* ((file (or file (buffer-file-name)))
         (counterpart (or counterpart (tlon-get-counterpart file)))
         (orig-paras (tlon-with-paragraphs file
					   (lambda (start end)
					     (buffer-substring-no-properties start end))))
         (trans-paras (tlon-with-paragraphs counterpart
					    (lambda (start end)
					      (buffer-substring-no-properties start end))))
         (max-len (max (length orig-paras) (length trans-paras)))
         pairs)
    (dotimes (i max-len)
      (push (cons (nth i orig-paras) (nth i trans-paras)) pairs))
    (setq pairs (nreverse pairs))
    (when (/= (length orig-paras) (length trans-paras))
      (with-current-buffer (get-buffer-create "/Paragraph Pairs/")
        (erase-buffer)
        (insert (format "Paragraph number mismatch: \n%s has %d paragraphs\n%s has %d paragraphs\n\n"
			(file-name-nondirectory file) (length orig-paras)
			(file-name-nondirectory counterpart) (length trans-paras)))
        (dolist (pair pairs)
          (insert "Original:\n"
                  (or (car pair) "[Missing paragraph]")
                  "\n\nTranslation:\n"
                  (or (cdr pair) "[Missing paragraph]")
                  "\n\n"
                  (make-string 40 ?-)
                  "\n\n"))
        (goto-char (point-min))
        (display-buffer (current-buffer))
        (user-error "Paragraph number mismatch")))
    pairs))

(defun tlon-display-corresponding-paragraphs (pairs-or-fn)
  "Display PAIRS-OR-FN of corresponding paragraphs in parallel.
PAIRS-OR-FN can be either the output of `tlon-get-corresponding-paragraphs'
or the function itself."
  (interactive (list #'tlon-get-corresponding-paragraphs))
  (condition-case _err
      (let* ((pairs (if (functionp pairs-or-fn)
                        (funcall pairs-or-fn)
                      pairs-or-fn))
             (buf (get-buffer-create "/Paragraph Pairs/")))
        (with-current-buffer buf
          (erase-buffer)
          (dolist (pair pairs)
            (insert "Original:\n"
                    (or (car pair) "[Missing paragraph]")
                    "\n\nTranslation:\n"
                    (or (cdr pair) "[Missing paragraph]")
                    "\n\n"
                    (make-string 40 ?-)
                    "\n\n"))
          (goto-char (point-min)))
        (display-buffer buf))
    (user-error
     (display-buffer (get-buffer "/Paragraph Pairs/")))))

;;;;; Menu

;;;###autoload (autoload 'tlon-counterpart-menu "tlon-counterpart" nil t)
(transient-define-prefix tlon-counterpart-menu ()
  "`tlon-counterpart' menu."
  [["Navigation"
    ("u" "visit counterpart"                     tlon-open-counterpart-dwim)
    ("H-u" "visit counterpart other window"      tlon-open-counterpart-in-other-window-dwim)
    ("U" "open counterpart in Dired"             tlon-open-counterpart-in-dired)]
   ["Matching"
    ("d" "display corresponding paragraphs"      tlon-display-corresponding-paragraphs)]
   ["Metadata"
    ("o" "set ‘original_path’"                   tlon-yaml-insert-original-path)]])

;;;;; Temporary

(autoload 'simple-extras-asciify-string "simple-extras")
(defun tlon-add-counterpart-metadata (file language bare-dir)
  "Read FILE in LANGUAGE located in BARE-DIR."
  (let ((dir (tlon-repo-lookup :dir :subproject "uqbar" :language language))
	(subdir (tlon-get-bare-dir-translation language "en" bare-dir))
	(lines (split-string (with-temp-buffer
			       (insert-file-contents file)
			       (buffer-string))
			     "\n" t)))
    (dolist (line lines)
      (let* ((pair (split-string line "," t))
	     (original (car pair))
	     (translation (simple-extras-asciify-string (cadr pair)))
	     (translation-path (file-name-concat dir subdir translation))
	     (metadata (format "original_path:       %s\n" original)))
	(message "Processing %s" translation-path)
	(with-current-buffer (find-file-noselect translation-path)
	  (goto-char (point-min))
	  (forward-line 1)
	  (search-forward tlon-yaml-delimiter)
	  (forward-line -1)
	  (insert metadata)
	  (save-buffer))))))

(declare-function simple-extras-slugify "simple-extras")
(defun tlon-add-author-metadata (language)
  "Add metadata to author files in LANGUAGE."
  (let* ((dir (tlon-repo-lookup :dir :subproject "uqbar" :language language))
	 (subdir (tlon-get-bare-dir-translation language "en" "authors"))
	 (full-dir (file-name-concat dir subdir))
	 (files (directory-files full-dir t directory-files-no-dot-files-regexp)))
    (dolist (file files)
      (message "Processing %s" file)
      (with-current-buffer (find-file-noselect file)
	(let* ((title (tlon-yaml-get-key "title"))
	       (slug (simple-extras-slugify title))
	       (filename (file-name-with-extension slug "md"))
	       (metadata `(("type" . "author")
			   ("title" . ,title)
			   ("role" . "[\"creator\"]")
			   ("original_path" . ,filename)
			   ("publication_status" . "production"))))
	  (tlon-yaml-delete-metadata)
	  (tlon-yaml-insert-fields metadata))))))

(provide 'tlon-counterpart)
;;; tlon-counterpart.el ends here
