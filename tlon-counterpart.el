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
(require 'tlon-paragraphs)
(require 'tlon-yaml)
(require 'cl-lib)

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

(declare-function files-extras-buffer-file-name "files-extras")
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

(defun tlon-get-counterpart-repo (&optional file prompt)
  "Get the counterpart repo of FILE.
A file's counterpart repo is the repo of that file's counterpart.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer.
PROMPT controls whether `tlon-get-counterpart-language' should prompt."
  (setq prompt (or prompt t))
  (let* ((file (or file (files-extras-buffer-file-name)))
	 (repo (tlon-get-repo-from-file file))
	 (subproject (tlon-repo-lookup :subproject :dir repo))
	 (language (tlon-get-counterpart-language repo prompt))
	 (counterpart-repo
	  (tlon-repo-lookup :dir
			    :subproject subproject
			    :language language)))
    counterpart-repo))

(defun tlon-get-counterpart-language (&optional repo prompt)
  "Return the language code of the counterpart of REPO.
If PROMPT is non-nil and the source repo’s language is \"en\",
ask the user which target language to use.  When PROMPT is nil
the first entry in `tlon-project-target-languages' is selected
automatically.  If the source language is not \"en\" the
counterpart is always \"en\"."
  (setq prompt (or prompt t))
  (let* ((repo (or repo (tlon-get-repo)))
	 (language (tlon-repo-lookup :language :dir repo))
	 (languages (mapcar (lambda (lang)
			      (tlon-get-formatted-languages lang 'code))
			    tlon-project-target-languages)))
    (pcase language
      ("en" (if prompt
		(completing-read "Language: " languages nil t)
	      (car languages)))
      ((pred (lambda (lang)
	       (member lang languages)))
       "en")
      (_ (user-error "Language not recognized")))))

(defun tlon-get-counterpart-dir (&optional file target-language-code)
  "Get the counterpart directory of FILE.
A file's counterpart directory is the directory of that file's counterpart. For
example, the counterpart directory of `~/Dropbox/repos/uqbar-es/autores/' is
`~/Dropbox/repos/uqbar-en/authors/'.

If FILE is nil, use the file visited by the current buffer.
If TARGET-LANGUAGE-CODE is provided, use it to determine the counterpart repo
and target language, avoiding prompts. Otherwise, determine them automatically,
which may prompt if the source file is an original."
  (let* ((file (or file (buffer-file-name)))
         (repo (tlon-get-repo-from-file file)) ; Source repo
         (source-lang (tlon-repo-lookup :language :dir repo))
         (bare-dir (tlon-get-bare-dir file))
         (final-target-lang target-language-code)
         (final-counterpart-repo nil))

    (if target-language-code
        (progn
          (setq final-target-lang target-language-code)
          (setq final-counterpart-repo
                (tlon-repo-lookup :dir
                                  :subproject (tlon-repo-lookup :subproject :dir repo)
                                  :language final-target-lang)))
      ;; No target-language-code provided, determine automatically (non-prompting)
      (progn
        (setq final-target-lang (tlon-get-counterpart-language repo nil)) ; nil for prompt
        (setq final-counterpart-repo (tlon-get-counterpart-repo file nil)))) ; nil for prompt

    (if (and final-counterpart-repo final-target-lang)
        (let ((counterpart-bare-dir (tlon-get-bare-dir-translation final-target-lang source-lang bare-dir)))
          (when counterpart-bare-dir
            (file-name-concat final-counterpart-repo counterpart-bare-dir)))
      (progn
        (when tlon-debug
          (message "tlon-get-counterpart-dir: Could not determine counterpart repo or target language for %s (target-code: %s)"
                   file target-language-code))
        nil))))

;;;###autoload
(defun tlon-get-image-counterpart (translated-src &optional file)
  "Return the counterpart path for a TRANSLATED-SRC image path.
This function translates components of the path (repo, directories, filename)
from the language of FILE to the source language (English).
If FILE is nil, use the current buffer's file."
  (let* ((file (or file (buffer-file-name)))
         (current-lang (tlon-get-language-in-file file))
         (target-lang "en")
         (repo (tlon-get-counterpart-repo file))
         (image-dir (tlon-lookup tlon-image-dirs :name :language target-lang))
         (bare-dir (tlon-get-bare-dir-translation target-lang current-lang (tlon-get-bare-dir file)))
         (slug (file-name-base (tlon-yaml-get-key "original_path" file)))
	 (figure-current (tlon-lookup tlon-figure-names :name :language current-lang))
	 (figure-target (tlon-lookup tlon-figure-names :name :language target-lang))
	 (file-name (replace-regexp-in-string figure-current figure-target (file-name-nondirectory translated-src))))
    (file-name-concat repo image-dir bare-dir slug file-name)))

(autoload 'markdown-forward-paragraph "markdown-mode" nil t)
;;;###autoload
(defun tlon-open-counterpart (&optional other-win file)
  "Open the counterpart of file in FILE and move point to matching position.
If FILE is nil, open the counterpart of the file visited by the current buffer.

If called with a prefix argument, or OTHER-WIN is non-nil, open the counterpart
in the other window."
  (interactive "P")
  (unless file
    (save-buffer))
  (if-let* ((file (or file (buffer-file-name)))
	    (counterpart (tlon-get-counterpart file)))
      (let* ((fun (if other-win #'find-file-other-window #'find-file))
	     (paragraphs (tlon-get-number-of-paragraphs
			  (point-min)
			  (point)))
	     (offset (if (tlon-is-between-paragraphs-p) -1 0)))
	(funcall fun counterpart)
	(goto-char (or (cdr (tlon-get-delimited-region-pos
			     tlon-yaml-delimiter))
		       (point-min)))
	(markdown-forward-paragraph (- paragraphs offset))
	(goto-char (1+ (point))))
    (message "Counterpart not found for file `%s'" file)))

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



;;;;; Translate links

(defun tlon-get-counterpart-link (original-relative-link current-buffer-file)
  "Find the counterpart link for ORIGINAL-RELATIVE-LINK in CURRENT-BUFFER-FILE.
Returns the relative path string for the counterpart link, or nil if not found."
  (cl-block tlon-get-counterpart-link
    (let* ((current-dir (file-name-directory current-buffer-file))
           (target-repo (tlon-get-repo-from-file current-buffer-file))
           (target-lang-code (tlon-repo-lookup :language :dir target-repo))
           (buffer-original-path (tlon-yaml-get-key "original_path" current-buffer-file)))
      (unless buffer-original-path
	(warn "No 'original_path' found in metadata for %s" current-buffer-file)
	(cl-return-from tlon-get-counterpart-link nil))
      (let* ((original-repo (tlon-get-counterpart-repo current-buffer-file)) ; Repo of the original file
             (original-buffer-abs-path (file-name-concat original-repo buffer-original-path))
             (original-buffer-dir (file-name-directory original-buffer-abs-path))
             ;; Resolve the original relative link against the original buffer's directory
             (linked-original-abs-path (expand-file-name original-relative-link original-buffer-dir))
             ;; Get the path relative to the original repo root, used as the key in metadata
             (linked-original-relative-path (file-relative-name linked-original-abs-path original-repo))
             ;; Lookup the counterpart file in the target repo's metadata
             (counterpart-abs-path (tlon-metadata-lookup (tlon-metadata-in-repo target-repo)
							 "file"
							 "original_path"
							 linked-original-relative-path)))
	;; If metadata lookup failed, try to build the counterpart path
	;; directly using `tlon-get-counterpart-dir'.  This is necessary for
	;; links that live in sibling sub-directories such as
	;; “../authors/derek-parfit.md”, where no metadata entry exists yet.
	(unless counterpart-abs-path
          (let* ((fallback-dir (tlon-get-counterpart-dir linked-original-abs-path target-lang-code))
		 (fallback-path (when fallback-dir
                                  (file-name-concat fallback-dir
                                                    (file-name-nondirectory linked-original-abs-path)))))
            ;; Accept the fallback even if the file does not exist yet – we still
            ;; want the link to point to the *expected* location of the
            ;; translation.  Emit a debug message when the file is missing so the
            ;; user is aware.
            (when fallback-path
              (setq counterpart-abs-path fallback-path))))
	(if counterpart-abs-path
            (let ((new-relative (file-relative-name counterpart-abs-path current-dir)))
              new-relative)
          (progn
            (warn "Counterpart not found for original link '%s' (resolved original lookup key: %s)"
                  original-relative-link linked-original-relative-path)
            nil))))))

;;;###autoload
(defun tlon-replace-internal-links ()
  "Replace internal Markdown links with their counterparts.
If a region is active, operate only within the region. Otherwise, operate on
the entire buffer.

Searches for links like '[text](./file.md)' or '[text](../dir/file.md)'
and replaces the target path with the path to the corresponding translated file."
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (user-error "This command only works in Markdown buffers"))
  (let* ((buffer-file (buffer-file-name))
         (cnt 0)
         (errors 0)
         (region-active (region-active-p))
         (start (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max)))
         ;; Regex to find markdown links ending in .md, excluding URLs like http:
         ;; Matches [text](link.md) or [text](../path/link.md) etc., allowing whitespace.
         ;; Group 1 captures the relative path we need to replace.
         (link-regex "\\[[^]]*\\](\\s-*\\(\\([.]\\{1,2\\}/\\)?[^):]+\\.md\\)\\s-*)")
         (case-fold-search nil)) ; Ensure case-sensitive matching for paths
    (unless buffer-file
      (user-error "Buffer is not visiting a file"))
    (save-excursion
      (goto-char start)
      (while (re-search-forward link-regex end t)
        (let* ((original-relative-link (match-string 1)) ; Path is group 1
               (match-start (match-beginning 1)))        ; Use group 1 start
          ;; Skip processing for links pointing only to current or parent directory
          (if (member original-relative-link '("./" "../"))
              (goto-char (match-end 0)) ; Skip this match entirely
            ;; Process potentially replaceable links
            (let* ((new-relative-link
                    (save-match-data
                      (tlon-get-counterpart-link original-relative-link buffer-file))))
              ;; Preserve an explicit "./" prefix when the original link used one.
              (when (and new-relative-link
                         (string-prefix-p "./" original-relative-link)
                         (not (string-prefix-p "./" new-relative-link)))
                (setq new-relative-link (concat "./" new-relative-link)))
              (if new-relative-link
                  ;; Counterpart found
                  (if (string= original-relative-link new-relative-link)
                      ;; Counterpart is the same, no replacement needed. Advance past the whole match.
                      (goto-char (match-end 0))
                    ;; Counterpart is different, perform replacement.
                    (replace-match new-relative-link t t nil 1) ; Replace group 1
                    (setq cnt (1+ cnt))
                    ;; Adjust search position: start right after the modified link target
                    (goto-char (+ match-start (length new-relative-link))))
                ;; Counterpart not found
                (setq errors (1+ errors))
                ;; Move past the entire link match to continue searching
                (goto-char (match-end 0))))))))
    (message "Replaced %d internal links. %d counterparts not found." cnt errors)))

;;;;; Menu

;;;###autoload (autoload 'tlon-counterpart-menu "tlon-counterpart" nil t)
(transient-define-prefix tlon-counterpart-menu ()
  "`tlon-counterpart' menu."
  [["Navigation"
    ("u" "visit counterpart"                     tlon-open-counterpart-dwim)
    ("H-u" "visit counterpart other window"      tlon-open-counterpart-in-other-window-dwim)
    ("U" "open counterpart in Dired"             tlon-open-counterpart-in-dired)]
   ["Links"
    ("l" "replace internal links"                tlon-replace-internal-links)]
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
