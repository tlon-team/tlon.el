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
(require 'seq)

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
(defun tlon-get-counterpart (&optional file target-language-code)
  "Get the counterpart file of FILE.
If FILE is nil, return the counterpart of the file visited by the current
buffer.

A file's counterpart is the original if it is a translation, and a translation
into some language if it is the original. If the latter, prompt the user for a
language, unless TARGET-LANGUAGE-CODE is provided."
  (let* ((file (or file (files-extras-buffer-file-name)))
	 (repo (tlon-get-repo-from-file file)))
    (pcase (tlon-repo-lookup :subtype :dir repo)
      ('translations (tlon-get-counterpart-in-translations file))
      ('originals (tlon-get-counterpart-in-originals file target-language-code))
      (_ (user-error "Subtype of repo `%s' is neither `originals' nor `translations'" repo)))))

(declare-function tlon-bibliography-lookup "tlon-bib")
(defun tlon-get-counterpart-in-translations (file)
  "Return the original counterpart of translation FILE.

Determine the original key to search for as follows:
- If FILE is in an Uqbar translation repo, interpret its YAML ‘key’
  as a translation key and map it to the original key via the
  bibliography with `tlon-get-counterpart-key'.
- If FILE is in a non‑Uqbar translation repo, interpret its YAML
  ‘key’ as the original key directly.

Then, search across all repositories registered with
:language \"en\" and :subtype 'originals, using a per-repo cache
that maps original keys to files. If exactly one match is found,
return its absolute path; if none, return nil; if multiple,
prompt to disambiguate."
  (let* ((repo (tlon-get-repo-from-file file))
         (uqbar (tlon-counterpart--uqbar-repo-p repo))
         (tr-key (tlon-yaml-get-key "key" file))
         (orig-key (and tr-key (if uqbar (tlon-get-counterpart-key tr-key) tr-key)))
         (repos (tlon-counterpart--original-repos "en"))
         (hits nil))
    (unless orig-key
      (user-error "Translation file %s has no usable key" file))
    (dolist (r repos)
      (let* ((table (tlon-counterpart--original-table-for-repo r))
             (hit (and table (gethash orig-key table))))
        (when hit (push hit hits))))
    (pcase (length hits)
      (0 nil)
      (1 (car hits))
      (_ (completing-read "Disambiguate original: " (nreverse hits) nil t)))))

(defun tlon-get-counterpart-in-originals (file &optional target-language-code)
  "Return the translation counterpart of original FILE.

TARGET-LANGUAGE-CODE is the target translation language code. If nil,
the user may be prompted to select a language.

This searches explicitly across all repos registered with
:language TARGET-LANGUAGE-CODE and :subtype 'translations. For Uqbar
translation repos, a file’s YAML ‘key’ is a translation key that must
be mapped to the original key via the bibliography. For non‑Uqbar
translation repos, the YAML ‘key’ is taken to be the original key.

If exactly one match is found, return its absolute path; if none,
return nil; if multiple, prompt to disambiguate."
  (let* ((target-language-code (or target-language-code (tlon-select-language 'code 'babel)))
         (orig-key (tlon-yaml-get-key "key" file))
         (repos (tlon-counterpart--translation-repos target-language-code))
         (hits nil))
    (unless orig-key
      (user-error "Original file %s has no key" file))
    (dolist (repo repos)
      (let* ((table (tlon-counterpart--translation-table-for-repo repo target-language-code))
             (hit (and table (gethash orig-key table))))
        (when hit (push hit hits))))
    (pcase (length hits)
      (0 nil)
      (1 (car hits))
      (_ (completing-read "Disambiguate translation: " (nreverse hits) nil t)))))

(defun tlon-get-counterpart-repo (&optional file)
  "Get the counterpart repo of FILE.
A file's counterpart repo is the repo of that file's counterpart. If FILE is
nil, return the counterpart repo of the file visited by the current buffer."
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
  "Return the language code of the counterpart of REPO."
  (let* ((repo (or repo (tlon-get-repo)))
	 (language (tlon-repo-lookup :language :dir repo))
	 (languages (mapcar (lambda (lang)
			      (tlon-get-formatted-languages lang 'code))
			    tlon-project-target-languages)))
    (pcase language
      ("en" (completing-read "Language: " languages nil t))
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
      ;; No TARGET-LANGUAGE-CODE given: prompt exactly once via the repo helper.
      (progn
	(setq final-counterpart-repo (tlon-get-counterpart-repo file)) ; single prompt
	(setq final-target-lang (tlon-repo-lookup :language :dir final-counterpart-repo))))

    (if (and final-counterpart-repo final-target-lang)
	(let* ((is-uqbar (string= "uqbar" (tlon-repo-lookup :subproject :dir final-counterpart-repo)))
               (counterpart-bare-dir (and is-uqbar
                                          (tlon-get-bare-dir-translation final-target-lang source-lang bare-dir))))
          (if is-uqbar
              (when counterpart-bare-dir
                (file-name-concat final-counterpart-repo counterpart-bare-dir))
            final-counterpart-repo))
      (progn
	(when tlon-debug
	  (message "tlon-get-counterpart-dir: Could not determine counterpart repo or target language for %s (target-code: %s)"
		   file target-language-code))
	nil))))

;;;###autoload
(defun tlon-get-image-counterpart (path &optional target-lang)
  "Return the counterpart path for an image PATH.
This function translates components of the path (repo, directories, filename)
from the language of FILE to TARGET-LANG."
  (let* ((file (tlon-get-file-from-image path))
	 (current-lang (tlon-get-language-in-file file))
	 (target-lang (or target-lang (if (string= current-lang "en")
					  (tlon-select-language 'code 'babel)
					"en")))
	 (subproject (tlon-repo-lookup :subproject :dir (tlon-get-repo-from-file file)))
	 (target-repo (tlon-repo-lookup :dir
					:subproject subproject
					:language target-lang))
	 (image-dir (tlon-lookup tlon-image-dirs :name :language target-lang))
	 (bare-dir (tlon-get-bare-dir-translation target-lang current-lang (tlon-get-bare-dir file)))
	 (slug (file-name-base (tlon-get-counterpart file target-lang)))
	 (figure-current (tlon-lookup tlon-figure-names :name :language current-lang))
	 (figure-target (tlon-lookup tlon-figure-names :name :language target-lang))
	 (file-name (replace-regexp-in-string figure-current figure-target (file-name-nondirectory path))))
    (file-name-concat target-repo image-dir bare-dir slug file-name)))

(defun tlon-get-file-from-image (path)
  "Return the file path corresponding to image PATH.
For example, if PATH is
\"../uqbar-en/images/articles/on-caring/figure-1.jpg\", the function returns
\"../uqbar-en/articles/on-caring.md\"."
  (let* ((up-dir (lambda (dir)
		   (directory-file-name (file-name-directory dir))))
	 (one-up (funcall up-dir path))
	 (two-up (funcall up-dir one-up))
	 (slug (file-name-base one-up))
	 (bare-dir (file-name-base two-up))
	 (repo (tlon-get-repo-from-file path))
	 (filename (file-name-with-extension slug "md")))
    (file-name-concat repo bare-dir filename)))

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
    (message "Counterpart not found for file `%s'. Call `tlon-yaml-guess-english-counterpart' from translation" file)))

(autoload 'dired-get-file-for-visit "dired")
(autoload 'dired-get-marked-files "dired")
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

;;;;;  Internal caches to speed up counterpart lookup

(defvar tlon-counterpart--orig->trans-cache (make-hash-table :test #'equal)
  "Cache mapping repository directories to translation tables.
Each value is a hash table mapping original bibliography keys to their
corresponding translation file paths within that repository.")

(defun tlon-counterpart--uqbar-repo-p (repo-dir)
  "Return non-nil iff REPO-DIR belongs to subproject \"uqbar\"."
  (string= "uqbar" (tlon-repo-lookup :subproject :dir repo-dir)))

(defun tlon-counterpart--translation-repos (language)
  "Return a list of translation repo dirs for LANGUAGE."
  (tlon-repo-lookup-all :dir :language language :subtype 'translations))

(defun tlon-counterpart--original-repos (language)
  "Return a list of originals repo dirs for LANGUAGE."
  (tlon-repo-lookup-all :dir :language language :subtype 'originals))

(defun tlon-counterpart--translation-table-for-repo (repo-dir target-language)
  "Return a hash table mapping original keys to translation files in REPO-DIR.

The mapping differs by repo type:
- Uqbar translation repos: YAML ‘key’ is a translation key; map it to
  the original key via `tlon-get-counterpart-key'.
- Non‑Uqbar translation repos: YAML ‘key’ is the original key.

The table is cached in `tlon-counterpart--orig->trans-cache'."
  (or (gethash repo-dir tlon-counterpart--orig->trans-cache)
      (puthash
       repo-dir
       (let* ((uqbar (tlon-counterpart--uqbar-repo-p repo-dir))
              (table (make-hash-table :test #'equal)))
         (dolist (file (directory-files-recursively repo-dir "\\.md\\'"))
           (when-let ((tr-key (tlon-yaml-get-key "key" file)))
             (let ((orig-key (if uqbar
                                 (tlon-get-counterpart-key tr-key target-language)
                               tr-key)))
               (when orig-key
                 (puthash orig-key file table)))))
         table)
       tlon-counterpart--orig->trans-cache)))

(defvar tlon-counterpart--orig-key->orig-file-cache (make-hash-table :test #'equal)
  "Cache mapping repository directories to originals tables.
Each value maps original keys to original file paths within that repo.")

(defun tlon-counterpart--original-table-for-repo (repo-dir)
  "Return a hash table mapping original keys to original files in REPO-DIR.
The table is cached in `tlon-counterpart--orig-key->orig-file-cache'."
  (or (gethash repo-dir tlon-counterpart--orig-key->orig-file-cache)
      (puthash
       repo-dir
       (let ((table (make-hash-table :test #'equal)))
         (dolist (file (directory-files-recursively repo-dir "\\.md\\'"))
           (when-let ((ok (tlon-yaml-get-key "key" file)))
             (puthash ok file table)))
         table)
       tlon-counterpart--orig-key->orig-file-cache)))

;;;;;  File lookup helpers

(defun tlon-counterpart--file-for-key (key language)
  "Return path of the file whose YAML ‘key’ is KEY from repo in LANGUAGE.
If no file is found, return nil."
  (when-let* ((repo (tlon-repo-lookup :dir :subproject "uqbar" :language language)))
    (seq-find (lambda (file)
                (string= key (tlon-yaml-get-key "key" file)))
              (directory-files-recursively repo "\\.md\\'"))))

;;;;; Translate relative links

(defconst tlon-translate-relative-links--cur-dir-pattern
  "\\`\\(\\.\\/[^#) \t\n\r/]+\\)\\(#[^) \t\n\r]*\\)?\\'"
  "Regex pattern to match relative links in the current directory.")

(defconst tlon-translate-relative-links--parent-dir-pattern
  "\\`\\(\\.\\./[^/]+/[^#) \t\n\r]+\\)\\(#[^) \t\n\r]*\\)?\\'"
  "Regex pattern to match relative links in the parent directory.")

;;;###autoload
(defun tlon-translate-relative-links (&optional file)
  "Translate relative Markdown links in FILE.
If FILE is nil, use the file visited by the current buffer, if available, else
prompt for a file. Handles \"./filename.md\" and \"../bare-dir/filename.md\"
forms, preserving anchors."
  (interactive)
  (let* ((file (or file (buffer-file-name) (read-file-name "File: ")))
	 (trans-dir (file-name-directory file))
         (trans-lang (tlon-get-language-in-file file))
	 (trans-root (tlon-get-repo-from-file file))
	 (trans-bare (tlon-get-bare-dir file))
	 (subproject (tlon-repo-lookup :subproject :dir trans-root))
         (orig-lang "en")
	 (orig-root (tlon-repo-lookup :dir :language orig-lang :subproject subproject))
         (orig-bare (tlon-get-bare-dir-translation orig-lang trans-lang trans-bare))
         (changes 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-regex-link-inline nil t)
        (when-let ((url (match-string-no-properties 6)))
	  (let ((start (match-beginning 6))
		(end (match-end 6)))
            (when-let ((result (tlon-translate-relative-links--process-link
				url start end trans-dir trans-lang orig-root orig-bare orig-lang)))
              (setq changes (+ changes result)))))))
    (message "Translated %d relative link%s" changes (if (= changes 1) "" "s"))))

(defun tlon-translate-relative-links--process-link (url start end trans-dir trans-lang orig-root orig-bare orig-lang)
  "Process a single relative link URL and return number of modifications made.
URL is the relative link URL to process. START is the buffer position where the
URL begins. END is the buffer position where the URL ends. TRANS-DIR is the
directory of the translation file. TRANS-LANG is the language code of the
translation. ORIG-ROOT is the root directory of the original files. ORIG-BARE is
the bare directory name of the original file. ORIG-LANG is the language code of
the original file."
  (cond
   ((string-match tlon-translate-relative-links--cur-dir-pattern url)
    (tlon-translate-relative-links--process-link-with-pattern
     url start end trans-dir trans-lang orig-root orig-bare orig-lang
     tlon-translate-relative-links--cur-dir-pattern t))
   ((string-match tlon-translate-relative-links--parent-dir-pattern url)
    (tlon-translate-relative-links--process-link-with-pattern
     url start end trans-dir trans-lang orig-root orig-bare orig-lang
     tlon-translate-relative-links--parent-dir-pattern nil))))

(defun tlon-translate-relative-links--process-link-with-pattern
    (url start end trans-dir trans-lang orig-root orig-bare orig-lang pattern add-dot-slash)
  "Apply link translation for URL matched by PATTERN. Return 1 if modified.
URL is the URL string to process. START and END bound the URL in the buffer.
TRANS-DIR is the translation file directory. TRANS-LANG is the translation
language code. ORIG-ROOT is the originals repo root. ORIG-BARE is the originals
bare directory. ORIG-LANG is the originals language code. PATTERN determines how
to parse URL. When ADD-DOT-SLASH is non-nil, prefix the rewritten link with
\"./\"."
  (when-let ((parts (tlon-translate-relative-links--extract-file-part-and-anchor url pattern)))
    (let* ((file-part (nth 0 parts))
           (anchor (nth 1 parts))
           (local-path (expand-file-name file-part trans-dir)))
      (unless (file-exists-p local-path)
        (let ((orig-path (tlon-translate-relative-links--get-original-path
                          file-part orig-root orig-bare trans-lang orig-lang)))
          (when (and orig-path (file-exists-p orig-path))
            (condition-case _err
                (when-let* ((trans-path (tlon-get-counterpart orig-path trans-lang)))
                  (let* ((rel (file-relative-name trans-path trans-dir))
                         (new-url (concat (if add-dot-slash "./" "") rel anchor)))
                    (tlon-translate-relative-links--replace-url start end new-url)))
              (error nil))))))))

(defun tlon-translate-relative-links--extract-file-part-and-anchor (url pattern)
  "Extract file part and anchor from URL using PATTERN.
URL is the URL string to extract parts from. PATTERN is the regular expression
pattern to match against URL."
  (when (string-match pattern url)
    (list (match-string 1 url)
          (or (match-string 2 url) ""))))

(defun tlon-translate-relative-links--get-original-path (file-part orig-root orig-bare trans-lang orig-lang)
  "Get original path for FILE-PART.
FILE-PART is the file part of the relative link. TRANS-DIR is the directory of
the translation file. ORIG-ROOT is the root directory of the original files.
ORIG-BARE is the bare directory name of the original file. TRANS-LANG is the
language code of the translation. ORIG-LANG is the language code of the original
file."
  (let ((file-part (car (split-string file-part "#" t))))
    (cond
     ((string-prefix-p "./" file-part)
      (let ((filename (file-name-nondirectory file-part)))
        (file-name-concat orig-root orig-bare filename)))
     ((string-prefix-p "../" file-part)
      (let* ((after-dotdot (substring file-part 3))
             (bare-other (car (split-string after-dotdot "/" t)))
             (filename (file-name-nondirectory file-part))
             (orig-bare-other-guess (tlon-get-bare-dir-translation orig-lang trans-lang bare-other))
             (orig-bare-other (or orig-bare-other-guess bare-other)))
        (file-name-concat orig-root orig-bare-other filename))))))

(defun tlon-translate-relative-links--replace-url (start end new-url)
  "Replace URL from START to END with NEW-URL and return 1.
START is the buffer position where the URL begins. END is the buffer position
where the URL ends. NEW-URL is the replacement URL string to insert."
  (goto-char start)
  (delete-region start end)
  (insert new-url)
  1)

;;;###autoload
(defun tlon-translate-relative-links-in-dired (&optional files)
  "Translate relative Markdown links in marked FILES in Dired.
When called interactively in Dired, operate on all marked files. FILES, when
non-nil, should be a list of absolute file paths to process. When errors occur,
also collect and display their messages in a dedicated buffer."
  (interactive)
  (let* ((files (or files
                    (when (derived-mode-p 'dired-mode)
                      (dired-get-marked-files))
                    (user-error "Call from Dired or provide FILES")))
         (processed 0)
         (errors 0)
         (error-messages nil))
    (dolist (f files)
      (when (and (stringp f) (string-suffix-p ".md" f t))
        (condition-case err
            (with-current-buffer (find-file-noselect f)
              (save-excursion
                (tlon-translate-relative-links nil))
              (setq processed (1+ processed)))
          (error
           (setq errors (1+ errors))
           (push (format "%s: %s" f (error-message-string err)) error-messages)))))
    (message "Translated relative links in %d file%s (%d error%s)"
             processed (if (= processed 1) "" "s")
             errors (if (= errors 1) "" "s"))
    (when (> errors 0)
      (with-current-buffer (get-buffer-create "*tlon-translate-relative-links errors*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Errors while translating relative links:\n\n")
          (dolist (m (nreverse error-messages)) (insert "• " m "\n"))
          (goto-char (point-min))
          (view-mode 1))
        (pop-to-buffer (current-buffer))))))

;;;;; bibtex keys

(declare-function tlon-db-get-translation-key "tlon-db")
(defun tlon-get-counterpart-key (key &optional language)
  "Return the bibliography key that is the counterpart of KEY.
If KEY belongs to a translation entry, return the original key stored in its
\"translation\" field. Otherwise treat KEY as an original key and, when LANGUAGE
is non-nil, return the translation key for LANGUAGE via a direct database
lookup. When no counterpart exists, return nil."
  (or
   ;; translation → original
   (tlon-bibliography-lookup "=key=" key "translation")
   ;; original → translation
   (when language
     (condition-case _err
         (tlon-db-get-translation-key key language)
       (error nil)))))

;;;; Reports

;;;###autoload
(defun tlon-counterpart-report-missing ()
  "Report files missing counterparts between two languages for an entity type.
Prompt for an entity type (articles, authors or tags) and two languages, and
list files of that type in each language that lack a counterpart in the other
language within the current subproject."
  (interactive)
  (let* ((type (tlon-counterpart--prompt-entity-type))
         (lang-a (tlon-counterpart--select-language-code "First language code: "))
         (lang-b (tlon-counterpart--select-language-code "Second language code: ")))
    (when (string= lang-a lang-b)
      (user-error "Languages must be different"))
    (let* ((repo-a (tlon-repo-lookup :dir :subproject "uqbar" :language lang-a))
           (repo-b (tlon-repo-lookup :dir :subproject "uqbar" :language lang-b)))
      (unless (and repo-a repo-b)
        (user-error "Could not resolve repositories for %s and %s" lang-a lang-b))
      (let* ((files-a (tlon-counterpart--files-of-type-in-repo repo-a type))
             (files-b (tlon-counterpart--files-of-type-in-repo repo-b type))
             (missing-in-b (seq-filter
                            (lambda (f)
                              (null (tlon-counterpart--counterpart-exists-p f lang-b)))
                            files-a))
             (missing-in-a (seq-filter
                            (lambda (f)
                              (null (tlon-counterpart--counterpart-exists-p f lang-a)))
                            files-b)))
        (tlon-counterpart--display-missing-report
         type lang-a lang-b repo-a repo-b missing-in-b missing-in-a)))))

(defun tlon-counterpart--list-md-nonrecursive (dir)
  "Return a list of markdown files directly under DIR."
  (directory-files dir t "\\.md\\'"))

(defun tlon-counterpart--find-original-for-translation (file)
  "Return absolute path to original counterpart of translation FILE, or nil."
  (let* ((dir (tlon-get-counterpart-dir file "en"))
         (op (tlon-yaml-get-key "original_path" file)))
    (cond
     ((and dir op)
      (let ((path (expand-file-name op dir)))
        (when (file-exists-p path) path)))
     (dir
      (let* ((tr-key (tlon-yaml-get-key "key" file))
             (orig-key (and tr-key (tlon-get-counterpart-key tr-key))))
        (when orig-key
          (seq-find (lambda (f) (string= orig-key (tlon-yaml-get-key "key" f)))
                    (tlon-counterpart--list-md-nonrecursive dir))))))))

(defun tlon-counterpart--counterpart-exists-p (file target-language-code)
  "Return non-nil if FILE has a counterpart in TARGET-LANGUAGE-CODE."
  (let* ((repo (tlon-get-repo-from-file file))
         (src-lang (tlon-repo-lookup :language :dir repo))
         (subtype (tlon-repo-lookup :subtype :dir repo)))
    (cond
     ((string= src-lang target-language-code) t)
     ((eq subtype 'originals)
      (not (null (tlon-get-counterpart-in-originals file target-language-code))))
     ((eq subtype 'translations)
      (if (string= target-language-code "en")
          (not (null (tlon-get-counterpart-in-translations file)))
        (when-let* ((orig (tlon-get-counterpart-in-translations file)))
          (not (null (tlon-get-counterpart-in-originals orig target-language-code))))))
     (t nil))))

(defun tlon-counterpart--prompt-entity-type ()
  "Prompt for an entity type and return the YAML type string."
  (let* ((choices '(("articles" . "article")
                    ("authors"  . "author")
                    ("tags"     . "tag")))
         (sel (completing-read "Entity type: " (mapcar #'car choices) nil t)))
    (cdr (assoc sel choices))))

(defun tlon-counterpart--select-language-code (prompt)
  "Prompt with PROMPT for a language code using project language data."
  (tlon-select-language 'code 'babel prompt t))

(defun tlon-counterpart--files-of-type-in-repo (repo-dir yaml-type)
  "Return a list of markdown files in REPO-DIR whose YAML type is YAML-TYPE.

Scan only the repository root and its immediate subdirectories (non-recursive)."
  (let* ((top-level (directory-files repo-dir t "\\.md\\'"))
         (subdirs (seq-filter
                   (lambda (p)
                     (and (file-directory-p p)
                          (not (string-prefix-p "." (file-name-nondirectory p)))))
                   (directory-files repo-dir t "\\`[^.]")))
         (in-subdirs (apply #'append
                            (mapcar (lambda (d) (directory-files d t "\\.md\\'"))
                                    subdirs)))
         (candidates (append top-level in-subdirs)))
    (seq-filter (lambda (file)
                  (and (string-suffix-p ".md" file t)
                       (string= yaml-type (tlon-yaml-get-type file))))
                candidates)))

(defun tlon-counterpart--counterpart-in-language (file target-language-code)
  "Return counterpart of FILE in TARGET-LANGUAGE-CODE, or nil if missing."
  (let* ((repo (tlon-get-repo-from-file file))
         (src-lang (tlon-repo-lookup :language :dir repo))
         (subtype (tlon-repo-lookup :subtype :dir repo)))
    (cond
     ((string= src-lang target-language-code) file)
     ((eq subtype 'originals)
      (tlon-get-counterpart-in-originals file target-language-code))
     ((eq subtype 'translations)
      (if (string= target-language-code "en")
          (tlon-get-counterpart-in-translations file)
        (when-let* ((orig (tlon-get-counterpart-in-translations file)))
          (tlon-get-counterpart-in-originals orig target-language-code))))
     (t nil))))

(defun tlon-counterpart--display-missing-report
    (yaml-type lang-a lang-b repo-a repo-b missing-in-b missing-in-a)
  "Display a report buffer for missing counterparts.

YAML-TYPE, LANG-A, LANG-B identify the query. REPO-A and REPO-B are the
repository roots for each language. MISSING-IN-B lists files in REPO-A with no
counterpart in LANG-B, and MISSING-IN-A lists files in REPO-B with no
counterpart in LANG-A."
  (let* ((buf-name (format "*Counterparts Missing: %s %s↔%s*"
                           yaml-type lang-a lang-b))
         (in-a (mapcar (lambda (f) (file-relative-name f repo-a)) missing-in-b))
         (in-b (mapcar (lambda (f) (file-relative-name f repo-b)) missing-in-a)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Missing counterparts for type \"%s\" between %s and %s\n\n"
                        yaml-type lang-a lang-b))
        (insert (format "- In %s (missing in %s): %d\n"
                        lang-a lang-b (length in-a)))
        (dolist (p in-a) (insert "  • " p "\n"))
        (insert "\n")
        (insert (format "- In %s (missing in %s): %d\n"
                        lang-b lang-a (length in-b)))
        (dolist (p in-b) (insert "  • " p "\n"))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf-name)))

;;;;; Menu

;;;###autoload (autoload 'tlon-counterpart-menu "tlon-counterpart" nil t)
(transient-define-prefix tlon-counterpart-menu ()
  "`tlon-counterpart' menu."
  [["Navigation"
    ("u" "visit counterpart"                     tlon-open-counterpart-dwim)
    ("H-u" "visit counterpart other window"      tlon-open-counterpart-in-other-window-dwim)
    ("U" "open counterpart in Dired"             tlon-open-counterpart-in-dired)]
   ["Links"
    ("l" "translate relative links"                tlon-translate-relative-links)
    ("L" "translate relative links (marked files)" tlon-translate-relative-links-in-dired)]
   ["Metadata"
    ("o" "set ‘original_path’"                   tlon-yaml-insert-original-path)]
   ["Report missing"
    ("r" "report missing counterparts"           tlon-counterpart-report-missing)]])

(provide 'tlon-counterpart)
;;; tlon-counterpart.el ends here
