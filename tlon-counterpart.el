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

Read FILE’s ‘key’, resolve the original key, and locate in the
mirrored originals directory the Markdown file whose ‘key’ matches
that value. If a unique match is found, return its absolute path.
If there are zero or multiple candidates, return nil without
prompting."
  (let* ((dir (tlon-get-counterpart-dir file))
	 (trans-key (tlon-yaml-get-key "key" file))
	 (orig-key  (when trans-key
		      (tlon-get-counterpart-key trans-key)))
	 (candidates (when dir
		       (seq-filter (lambda (f) (string-suffix-p ".md" f t))
				   (directory-files dir t "\\`[^.]")))))
    ;; First try the fast key-based narrowing
    (when (and orig-key candidates)
      (setq candidates
            (seq-filter (lambda (f)
                          (string= orig-key (tlon-yaml-get-key "key" f)))
                        candidates)))
    ;; Fallback for entities (tags/authors) that lack keys: rely on
    ;; their `original_path' metadata.  We run this when either the
    ;; translation has no key at all *or* the key-based narrowing
    ;; produced no unique hit.
    (when (or (null orig-key) (null candidates))
      (let ((op (tlon-yaml-get-key "original_path" file)))
        (when (and op dir)
          (setq candidates (list (expand-file-name op dir))))))
    (pcase candidates
      ((pred null) nil)
      ((pred (lambda (c) (= (length c) 1))) (car candidates))
      (_ nil))))

(defun tlon-get-counterpart-in-originals (file &optional target-language-code)
  "Return the translation counterpart of original FILE.

TARGET-LANGUAGE-CODE is the target translation language code. If nil,
the user may be prompted to select a language.

If a unique counterpart is found, return its absolute path. If none or
multiple candidates are found, return nil without prompting.

This implementation maintains a per-repository cache so that, after the first
lookup, subsequent queries are instantaneous."
  (let* ((target-language-code (or target-language-code (tlon-select-language 'code 'babel)))
	 (dir      (tlon-get-counterpart-dir file target-language-code))
	 (repo     (and dir (tlon-get-repo-from-dir dir)))
	 (orig-key (tlon-yaml-get-key "key" file)))
    (unless (and dir repo)
      (user-error "Unable to determine translation repo for %s" file))
    (let* ((target-language (tlon-lookup tlon-languages-properties
					 :standard :code target-language-code))
	   (table (and orig-key
                       (tlon-counterpart--translation-table-for-repo repo target-language)))
	   (hit   (and orig-key table (gethash orig-key table))))
      (or hit
	  (let* ((candidates (seq-filter
			      (lambda (f) (string-suffix-p ".md" f t))
			      (directory-files dir t "\\`[^.]"))))
	    ;; First, narrow via key+bibliography when possible
            (when (and candidates orig-key)
              (setq candidates
                    (seq-filter
                     (lambda (f)
                       (let ((tr-key (tlon-yaml-get-key "key" f)))
                         (and tr-key
                              (string= orig-key
                                       (tlon-get-counterpart-key tr-key target-language-code)))))
                     candidates)))
            ;; Second pass for entities without keys: match their
            ;; `original_path' against the current original filename.
            (let* ((orig-name (file-name-nondirectory file))
                   (filtered (seq-filter
                              (lambda (f)
                                (string= orig-name
                                         (tlon-yaml-get-key "original_path" f)))
                              candidates)))
              (when filtered
                (setq candidates filtered))
	      (pcase candidates
		((pred null)
		 nil)
		((pred (lambda (c) (= (length c) 1)))
		 (let ((file (car candidates)))
		   (when table
		     (puthash orig-key file table))
		   file))
		(_ nil))))))))

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
	(let ((counterpart-bare-dir (tlon-get-bare-dir-translation final-target-lang source-lang bare-dir)))
	  (when counterpart-bare-dir
	    (file-name-concat final-counterpart-repo counterpart-bare-dir)))
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

(defun tlon-counterpart--translation-table-for-repo (repo-dir target-language)
  "Return a hash table mapping original keys to translation files in REPO-DIR.
The hash table maps original bibliography keys to their corresponding
translation file paths. The table is cached in
`tlon-counterpart--orig->trans-cache' to avoid repeated computation.

For each markdown file in REPO-DIR:
1. Extract the translation key from the file's YAML metadata
2. Look up the corresponding original key in the bibliography
3. Map the original key to the translation file path

REPO-DIR is a directory path containing markdown translation files.
TARGET-LANGUAGE is the language code of the translations in that repository.

Returns a hash table where keys are original bibliography keys (strings)
and values are absolute file paths to translation files."
  (or (gethash repo-dir tlon-counterpart--orig->trans-cache)
      (puthash
       repo-dir
       (let ((table (make-hash-table :test #'equal)))
	 (dolist (file (directory-files-recursively repo-dir "\\.md\\'"))
	   (when-let* ((tr-key (tlon-yaml-get-key "key" file))
		       (orig-key (tlon-get-counterpart-key tr-key target-language)))
	     (puthash orig-key file table)))
	 table)
       tlon-counterpart--orig->trans-cache)))

;;;;;  File lookup helpers

(defun tlon-counterpart--file-for-key (key language)
  "Return path of the file whose YAML ‘key’ is KEY from repo in LANGUAGE.
If no file is found, return nil."
  (when-let* ((repo (tlon-repo-lookup :dir :subproject "uqbar" :language language)))
    (seq-find (lambda (file)
                (string= key (tlon-yaml-get-key "key" file)))
              (directory-files-recursively repo "\\.md\\'"))))

;;;;; Translate links

(defun tlon-get-counterpart-link (relative-link current-buffer-file)
  "Return the counterpart path for RELATIVE-LINK found in CURRENT-BUFFER-FILE.

The function resolves RELATIVE-LINK to an absolute path, asks
`tlon-get-counterpart' for its peer, and converts the result back
to a path relative to the current buffer directory.  Nil is
returned when no counterpart exists."
  (let* ((current-dir (file-name-directory current-buffer-file))
	 (linked-abs (expand-file-name relative-link current-dir))
	 (counterpart (ignore-errors (tlon-get-counterpart linked-abs))))
    (when counterpart
      (file-relative-name counterpart current-dir))))

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

;;;;; bibtex keys

(defun tlon-get-counterpart-key (key &optional language)
  "Return the bibliography key that is the counterpart of KEY.

If KEY belongs to a *translation* entry, return the original key
stored in its ‘translation’ field.  Otherwise treat KEY as an
*original* key and, provided LANGUAGE is non-nil, look for the
corresponding translation in the repository for LANGUAGE and
return its key.  When no counterpart exists, return nil."
  (or
   ;; KEY is a translation → original
   (tlon-bibliography-lookup "=key=" key "translation")
   ;; KEY is an original → translation (only when LANGUAGE provided)
   (when language
     (when-let* ((orig-file (tlon-counterpart--file-for-key key "en"))
		 (tr-file  (tlon-get-counterpart-in-originals orig-file language)))
       (tlon-yaml-get-key "key" tr-file)))))

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
      (let* ((dir (tlon-get-counterpart-dir file target-language-code))
             (orig-key (tlon-yaml-get-key "key" file)))
        (when dir
          (let ((cands (tlon-counterpart--list-md-nonrecursive dir)))
            (or (and orig-key
                     (seq-some
                      (lambda (f)
                        (let ((tr-key (tlon-yaml-get-key "key" f)))
                          (and tr-key (equal orig-key (tlon-get-counterpart-key tr-key)))))
                      cands))
                (let ((orig-name (file-name-nondirectory file)))
                  (seq-some
                   (lambda (f) (equal orig-name (tlon-yaml-get-key "original_path" f)))
                   cands)))))))
     ((eq subtype 'translations)
      (if (string= target-language-code "en")
          (let* ((dir (tlon-get-counterpart-dir file "en"))
                 (tr-key (tlon-yaml-get-key "key" file))
                 (orig-key (and tr-key (tlon-get-counterpart-key tr-key))))
            (when dir
              (if orig-key
                  (seq-some
                   (lambda (f) (equal orig-key (tlon-yaml-get-key "key" f)))
                   (tlon-counterpart--list-md-nonrecursive dir))
                (let ((op (tlon-yaml-get-key "original_path" file)))
                  (and op (file-exists-p (expand-file-name op dir)))))))
        (when-let* ((orig (tlon-counterpart--find-original-for-translation file)))
          (tlon-counterpart--counterpart-exists-p orig target-language-code))))
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
    ("l" "replace internal links"                tlon-replace-internal-links)]
   ["Metadata"
    ("o" "set ‘original_path’"                   tlon-yaml-insert-original-path)]
   ["Report missing"
    ("r" "report missing counterparts"          tlon-counterpart-report-missing)]])

(provide 'tlon-counterpart)
;;; tlon-counterpart.el ends here
