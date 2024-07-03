;;; tlon.el --- A companion package for the Babel project. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 1.4.10
;; URL: https://github.com/tlon-team/tlon
;; Keywords: convenience tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(require 'org-capture)
(require 'paths)
(require 'tlon-core)
(require 'tlon-tex)
(require 'transient)

;;;; Customization:

;;;; User options

(defgroup tlon ()
  "A companion package for the Babel project."
  :group 'files)

;;;; Variables

;;;;; Files and dirs

(defvar tlon-todos-jobs-file nil
  "Org file that contains the ID in `paths-tlon-todos-jobs-id'.
This variable should not be set manually.")

(defvar tlon-todos-generic-file nil
  "Org file that contains the ID in `paths-tlon-todos-generic-id'.
This variable should not be set manually.")

(defvar tlon-post-init-hook nil
  "Hook run at the end of `tlon-initialize'.")

;;;;;; lookup

;; TODO: all this should be adapted following the model of
;; `tlon-get-file-glossary' i.e. a repo-relative path stored in a variable
;; and a function to get it for specific languages

(defvar tlon-dir-correspondences
  (file-name-concat (tlon-repo-lookup :dir :name "babel-es") "correspondences/")
  "Directory where correspondence files are stored.")

(defvar tlon-dir-dict
  (file-name-concat (tlon-repo-lookup :dir :name "babel-es") "dict/")
  "Directory where dictionary files are stored.")

(defvar tlon-file-hyphenation
  "hyphenation.json"
  "File containing hyphenation rules.")

(defvar tlon-file-babel-manual
  (file-name-concat (tlon-repo-lookup :dir :name "babel-core") "manual.org")
  "File containing the Babel manual.")

(defvar tlon-file-url-correspondences
  (file-name-concat tlon-dir-correspondences "url-correspondences.json")
  "File containing the URL correspondences.")

(defvar tlon-file-section-correspondences
  (file-name-concat tlon-dir-correspondences "section-correspondences.json")
  "File containing the section correspondences.")

(defvar tlon-file-bibtex-correspondences
  (file-name-concat tlon-dir-correspondences "bibtex-correspondences.json")
  "File containing the BibTeX correspondences.")

(defmacro tlon-create-file-opening-command (file)
  "Create a command to open FILE."
  (let* ((file-base (downcase (file-name-base file)))
	 (file-name (file-name-nondirectory file))
	 (command-name (intern (concat "tlon-open-" file-base))))
    `(defun ,command-name ()
       ,(format "Open `%s' file." file-name)
       (interactive)
       (find-file (symbol-value (intern (concat "tlon-file-" ,file)))))))

(tlon-create-file-opening-command "manual")
(tlon-create-file-opening-command "readme")
(tlon-create-file-opening-command "jobs")
(tlon-create-file-opening-command "fluid")
(tlon-create-file-opening-command "stable")
(tlon-create-file-opening-command "url-correspondences")
(tlon-create-file-opening-command "section-correspondences")
(tlon-create-file-opening-command "bibtex-correspondences")

;;;;; URLs

(defconst tlon-github-url
  "https://github.com/tlon-team/"
  "URL of the Tlön account on GitHub.")

;;;; Functions

;;;;; init

(defun tlon-initialize ()
  "Initialize `tlon'."
  (interactive)
  (dolist (var '(paths-tlon-todos-jobs-id paths-tlon-todos-generic-id))
    (tlon-warn-if-unbound var))
  (setq paths-files-bibliography-all
	`(,@paths-files-bibliography-personal
	  ,@tlon-bibliography-files))
  (run-hooks 'tlon-post-init-hook)
  (message "Initialized `tlon'."))

(defvar org-capture-templates)
(dolist (template `(("tbJ" "Tlön: Babel: Create a new Babel job" entry
		     (id ,paths-tlon-todos-jobs-id)
		     "** %c" :immediate-finish t :empty-lines 1 :jump-to-captured t)
		    ("tbG" "Tlön: Babel: Create new generic todo from GitHub" entry
		     (id ,paths-tlon-todos-generic-id)
		     "** %c" :immediate-finish t :empty-lines 1 :prepend t :jump-to-captured t)))
  (push template org-capture-templates))

(defun tlon-warn-if-unbound (var)
  "Signal an error if the value of VAR is not set."
  (unless (symbol-value var)
    (user-error "Please set the value of `%s'" (symbol-name var))))

;;;;; [name]

(declare-function tlon-metadata-in-repos "tlon-yaml")
(defun tlon-get-file-from-key (key)
  "Return the file path of KEY."
  (if-let ((file (tlon-metadata-lookup
		  (tlon-metadata-in-repos :subtype 'translations) "file" "original_key" key)))
      file
    (user-error "Metadata lookup for key `%s' returned nil" key)))

(declare-function tlon-metadata-in-repo "tlon-yaml")
(declare-function tlon-get-counterpart "tlon-counterpart")
(declare-function tlon-yaml-get-key "tlon-yaml")
(defun tlon-get-key-from-file (file)
  "Return the bibtex key of FILE."
  (or
   ;; when in `translations'
   (tlon-metadata-lookup (tlon-metadata-in-repo) "translation_key" "file" file)
   ;; when file in `originals'
   (let ((translation (tlon-get-counterpart file)))
     (tlon-yaml-get-key "original_key" translation))))

(defun tlon-set-translation-language (language)
  "Set the translation LANGUAGE."
  (interactive (list (tlon-select-language 'code 'babel)))
  (setq tlon-translation-language language))

;;;;; User commits

(defun tlon-latest-user-commit-in-file (&optional file)
  "Return latest commit by the current user in FILE.
If no FILE is provided, use the file visited by the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (default-directory (file-name-directory file))
	 (user (tlon-user-lookup :git :name user-full-name))
	 ;; get most recent commit in FILE by USER
	 (output (shell-command-to-string (format "git log --pretty=format:'%%h %%an %%s' --follow -- '%s' | grep -m 1 '%s' | awk '{print $1}'" file user)))
	 (commit (car (split-string output "\n"))))
    commit))

(declare-function magit-diff-range "magit-diff")
(defun tlon-log-buffer-latest-user-commit (&optional file)
  "Show modifications to FILE since the latest commit by the current user.
If no FILE is provided, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-latest-user-commit-in-file file)))
    (magit-diff-range commit nil (list file))))

(defun tlon-log-buffer-latest-user-commit-ediff (&optional file)
  "Run `ediff' session for FILE and its state when last committed by current user.
If FILE is not provided, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-latest-user-commit-in-file file))
	 (commit-file (tlon-create-file-from-commit file commit)))
    (ediff-files commit-file file)))

;;;;;; Metadata

(declare-function tlon-ensure-markdown-mode "tlon-md")
(defun tlon-get-key-in-buffer ()
  "Get the BibTeX key in the current Markdown buffer."
  (tlon-ensure-markdown-mode)
  (save-buffer)
  (let ((key (tlon-yaml-get-key "original_key")))
    (unless key
      (user-error "No key found"))
    key))

;;;;; Magit/Forge

(declare-function magit-status-setup-buffer "magit-status")
(defun tlon-magit-status ()
  "Show the status of the current repository in a buffer."
  (interactive)
  (let ((default-directory (tlon-get-repo nil 'include-all)))
    (magit-status-setup-buffer)))

(defun tlon-magit-prompt (repo)
  "Prompt the user for a REPO and show it in Magit."
  (interactive (list
		(completing-read
		 "Repo: " (tlon-repo-lookup-all :name))))
  (if-let ((default-directory (tlon-repo-lookup :dir :name repo)))
      (magit-status-setup-buffer)
    (user-error "Repo `%s' not found" repo)))

(declare-function files-extras-buffer-file-name "file-extras")
(declare-function magit-extras-get-commit-file "magit-extra")
(defun tlon-get-commit-key ()
  "Get key of commit file."
  (let* ((dir (file-name-directory (files-extras-buffer-file-name)))
	 (file (magit-extras-get-commit-file))
	 (path (file-name-concat (replace-regexp-in-string "\\.git/" "" dir) file)))
    (tlon-get-key-from-file path)))

;;;;; Checks

(declare-function magit-get-current-branch "magit-git")
(defun tlon-check-branch (branch repo)
  "Throw an error unless current buffer is in REPO branch BRANCH."
  (let ((default-directory repo))
    (unless (string= (magit-get-current-branch) branch)
      (user-error "Please switch to the branch `%s' before proceeding" branch)
      t)))

(declare-function tlon-get-clock-key "tlon-clock")
(defun tlon-check-file (&optional original)
  "Throw an error unless current file matches file in clock.
If ORIGINAL is non-nil, check that current file matches original; otherwise,
check that current file matches translation."
  (let* ((key (tlon-get-clock-key))
	 (field (if original "original_path" "file"))
	 (expected-file (file-name-nondirectory
			 (tlon-metadata-lookup (tlon-metadata-in-repo) field "original_key" key)))
	 (actual-file (file-name-nondirectory
		       (buffer-file-name))))
    (if (string= expected-file actual-file)
	t
      (user-error "Current file does not match file in clock"))))

;; TODO: the two functions below appear to check for the same thing, except that
;; one ignores changes to the file itself. They should be merged into a single
;; function.
(defun tlon-check-staged-or-unstaged (file)
  "Check if there are staged or unstaged modifications in repo involving FILE."
  (catch 'found
    (dolist (flag '("staged" ""))
      (let ((git-command (format "git diff --%s --name-only %s" flag file)))
	(when (not (string-empty-p (shell-command-to-string git-command)))
	  (throw 'found t))))))

(declare-function magit-git-str "magit-git")
(defun tlon-ensure-no-uncommitted-changes (file)
  "Throw an error if there are uncommitted modifications in repo of FILE.
FILE is excluded from the check."
  (let* ((default-directory (tlon-get-repo-from-file file))
	 (all-changes (magit-git-str "diff" "HEAD" "--" "."))
	 (filtered-changes (magit-git-str "diff" "HEAD" "--" file)))
    (unless (string= all-changes filtered-changes)
      (let ((repo-name (tlon-repo-lookup :name :dir (tlon-get-repo-from-file file))))
	(user-error "There are uncommitted changes in repo `%s'" repo-name)))))

(declare-function simple-extras-slugify "simple-extras")
(defun tlon-check-file-title-match  (&optional file)
  "Check that FILE matches its title.
If FILE is nil, check the current buffer."
  (when-let* ((file (or file (buffer-file-name)))
	      (base (file-name-base file))
	      (title (tlon-yaml-get-key "title" file))
	      (slugified-title (simple-extras-slugify title)))
    (unless (or
	     (string= base slugified-title)
	     ;; for articles with duplicate titles
	     (string-match-p (concat "^" (regexp-quote slugified-title) "-[0-9]+$") base))
      (error "The file `%s' does not match its title" title))))

(defun tlon-check-file-type-match (&optional file)
  "Check that FILE matches its tile.
Return an error if the file is not in a subdirectory of the repository whose
name, or its translation, is the value of the file’s `type' metadata field. For
example, if the FILE is of type `article', the function will throw an error if
FILE is located in `uqbar-es/temas/FILE' or `uqbar-es/imagenes/articulos/FILE',
but will not throw an error if it is located in `uqbar-en/articles/FILE' or
`uqbar-es/articulos/FILE'."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-get-repo-from-file file))
	 (lang (tlon-repo-lookup :language :dir repo))
	 (dir-raw (file-name-directory (file-relative-name file repo)))
	 (dir-lang (tlon-get-bare-dir-translation
		    "en" lang (directory-file-name dir-raw)))
	 (type (tlon-yaml-get-key "type" file)))
    (unless (string-match type dir-lang) ; we use `string-match' instead of `string=' to handle plurals
      (user-error "The file `%s' does not match its type" file))))

(defvar tlon-md-image-sans-alt)
(defun tlon-check-image-alt-text ()
  "Check if all images have alt text, else signal an error."
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward tlon-md-image-sans-alt nil t)
	(setq pos (match-beginning 0))))
    (when pos
      (goto-char pos)
      (user-error "There are images without alt text. Use `tlon-md-insert-alt-text' to fix them"))))

;;;;; Search

(declare-function magit-log-all "magit-log")
(defun tlon-search-commits (search-string &optional repo)
  "Search for SEARCH-STRING in REPO's commit history.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let ((default-directory (or repo default-directory)))
    (magit-log-all (list "--grep" search-string))))

(defun tlon-search-commit-diffs (search-string &optional repo)
  "Search for SEARCH-STRING in REPO's commit diff history.
If REPO is nil, use the current repo."
  (interactive "sSearch commit diffs : ")
  (let ((default-directory (or repo default-directory)))
    (magit-log-all `("-S" ,search-string))))

(declare-function consult-ripgrep "consult")
(defun tlon-search-files (search-string &optional repo)
  "Search for SEARCH-STRING in REPO files.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let ((repo (or repo (tlon-get-repo nil 'include-all))))
    (consult-ripgrep repo search-string)))

(declare-function magit-stage-file "magit")
(declare-function magit-pull-from-upstream "magit-pull")
(declare-function magit-push-current-to-pushremote "magit-push")
(defun tlon-commit-and-push (action file)
  "Commit and push modifications to FILE.
The commit message is ACTION followed by the name of FILE."
  (let* ((default-directory (tlon-get-repo-from-file file)))
    (tlon-ensure-no-uncommitted-changes file)
    (when (string= (magit-get-current-branch) "main")
      ;; TODO: Replace interactive call with programmatic way of doing this
      (call-interactively #'magit-pull-from-upstream nil)
      (sleep-for 2))
    (magit-stage-file file)
    (tlon-create-commit action file)
    (call-interactively #'magit-push-current-to-pushremote)))

(declare-function magit-commit-create "magit-commit")
(defun tlon-create-commit (action file)
  "Create a commit modifications to FILE.
The commit message is ACTION followed by either FILE or its BibTeX key,
depending on whether the repo is of subtype `translations' or `biblio',
respectively."
  ;; we check for staged or unstaged changes to FILE because
  ;; `magit-commit-create' interrupts the process if there aren't any
  (when (tlon-check-staged-or-unstaged file)
    (let* ((repo (tlon-get-repo-from-file file))
	   (subtype (tlon-repo-lookup :subtype :dir repo))
	   (file-or-key (pcase subtype
			  ('translations (tlon-get-key-from-file file))
			  ('biblio (file-name-nondirectory file)))))
      (magit-commit-create (list "-m" (format "%s %s" action file-or-key))))))

;;;;; URL correspondences

(defun tlon-url-correspondence-dwim ()
  "Add a new URL correspondence or modify an existing one."
  (interactive)
  (let* ((data (tlon-read-json tlon-file-url-correspondences 'hash-table 'vector 'symbol))
	 (keys (tlon-get-keys data))
	 (selected-key (completing-read "Select existing URL or enter a new one: " keys))
	 (default-value (gethash selected-key data))
	 (new-value (read-string (format "Enter value for key '%s': " selected-key) default-value)))
    (puthash selected-key new-value data)
    (with-temp-file tlon-file-url-correspondences
      (insert "{\n")
      (maphash (lambda (k v)
		 (insert (format "  \"%s\": \"%s\",\n" k v)))
	       data)
      ;; Remove last comma
      (goto-char (- (point) 2))
      (delete-char 1)
      (insert "\n}")
      (write-file tlon-file-url-correspondences)
      (tlon-url-correspondence-commit))))

(declare-function magit-staged-files "magit-git")
(declare-function magit-save-repository-buffers "magit-mode")
(declare-function magit-run-git "magit-process")
(defvar magit-commit-ask-to-stage)
;; TODO: consider adapting `tlon-commit-and-push' instead
(defun tlon-url-correspondence-commit ()
  "Commit modifications in `url-correspondences.json'."
  (let ((default-directory (tlon-repo-lookup :dir :name "babel-es")))
    ;; save all unsaved files in repo
    (magit-save-repository-buffers)
    (call-interactively #'magit-pull-from-upstream nil)
    ;; if there are staged files, we do not commit or push the changes
    (unless (magit-staged-files)
      (tlon-check-branch "main" default-directory)
      (magit-run-git "add" tlon-file-url-correspondences)
      (let ((magit-commit-ask-to-stage nil))
	(magit-commit-create (list "-m" "Update URL correspondences"))))))

(defun tlon-highlight-url-correspondences ()
  "Highlight source URLs in URL correspondences file."
  (interactive)
  ;; Load JSON file
  (let* ((json-data (tlon-read-json tlon-file-url-correspondences 'hash-table 'vector 'symbol))
	 (key-urls (tlon-get-keys json-data))
	 ;; Remove URL prefixes from keys
	 (search-keywords (mapcar (lambda (url)
				    (replace-regexp-in-string "^https?://\\(www\\.\\)?" "" url))
				  key-urls))
	 ;; Build list of keys
	 (keywords-regex (regexp-opt search-keywords 'words))
	 ;; Specify a custom face for highlighting
	 (highlight-face '(:background "#D3FFD2")))

    ;; Remove the previous highlighting
    (with-silent-modifications
      (remove-list-of-text-properties (point-min) (point-max) '(font-lock-face))

      ;; Highlight each occurrence of a key from the JSON file
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward keywords-regex nil t)
	  (add-text-properties (match-beginning 0) (match-end 0)
			       `(font-lock-face ,highlight-face)))))))

;;;;; section correspondences

(declare-function citar-select-refs "citar")
(defun tlon-section-correspondence-dwim ()
  "Add a new section correspondence or modify an existing one."
  (interactive)
  (let* ((data (tlon-read-json tlon-file-section-correspondences 'hash-table 'list 'symbol))
	 (selected-key (citar-select-refs)))
    (tlon-section-correspondence-check selected-key)
    (let ((default-value (gethash selected-key data))
	  (new-sectionOriginal (read-string (format "Enter value for key '%s', sectionOriginal: " selected-key)))
	  (new-sectionSpanish (read-string (format "Enter value for key '%s', sectionSpanish: " selected-key)))
	  (new-value (make-hash-table)))
      (puthash "sectionOriginal" new-sectionOriginal new-value)
      (puthash "sectionSpanish" new-sectionSpanish new-value)
      (puthash selected-key
	       (if default-value
		   (append default-value (list new-value))
		 (list new-value))
	       data)
      (with-temp-file tlon-file-section-correspondences
	(insert "{\n")
	(maphash (lambda (k v)
		   (insert (format "  \"%s\": [\n" k))
		   (dolist (item v)
		     (insert (format "    %s,\n" (json-encode item))))
		   (delete-char -2)
		   (insert "\n  ],\n"))
		 data)
	;; Remove last comma
	(goto-char (- (point) 2))
	(delete-char 1)
	(insert "\n}")
	(json-pretty-print (point-min) (point-max))
	(write-file tlon-file-section-correspondences)
	;; (tlon-url-correspondence-commit)
	))))

(declare-function ebib-extras-open-key "ebib-extras")
(declare-function ebib-extras-get-field "ebib-get-field")
(defun tlon-section-correspondence-check (key)
  "Check that selected BibTeX KEY is associated with the original work."
  (save-window-excursion
    (ebib-extras-open-key key)
    (let ((langid (ebib-extras-get-field "langid")))
      (unless (member langid '("english" "american"))
	(unless (y-or-n-p "The BibTeX entry you selected is not in English. In the `section-correspondences.json' file, you should use the BibTeX entry associated with the original work rather than with its translation. Are you sure you want to proceed?")
	  (user-error "Aborted"))))))

;;;;; hyphenation

;; TODO: the function that adds words to the json file should also add the
;; hyphenated variant to `jinx-local-words'

;;;;; open file commands

(declare-function files-extras-read-file "files-extras")
(defun tlon-browse-file (&optional file)
  "Browse Tlön FILE externally."
  (interactive)
  (let* ((file (files-extras-read-file file))
	 (url (tlon-path-to-url file)))
    (browse-url url)))

(defun tlon-find-file-in-repo (&optional repo)
  "Interactively open a file from a list of all files in REPO.
If REPO is nil, default to the current repository."
  (interactive)
  (let* ((repo (or repo (tlon-get-repo 'error)))
	 (alist (tlon-files-and-display-names-alist (list repo) repo)))
    (tlon-open-file-in-alist alist)))

(declare-function ffap-url-p "ffap")
(declare-function eww-current-url "eww")
(defun tlon-find-file-of-url (&optional url)
  "Open the file corresponding to URL."
  (interactive)
  (let ((url (or url (read-string "URL: " (or (thing-at-point 'url)
					      (eww-current-url)
					      (ffap-url-p (current-kill 0)))))))
    (find-file (tlon-url-to-path url))))

(defun tlon-open-file-across-repos ()
  "Interactively open a file from a list of all files in all repos."
  (interactive)
  (let* ((alist (tlon-files-and-display-names-alist
		 (tlon-repo-lookup-all :dir)
		 paths-dir-tlon-repos)))
    (tlon-open-file-in-alist alist)))

(defun tlon-files-and-display-names-alist (dirs relative-path)
  "Return a alist of files, and display names, in DIRS.
The display names are constructed by subtracting the RELATIVE-PATH."
  (mapcar (lambda (file)
	    (cons (file-relative-name file relative-path) file))
	  (apply 'append (mapcar
			  (lambda (dir)
			    (directory-files-recursively dir "^[^.][^/]*$"))
			  dirs))))

(defun tlon-open-file-in-alist (alist)
  "Interactively open a file from ALIST.
The car in each cons cell is the file to open, and its cdr is the candidate
presented to the user."
  (let* ((selection (completing-read "Select file: " alist))
	 (file (cdr (assoc selection alist))))
    (find-file file)))

;;;;;; path <> url conversion

(defun tlon-path-to-url (path)
  "Convert a Tlön file PATH to its URL."
  (let* ((dir (tlon-get-repo))
	 (relative (file-relative-name path dir))
	 (sans-extension (file-name-sans-extension relative))
	 (root (tlon-repo-lookup :url :dir dir)))
    (concat root sans-extension)))

(defun tlon-url-to-path (url)
  "Convert a Tlön URL to its file path."
  (let* ((root (tlon-get-url-root url))
	 (rest (substring url (length root)))
	 (dir (tlon-repo-lookup :dir :url root))
	 ;; TODO: support non-markdown files (e.g. images)
	 (with-extension (file-name-with-extension rest "md")))
    (file-name-concat dir with-extension)))

(defun tlon-get-url-root (url)
  "Extract the root of the URL."
  (let* ((parsed-url (url-generic-parse-url url))
         (scheme (url-type parsed-url))
         (host (url-host parsed-url)))
    (concat scheme "://" host "/")))

;;;;; Fix log errors helper functions

(defvar tlon-mdx-cite-pattern)
(defun tlon-collect-bibtex-keys-in-buffer ()
  "Collect all the bibtex keys in the current buffer.
Display the collected keys in a new buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (keys)
      (while (re-search-forward tlon-mdx-cite-pattern nil t)
	(push (match-string 1) keys))
      (with-output-to-temp-buffer "*Bibtex keys*"
	(princ (mapconcat #'identity (delete-dups keys) "\n"))))))

(defun tlon-move-bibtex-entries-to-babel ()
  "Move the bibtex entries in the current buffer from `old.bib' to `fluid.bib'.
If the key is not found, it is added to the list of missing keys."
  (interactive)
  (let ((missing-keys '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((bibtex-key (buffer-substring-no-properties
			   (line-beginning-position)
			   (line-end-position))))
	  (save-excursion
	    (with-current-buffer (find-file-noselect paths-file-personal-bibliography-old)
	      (goto-char (point-min))
	      (if (re-search-forward (format "{%s," bibtex-key) nil t)
		  (call-interactively 'bibtex-extras-move-entry-to-tlon)
		(push 'missing-keys bibtex-key))))
	  (forward-line)))
      (with-output-to-temp-buffer "*Missing BibTeX Keys*"
	(dolist (key missing-keys)
	  (princ (format "%s\n" key)))))))

;; TODO: Move to appropriate section
(defun tlon-create-file-from-commit (file commit-hash)
  "Create a temporary file with the state of the FILE at the COMMIT-HASH.
Return the path of the temporary file created."
  (let* ((file-name (file-name-nondirectory file))
	 (repo-root (locate-dominating-file file ".git"))
	 (relative-file (file-relative-name file repo-root))
	 (new-file-name (format "%s_%s" commit-hash file-name))
	 (new-file (make-temp-file new-file-name nil ".md"))
	 (git-command
	  (format "git show %s:\"%s\""
		  commit-hash
		  (shell-quote-argument relative-file))))
    (let ((default-directory repo-root))
      (with-temp-buffer
	(insert (shell-command-to-string git-command))
	(write-file new-file)))
    (message "File created: %s" new-file)
    new-file))

(declare-function goldendict-ng-search-string "goldendict-ng")
;; TODO: move to relevant section
(defun tlon-search-for-translation (string)
  "Search for a Spanish translation of English STRING."
  (interactive "sString to translate: ")
  (let ((urls '("https://spanish.stackexchange.com/search?q=%s"
		"https://es.bab.la/diccionario/ingles-espanol/%s"
		"https://en.wikipedia.org/w/index.php?fulltext=Search&profile=default&search=%s"
		"https://context.reverso.net/traduccion/ingles-espanol/%s"
		"https://www.linguee.com/english-spanish/search?query=%s")))
    (dolist (url urls)
      (browse-url (format url (url-hexify-string string)) 'new-buffer)))
  (goldendict-ng-search-string string))

(provide 'tlon)
;;; tlon.el ends here
