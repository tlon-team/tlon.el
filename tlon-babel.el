;;; tlon-babel.el --- A companion package for the Babel project. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.14
;; Homepage: https://github.com/tlon-team/tlon-babel
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

(require 'tlon-babel-repo)

;;;; Requirements:

(require 'citar)
(require 'consult)
(require 'doi-utils)
(require 'files-extras)
(require 'forge)
(require 'forge-search)
(require 'goldendict-ng)
(require 'magit)
(require 'magit-extra)
(require 'org)
(require 'org-clock)
(require 'org-roam)
(require 'substitute)
(require 'tlon-core)
(require 'unfill)
(require 'window-extras)
(require 'winum)
;; (require 'ebib-extras) ; recursive requirement

;;;; Customization:

;;;; User options

(defgroup tlon-babel ()
  "A companion package for the Babel project."
  :group 'files)

;;;; Variables

;;;;; Files and dirs

(defcustom tlon-babel-todos-generic-id nil
  "ID of the user-specific `org-mode' heading where generic TODOs are stored.
\"Generic\" TODOs are all TODOs except those related to a translation job."
  :type 'string
  :group 'tlon-babel)

(defcustom tlon-babel-todos-jobs-id nil
  "ID of the user-specific `org-mode' heading where job TODOs are stored.
A job TODO is a TODO for a translation job."
  :type 'string
  :group 'tlon-babel)

(defvar tlon-babel-todos-jobs-file nil
  "Org file that contains the ID in `tlon-babel-todos-jobs-id'.
This variable should not be set manually.")

(defvar tlon-babel-todos-generic-file nil
  "Org file that contains the ID in `tlon-babel-todos-generic-id'.
This variable should not be set manually.")



(defconst tlon-babel-labels
  '((:label "Awaiting processing"
	    :action "Process"
	    :assignee "worldsaround")
    (:label "Awaiting translation"
	    :action "Translate"
	    :assignee "")
    (:label "Awaiting revision"
	    :action "Revise"
	    :assignee "worldsaround")
    (:label "Awaiting check"
	    :action "Check"
	    :assignee "worldsaround")
    (:label "Awaiting review"
	    :action "Review"
	    :assignee "benthamite")
    (:label "Published"
	    :action "Publish"
	    :assignee ""))
  "List of labels and associated properties.")

(defvar tlon-babel-users
  '((:name "Pablo Stafforini"
	   :git "Pablo Stafforini"
	   :github "benthamite"
	   :substitute "worldsaround")
    (:name "Federico Stafforini"
	   :git "Federico Stafforini"
	   :github "fstafforini")
    (:name "Leonardo Picón"
	   :git "cartago"
	   :github "worldsaround"
	   :substitute "benthamite"))
  "Property list of users and associated properties.
The special property `:substitute' is used to determine which user should
perform a given phase of the translation process when the designated user is not
the actual user.")

(defconst tlon-babel-bare-dirs
  '((("en" . "articles")
     ("es" . "articulos"))
    (("en" . "tags")
     ("es" . "temas"))
    (("en". "authors")
     ("es" . "autores"))
    (("en" . "collections")
     ("es" . "colecciones")))
  "Alist of bare directories and associated translations.")

(defconst tlon-babel-languages
  '(("en" . "english")
    ("es" . "spanish")
    ("it" . "italian")
    ("fr" . "french")
    ("de" . "german")
    ("pt" . "portuguese"))
  "Alist of languages and associated names.")

(defvar tlon-babel-translation-language "es"
  "The current translation language.")

(defvar tlon-babel-post-init-hook nil
  "Hook run at the end of `tlon-babel-init'.")

;;;;;; lookup



(defun tlon-babel-get-entity-types ()
  "Return a list of entity types."
  (let (collection)
    (dolist (list tlon-babel-bare-dirs)
      (dolist (cons list)
	(when (string= (car cons) "en")
	  (push (cdr cons) collection))))
    collection))

(defmacro tlon-babel-generate-repo-commands (name dir)
  "Generate commands for browsing repo named NAME.
DIR is the directory where the repo is stored."
  `(progn
     (defun ,(intern (format "tlon-babel-dired-browse-%s" name)) ()
       ,(format "Browse the %s repository in Dired." name)
       (interactive)
       (dired ,dir))
     (defun ,(intern (format "tlon-babel-magit-browse-%s" name)) ()
       ,(format "Browse the %s repository in Magit." name)
       (interactive)
       (magit-status ,dir))))

(dolist (repo tlon-babel-repo-props)
  (eval `(tlon-babel-generate-repo-commands
	  ,(plist-get repo :abbrev)
	  ,(plist-get repo :dir))))

(defmacro tlon-babel-generate-dir-commands (name dir entity)
  "Generate commands for browsing ENTITY subdirectory in repo named NAME.
DIR is the directory where the repo is stored."
  `(progn
     (defun ,(intern (format "tlon-babel-dired-browse-%s-dir-in-%s" entity name)) ()
       ,(format "Browse the `%s' directory in the `%s' repository." entity name)
       (interactive)
       (tlon-babel-browse-entity-dir ,entity ,dir))))

(dolist (repo tlon-babel-repo-props)
  (dolist (entity (tlon-babel-get-entity-types))
    (eval `(tlon-babel-generate-dir-commands
	    ,(plist-get repo :abbrev)
	    ,(plist-get repo :dir)
	    ,entity))))

(defun tlon-babel-plist-lookup (list prop &rest props-values)
  "Return the value of PROP in LIST matching one or more PROPS-VALUES pairs.
If multiple matches are found, return the first match."
  (cl-loop for plist in list
	   when (cl-loop for (prop value) on props-values by #'cddr
			 always (equal (plist-get plist prop) value))
	   return (plist-get plist prop)))

(defun tlon-babel-repo-lookup (prop &rest props-values)
  "Return the value of PROP in repos matching one or more PROPS-VALUES pairs."
  (apply #'tlon-babel-plist-lookup tlon-babel-repo-props prop props-values))

(defun tlon-babel-user-lookup (prop &rest props-values)
  "Return the value of PROP in users matching one or more PROPS-VALUES pairs."
  (apply #'tlon-babel-plist-lookup tlon-babel-users prop props-values))

(defun tlon-babel-label-lookup (prop &rest props-values)
  "Return the value of PROP in labels matching one or more PROPS-VALUES pairs.."
  (apply #'tlon-babel-plist-lookup tlon-babel-labels prop props-values))

(defun tlon-babel-get-property-of-repo (prop repo)
  "Return the value of PROP in REPO."
  (tlon-babel-plist-lookup tlon-babel-repo-props prop :dir repo))

(defun tlon-babel-get-property-of-repo-name (prop repo-name)
  "Return the value of PROP in REPO-NAME.
REPO-NAME is named in its abbreviated form, i.e. the value of `:abbrev' rather
than `:name'."
  (tlon-babel-plist-lookup tlon-babel-repo-props prop :abbrev repo-name))

(defun tlon-babel-get-property-of-user (prop user)
  "Return the value of PROP in USER."
  (tlon-babel-plist-lookup tlon-babel-users prop :name user))

(defun tlon-babel-get-property-of-label (prop user)
  "Return the value of PROP in USER."
  (tlon-babel-plist-lookup prop tlon-babel-users :name user))

(defun tlon-babel-get-property-of-plists (prop plist &optional target-prop target-value)
  "Return a list of all PROP values in PLIST.
Optionally, return only the subset of values such that TARGET-PROP matches
TARGET-VALUE."
  (let ((result '()))
    (dolist (plist plist)
      (let* ((value1 (plist-get plist prop #'string=))
	     (target-value-test (when target-prop (plist-get plist target-prop #'string=))))
	(when value1
	  (if target-prop
	      (when (string= target-value target-value-test)
		(setq result (append result (list value1))))
	    (setq result (append result (list value1)))))))
    result))

(defun tlon-babel-get-property-of-repos (prop &optional target-prop target-value)
  "Return a list of all PROP values in `tlon-babel-repo-props'.
Optionally, return only the subset of values such that TARGET-PROP matches
TARGET-VALUE."
  (tlon-babel-get-property-of-plists prop tlon-babel-repo-props target-prop target-value))

(defun tlon-babel-get-property-of-users (prop &optional target-prop target-value)
  "Return a list of all PROP values in PLIST `tlon-babel-users'.
Optionally, return only the subset of values such that TARGET-PROP matches
TARGET-VALUE."
  (tlon-babel-get-property-of-plists prop tlon-babel-users target-prop target-value))

(defun tlon-babel-get-property-of-labels (prop &optional target-prop target-value)
  "Return a list of all PROP values in PLIST `tlon-babel-labels'.
Optionally, return only the subset of values such that TARGET-PROP matches
TARGET-VALUE."
  (tlon-babel-get-property-of-plists prop tlon-babel-labels target-prop target-value))

(defun tlon-babel-get-bare-dir-translation (target-lang source-lang bare-dir)
  "For BARE-DIR in SOURCE-LANG, get its translation into TARGET-LANG."
  (let (result)
    (dolist (outer tlon-babel-bare-dirs result)
      (dolist (inner outer)
	(when (and (equal (cdr inner) bare-dir)
		   (equal (car inner) source-lang))
	  (setq result (cdr (assoc target-lang outer))))))))

(defvar tlon-babel-dir-refs
  (file-name-concat (tlon-babel-get-property-of-repo-name :dir "babel-refs"))
  "Directory where references files are stored.")

(defvar tlon-babel-dir-correspondences
  (file-name-concat (tlon-babel-get-property-of-repo-name :dir "babel-es") "correspondences/")
  "Directory where correspondence files are stored.")

(defvar tlon-babel-dir-dict
  (file-name-concat (tlon-babel-get-property-of-repo-name :dir "babel-es") "dict/")
  "Directory where dictionary files are stored.")

(defvar tlon-babel-dir-bib
  (file-name-concat tlon-babel-dir-refs "bib/")
  "Directory where BibTeX files are stored.")

(defvar tlon-babel-dir-locales
  (file-name-concat tlon-babel-dir-refs "locales/")
  "Directory where CSL locale files are stored.")

(defvar tlon-babel-dir-styles
  (file-name-concat tlon-babel-dir-refs "styles/")
  "Directory where CSL style files are stored.")

(defvar tlon-babel-file-babel-manual
  (file-name-concat (tlon-babel-get-property-of-repo-name :dir "babel-core") "manual.org")
  "File containing the Babel manual.")

(defvar tlon-babel-file-jobs
  (file-name-concat (tlon-babel-get-property-of-repo-name :dir "babel-core") "jobs.org")
  "File containing the jobs.")

(defvar tlon-babel-file-fluid
  (file-name-concat tlon-babel-dir-bib "fluid.bib")
  "File containing the fluid bibliography.")

(defvar tlon-babel-file-stable
  (file-name-concat tlon-babel-dir-bib "stable.bib")
  "File containing the stable bibliography.")

(defvar tlon-babel-bibliography-files
  `(,tlon-babel-file-fluid
    ,tlon-babel-file-stable)
  "List of bibliography files.")

(defvar tlon-babel-file-url-correspondences
  (file-name-concat tlon-babel-dir-correspondences "url-correspondences.json")
  "File containing the URL correspondences.")

(defvar tlon-babel-file-section-correspondences
  (file-name-concat tlon-babel-dir-correspondences "section-correspondences.json")
  "File containing the section correspondences.")

(defvar tlon-babel-file-bibtex-correspondences
  (file-name-concat tlon-babel-dir-correspondences "bibtex-correspondences.json")
  "File containing the BibTeX correspondences.")

(defmacro tlon-babel-create-file-opening-command (file)
  "Create a command to open FILE."
  (let* ((file-base (downcase (file-name-base file)))
	 (file-name (file-name-nondirectory file))
	 (command-name (intern (concat "tlon-babel-open-" file-base))))
    `(defun ,command-name ()
       ,(format "Open `%s' file." file-name)
       (interactive)
       (find-file (symbol-value (intern (concat "tlon-babel-file-" ,file)))))))

(tlon-babel-create-file-opening-command "manual")
(tlon-babel-create-file-opening-command "readme")
(tlon-babel-create-file-opening-command "jobs")
(tlon-babel-create-file-opening-command "fluid")
(tlon-babel-create-file-opening-command "stable")
(tlon-babel-create-file-opening-command "url-correspondences")
(tlon-babel-create-file-opening-command "section-correspondences")
(tlon-babel-create-file-opening-command "bibtex-correspondences")

;;;;; Org-mode ids

;;;;; html import

(defvar tlon-babel-pandoc-convert-from-file
  "pandoc -s '%s' -t markdown -o '%s'"
  "Command to convert from HTML file to Markdown.")

(defvar tlon-babel-pandoc-convert-from-url
  "pandoc -s -r html '%s' -o '%s'"
  "Command to convert from URL to Markdown.")

;;;;; Version

(defvar tlon-babel-version "0.1.13")

;;;;; Import

(defvar tlon-babel-pdf2md
  (file-name-concat paths-dir-source "pdf2md/lib/pdf2md-cli.js")
  "Path to `pdf2md-cli.js' executable.")

(defvar tlon-babel-pdftotext
  (file-name-concat paths-dir-source "xpdf-tools-mac-4.04/bin64/pdftotext")
  "Path to `pdftotext' executable.")

;;;;; Cleanup

(defconst tlon-babel-markdown-eawiki-footnote-source
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\{1,2\\}\\)\\\\\\]\\](#.+?)\\^\\]{#.+? .footnote-reference role=\"doc-noteref\"}"
  "Regexp to match footnotes in the main body.")

(defconst tlon-babel-markdown-eawiki-footnote-source2
  "\\[\\^\\[\\\\\\[\\([[:digit:]]\\)\\\\\\]\\](#.+?)\\^\\]{#\\\\\\\\\\\\\".+?\\\\\\\\\\\\\" .\\\\\\\\\\\\\\\"footnote-reference\\\\\\\\\\\\\" role=\"\\\\\\\\\\\\\"doc-noteref\\\\\\\\\\\\\"\"}"
  "Regexp to match footnotes in the main body.")

(defconst tlon-babel-markdown-eawiki-footnote-target
  "\\([[:digit:]]\\{1,3\\}\\). +?\\[\\[^\\*\\*\\[\\\\^\\](#[[:alnum:]]\\{12,18\\})\\*\\*^\\]\\]{#[[:alnum:]]\\{10,15\\}}

  footnote-content.*?"
  "Regexp to match footnotes in the footnote section.")

(defconst tlon-markdown-eawiki-links
  "\\[\\(.+?\\)\\](\\\\%22\\(.+?\\)\\\\%22)"
  "Regexp to match links.")

;;;;; Clocked heading

(defconst tlon-babel-key-regexp "`\\(.+?\\)\\(\\.md\\)?`"
  "Regular expression for matching bibtex keys in clocked headings.
The second capture group handles the `.md' extension, which we used previously.")

;;;;; Read aloud

(defvar tlon-babel-read-aloud-next-action
  'read-aloud-buf)

;;;;; Sentence highlighting

;; TODO: (1) highlight sentence in target window; (2) diagnose why first
;; two characters in a sentence are matched to the previous sentence;
;; (3) diagnose performance issues, or else disable `post-command-hook'
;; and rely on other triggers; (4) use `lin-blue' as face for highlighting))))
(defvar tlon-babel-sentence-highlight-offset 0
  "Number of sentences to offset the sentence count in the source window.")

(defvar tlon-babel-enable-automatic-highlighting nil
  "Whether to automatically highlight corresponding sentences.")

;;;;; Word count

(defconst tlon-babel-local-variables-line-start
  "<!-- Local Variables: -->"
  "Start of the line that contains file local variables.")

(defconst tlon-babel-local-variables-line-end
  "<!-- End: -->"
  "End of the line that contains file local variables.")

;;;; Functions

;;;;; version

(defun tlon-babel-version ()
  "Return the version of the `tlon-babel' package."
  (interactive)
  (message "`tlon-babel' version %s" tlon-babel-version))

;;;;; init

(defun tlon-babel-init ()
  "Initialize `tlon-babel'."
  (interactive)
  (mapc #'tlon-babel-repo-set-dir tlon-babel-repo-props)
  (tlon-babel-set-value-of-var 'tlon-babel-todos-jobs-id)
  (tlon-babel-set-value-of-var 'tlon-babel-todos-generic-id)
  (dolist (template `(("tbJ" "Tlön: Babel: Create a new Babel job" entry
		       (id ,tlon-babel-todos-jobs-id)
		       "** %c" :immediate-finish t :empty-lines 1 :jump-to-captured t)
		      ("tbG" "Tlön: Babel: Create new generic todo from GitHub" entry
		       (id ,tlon-babel-todos-generic-id)
		       "** %c" :immediate-finish t :empty-lines 1 :prepend t :jump-to-captured t)))
    (push template org-capture-templates))
  (setq paths-files-bibliography-all
	`(,@paths-files-bibliography-personal
	  ,@tlon-babel-bibliography-files))
  (run-hooks 'tlon-babel-post-init-hook))

(defun tlon-babel-set-value-of-var (var)
  "Signal an error if the value of VAR is not set."
  (unless (symbol-value var)
    (user-error "Please set the value of `%s'" (symbol-name var))))

;;;;; [name]

(defun tlon-babel-get-repo-from-file (&optional file)
  "Return the repo to which FILE belongs.
If FILE is nil, use the current buffer's file name."
  (let* ((file (or file (tlon-babel-buffer-file-name) default-directory))
	 (directory-path (file-name-directory file)))
    (catch 'found
      (dolist (dir (tlon-babel-get-property-of-repos :dir))
	(when (string-prefix-p (file-name-as-directory dir)
			       directory-path)
	  (throw 'found dir))))))

(defun tlon-babel-buffer-file-name ()
  "Return name of file BUFFER is visiting, handling `git-dirs' path."
  ;; check that current buffer is visiting a file
  (when-let ((file (buffer-file-name)))
    (replace-regexp-in-string
     "/git-dirs/"
     "/Library/CloudStorage/Dropbox/repos/"
     (buffer-file-name))))

(defun tlon-babel-get-repo-from-key (key)
  "Return the repo corresponding to original KEY."
  (if-let ((file (tlon-babel-metadata-lookup "file" "original_key" key (tlon-babel-get-metadata-in-repos))))
      (if-let ((repo (catch 'found
		       (dolist (dir (tlon-babel-get-property-of-repos :dir))
			 (when (string-prefix-p (file-name-as-directory dir) file)
			   (throw 'found dir))))))
	  repo
	(user-error "No repo found for key %s" key))
    (user-error "Metadata lookup for key `%s' returned nil" key)))

(defun tlon-babel-get-file-from-key (key)
  "Return the file path of KEY."
  (if-let ((file (tlon-babel-metadata-lookup "file" "original_key" key
					     (tlon-babel-get-metadata-in-repos))))
      file
    (user-error "Metadata lookup for key `%s' returned nil" key)))

(defun tlon-babel-get-key-from-file (file)
  "Return the bibtex key of FILE."
  (or
   ;; when in `translations'
   (tlon-babel-metadata-lookup "translation_key" "file" file (tlon-babel-get-metadata-in-repo))
   ;; when file in `originals'
   (let ((translation (tlon-babel-get-counterpart file)))
     (tlon-babel-metadata-get-field-value-in-file "original_key" translation))))

(defun tlon-babel-set-translation-language (lang)
  "Set the translation LANG."
  (interactive (list (completing-read "Language: " tlon-babel-languages)))
  (setq tlon-babel-translation-language lang))

;;;;;; get file paths

;; Some file paths cannot be set as vars because they change dynamically
;; depending on the value of `tlon-babel-gpt-translate-file'. We define the
;; relevant functions here.

(defun tlon-babel-get-file-glossary (&optional language)
  "Return the file containing the glossary for LANGUAGE.
If LANGUAGE is nil, default to the languageuage set in
`tlon-babel-translation-language'."
  (let* ((language (or language tlon-babel-translation-language))
	 (repo (tlon-babel-repo-lookup :dir
				       :subproject "babel"
				       :language language)))
    (file-name-concat repo "dict/Glossary.csv")))

;;;;;; Create/visit todos

;;;;; User commits

(defun tlon-babel-latest-user-commit-in-file (&optional file)
  "Return latest commit by the current user in FILE.
If no FILE is provided, use the file visited by the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (default-directory (file-name-directory file))
	 (user (tlon-babel-user-lookup :git :name user-full-name))
	 ;; get most recent commit in FILE by USER
	 (output (shell-command-to-string (format "git log --pretty=format:'%%h %%an %%s' --follow -- '%s' | grep -m 1 '%s' | awk '{print $1}'" file user)))
	 (commit (car (split-string output "\n"))))
    commit))

(defun tlon-babel-log-buffer-latest-user-commit (&optional file)
  "Show modifications to FILE since the latest commit by the current user.
If no FILE is provided, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-babel-latest-user-commit-in-file file)))
    (magit-diff-range commit nil (list file))))

(defun tlon-babel-log-buffer-latest-user-commit-ediff (&optional file)
  "Run `ediff' session for FILE and its state when last committed by current user.
If FILE is not provided, use the file visited by the current buffer."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (commit (tlon-babel-latest-user-commit-in-file file))
	 (commit-file (tlon-babel-create-file-from-commit file commit)))
    (ediff-files commit-file file)))

;;;;; Markdown

(defun tlon-babel-check-in-markdown-mode ()
  "Check if the current buffer is in a Markdown-derived mode."
  (unless (derived-mode-p 'markdown-mode)
    (user-error "Not in a Markdown buffer")))

;;;;;; Cleanup

;;;;;;; autofix

(defun tlon-babel-autofix (regexp-list newtext)
  "Replace matches in REGEXP-LIST with NEWTEXT."
  (widen)
  (save-excursion
    (dolist (regexp regexp-list)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match newtext)))))

(defun tlon-babel-autofix-curly-quotes ()
  "Replace straight quotes with curly quotes when appropriate."
  (tlon-babel-autofix '("\\([^\\.\\?]\"\\)\\[")
		      "\\1["))

(defun tlon-babel-autofix-footnote-punctuation ()
  "Place footnotes after punctuation mark."
  (tlon-babel-autofix '("\\(.\\)\\(\\[\\^[[:digit:]]\\{1,3\\}\\]\\)\\([[:punct:]]\\)")
		      "\\1\\3\\2")
  (tlon-babel-autofix-footnote-punctuation-amend))

(defun tlon-babel-autofix-footnote-punctuation-amend ()
  "Reverse undesired effects of `tlon-babel-autofix-footnote-punctuation'.
Ideally the function should be amended so that it doesn’t introduce these
effects to begin with."
  (tlon-babel-autofix '("\\[\\[\\^\\([0-9]+\\)\\]\\^\\([0-9]+\\)\\]"  ; fixes `[[^1]^2]'
			"\\[\\^\\[\\^\\([0-9]+\\)\\]\\([0-9]+\\)\\]") ; fixes `[^[^1]2]'
		      "[^\\1][^\\2]"))

(defun tlon-babel-autofix-periods-in-headings ()
  "Remove periods at the end of headings."
  (tlon-babel-autofix '("^\\(#\\{2,6\\}.*\\)\\.$")
		      "\\1"))

(defun tlon-babel-autofix-percent-signs ()
  "Add non-breaking space before percent sign."
  (tlon-babel-autofix '("\\([[:digit:],()]+\\)%\\([^\";[:alnum:]]\\)"
			"\\([[:digit:],()]+\\) %\\([^\";[:alnum:]]\\)")
		      "\\1 %\\2"))

(defun tlon-babel-autofix-all ()
  "Run all the `tlon-babel-autofix' commands."
  (interactive)
  (tlon-babel-autofix-curly-quotes)
  (tlon-babel-autofix-footnote-punctuation)
  (tlon-babel-autofix-periods-in-headings)
  (tlon-babel-autofix-percent-signs)
  (let ((after-save-hook (remove #'tlon-babel-autofix-all after-save-hook)))
    (save-buffer)
    (add-hook 'after-save-hook #'tlon-babel-autofix-all nil t)))

;;;;;;; manual-fix

(defun tlon-babel-manual-fix (regexp-list newtext &optional keep-case)
  "Prompt user to replace matches in REGEXP-LIST with NEWTEXT.
If KEEP-CASE is non-nil, keep the case of the matched text."
  (widen)
  (save-excursion
    (point-min)
    (dolist (regexp regexp-list)
      (goto-char (point-min))
      (let ((case-replace keep-case))
	(query-replace-regexp regexp newtext nil (point-min) (point-max))))))

(defun tlon-babel-manual-fix-em-dashes ()
  "Prompt the user to replace hyphens with em dashes, when appropriate."
  (tlon-babel-manual-fix '("\\([^ ][ ,)]\\)-\\([(\"[:alnum:]]\\)" ; opening dash
			   "\\([)\\.%\"[:alnum:]]\\)-\\([ ,(]\\)" ; closing dash
			   "\\([^ >)] \\)-\\( \\)")
			 "\\1—\\2"))

(defun tlon-babel-manual-fix-number-ranges ()
  "Prompt the user to replace hyphens with em dashes, when appropriate."
  (tlon-babel-manual-fix '("\\([ \\[]\\)\\([[:digit:]]\\{1,12\\}\\)-\\([[:digit:]]\\{1,12\\}\\)\\([,.:;?!   ]\\)")
			 "\\1\\2–\\3\\4"))

(defun tlon-babel-manual-fix-roman-numerals ()
  "Prompt the user to add small caps tags to roman numerals."
  (tlon-babel-manual-fix '(" \\b\\([IVXLCDM]+\\)\\b")
			 " <abbr>\\1</abbr>"))

(defun tlon-babel-manual-fix-thin-spaces ()
  "Prompt the user to add a thin space between abbreviations followed by a period."
  (tlon-babel-manual-fix '("\\([A-Z]\\.\\)\\([A-Z]\\)")
			 "\\1 \\2"))

(defun tlon-babel-manual-fix-solo ()
  "Prompt the user to replace `sólo' with `solo'."
  (tlon-babel-manual-fix '("sólo")
			 "solo"
			 'keep-case))

(defun tlon-babel-manual-fix-podcast ()
  "Prompt the user to replace `podcast' with `pódcast'.
Enchant/Aspell do not make the correct suggestion, so it's easier to use a
dedicated function."
  (tlon-babel-manual-fix '(" podcast")
			 " pódcast"
			 'keep-case))

(defun tlon-babel-manual-fix-all ()
  "Run all the `tlon-babel-manual-fix' commands."
  (interactive)
  (tlon-babel-manual-fix-em-dashes)
  (tlon-babel-manual-fix-number-ranges)
  (tlon-babel-manual-fix-roman-numerals)
  (tlon-babel-manual-fix-thin-spaces)
  (tlon-babel-manual-fix-solo)
  (tlon-babel-manual-fix-podcast))

(defun tlon-babel-fix-internet-archive-links ()
  "Fix Internet Archive links in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((cnt 0))
      (while (re-search-forward "https://web\.archive\.org/web/[[:digit:]]*?/" nil t)
	(replace-match "")
	(setq cnt (1+ cnt)))
      (message "Done. %d URLs were fixed." cnt))))

(defun tlon-babel-repos-transient ()
  "Transient for opening projects in Magit."
  (interactive)
  (let ((transient-args (mapcar (lambda (repo)
				  (let* ((abbrev (plist-get repo :abbrev))
					 (fun-name (intern (concat "tlon-open-repo-" abbrev))))
				    (fset fun-name
					  `(lambda ()
					     (interactive)
					     (magit-status ,(plist-get repo :dir))))
				    `[,fun-name ,(plist-get repo :key) ,(plist-get repo :name)]))
				tlon-babel-repo-props)))
    (eval `(transient-define-prefix tlon-open-repo-transient ()
	     "Transient that dispatches to Magit open repo commands."
	     ,@transient-args))))

;;;;; Metadata

;;;;;; Get metadata

(defun tlon-babel-get-metadata-in-repo (&optional repo)
  "Return metadata of REPO.
If REPO is nil, return metadata of current repository."
  (let* ((repo (or repo (tlon-babel-get-repo))))
    (if (eq (tlon-babel-get-property-of-repo :subtype repo) 'translations)
	(tlon-babel-get-dir-metadata repo)
      (user-error "The repository `%s' is not a `translations' repository" repo))))

(defun tlon-babel-get-metadata-in-repos ()
  "Return metadata of all `translation' repos."
  (let ((metadata '()))
    (dolist (dir (tlon-babel-get-property-of-repos :dir :subtype 'translations))
      (setq metadata (append (tlon-babel-get-dir-metadata dir) metadata)))
    metadata))

(defun tlon-babel-get-dir-metadata (dir)
  "Return the metadata in DIR and all its subdirectories as an association list."
  (let ((metadata '()))
    (dolist (file (directory-files-recursively dir "\\.md$"))
      (push (tlon-babel-get-metadata-in-file-or-buffer file) metadata))
    metadata))

(defun tlon-babel-get-metadata-in-file-or-buffer (file-or-buffer)
  "Return the metadata in FILE-OR-BUFFER as an association list."
  (let* ((metadata (tlon-babel-yaml-format-values-of-alist
		    (tlon-babel-yaml-get-front-matter file-or-buffer)))
	 (extras `(("file" . ,file-or-buffer)
		   ("type" . "online")
		   ("database" . "Tlön")
		   ("landid" . "es"))))
    (append metadata extras)))

;;;;;; Query metadata

(defun tlon-babel-metadata-lookup (field1 field2 value2 metadata)
  "Search METADATA for VALUE2 in FIELD2 and return the value of FIELD1."
  (let ((found nil)
	(i 0))
    (while (and (not found) (< i (length metadata)))
      (when (equal (cdr (assoc field2 (nth i metadata))) value2)
	(setq found (cdr (assoc field1 (nth i metadata)))))
      (setq i (1+ i)))
    found))

(defun tlon-babel-metadata-get-all-field-values (field metadata &optional other-field match)
  "Return all the values for FIELD in METADATA.
If OTHER-FIELD is non-nil, return only FIELD values when OTHER-FIELD value in
entry matches the regex MATCH."
  (let ((result '()))
    (dolist (entry metadata)
      (when-let ((value (cdr (assoc field entry))))
	(if other-field
	    (when (string-match match (cdr (assoc other-field entry)))
	      (push value result))
	  (push value result))))
    result))

(defun tlon-babel-metadata-get-field-value-in-file (field &optional file-or-buffer)
  "Return the value of FIELD in metadata of FILE-OR-BUFFER.
If FILE is nil, use the file visited by the current buffer."
  (when-let* ((file-or-buffer (or file-or-buffer
				  (buffer-file-name)
				  (current-buffer)))
	      (metadata (tlon-babel-get-metadata-in-file-or-buffer file-or-buffer)))
    (alist-get field metadata nil nil #'string=)))

(defun tlon-babel-get-key-in-buffer ()
  "Get the BibTeX key in the current Markdown buffer."
  (tlon-babel-check-in-markdown-mode)
  (save-buffer)
  (let ((key (tlon-babel-metadata-get-field-value-in-file "original_key")))
    (unless key
      (user-error "No key found"))
    key))

(defun tlon-babel-get-filenames-in-dir (&optional dir extension)
  "Return a list of all filenames in DIR.
If DIR is nil, use the current directory. EXTENSION defaults to \"md\". If you
want to search all files, use the empty string."
  (let* ((dir (or dir default-directory))
	 (extension (or extension "md"))
	 (extension-regex (format "\\.%s$" extension))
	 (files (directory-files-recursively dir extension-regex)))
    (mapcar #'file-name-nondirectory files)))

;;;;; YAML front matter

(defconst tlon-babel-yaml-delimiter "---\n"
  "Delimiter for YAML front matter.")

(defconst tlon-babel-yaml-article-keys
  '("title" "authors" "translators" "tags" "date" "original_path" "original_key" "translation_key" "publication_status" "description")
  "List of YAML keys of fields to include in `uqbar-es' articles.
The order of the keys determines the sort order by
`tlon-babel--yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-tag-keys
  '("title" "brief_title" "original_path" "publication_status")
  "List of YAML keys of fields to include in `uqbar-es' tags.
The order of the keys determines the sort order by
`tlon-babel--yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-author-keys
  '("title" "original_path" "publication_status")
  "List of YAML keys of fields to include in `uqbar-es' authors.
The order of the keys determines the sort order by
`tlon-babel--yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-original-author-keys
  '("title")
  "List of YAML keys of fields to include in `uqbar-es' authors.
The order of the keys determines the sort order by
`tlon-babel--yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-publication-statuses
  '("unpublished" "test" "production")
  "List of publication statuses.")

;;;;;; Get YAML values

(defun tlon-babel-yaml-get-front-matter (&optional file-or-buffer raw)
  "Return the YAML front matter from FILE-OR-BUFFER as strings in a list.
If FILE-OR-BUFFER is nil, use the current buffer. Return the front matter as an
alist, unless RAW is non-nil."
  (let ((file-or-buffer (or file-or-buffer
			    (buffer-file-name)
			    (current-buffer))))
    (with-temp-buffer
      (cond
       ;; If `file-or-buffer' is a buffer object
       ((bufferp file-or-buffer)
	(insert (with-current-buffer file-or-buffer (buffer-string))))
       ;; If `file-or-buffer' is a string
       ((stringp file-or-buffer)
	(insert-file-contents file-or-buffer)))
      (goto-char (point-min))
      (when (looking-at-p tlon-babel-yaml-delimiter)
	(forward-line)
	(let ((front-matter (tlon-babel-read-until-match tlon-babel-yaml-delimiter)))
	  (if raw
	      front-matter
	    (tlon-babel-yaml-to-alist front-matter)))))))

(defun tlon-babel-yaml-to-alist (strings)
  "Convert YAML STRINGS to an alist."
  (let ((metadata '()))
    (dolist (line strings)
      (when (string-match "^\\(.*?\\):\\s-+\\(.*\\)$" line)
	(let* ((key (match-string 1 line))
	       (value (match-string 2 line))
	       (trimmed-value (string-trim value)))
	  (push (cons (string-trim key) trimmed-value) metadata))))
    (nreverse metadata)))

(defun tlon-babel-yaml-format-values-of-alist (alist)
  "Format the values of ALIST, converting from YAML format to Elisp format."
  (mapcar (lambda (pair)
	    (cons (car pair)
		  (tlon-babel-yaml-format-value (cdr pair))))
	  alist))

(defun tlon-babel-yaml-format-value (value)
  "Format VALUE by converting from the YAML format to an Elisp format."
  (cond
   ((and (string-prefix-p "[" value) (string-suffix-p "]" value)) ;; list
    (mapcar #'string-trim
	    (mapcar (lambda (s)
		      (if (and (string-prefix-p "\"" s) (string-suffix-p "\"" s))
			  (substring s 1 -1)
			s))
		    (split-string (substring value 1 -1) "\\s *,\\s *"))))
   ((and (string-prefix-p "\"" value) (string-suffix-p "\"" value)) ;; string
    (substring value 1 -1))
   (t value)))

(defun tlon-babel-read-until-match (delimiter)
  "Return a list of lines until DELIMITER is matched.
The delimiter is not included in the result. If DELIMITER is not found, signal
an error."
  (let ((result '()))
    (while (not (or (looking-at-p delimiter) (eobp)))
      (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) result)
      (forward-line))
    (if (eobp)
	(error "Delimiter not found")
      (nreverse result))))

;;;;; Set YAML values

(defun tlon-babel--yaml-set-front-matter-fields (fields &optional title)
  "Set the field values for the given FIELDS in the current buffer.
If TITLE is non-nil, use it instead of prompting for one."
  (let* ((var-generators
	  `(("date" . ,(lambda () (format-time-string "%FT%T%z")))
	    ("title" . ,(lambda () (or title (read-string "Title: "))))
	    ("authors-list" . ,(lambda () (tlon-babel-yaml-set-multi-value-field "title" "authors")))
	    ("translators" . ,#'tlon-babel-yaml-set-translators)
	    ("tags" . ,#'tlon-babel-yaml-set-tags)
	    ("original_path" . ,#'tlon-babel-yaml-set-original-path)))
	 (processed-fields (if (member "authors" fields)
			       (cons "authors-list" fields)
			     fields))
	 (field-values (cl-loop for field in processed-fields
				for generator = (cdr (assoc field var-generators))
				if generator collect `(,field . ,(funcall generator)))))
    ;; calculate first-author and adjust field-values
    (let* ((first-author (cdr (or (assoc "authors" field-values) (assoc "authors-list" field-values))))
	   (authors (when first-author
		      (or
		       (cdr (assoc "authors" field-values))
		       (tlon-babel-elisp-list-to-yaml first-author))))
	   (cmpl-generators
	    `(("first-author" . ,first-author)
	      ("authors" . ,authors)
	      ("publication_status" . "no publicado")
	      ("original_key" . ,(when first-author (tlon-babel-yaml-set-original-key (car first-author))))
	      ("translation_key" . ,(when first-author
				      (tlon-babel-bibtex-generate-autokey
				       (car first-author)
				       (substring (cdr (assoc "date" field-values)) 0 4)
				       (cdr (assoc "title" field-values))))))))
      ;; revise field-values
      (setq field-values (assoc-delete-all "authors-list" field-values))
      (dolist (field fields)
	(when (cdr (assoc field cmpl-generators))
	  (push `(,field . ,(cdr (assoc field cmpl-generators))) field-values))))
    field-values))

(defun tlon-babel-yaml-set-front-matter (keys &optional title)
  "Insert YAML fields for KEYS for `uqbar-en' article in the current buffer.
If TITLE is non-nil, use it instead of prompting for one. The fields will be
inserted in the order in which KEYS are listed."
  (let* ((fields (tlon-babel--yaml-set-front-matter-fields keys title))
	 (sorted-fields (tlon-babel--yaml-sort-fields fields keys)))
    (tlon-babel-insert-yaml-fields sorted-fields)))

(defun tlon-babel--yaml-sort-fields (fields &optional keys no-error)
  "Sort alist of YAML FIELDS by order of KEYS.
If one of FIELDS is not found, throw an error unless NO-ERROR is non-nil."
  (mapcar (lambda (key)
	    (if-let ((match (assoc key fields)))
		match
	      (unless no-error
		(user-error "Key `%s' not found in file `%s'" key (buffer-file-name)))))
	  keys))

(defun tlon-babel-yaml-set-front-matter-for-article (&optional title)
  "Insert YAML fields for `uqbar-en' article in the current buffer.
If TITLE is non-nil, use it instead of prompting for one."
  (interactive)
  (tlon-babel-yaml-set-front-matter tlon-babel-yaml-article-keys title))

(defun tlon-babel-yaml-set-front-matter-for-tag ()
  "Insert YAML fields for `uqbar-en' tag in the current buffer.
If TITLE is non-nil, use it instead of prompting for one."
  (interactive)
  (tlon-babel-yaml-set-front-matter tlon-babel-yaml-tag-keys))

(defun tlon-babel-yaml-set-front-matter-for-author ()
  "Insert YAML fields for `uqbar-en' author in the current buffer.
If TITLE is non-nil, use it instead of prompting for one."
  (interactive)
  (tlon-babel-yaml-set-front-matter tlon-babel-yaml-author-keys))

(defun tlon-babel-yaml-set-front-matter-for-original-author (&optional title)
  "Insert YAML fields for `uqbar-en' author in the current buffer.
If TITLE is non-nil, use it instead of prompting for one."
  (interactive)
  (tlon-babel-yaml-set-front-matter tlon-babel-yaml-original-author-keys title))

;; TODO: throw error if any of fields already present
(defun tlon-babel-insert-yaml-fields (fields)
  "Insert YAML FIELDS in the buffer at point.
FIELDS is an alist, typically generated via `tlon-babel-yaml-to-alist'."
  (when (looking-at-p tlon-babel-yaml-delimiter)
    (user-error "File appears to already contain a front matter section"))
  (save-excursion
    (goto-char (point-min))
    ;; calculate the max key length
    (let ((max-key-len (cl-reduce 'max (mapcar (lambda (cons) (length (car cons))) fields)))
	  format-str)
      ;; determine the format for string
      (setq format-str (format "%%-%ds %%s\n" (+ max-key-len 2)))
      ;; insert the yaml delimiter & fields
      (insert tlon-babel-yaml-delimiter)
      (dolist (cons fields)
	(insert (format format-str (concat (car cons) ":") (cdr cons))))
      (insert tlon-babel-yaml-delimiter))))

(defun tlon-babel-delete-yaml-front-matter ()
  "Delete YAML front matter section."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p tlon-babel-yaml-delimiter)
      (user-error "File does not appear to contain a front matter section"))
    (forward-line)
    (re-search-forward tlon-babel-yaml-delimiter)
    (delete-region (point-min) (point))))

(defun tlon-babel-yaml-reorder-front-matter ()
  "Reorder the YAML front matter in the buffer at point."
  (interactive)
  (save-excursion
    (let* ((unsorted (tlon-babel-yaml-get-front-matter))
	   (sorted (tlon-babel--yaml-sort-fields
		    unsorted (tlon-babel-yaml-get-uqbar-keys) 'no-error)))
      (tlon-babel-delete-yaml-front-matter)
      (tlon-babel-insert-yaml-fields sorted))))

(defun tlon-babel-yaml-get-uqbar-keys (&optional file)
  "Return the admissible keys for `uqbar-es' FILE.
If FILE is nil, return the work type of the file visited by the current buffer."
  (let* ((file (or file (buffer-file-name))))
    (pcase (file-name-nondirectory (directory-file-name (file-name-directory file)))
      ("articulos" tlon-babel-yaml-article-keys)
      ("temas" tlon-babel-yaml-tag-keys)
      ("authors" tlon-babel-yaml-author-keys))))

;; TODO: revise `translations' now that we use separate repos
(defun tlon-babel-yaml-set-multi-value-field (field &optional dir repo)
  "Set the value of multivalue FIELD in metadata of REPO.
If DIR is non-nil, only search in directory within the repo. Note DIR does not
include the `translations' directory. That is, it is the directory component of
the repo's locator. For example, to search only in `translations/autores', use
`autores' as DIR."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (metadata (tlon-babel-get-metadata-in-repo repo))
	 (full-dir (when dir (file-name-concat repo "translations" dir))))
    (completing-read-multiple (format "%s: " (capitalize dir))
			      (tlon-babel-metadata-get-all-field-values
			       field metadata (when dir "file") (when dir full-dir)))))

(defun tlon-babel-yaml-set-authors ()
  "Set the value of `authors' YAML field."
  (tlon-babel-elisp-list-to-yaml
   (tlon-babel-yaml-set-multi-value-field "title" "authors")))

(defun tlon-babel-yaml-set-translators ()
  "Set the value of `translators' YAML field."
  (tlon-babel-elisp-list-to-yaml
   (completing-read-multiple
    "Translators: "
    (tlon-babel-metadata-get-all-field-values "translators" (tlon-babel-get-metadata-in-repos)))))

(defun tlon-babel-yaml-set-tags ()
  "Set the value of `tags' YAML field."
  (tlon-babel-elisp-list-to-yaml
   (tlon-babel-yaml-set-multi-value-field "title" "tags")))

(defun tlon-babel-elisp-list-to-yaml (list)
  "Convert an Elisp LIST to a YAML list."
  (concat "[\"" (mapconcat 'identity list "\", \"") "\"]"))

(defun tlon-babel-yaml-set-original-path ()
  "Set the value of `original_path' YAML field."
  (let ((dir (tlon-babel-get-counterpart-dir (buffer-file-name))))
    (completing-read "Original filename: "
		     (tlon-babel-get-filenames-in-dir dir))))

(defun tlon-babel-get-repo-in-subproject-language (language &optional repo)
  "Return the path of the subproject in REPO corresopnding to LANGUAGE.
If REPO is nil, use the current repository."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (subproject (tlon-babel-get-property-of-repo :subproject repo)))
    (tlon-babel-repo-lookup :dir :subproject subproject :language language)))

(defun tlon-babel-yaml-set-original-key (author)
  "Set the value of `original_key' YAML field.
AUTHOR is the first author of the original work."
  (let ((first-author (car (last (split-string author)))))
    (car (split-string
	  (completing-read
	   "English original: "
	   (citar--completion-table (citar--format-candidates) nil)
	   nil
	   nil
	   (format "%s " first-author)
	   'citar-history citar-presets nil)))))

(defun tlon-babel-replace-footnotes ()
  "Replace footnotes in counterpart of Spanish tag in current buffer."
  (interactive)
  (tlon-babel-open-counterpart nil)
  (widen)
  (goto-char (point-min))
  (let ((fn-regexp "^\\[^[[:digit:]]+\\]:"))
    (if (re-search-forward fn-regexp nil t)
	(progn
	  (beginning-of-line)
	  (let ((start (point)))
	    (goto-char (point-max))
	    (copy-region-as-kill start (point)))
	  (tlon-babel-open-counterpart)
	  (goto-char (point-min))
	  (re-search-forward fn-regexp nil t)
	  (goto-char (match-beginning 0))
	  (let ((start (point)))
	    (goto-char (point-max))
	    (delete-region start (point))
	    (yank)
	    (save-buffer)))
      (tlon-babel-open-counterpart)
      (message "No footnotes found"))))

;;;;;; Interactive editing

(defun tlon-babel-yaml-edit-field ()
  "Edit the YAML field at point."
  (interactive)
  (cl-destructuring-bind (key value) (tlon-babel-yaml-get-field-at-point)
    (tlon-babel-yaml-get-completions key value)))

(defun tlon-babel-yaml-get-completions (key value)
  "Get completions based on KEY.
If KEY already has VALUE, use it as the initial input."
  (if-let ((fun (tlon-babel-yaml-get-completion-functions key))
	   (val (tlon-babel-yaml-get-completion-values key)))
      (funcall fun val)
    (tlon-babel-yaml-insert-string (list value))))

(defun tlon-babel-yaml-get-completion-values (key)
  "Get completion values for a YAML field with KEY."
  (pcase key
    ("translators" (tlon-babel-get-translators))
    ("tags" (tlon-babel-get-uqbar-fields "tags"))
    ("authors" (tlon-babel-get-uqbar-fields "authors"))
    ("original_path" (tlon-babel-get-filenames-in-dir))
    ("original_key" (citar--completion-table (citar--format-candidates) nil))
    ("translation_key" (citar--completion-table (citar--format-candidates) nil))
    ("publication_status" tlon-babel-yaml-publication-statuses)
    (_ nil)))

(defun tlon-babel-yaml-get-completion-functions (key)
  "Get completion functions for a YAML field with KEY."
  (pcase key
    ((or "authors" "translators" "tags") #'tlon-babel-yaml-insert-list)
    ((or "original_path" "original_key" "translation_key" "publication_status") #'tlon-babel-yaml-insert-string)
    (_ nil)))

;; TODO: integrate `tlon-babel-yaml-get-completion-values'
(defun tlon-babel-yaml-insert-field (&optional key value file field-exists)
  "Insert a new field in the YAML front matter of FILE.
If FILE is nil, use the file visited by the current buffer. If KEY or VALUE are
nil, prompt for one. If field exists, throw an error if FIELD-EXISTS is
`throw-error', overwrite if it is `overwrite', and do nothing otherwise."
  (interactive)
  (let ((key (or key (completing-read "Key: " (tlon-babel-yaml-get-uqbar-keys))))
	(value (or value (read-string "Value: ")))
	(file (or file (buffer-file-name))))
    (if-let ((front-matter (tlon-babel-yaml-get-front-matter file)))
	(if-let ((key-exists-p (assoc key front-matter)))
	    (cond ((eq field-exists 'overwrite)
		   (tlon-babel-yaml-delete-field key file)
		   (tlon-babel-yaml-write-field key value file))
		  ((eq field-exists 'throw-error)
		   (user-error "Field `%s' already exists in `%s'" key file)))
	  (tlon-babel-yaml-write-field key value file))
      (user-error "File `%s' does not appear to contain a front matter section" file))))

(defun tlon-babel-yaml-write-field (key value file)
  "Set KEY to VALUE in FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (forward-line)
    (insert (format "%s:  %s\n" key value))
    (save-buffer)
    (tlon-babel-yaml-reorder-front-matter)))

;; TODO: refactor with above
(defun tlon-babel-yaml-delete-field (&optional key file)
  "Delete the YAML field with KEY in FILE."
  (let ((key (or key (completing-read "Field: " tlon-babel-yaml-article-keys)))
	(file (or file (buffer-file-name))))
    (if-let ((front-matter (tlon-babel-yaml-get-front-matter file)))
	(if (assoc key front-matter)
	    (with-current-buffer (find-file-noselect file)
	      (goto-char (point-min))
	      (re-search-forward (format "%s:.*\n" key))
	      (delete-region (match-beginning 0) (match-end 0))
	      (save-buffer))
	  (user-error "Key `%s' not found in file `%s'" key file))
      (user-error "File does not appear to contain a front matter section"))))

;; TODO: Handle multiline fields, specifically `description’
(defun tlon-babel-yaml-get-field-at-point ()
  "Return a list with the YAML key and value at point, or nil if there is none."
  (when-let* ((bounds (bounds-of-thing-at-point 'line))
	      (line (buffer-substring-no-properties (car bounds) (cdr bounds)))
	      (elts (split-string line ":" nil "\\s-+")))
    elts))

(defun tlon-babel-yaml-get-key (key)
  "Get value of KEY in YAML metadata."
  (alist-get key (tlon-babel-yaml-get-front-matter) nil nil #'string=))

(defun tlon-babel-yaml-insert-list (candidates)
  "Insert a list in YAML field at point.
Prompt the user to select one or more elements in CANDIDATES. If point is on a
list, use them pre-populate the selection."
  (let* ((bounds (bounds-of-thing-at-point 'line))
	 ;; retrieve the line
	 (line (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when (string-match "\\[\\(.*?\\)\\]" line)
      ;; retrieve and parse the elements in the list at point, removing quotes
      (let ((elems-at-point (mapcar (lambda (s)
				      (replace-regexp-in-string "\\`\"\\|\"\\'" "" s))
				    (split-string (match-string 1 line) ", "))))
	;; prompt the user to select multiple elements from the list,
	;; prefilling with previously selected items
	(let ((choices (completing-read-multiple "Value (comma-separated): "
						 candidates
						 nil nil
						 (mapconcat 'identity elems-at-point ", "))))
	  ;; delete the old line
	  (delete-region (car bounds) (cdr bounds))
	  ;; insert the new line into the current buffer
	  (insert (replace-regexp-in-string "\\[.*?\\]"
					    (concat "["
						    (mapconcat (lambda (item)
								 (format "\"%s\"" item))
							       choices ", ")
						    "]")
					    line)))))))

(defun tlon-babel-yaml-insert-string (candidates)
  "Insert a string in the YAML field at point.
Prompt the user for a choice in CANDIDATES. If point is on a string, use it to
pre-populate the selection."
  (cl-destructuring-bind (key _) (tlon-babel-yaml-get-field-at-point)
    (let* ((choice (completing-read (format "Value of `%s': " key)
				    candidates))
	   (bounds (bounds-of-thing-at-point 'line)))
      (delete-region (car bounds) (cdr bounds))
      (insert (format "%s:  %s\n" key choice)))))

;;;;;; Get repo-specific entities

(defun tlon-babel-get-uqbar-fields (type &optional field language)
  "Return FIELDS in files of TYPE in `uqbar' repo of LANGUAGE.
If FIELD is nil, default to \"title\". If LANG is nil, default to
`tlon-babel-translation-language'."
  (let* ((field (or field "title"))
	 (language (or language tlon-babel-translation-language))
	 (type-in-language (tlon-babel-get-bare-dir-translation language "en" type))
	 (repo (tlon-babel-repo-lookup :dir :subproject "uqbar" :language language)))
    (tlon-babel-metadata-get-all-field-values
     field (tlon-babel-get-metadata-in-repo repo) "file" (file-name-concat repo type-in-language))))

(defun tlon-babel-get-all-uqbar-entities ()
  "Get a list of all `uqbar-en' entities."
  (append
   (tlon-babel-get-uqbar-fields "articles")
   (tlon-babel-get-uqbar-fields "authors")
   (tlon-babel-get-uqbar-fields "tags")))

;;;;;; Create repo-specific entities

;; TODO: fix; `tlon-babel-yaml-set-front-matter-for-tag-or-author' does not exist
(defun tlon-babel-create-uqbar-entity (dir)
  "Create a new file for `uqbar-es' entity in DIR."
  (let ((default-directory (file-name-concat
			    (tlon-babel-get-property-of-repo-name :dir "uqbar-es")
			    (file-name-as-directory dir))))
    (files-extras-new-empty-buffer)
    (tlon-babel-yaml-set-front-matter-for-tag-or-author)
    (goto-char (point-max))
    (tlon-babel-name-file-from-title)
    (insert (format "**%s** es " (tlon-babel-metadata-get-field-value-in-file "title")))
    (save-buffer)))

(defun tlon-babel-name-file-from-title (&optional title)
  "Save the current buffer to a file named after TITLE.
Set the name to the slugified version of TITLE with the extension `.md'. If
TITLE is nil, get it from the file metadata. If the file doesn't have metadata,
prompt the user for a title.

When buffer is already visiting a file, prompt the user for confirmation before
renaming it."
  (interactive)
  (let* ((title (or title
		    (tlon-babel-metadata-get-field-value-in-file "title")
		    (read-string "Title: ")))
	 (target (tlon-babel-set-file-from-title title default-directory)))
    (if-let ((buf (buffer-file-name)))
	(when (yes-or-no-p (format "Rename `%s` to `%s`? "
				   (file-name-nondirectory buf)
				   (file-name-nondirectory target)))
	  (rename-file buf target)
	  (set-visited-file-name target)
	  (save-buffer))
      (write-file target))))

(defun tlon-babel-set-file-from-title (&optional title dir)
  "Set the file path based on its title.
The file name is the slugified version of TITLE with the extension `.md'. This
is appended to DIR to generate the file path. If DIR is not provided, use the
current repository followed by `originals/'."
  (let* ((title (or title
		    (read-string "Title: ")))
	 (filename (file-name-with-extension (tlon-core-slugify title) "md"))
	 (dirname (file-name-as-directory
		   (or dir
		       (file-name-concat (tlon-babel-get-repo) "originals")))))
    (file-name-concat dirname filename)))

(defun tlon-babel-create-uqbar-author ()
  "Create a new file for `uqbar-es' author."
  (interactive)
  (tlon-babel-create-uqbar-entity "autores"))

(defun tlon-babel-create-uqbar-tag ()
  "Create a new file for `uqbar-es' tag."
  (interactive)
  (tlon-babel-create-uqbar-entity "temas"))

;;;;;; Get repo-agnostic elements

(defun tlon-babel-get-translators ()
  "Get a list of translators.
Note that this searches in all repos, not just `uqbar-en'."
  (tlon-babel-metadata-get-all-field-values
   "title"
   (tlon-babel-get-metadata-in-repos)))

;;;;;; Counterparts

(defun tlon-babel-get-content-subtype (&optional file)
  "For repo of FILE, get the value of its `:subtype' property.
If FILE is nil, return the counterpart of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-get-repo-from-file file))
	 (type (tlon-babel-repo-lookup :type :dir repo)))
    (unless (eq type 'content)
      (user-error "Repo of file `%s' is not of type `content'" file))
    (tlon-babel-repo-lookup :subtype :dir repo)))

(defun tlon-babel-get-counterpart (&optional file)
  "Get the counterpart file of FILE.
A file's counterpart is its translation if it is an original, and vice versa.
The translation language is defined by `tlon-babel-translation-language'.

If FILE is nil, return the counterpart of the file visited by the current
buffer."
  (let* ((file (or file (tlon-babel-buffer-file-name)))
	 (repo (tlon-babel-get-repo-from-file file)))
    (pcase (tlon-babel-get-property-of-repo :subtype repo)
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
    (tlon-babel-metadata-lookup "file"
				"original_path"
				(file-name-nondirectory file)
				(tlon-babel-get-metadata-in-repo translations-repo))))

(defun tlon-babel-get-counterpart-repo (&optional file)
  "Get the counterpart repo of FILE.
A file's counterpart repo is the repo of that file's counterpart.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (tlon-babel-buffer-file-name)))
	 (repo (tlon-babel-get-repo-from-file file))
	 (subproject (tlon-babel-get-property-of-repo :subproject repo))
	 (language (tlon-babel-get-counterpart-language repo))
	 (counterpart-repo
	  (tlon-babel-repo-lookup :dir
				  :subproject subproject
				  :language language)))
    counterpart-repo))

(defun tlon-babel-get-counterpart-language (&optional repo)
  "Return the language of the counterpart of REPO."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (language (tlon-babel-get-property-of-repo :language repo)))
    (pcase language
      ("en" tlon-babel-translation-language)
      ((pred (lambda (lang) (member lang (mapcar #'car tlon-babel-languages)))) "en")
      (_ (user-error "Language not recognized")))))

(defun tlon-babel-get-counterpart-dir (&optional file)
  "Get the counterpart directory of FILE.
A file's counterpart directory is the directory of that file's counterpart. For
example, the counterpart directory of `~/Dropbox/repos/uqbar-es/autores/' is
`~/Dropbox/repos/uqbar-en/authors/'.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-get-repo-from-file file))
	 (counterpart-repo (tlon-babel-get-counterpart-repo file))
	 (bare-dir (tlon-babel-get-bare-dir file))
	 (source-lang (tlon-babel-get-property-of-repo :language repo))
	 (target-lang (tlon-babel-get-counterpart-language repo))
	 (counterpart-bare-dir (tlon-babel-get-bare-dir-translation target-lang source-lang bare-dir)))
    (file-name-concat counterpart-repo counterpart-bare-dir)))

(defun tlon-babel-get-bare-dir (&optional file)
  "Get the bare directory of FILE.
A file’s bare directory is its directory minus its repository. For example, the
bare directory of `~/Dropbox/repos/uqbar-es/autores/' is `autores'.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-get-repo-from-file file)))
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
		       (or file (thing-at-point 'filename)))))
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

;;;;; Word count

(defun tlon-babel-get-text-between-lines (start-line end-line)
  "Return the text between START-LINE and END-LINE in the current buffer."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward start-line nil t)
	(let* ((start (line-beginning-position))
	       (end (when (re-search-forward end-line nil t)
		      (line-end-position))))
	  (when (and start end)
	    (buffer-substring-no-properties start end)))))))

(defun tlon-babel-get-local-variables ()
  "Get the text in the \"local variables\" section of the current buffer."
  (tlon-babel-get-text-between-lines
   tlon-babel-local-variables-line-start
   tlon-babel-local-variables-line-end))

(defun tlon-babel-count-words-extra ()
  "Count extraneous words in current buffer."
  (let ((metadata (mapconcat 'identity (tlon-babel-yaml-get-front-matter nil 'raw) " ")))
    (with-temp-buffer
      (insert metadata)
      (when-let ((vars (tlon-babel-get-local-variables)))
	(insert vars))
      (goto-char (point-min))
      (count-words-region (point-min) (point-max)))))

(defun tlon-babel-count-words-substance ()
  "Count substantive words in current buffer."
  (save-restriction
    (widen)
    (let ((raw (count-words (point-min) (point-max))))
      (- raw (tlon-babel-count-words-extra)))))

(defun tlon-babel-count-words-in-repo (&optional repo)
  "Count words in Markdown files in REPO.
If REPO is nil, prompt the user for one."
  (interactive)
  (let* ((repo (or repo
		   (intern (completing-read
			    "Repo: "
			    (tlon-babel-get-property-of-repos :abbrev :type 'translations)))))
	 (initial-buffers (buffer-list))
	 (files (directory-files-recursively
		 (tlon-babel-get-property-of-repo-name :dir repo) "\\.md$"))
	 (total-words 0))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
	(let ((words-in-file (tlon-babel-count-words-substance)))
	  (setq total-words (+ total-words words-in-file)))
	(unless (member (current-buffer) initial-buffers)
	  (kill-buffer (current-buffer)))))
    (message (number-to-string total-words))))

;;;;; Clocked heading

(defun tlon-babel-get-clock ()
  "Return the currently clocked heading."
  (if org-clock-current-task
      (substring-no-properties org-clock-current-task)
    (user-error "No clock running")))

(defun tlon-babel-get-clock-key ()
  "Return bibtex key in clocked heading.
Assumes key is enclosed in backticks."
  ;; second capture group handles optional .md extension
  (if (string-match tlon-babel-key-regexp (tlon-babel-get-clock))
      (match-string 1 (tlon-babel-get-clock))
    (user-error "I wasn't able to find a file in clocked heading")))

(defun tlon-babel-get-clock-file ()
  "Return the file path of the clocked task."
  (let ((key (tlon-babel-get-clock-key)))
    (tlon-babel-get-file-from-key key)))

(defun tlon-babel-open-clock-file ()
  "Open file of clocked task."
  (interactive)
  (find-file (tlon-babel-get-clock-file)))

(defun tlon-babel-get-clock-issue ()
  "Get issue GID from `orgit-forge' link in heading at point."
  (unless org-clock-heading
    (user-error "No clock running"))
  (save-window-excursion
    (org-clock-goto)
    (org-narrow-to-subtree)
    (when (re-search-forward org-link-bracket-re)
      (let ((raw-link (org-link-unescape (match-string-no-properties 1))))
	(string-match "orgit-topic:\\(.+\\)" raw-link)
	(match-string 1 raw-link)))))

(defun tlon-babel-open-clock-issue ()
  "Open the issue from `orgit-forge' link in heading at point."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo))
	(issue (tlon-babel-get-clock-issue)))
    (forge-visit-issue issue)))

(defun tlon-babel-get-clock-action ()
  "Return action in clock.
Assumes action is first word of clocked task."
  ;; as rough validation, we check that the clocked heading contains a file
  (tlon-babel-get-clock-key)
  (let ((action (nth 1 (split-string (tlon-babel-get-clock))))
	(actions (tlon-babel-get-property-of-labels :action)))
    (if (member action actions)
	action
      (user-error "I wasn't able to find a relevant action in clocked heading"))))

(defun tlon-babel-get-clock-label ()
  "Return label associated with action in heading at point."
  (let ((label (tlon-babel-label-lookup :label :action (tlon-babel-get-clock-action))))
    label))

(defun tlon-babel-get-clock-next-label ()
  "Return label associated with the action after the one in heading at point."
  (tlon-babel-next-value :label (tlon-babel-get-clock-label) tlon-babel-labels))

(defun tlon-babel-next-value (property value alist)
  "Return the \"next\" value of PROPERTY with VALUE in ALIST."
  (catch 'found
    (let ((found nil))
      (dolist (item alist)
	(when (and found (not (equal (plist-get item property) value)))
	  (throw 'found (plist-get item property)))
	(when (equal (plist-get item property) value)
	  (setq found t))))
    nil))

(defun tlon-babel-copy-buffer (&optional file deepl)
  "Copy the contents of FILE to the kill ring.
Defaults to the current buffer if no FILE is specified. If DEEPL is non-nil,
open DeepL."
  (let ((file (or file (buffer-file-name))))
    (with-current-buffer (find-file-noselect file)
      (copy-region-as-kill (point-min) (point-max)))
    (message "Copied the contents of `%s' to kill ring" (file-name-nondirectory file)))
  (when deepl
    (shell-command "open '/Applications/DeepL.app/Contents/MacOS/DeepL'")))

(defun tlon-babel-copy-region (beg end)
  "Copy the contents between BEG and END to the kill ring."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (copy-region-as-kill (point-min) (point-max))))
  (message "Copied the contents of the region to kill ring"))

(defun tlon-babel-copy-dwim ()
  "Copy the contents of the region or buffer to the kill ring."
  (interactive)
  (if (region-active-p)
      (tlon-babel-copy-region (region-beginning) (region-end))
    (tlon-babel-copy-buffer)))

(defun tlon-babel-set-paths-from-clock ()
  "Return paths for original and translation files based on clocked task."
  (let* ((key (tlon-babel-get-clock-key))
	 (metadata (tlon-babel-get-metadata-in-repos))
	 (repo (tlon-babel-get-repo-from-key key))
	 (identifier (tlon-babel-metadata-lookup "original_path" "original_key" key metadata))
	 (original-path (file-name-concat repo "originals" identifier))
	 (translation-path (tlon-babel-metadata-lookup "file" "original_key" key metadata)))
    (cl-values original-path translation-path key)))

(defun tlon-babel-set-windows (original-path translation-path)
  "Open ORIGINAL-PATH and TRANSLATION-PATH in windows 1 and 2."
  (window-extras-split-if-unsplit)
  (winum-select-window-1)
  (find-file original-path)
  (winum-select-window-2)
  (find-file translation-path))

;;;;; Forge

(defun tlon-babel-magit-status ()
  "Show the status of the current repository in a buffer."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo nil 'include-all)))
    (magit-status-setup-buffer)))

(defun tlon-babel-magit-prompt (repo)
  "Prompt the user for a REPO and show it in Magit."
  (interactive (list
		(completing-read
		 "Repo: " (tlon-babel-get-property-of-repos :name))))
  (if-let ((default-directory (tlon-babel-repo-lookup :dir :name repo)))
      (magit-status-setup-buffer)
    (user-error "Repo `%s' not found" repo)))

(defun tlon-babel-forge ()
  "Launch the Forge dispatcher.
If the current directory matches none of the directories in
`tlon-babel-repo-props', prompt the user to select a repo from that list."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo nil 'include-all)))
    (call-interactively 'forge-dispatch)))

(defun tlon-babel-forge-update-repo (repo)
  "Update issues and notifications for REPO name."
  (let* ((default-directory (tlon-babel-repo-lookup :dir :name repo))
	 (repo (forge-get-repository 'full)))
    (save-window-excursion
      (with-current-buffer (dired-noselect default-directory)
	(forge-pull repo)))))

(defun tlon-babel-forge-update-all-repos ()
  "Update issues and notifications for all active repos."
  (interactive)
  (dolist (repo (tlon-babel-get-property-of-repos :name))
    (tlon-babel-forge-update-repo repo)))

(defun tlon-babel-get-repo (&optional no-prompt include-all)
  "Get Babel repository path.
If the current directory matches any of the directories in `tlon-babel-repo-props',
return it. Else, prompt the user to select a repo from that list, unless
NO-PROMPT is non-nil. In that case, signal an error if its value is `error',
else return nil. If INCLUDE-ALL is non-nil, include all repos. In that case,
matching will be made against repos with any value for the property `:type'."
  (if-let ((current-repo (tlon-babel-get-repo-from-file)))
      current-repo
    (if no-prompt
	(when (eq no-prompt 'error)
	  (user-error "Not in a recognized Babel repo"))
      (let* ((content (tlon-babel-get-property-of-repos :name :type 'translations))
	     (all (tlon-babel-get-property-of-repos :name)))
	(tlon-babel-repo-lookup :dir :name
				(completing-read "Select repo: "
						 (if include-all all content)))))))

(defun tlon-babel-get-commit-key ()
  "Get key of commit file."
  (let ((path (file-name-concat (file-name-directory (tlon-babel-buffer-file-name))
				(magit-extras-get-commit-file))))
    (tlon-babel-get-key-from-file path)))

(defun tlon-babel-create-translation-file (&optional repo)
  "Create a new translation file and set its front matter.
If REPO is nil, prompt the user for one."
  (interactive)
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (title (read-string "Translated title: "))
	 (dir (file-name-concat repo "translations/articulos/"))
	 (path (tlon-babel-set-file-from-title title dir)))
    (find-file path)
    (tlon-babel-yaml-set-front-matter-for-article title)
    (save-buffer)))

(defun tlon-babel-create-record-for-job (&optional key)
  "Create a record based on KEY.
Creates a new record in the repository (with the format `Job: KEY') and a new
heading in the file `jobs.org'. If KEY is not provided, the key in the current
Markdown buffer at point is used."
  (interactive)
  (if-let ((key (or key
		    (pcase major-mode
		      ('markdown-mode (tlon-babel-get-key-in-buffer))
		      ('ebib-entry-mode (ebib--get-key-at-point))))))
      (progn
	(tlon-babel-ogh-create-issue-from-key key)
	(tlon-babel-create-heading-for-job key 'commit))
    (user-error "I wasn't able to create a record because I didn't find a key")))

;;;;; Sentence highlighting

(defun tlon-babel-sentence-highlight-offset-set ()
  "Set the sentence offset.
This command should be run from the source window."
  (interactive)
  (let ((source-window-sentences (count-sentences (point-min) (point)))
	target-window-sentences)
    (with-selected-window (cadr (window-list))
      (setq target-window-sentences (count-sentences (point-min) (point))))
    (setq tlon-babel-sentence-highlight-offset
	  (- source-window-sentences target-window-sentences))))

(defun tlon-babel-remove-source-overlays ()
  "Remove all existing overlays in the source window."
  (remove-overlays (point-min) (point-max)))

(defun tlon-babel-current-window-line ()
  "Get the current line number in the window."
  (save-excursion
    (let ((end (point)))
      (move-to-window-line 0)
      (count-screen-lines (point) end))))

(defun tlon-babel-highlight-corresponding-sentence ()
  "Highlight the corresponding sentence in the source text and unhighlight others."
  (interactive)
  (let* ((source-window (cadr (window-list)))
	 (target-window (car (window-list)))
	 (target-sentence-index)
	 (overlay (make-overlay (point) (point)))
	 (target-window-line (tlon-babel-current-window-line)))
    (with-selected-window target-window
      (save-excursion
	(backward-sentence)
	(setq target-sentence-index (count-sentences (point-min) (point)))))
    (with-selected-window source-window
      (tlon-babel-remove-source-overlays)
      (let ((beg)
	    (end))
	;; +1 because otherwise `count-sentences' throws an error
	(goto-char (1+ (point-min)))
	(while (< (count-sentences (point-min) (point))
		  (+ target-sentence-index tlon-babel-sentence-highlight-offset))
	  (forward-sentence))
	(setq beg (point))
	(forward-sentence)
	(setq end (point))
	(move-overlay overlay beg end (current-buffer))
	(overlay-put overlay 'face 'highlight)
	(backward-sentence)
	(recenter target-window-line)))))

(defun tlon-babel-toggle-automatic-highlighting ()
  "Toggle automatic highlighting of corresponding sentences."
  (interactive)
  (if tlon-babel-enable-automatic-highlighting
      (progn
	(remove-hook 'post-command-hook 'tlon-babel-highlight-corresponding-sentence t)
	(setq tlon-babel-enable-automatic-highlighting nil)
	(with-selected-window (cadr (window-list))
	  (tlon-babel-remove-source-overlays))
	(message "Automatic sentence highlighting disabled."))
    (add-hook 'post-command-hook 'tlon-babel-highlight-corresponding-sentence nil t)
    (setq tlon-babel-enable-automatic-highlighting t)
    (message "Automatic sentence highlighting enabled.")))

;;;;; Checking

(defun tlon-babel-check-branch (branch repo)
  "Throw an error unless current buffer is in REPO branch BRANCH."
  (let ((default-directory repo))
    (unless (string= (magit-get-current-branch) branch)
      (user-error "Please switch to the branch `%s' before proceeding" branch)
      t)))

(defun tlon-babel-check-file (&optional original)
  "Throw an error unless current file matches file in clock.
If ORIGINAL is non-nil, check that current file matches original; otherwise,
check that current file matches translation."
  (let* ((key (tlon-babel-get-clock-key))
	 (field (if original "original_path" "file"))
	 (expected-file (file-name-nondirectory
			 (tlon-babel-metadata-lookup field "original_key" key (tlon-babel-get-metadata-in-repo))))
	 (actual-file (file-name-nondirectory
		       (buffer-file-name))))
    (if (string= expected-file actual-file)
	t
      (user-error "Current file does not match file in clock"))))

(defun tlon-babel-check-label-and-assignee (repo)
  "Check that clocked action, user match label, assignee of issue in REPO."
  (save-window-excursion
    (let* ((default-directory repo)
	   (key (tlon-babel-get-clock-key))
	   (issue (format "Job: `%s" key))
	   (clocked-label (tlon-babel-get-clock-label)))
      (magit-status-setup-buffer repo)
      (magit-section-show-level-3-all)
      (goto-char (point-min))
      (if (search-forward issue nil t)
	  (let ((label (tlon-babel-ogh-get-label))
		(assignee (tlon-babel-user-lookup :name :github (tlon-babel-ogh-get-assignee))))
	    (unless (string= clocked-label label)
	      (user-error "The `org-mode' TODO says the label is `%s', but the actual issue label is `%s'"
			  clocked-label label))
	    (unless (string= user-full-name assignee)
	      (user-error "The `org-mode' TODO says the assignee is `%s', but the actual issue assignee is `%s'"
			  user-full-name assignee))
	    t)
	(user-error "No issue found for %s" key)))))

(defun tlon-babel-check-staged-or-unstaged (file)
  "Check if there are staged or unstaged modifications in repo involving FILE."
  (catch 'found
    (dolist (flag '("staged" ""))
      (let ((git-command (format "git diff --%s --name-only %s" flag file)))
	(when (not (string-empty-p (shell-command-to-string git-command)))
	  (throw 'found t))))))

(defun tlon-babel-check-staged-or-unstaged-other-than (file)
  "Check if there are staged or unstaged modifications in repo not involving FILE."
  (let* ((default-directory (tlon-babel-get-repo-from-file file))
	 (all-changes (magit-git-str "diff" "HEAD" "--" "."))
	 (filtered-changes (magit-git-str "diff" "HEAD" "--" file)))
    (unless (string= all-changes filtered-changes)
      (user-error "There are staged or unstaged changes in repo. Please commit or stash them before continuing"))))

(defun tlon-babel-check-file-title-match  (&optional file)
  "Check that FILE matches its title.
If FILE is nil, check the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (base (file-name-base file))
	 (title (tlon-babel-metadata-get-field-value-in-file "title" file))
	 (slugified-title (tlon-core-slugify title)))
    (unless (or
	     (string= base slugified-title)
	     ;; for articles with duplicate titles
	     (string-match-p (concat "^" (regexp-quote slugified-title) "-[0-9]+$") base))
      (error "The file `%s' does not match its title" title))))

;;;;; Bibtex

(defun tlon-babel-bibtex-generate-autokey (author year title)
  "Generate a BibTeX key based on AUTHOR, YEAR, and TITLE."
  ;; TODO: check that they key doesn't already exist in all metadata
  (let* ((author (tlon-babel-bibtex-autokey-get-names author))
	 (year (tlon-babel-bibtex-autokey-get-year year))
	 (title (tlon-babel-bibtex-autokey-get-title title))
	 (autokey (concat bibtex-autokey-prefix-string
			  author
			  (unless (or (equal author "")
				      (equal year ""))
			    bibtex-autokey-name-year-separator)
			  year
			  (unless (or (and (equal author "")
					   (equal year ""))
				      (equal title ""))
			    bibtex-autokey-year-title-separator)
			  title)))
    (if bibtex-autokey-before-presentation-function
	(funcall bibtex-autokey-before-presentation-function autokey)
      autokey)))

(defun tlon-babel-bibtex-autokey-get-names (name)
  "Return formatted contents of NAME field."
  (if (string= "" name)
      name
    (let* ((case-fold-search t)
	   (name-list (mapcar #'bibtex-autokey-demangle-name
			      (split-string name "[ \t\n]+and[ \t\n]+")))
	   additional-name)
      (unless (or (not (numberp bibtex-autokey-names))
		  (<= (length name-list)
		      (+ bibtex-autokey-names
			 bibtex-autokey-names-stretch)))
	(setq name-list (nreverse (nthcdr (- (length name-list)
					     bibtex-autokey-names)
					  (nreverse name-list)))
	      additional-name bibtex-autokey-additional-names))
      (concat (mapconcat #'identity name-list
			 bibtex-autokey-name-separator)
	      additional-name))))

(defun tlon-babel-bibtex-autokey-get-year (year)
  "Get formatted contents of YEAR field."
  (substring year (max 0 (- (length year) bibtex-autokey-year-length))))

(defun tlon-babel-bibtex-autokey-get-title (title)
  "Get formatted contents of TITLE field."
  (let ((case-fold-search t))
    (if (string-match bibtex-autokey-title-terminators title)
	(setq title (substring title 0 (match-beginning 0))))
    (let ((counter 0)
	  (ignore-re (concat "\\`\\(?:"
			     (mapconcat #'identity
					bibtex-autokey-titleword-ignore "\\|")
			     "\\)\\'"))
	  titlewords titlewords-extra word)
      (while (and (or (not (numberp bibtex-autokey-titlewords))
		      (< counter (+ bibtex-autokey-titlewords
				    bibtex-autokey-titlewords-stretch)))
		  (string-match "\\b\\w+" title))
	(setq word (match-string 0 title)
	      title (substring title (match-end 0)))
	;; `bibtex-autokey-titleword-ignore'.
	(unless (let (case-fold-search)
		  (string-match ignore-re word))
	  (setq counter (1+ counter))
	  (if (or (not (numberp bibtex-autokey-titlewords))
		  (<= counter bibtex-autokey-titlewords))
	      (push word titlewords)
	    (push word titlewords-extra))))
      (unless (string-match "\\b\\w+" title)
	(setq titlewords (append titlewords-extra titlewords)))
      (mapconcat #'bibtex-autokey-demangle-title (nreverse titlewords)
		 bibtex-autokey-titleword-separator))))

(defun tlon-babel-bibtex-add-lang-id-to-bib-files ()
  "Supply missing Spanish `landid' field to all bib files."
  (interactive)
  (dolist (file `(,tlon-babel-file-fluid ,tlon-babel-file-stable))
    (with-current-buffer (or (find-buffer-visiting file)
			     (find-file-noselect file))
      (goto-char (point-min))
      (bibtex-map-entries 'tlon-babel-add-lang-id-to-entry))))

;; TODO: support arbitrary langs
(defun tlon-babel-add-lang-id-to-entry (&optional _ _ _)
  "Add `langid' field to entry at point, if appropriate.
If the field `landig' is present, the function does nothing; else, it sets the
`langid' field to `spanish' if the entry has either a `translation' or a
`translator' field, and to `english' otherwise."
  (unless (bibtex-text-in-field "langid")
    (if (or (bibtex-text-in-field "translation")
	    (bibtex-text-in-field "translator"))
	(bibtex-set-field "langid" "spanish")
      (bibtex-set-field "langid" "english"))))

;;;;; Search

(defun tlon-babel-search-issues (search-string &optional repo)
  "Search for SEARCH-STRING in GitHub REPO's issues and pull requests.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let* ((repo (or repo (tlon-babel-get-repo nil 'include-all)))
	 (default-directory repo))
    (forge-search search-string)))

(defun tlon-babel-search-commits (search-string &optional repo)
  "Search for SEARCH-STRING in REPO's commit history.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let ((default-directory (or repo default-directory)))
    (magit-log-all (list "--grep" search-string))))

(defun tlon-babel-search-commit-diffs (search-string &optional repo)
  "Search for SEARCH-STRING in REPO's commit diff history.
If REPO is nil, use the current repo."
  (interactive "sSearch commit diffs : ")
  (let ((default-directory (or repo default-directory)))
    (magit-log-all `("-S" ,search-string))))

(defun tlon-babel-search-files (search-string &optional repo)
  "Search for SEARCH-STRING in REPO files.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let* ((repo (or repo (tlon-babel-get-repo nil 'include-all))))
    (consult-ripgrep repo search-string)))

(defun tlon-babel-search-multi (search-string &optional repo)
  "Search for SEARCH-STRING in REPO files, commit history, and GitHub issues.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let ((repo (or repo (tlon-babel-get-repo nil 'include-all)))
	(win1 (selected-window)))
    (window-extras-split-if-unsplit)
    (tlon-babel-search-issues search-string repo)
    (split-window-below)
    (tlon-babel-search-commits search-string repo)
    (select-window win1)
    (tlon-babel-search-files search-string repo)))

(defun tlon-babel-commit-and-push (action file)
  "Commit and push modifications to FILE.
The commit message is ACTION followed by the name of FILE."
  (let* ((default-directory (tlon-babel-get-repo-from-file file)))
    (tlon-babel-check-staged-or-unstaged-other-than file)
    (when (string= (magit-get-current-branch) "main")
      ;; TODO: Replace interactive call with programmatic way of doing this
      (call-interactively #'magit-pull-from-upstream nil)
      (sleep-for 2))
    (magit-stage-file file)
    (tlon-babel-create-commit action file)
    (call-interactively #'magit-push-current-to-pushremote)))

(defun tlon-babel-create-commit (action file)
  "Create a commit modifications to FILE.
The commit message is ACTION followed by either FILE or its BibTeX key,
depending on whether the repo is of subtype `translations' or `biblio',
respectively."
  ;; we check for staged or unstaged changes to FILE because
  ;; `magit-commit-create' interrupts the process if there aren't any
  (when (tlon-babel-check-staged-or-unstaged file)
    (let* ((repo (tlon-babel-get-repo-from-file file))
	   (subtype (tlon-babel-repo-lookup :type :dir repo))
	   (file-or-key (pcase subtype
			  ('translations (tlon-babel-get-key-from-file file))
			  ('biblio (file-name-nondirectory file)))))
      (magit-commit-create (list "-m" (format "%s %s" action file-or-key))))))

;;;;; URL correspondences

(defun tlon-babel-parse-json (type file)
  "Parse JSON FILE using array TYPE."
  (let ((json-object-type 'hash-table)
	(json-key-type 'string)
	(json-array-type type)
	(json-false :json-false))
    (json-read-file file)))

(defun tlon-babel-get-keys (data)
  "Get keys from hash table DATA."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) data)
    keys))

(defun tlon-babel-url-correspondence-dwim ()
  "Add a new URL correspondence or modify an existing one."
  (interactive)
  (let* ((data (tlon-babel-parse-json 'hash-table tlon-babel-file-url-correspondences))
	 (keys (tlon-babel-get-keys data))
	 (selected-key (completing-read "Select existing URL or enter a new one: " keys))
	 (default-value (gethash selected-key data))
	 (new-value (read-string (format "Enter value for key '%s': " selected-key) default-value)))
    (puthash selected-key new-value data)
    (with-temp-file tlon-babel-file-url-correspondences
      (insert "{\n")
      (maphash (lambda (k v)
		 (insert (format "  \"%s\": \"%s\",\n" k v)))
	       data)
      ;; Remove last comma
      (goto-char (- (point) 2))
      (delete-char 1)
      (insert "\n}")
      (write-file tlon-babel-file-url-correspondences)
      (tlon-babel-url-correspondence-commit))))

;; TODO: consider adapting `tlon-babel-commit-and-push' instead
(defun tlon-babel-url-correspondence-commit ()
  "Commit modifications in `url-correspondences.json'."
  (let ((default-directory (tlon-babel-get-property-of-repo-name :dir "babel-es")))
    ;; save all unsaved files in repo
    (magit-save-repository-buffers)
    (call-interactively #'magit-pull-from-upstream nil)
    ;; if there are staged files, we do not commit or push the changes
    (unless (magit-staged-files)
      (tlon-babel-check-branch "main" default-directory)
      (magit-run-git "add" tlon-babel-file-url-correspondences)
      (let ((magit-commit-ask-to-stage nil))
	(magit-commit-create (list "-m" "Update URL correspondences"))))))

(defun tlon-babel-highlight-url-correspondences ()
  "Highlight source URLs in URL correspondences file."
  (interactive)
  ;; Load JSON file
  (let* ((json-data (tlon-babel-parse-json 'hash-table tlon-babel-file-url-correspondences))
	 (key-urls (tlon-babel-get-keys json-data))
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

(defun tlon-babel-section-correspondence-dwim ()
  "Add a new section correspondence or modify an existing one."
  (interactive)
  (let* ((data (tlon-babel-parse-json 'list tlon-babel-file-section-correspondences))
	 (selected-key (citar-select-refs)))
    (tlon-babel-section-correspondence-check selected-key)
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
      (with-temp-file tlon-babel-file-section-correspondences
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
	(write-file tlon-babel-file-section-correspondences)
	;; (tlon-babel-url-correspondence-commit)
	))))

(defun tlon-babel-section-correspondence-check (key)
  "Check that selected BibTeX KEY is associated with the original work."
  (save-window-excursion
    (ebib-extras-open-key key)
    (let ((langid (ebib-extras-get-field-value "langid")))
      (unless (member langid '("english" "american"))
	(unless (y-or-n-p "The BibTeX entry you selected is not in English. In the `section-correspondences.json` file, you should use the BibTeX entry associated with the original work rather than with its translation. Are you sure you want to proceed?")
	  (user-error "Aborted"))))))

;;;;; Bibtex correspondences

(defun tlon-babel-read-json (file)
  "Read the json list in FILE."
  (let ((json-array-type 'list))
    (json-read-file file)))

(defun tlon-babel-write-json (file json)
  "Write the json list JSON to FILE."
  (with-temp-file file
    (insert (json-encode json))))

(defun tlon-babel-get-category-candidates (json category)
  "Get completion candidates from JSON for CATEGORY."
  (let (candidates)
    (dolist (entry json candidates)
      (when (string= (nth 2 entry) category)
	(push (cons (nth 0 entry) (nth 1 entry)) candidates)))))

(defun tlon-babel-edit-json-entry (file)
  "Edit a json entry in FILE."
  (interactive "f")
  (let* ((json (tlon-babel-read-json file))
	 (category (completing-read "Select category: " (mapcar (lambda (x) (nth 2 x)) json)))
	 (candidate (completing-read "Select or enter term: " (tlon-babel-get-category-candidates json category))))
    (let* ((existing (assoc candidate (tlon-babel-get-category-candidates json category)))
	   (translation (if existing
			    (read-string "Enter new translation: " (cdr existing))
			  (read-string "Enter new translation: "))))
      (if existing
	  (setcdr existing translation)
	(push (list candidate translation category) json))
      (tlon-babel-write-json file json))))

;;;;; dispatchers

;; TODO: add flag to set translation language, similar to Magit dispatch
(transient-define-prefix tlon-babel-dispatch ()
  "Dispatch a `tlon-babel' command."
  [["Main"
    ("j" "job"                            tlon-babel-jobs-create-job)
    ("r" "dwim"                           tlon-babel-jobs-dwim)
    ("m" "Magit"                          tlon-babel-magit-repo-dispatch)
    ("." "gh-notify"                      gh-notify)
    """Request"
    ("q q" "uqbar"                        tlon-babel-api-request)
    ]
   ["Add or modify"
    ("a a" "glossary"                     tlon-babel-glossary-dwim)
    ("a s" "section corresp"              tlon-babel-section-correspondence-dwim)
    ("a u" "URL corresp"                  tlon-babel-url-correspondence-dwim)
    """Search"
    ("s s" "multi"                        tlon-babel-search-multi)
    ("s c" "commits"                      tlon-babel-search-commits)
    ("s d" "commit-diffs"                 tlon-babel-search-commit-diffs)
    ("s f" "files"                        tlon-babel-search-files)
    ("s i" "issues"                       tlon-babel-search-issues)
    ("s t" "translation"                  tlon-babel-search-for-translation)
    ]
   ["Browse in Dired"
    ("d" "dir"                            tlon-babel-dired-dir-dispatch)
    ("H-d" "repo"                         tlon-babel-dired-repo-dispatch)
    ""
    """Browse externally"
    ("b" "current file"                   tlon-babel-browse-file)
    ("H-b" "current repo"                 tlon-babel-browse-repo)
    """Open"
    ("o" "in repo"                        tlon-babel-open-repo-dispatch)
    ("H-o" "in all repos"                 tlon-babel-open-file-in-all-repos)
    ]
   [
    "File changes"
    ("h h" "log"                          magit-log-buffer-file)
    ("h d" "diffs since last user change" tlon-babel-log-buffer-latest-user-commit)
    ("h e" "ediff with last user change"  tlon-babel-log-buffer-latest-user-commit-ediff)
    """Counterpart"
    ("f" "current win"                    tlon-babel-open-counterpart-dwim)
    ("H-f" "other win"                    tlon-babel-open-counterpart-other-window-dwim)
    """Translate"
    ("t t" "GPT"                          tlon-babel-gpt-translate)
    ("t w" "Web"                          tlon-babel-search-for-translation)]
   ["Clock"
    ("c c" "issue"                        tlon-babel-open-clock-issue)
    ("c f" "file"                         tlon-babel-open-clock-file )
    ("c o" "heading"                      org-clock-goto)
    """Issue"
    ("i i" "open counterpart"             tlon-babel-ogh-open-forge-counterpart)
    ("i I" "open file"                    tlon-babel-ogh-open-forge-file)
    ]
   ["Sync"
    ("y y" "visit or capture"             tlon-babel-ogh-visit-counterpart-or-capture)
    ("y v" "visit"                        tlon-babel-ogh-visit-counterpart)
    ("y p" "post"                         tlon-babel-ogh-create-issue-from-todo)
    ("y c" "capture"                      tlon-babel-ogh-capture-issue)
    ("y C" "capture all"                  tlon-babel-ogh-capture-all-issues)
    ("y r" "reconcile"                    tlon-babel-ogh-reconcile-issue-and-todo)
    ("y R" "reconcile all"                tlon-babel-ogh-reconcile-all-issues-and-todos)
    ("y x" "close"                        tlon-babel-ogh-close-issue-and-todo)]
   ]
  )

(transient-define-prefix tlon-babel-magit-repo-dispatch ()
  "Browse a Tlön repo in Magit."
  [["Babel"
    ("b c" "babel-core"                       tlon-babel-magit-browse-babel-core)
    ("b r" "babel-refs"                       tlon-babel-magit-browse-babel-refs)
    ("b s" "babel-es"                         tlon-babel-magit-browse-babel-es)
    ]
   ["Uqbar"
    ("q i" "uqbar-issues"                     tlon-babel-magit-browse-uqbar-issues)
    ("q f" "uqbar-front"                      tlon-babel-magit-browse-uqbar-front)
    ("q a" "uqbar-api"                        tlon-babel-magit-browse-uqbar-api)
    ("q n" "uqbar-en"                         tlon-babel-magit-browse-uqbar-en)
    ("q s" "uqbar-es"                         tlon-babel-magit-browse-uqbar-es)
    ]
   ["Utilitarismo"
    ("u n" "utilitarismo-en"                     tlon-babel-magit-browse-utilitarismo-en)
    ("u s" "utilitarismo-es"                     tlon-babel-magit-browse-utilitarismo-es)
    ]
   ["Ensayos sobre largoplacismo"
    ("e n" "ensayos-en"                     tlon-babel-magit-browse-ensayos-en)
    ("e s" "ensayos-es"                     tlon-babel-magit-browse-ensayos-es)
    ]
   ["EA News"
    ("n i" "ean-issues"                     tlon-babel-magit-browse-ean-issues)
    ("n f" "ean-front"                     tlon-babel-magit-browse-ean-front)
    ("n a" "ean-api"                     tlon-babel-magit-browse-ean-api)
    ]
   ["La Bisagra"
    ("s s" "bisagra"                     tlon-babel-magit-browse-bisagra)
    ]
   ["Docs"
    ("d d" "tlon-docs"                     tlon-babel-magit-browse-docs)
    ]
   ]
  )

(transient-define-prefix tlon-babel-open-repo-dispatch ()
  "Interactively open a file from a Tlön repo."
  [["Babel"
    ("b c" "babel-core"                       tlon-babel-open-file-in-babel-core)
    ("b r" "babel-refs"                       tlon-babel-open-file-in-babel-refs)
    ("b s" "babel-es"                         tlon-babel-open-file-in-babel-es)
    ]
   ["Uqbar"
    ("q i" "uqbar-issues"                     tlon-babel-open-file-in-uqbar-issues)
    ("q f" "uqbar-front"                      tlon-babel-open-file-in-uqbar-front)
    ("q a" "uqbar-api"                        tlon-babel-open-file-in-uqbar-api)
    ("q n" "uqbar-en"                         tlon-babel-open-file-in-uqbar-en)
    ("q s" "uqbar-es"                         tlon-babel-open-file-in-uqbar-es)
    ]
   ["Utilitarismo"
    ("u n" "utilitarismo-en"                     tlon-babel-open-file-in-utilitarismo-en)
    ("u s" "utilitarismo-es"                     tlon-babel-open-file-in-utilitarismo-es)
    ]
   ["Ensayos sobre largoplacismo"
    ("e n" "ensayos-en"                     tlon-babel-open-file-in-ensayos-en)
    ("e s" "ensayos-es"                     tlon-babel-open-file-in-ensayos-es)
    ]
   ["EA News"
    ("n i" "ean-issues"                     tlon-babel-open-file-in-ean-issues)
    ("n f" "ean-front"                     tlon-babel-open-file-in-ean-front)
    ("n a" "ean-api"                     tlon-babel-open-file-in-ean-api)
    ]
   ["La Bisagra"
    ("s s" "bisagra"                     tlon-babel-open-file-in-bisagra)
    ]
   ["Docs"
    ("d d" "tlon-docs"                     tlon-babel-open-file-in-docs)
    ]
   ]
  )

(transient-define-prefix tlon-babel-dired-repo-dispatch ()
  "Browse a Tlön repo in Dired."
  [["Babel"
    ("b c" "babel-core"                       tlon-babel-dired-browse-babel-core)
    ("b r" "babel-refs"                       tlon-babel-dired-browse-babel-refs)
    ("b s" "babel-es"                         tlon-babel-dired-browse-babel-es)
    ]
   ["Uqbar"
    ("q i" "uqbar-issues"                     tlon-babel-dired-browse-uqbar-issues)
    ("q f" "uqbar-front"                      tlon-babel-dired-browse-uqbar-front)
    ("q a" "uqbar-api"                        tlon-babel-dired-browse-uqbar-api)
    ("q n" "uqbar-en"                         tlon-babel-dired-browse-uqbar-en)
    ("q s" "uqbar-es"                         tlon-babel-dired-browse-uqbar-es)
    ]
   ["Utilitarismo"
    ("u n" "utilitarismo-en"                     tlon-babel-dired-browse-utilitarismo-en)
    ("u s" "utilitarismo-es"                     tlon-babel-dired-browse-utilitarismo-es)
    ]
   ["Ensayos sobre largoplacismo"
    ("e n" "ensayos-en"                     tlon-babel-dired-browse-ensayos-en)
    ("e s" "ensayos-es"                     tlon-babel-dired-browse-ensayos-es)
    ]
   ["EA News"
    ("n i" "ean-issues"                     tlon-babel-dired-browse-ean-issues)
    ("n f" "ean-front"                     tlon-babel-dired-browse-ean-front)
    ("n a" "ean-api"                     tlon-babel-dired-browse-ean-api)
    ]
   ["La Bisagra"
    ("s s" "bisagra"                     tlon-babel-dired-browse-bisagra)
    ]
   ["Docs"
    ("d d" "tlon-docs"                     tlon-babel-dired-browse-docs)
    ]
   ]
  )

(transient-define-prefix tlon-babel-dired-dir-dispatch ()
  "Browse a Tlön repo directory in Dired."
  [
   ;; ["Babel"
   ;; ("b c" "babel-core"                       tlon-babel-dired-babel-core)
   ;; ("b r" "babel-refs"                       tlon-babel-dired-babel-refs)
   ;; ("b s" "babel-es"                         tlon-babel-dired-babel-es)
   ;; ]
   ["Uqbar"
    ;; ("q i" "uqbar-issues"                     )
    ;; ("q f" "uqbar-front"                      )
    ;; ("q a" "uqbar-api"                        )
    ("q n" "uqbar-en"                         tlon-babel-browse-entity-in-uqbar-en-dispatch)
    ("q s" "uqbar-es"                         tlon-babel-browse-entity-in-uqbar-es-dispatch)
    ]
   ["Utilitarismo"
    ("u n" "utilitarismo-en"                     tlon-babel-browse-entity-in-utilitarismo-en-dispatch)
    ("u s" "utilitarismo-es"                     tlon-babel-browse-entity-in-utilitarismo-es-dispatch)
    ]
   ["Ensayos sobre largoplacismo"
    ("e n" "ensayos-en"                     tlon-babel-browse-entity-in-ensayos-en-dispatch)
    ("e s" "ensayos-es"                     tlon-babel-browse-entity-in-ensayos-es-dispatch)
    ]
   ;; ["EA News"
   ;; ("n i" "ean-issues"                     )
   ;; ("n f" "ean-front"                     )
   ;; ("n a" "ean-api"                     )
   ;; ]
   ;; ["La Bisagra"
   ;; ("s s" "bisagra"                     )
   ;; ]
   ;; ["Docs"
   ;; ("d d" "tlon-docs"                     )
   ;; ]
   ]
  )

(defmacro tlon-babel-generate-entity-dispatch (name)
  "Generate a dispatcher for browsing an entity named NAME in a repo."
  `(transient-define-prefix ,(intern (format "tlon-babel-browse-entity-in-%s-dispatch" name)) ()
     ,(format "Browse a directory in the `%s' repo." name)
     [["directories"
       ("a" "articles"         ,(intern (format "tlon-babel-dired-browse-articles-dir-in-%s" name)))
       ("t" "tags"             ,(intern (format "tlon-babel-dired-browse-tags-dir-in-%s" name)))
       ("u" "authors"          ,(intern (format "tlon-babel-dired-browse-authors-dir-in-%s" name)))
       ("c" "collections"      ,(intern (format "tlon-babel-dired-browse-collections-dir-in-%s" name)))
       ;; ("i" "images"           ,(intern (format "tlon-babel-dired-browse-images-dir-in-%s" name)))
       ]]
     ))

(dolist (repo (tlon-babel-get-property-of-repos :abbrev :type 'content))
  (eval `(tlon-babel-generate-entity-dispatch ,repo)))

(defun tlon-babel-browse-file ()
  "Browse the current file in the tlon-babel repository."
  (interactive)
  (if-let (file (buffer-file-name))
      (if-let ((repo (tlon-babel-get-repo-from-file file)))
	  (let* ((repo-name (tlon-babel-repo-lookup :name :dir repo))
		 (repo-url (concat "https://github.com/tlon-team/" repo-name))
		 (file-url (concat "/blob/main/" (file-relative-name (buffer-file-name) repo))))
	    (browse-url (concat repo-url file-url)))
	(user-error "File is not in a recognized repository"))
    (user-error "Buffer is not visiting a file")))

(defun tlon-babel-browse-repo ()
  "Browse a Babel repository.
If the current buffer is visiting a file in a Babel repository, browse that;
otherwise prompt for a repo."
  (interactive)
  (let* ((repo-name (tlon-babel-repo-lookup :name :dir (tlon-babel-get-repo))))
    (browse-url (concat "https://github.com/tlon-team/" repo-name))))

(defun tlon-babel-browse-entity-dir (entity &optional repo source-lang)
  "Browse the directory of ENTITY in REPO.
ENTITY should be passed as a string, in SOURCE-LANG, defaulting to English. If
REPO is nil, default to the current repository."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (source-lang (or source-lang "en"))
	 (target-lang (tlon-babel-repo-lookup :language :dir repo))
	 (dir (tlon-babel-get-bare-dir-translation target-lang source-lang entity))
	 (path (file-name-concat repo dir)))
    (dired path)))

(defmacro tlon-babel-generate-browse-entity-dir-commands (entity)
  "Generate commands to browse ENTITY dirs."
  (let ((command-name (intern (concat "tlon-babel-browse-dir-" entity))))
    `(defun ,command-name (&optional repo)
       ,(format "Browse the `%s' directory in REPO.
If REPO is nil, default to the current repository." entity)
       (interactive)
       (let ((repo (or repo (tlon-babel-get-repo))))
	 (tlon-babel-browse-entity-dir ,entity repo)))))

(dolist (entity (tlon-babel-get-entity-types))
  (eval `(tlon-babel-generate-browse-entity-dir-commands ,entity)))

(defun tlon-babel-open-file-in-repo (&optional repo)
  "Interactively open a file from a list of all files in REPO.
If REPO is nil, default to the current repository."
  (let* ((repo (or repo (tlon-babel-get-repo)))
	 (alist (tlon-babel-files-and-display-names-alist (list repo) repo)))
    (tlon-babel-open-file-in-alist alist)))

(defun tlon-babel-open-file-in-all-repos ()
  "Interactively open a file froma list of all files in all repos."
  (interactive)
  (let* ((alist (tlon-babel-files-and-display-names-alist
		 (tlon-babel-get-property-of-repos :dir)
		 tlon-core-repo-dirs)))
    (tlon-babel-open-file-in-alist alist)))

(defun tlon-babel-files-and-display-names-alist (dirs relative-path)
  "Return a alist of files, and display names, in DIRS.
The display names are constructed by subtracting the RELATIVE-PATH."
  (mapcar (lambda (file)
	    (cons (file-relative-name file relative-path) file))
	  (apply 'append (mapcar
			  (lambda (dir)
			    (directory-files-recursively dir "^[^.][^/]*$"))
			  dirs))))

(defun tlon-babel-open-file-in-alist (alist)
  "Interactively open a file from ALIST.
The car in each cons cell is the file to open, and its cdr is the candidate
presented to the user."
  (let* ((selection (completing-read "Select file: " alist))
	 (file (cdr (assoc selection alist))))
    (find-file file)))

(defmacro tlon-babel-generate-open-file-in-repo-commands (repo)
  "Generate commands to open file in REPO."
  (let* ((repo-name (tlon-babel-repo-lookup :abbrev :dir repo))
	 (command-name (intern (concat "tlon-babel-open-file-in-" repo-name))))
    `(defun ,command-name ()
       ,(format "Interactively open a file from a list of all files in `%s'" repo-name)
       (interactive)
       (tlon-babel-open-file-in-repo ,repo))))

(dolist (repo (tlon-babel-get-property-of-repos :dir))
  (eval `(tlon-babel-generate-open-file-in-repo-commands ,repo)))

;;;;;;; Fix log errors helper functions

(defun tlon-babel-collect-bibtex-keys-in-buffer ()
  "Collect all the bibtex keys in the current buffer.
Display the collected keys in a new buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (keys)
      (while (re-search-forward "@\\([[:alnum:]]+[[:digit:]]\\{4\\}[[:alnum:]]+\\)" nil t)
	(push (match-string 1) keys))
      (with-output-to-temp-buffer "*Bibtex keys*"
	(princ (mapconcat #'identity (delete-dups keys) "\n"))))))

(defun tlon-babel-move-bibtex-entries-to-babel ()
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
(defun tlon-babel-create-file-from-commit (file commit-hash)
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

;; TODO: move to relevant section
(defun tlon-babel-search-for-translation (string)
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

;;;;; Misc

(defun tlon-babel-historic-word-count (&optional repo-name days chars-per-word)
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
					 (tlon-babel-get-property-of-repos :name :type 'translations))))
	 (dir (tlon-babel-repo-lookup :dir :name repo-name))
	 (days (or days (read-number "How many days into the past? ")))
	 (chars-per-word (or chars-per-word 5.5))
	 (buffer (get-buffer-create "*Directory Size*"))
	 (script (file-name-concat (tlon-babel-get-property-of-repo-name :dir "babel")
				   "count/historic-word-count")))
    (shell-command (format "sh %s %s %s %s" script dir days chars-per-word) buffer)))

(provide 'tlon-babel)
;;; tlon-babel.el ends here
