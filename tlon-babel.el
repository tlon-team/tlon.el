;;; tlon-babel.el --- A companion package for the Babel project. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.14
;; Homepage: https://tlon.team
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

;;;; Requirements:

(require 'bibtex)
(require 'citar)
(require 'cl-lib)
(require 'consult)
(require 'dired)
(require 'doi-utils)
;; (require 'ebib-extras)
(require 'files-extras)
(require 'forge)
(require 'forge-search)
(require 'goldendict-ng)
(require 'gptel)
(require 'gptel)
(require 'json)
(require 'magit)
(require 'markdown-mode-extras)
(require 'org)
(require 'org-clock)
(require 'orgit-forge)
(require 'read-aloud)
(require 'request)
(require 'simple-extras)
(require 'subr-x)
(require 'tlon-core)
(require 'transient)
(require 'unfill)
(require 'url)
(require 'window-extras)
(require 'winum)

;;;; Customization:

;;;; User options

(defgroup tlon-babel ()
  "A companion package for the Babel project."
  :group 'files)

;;;; Variables

;;;;; Files and dirs

(defvar tlon-babel-dir-repos
  (let ((dir (pcase (expand-file-name "~")
	       ("/Users/pablostafforini" "Library/CloudStorage/Dropbox/repos/")
	       ("/Users/fede" "source/")
	       ("/Users/cartago" "Library/CloudStorage/Dropbox/repos/")
	       (_ (user-error "Home directory does not match that of known user")))))
    (file-name-concat (getenv "HOME") dir))
  "Directory where the Tlön repos are stored.")

(defvar tlon-babel-repos
  `((:name "babel-core"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-core"
	   :type meta
	   :key "b c")
    (:name "babel-refs"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-refs"
	   :type meta
	   :subtype biblio
	   :key "b r")
    (:name "babel-es"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-es"
	   :type meta
	   :language "es"
	   :key "b s")
    (:name "uqbar-issues"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-issues"
	   :type development
	   :subtype issues
	   :key "q i")
    (:name "uqbar-front"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-front"
	   :type development
	   :subtype front
	   :key "q f")
    (:name "uqbar-api"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-api"
	   :type development
	   :subtype api
	   :key "q a")
    (:name "uqbar-en"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-en"
	   :type content
	   :subtype originals
	   :language "en"
	   :key "q n")
    (:name "uqbar-es"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-es"
	   :type content
	   :subtype translations
	   :language "es"
	   :key "q s")
    (:name "utilitarismo-en"
	   :project "babel"
	   :subproject "utilitarismo"
	   :abbrev "utilitarismo-en"
	   :type content
	   :subtype originals
	   :key "u n")
    (:name "utilitarismo"
	   :project "babel"
	   :subproject "utilitarismo"
	   :abbrev "utilitarismo-es"
	   :type content
	   :subtype translations
	   :key "u s")
    (:name "ensayos-sobre-largoplacismo-en"
	   :project "babel"
	   :subproject "ensayos-sobre-largoplacismo"
	   :abbrev "ensayos-en"
	   :type content
	   :subtype originals
	   :key "l n")
    (:name "ensayos-sobre-largoplacismo-es"
	   :project "babel"
	   :subproject "ensayos-sobre-largoplacismo"
	   :abbrev "ensayos-es"
	   :type content
	   :subtype translations
	   :key "l s")
    (:name "ea.news-issues"
	   :project "other"
	   :subproject "ea.news"
	   :abbrev "ean-issues"
	   :type development
	   :subtype issues
	   :key "e i")
    (:name "ea.news-front"
	   :project "other"
	   :subproject "ea.news"
	   :abbrev "ean-front"
	   :type development
	   :subtype front
	   :key "f")
    (:name "ea.news-api"
	   :project "other"
	   :subproject "ea.news"
	   :abbrev "ean-api"
	   :type development
	   :subtype api
	   :key "a")
    (:name "bisagra"
	   :project "other"
	   :subproject "bisagra"
	   :abbrev "bisagra"
	   :type misc
	   :key "b")
    (:name "tlon-docs"
	   :project "other"
	   :subproject "docs"
	   :abbrev "docs"
	   :type docs
	   :key "d"))
  "List of repos and associated properties.
The `:name' property is the full name of the repo, as it appears in the URL. The
`:abbrev' property is an abbreviated form of the name, used, for example, for
creating `org-mode' TODOs.")

(defvar tlon-babel-labels
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

(defvar tlon-babel-bare-dirs
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

(defun tlon-babel-set-dir (repo)
  "Set the `:directory' property for REPO in `tlon-babel-repos'."
  (let* ((dir (file-name-as-directory
	       (file-name-concat tlon-babel-dir-repos
				 (plist-get repo :name)))))
    (plist-put repo :dir dir)))

(mapc #'tlon-babel-set-dir tlon-babel-repos)

(defun tlon-babel-get-entity-types ()
  "Return a list of entity types."
  (let (collection)
    (dolist (list tlon-babel-bare-dirs)
      (dolist (cons list)
	(when (string= (car cons) "en")
	  (push (cdr cons) collection))))
    collection))

(defmacro tlon-babel-generate-repo-commands (name dir)
  `(progn
     (defun ,(intern (format "tlon-babel-dired-browse-%s" name)) ()
       ,(format "Browse the %s repository in Dired." name)
       (interactive)
       (dired ,dir))
     (defun ,(intern (format "tlon-babel-magit-browse-%s" name)) ()
       ,(format "Browse the %s repository in Magit." name)
       (interactive)
       (magit-status ,dir))))

(dolist (repo tlon-babel-repos)
  (eval `(tlon-babel-generate-repo-commands
	  ,(plist-get repo :abbrev)
	  ,(plist-get repo :dir))))

(defmacro tlon-babel-generate-dir-commands (name dir entity)
  `(progn
     (defun ,(intern (format "tlon-babel-dired-browse-%s-dir-in-%s" entity name)) ()
       ,(format "Browse the `%s' directory in the `%s' repository." entity name)
       (interactive)
       (tlon-babel-browse-entity-dir ,entity ,dir))))

(dolist (repo tlon-babel-repos)
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
  (apply #'tlon-babel-plist-lookup tlon-babel-repos prop props-values))

(defun tlon-babel-user-lookup (prop &rest props-values)
  "Return the value of PROP in users matching one or more PROPS-VALUES pairs."
  (apply #'tlon-babel-plist-lookup tlon-babel-users prop props-values))

(defun tlon-babel-label-lookup (prop &rest props-values)
  "Return the value of PROP in labels matching one or more PROPS-VALUES pairs.."
  (apply #'tlon-babel-plist-lookup tlon-babel-labels prop props-values))

(defun tlon-babel-get-property-of-repo (prop repo)
  "Return the value of PROP in REPO."
  (tlon-babel-plist-lookup tlon-babel-repos prop :dir repo))

(defun tlon-babel-get-property-of-repo-name (prop repo-name)
  "Return the value of PROP in REPO-NAME.
REPO-NAME is named in its abbreviated form, i.e. the value of `:abbrev' rather
than `:name'."
  (tlon-babel-plist-lookup prop tlon-babel-repos :abbrev repo-name))

(defun tlon-babel-get-property-of-user (prop user)
  "Return the value of PROP in USER."
  (tlon-babel-plist-lookup prop tlon-babel-users :name user))

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
  "Return a list of all PROP values in `tlon-babel-repos'.
Optionally, return only the subset of values such that TARGET-PROP matches
TARGET-VALUE."
  (tlon-babel-get-property-of-plists prop tlon-babel-repos target-prop target-value))

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

(defvar tlon-babel-manual-processing-id
  "60251C8E-6A6F-430A-9DB3-15158CC82EAE"
  "Org ID of the `processing' heading in `manual.org'.")

(defvar tlon-babel-jobs-id
  "820BEDE2-F982-466F-A391-100235D4C596"
  "ID of the `jobs' heading in `jobs.org'.")

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

;;;;; API

(defconst tlon-babel-eaf-api-url
  "https://forum.effectivealtruism.org/graphql"
  "URL for the EAF GraphQL API endpoint.")

(defvar tlon-babel-eaf-objects
  '(post tag)
  "List of entities supported by the EAF GraphQL API.")

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

;;;;; YAML

(defconst tlon-babel-publication-statuses
  '("no publicado" "prueba" "produccion")
  "List of publication statuses.")

(defconst tlon-babel-yaml-delimiter "---\n"
  "Delimiter for YAML front matter.")

(defconst tlon-babel-yaml-article-keys
  '("titulo" "autores" "traductores" "temas" "fecha" "path_original" "key_original" "key_traduccion" "estado_de_publicacion" "descripcion")
  "List of YAML keys of fields to include in `uqbar-es' articles.
The order of the keys determines the sort order by
`tlon-babel--yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-tag-keys
  '("titulo" "titulo_breve" "path_original" "estado_de_publicacion")
  "List of YAML keys of fields to include in `uqbar-es' tags.
The order of the keys determines the sort order by
`tlon-babel--yaml-sort-fields', unless overridden.")

(defconst tlon-babel-yaml-author-keys
  '("titulo" "estado_de_publicacion")
  "List of YAML keys of fields to include in `uqbar-es' authors.
The order of the keys determines the sort order by
`tlon-babel--yaml-sort-fields', unless overridden.")

;;;;; Validation

(defconst tlon-babel-eaf-p
  "forum\\.effectivealtruism\\.org/"
  "Regular expression for validating EAF URLs.")

(defconst tlon-babel-eaf-post-id-regexp
  "\\([[:alnum:]]\\{17\\}\\)"
  "Regular expression for validating post IDs.")

(defconst tlon-babel-eaf-tag-slug-regexp
  "\\([[:alnum:]-]*\\)"
  "Regular expression for validating tag slugs.")

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
  (mapc #'tlon-babel-set-dir tlon-babel-repos)
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
  (if-let ((file (tlon-babel-metadata-lookup "file" "key_original" key (tlon-babel-get-metadata-in-repos))))
      (if-let ((repo (catch 'found
		       (dolist (dir (tlon-babel-get-property-of-repos :dir))
			 (when (string-prefix-p (file-name-as-directory dir) file)
			   (throw 'found dir))))))
	  repo
	(user-error "No repo found for key %s" key))
    (user-error "Metadata lookup for key `%s' returned nil" key)))

(defun tlon-babel-get-file-from-key (key)
  "Return the file path of KEY."
  (if-let ((file (tlon-babel-metadata-lookup "file" "key_original" key
					     (tlon-babel-get-metadata-in-repos))))
      file
    (user-error "Metadata lookup for key `%s' returned nil" key)))

(defun tlon-babel-get-key-from-file (file)
  "Return the bibtex key of FILE."
  (or
   ;; when in `translations'
   (tlon-babel-metadata-lookup "key_traduccion" "file" file (tlon-babel-get-metadata-in-repo))
   ;; when file in `originals'
   (let ((translation (tlon-babel-get-counterpart file)))
     (tlon-babel-metadata-get-field-value-in-file "key_original" translation))))

(defun tlon-babel-set-translation-language (lang)
  "Set the translation LANG."
  (interactive (list (completing-read "Language: " tlon-babel-languages)))
  (setq tlon-babel-translation-language lang))

;;;;;; get file paths

;; Some file paths cannot be set as vars because they change dynamically
;; depending on the value of `tlon-babel-gpt-translate-file'. We define the
;; relevant functions here.

(defun tlon-babel-get-file-glossary (&optional lang)
  "Return the file containing the glossary for LANG.
If LANG is nil, default to the language set in
`tlon-babel-translation-language'."
  (let ((lang (or lang tlon-babel-translation-language))
	(repo (tlon-babel-repo-lookup :dir
				      :subproject "babel"
				      :language tlon-babel-translation-language)))
    (file-name-concat repo "dict/Glossary.csv")))

;;;;;; Create/visit todos

(defun tlon-babel-visit-counterpart-or-capture ()
  "Visit the issue associated with TODO, or vice versa, creating TODO if necessary."
  (interactive)
  (tlon-babel-todo-issue-funcall #'tlon-babel-visit-issue
				 #'tlon-babel-visit-todo-or-capture))

(defun tlon-babel-visit-todo-or-capture ()
  "Visit the TODO associated with the current issue, creating one if necessary."
  (if-let ((pos (tlon-babel-get-todo-position-from-issue)))
      (tlon-babel-visit-todo pos)
    (tlon-babel-capture-issue)))

(defun tlon-babel-get-todo-position-from-issue (&optional issue)
  "Get the TODO position of ISSUE, using the appropriate method.
If the issue is a job, use the heading name, else use the `orgit-topic' ID. If
ISSUE is nil, use the issue at point."
  (when-let ((issue (or issue (forge-current-topic))))
    (if (tlon-babel-issue-is-job-p issue)
	(tlon-babel-get-todo-position
	 (tlon-babel-make-todo-name-from-issue nil 'no-state issue)
	 (tlon-babel-get-todos-jobs-file))
      (tlon-babel-get-todo-position
       (oref issue id)
       (tlon-babel-get-todos-generic-file) 'loose))))

(defun tlon-babel-visit-todo (&optional pos file)
  "Visit TODO at POS in FILE.
If POS is nil, use the position of the TODO associated with the issue at point.
If FILE is nil, use the file where the issue at point would be stored (depending
on whether or not is a job)."
  (if-let ((pos (or pos (tlon-babel-get-todo-position-from-issue)))
	   (file (or file (tlon-babel-get-todos-file-from-issue))))
      (tlon-babel-open-todo file pos)
    (user-error "No TODO found")))

(defun tlon-babel-capture-issue (&optional issue)
  "Create a new `org-mode' TODO based on ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer.

This command triggers one of two `org-capture' capture templates, depending on
whether the issue is or is not a job. If it is a job, it will process it as new
job if it has neither a label nor an assignee, else it will refile it under the
appropriate heading."
  (interactive)
  (let ((issue (or issue (forge-current-topic))))
    (if (tlon-babel-issue-is-job-p issue)
	(tlon-babel-create-job-todo-from-issue issue)
      (tlon-babel-create-generic-todo-from-issue issue))))

(defun tlon-babel-capture-all-issues ()
  "Capture all issues in the current repository."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo 'error 'include-all))
	(current-user (tlon-babel-user-lookup :github :name user-full-name))
	(num-captured 0))
    (dolist (issue (tlon-babel-get-open-issues))
      (let ((assignee (tlon-babel-forge-get-assignee issue))
	    (issue-name (tlon-babel-get-issue-name issue)))
	(when (not assignee)
	  (if (y-or-n-p (format "Issue `%s' has no assignee. Assign to you?" issue-name))
	      (progn
		(tlon-babel-assign-issue issue current-user)
		(while (not (string= current-user assignee))
		  (setq assignee (tlon-babel-forge-get-assignee issue))
		  (sleep-for 1)))
	    (message "Issue `%s' skipped: assigned to no one." issue-name)))
	(if (and (string= current-user assignee)
		 (not (tlon-babel-get-todo-position-from-issue issue)))
	    (save-window-excursion
	      (tlon-babel-capture-issue issue)
	      (message "Issue `%s' captured." issue-name)
	      (setq num-captured (1+ num-captured)))
	  (message "Issue `%s' skipped: assigned to %s." issue-name assignee))))
    (message "%s issues captured." num-captured)))

(defun tlon-babel-get-open-issues ()
  "Return a list of all open issues in the current repository."
  (let* ((repo (forge-get-repository nil))
	 (all-issues (forge-ls-topics repo 'forge-issue)))
    (seq-filter (lambda (issue)
		  (string= (oref issue state) "open"))
		all-issues)))

(defun tlon-babel-issue-is-job-p (&optional issue)
  "Return t if ISSUE at point is a job.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (when (string-match-p "^Job: " (oref issue title))
      t)))

(defun tlon-babel-create-job-todo-from-issue (&optional issue)
  "Create a new `org-mode' job TODO based on ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (tlon-babel-check-label-or-assignee-present issue)
    (tlon-babel-check-label-present issue)
    (when (tlon-babel-capture-issue-p issue)
      (tlon-babel-store-or-refile-job-todo issue))))

(defun tlon-babel-create-generic-todo-from-issue (&optional issue)
  "Create a new `org-mode' generic TODO based on ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (when (tlon-babel-capture-issue-p issue)
    (let ((label (tlon-babel-forge-get-label issue))
	  (issue (or issue (forge-current-topic))))
      (unless (tlon-babel-is-valid-status-p label issue)
	(tlon-babel-set-label (tlon-babel-set-status-label) issue)
	(forge-pull-topic issue)
	(while (not (tlon-babel-is-valid-status-p label issue))
	  (sleep-for 1))))
    (tlon-babel-store-todo "tbG" nil issue)))

(defun tlon-babel-check-label-or-assignee-present (&optional issue)
  "Check that ISSUE has a label or an assignee.
If not, offer to process it as a new job.

If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (assignee (tlon-babel-user-lookup :name :github (tlon-babel-forge-get-assignee issue)))
	 (label (tlon-babel-forge-get-label issue)))
    (unless (and assignee label)
      (if (y-or-n-p "Process issue as a new job (this will assign the issue to you, add the label 'Awaiting processing', and create a new master TODO in your org mode file)?")
	  (save-window-excursion
	    (tlon-babel-store-master-job-todo 'set-topic)
	    (while (not (and (tlon-babel-forge-get-assignee)
			     (tlon-babel-forge-get-label)))
	      (sleep-for 1))
	    (tlon-babel-capture-issue issue))
	(user-error "Aborted")))))

(defun tlon-babel-assign-issue (&optional issue user)
  "Assign ISSUE to USER.
If ISSUE is nil, use the issue at point or in the current buffer."
  (interactive)
  (let ((issue (or issue (forge-current-topic))))
    (unless (tlon-babel-forge-get-assignee issue)
      (let ((user (or user
		      (completing-read (format "Issue `%s' has no assignee. Assign to "
					       (tlon-babel-get-issue-name issue))
				       (tlon-babel-get-property-of-users :github)))))
	(tlon-babel-set-assignee user issue)))))

(defun tlon-babel-capture-issue-p (&optional issue)
  "Return t iff ISSUE should be captured.
An issue should be captured either if it is assigned to the current user or if
the user says so when prompted.

If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (assignee (tlon-babel-user-lookup :name :github (tlon-babel-forge-get-assignee issue))))
    (if (string= user-full-name assignee)
	t
      (pcase (read-char-choice
	      (format "The assignee of `%s' is %s.\nSelf-assign? [y]es | no, and [c]apture | no, and do [n]ot capture"
		      (oref issue title) assignee)
	      '(?y ?c ?n))
	(?y (tlon-babel-set-assignee (tlon-babel-user-lookup :github :name user-full-name))
	    (while (not (tlon-babel-capture-issue-p issue))
	      (sleep-for 1)))
	(?c t)
	(?n nil)))))

(defun tlon-babel-check-label-present (&optional issue)
  "Check that ISSUE has a label.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (label (tlon-babel-forge-get-label issue)))
    (unless label
      (if (y-or-n-p "The topic has no label. Would you like to add one?")
	  (tlon-babel-set-label (tlon-babel-set-job-label))
	(user-error "Aborted")))))

(defun tlon-babel-store-or-refile-job-todo (&optional issue)
  "Refile TODO under appropriate heading, or create new master TODO if none exists.
If ISSUE is nil, use the issue at point or in the current buffer."
  (if-let* ((issue (or issue (forge-current-topic)))
	    (pos (tlon-babel-get-todo-position
		  (tlon-babel-make-todo-name-from-issue 'no-action 'no-state issue)
		  (tlon-babel-get-todos-jobs-file))))
      (save-window-excursion
	(tlon-babel-store-todo "tbJ" nil issue)
	(let* ((inhibit-message t))
	  (org-extras-refile-at-position pos)
	  (org-extras-refile-goto-latest)))
    (when (y-or-n-p (format "No master TODO found for issue `%s'. Create?" (oref issue title)))
      (tlon-babel-store-master-job-todo nil issue)
      (tlon-babel-capture-issue issue))))

(defun tlon-babel-get-todo-position (todo file &optional loose)
  "Return the position of TODO exactly matching heading in FILE.
If LOOSE is non-nil, return the position of the first TODO matching a substring
rather than strictly matching the heading."
  (if loose
      (tlon-babel-find-loose-headline-in-file todo file)
    (org-find-exact-headline-in-buffer todo (find-file-noselect file))))

(defun tlon-babel-find-loose-headline-in-file (todo file)
  "Move point to TODO in FILE matching TODO."
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\*+.*" todo) nil t)
	(point)))))

(defun tlon-babel-get-todo-position-substring (todo file)
  "Return the position of TODO matching a substring of heading in FILE."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (case-fold-search)
       (when (re-search-forward (concat "^\\*+ .*" todo ".*") nil t)
	 (point-marker))))))

(defun tlon-babel-get-todos-file-from-issue ()
  "Get the file where the current issue is or would be stored."
  (if (tlon-babel-issue-is-job-p)
      (tlon-babel-get-todos-jobs-file)
    (tlon-babel-get-todos-generic-file)))

(defun tlon-babel-get-todos-jobs-file ()
  "Get the file containing the jobs `org-mode' ID."
  (or tlon-babel-todos-jobs-file
      (tlon-babel-set-value-of-var 'tlon-babel-todos-jobs-id)
      (setq tlon-babel-todos-jobs-file
	    (tlon-babel-get-file-with-id tlon-babel-todos-jobs-id))))

(defun tlon-babel-get-todos-generic-file ()
  "Get the file containing the generic `org-mode' ID."
  (or tlon-babel-todos-generic-file
      (tlon-babel-set-value-of-var 'tlon-babel-todos-generic-id)
      (setq tlon-babel-todos-generic-file
	    (tlon-babel-get-file-with-id tlon-babel-todos-generic-id))))

(defun tlon-babel-set-value-of-var (var)
  "Signal an error if the value of VAR is not set."
  (unless (symbol-value var)
    (user-error "Please set the value of `%s'" (symbol-name var))))

(defun tlon-babel-get-file-with-id (id)
  "Return the file containing the heading with the given `org-mode' ID."
  (when-let ((location (org-roam-id-find id)))
    (car location)))

(defun tlon-babel-open-todo (file position)
  "Open FILE at TODO POSITION."
  (find-file file)
  (widen)
  (org-kill-note-or-show-branches)
  (goto-char position))

(defun tlon-babel-store-todo (template &optional no-action issue)
  "Store a new TODO using TEMPLATE.
If TODO already exists, signal an error. If NO-ACTION is non-nil, store a master
TODO. If ISSUE is non-nil, use it instead of the issue at point."
  (let ((issue (or issue (forge-current-topic))))
    (when (tlon-babel-get-todo-position-from-issue issue)
      (user-error "TODO `%s' already exists" (tlon-babel-get-issue-name issue)))
    (let ((todo (tlon-babel-make-todo-name-from-issue no-action nil issue)))
      (kill-new todo)
      (org-capture nil template))))

(defun tlon-babel-store-master-job-todo (&optional set-topic issue)
  "Create a new job master TODO.
If SET-TOPIC is non-nil, set topic label to `Awaiting processing' and assignee
to the current user. If ISSUE is non-nil, use the issue at point or in the
current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (todo (tlon-babel-make-todo-name-from-issue 'no-action 'no-state issue)))
    (if-let ((pos (tlon-babel-get-todo-position todo (tlon-babel-get-todos-jobs-file))))
	(tlon-babel-visit-todo pos)
      (save-window-excursion
	(when set-topic
	  (tlon-babel-set-initial-label-and-assignee))
	(tlon-babel-store-todo "tbJ" 'master-todo issue)))))

;;;;; Org-github integration

;;;;;; Movement

(defun tlon-babel-visit-issue (&optional number repo)
  "Visit Github issue.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (interactive)
  (forge-visit-issue (tlon-babel-get-issue number repo)))

(defun tlon-babel-get-issue (&optional number repo)
  "Get Github issue.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (when-let* ((number (or number
			  (tlon-babel-get-issue-number-from-heading)))
	      (repo (or repo
			(tlon-babel-get-repo-from-heading)))
	      (default-directory repo)
	      (forge-repo (forge-get-repository nil))
	      (issue-id (caar (forge-sql [:select [id] :from issue
						  :where (and (= repository $s1)
							      (= number $s2))]
					 (oref forge-repo id)
					 number))))
    (forge-get-topic issue-id)))

(defun tlon-babel-get-issue-buffer (&optional number repo)
  "Get Github issue buffer.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (save-window-excursion
    (tlon-babel-visit-issue number repo)
    (current-buffer)))

(defun tlon-babel-visit-counterpart ()
  "Visit the ID associated with TODO, or vice versa."
  (interactive)
  (tlon-babel-todo-issue-funcall #'tlon-babel-visit-issue
				 #'tlon-babel-visit-todo))

(defun tlon-babel-todo-issue-funcall (todo-fun issue-fun)
  "Call TODO-FUN or ISSUE-FUN depending on the current major mode."
  (pcase major-mode
    ('org-mode
     (unless (org-at-heading-p)
       (user-error "I could not find an `org-mode' heading at point"))
     (funcall todo-fun))
    ((or 'forge-topic-mode 'forge-issue-mode 'forge-issue-list-mode 'magit-status-mode)
     (unless (tlon-babel-get-issue-name)
       (user-error "I could not find a GitHub issue at point"))
     (funcall issue-fun))
    (_ (user-error "This command cannot be invoked in `%s`" major-mode))))

;;;;;; Get heading elements

(defun tlon-babel-get-element-from-heading (regexp)
  "Get element matching REGEXP from the heading at point."
  (when (org-at-heading-p)
    (let ((heading (substring-no-properties (org-get-heading t t t t))))
      (when (string-match regexp heading)
	(match-string 1 heading)))))

(defun tlon-babel-get-issue-number-from-heading ()
  "Get the GitHub issue number from the `org-mode' heading at point."
  (when-let ((issue-number (tlon-babel-get-element-from-heading "#\\([[:digit:]]\\{1,4\\}\\)")))
    (string-to-number issue-number)))

(defun tlon-babel-get-repo-from-heading ()
  "Get the repo from the heading at point."
  (let* ((abbrev-repo (tlon-babel-get-element-from-heading "^\\[\\(.*?\\)\\]")))
    (tlon-babel-repo-lookup :dir :abbrev abbrev-repo)))

(defun tlon-babel-get-issue-number-from-open-issues ()
  "Prompt user to select from a list of open issues and return number of selection."
  (let* ((default-directory (tlon-babel-get-repo nil 'include-all))
	 (repo (forge-get-repository 'full))
	 ;; Fetch all issues, but filter for open ones
	 (issue-list (mapcar #'(lambda (issue)
				 (cons (format "#%d %s"
					       (oref issue number)
					       (oref issue title))
				       (oref issue number)))
			     (cl-remove-if-not (lambda (issue)
						 (string= (oref issue state) "open"))
					       (oref repo issues))))
	 ;; Let the user select one
	 (selected-issue (cdr (assoc (completing-read "Select an issue: " issue-list) issue-list))))
    ;; Return the selected issue number
    selected-issue))

(defun tlon-babel-get-issues (&optional repo)
  "Return a list of all open issues in REPO.
If REPO is nil, use the current repository."
  (let* ((repo (or repo (forge-get-repository t)))
	 (issues (forge-ls-issues repo)))
    issues))

(defun tlon-babel-get-latest-issue (&optional repo)
  "Return the most recently created issue in REPO.
If REPO is nil, use the current repository."
  (let* ((issues (tlon-babel-get-issues repo))
	 (latest-issue (car (sort issues (lambda (a b)
					   (time-less-p
					    (date-to-time (oref b created))
					    (date-to-time (oref a created))))))))
    (list (oref latest-issue number) (oref latest-issue title))))

(defun tlon-babel-count-issues (&optional repo)
  "Return the number of open issues in REPO.
If REPO is nil, use the current repository."
  (length (tlon-babel-get-issues repo)))

;;;;;; Set heading elements

(defun tlon-babel-set-repo-in-heading ()
  "Set the repo in the heading at point if not already present."
  (when (and (org-at-heading-p)
	     (not (tlon-babel-get-repo-from-heading)))
    (let* ((repo-name (completing-read "Select repo: " (tlon-babel-get-property-of-repos :name)))
	   (abbrev-repo (tlon-babel-repo-lookup :abbrev :name repo-name)))
      (org-extras-goto-beginning-of-heading-text)
      (insert (format "[%s] " abbrev-repo)))))

(defun tlon-babel-set-issue-number-in-heading (issue-number)
  "Set ISSUE-NUMBER in heading at point if not already present."
  (unless (tlon-babel-get-issue-number-from-heading)
    (org-extras-goto-beginning-of-heading-text)
    ;; move past repo name
    (re-search-forward "\\[.+?\\] ")
    (insert (format "#%s " (number-to-string issue-number)))))


;;;;;; Close issues/todo

(defun tlon-babel-close-issue-and-todo ()
  "With point on either, close issue and associated TODO."
  (interactive)
  (tlon-babel-todo-issue-funcall
   #'tlon-babel-close-issue-and-todo-from-issue
   (lambda ()
     (tlon-babel-visit-counterpart)
     (tlon-babel-close-issue-and-todo-from-issue))))

(defun tlon-babel-close-issue-and-todo-from-issue ()
  "With point on issue, close issue and associated TODO."
  (let ((issue-number (tlon-babel-get-issue-number-from-heading))
	(repo (tlon-babel-get-repo-from-heading)))
    (tlon-babel-close-issue issue-number repo)
    (tlon-babel-visit-todo)
    (org-todo "DONE")
    (message "Closed issue and TODO.")))

;; shouldn’t this be done using the orgit-link rather than issue-number?
(defun tlon-babel-close-issue (issue-number repo)
  "Close the topic with ISSUE-NUMBER in REPO."
  (tlon-babel-visit-issue issue-number repo)
  (tlon-babel-forge-close-issue))

;;;;;; Set TODO statuses

(defvar tlon-babel-todo-statuses
  '("TODO" "IMPORTANT" "URGENT" "SOMEDAY" "MAYBE")
  "List of admissible TODO statuses.
Note that your `org-todo-keywords' user option should include these labels for
`org-mode' to recognize them, and that the buffer has to be refreshed after the
value of that option is reset.")

(defun tlon-babel-get-issue-status (&optional issue)
  "Get remote status of ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (if (eq (tlon-babel-forge-get-state) 'closed)
	"DONE"
      (if-let ((label (tlon-babel-forge-get-label issue)))
	  (upcase label)
	""))))

(defun tlon-babel-get-corresponding-label ()
  "Get TODO status for `org-mode' heading at point from corresponding issue."
  (save-window-excursion
    (tlon-babel-visit-issue)
    (tlon-babel-forge-get-label)))

(defun tlon-babel-is-valid-status-p (&optional status issue)
  "Return t iff STATUS it is a valid TODO status.
A status is valid iff it is a member of `tlon-babel-todo-statuses'. If STATUS is
nil, use the status of heading or issue at point.

If ISSUE is nil, use the issue at point or in the current buffer."
  (if-let ((status (or status (pcase major-mode
				('org-mode (org-get-todo-state))
				((or 'forge-topic-mode 'forge-issue-mode 'forge-issue-list-mode 'magit-status-mode)
				 (tlon-babel-forge-get-label issue))))))
      (when (or (member status tlon-babel-todo-statuses)
		(member status (mapcar #'downcase tlon-babel-todo-statuses)))
	t)
    nil))

;;;;; Re-sync

(defun tlon-babel-reconcile-issue-and-todo ()
  "With point on either, reconcile issue and associated TODO."
  (interactive)
  (tlon-babel-todo-issue-funcall
   (lambda ()
     (with-current-buffer (tlon-babel-get-issue-buffer)
       (tlon-babel-reconcile-issue-and-todo-from-issue)))
   #'tlon-babel-reconcile-issue-and-todo-from-issue))

(defun tlon-babel-reconcile-all-issues-and-todos ()
  "Reconcile all TODOs under `tlon-babel-todos-generic-id'."
  (interactive)
  (save-window-excursion
    (org-roam-id-open tlon-babel-todos-generic-id nil)
    (let ((level (org-current-level)))
      (call-interactively 'org-next-visible-heading)
      (while (> (org-current-level) level)
	(if (or (not (tlon-babel-get-issue))
		(member org-archive-tag (org-get-tags)))
	    (org-next-visible-heading 1)
	  (tlon-babel-reconcile-issue-and-todo)
	  (call-interactively 'org-next-visible-heading))))
    (message "Finished reconciling.")))

(defun tlon-babel-reconcile-issue-and-todo-from-issue ()
  "With point on issue, reconcile issue and associated TODO-NAME."
  (let ((issue-name (tlon-babel-make-todo-name-from-issue))
	(pos (tlon-babel-get-todo-position-from-issue)))
    (save-window-excursion
      (tlon-babel-visit-todo pos)
      (let ((todo-name (substring-no-properties (org-get-heading t nil t t))))
	(unless (string= issue-name todo-name)
	  (tlon-babel-reconcile-issue-and-todo-prompt issue-name todo-name))))))

(defun tlon-babel-reconcile-issue-and-todo-prompt (issue-name todo-name)
  "Prompt the user to reconcile discrepancies between ISSUE-NAME and TODO-NAME."
  (pcase (read-char-choice
	  (format "The issue differs from its todo. Keep (i)ssue | Keep (t)odo | (a)bort\nissue: `%s'\ntodo:  `%s' "
		  issue-name todo-name)
	  '(?i ?t ?a))
    (?i (tlon-babel-update-todo-from-issue issue-name))
    (?t (tlon-babel-update-issue-from-todo todo-name))
    (_ (user-error "Aborted"))))

(defun tlon-babel-update-todo-from-issue (issue-name)
  "Update TODO to match ISSUE-NAME."
  (let ((original-visual-line-mode visual-line-mode))
    (visual-line-mode -1)
    (save-window-excursion
      (beginning-of-line)
      (re-search-forward " ")
      (let ((tags (org-get-tags nil t)))
	(org-fold-show-subtree)
	(org-kill-line)
	(insert issue-name)
	(when tags
	  (org-set-tags tags)
	  (org-align-tags))
	(message "TODO updated"))
      (visual-line-mode original-visual-line-mode))))

(defun tlon-babel-update-issue-from-todo (todo-name)
  "Update ISSUE to match TODO-NAME."
  (user-error "This command has not yet been developed. Sorry!"))
;; TODO: Develop function. The below approach doesn't work because
;; `org-complex-heading-regexp' fails to match a heading

;; (string-match org-complex-heading-regexp (concat "* " todo-name))
;; (let ((title (match-string-no-properties 4))
;; (state (match-string-no-properties 2)))
;; (message "%s %s" title state)))

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

;;;;;;; Cleanup common

(defun tlon-babel-markdown-cleanup-common ()
  "Cleanup a buffer visiting an imported document.
These functions are to be called for all imported documents: both EAF and
non-EAF."
  (interactive)
  (tlon-babel-markdown-cleanup-unescape-chars)
  (tlon-babel-markdown-cleanup-unescape-lines)
  (tlon-babel-markdown-cleanup-remove-linebreaks)
  (tlon-babel-markdown-cleanup-convert-hyphens)
  (tlon-babel-markdown-cleanup-format-heading)
  (tlon-babel-markdown-cleanup-set-heading-levels)
  (tlon-babel-markdown-cleanup-remove-double-brackets)
  (tlon-babel-markdown-cleanup-remove-nonbreaking-spaces)
  (unfill-region (point-min) (point-max)))

(defun tlon-babel-markdown-cleanup-unescape-chars ()
  "Unescape relevant characters in the current buffer."
  ;; characters that need to be escaped
  (dolist (char '("." "[" "]" "$"))
    (let ((regexp (concat "\\\\\\" char)))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match char))))
  ;; characters that do not need to be escaped
  (dolist (char '("@" "\"" "'" "|" ">" "<" "~"))
    (let ((regexp (concat (regexp-quote "\\") char)))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match char)))))

(defun tlon-babel-markdown-cleanup-unescape-lines ()
  "Unescape consecutive empty lines."
  (goto-char (point-min))
  (while (re-search-forward "\\\\\n\\\\\n" nil t)
    (replace-match "\n\n")))

(defun tlon-babel-markdown-cleanup-remove-linebreaks ()
  "Remove extra line breaks in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n" nil t)
    (replace-match "\n\n")))

(defun tlon-babel-markdown-cleanup-format-heading ()
  "Remove boldfacing in headline elements."
  (goto-char (point-min))
  (while (re-search-forward "^\\(#\\{1,6\\} \\)\\*\\*\\(.*\\)\\*\\*$" nil t)
    (replace-match "\\1\\2")))

(defun tlon-babel-markdown-cleanup-convert-hyphens ()
  "Convert double and triple hyphens into en and em dashes, respectively."
  (dolist (cons '(("---" . "—")
		  ("--" . "–")))
    (goto-char (point-min))
    (while (re-search-forward (car cons) nil t)
      (replace-match (cdr cons)))))

(defun tlon-babel-markdown-cleanup-set-heading-levels ()
  "Promote or demote headings in the current buffer when appropriate.
Specifically, when the buffer contains at least one heading, demote all headings
if there is at least one level 1 heading, and promote all headings while there
are no level 2 headings and some headings level 3 or higher."
  (save-excursion
    (goto-char (point-min))
    (when (> (point-max) (markdown-next-heading)) ; buffer has at least one heading
      (goto-char (point-min))
      (when (re-search-forward "^# " nil t)
	(substitute-target-in-buffer "^#" "##"))
      (goto-char (point-min))
      (while (not (re-search-forward "^## " nil t))
	(substitute-target-in-buffer "^###" "##")))))

;; Not sure what the cause of these double brackets is; for now, just remove them
(defun tlon-babel-markdown-cleanup-remove-double-brackets ()
  "Remove consecutive double brackets."
  (dolist (string '("\\(\\]\\)\\]" "\\(\\[\\)\\["))
    (goto-char (point-min))
    (while (re-search-forward string nil t)
      (replace-match "\\1"))))

(defun tlon-babel-markdown-cleanup-remove-nonbreaking-spaces ()
  "Remove selected nonbreaking spaces."
  (goto-char (point-min))
  (while (re-search-forward "\\. \\([ \\[]\\)" nil t)
    (replace-match ".\\1")))

;;;;;;; Cleanup EA Forum

(defun tlon-babel-markdown-cleanup-eaf ()
  "Cleanup a buffer visiting an imported document from the EA Forum.
Please note that the order in which these functions are called is relevant. Do
not alter it unless you know what you are doing."
  (interactive)
  (tlon-babel-markdown-cleanup-fix-footnote-refs)
  (tlon-babel-markdown-cleanup-remove-text))

;; If problems arise, test against documents imported from these URLs:
;; https://forum.effectivealtruism.org/s/vSAFjmWsfbMrTonpq/p/u5JesqQ3jdLENXBtB
(defun tlon-babel-markdown-cleanup-fix-footnote-refs ()
  "Convert footnote references to valid Markdown syntax."
  (let* ((ref-number "[[:digit:]]\\{1,3\\}")
	 (ref-source (format "\\^\\[\\(%s\\)\\](#fn.*?){.*?}\\^" ref-number))
	 (ref-target (format "\\(%s\\)\\.  \\(::: \\)?{#fn.*?} " ref-number))
	 (find-replace `((,ref-source . "[^\\1]")
			 (,ref-target . "[^\\1]: "))))
    (dolist (elt find-replace)
      (goto-char (point-min))
      (while (re-search-forward (car elt) nil t)
	(replace-match (cdr elt))))))

(defun tlon-babel-markdown-cleanup-remove-text ()
  "Remove various strings of text."
  (dolist (string '("::: footnotes\n"
		    "{rev=\"footnote\"} :::"
		    "(#fnref[[:digit:]]\\{1,3\\})"
		    " \\[↩︎](#fnref-.*?){\\.footnote-backref}"
		    "\\[↩]"
		    " :::"
		    "————————————————————————

  ::: {.section .footnotes}"
		    "{.underline}"
		    "\\*This work is licensed under a \\[Creative Commons Attribution 4.0 International License.\\](https://creativecommons.org/licenses/by/4.0/)\\*\n\n"))
    (goto-char (point-min))
    (while (re-search-forward string nil t)
      (replace-match ""))))

(defun tlon-babel-split-footnotes-into-separate-paragraphs ()
  "Split footnotes into separate paragraphs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[\\^[[:digit:]]\\{1,3\\}\\]:\\)" nil t)
      (replace-match "\n\n\\1"))))

(defun tlon-babel-consolidate-all-footnotes (dir)
  "Consolidate all footnotes in DIR."
  (interactive "D")
  (dolist (file (directory-files dir nil "\\.md$"))
    (with-current-buffer (find-file-noselect file)
      (message "Consolidating footnotes in %s" (buffer-name))
      (tlon-babel-consolidate-footnotes)
      (save-buffer))))

(defun tlon-babel-consolidate-footnotes ()
  "Consolidate consecutive footnotes."
  (interactive)
  (goto-char (point-min))
  (let ((regex "\\[\\^\\([[:digit:]]\\{1,3\\}\\)\\]\\ ?\\[\\^\\([[:digit:]]\\{1,3\\}\\)\\]"))
    (while (re-search-forward regex nil t)
      (let* ((n1 (string-to-number (match-string-no-properties 1)))
	     (n2 (string-to-number (match-string-no-properties 2))))
	(replace-match "" nil nil)
	(let* ((fn1 (tlon-babel-markdown-get-footnote n1 'delete))
	       (fn2 (tlon-babel-markdown-get-footnote n2 'delete))
	       (consolidated (tlon-babel-consolidate-bibtex-keys (format "%s; %s" fn1 fn2))))
	  (markdown-insert-footnote)
	  (insert (format "%s." consolidated))
	  (goto-char (point-min)))))))

(defun tlon-babel-markdown-get-footnote (n &optional delete)
  "Get the content of footnote number N.
If DELETE is non-nil, delete the footnote."
  (save-excursion                         ; Preserve initial position
    (goto-char (point-min))               ; Go to beginning of buffer
    (let ((footnote-start)
	  (footnote-end)
	  (footnote-content))
      ;; Locate the footnote
      (unless (re-search-forward (format "\\[\\^%d\\]:\\ " n) nil t)
	(error (format "Footnote %d not found" n)))
      (setq footnote-start (point))
      ;; Locate end of footnote content
      (if (re-search-forward "\\[\\^[[:digit:]]\\{1,3\\}\\]:\\ " nil t)
	  (goto-char (match-beginning 0))
	(goto-char (point-max)))
      (setq footnote-end (if (bolp) (- (point) 1) (point)))
      ;; Extract footnote content
      (setq footnote-content (buffer-substring-no-properties footnote-start footnote-end))
      (when delete
	(tlon-babel-markdown-delete-footnote n))
      footnote-content)))

(defun tlon-babel-markdown-delete-footnote (n)
  "Delete footnote number N."
  (save-excursion                         ; Preserve initial position
    (goto-char (point-min))               ; Go to beginning of buffer
    (let (footnote-start)
      ;; Locate the footnote
      (unless (re-search-forward (format "\\[\\^%d\\]:\\ " n) nil t)
	(error (format "Footnote ^%d not found" n)))
      (setq footnote-start (match-beginning 0))
      ;; Locate end of footnote content
      (if (re-search-forward "\\[\\^[[:digit:]]\\{1,3\\}\\]:\\ " nil t)
	  (goto-char (match-beginning 0))
	(goto-char (point-max)))
      (delete-region footnote-start (point)))))

(defun tlon-babel-consolidate-bibtex-keys (string)
  "Consolidate Bibtex keys in STRING."
  (let ((start 0)
	matches)
    (while (string-match "\\[\\(@.*?\\)\\]" string start)
      (push (match-string 1 string) matches)
      (setq start (match-end 0)))
    (format "[%s]" (mapconcat 'identity (nreverse matches) "; "))))

(defun tlon-babel-make-list ()
  "Format the current paragraph into a proper list."
  (interactive)
  (save-excursion
    (let ((beg (progn (backward-paragraph) (point)))
	  (end (progn (forward-paragraph) (point))))
      (goto-char beg)
      (replace-regexp-in-region " - " "\n- " beg end))))

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

;;;;;; Insertion commands

(defun tlon-babel-markdown-insert-element ()
  "Insert a link to an element at point.
The element can be a tag or an author."
  (interactive)
  (tlon-babel-check-in-markdown-mode)
  (let* ((selection (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))
	 (current-link (markdown-link-at-pos (point)))
	 (current-desc (nth 2 current-link))
	 (current-target (nth 3 current-link))
	 current-element-title)
    (when current-target
      (setq current-element-title
	    (tlon-babel-markdown-get-title-in-link-target
	     current-target)))
    (let* ((new-element-title (completing-read "Selection: " (tlon-babel-get-all-uqbar-entities)
					       nil t
					       (or current-element-title
						   selection)))
	   (new-target-file (tlon-babel-metadata-lookup "file" "titulo" new-element-title (tlon-babel-get-metadata-in-repo)))
	   (new-target-dir (file-relative-name
			    (file-name-directory new-target-file) (file-name-directory (buffer-file-name))))
	   (new-target (file-name-concat new-target-dir (file-name-nondirectory new-target-file)))
	   (new-desc (if (and current-desc (string= new-target current-target))
			 current-desc
		       (or selection new-element-title)))
	   (link (format "[%s](%s)" new-desc new-target)))
      (when current-target
	(markdown-mode-extras-delete-link))
      (when selection
	(delete-region (region-beginning) (region-end)))
      (insert link))))

(defun tlon-babel-markdown-get-title-in-link-target (target)
  "Return the title of the tag to which the TARGET of a Markdown link points."
  (let* ((file (expand-file-name target default-directory))
	 (title (tlon-babel-metadata-lookup "titulo" "file" file (tlon-babel-get-metadata-in-repo))))
    title))

(defun tlon-babel-markdown-sort-elements-in-paragraph (separator)
  "Sort the elements separated by SEPARATOR in the current paragraph."
  (save-excursion
    ;; Get paragraph boundaries
    (let* ((para-start (progn (backward-paragraph)
			      (skip-chars-forward "\n\t ")
			      (point)))
	   (para-end (progn (end-of-paragraph-text)
			    (point)))
	   ;; Get paragraph text, separate the links
	   (para-text (buffer-substring-no-properties para-start para-end))
	   (link-list (mapcar 'ucs-normalize-NFD-string (split-string para-text separator)))
	   ;; Trim and sort the links
	   (sorted-links (seq-sort-by 'downcase
				      (lambda (s1 s2)
					(string-collate-lessp s1 s2 nil t))
				      (mapcar 'string-trim link-list))))
      ;; Clear the current paragraph
      (delete-region para-start para-end)
      ;; Replace it with sorted links
      (goto-char para-start)
      (insert (mapconcat 'identity sorted-links separator)))))

(defun tlon-babel-markdown-sort-related-entries ()
  "Sort the links in the `related entries' section in current buffer.
If no section is found, do nothing."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^## Entradas relacionadas" nil t)
      (forward-paragraph)
      (tlon-babel-markdown-sort-elements-in-paragraph " • "))))

;;;;;;; Insert elements

(defun tlon-babel-markdown-insert-element-pair (open close)
  "Insert an element pair at point or around the selected region.
OPEN is the opening element and CLOSE is the closing element."
  (interactive)
  (tlon-babel-check-in-markdown-mode)
  (if (use-region-p)
      (let ((begin (region-beginning)))
	(goto-char (region-end))
	(insert close)
	(goto-char begin)
	(insert open))
    (insert (concat open close))
    (backward-char (length close))))

(defun tlon-babel-markdown-insert-mdx-cite ()
  "Insert an MDX `Cite' element pair at point or around the selected region.
When a Bibtex key is enclosed in a `Cite' element pair, only its title will be
displayed in the exported web page."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "<Cite>" "</Cite>"))

(defun tlon-babel-markdown-insert-mdx-aside ()
  "Insert an MDX `Aside' element pair at point or around the selected region."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "<Aside>" "</Aside>"))

(defun tlon-babel-markdown-insert-mdx-lang (language)
  "Insert an MDX `Lang' element pair at point or around the selected region.
Prompt the user to select a LANGUAGE. The enclosed text will be interpreted as
written in that language."
  (interactive (list (completing-read "Language: " (mapcar #'car tlon-babel-languages))))
  (tlon-babel-markdown-insert-element-pair (format "<Lang id=\"%s\">"
						   language)
					   "</Lang>"))

(defun tlon-babel-markdown-insert-mdx-small-caps ()
  "Insert an MDX `SmallCaps' element pair at point or around the selected region.
Text enclosed by an `SmallCaps' element pair will be displayed in small caps."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "<SmallCaps>" "</SmallCaps>"))

(defun tlon-babel-markdown-insert-mdx-footnote ()
  "Insert an MDX `Footnote' element pair at point or around the selected region.
Text enclosed by a `Footnote' element pair will be displayed as a footnote, as
opposed to a sidenote."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "<Footnote>" "</Footnote>"))

(defun tlon-babel-markdown-insert-mdx-sidenote ()
  "Insert an MDX `Sidenote' element pair at point or around the selected region.
Text enclosed by a `Sidenote' element pair will be displayed as a sidenote, as
opposed to a footnote."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "<Sidenote>" "</Sidenote>"))

(defun tlon-babel-markdown-insert-math-inline ()
  "Insert an inline math element pair at point or around the selected region."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "$`" "`$"))

(defun tlon-babel-markdown-insert-math-display ()
  "Insert a display math element pair at point or around the selected region."
  (interactive)
  (tlon-babel-markdown-insert-element-pair "$$\n" "\n$$"))

(defun tlon-babel-markdown-end-of-buffer-dwim ()
  "Move point to the end of the relevant part of the buffer.
The relevant part of the buffer is the part of the buffer that excludes the
\"local variables\" section.

If this function is called twice consecutively, it will move the point to the
end of the buffer unconditionally."
  (interactive)
  (let ((match (re-search-forward tlon-babel-local-variables-line-start nil t)))
    (if (or (not match) (eq this-command last-command))
	(goto-char (point-max))
      (goto-char (- (match-beginning 0) 1)))))

(transient-define-prefix tlon-babel-markdown-insert-dispatch ()
  "Dispatch a `tlon-babel' command for Markdown insertion."
  [["MDX"
    ("c" "cite"                 tlon-babel-markdown-insert-mdx-cite)
    ("l" "lang"                 tlon-babel-markdown-insert-mdx-lang)
    ("m" "small caps"           tlon-babel-markdown-insert-mdx-small-caps)
    ("f" "footnote"             tlon-babel-markdown-insert-mdx-footnote)
    ("s" "sidenote"             tlon-babel-markdown-insert-mdx-sidenote)
    ]
   ["Math"
    ("i" "inline"               tlon-babel-markdown-insert-math-inline)
    ("d" "display"              tlon-babel-markdown-insert-math-display)
    ]]
  )

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
				tlon-babel-repos)))
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
    (dolist (dir (tlon-babel-get-property-of-repos :dir :type 'translations))
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

(defun tlon-babel-alist-key (value alist)
  "Find the first key from ALIST that corresponds to VALUE."
  (cl-loop for (key . val) in alist
	   when (equal val value)
	   return key))

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
  (let ((key (tlon-babel-metadata-get-field-value-in-file "key_original")))
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
	  `(("fecha" . ,(lambda () (format-time-string "%FT%T%z")))
	    ("titulo" . ,(lambda () (or title (read-string "Título: "))))
	    ("authors-list" . ,(lambda () (tlon-babel-yaml-set-multi-value-field "titulo" "autores")))
	    ("traductores" . ,#'tlon-babel-yaml-set-translators)
	    ("temas" . ,#'tlon-babel-yaml-set-tags)
	    ("path_original" . ,#'tlon-babel-yaml-set-original-path)))
	 (processed-fields (if (member "autores" fields)
			       (cons "authors-list" fields)
			     fields))
	 (field-values (cl-loop for field in processed-fields
				for generator = (cdr (assoc field var-generators))
				if generator collect `(,field . ,(funcall generator)))))
    ;; calculate first-author and adjust field-values
    (let* ((first-author (cdr (or (assoc "autores" field-values) (assoc "authors-list" field-values))))
	   (autores (when first-author
		      (or
		       (cdr (assoc "autores" field-values))
		       (tlon-babel-elisp-list-to-yaml first-author))))
	   (cmpl-generators
	    `(("first-author" . ,first-author)
	      ("autores" . ,autores)
	      ("estado_de_publicacion" . "no publicado")
	      ("key_original" . ,(when first-author (tlon-babel-yaml-set-original-key (car first-author))))
	      ("key_traduccion" . ,(when first-author
				     (tlon-babel-bibtex-generate-autokey
				      (car first-author)
				      (substring (cdr (assoc "fecha" field-values)) 0 4)
				      (cdr (assoc "titulo" field-values))))))))
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
      ("autores" tlon-babel-yaml-author-keys))))

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
  "Set the value of `autores' YAML field."
  (tlon-babel-elisp-list-to-yaml
   (tlon-babel-yaml-set-multi-value-field "titulo" "autores")))

(defun tlon-babel-yaml-set-translators ()
  "Set the value of `traductores' YAML field."
  (tlon-babel-elisp-list-to-yaml
   (completing-read-multiple
    "Traductores: "
    (tlon-babel-metadata-get-all-field-values "traductores" (tlon-babel-get-metadata-in-repos)))))

(defun tlon-babel-yaml-set-tags ()
  "Set the value of `temas' YAML field."
  (tlon-babel-elisp-list-to-yaml
   (tlon-babel-yaml-set-multi-value-field "titulo" "temas")))

(defun tlon-babel-elisp-list-to-yaml (list)
  "Convert an Elisp LIST to a YAML list."
  (concat "[\"" (mapconcat 'identity list "\", \"") "\"]"))

(defun tlon-babel-yaml-set-original-path ()
  "Set the value of `path_original' YAML field."
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
  "Set the value of `key_original' YAML field.
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

(defun tlon-babel-yaml-set-publication (&optional state)
  "Set the value of `estado_de_publicacion' YAML field to STATE.
If STATE is nil, default to `borrador'."
  (let ((publicacion (completing-read "Publicación: " (tlon-babel-get-publicaciones))))
    (if (string= publicacion "Tlön")
	"online"
      publicacion)))

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
	  (markdown-narrow-to-subtree)
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
  (cl-destructuring-bind (key value) (tlon-babel-yaml-get-field)
    (tlon-babel-yaml-get-completions key value)))

(defun tlon-babel-yaml-get-completions (key value)
  "Get completions based on KEY.
If KEY already has VALUE, use it as the initial input."
  (if-let ((val (tlon-babel-yaml-get-completion-values key))
	   (fun (tlon-babel-yaml-get-completion-functions key)))
      (funcall fun val)
    (tlon-babel-yaml-insert-string (list value))))

(defun tlon-babel-yaml-get-completion-values (key)
  "Get completion values for a YAML field with KEY."
  (pcase key
    ("traductores" (tlon-babel-get-translators))
    ("temas" (tlon-babel-get-uqbar-tags))
    ("autores" (tlon-babel-get-uqbar-authors))
    ("path_original" (tlon-babel-get-filenames-in-dir))
    ("key_original" (citar--completion-table (citar--format-candidates) nil))
    ("key_traduccion" (citar--completion-table (citar--format-candidates) nil))
    ("estado_de_publicacion" tlon-babel-publication-statuses)
    (_ nil)))

(defun tlon-babel-yaml-get-completion-functions (key)
  "Get completion functions for a YAML field with KEY."
  (pcase key
    ((or "autores" "traductores" "temas") #'tlon-babel-yaml-insert-list)
    ((or "path_original" "key_original" "key_traduccion" "estado_de_publicacion") #'tlon-babel-yaml-insert-string)
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

(defun tlon-babel-yaml-get-field ()
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
  (cl-destructuring-bind (key value) (tlon-babel-yaml-get-field)
    (let* ((choice (completing-read (format "Value of `%s': " key)
				    candidates))
	   (bounds (bounds-of-thing-at-point 'line))
	   (line (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (delete-region (car bounds) (cdr bounds))
      (insert (format "%s:  %s\n" key choice)))))

;;;;;; Get repo-specific entities

;; TODO: handle changing type names in different langs
(defun tlon-babel-get-uqbar-entity (type)
  "Return a list of `uqbar-en' elements of TYPE."
  (tlon-babel-metadata-get-all-field-values
   "titulo"
   (tlon-babel-get-metadata-in-repo (tlon-babel-get-property-of-repo-name :dir "uqbar-es"))
   "file"
   (file-name-concat (tlon-babel-get-property-of-repo-name :dir "uqbar-es") type)))

(defun tlon-babel-get-uqbar-articles ()
  "Get a list of `uqbar-en' articles."
  (tlon-babel-get-uqbar-entity "articulos"))

(defun tlon-babel-get-uqbar-authors ()
  "Get a list of `uqbar-en' authors."
  (tlon-babel-get-uqbar-entity "autores"))

(defun tlon-babel-get-uqbar-tags ()
  "Get a list of `uqbar-en' tags."
  (tlon-babel-get-uqbar-entity "temas"))

(defun tlon-babel-get-all-uqbar-entities ()
  "Get a list of all `uqbar-en' entities."
  (append
   (tlon-babel-get-uqbar-entity "articulos")
   (tlon-babel-get-uqbar-entity "autores")
   (tlon-babel-get-uqbar-entity "temas")))

;;;;;; Create repo-specific entities

(defun tlon-babel-create-uqbar-entity (dir)
  "Create a new file for `uqbar-es' entity in DIR."
  (let ((default-directory (file-name-concat
			    (tlon-babel-get-property-of-repo-name :dir "uqbar-es")
			    (file-name-as-directory dir))))
    (files-extras-new-empty-buffer)
    (tlon-babel-yaml-set-front-matter-for-tag-or-author)
    (goto-char (point-max))
    (tlon-babel-name-file-from-title)
    (insert (format "**%s** es " (tlon-babel-metadata-get-field-value-in-file "titulo")))
    (save-buffer)))

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
   "titulo"
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

;; TODO: delete
(defun tlon-babel-get-work-type (&optional reversed file)
  "Return the work type of file in FILE.
A work is either `original' or `translation'. If REVERSED is non-nil, return
`originals' when the work type is `translations' and vice versa. If FILE is nil,
return the work type of the file visited by the current buffer."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-babel-get-repo-from-file file))
	 (repo-path (file-relative-name file repo))
	 (root-dir-in-repo-path (car (split-string repo-path "/"))))
    (pcase root-dir-in-repo-path
      ("originals" (if reversed "translations" "originals"))
      ("translations" (if reversed "originals" "translations")))))

(defun tlon-babel-get-counterpart (&optional file)
  "Get the counterpart file of FILE.
A file's counterpart is its translation if it is an original, and vice versa.
The translation language is defined by `tlon-babel-translation-language'.

If FILE is nil, return the counterpart of the file visited by the current
buffer."
  (let* ((file (or file (tlon-babel-buffer-file-name))))
    (if-let ((dir (tlon-babel-get-counterpart-dir file))
	     (locator (tlon-babel-metadata-get-field-value-in-file "path_original" file)))
	(file-name-concat dir locator)
      (let ((translations-repo (tlon-babel-get-counterpart-repo file)))
	(tlon-babel-metadata-lookup "file"
				    "path_original"
				    (file-name-nondirectory file)
				    (tlon-babel-get-metadata-in-repo translations-repo))))))

(defun tlon-babel-get-counterpart-repo (&optional file)
  "Get the counterpart repo of FILE.
A file's counterpart repo is the repo of that file's counterpart.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (let* ((file (or file (tlon-babel-buffer-file-name)))
	 (repo (tlon-babel-get-repo-from-file file))
	 (subtype (tlon-babel-get-property-of-repo :subtype repo))
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
		       (or file (dired-x-guess-file-name-at-point)))))
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

;;;;; EAF validation

(defun tlon-babel-eaf-p (url)
  "Return t if URL is an EAF URL, nil otherwise."
  (not (not (string-match tlon-babel-eaf-p url))))

(defun tlon-babel-eaf-post-id-p (identifier)
  "Return t if IDENTIFIER is a post ID, nil otherwise."
  (not (not (string-match (format "^%s$" tlon-babel-eaf-post-id-regexp) identifier))))

(defun tlon-babel-eaf-tag-slug-p (identifier)
  "Return t if IDENTIFIER is a tag slug, nil otherwise."
  (not (not (string-match (format "^%s$" tlon-babel-eaf-tag-slug-regexp) identifier))))

(defun tlon-babel-eaf-get-id-or-slug-from-identifier (identifier)
  "Return the EAF post ID or tag slug from IDENTIFIER, if found.
IDENTIFIER can be an URL, a post ID or a tag slug."
  (interactive "sURL: ")
  (if (simple-extras-string-is-url-p identifier)
      (or (tlon-babel-eaf-get-id-from-identifier identifier)
	  (tlon-babel-eaf-get-slug-from-identifier identifier))
    ;; return id or slug if identifier is an id or slug
    (pcase identifier
      ((pred tlon-babel-eaf-post-id-p) identifier)
      ((pred tlon-babel-eaf-tag-slug-p) identifier))))

(defun tlon-babel-eaf-get-id-from-identifier (identifier)
  "Return the EAF post ID from IDENTIFIER, if found."
  (when-let ((id (or (when (string-match (format "^.+?forum.effectivealtruism.org/posts/%s"
						 tlon-babel-eaf-post-id-regexp)
					 identifier)
		       (match-string-no-properties 1 identifier))
		     (when (string-match (format "^.+?forum.effectivealtruism.org/s/%s/p/%s"
						 tlon-babel-eaf-post-id-regexp tlon-babel-eaf-post-id-regexp)
					 identifier)
		       (match-string-no-properties 2 identifier)))))
    id))

(defun tlon-babel-eaf-get-slug-from-identifier (identifier)
  "Return the EAF tag slug from IDENTIFIER, if found."
  (when (string-match (format "^.+?forum.effectivealtruism.org/topics/%s"
			      tlon-babel-eaf-tag-slug-regexp)
		      identifier)
    (match-string-no-properties 1 identifier)))

(defun tlon-babel-eaf-get-object (id-or-slug)
  "Return the EAF object in ID-OR-SLUG."
  (let ((object (cond ((tlon-babel-eaf-post-id-p id-or-slug)
		       'post)
		      ((tlon-babel-eaf-tag-slug-p id-or-slug)
		       'tag)
		      (t (user-error "Not an ID or slug: %S" id-or-slug)))))
    object))

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
  "Get topic GID from `orgit-forge' link in heading at point."
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
  "Open the topic from `orgit-forge' link in heading at point."
  (interactive)
  (let ((default-directory (tlon-babel-get-repo))
	(topic (tlon-babel-get-clock-issue)))
    (forge-visit-issue topic)))

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

;;;;;

(defun tlon-babel-get-action-in-label (label)
  "Return action associated with LABEL."
  (let ((action (cadr (split-string label))))
    action))

(defun tlon-babel-get-issue-name (&optional issue)
  "Get the name of ISSUE.
An issue name is its number followed by its title.

If ISSUE is nil, get the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (title (oref issue title))
	 (number (oref issue number)))
    (format "#%s %s" number title)))

(defun tlon-babel-get-issue-link (&optional issue)
  "Get an `org-mode' link to ISSUE.
If ISSUE is nil, get the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (name (tlon-babel-get-issue-name issue))
	 (id (oref issue id)))
    (org-link-make-string (format "orgit-topic:%s" id) name)))

(defun tlon-babel-make-todo-name-from-issue (&optional no-action no-state issue)
  "Construct the name of TODO from ISSUE.
For job TODOs, the resulting name will have a name with the form \"[REPO] ACTION
NAME\". ACTION is optional, and used only for job TODOs. For example, if the
TODO is \"[uqbar-es] #591 Job: `Handbook2022ExerciseForRadical`\", and ACTION is
\"Process\", the function returns \"[uqbar-es] Process #591 Job:
`Handbook2022ExerciseForRadical`\".

If NO-ACTION is non-nil, omit, the ACTION element. If NO-STATE is non-nil, omit
the STATE element. If ISSUE is nil, use the issue at point or in the current
buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (action (if (and (tlon-babel-issue-is-job-p issue)
			  (not no-action))
		     (or (tlon-babel-label-lookup :action :label (tlon-babel-forge-get-label issue))
			 "")
		   ""))
	 (state (if (tlon-babel-issue-is-job-p issue)
		    "TODO"
		  (tlon-babel-get-issue-status issue)))
	 (repo-abbrev (tlon-babel-repo-lookup :abbrev :dir (tlon-babel-get-repo 'error 'include-all)))
	 (todo-name (replace-regexp-in-string
		     "[[:space:]]\\{2,\\}"
		     " "
		     (concat
		      (unless no-state (format "%s " state))
		      (format "[%s] %s %s" repo-abbrev action (tlon-babel-get-issue-link issue))))))
    todo-name))

(defun tlon-babel-get-file-from-issue (&optional issue)
  "Get the file path of ISSUE.
If ISSUE is nil, use the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (name (tlon-babel-get-issue-name issue)))
    (if (string-match tlon-babel-key-regexp name)
	(tlon-babel-get-file-from-key (match-string 1 name))
      (user-error "I wasn't able to find a file at point or in the forge buffer"))))

(defun tlon-babel-open-forge-file ()
  "Open the file of the topic at point or in the current buffer."
  (interactive)
  (find-file (tlon-babel-get-file-from-issue)))

(defun tlon-babel-open-forge-counterpart ()
  "Open the file counterpart of the topic at point or in the current buffer."
  (interactive)
  (tlon-babel-open-counterpart nil (tlon-babel-get-file-from-issue)))

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
	 (identifier (tlon-babel-metadata-lookup "path_original" "key_original" key metadata))
	 (original-path (file-name-concat repo "originals" identifier))
	 (translation-path (tlon-babel-metadata-lookup "file" "key_original" key metadata)))
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
`tlon-babel-repos', prompt the user to select a repo from that list."
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
If the current directory matches any of the directories in `tlon-babel-repos',
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

(defun tlon-babel-create-job ()
  "Create a new job for IDENTIFIER based on Ebib entry at point.
Creating a new job means (1) importing a document and (2) creating a record for
it. A record is (a) an issue in GitHub and (b) a heading in `jobs.org'.

IDENTIFIER can be a URL or a PDF file path."
  (interactive)
  (unless (derived-mode-p 'ebib-entry-mode 'ebib-index-mode)
    (user-error "This command must be run from an Ebib buffer"))
  (if-let ((id (or (ebib-extras-get-field-value "url")
		   (ebib-extras-get-file "md")))
	   (title (ebib-extras-get-field-value "title"))
	   (key (ebib-extras-get-field-value "=key="))
	   (repo (completing-read "Repo: " (tlon-babel-get-property-of-repos :dir :type 'translations))))
      (progn
	(tlon-babel-import-document id title)
	(tlon-babel-create-translation-file repo)
	(tlon-babel-create-record-for-job key))
    (user-error "The current Ebib entry seems to be missing one of the following
fields, which are needed to create a new job: `url' or `file',
`title' and `key'")))

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

;;;;; Importing

(defun tlon-babel-import-document (&optional identifier title)
  "Import a document with IDENTIFIER.
IDENTIFIER can be a URL or a PDF file path.

This command also imports EA Forum posts and tags. TITLE optionally specifies
the title of the document to be imported."
  (interactive)
  (let ((identifier (or identifier (read-string "Identifier (URL or PDF path): "))))
    (if (simple-extras-string-is-url-p identifier)
	(tlon-babel-import-html identifier title)
      (tlon-babel-import-pdf (expand-file-name identifier)))))

(defun tlon-babel-import-html (url &optional title)
  "Import the HTML in URL and convert it to Markdown.
TITLE optionally specifies the title of the file to be imported."
  (if-let ((id-or-slug (tlon-babel-eaf-get-id-or-slug-from-identifier url)))
      (tlon-babel-import-html-eaf id-or-slug title)
    (tlon-babel-html-to-markdown url title)))

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

(defun tlon-babel-name-file-from-title (&optional title)
  "Save the current buffer to a file named after TITLE.
Set the name to the slugified version of TITLE with the extension `.md'. If
TITLE is nil, get it from the file metadata. If the file doesn't have metadata,
prompt the user for a title.

When buffer is already visiting a file, prompt the user for confirmation before
renaming it."
  (interactive)
  (let* ((title (or title
		    (tlon-babel-metadata-get-field-value-in-file "titulo")
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

;; TODO: make it also work with LessWrong
(defun tlon-babel-import-html-eaf (id-or-slug &optional title)
  "Import the HTML of EAF entity with ID-OR-SLUG to TARGET and convert it to MD.
TITLE optionally specifies the title of the entity to be imported."
  (let* ((response (tlon-babel-eaf-request id-or-slug))
	 (object (tlon-babel-eaf-get-object id-or-slug))
	 (title (or title (pcase object
			    ('post (tlon-babel-eaf-get-post-title response))
			    ('tag (tlon-babel-eaf-get-tag-title response)))))
	 (dir (tlon-babel-get-property-of-repo-name :dir "uqbar-en"))
	 (target (read-string "Save file in: " (tlon-babel-set-file-from-title title dir)))
	 (html (pcase object
		 ('post (tlon-babel-eaf-get-post-html response))
		 ('tag (tlon-babel-eaf-get-tag-html response))))
	 (html-file (tlon-babel-save-html-to-file html)))
    (shell-command
     (format tlon-babel-pandoc-convert-from-file html-file target))
    (with-current-buffer (find-file-noselect target)
      (tlon-babel-markdown-cleanup-common)
      (tlon-babel-markdown-cleanup-eaf)
      (tlon-babel-autofix-all))
    (find-file target)))

(defun tlon-babel-save-html-to-file (html)
  "Save the HTML string HTML to a temporary file."
  (let ((filename (make-temp-file "tlon-babel-request-" nil ".html")))
    (with-temp-file filename
      (insert html))
    filename))

(defun tlon-babel-html-to-markdown (source &optional title)
  "Convert HTML text in SOURCE to Markdown.
SOURCE can be a URL or a file path. If TITLE is not provided, prompt the user
for one."
  (let* ((target (read-string "Save file in: " (tlon-babel-set-file-from-title title)))
	 (pandoc (if (simple-extras-string-is-url-p source)
		     tlon-babel-pandoc-convert-from-url
		   tlon-babel-pandoc-convert-from-file)))
    (shell-command
     (format pandoc source target))
    (with-current-buffer (find-file-noselect target)
      (tlon-babel-markdown-cleanup-common)
      (tlon-babel-autofix-all))
    (find-file target)))

(defun tlon-babel-import-pdf (path &optional title)
  "Import the PDF in PATH to TARGET and convert it to Markdown.
This command requires the user to supply values for the header and footer
elements to be excluded from the conversion, which are different for each PDF.
To determine these values, measure the distance between the top/bottom of the
PDF (which will open in the other window) and note the number of pixels until
the end of the header/footer. (You can measure the number of pixels between two
points by taking a screenshot: note the numbers next to the pointer.) Then enter
these values when prompted.

If TITLE is nil, prompt the user for one."
  (find-file-other-window path)
  (let ((target (read-string "Save file in: " (tlon-babel-set-file-from-title title)))
	(header (read-string "Header: "))
	(footer (read-string "Footer: ")))
    (unless (executable-find "pdftotext")
      (user-error "`pdftotext' not found. Please install it (`brew install poppler') and set `tlon-babel-pdftotext' to its path"))
    (shell-command (format "'%s' -margint %s -marginb %s '%s' '%s'"
			   tlon-babel-pdftotext header footer path target))
    (find-file target)))

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
	(tlon-babel-create-issue-from-key key)
	(tlon-babel-create-heading-for-job key 'commit))
    (user-error "I wasn't able to create a record because I didn't find a key")))

(defun tlon-babel-create-issue (title &optional repo body)
  "Create new GitHub issue in REPO with TITLE and BODY."
  (let* ((repo (or repo (tlon-babel-get-repo 'error 'include-all)))
	 (body (or body ""))
	 (default-directory repo)
	 (repo (forge-get-repository t))
	 (owner (oref repo owner))
	 (reponame (oref repo name))
	 (resource (format "/repos/%s/%s/issues" owner reponame))
	 (data `(("title" . ,title)
		 ("body" . ,body))))
    (ghub-post resource data
	       :auth 'forge
	       :noerror t ;; avoid showing the original large output
	       :reader 'ignore) ;; do not parse the response json
    (message "Created issue with title %s" title)))

(defun tlon-babel-create-issue-from-todo ()
  "Create a new GitHub issue based on the current `org-mode' heading."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "You need to be in `org-mode' to use this function"))
  (when (tlon-babel-get-issue-number-from-heading)
    (user-error "This heading already has an issue"))
  (unless (tlon-babel-is-valid-status-p)
    (user-error "Invalid TODO status"))
  (unless (tlon-babel-get-repo-from-heading)
    (tlon-babel-set-repo-in-heading))
  (let (todo-linkified)
    (save-excursion
      (let* ((default-directory (tlon-babel-get-repo-from-heading))
	     (heading (substring-no-properties (org-get-heading t t t t)))
	     (status (downcase (org-get-todo-state)))
	     (abbrev-repo (tlon-babel-repo-lookup :abbrev :dir default-directory))
	     (issue-title (substring heading (+ (length abbrev-repo) 3)))
	     (latest-issue-pre (car (tlon-babel-get-latest-issue)))
	     (latest-issue-post latest-issue-pre))
	(tlon-babel-create-issue issue-title default-directory)
	(forge-pull)
	(message (concat "Reflect on this fine proverb while you wait: " (tlon-core-proverb)))
	(while (eq latest-issue-pre latest-issue-post)
	  (sleep-for 0.1)
	  (setq latest-issue-post (car (tlon-babel-get-latest-issue))))
	(tlon-babel-set-issue-number-in-heading latest-issue-post)
	(tlon-babel-visit-issue)
	(tlon-babel-set-assignee (tlon-babel-user-lookup :github :name user-full-name))
	(tlon-babel-set-label status)
	(setq todo-linkified (tlon-babel-make-todo-name-from-issue nil 'no-state))))
    (org-edit-headline todo-linkified)))

(defun tlon-babel-create-issue-or-todo ()
  "Create issue from TODO or vice versa."
  (interactive)
  (tlon-babel-todo-issue-funcall #'tlon-babel-create-issue-from-todo
				 #'tlon-babel-capture-issue))

(defun tlon-babel-create-issue-from-key (&optional key)
  "Create an issue based on KEY.
If KEY is not provided, the key in the Markdown buffer at point is used."
  (let ((default-directory (tlon-babel-get-repo 'error))
	(key (or key (tlon-babel-get-key-in-buffer))))
    (tlon-babel-create-issue (format "Job: `%s`" key) default-directory)))

(defun tlon-babel-create-heading-for-job (&optional key commit)
  "Create a heading based on BibTeX KEY in `jobs.org'.
If KEY is not provided, the key in the Markdown buffer at point is used. If
COMMIT is non-nil, commit the change."
  (interactive)
  (let* ((key (or key (tlon-babel-get-key-in-buffer)))
	 (heading (format "[cite:@%s]" key))
	 (file (tlon-babel-metadata-lookup "file" "key_original" key (tlon-babel-get-metadata-in-repo)))
	 (repo (tlon-babel-get-repo-from-file file))
	 (repo-abbrev (tlon-babel-repo-lookup :abbrev :dir repo)))
    (with-current-buffer (or (find-buffer-visiting tlon-babel-file-jobs)
			     (find-file-noselect tlon-babel-file-jobs))
      (widen)
      (goto-char (point-min))
      (unless (search-forward heading nil t)
	(re-search-forward tlon-babel-jobs-id nil t)
	(while (and (not (org-at-heading-p)) (not (eobp)))
	  (forward-line))
	(org-insert-heading)
	(insert heading)
	(org-todo 'todo)
	(org-set-tags repo-abbrev)
	(tlon-babel-sort-headings tlon-babel-file-jobs)
	(save-buffer)))
    (when commit
      (tlon-babel-commit-and-push "Update" tlon-babel-file-jobs))))

(defun tlon-babel-sort-headings (&optional file)
  "Sort all headings under parent in FILE alphabetically and by TODO order."
  (interactive)
  (with-current-buffer (or (find-buffer-visiting file)
			   (find-file-noselect file))
    (widen)
    (org-up-heading-safe)
    (org-sort-entries nil ?a)
    (org-sort-entries nil ?o)
    (save-buffer)))

(defun tlon-babel-get-parent-todo (todo)
  "Get parent of TODO in `tlon-babel-todos-jobs-file'."
  (let ((pos (tlon-babel-get-todo-position todo (tlon-babel-get-todos-jobs-file))))
    (save-window-excursion
      (tlon-babel-visit-todo pos (tlon-babel-get-todos-jobs-file))
      (widen)
      (org-up-heading-safe)
      (org-no-properties (org-get-heading)))))

(defun tlon-babel-mark-todo-done (todo file)
  "Mark TODO in FILE as DONE."
  (let ((pos (tlon-babel-get-todo-position todo file)))
    (save-window-excursion
      (tlon-babel-visit-todo pos)
      (org-todo "DONE")
      (save-buffer)
      (message "Marked `%s' as DONE" todo))))

(defun tlon-babel-get-key-in-heading ()
  "Get the key of the currently clocked task."
  (unless (org-at-heading-p)
    (user-error "Not in an org-mode heading"))
  (let ((heading (substring-no-properties (org-get-heading t t t t))))
    (if (string-match "\\[cite:@\\(.+?\\)\\]\\|Job: `\\(.+?\\)\\.md`" heading)
	(or (match-string 1 heading)
	    (match-string 2 heading))
      (user-error "I wasn't able to find a key in clocked heading"))))

(defun tlon-babel-goto-heading (key)
  "Move point to the heading in `jobs.org' with KEY."
  (with-current-buffer (or (find-buffer-visiting tlon-babel-file-jobs)
			   (find-file-noselect tlon-babel-file-jobs))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
	(when (string= (org-element-property :raw-value headline) (format "[cite:@%s]" key))
	  (goto-char (org-element-property :begin headline)))))))

;;;;; Initialize & finalize functions

(defun tlon-babel-dwim ()
  "Initialize or finalize process based on clocked task."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-clock-in))
  (save-buffer)
  (let* ((action (tlon-babel-get-action-in-label (tlon-babel-get-clock-label)))
	 (stage (pcase major-mode
		  ('org-mode 'initialize)
		  ('markdown-mode 'finalize)
		  (_ (user-error "I don't know what to do in `%s`" major-mode))))
	 (fun (intern (format "tlon-babel-%s" stage)))
	 (arg (intern (format "tlon-babel-%s-%s" stage action))))
    (if (eq stage 'initialize)
	(funcall fun arg)
      (funcall fun))))

(defun tlon-babel-initialize (fun)
  "Initialize process associated with FUN.
Runs all the general initialization functions, followed by the specific function
for the process that is being initialized."
  (let* ((key (tlon-babel-get-clock-key))
	 (repo (tlon-babel-get-repo-from-key key))
	 (default-directory repo))
    (tlon-babel-check-label-and-assignee repo)
    (tlon-babel-check-branch "main" repo)
    (call-interactively #'magit-pull-from-upstream nil)
    (sleep-for 2)
    (cl-multiple-value-bind
	(original-path translation-path)
	(tlon-babel-set-paths-from-clock)
      (let ((topic (tlon-babel-get-clock-issue)))
	(tlon-babel-set-windows original-path translation-path)
	(write-file translation-path)
	(winum-select-window-2)
	(orgit-topic-open topic)
	(tlon-babel-copy-buffer original-path)
	(funcall fun)))))

(defun tlon-babel-finalize ()
  "Finalize current stage of translation process."
  (save-buffer)
  (cl-multiple-value-bind
      (original-path translation-path original-key)
      (tlon-babel-set-paths-from-clock)
    (let* ((repo (tlon-babel-get-repo))
	   (current-action (tlon-babel-get-clock-action))
	   (next-label (tlon-babel-get-clock-next-label))
	   (next-assignee (tlon-babel-get-next-assignee)))
      (tlon-babel-check-branch "main" repo)
      (tlon-babel-check-label-and-assignee repo)
      (tlon-babel-check-file
       (when (string= current-action "Process")
	 'original))
      (when (string= current-action "Translate")
	(unless (y-or-n-p "Have you processed all Jinx and Flycheck warnings, and ran `tlon-babel-manual-fix-all'?")
	  (user-error "Aborted")))
      (when (string= current-action "Check")
	(tlon-babel-read-mode -1))
      (save-buffer)
      (if (string= current-action "Process")
	  (write-file original-path)
	(write-file translation-path))
      (when (string= current-action "Process")
	(tlon-babel-commit-and-push current-action original-path))
      (tlon-babel-commit-and-push current-action translation-path)
      (tlon-babel-act-on-topic original-key next-label next-assignee
			       (when (string= current-action "Review")
				 'close))
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'"
	       next-label next-assignee))
    (tlon-babel-finalize-set-todos)))

(defun tlon-babel-finalize-set-todos ()
  "Set relevant TODO statuses during the finalize process."
  (let ((todo (tlon-babel-get-clock))
	(key (tlon-babel-get-clock-key))
	(current-action (tlon-babel-get-clock-action)))
    (tlon-babel-mark-todo-done todo (tlon-babel-get-todos-jobs-file))
    (when (or (string= current-action "Review") (string= current-action "Check"))
      (let ((parent-todo (tlon-babel-get-parent-todo todo)))
	(tlon-babel-mark-todo-done parent-todo (tlon-babel-get-todos-jobs-file))))
    (when (string= current-action "Review")
      (let ((job-todo (format "[cite:@%s]" key)))
	(tlon-babel-mark-todo-done job-todo tlon-babel-file-jobs)
	(tlon-babel-sort-headings tlon-babel-file-jobs)
	(tlon-babel-commit-and-push "Update"
				    tlon-babel-file-jobs)))))

(defun tlon-babel-initialize-processing ()
  "Initialize processing."
  (cl-multiple-value-bind
      (original-path)
      (tlon-babel-set-paths-from-clock)
    (tlon-babel-set-windows original-path tlon-babel-file-babel-manual)
    (org-id-goto tlon-babel-manual-processing-id)
    (org-narrow-to-subtree)
    (org-extras-show-subtree-hide-drawers)
    (winum-select-window-2)
    (let ((topic (tlon-babel-get-clock-issue)))
      (orgit-topic-open topic))))

(defun tlon-babel-initialize-translation ()
  "Initialize translation."
  (tlon-core-macos-open-app "deepl"))

(defun tlon-babel-initialize-revision ()
  "Initialize stylistic revision.")

(defun tlon-babel-initialize-check ()
  "Initialize accuracy check."
  ;; we move the buffer displaying the issue to the right, to uncover
  ;; the original file
  (window-extras-buffer-move-dwim)
  (window-extras-switch-to-last-window)
  (markdown-preview)
  (tlon-babel-read-mode)
  (read-aloud-buf))

(defun tlon-babel-initialize-review ()
  "Initialize review."
  (cl-multiple-value-bind
      (original-path translation-path)
      (tlon-babel-set-paths-from-clock)
    (tlon-babel-log-buffer-latest-user-commit-ediff translation-path)
    (winum-select-window-1)
    (setq-local jinx-languages "es")
    (add-file-local-variable 'jinx-languages jinx-languages)
    (setf (alist-get 'jinx-languages file-local-variables-alist) jinx-languages)
    (jinx--load-dicts)
    (jinx--cleanup)
    (goto-char (point-min))
    (save-buffer)
    (files-extras-switch-to-alternate-buffer)))

;;;;; TTS

(define-minor-mode tlon-babel-read-mode
  "Enable TTS mode locally."
  :global nil
  :init-value nil)

(defvar tlon-babel-read-mode-map (make-sparse-keymap)
  "Keymap for `tlon-babel-read-mode'.")

(define-key tlon-babel-read-mode-map (kbd "H-,") 'tlon-babel-read-backward)
(define-key tlon-babel-read-mode-map (kbd "H-.") 'tlon-babel-read-forward)
(define-key tlon-babel-read-mode-map (kbd "H-;") 'tlon-babel-read-target-start-or-stop)

(defun tlon-babel-read-target-buffer ()
  "Return the buffer that `read-aloud' should read."
  (let ((buffer-list (cl-remove-if-not
		      (lambda (buffer)
			(string-match-p (regexp-quote "**markdown-output* # eww*") (buffer-name buffer)))
		      (buffer-list)))
	buffer)
    (cond ((= (length buffer-list) 1)
	   (setq buffer (car buffer-list)))
	  ((< (length buffer-list) 1)
	   (user-error "No buffer found"))
	  ((> (length buffer-list) 1)
	   (user-error "More than one buffer found")))
    buffer))

(defun tlon-babel-read-target-start-or-stop ()
  "Start or stop reading the target buffer."
  (interactive)
  (let ((buffer (tlon-babel-read-target-buffer))
	(current-buffer (current-buffer)))
    (pop-to-buffer buffer)
    (when read-aloud--c-bufpos
      (goto-char read-aloud--c-bufpos))
    (read-aloud-buf)
    ;; we move point to the previous chunk, using the chunk divider
    ;; defined in `read-aloud--grab-text'
    (re-search-backward "[,.:!;]\\|\\(-\\|\n\\|\r\n\\)\\{2,\\}" nil t)
    (pop-to-buffer current-buffer)))

(defun tlon-babel--read-backward-or-forward (direction)
  "Move in DIRECTION in the target buffer."
  (interactive)
  (let ((buffer (tlon-babel-read-target-buffer))
	(current-buffer (current-buffer))
	(fun (if (eq direction 'backward)
		 're-search-backward
	       're-search-forward)))
    (when read-aloud--c-bufpos
      (read-aloud-buf))
    (pop-to-buffer buffer)
    (funcall fun "[,.:!;]\\|\\(-\\|\n\\|\r\n\\)\\{2,\\}" nil t 1)
    (pop-to-buffer current-buffer)))

(defun tlon-babel-read-backward ()
  "Move backward in the target buffer."
  (interactive)
  (tlon-babel--read-backward-or-forward 'backward))

(defun tlon-babel-read-forward ()
  "Move forward in the target buffer."
  (interactive)
  (tlon-babel--read-backward-or-forward 'forward))

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
	 (field (if original "path_original" "file"))
	 (expected-file (file-name-nondirectory
			 (tlon-babel-metadata-lookup field "key_original" key (tlon-babel-get-metadata-in-repo))))
	 (actual-file (file-name-nondirectory
		       (buffer-file-name))))
    (if (string= expected-file actual-file)
	t
      (user-error "Current file does not match file in clock"))))

(defun tlon-babel-check-label-and-assignee (repo)
  "Check that clocked action, user match label, assignee of topic in REPO."
  (save-window-excursion
    (let* ((default-directory repo)
	   (key (tlon-babel-get-clock-key))
	   (topic (format "Job: `%s" key))
	   (clocked-label (tlon-babel-get-clock-label)))
      (magit-status-setup-buffer repo)
      (magit-section-show-level-3-all)
      (goto-char (point-min))
      (if (search-forward topic nil t)
	  (let ((label (tlon-babel-forge-get-label))
		(assignee (tlon-babel-user-lookup :name :github (tlon-babel-forge-get-assignee))))
	    (unless (string= clocked-label label)
	      (user-error "The `org-mode' TODO says the label is `%s', but the actual topic label is `%s'"
			  clocked-label label))
	    (unless (string= user-full-name assignee)
	      (user-error "The `org-mode' TODO says the assignee is `%s', but the actual topic assignee is `%s'"
			  user-full-name assignee))
	    t)
	(user-error "No topic found for %s" key)))))

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
	 (title (tlon-babel-metadata-get-field-value-in-file "titulo" file))
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
(defun tlon-babel-add-lang-id-to-entry (&optional key beg end)
  "Add `langid' field to entry at point, if appropriate.
If the field `landig' is present, the function does nothing; else, it sets the
`langid' field to `spanish' if the entry has either a `translation' or a
`translator' field, and to `english' otherwise.

KEY, BEG and END are ignored; they are only there to satisfy the signature of
`bibtex-map-entries'."
  (unless (bibtex-text-in-field "langid")
    (if (or (bibtex-text-in-field "translation")
	    (bibtex-text-in-field "translator"))
	(bibtex-set-field "langid" "spanish")
      (bibtex-set-field "langid" "english"))))

(defun tlon-babel-act-on-topic (original-key label assignee &optional close)
  "Apply LABEL and ASSIGNEE to topic associated with ORIGINAL-KEY.
If CLOSE is non-nil, close the issue."
  (let* ((issue-title (format "Job: `%s" original-key))
	 (issue (tlon-babel-forge-issue-lookup issue-title))
	 (default-directory (tlon-babel-get-repo 'error 'include-all)))
    (tlon-babel-set-label label issue)
    (tlon-babel-set-assignee assignee issue)
    (when close
      (tlon-babel-forge-close-issue issue))))

(defun tlon-babel-forge-issue-lookup (string &optional repo)
  "Return the first issue in REPO whose title includes STRING.
If REPO is nil, use the current repository."
  (let* ((string (concat "%" string "%"))
	 (repo (or repo (forge-get-repository nil)))
	 (issue-id (caar (emacsql (forge-db)
				  [:select [number]
					   :from 'issue
					   :where (and (= repository $s1)
						       (like title $s2))]
				  (oref repo id)
				  string))))
    (when issue-id
      (forge-get-issue repo issue-id))))

(defun tlon-babel-forge-close-issue (&optional issue)
  "Close the topic ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (repo (forge-get-repository issue)))
    (when (eq 'open (oref issue state))
      (forge--set-topic-state repo issue 'closed))))

;;;;; Search

(defun tlon-babel-search-topics (search-string &optional repo)
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
    (tlon-babel-search-topics search-string repo)
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

;;;;; Change topic properties

(defun tlon-babel-set-property (property type &optional issue)
  "Set PROPERTY of TYPE in ISSUE.
If ISSUE is nil, use issue at point or in the current buffer."
  (interactive
   (list ))
  (let* ((fun (pcase type
		('label #'forge--set-topic-labels)
		('assignee #'forge--set-topic-assignees)
		(_ (user-error "Property type `%s' not recognized" type))))
	 (issue (or issue (forge-get-topic (forge-current-topic))))
	 (repo (forge-get-repository issue))
	 (crm-separator ","))
    (funcall fun repo issue (list property))))

;; TODO: Cleanup the three functions below
(defun tlon-babel-set-label (&optional label issue)
  "Apply LABEL to ISSUE.
If ISSUE is nil, use issue at point or in the current buffer."
  (interactive)
  (let* ((issue (or issue (forge-get-topic (forge-current-topic))))
	 (label (or label (if (tlon-babel-issue-is-job-p issue)
			      (tlon-babel-set-job-label)
			    (tlon-babel-set-status-label)))))
    (tlon-babel-set-property label 'label issue)))

(defun tlon-babel-set-job-label ()
  "Prompt the user to select a job label."
  (let ((label (completing-read "What should be the label? "
				(tlon-babel-get-property-of-labels :label))))
    label))

(defun tlon-babel-set-status-label ()
  "Prompt the user to select a status label."
  (let ((label (completing-read "TODO status? " (mapcar #'downcase tlon-babel-todo-statuses) nil t)))
    label))

(defun tlon-babel-set-assignee (assignee &optional issue)
  "Make ASSIGNEE the assignee of ISSUE.
If ISSUE is nil, use issue at point or in the current buffer."
  (interactive
   (list (tlon-babel-select-assignee)))
  (let ((issue (or issue (forge-get-topic (forge-current-topic)))))
    (tlon-babel-set-property assignee 'assignee issue)))

(defun tlon-babel-select-assignee ()
  "Prompt the user to select an ASSIGNEE.
The prompt defaults to the current user."
  (let ((assignee (completing-read "Who should be the assignee? "
				   (tlon-babel-get-property-of-users :github) nil nil
				   (tlon-babel-user-lookup :github :name user-full-name))))
    assignee))

(defun tlon-babel-get-next-assignee ()
  "Get the next assignee based on the current user and clock label.
This function returns the assignee designated for the next label if the current
user is the user designated for the current label; otherwise, it returns the
substitute assignee."
  (let*
      ((current-user (tlon-babel-user-lookup :github :name user-full-name))
       (current-assignee (tlon-babel-label-lookup :assignee :label (tlon-babel-get-clock-label)))
       (designated-next-assignee (tlon-babel-label-lookup :assignee :label (tlon-babel-get-clock-next-label)))
       (substitute-next-assigne (tlon-babel-user-lookup :substitute :github designated-next-assignee)))
    (if (string= current-user current-assignee)
	designated-next-assignee
      substitute-next-assigne)))

(defun tlon-babel-set-initial-label-and-assignee ()
  "Set label to `Awaiting processing' and assignee to current user."
  (tlon-babel-set-label "Awaiting processing")
  (tlon-babel-set-assignee (tlon-babel-user-lookup :github :name user-full-name)))

(defun tlon-babel-get-element (element &optional issue)
  "Return ELEMENT of ISSUE.
If the topic has more than one element, return the first. If ISSUE is nil, use
the issue at point or in the current buffer."
  (when-let ((issue (or issue (forge-current-topic))))
    (caar (closql--iref issue element))))

(defun tlon-babel-forge-get-assignee (&optional issue)
  "Return the assignee of the current ISSUE.
If the topic has more than one assignee, return the first. If ISSUE is nil, use
the issue at point or in the current buffer."
  (tlon-babel-get-element 'assignees issue))

(defun tlon-babel-forge-get-label (&optional issue)
  "Return the label of the topic at point.
If the topic has more than one label, return the first. If ISSUE is nil, use the
issue at point or in the current buffer."
  (tlon-babel-get-element 'labels issue))

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

(defun tlon-babel-forge-get-state (&optional issue)
  "Return state of ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (when-let ((issue (or issue (forge-current-topic))))
    (oref (forge-current-topic) state)))

;;;;; Glossary

(defun tlon-babel-glossary-alist ()
  "Read `Glossary.csv` and return it as an alist."
  (with-temp-buffer
    (insert-file-contents (tlon-babel-get-file-glossary))
    (let ((lines (split-string (buffer-string) "\n" t))
	  (result '()))
      (dolist (line lines result)
	(let* ((elements (split-string line "\",\""))
	       (key (substring (nth 0 elements) 1)) ; removing leading quote
	       (value (if (string-suffix-p "\"" (nth 1 elements))
			  (substring (nth 1 elements) 0 -1)   ; if trailing quote exists, remove it
			(nth 1 elements)))) ; otherwise, use as-is
	  (push (cons key value) result))))))

(defun tlon-babel-glossary-dwim ()
  "Add a new entry to the glossary or modify an existing entry."
  (interactive)
  (let* ((terms (mapcar 'car (tlon-babel-glossary-alist)))
	 (term (completing-read "Term: " terms)))
    (if (member term terms)
	(tlon-babel-glossary-modify term)
      (tlon-babel-glossary-add term))))

(defun tlon-babel-glossary-add (&optional original translation)
  "Add a new entry to the glossary for ORIGINAL and TRANSLATION terms."
  (interactive)
  (let ((original (or original (read-string "original term: "))))
    (cl-destructuring-bind (translation explanation) (tlon-babel-glossary-prompt translation)
      (with-current-buffer (find-file-noselect (tlon-babel-get-file-glossary translation))
	(goto-char (point-max))
	(insert (tlon-babel-glossary-regexp-pattern original translation))
	(tlon-babel-glossary-finalize "add" original explanation)))))

(defun tlon-babel-glossary-modify (original)
  "Modify an entry in the glossary corresponding to the ORIGINAL term."
  (let* ((existing-translation (cdr (assoc original (tlon-babel-glossary-alist)))))
    (cl-destructuring-bind (new-translation explanation) (tlon-babel-glossary-prompt)
      (with-current-buffer (find-file-noselect (tlon-babel-get-file-glossary))
	(goto-char (point-min))
	(while (re-search-forward (tlon-babel-glossary-regexp-pattern original existing-translation) nil t)
	  (replace-match (tlon-babel-glossary-regexp-pattern original new-translation)))
	(tlon-babel-glossary-finalize "modify" original explanation)
	(message "Remember to run a `ripgrep' search for the original translation (\"%s\") across all the Babel repos in the translation language (%s), making any necessary replacements."
		 existing-translation tlon-babel-translation-language)))))

(defun tlon-babel-glossary-prompt (&optional translation)
  "Prompt the user for a translation and and an explanation.
If TRANSLATION is non-nil, prompt for an explanation only."
  (let ((translation (or translation (read-string
				      (format "translation term [%s]: "
					      tlon-babel-translation-language))))
	(explanation (read-string (format
				   "Explanation (optional; please write it in the translation language [%s]): "
				   tlon-babel-translation-language))))
    (list translation explanation)))

(defun tlon-babel-glossary-regexp-pattern (original translation)
  "Get the regexp pattern for the glossary entry corresponding to ORIGINAL and TRANSLATION."
  (format "\"%s\",\"%s\",\"EN\",\"%s\"" original translation
	  (upcase tlon-babel-translation-language)))

(defun tlon-babel-glossary-finalize (action original explanation)
  "Finalize the addition of a word to the glossary or its modification.
ACTION is either \"add\" or \"modify\". ORIGINAL is the term in the original
language. EXPLANATION is the explanation of the translation."
  (goto-char (point-min))
  (flush-lines "^$")
  (save-buffer)
  (tlon-babel-glossary-commit action original explanation))

;; TODO: fix this
(defun tlon-babel-glossary-commit (action term &optional explanation)
  "Commit glossary modifications.
ACTION describes the action (\"add\" or \"modify\") performed on the glossary.
TERM refers to the English glossary term to which this action was performed.
These two variables are used to construct a commit message of the form
\='Glossary: ACTION \"TERM\"\=', such as \='Glossary: add \"repugnant
conclusion\"\='. Optionally, EXPLANATION provides an explanation of the change."
  (let ((default-directory (tlon-babel-get-property-of-repo-name :dir "babel-es"))
	(explanation (if explanation (concat "\n\n" explanation) "")))
    ;; save all unsaved files in repo
    (magit-save-repository-buffers)
    (call-interactively #'magit-pull-from-upstream nil)
    ;; if there are staged files, we do not commit or push the changes
    (unless (magit-staged-files)
      (tlon-babel-check-branch "main" default-directory)
      (magit-run-git "add" (tlon-babel-get-file-glossary))
      (let ((magit-commit-ask-to-stage nil))
	(magit-commit-create (list "-m" (format  "Glossary: %s \"%s\"%s"
						 action term explanation))))))
  (call-interactively #'magit-push-current-to-pushremote))

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
	 (keys (tlon-babel-get-keys data))
	 (selected-key (let* ((keys (citar-select-refs)))
			 (car keys))))
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
    ("j" "job"                            tlon-babel-create-job)
    ("r" "dwim"                           tlon-babel-dwim)
    ("m" "Magit"                          tlon-babel-magit-repo-dispatch)
    ("n" "forge"                          tlon-babel-forge)
    ("." "gh-notify"                      gh-notify)
    """Request"
    ("q g" "update babel"                 tlon-babel-update-babel-in-uqbar-es)
    ("q i" "update images"                tlon-babel-update-uqbar-images)
    ("q l" "show log"                     tlon-babel-show-uqbar-log)
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
    ("s i" "issues"                       tlon-babel-search-topics)
    ("s t" "translation"                  tlon-babel-search-for-translation)
    ]
   ["Browse in Dired"
    ("d" "dir"               tlon-babel-dired-dir-dispatch)
    ("H-d" "repo"                   tlon-babel-dired-repo-dispatch)
    ""
    """Browse externally"
    ("b" "current file"                         tlon-babel-browse-file)
    ("H-b" "current repo"                         tlon-babel-browse-repo)
    """Counterpart"
    ("f" "current win" tlon-babel-open-counterpart-dwim)
    ("H-f" "other win" tlon-babel-open-counterpart-other-window-dwim)
    ]
   [
    "File changes"
    ("h h" "log"                          magit-log-buffer-file)
    ("h d" "diffs since last user change" tlon-babel-log-buffer-latest-user-commit)
    ("h e" "ediff with last user change"  tlon-babel-log-buffer-latest-user-commit-ediff)
    """Translate"
    ("t t" "GPT"                          tlon-babel-gpt-translate)
    ("t w" "Web"                          tlon-babel-search-for-translation)]
   ["Clock"
    ("c c" "issue"                        tlon-babel-open-clock-issue)
    ("c f" "file"                         tlon-babel-open-clock-file )
    ("c o" "heading"                      org-clock-goto)
    """Issue"
    ("i i" "open counterpart"             tlon-babel-open-forge-counterpart)
    ("i I" "open file"                    tlon-babel-open-forge-file)
    ]
   ["Sync"
    ("y y" "visit or capture"             tlon-babel-visit-counterpart-or-capture)
    ("y v" "visit"                        tlon-babel-visit-counterpart)
    ("y p" "post"                         tlon-babel-create-issue-from-todo)
    ("y c" "capture"                      tlon-babel-capture-issue)
    ("y C" "capture all"                  tlon-babel-capture-all-issues)
    ("y r" "reconcile"                    tlon-babel-reconcile-issue-and-todo)
    ("y R" "reconcile all"                tlon-babel-reconcile-all-issues-and-todos)
    ("y x" "close"                        tlon-babel-close-issue-and-todo)]
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

(defun tlon-babel-open-repo ()
  "Open the Babel repository."
  (interactive)
  (dired (tlon-babel-get-repo nil 'include-all)))

(defun tlon-babel-open-uqbar-es-repo ()
  "Open the `uqbar-es' repository."
  (interactive)
  (dired (tlon-babel-get-property-of-repo-name :dir "uqbar-es")))

(defun tlon-babel-open-utilitarismo-repo ()
  "Open the Utilitarismo repository."
  (interactive)
  (dired (tlon-babel-get-property-of-repo-name :dir "util")))

(defun tlon-babel-open-largoplacismo-repo ()
  "Open the Largoplacismo repository."
  (interactive)
  (dired (tlon-babel-get-property-of-repo-name :dir "ensayos")))

(defun tlon-babel-open-babel-repo ()
  "Open the Babel repository."
  (interactive)
  (dired (tlon-babel-get-property-of-repo-name :dir "babel")))

;; TODO: this should be revised to support folders in different `uqbar' repos
(defun tlon-babel-open-uqbar-es-folder (folder)
  "Open FOLDER of the `uqbar-es' repository."
  (dired (file-name-concat
	  (tlon-babel-get-property-of-repo-name :dir "uqbar-es") folder)))

(defun tlon-babel-open-uqbar-es-articles ()
  "Open the `articulos' folder of the `uqbar-es' repository."
  (interactive)
  (tlon-babel-open-uqbar-es-folder "articulos"))

(defun tlon-babel-open-uqbar-es-authors ()
  "Open the `autores' folder of the `uqbar-es' repository."
  (interactive)
  (tlon-babel-open-uqbar-es-folder "autores"))

(defun tlon-babel-open-uqbar-es-tags ()
  "Open the `temas' folder of the `uqbar-es' repository."
  (interactive)
  (tlon-babel-open-uqbar-es-folder "temas"))

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

;;;;; Request

;;;;;; EAF API

(defun tlon-babel-eaf-post-query (id)
  "Return an EA Forum GraphQL query for post whose ID is ID."
  (concat "{\"query\":\"{\\n  post(\\n    input: {\\n      selector: {\\n        _id: \\\""
	  id
	  "\\\"\\n      }\\n    }\\n  ) {\\n    result {\\n      _id\\n      postedAt\\n      url\\n      canonicalSource\\n      title\\n      contents {\\n        markdown\\n        ckEditorMarkup\\n      }\\n      slug\\n      commentCount\\n      htmlBody\\n      baseScore\\n      voteCount\\n      pageUrl\\n      legacyId\\n      question\\n      tableOfContents\\n      author\\n      user {\\n        username\\n        displayName\\n        slug\\n        bio\\n      }\\n      coauthors {\\n        _id\\n        username\\n        displayName\\n        slug\\n      }\\n    }\\n  }\\n}\\n\"}"))

(defun tlon-babel-eaf-tag-query (slug)
  "Return an EA Forum GraphQL query for tag whose slug is SLUG."
  (concat "{\"query\":\"{\\n  tag(input: { selector: { slug: \\\""
	  slug
	  "\\\" } }) {\\n    result {\\n      name\\n      slug\\n      description {\\n        html\\n      }\\n      parentTag {\\n        name\\n      }\\n    }\\n  }\\n}\\n\"}"))

(defun tlon-babel-eaf-request (id-or-slug &optional async)
  "Run an EAF request for ID-OR-SLUG.
  If ASYNC is t, run the request asynchronously."
  (let* ((object (tlon-babel-eaf-get-object id-or-slug))
	 (fun (pcase object
		('post 'tlon-babel-eaf-post-query)
		('tag 'tlon-babel-eaf-tag-query)
		(_ (error "Invalid object: %S" object))))
	 (query (funcall fun id-or-slug))
	 response)
    (request
      tlon-babel-eaf-api-url
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data query
      :parser 'json-read
      :sync (not async)
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq response (cdr (assoc 'data data)))))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error: %S" error-thrown))))
    response))

(defun tlon-babel--eaf-get-post-result (response)
  "Get post details from EA Forum API RESPONSE."
  (let* ((post (cdr (assoc 'post response)))
	 (result (cdr (assoc 'result post))))
    result))

(defun tlon-babel-eaf-get-post-id (response)
  "Get post ID from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-post-result response))
	 (id (cdr (assoc '_id result))))
    id))

(defun tlon-babel-eaf-get-post-html (response)
  "Get post HTML from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-post-result response))
	 (html (cdr (assoc 'htmlBody result))))
    html))

(defun tlon-babel-eaf-get-post-title (response)
  "Get post title from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-post-result response))
	 (title (cdr (assoc 'title result))))
    title))

(defun tlon-babel-eaf-get-post-author (response)
  "Get post author from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-post-result response))
	 (author (cdr (assoc 'author result))))
    author))

(defun tlon-babel-eaf-get-post-username (response)
  "Get post author username from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-post-result response))
	 (user (cdr (assoc 'user result)))
	 (username (cdr (assoc 'username user))))
    username))

(defun tlon-babel--eaf-get-tag-result (response)
  "Get tag details from EA Forum API RESPONSE."
  (let* ((tag (cdr (assoc 'tag response)))
	 (result (cdr (assoc 'result tag))))
    result))

(defun tlon-babel-eaf-get-tag-slug (response)
  "Get tag slug from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-tag-result response))
	 (slug (cdr (assoc 'slug result))))
    slug))

(defun tlon-babel-eaf-get-tag-html (response)
  "Get tag HTML from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-tag-result response))
	 (description (cdr (assoc 'description result)))
	 (html (cdr (assoc 'html description))))
    html))

(defun tlon-babel-eaf-get-tag-title (response)
  "Get tag title from EA Forum API RESPONSE."
  (let* ((result (tlon-babel--eaf-get-tag-result response))
	 (title (cdr (assoc 'name result))))
    (tlon-babel-shorten-title title)))

(defun tlon-babel-shorten-title (title)
  "Return a shortened version of TITLE."
  (string-match "\\([[:alnum:] ,'‘’“”@#$%*\\^`~&\"]*\\)" title)
  (match-string 1 title))

;;;;;; `uqbar' API

(defun tlon-babel-show-uqbar-log ()
  "Show the `uqbar-es' error log."
  (interactive)
  ;; (tlon-babel-get-uqbar-log "POST" "https://altruismoeficaz.net/api/update/bae")
  (tlon-babel-print-log
   (tlon-babel-process-api-buffer
    (tlon-babel-get-uqbar-log "GET" "https://altruismoeficaz.net/api/update/bae/log"))))

;; TODO: make it work with POST request. Look at the script in
;; https://github.com/tlon-team/uqbar-api/blob/main/deployment/request.sh
(defun tlon-babel-get-uqbar-log (request url)
  "Get error log from `uqbar-es' repo as an association list.
If not already authenticated, run `tlon-babel-get-uqbar-token' first."
  (when-let* ((url-request-method request)
	      (url-mime-charset-string "utf-8;q=1, iso-8859-1")
	      (buffer (url-retrieve-synchronously url)))
    buffer))

(defun tlon-babel-process-api-buffer (buffer)
  ""
  (let ((json-object-type 'plist)
	(json-array-type 'list)
	(output-buffer-name "*uqbar error log*"))
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      ;; Kill any existing buffer with the same name before creating a new one
      (when (get-buffer output-buffer-name)
	(kill-buffer output-buffer-name))
      (let* ((output-buffer (get-buffer-create output-buffer-name))
	     (parsed-json (json-read))
	     (modified-json (mapcar (lambda (entry)
				      (plist-put entry :source_filename
						 (concat tlon-babel-dir-repos
							 (plist-get entry :source_filename))))
				    parsed-json)))
	modified-json))))

(defun tlon-babel-print-log (log)
  "Print LOG in a human-friendly way."
  (let* ((buffer (generate-new-buffer "*Log*")))
    (pp log buffer)
    (switch-to-buffer buffer)
    (emacs-lisp-mode)
    (tlon-babel-make-paths-clickable)
    (read-only-mode)
    (goto-char (point-min))))

(defun tlon-babel-make-paths-clickable ()
  "Make file paths in the current buffer clickable."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\"\\([^\"]+\\)\"" nil t)
      (let* ((match (match-string-no-properties 0))
	     (url (substring match 1 -1))
	     (path (when (file-exists-p url)
		     (abbreviate-file-name (expand-file-name url)))))
	(when path
	  (make-button (match-beginning 0) (match-end 0)
		       'action (lambda (_) (find-file path))
		       'follow-link t))))))

(defun tlon-babel-make-uqbar-request (url &optional retries)
  "Make URL request to `uqbar' API and show updated logs.
It will first authenticate and then make the request.

Retries 2 more times if response code is 504 before giving up. If RETRIES is a
number, it will retry that many times instead of 2."
  (let* ((token (tlon-babel-get-uqbar-token))
	 (retries (if (numberp retries) retries 0)))
    (request
      url
      :type "POST"
      :headers `(("Authorization" . ,(concat "Bearer " token)))
      :sync nil
      :status-code '((504 . (lambda (&rest _)
			      (message "Got 504, Gateway Timeout. My patience has a limit!"))))
      :complete (cl-function
		 (lambda (&key response &allow-other-keys)
		   (unless (request-response-error-thrown response)
		     (tlon-babel-get-uqbar-log)))))))

(defun tlon-babel-get-uqbar-token ()
  "Get `uqbar-es' API token."
  (let* ((username (tlon-babel-user-lookup :github :name user-full-name))
	 (url "https://altruismoeficaz.net/api/auth/login")
	 (url-request-method "POST")
	 (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (url-request-data
	  (concat "username=" (url-hexify-string username)
		  "&password=" (url-hexify-string
				(auth-source-pass-get 'secret (concat "tlon/BAE/altruismoeficaz.net/" username))))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (let* ((json-object-type 'alist)
	     (json-array-type 'list)
	     (json-key-type 'string)
	     (json-response (json-read-from-string (buffer-string))))
	(cdr (assoc "access_token" json-response))))))

(defun tlon-babel-update-babel-in-uqbar-es ()
  "Update the `uqbar-es' website to reflect latest changes in `babel'."
  (interactive)
  (tlon-babel-make-uqbar-request "https://altruismoeficaz.net/api/update"))

(defun tlon-babel-update-uqbar-images ()
  "Update the `uqbar-es' website to reflect latest image modifications."
  (interactive)
  (tlon-babel-make-uqbar-request "https://altruismoeficaz.net/api/update/rebuild-frontend"))

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

;;;;; GPT-4

(defun tlon-babel-file-to-string (file)
  "Read the contents of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (unfill-region (point-min) (point-max))
    (buffer-string)))

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
					;
;;;;; Translation

(defun tlon-babel-gpt-rewrite ()
  "Docstring."
  (interactive)
  (let* ((text (if (region-active-p)
		   (buffer-substring-no-properties (region-beginning) (region-end))
		 (read-string "Text to rewrite: "))))
    (gptel-request
     (format "Por favor, genera las mejores diez variantes del siguiente texto castellano:\n\n```\n%s\n```\n\n. Por favor, devuelve todas las variantes en una única linea, separadas por '|'. No insertes un espacio ni antes ni después de '|'. No agregues ningún comentario aclaratorio: solo necesito la lista de variantes. A modo de ejemplo, para la expresión 'búsqueda de poder' el texto a devolver sería: 'ansia de poder|ambición de poder|búsqueda de autoridad|sed de poder|afán de poder|aspiración de poder|anhelo de poder|deseo de control|búsqueda de dominio|búsqueda de control' (esta lista solo pretende ilustrar el formato en que debes presentar tu respuesta). Gracias!" text)
     :callback
     (lambda (response info)
       (if (not response)
	   (message "gptel-quick failed with message: %s" (plist-get info :status))
	 (let* ((variants (split-string response "|"))
		(variant (completing-read "Variant: " variants)))
	   (delete-region (region-beginning) (region-end))
	   (kill-new variant)))))))

(defun tlon-babel-gpt-translate (text)
  "Return ten alternative translations of TEXT."
  (interactive "sText to translate: ")
  (gptel-request
   (format "Generate the best ten Spanish translations of the following English text:\n\n```\n%s\n```\n\nPlease return each translation on the same line, separated by '|'. Do not add a space either before or after the '|'. Do not precede your answer by 'Here are ten Spanish translations' or any comments of that sort: just return the translations. An example return string for the word 'very beautiful' would be: 'muy bello|muy bonito|muy hermoso|muy atractivo' (etc). Thanks!" text)
   :callback
   (lambda (response info)
     (if (not response)
	 (message "gptel-quick failed with message: %s" (plist-get info :status))
       (let ((translations (split-string response "|")))
	 (kill-new (completing-read "Translation: " translations)))))))

(defun tlon-babel-gpt-translate-file (file)
  "Translate FILE."
  (let* ((counterpart (tlon-babel-get-counterpart file))
	 (filename (file-name-nondirectory counterpart))
	 (target-path (concat
		       (file-name-sans-extension filename)
		       "--gpt-translated.md")))
    (gptel-request
     (format "Translate the following text into Spanish:\n\n```\n%s\n```\n\n"
	     (tlon-babel-file-to-string file))
     :callback
     (lambda (response info)
       (if (not response)
	   (message "gptel-quick failed with message: %s" (plist-get info :status))
	 (with-temp-buffer
	   (insert response)
	   (write-region (point-min) (point-max) target-path)))))))

;; TODO: move to relevant section; not GPT-related
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

;;;;;; Summarization

(defun tlon-babel-gpt-summarize (model)
  "Summarize and copy the summary to the kill ring using AI MODEL.
If region is active, summarize the region; otherwise, prompt for a file to
summarize."
  (interactive (list (completing-read "Model: " gptel-extras-backends)))
  (require 'gptel)
  (require 'gptel-extras)
  (gptel-model-config model)
  (let ((string
	 (if (region-active-p)
	     (buffer-substring-no-properties (region-beginning) (region-end))
	   (let* ((current-file (buffer-file-name))
		  (selected-file (read-file-name "Select file to summarize (if you would like to summarize a region, run this command with an active region): " nil current-file nil (file-name-nondirectory current-file))))
	     (with-temp-buffer
	       (insert-file-contents selected-file)
	       (buffer-string))))))
    (message "Generating summary. This may take 5–30 seconds, depending on length...")
    (gptel-request
     (format "Por favor, genera un resumen del siguiente artículo:\n\n```\n%s\n```\n\n. El resumen debe tener solamente un párrafo y no es necesario que mencione datos bibliográficos de la obra reusmida (como título o autor). Escribe el resumen afirmando directamente lo que el artículo sostiene, en lugar de utilizar giros como ‘El artículo sostiene que...’. Por ejemplo, en lugar de escribir ‘El artículo cuenta que la humanidad luchó contra la viruela durante siglos...’, escribe ‘La humanidad luchó contra la viruela durante siglos...’"
	     string)
     :callback
     (lambda (response info)
       (if (not response)
	   (message "`gptel' failed with message: %s" (plist-get info :status))
	 (kill-new response)
	 (message "Copied AI-generated summary to the kill ring:\n\n%s" response))))))

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
