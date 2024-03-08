;;; tlon-babel-dispatch.el --- Transient dispatchers -*- lexical-binding: t -*-

;; Copyright (C) 2024

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

;;  Transient dispatchers.

;;; Code:

(require 'tlon-babel-core)

;;;; Functions

;; maybe move this to `simple-extras', since they are universal functions for reading symbols, numbers
(defun tlon-babel-transient-read-symbol-choice (prompt choices)
  "Return a list of CHOICES with PROMPT to be used as an `infix' reader function."
  (let* ((input (completing-read prompt (mapcar 'symbol-name choices))))
    (intern input)))

(defun tlon-babel-transient-read-number-choice (prompt choices)
  "Return a list of CHOICES with PROMPT to be used as an `infix' reader function."
  (let* ((input (completing-read prompt (mapcar 'number-to-string choices))))
    (string-to-number input)))

(defun tlon-babel-transient-read-string-choice (prompt choices)
  "Return a list of CHOICES with PROMPT to be used as an `infix' reader function."
  (let* ((input (completing-read prompt choices)))
    (substring-no-properties input)))

;;;;; Main dispatcher

;; TODO: add flag to set translation language, similar to Magit dispatch
(transient-define-prefix tlon-babel-dispatch ()
  "Dispatch a `tlon-babel' command."
  :info-manual "(tlon-babel) Main menu"
  [["Main"
    ("j" "job"                            tlon-babel-jobs-create-job)
    ("r" "dwim"                           tlon-babel-jobs-dwim)
    ("k" "Markdown"                       tlon-babel-md-menu)
    ("m" "Magit"                          tlon-babel-magit-repo-dispatch)
    ("H-m" "Meet"                         tlon-babel-meet-menu)
    ("g" "AI"                             tlon-babel-ai-menu)
    ("y" "Forg"                           tlon-babel-forg-menu)
    ("." "Notifications"                  forge-list-notifications)
    """Request"
    ("q q" "uqbar"                        tlon-babel-api-request)]
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
    ("s t" "translation"                  tlon-babel-search-for-translation)]
   ["Browse in Dired"
    ("d" "dir"                            tlon-babel-dired-dir-dispatch)
    ("H-d" "repo"                         tlon-babel-dired-repo-dispatch)
    """Browse externally"
    ("b" "current file"                   tlon-babel-browse-file)
    ("H-b" "current repo"                 tlon-babel-browse-repo)
    """Open"
    ("o" "in repo"                        tlon-babel-open-repo-dispatch)
    ("H-o" "in all repos"                 tlon-babel-open-file-in-all-repos)]
   ["File changes"
    ("h h" "log"                          magit-log-buffer-file)
    ("h d" "diffs since last user change" tlon-babel-log-buffer-latest-user-commit)
    ("h e" "ediff with last user change"  tlon-babel-log-buffer-latest-user-commit-ediff)
    """Counterpart"
    ("f" "current win"                    tlon-babel-open-counterpart-dwim)
    ("H-f" "other win"                    tlon-babel-open-counterpart-in-other-window-dwim)]
   ["""Clock"
    ("c c" "issue"                        tlon-babel-open-clock-issue)
    ("c f" "file"                         tlon-babel-open-clock-file )
    ("c o" "heading"                      org-clock-goto)
    """Issue"
    ("i i" "open counterpart"             tlon-babel-open-forge-counterpart)
    ("i I" "open file"                    tlon-babel-open-forge-file)]])

;;;;; Browse repo in Magit

(defmacro tlon-babel-generate-magit-browse-commands (name dir)
  "Generate commands for browsing repo named NAME in Magit.
DIR is the directory where the repo is stored."
  `(defun ,(intern (format "tlon-babel-magit-browse-%s" name)) ()
     ,(format "Browse the %s repository in Magit." name)
     (interactive)
     (magit-status ,dir)))

(dolist (repo tlon-babel-repos)
  (eval `(tlon-babel-generate-magit-browse-commands
	  ,(plist-get repo :abbrev)
	  ,(plist-get repo :dir))))

(transient-define-prefix tlon-babel-magit-repo-dispatch ()
  "Browse a Tlön repo in Magit."
  [["Tlon"
    ("t s" "tlon-site"                    tlon-babel-magit-browse-tlon-site)
    ("t d" "tlon-docs"                    tlon-babel-magit-browse-docs)]
   ["Babel"
    ("b c" "babel-core"                   tlon-babel-magit-browse-babel-core)
    ("b r" "babel-refs"                   tlon-babel-magit-browse-babel-refs)
    ("b s" "babel-es"                     tlon-babel-magit-browse-babel-es)
    ("b i" "babel-issues"                 tlon-babel-magit-browse-babel-issues)]
   ["Uqbar"
    ("q i" "uqbar-issues"                 tlon-babel-magit-browse-uqbar-issues)
    ("q f" "uqbar-front"                  tlon-babel-magit-browse-uqbar-front)
    ("q a" "uqbar-api"                    tlon-babel-magit-browse-uqbar-api)
    ("q n" "uqbar-en"                     tlon-babel-magit-browse-uqbar-en)
    ("q s" "uqbar-es"                     tlon-babel-magit-browse-uqbar-es)]
   ["utilitarianism"
    ("u n" "utilitarianism-en"            tlon-babel-magit-browse-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-babel-magit-browse-utilitarianism-es)]
   ["Essays on Longtermism"
    ("e n" "essays-en"                    tlon-babel-magit-browse-essays-en)
    ("e s" "essays-es"                    tlon-babel-magit-browse-essays-es)]
   ["EA News"
    ("n i" "ean-issues"                   tlon-babel-magit-browse-ean-issues)
    ("n f" "ean-front"                    tlon-babel-magit-browse-ean-front)
    ("n a" "ean-api"                      tlon-babel-magit-browse-ean-api)]
   ["EA International"
    ("i i" "ea.international"             tlon-babel-magit-browse-ea-international)]
   ["La Bisagra"
    ("s s" "bisagra"                      tlon-babel-magit-browse-bisagra)]
   ["Boletín"
    ("a a" "boletin"                      tlon-babel-magit-browse-boletin)]
   ["Meetings"
    ("m l" "Leo-Pablo"                    tlon-babel-magit-browse-meetings-leo-pablo)
    ("m f" "Fede-Pablo"                   tlon-babel-magit-browse-meetings-fede-pablo)
    ("m m" "Fede-Leo"                     tlon-babel-magit-browse-meetings-fede-leo)]])

;;;;; Open repo

(defmacro tlon-babel-generate-open-file-in-repo-commands (repo)
  "Generate commands to open file in REPO."
  (let* ((repo-name (tlon-babel-repo-lookup :abbrev :dir repo))
	 (command-name (intern (concat "tlon-babel-open-file-in-" repo-name))))
    `(defun ,command-name ()
       ,(format "Interactively open a file from a list of all files in `%s'" repo-name)
       (interactive)
       (tlon-babelq-open-file-in-repo ,repo))))

(dolist (repo (tlon-babel-repo-lookup-all :dir))
  (eval `(tlon-babel-generate-open-file-in-repo-commands ,repo)))

(transient-define-prefix tlon-babel-open-repo-dispatch ()
  "Interactively open a file from a Tlön repo."
  [["Tlon"
    ("t s" "tlon-site"                    tlon-babel-open-file-in-tlon-site)
    ("t d" "tlon-docs"                    tlon-babel-open-file-in-docs)]
   ["Babel"
    ("b c" "babel-core"                   tlon-babel-open-file-in-babel-core)
    ("b r" "babel-refs"                   tlon-babel-open-file-in-babel-refs)
    ("b s" "babel-es"                     tlon-babel-open-file-in-babel-es)
    ("b i" "babel-issues"                 tlon-babel-open-file-in-babel-issues)]
   ["Uqbar"
    ("q i" "uqbar-issues"                 tlon-babel-open-file-in-uqbar-issues)
    ("q f" "uqbar-front"                  tlon-babel-open-file-in-uqbar-front)
    ("q a" "uqbar-api"                    tlon-babel-open-file-in-uqbar-api)
    ("q n" "uqbar-en"                     tlon-babel-open-file-in-uqbar-en)
    ("q s" "uqbar-es"                     tlon-babel-open-file-in-uqbar-es)]
   ["utilitarianism"
    ("u n" "utilitarianism-en"            tlon-babel-open-file-in-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-babel-open-file-in-utilitarianism-es)]
   ["Essays on Longtermism"
    ("e n" "essays-en"                    tlon-babel-open-file-in-essays-en)
    ("e s" "essays-es"                    tlon-babel-open-file-in-essays-es)]
   ["EA News"
    ("n i" "ean-issues"                   tlon-babel-open-file-in-ean-issues)
    ("n f" "ean-front"                    tlon-babel-open-file-in-ean-front)
    ("n a" "ean-api"                      tlon-babel-open-file-in-ean-api)]
   ["EA International"
    ("i " "ea-international"              tlon-babel-open-file-in-ea-international)]
   ["La Bisagra"
    ("s s" "bisagra"                      tlon-babel-open-file-in-bisagra)]
   ["Boletín"
    ("a a" "boletin"                      tlon-babel-open-file-in-boletin)]])

;;;;; Browse repo in Dired

(defmacro tlon-babel-generate-dired-browse-commands (name dir)
  "Generate commands for browsing repo named NAME.
DIR is the directory where the repo is stored."
  `(defun ,(intern (format "tlon-babel-dired-browse-%s" name)) ()
     ,(format "Browse the %s repository in Dired." name)
     (interactive)
     (dired ,dir)))

(dolist (repo tlon-babel-repos)
  (eval `(tlon-babel-generate-dired-browse-commands
	  ,(plist-get repo :abbrev)
	  ,(plist-get repo :dir))))

(transient-define-prefix tlon-babel-dired-repo-dispatch ()
  "Browse a Tlön repo in Dired."
  [["Tlon"
    ("t s" "tlon-site"                    tlon-babel-dired-browse-tlon-site)
    ("t d" "tlon-docs"                    tlon-babel-dired-browse-docs)]
   ["Babel"
    ("b c" "babel-core"                   tlon-babel-dired-browse-babel-core)
    ("b r" "babel-refs"                   tlon-babel-dired-browse-babel-refs)
    ("b s" "babel-es"                     tlon-babel-dired-browse-babel-es)
    ("b i" "babel-issues"                 tlon-babel-dired-browse-babel-issues)]
   ["Uqbar"
    ("q i" "uqbar-issues"                 tlon-babel-dired-browse-uqbar-issues)
    ("q f" "uqbar-front"                  tlon-babel-dired-browse-uqbar-front)
    ("q a" "uqbar-api"                    tlon-babel-dired-browse-uqbar-api)
    ("q n" "uqbar-en"                     tlon-babel-dired-browse-uqbar-en)
    ("q s" "uqbar-es"                     tlon-babel-dired-browse-uqbar-es)]
   ["utilitarianism"
    ("u n" "utilitarianism-en"            tlon-babel-dired-browse-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-babel-dired-browse-utilitarianism-es)]
   ["Essays on Longtermism"
    ("e n" "essays-en"                    tlon-babel-dired-browse-essays-en)
    ("e s" "essays-es"                    tlon-babel-dired-browse-essays-es)]
   ["EA News"
    ("n i" "ean-issues"                   tlon-babel-dired-browse-ean-issues)
    ("n f" "ean-front"                    tlon-babel-dired-browse-ean-front)
    ("n a" "ean-api"                      tlon-babel-dired-browse-ean-api)]
   ["EA International"
    ("i i" "ea-international"             tlon-babel-dired-browse-ea-international)]
   ["La Bisagra"
    ("s s" "bisagra"                      tlon-babel-dired-browse-bisagra)]
   ["Boletín"
    ("a a" "boletin"                      tlon-babel-dired-browse-boletin)]])

;;;;; Browse repo dir in Dired

(defmacro tlon-babel-generate-dir-commands (name dir entity)
  "Generate commands for browsing ENTITY subdirectory in repo named NAME.
DIR is the directory where the repo is stored."
  `(progn
     (defun ,(intern (format "tlon-babel-dired-browse-%s-dir-in-%s" entity name)) ()
       ,(format "Browse the `%s' directory in the `%s' repository." entity name)
       (interactive)
       (tlon-babel-browse-entity-dir ,entity ,dir))))

(defun tlon-babel-get-entity-types ()
  "Return a list of entity types."
  (let (collection)
    (dolist (list tlon-babel-core-bare-dirs)
      (dolist (cons list)
	(when (string= (car cons) "en")
	  (push (cdr cons) collection))))
    collection))

(dolist (repo tlon-babel-repos)
  (dolist (entity (tlon-babel-get-entity-types))
    (eval `(tlon-babel-generate-dir-commands
	    ,(plist-get repo :abbrev)
	    ,(plist-get repo :dir)
	    ,entity))))

(defmacro tlon-babel-generate-entity-dispatch (name)
  "Generate a dispatcher for browsing an entity named NAME in a repo."
  `(transient-define-prefix ,(intern (format "tlon-babel-browse-entity-in-%s-dispatch" name)) ()
     ,(format "Browse a directory in the `%s' repo." name)
     [["directories"
       ("a" "articles"         ,(intern (format "tlon-babel-dired-browse-articles-dir-in-%s" name)))
       ("t" "tags"             ,(intern (format "tlon-babel-dired-browse-tags-dir-in-%s" name)))
       ("u" "authors"          ,(intern (format "tlon-babel-dired-browse-authors-dir-in-%s" name)))
       ("c" "collections"      ,(intern (format "tlon-babel-dired-browse-collections-dir-in-%s" name)))]]))

(dolist (repo (tlon-babel-repo-lookup-all :abbrev :type 'content))
  (eval `(tlon-babel-generate-entity-dispatch ,repo)))

(transient-define-prefix tlon-babel-dired-dir-dispatch ()
  "Browse a Tlön repo directory in Dired."
  [["Uqbar"
    ("q n" "uqbar-en"                    tlon-babel-browse-entity-in-uqbar-en-dispatch)
    ("q s" "uqbar-es"                    tlon-babel-browse-entity-in-uqbar-es-dispatch)]
   ["utilitarianism"
    ("u n" "utilitarianism-en"           tlon-babel-browse-entity-in-utilitarianism-en-dispatch)
    ("u s" "utilitarianism-es"           tlon-babel-browse-entity-in-utilitarianism-es-dispatch)]
   ["Essays on Longtermism"
    ("e n" "essays-en"                   tlon-babel-browse-entity-in-essays-en-dispatch)
    ("e s" "essays-es"                   tlon-babel-browse-entity-in-essays-es-dispatch)]])

(provide 'tlon-babel-dispatch)
;;; tlon-babel-dispatch.el ends here

