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

;;;;; Main menu

;; TODO: add flag to set translation language, similar to Magit dispatch
;;;###autoload (autoload 'tlon-babel-dispatch "tlon-babel-dispatch" nil t)
(transient-define-prefix tlon-babel-dispatch ()
  "Dispatch a `tlon-babel' command."
  :info-manual "(tlon-babel) Main menu"
  [["Main menu"
    "Submenus"
    ("a" "AI"                             tlon-babel-ai-menu)
    ("e" "edit data"                      tlon-babel-edit-data-menu)
    ("f" "files"                          tlon-babel-files-menu)
    ("i" "api"                            tlon-babel-api-menu)
    ("j" "jobs"                           tlon-babel-jobs-menu)
    ("k" "markdown"                       tlon-babel-md-menu)
    ("H-m" "meet"                         tlon-babel-meet-menu)
    ("r" "repos"                          tlon-babel-repos-menu)
    ("s" "search"                         tlon-babel-search-menu)
    ("y" "forg"                           tlon-babel-forg-menu)]
   ["""Browse repo"
    ("d" "in Dired"                       tlon-babel-dired-repo-menu)
    ("m" "in Magit"                       tlon-babel-magit-repo-menu)]
   [""""
    ("." "notifications"                  forge-list-notifications)]])

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

;;;###autoload (autoload 'tlon-babel-magit-repo-menu "tlon-babel-dispatch" nil t)
(transient-define-prefix tlon-babel-magit-repo-menu ()
  "Browse a Tlön repo in Magit."
  [["Tlon"
    ("t s" "tlon-site"                    tlon-babel-magit-browse-tlon-site)
    ("t d" "tlon-docs"                    tlon-babel-magit-browse-docs)]
   ["Babel"
    ("b c" "babel-core"                   tlon-babel-magit-browse-babel-core)
    ("b r" "babel-refs"                   tlon-babel-magit-browse-babel-refs)
    ("b i" "babel-issues"                 tlon-babel-magit-browse-babel-issues)
    ""
    ("b d" "babel-de"                     tlon-babel-magit-browse-babel-de)
    ("b s" "babel-es"                     tlon-babel-magit-browse-babel-es)
    ("b f" "babel-fr"                     tlon-babel-magit-browse-babel-fr)
    ("b t" "babel-it"                     tlon-babel-magit-browse-babel-it)]
   ["Uqbar"
    ("q i" "uqbar-issues"                 tlon-babel-magit-browse-uqbar-issues)
    ("q f" "uqbar-front"                  tlon-babel-magit-browse-uqbar-front)
    ("q a" "uqbar-api"                    tlon-babel-magit-browse-uqbar-api)
    ""
    ("q n" "uqbar-en"                     tlon-babel-magit-browse-uqbar-en)
    ("q s" "uqbar-es"                     tlon-babel-magit-browse-uqbar-es)
    ("q f" "uqbar-fr"                     tlon-babel-magit-browse-uqbar-fr)
    ("q t" "uqbar-it"                     tlon-babel-magit-browse-uqbar-it)]
   ["utilitarianism"
    ("u n" "utilitarianism-en"            tlon-babel-magit-browse-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-babel-magit-browse-utilitarianism-es)
    ""
    "Essays on Longtermism"
    ("e n" "essays-en"                    tlon-babel-magit-browse-essays-en)
    ("e s" "essays-es"                    tlon-babel-magit-browse-essays-es)
    ""
    "EA International"
    ("i i" "ea.international"             tlon-babel-magit-browse-ea-international)]
   ["La Bisagra"
    ("s d" "bisagra-dev"                  tlon-babel-magit-browse-bisagra-dev)
    ("s c" "bisagra-content"              tlon-babel-magit-browse-bisagra-content)
    ""
    "Boletín"
    ("a a" "boletin"                      tlon-babel-magit-browse-boletin)]
   ["EA News"
    ("n i" "ean-issues"                   tlon-babel-magit-browse-ean-issues)
    ("n f" "ean-front"                    tlon-babel-magit-browse-ean-front)
    ("n a" "ean-api"                      tlon-babel-magit-browse-ean-api)]
   ["Meetings"
    ("m l" "Leo-Pablo"                    tlon-babel-magit-browse-meetings-leo-pablo)
    ("m f" "Fede-Pablo"                   tlon-babel-magit-browse-meetings-fede-pablo)
    ("m m" "Fede-Leo"                     tlon-babel-magit-browse-meetings-fede-leo)]])

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

;;;###autoload (autoload 'tlon-babel-dired-repo-menu "tlon-babel-dispatch" nil t)
(transient-define-prefix tlon-babel-dired-repo-menu ()
  "Browse a Tlön repo in Dired."
  [["Tlon"
    ("t s" "tlon-site"                    tlon-babel-dired-browse-tlon-site)
    ("t d" "tlon-docs"                    tlon-babel-dired-browse-docs)]
   ["Babel"
    ("b c" "babel-core"                   tlon-babel-dired-browse-babel-core)
    ("b i" "babel-issues"                 tlon-babel-dired-browse-babel-issues)
    ("b r" "babel-refs"                   tlon-babel-dired-browse-babel-refs)
    ""
    ("b d" "babel-de"                     tlon-babel-dired-browse-babel-de)
    ("b s" "babel-es"                     tlon-babel-dired-browse-babel-es)
    ("b f" "babel-fr"                     tlon-babel-dired-browse-babel-fr)
    ("b t" "babel-it"                     tlon-babel-dired-browse-babel-it)]
   ["Uqbar"
    ("q i" "uqbar-issues"                 tlon-babel-dired-browse-uqbar-issues)
    ("q f" "uqbar-front"                  tlon-babel-dired-browse-uqbar-front)
    ("q a" "uqbar-api"                    tlon-babel-dired-browse-uqbar-api)
    ""
    ("q n" "uqbar-en"                     tlon-babel-dired-browse-uqbar-en)
    ("q s" "uqbar-es"                     tlon-babel-dired-browse-uqbar-es)
    ("q f" "uqbar-fr"                     tlon-babel-dired-browse-uqbar-fr)
    ("q t" "uqbar-it"                     tlon-babel-dired-browse-uqbar-it)]
   ["utilitarianism"
    ("u n" "utilitarianism-en"            tlon-babel-dired-browse-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-babel-dired-browse-utilitarianism-es)
    ""
    "Essays on Longtermism"
    ("e n" "essays-en"                    tlon-babel-dired-browse-essays-en)
    ("e s" "essays-es"                    tlon-babel-dired-browse-essays-es)
    ""
    "EA International"
    ("i i" "ea-international"             tlon-babel-dired-browse-ea-international)]
   ["La Bisagra"
    ("s d" "bisagra-dev"                  tlon-babel-dired-browse-bisagra-dev)
    ("s c" "bisagra-content"              tlon-babel-dired-browse-bisagra-content)
    ""
    "Boletín"
    ("a a" "boletin"                      tlon-babel-dired-browse-boletin)]
   ["EA News"
    ("n i" "ean-issues"                   tlon-babel-dired-browse-ean-issues)
    ("n f" "ean-front"                    tlon-babel-dired-browse-ean-front)
    ("n a" "ean-api"                      tlon-babel-dired-browse-ean-api)]])

;;;;; Open file in repo

(defmacro tlon-babel-generate-open-file-in-repo-commands (repo)
  "Generate commands to open file in REPO."
  (let* ((repo-name (tlon-babel-repo-lookup :abbrev :dir repo))
	 (command-name (intern (concat "tlon-babel-open-file-in-" repo-name))))
    `(defun ,command-name ()
       ,(format "Interactively open a file from a list of all files in `%s'" repo-name)
       (interactive)
       (tlon-babel-find-file-in-repo ,repo))))

(dolist (repo (tlon-babel-repo-lookup-all :dir))
  (eval `(tlon-babel-generate-open-file-in-repo-commands ,repo)))

;;;###autoload (autoload 'tlon-babel-find-file-in-repo-menu "tlon-babel-dispatch" nil t)
(transient-define-prefix tlon-babel-find-file-in-repo-menu ()
  "Interactively open a file from a Tlön repo."
  [["Tlon"
    ("t s" "tlon-site"                    tlon-babel-open-file-in-tlon-site)
    ("t d" "tlon-docs"                    tlon-babel-open-file-in-docs)]
   ["Babel"
    ("b c" "babel-core"                   tlon-babel-open-file-in-babel-core)
    ("b r" "babel-refs"                   tlon-babel-open-file-in-babel-refs)
    ("b i" "babel-issues"                 tlon-babel-open-file-in-babel-issues)
    ""
    ("b d" "babel-de"                     tlon-babel-open-file-in-babel-de)
    ("b s" "babel-es"                     tlon-babel-open-file-in-babel-es)
    ("b f" "babel-fr"                     tlon-babel-open-file-in-babel-fr)
    ("b t" "babel-it"                     tlon-babel-open-file-in-babel-it)]
   ["Uqbar"
    ("q i" "uqbar-issues"                 tlon-babel-open-file-in-uqbar-issues)
    ("q f" "uqbar-front"                  tlon-babel-open-file-in-uqbar-front)
    ("q a" "uqbar-api"                    tlon-babel-open-file-in-uqbar-api)
    ""
    ("q n" "uqbar-en"                     tlon-babel-open-file-in-uqbar-en)
    ("q s" "uqbar-es"                     tlon-babel-open-file-in-uqbar-es)
    ("q f" "uqbar-fr"                     tlon-babel-open-file-in-uqbar-fr)
    ("q t" "uqbar-it"                     tlon-babel-open-file-in-uqbar-it)]
   ["utilitarianism"
    ("u n" "utilitarianism-en"            tlon-babel-open-file-in-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-babel-open-file-in-utilitarianism-es)
    ""
    "Essays on Longtermism"
    ("e n" "essays-en"                    tlon-babel-open-file-in-essays-en)
    ("e s" "essays-es"                    tlon-babel-open-file-in-essays-es)
    ""
    "EA International"
    ("i " "ea-international"              tlon-babel-open-file-in-ea-international)]
   ["La Bisagra"
    ("s s" "bisagra-dev"                  tlon-babel-open-file-in-bisagra-dev)
    ("s c" "bisagra-content"              tlon-babel-open-file-in-bisagra-content)
    ""
    "Boletín"
    ("a a" "boletin"                      tlon-babel-open-file-in-boletin)]
   ["EA News"
    ("n i" "ean-issues"                   tlon-babel-open-file-in-ean-issues)
    ("n f" "ean-front"                    tlon-babel-open-file-in-ean-front)
    ("n a" "ean-api"                      tlon-babel-open-file-in-ean-api)]
   ["Meetings"
    ("m l" "Leo-Pablo"                    tlon-babel-open-file-in-meetings-leo-pablo)
    ("m f" "Fede-Pablo"                   tlon-babel-open-file-in-meetings-fede-pablo)
    ("m m" "Fede-Leo"                     tlon-babel-open-file-in-meetings-fede-leo)]])

;;;;; Files menu

;;;###autoload (autoload 'tlon-babel-files-menu "tlon-babel-dispatch" nil t)
(transient-define-prefix tlon-babel-files-menu ()
  "Files menu."
  :info-manual "(tlon-babel) Files"
  [["Find"
    ("c" "in current repo"                 tlon-babel-find-file-in-repo)
    ("o" "in another repo"                 tlon-babel-find-file-in-repo-menu)
    ("a" "across repos"                    tlon-babel-open-file-across-repos)]
   ["Open counterpart"
    ("f" "current window"                  tlon-babel-open-counterpart-dwim)
    ("H-f" "other window"                  tlon-babel-open-counterpart-in-other-window-dwim)]
   [""
    ("b" "Browse externally"               tlon-babel-browse-file)]
   ["Issue"
    ("i" "file"                            tlon-babel-open-forge-file)
    ("I" "counterpart"                     tlon-babel-open-forge-counterpart)]
   ["Version control"
    ("l" "log"                             magit-log-buffer-file)
    ("d" "diffs since last user change"    tlon-babel-log-buffer-latest-user-commit)
    ("e" "ediff with last user change"     tlon-babel-log-buffer-latest-user-commit-ediff)]])

;;;;; Search menu

;;;###autoload (autoload 'tlon-babel-search-menu "tlon-babel-dispatch" nil t)
(transient-define-prefix tlon-babel-search-menu ()
  "Search menu."
  ["Search"
   ("s" "multi"                        tlon-babel-search-multi)
   ("c" "commits"                      tlon-babel-search-commits)
   ("d" "commit-diffs"                 tlon-babel-search-commit-diffs)
   ("f" "files"                        tlon-babel-search-files)
   ("i" "issues"                       tlon-babel-search-issues)
   ("t" "translation"                  tlon-babel-search-for-translation)])

;;;;; Data files menu

;;;###autoload (autoload 'tlon-babel-edit-data-menu "tlon-babel-dispatch" nil t)
(transient-define-prefix tlon-babel-edit-data-menu ()
  "Data files menu."
  ["Edit data files"
   "Glossary"
   ("g" "edit glossary"              tlon-babel-edit-glossary)
   ("G" "extract glossary"           tlon-babel-extract-glossary)
   "TTS"
   ("a" "abbreviations"              tlon-babel-edit-abbreviations)
   ("r" "phonetic replacements"      tlon-babel-edit-phonetic-replacements)
   ("t" "phonetic transcriptions"    tlon-babel-edit-phonetic-transcriptions)
   ("A" "file-local abbreviations"   tlon-babel-add-file-local-abbreviation)
   ("R" "file-local replacements"    tlon-babel-add-file-local-replacement)])

(provide 'tlon-babel-dispatch)
;;; tlon-babel-dispatch.el ends here
