;;; tlon-dispatch.el --- Transient dispatchers -*- lexical-binding: t -*-

;; Copyright (C) 2025

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

;;  Transient dispatchers.

;;; Code:

(require 'tlon)
(require 'tlon-contacts) ; `tlon-contacts-menu' can't be autoloaded
(require 'tlon-core)
(require 'transient)

;;;; Functions

;;;;; Main menu

;; TODO: add flag to set translation language, similar to Magit dispatch
;;;###autoload (autoload 'tlon-dispatch "tlon-dispatch" nil t)
(transient-define-prefix tlon-dispatch ()
  "Dispatch a `tlon' command."
  :info-manual "(tlon) Main menu"
  [["Main menu"
    "Submenus"
    ("a" "AI"                             tlon-ai-menu)
    ("c" "clock"                          tlon-clock-menu)
    ("H-d" "dict"                         tlon-dict-menu)
    ("f" "files"                          tlon-files-menu)
    ("g" "glossary"                       tlon-glossary-menu)
    ("i" "images"                         tlon-images-menu)
    ("j" "jobs"                           tlon-jobs-menu)
    ("k" "markdown"                       tlon-md-menu)
    ("l" "DeepL"                          tlon-deepl-menu)
    ("H-l" "color"                        tlon-color-menu)
    ("H-m" "meet"                         tlon-meet-menu)]
   [""""
    ("o" "count"                          tlon-count-menu)
    ("p" "api"                            tlon-api-menu)
    ("r" "repos"                          tlon-repos-menu)
    ("s" "search"                         tlon-search-menu)
    ("S" "split"                          tlon-split-menu)
    ("t" "contacts"                       tlon-contacts-menu)
    ("u" "counterpart"                    tlon-counterpart-menu)
    ("v" "url"                            tlon-url-menu)
    ("y" "forg"                           tlon-forg-menu)
    ("x" "tex"                            tlon-tex-menu)
    ("z" "tts"                            tlon-tts-menu)]
   [""
    "Browse repo"
    ("d" "in Dired"                       tlon-dired-repo-menu)
    ("m" "in Magit"                       tlon-magit-repo-menu)
    ""
    ("." "notifications"                  forge-list-notifications)
    ""]
   [""
    "Package"
    ("H-u" "update & reload"              tlon-update-package-and-reload)
    ("H-o" "open"                         tlon-open-package-files)
    ("H-c" "copy info"                    tlon-copy-package-info)
    """"""""""
    ("?" "get help"                       tlon-ai-get-help)
    ("!" "report bug"                     tlon-report-bug)
    ""
    (:info* (lambda () (concat "Package version: " (propertize tlon-version 'face 'bold))))
    (:info* (lambda () (format "Latest commit: %s" (propertize (tlon-get-latest-commit) 'face 'bold))))]])

;;;;;; Common elements

(transient-define-infix tlon-menu-infix-toggle-debug ()
  "Toggle the value of `tlon-debug' in `ai' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-debug
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-debug)))

;;;;; Browse repo in Magit

(defmacro tlon-generate-magit-browse-commands (name dir)
  "Generate commands for browsing repo named NAME in Magit.
DIR is the directory where the repo is stored."
  `(defun ,(intern (format "tlon-magit-browse-%s" name)) ()
     ,(format "Browse the %s repository in Magit." name)
     (interactive)
     (magit-status ,dir)))

(dolist (repo tlon-repos)
  (eval `(tlon-generate-magit-browse-commands
	  ,(plist-get repo :abbrev)
	  ,(plist-get repo :dir))))

;;;###autoload (autoload 'tlon-magit-repo-menu "tlon-dispatch" nil t)
(transient-define-prefix tlon-magit-repo-menu ()
  "Browse a Tlön repo in Magit."
  [["Docs"
    ("d d" "tlon-docs"                    tlon-magit-browse-docs)
    ""
    "Tlön site"
    ("t t" "tlon"                         tlon-magit-browse-tlon)
    ("t c" "tlon-content"                 tlon-magit-browse-tlon-content)]
   ["Babel"
    ("b c" "babel-core"                   tlon-magit-browse-babel-core)
    ("b f" "babel-refs"                   tlon-magit-browse-babel-refs)
    ""
    ("b d" "babel-de"                     tlon-magit-browse-babel-de)
    ("b s" "babel-es"                     tlon-magit-browse-babel-es)
    ("b r" "babel-fr"                     tlon-magit-browse-babel-fr)
    ("b t" "babel-it"                     tlon-magit-browse-babel-it)]
   ["Uqbar"
    ("q q" "uqbar"                        tlon-magit-browse-uqbar)
    ("q d" "uqbar-audio"                  tlon-magit-browse-uqbar-audio)
    ""
    ("q a" "uqbar-ar"                     tlon-magit-browse-uqbar-ar)
    ("q n" "uqbar-en"                     tlon-magit-browse-uqbar-en)
    ("q s" "uqbar-es"                     tlon-magit-browse-uqbar-es)
    ("q r" "uqbar-fr"                     tlon-magit-browse-uqbar-fr)
    ("q t" "uqbar-it"                     tlon-magit-browse-uqbar-it)
    ("q j" "uqbar-ja"                     tlon-magit-browse-uqbar-ja)
    ("q k" "uqbar-ko"                     tlon-magit-browse-uqbar-ko)]
   ["utilitarianism"
    ("u n" "utilitarianism-en"            tlon-magit-browse-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-magit-browse-utilitarianism-es)
    ""
    "EA International"
    ("i i" "ea.international"             tlon-magit-browse-ea-international)
    ""
    "80k podcast"
    ("p p" "80k podcast"                  tlon-magit-browse-80k-podcast)]
   ["La bisagra de la historia"
    ("s s" "bisagra"                      tlon-magit-browse-bisagra)
    ("s c" "bisagra-content"              tlon-magit-browse-bisagra-content)
    ""
    "Altruismo Eficaz Argentina"
    ("a a" "aea"                          tlon-magit-browse-aea)
    ("a c" "aea-content"                  tlon-magit-browse-aea-content)
    ""
    "Boletín"
    ("l l" "boletin"                      tlon-magit-browse-boletin)
    ""
    "EA News"
    ("n n" "ean"                          tlon-magit-browse-ean)]
   ["Meetings"
    ("m l p" "Leo-Pablo"                  tlon-magit-browse-meetings-leo-pablo)
    ("m f p" "Fede-Pablo"                 tlon-magit-browse-meetings-fede-pablo)
    ("m f l" "Fede-Leo"                   tlon-magit-browse-meetings-fede-leo)
    ("m g" "group"                        tlon-magit-browse-meetings-group)
    ""
    "Clock"
    ("c l" "Leo"                          tlon-magit-browse-clock-leo)
    ("c f" "Fede"                         tlon-magit-browse-clock-fede)
    ("c p" "Pablo"                        tlon-magit-browse-clock-pablo)]])

;;;;; Browse repo in Dired

(defmacro tlon-generate-dired-browse-commands (name dir)
  "Generate commands for browsing repo named NAME.
DIR is the directory where the repo is stored."
  `(defun ,(intern (format "tlon-dired-browse-%s" name)) ()
     ,(format "Browse the %s repository in Dired." name)
     (interactive)
     (dired ,dir)))

(dolist (repo tlon-repos)
  (eval `(tlon-generate-dired-browse-commands
	  ,(plist-get repo :abbrev)
	  ,(plist-get repo :dir))))

;;;###autoload (autoload 'tlon-dired-repo-menu "tlon-dispatch" nil t)
(transient-define-prefix tlon-dired-repo-menu ()
  "Browse a Tlön repo in Dired."
  [["Docs"
    ("d d" "tlon-docs"                    tlon-dired-browse-docs)
    ""
    "Tlön site"
    ("t t" "tlon"                         tlon-dired-browse-tlon)
    ("t c" "tlon-content"                 tlon-dired-browse-tlon-content)]
   ["Babel"
    ("b c" "babel-core"                   tlon-dired-browse-babel-core)
    ("b f" "babel-refs"                   tlon-dired-browse-babel-refs)
    ""
    ("b d" "babel-de"                     tlon-dired-browse-babel-de)
    ("b s" "babel-es"                     tlon-dired-browse-babel-es)
    ("b r" "babel-fr"                     tlon-dired-browse-babel-fr)
    ("b t" "babel-it"                     tlon-dired-browse-babel-it)]
   ["Uqbar"
    ("q q" "uqbar"                        tlon-dired-browse-uqbar)
    ("q d" "uqbar-audio"                  tlon-dired-browse-uqbar-audio)
    ""
    ("q a" "uqbar-ar"                     tlon-dired-browse-uqbar-ar)
    ("q n" "uqbar-en"                     tlon-dired-browse-uqbar-en)
    ("q s" "uqbar-es"                     tlon-dired-browse-uqbar-es)
    ("q r" "uqbar-fr"                     tlon-dired-browse-uqbar-fr)
    ("q t" "uqbar-it"                     tlon-dired-browse-uqbar-it)
    ("q j" "uqbar-ja"                     tlon-dired-browse-uqbar-ja)
    ("q k" "uqbar-ko"                     tlon-dired-browse-uqbar-ko)]
   ["utilitarianism"
    ("u n" "utilitarianism-en"            tlon-dired-browse-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-dired-browse-utilitarianism-es)
    ""
    "EA International"
    ("i i" "ea.international"             tlon-dired-browse-ea-international)
    ""
    "80k podcast"
    ("p p" "80k podcast"                  tlon-dired-browse-80k-podcast)]
   ["La bisagra de la historia"
    ("s s" "bisagra"                      tlon-dired-browse-bisagra)
    ("s c" "bisagra-content"              tlon-dired-browse-bisagra-content)
    ""
    "Altruismo Eficaz Argentina"
    ("a a" "aea"                          tlon-dired-browse-aea)
    ("a c" "aea-content"                  tlon-dired-browse-aea-content)
    ""
    "Boletín"
    ("l l" "boletin"                      tlon-dired-browse-boletin)
    ""
    "EA News"
    ("n n" "ean"                          tlon-dired-browse-ean)]
   ["Meetings"
    ("m l p" "Leo-Pablo"                  tlon-dired-browse-meetings-leo-pablo)
    ("m f p" "Fede-Pablo"                 tlon-dired-browse-meetings-fede-pablo)
    ("m f l" "Fede-Leo"                   tlon-dired-browse-meetings-fede-leo)
    ("m g" "group"                        tlon-dired-browse-meetings-group)
    ""
    "Clock"
    ("c l" "Leo"                          tlon-dired-browse-clock-leo)
    ("c f" "Fede"                         tlon-dired-browse-clock-fede)
    ("c p" "Pablo"                        tlon-dired-browse-clock-pablo)]])

;;;;; Open file in repo

(defmacro tlon-generate-open-file-in-repo-commands (repo)
  "Generate commands to open file in REPO."
  (let* ((repo-name (tlon-repo-lookup :abbrev :dir repo))
	 (command-name (intern (concat "tlon-open-file-in-" repo-name))))
    `(defun ,command-name ()
       ,(format "Interactively open a file from a list of all files in `%s'" repo-name)
       (interactive)
       (tlon-find-file-in-repo ,repo))))

(dolist (repo (tlon-repo-lookup-all :dir))
  (eval `(tlon-generate-open-file-in-repo-commands ,repo)))

;;;###autoload (autoload 'tlon-find-file-in-repo-menu "tlon-dispatch" nil t)
(transient-define-prefix tlon-find-file-in-repo-menu ()
  "Interactively open a file from a Tlön repo."
  [["Tlon"
    ("t c" "tlon-content"                 tlon-open-file-in-tlon-content)
    ("t f" "tlon-front"                   tlon-open-file-in-tlon-front)
    ("t d" "tlon-docs"                    tlon-open-file-in-docs)
    ("t e" "tlon.el"                      tlon-open-file-in-tlon.el)]
   ["Babel"
    ("b c" "babel-core"                   tlon-open-file-in-babel-core)
    ("b f" "babel-refs"                   tlon-open-file-in-babel-refs)
    ("b i" "babel-issues"                 tlon-open-file-in-babel-issues)
    ""
    ("b d" "babel-de"                     tlon-open-file-in-babel-de)
    ("b s" "babel-es"                     tlon-open-file-in-babel-es)
    ("b r" "babel-fr"                     tlon-open-file-in-babel-fr)
    ("b t" "babel-it"                     tlon-open-file-in-babel-it)]
   ["Uqbar"
    ("q q" "uqbar"                        tlon-open-file-in-uqbar)
    ("q d" "uqbar"                        tlon-open-file-in-uqbar-audio)
    ""
    ("q a" "uqbar-ar"                     tlon-open-file-in-uqbar-ar)
    ("q n" "uqbar-en"                     tlon-open-file-in-uqbar-en)
    ("q s" "uqbar-es"                     tlon-open-file-in-uqbar-es)
    ("q r" "uqbar-fr"                     tlon-open-file-in-uqbar-fr)
    ("q t" "uqbar-it"                     tlon-open-file-in-uqbar-it)
    ("q j" "uqbar-ja"                     tlon-open-file-in-uqbar-ja)
    ("q k" "uqbar-ko"                     tlon-open-file-in-uqbar-ko)]
   ["utilitarianism"
    ("u n" "utilitarianism-en"            tlon-open-file-in-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-open-file-in-utilitarianism-es)
    ""
    "EA International"
    ("i " "ea-international"              tlon-open-file-in-ea-international)
    ""
    "80k podcast"
    ("i " "80 podcast"                    tlon-open-file-in-80k-podcast)]
   ["La bisagra de la historia"
    ("s a" "bisagra-dev"                  tlon-open-file-in-bisagra-api)
    ("s f" "bisagra-front"                tlon-open-file-in-bisagra-front)
    ("s c" "bisagra-content"              tlon-open-file-in-bisagra-content)
    ""
    "Altruismo Eficaz Argentina"
    ("r f" "aea-front"                    tlon-open-file-in-aea-front)
    ("r c" "aea-content"                  tlon-open-file-in-aea-content)
    ""
    "Boletín"
    ("a a" "boletin"                      tlon-open-file-in-boletin)]
   ["EA News"
    ("n i" "ean-issues"                   tlon-open-file-in-ean-issues)
    ("n f" "ean-front"                    tlon-open-file-in-ean-front)
    ("n a" "ean-api"                      tlon-open-file-in-ean-api)]
   ["Meetings"
    ("m l p" "Leo-Pablo"                  tlon-open-file-in-meetings-leo-pablo)
    ("m f p" "Fede-Pablo"                 tlon-open-file-in-meetings-fede-pablo)
    ("m f l" "Fede-Leo"                   tlon-open-file-in-meetings-fede-leo)
    ("m g" "group"                        tlon-open-file-in-meetings-group)
    ""
    "Clock"
    ("c l" "Leo"                          tlon-open-file-in-clock-leo)
    ("c f" "Fede"                         tlon-open-file-in-clock-fede)
    ("c p" "Pablo"                        tlon-open-file-in-clock-pablo)]])

;;;;; Files menu

;;;###autoload (autoload 'tlon-files-menu "tlon-dispatch" nil t)
(transient-define-prefix tlon-files-menu ()
  "Files menu."
  :info-manual "(tlon) Files"
  [["Find"
    ("c" "in current repo"                 tlon-find-file-in-repo)
    ("o" "in another repo"                 tlon-find-file-in-repo-menu)
    ("a" "across repos"                    tlon-open-file-across-repos)]
   ["Open counterpart"
    ("f" "current window"                  tlon-open-counterpart-dwim)
    ("H-f" "other window"                  tlon-open-counterpart-in-other-window-dwim)]
   ["URL<>File"
    ("u" "Open URL of file"                tlon-browse-file)
    ("H-u" "Open file of URL"              tlon-find-file-of-url)]
   ["Issue"
    ("i" "file"                            tlon-open-forge-file)
    ("H-i" "counterpart"                   tlon-open-forge-counterpart)]
   ["Version control"
    ("l" "log"                             magit-log-buffer-file)
    ("d" "diffs since last user change"    tlon-log-buffer-latest-user-commit)
    ("e" "ediff with last user change"     tlon-log-buffer-latest-user-commit-ediff)]])

(provide 'tlon-dispatch)
;;; tlon-dispatch.el ends here
