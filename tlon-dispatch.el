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
    ("b" "bib"                            tlon-bib-menu)
    ("H-b" "dub"                          tlon-dub-menu)
    ("c" "clock"                          tlon-clock-menu)
    ("H-c" "cleanup"                      tlon-cleanup-menu)
    ("H-d" "db"                           tlon-db-menu)
    ("e" "forg"                           tlon-forg-menu)
    ("f" "files"                          tlon-files-menu)
    ("H-f" "fix"                          tlon-fix-menu)
    ("g" "glossary"                       tlon-glossary-menu)
    ("i" "images"                         tlon-images-menu)
    ("H-i" "dict"                         tlon-dict-menu)
    ("j" "jobs"                           tlon-jobs-menu)
    ("k" "markdown"                       tlon-md-menu)
    ("l" "DeepL"                          tlon-deepl-menu)
    ("H-l" "local"                        tlon-local-menu)]
   [""""
    ("H-m" "meet"                         tlon-meet-menu)
    ("n" "newsletter"                     tlon-newsletter-menu)
    ("o" "count"                          tlon-count-menu)
    ("p" "paragraphs"                     tlon-paragraphs-menu)
    ("H-p" "api"                          tlon-api-menu)
    ("r" "repos"                          tlon-repos-menu)
    ("H-r" "color"                        tlon-color-menu)
    ("s" "search"                         tlon-search-menu)
    ("H-s" "split"                        tlon-split-menu)
    ("t" "translate"                      tlon-translate-menu)
    ("H-t" "contacts"                     tlon-contacts-menu)
    ("u" "counterpart"                    tlon-counterpart-menu)
    ("v" "url"                            tlon-url-menu)
    ("y" "yaml"                           tlon-yaml-menu)
    ("H-y" "youtube"                      tlon-youtube-menu)
    ("z" "tts"                            tlon-tts-menu)]
   [""
    "Browse repo"
    ("d" "in Dired"                       tlon-dired-repo-menu)
    ("m" "in Magit"                       tlon-magit-repo-menu)
    """"""
    "GitHub"
    ("," "dashboard"                      tlon-browse-dashboard)
    ("." "notifications"                  forge-list-notifications)]
   [""
    "Meta"
    ("?" "get help"                       tlon-ai-ask-for-help)
    ("!" "report bug"                     tlon-report-bug)
    ""
    ("-D" "Debug"                         tlon-menu-infix-toggle-debug)
    ""
    "Package (tlon.el)"
    ("H-u" "update & reload"              tlon-update-package-and-reload)
    ("H-o" "open"                         tlon-open-package-files)
    ("H-h" "copy info"                    tlon-copy-package-info)
    """"
    (:info* (lambda () (concat "Package version: " (propertize tlon-version 'face 'bold))))
    (:info* (lambda () (format "Latest commit: %s" (propertize (tlon-get-latest-commit tlon-package-dir) 'face 'bold))))]])

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
  [["Languages"
    ("l d" "de"                           tlon-magit-browse-de)
    ("l s" "es"                           tlon-magit-browse-es)
    ("l r" "fr"                           tlon-magit-browse-fr)
    ("l t" "it"                           tlon-magit-browse-it)]
   ["General"
    ("g c" "babel-core"                   tlon-magit-browse-babel-core)
    ("g f" "babel-refs"                   tlon-magit-browse-babel-refs)
    ""
    "Uqbar"
    ("q q" "uqbar"                        tlon-magit-browse-uqbar)
    ("q d" "uqbar-audio-content"          tlon-magit-browse-uqbar-audio-content)
    ("q p" "uqbar-audio-api"              tlon-magit-browse-uqbar-audio-api)
    ""
    ("q a" "uqbar-ar"                     tlon-magit-browse-uqbar-ar)
    ("q n" "uqbar-en"                     tlon-magit-browse-uqbar-en)
    ("q s" "uqbar-es"                     tlon-magit-browse-uqbar-es)
    ("q r" "uqbar-fr"                     tlon-magit-browse-uqbar-fr)
    ("q t" "uqbar-it"                     tlon-magit-browse-uqbar-it)
    ("q j" "uqbar-ja"                     tlon-magit-browse-uqbar-ja)
    ("q k" "uqbar-ko"                     tlon-magit-browse-uqbar-ko)
    ("q u" "uqbar-tr"                     tlon-magit-browse-uqbar-tr)]
   ["Projects"
    ("8 p" "80k podcast"                    tlon-magit-browse-80k-podcast)
    ;; ("8 w" "80k website"                    tlon-magit-browse-80k-website)
    ("7" "ai-2027"                        tlon-magit-browse-ai-2027)
    ("a a" "aea"                          tlon-magit-browse-aea)
    ("a c" "aea-content"                  tlon-magit-browse-aea-content)
    ("o" "altruismoeficaz.org"            tlon-magit-browse-altruismoeficaz-org)
    ("s s" "bisagra"                      tlon-magit-browse-bisagra)
    ("s c" "bisagra-content"              tlon-magit-browse-bisagra-content)
    ("b" "boletin"                        tlon-magit-browse-boletin)
    ("i" "ea.international"               tlon-magit-browse-ea-international)
    ("n" "ea.news"                        tlon-magit-browse-ea-news)
    ("r" "rational-animations"            tlon-magit-browse-rational-animations)
    ("t t" "tlon.team"                    tlon-magit-browse-tlon-team)
    ("t c" "tlon.team-content"            tlon-magit-browse-tlon-team-content)
    ("u n" "utilitarianism-en"            tlon-magit-browse-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-magit-browse-utilitarianism-es)]
   ["Meetings"
    ("m l p" "Leo-Pablo"                  tlon-magit-browse-meetings-leo-pablo)
    ("m f p" "Fede-Pablo"                 tlon-magit-browse-meetings-fede-pablo)
    ("m f l" "Fede-Leo"                   tlon-magit-browse-meetings-fede-leo)
    ("m g" "group"                        tlon-magit-browse-meetings-group)
    ""
    "Clock"
    ("c l" "Leo"                          tlon-magit-browse-clock-leo)
    ("c f" "Fede"                         tlon-magit-browse-clock-fede)
    ("c p" "Pablo"                        tlon-magit-browse-clock-pablo)
    ""
    "Emacs"
    ("e" "tlon.el"                        tlon-magit-browse-tlon-el)
    ""
    "Docs"
    ("d" "tlon-docs"                    tlon-magit-browse-docs)
    ""
    "Sandbox"
    ("H-s" "sandbox"                        tlon-magit-browse-sandbox)]])

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
  [["Languages"
    ("l d" "de"                           tlon-dired-browse-de)
    ("l s" "es"                           tlon-dired-browse-es)
    ("l r" "fr"                           tlon-dired-browse-fr)
    ("l t" "it"                           tlon-dired-browse-it)]
   ["General"
    ("g c" "babel-core"                   tlon-dired-browse-babel-core)
    ("g f" "babel-refs"                   tlon-dired-browse-babel-refs)
    ""
    "Uqbar"
    ("q q" "uqbar"                        tlon-dired-browse-uqbar)
    ("q d" "uqbar-audio-content"          tlon-dired-browse-uqbar-audio-content)
    ("q p" "uqbar-audio-api"              tlon-dired-browse-uqbar-audio-api)
    ""
    ("q a" "uqbar-ar"                     tlon-dired-browse-uqbar-ar)
    ("q n" "uqbar-en"                     tlon-dired-browse-uqbar-en)
    ("q s" "uqbar-es"                     tlon-dired-browse-uqbar-es)
    ("q r" "uqbar-fr"                     tlon-dired-browse-uqbar-fr)
    ("q t" "uqbar-it"                     tlon-dired-browse-uqbar-it)
    ("q j" "uqbar-ja"                     tlon-dired-browse-uqbar-ja)
    ("q k" "uqbar-ko"                     tlon-dired-browse-uqbar-ko)
    ("q u" "uqbar-tr"                     tlon-dired-browse-uqbar-tr)]
   ["Projects"
    ("8 p" "80k podcast"                    tlon-dired-browse-80k-podcast)
    ;; ("8 w" "80k website"                    tlon-dired-browse-80k-website)
    ("7" "ai-2027"                        tlon-dired-browse-ai-2027)
    ("a a" "aea"                          tlon-dired-browse-aea)
    ("a c" "aea-content"                  tlon-dired-browse-aea-content)
    ("o" "altruismoeficaz.org"            tlon-dired-browse-altruismoeficaz-org)
    ("s s" "bisagra"                      tlon-dired-browse-bisagra)
    ("s c" "bisagra-content"              tlon-dired-browse-bisagra-content)
    ("b" "boletin"                        tlon-dired-browse-boletin)
    ("i" "ea.international"               tlon-dired-browse-ea-international)
    ("n" "ea.news"                        tlon-dired-browse-ea-news)
    ("r" "rational-animations"            tlon-dired-browse-rational-animations)
    ("t t" "tlon.team"                    tlon-dired-browse-tlon-team)
    ("t c" "tlon.team-content"            tlon-dired-browse-tlon-team-content)
    ("u n" "utilitarianism-en"            tlon-dired-browse-utilitarianism-en)
    ("u s" "utilitarianism-es"            tlon-dired-browse-utilitarianism-es)]
   ["Meetings"
    ("m l p" "Leo-Pablo"                  tlon-dired-browse-meetings-leo-pablo)
    ("m f p" "Fede-Pablo"                 tlon-dired-browse-meetings-fede-pablo)
    ("m f l" "Fede-Leo"                   tlon-dired-browse-meetings-fede-leo)
    ("m g" "group"                        tlon-dired-browse-meetings-group)
    ""
    "Clock"
    ("c l" "Leo"                          tlon-dired-browse-clock-leo)
    ("c f" "Fede"                         tlon-dired-browse-clock-fede)
    ("c p" "Pablo"                        tlon-dired-browse-clock-pablo)
    ""
    "Emacs"
    ("e" "tlon.el"                         tlon-dired-browse-tlon-el)
    ""
    "Docs"
    ("d" "tlon-docs"                      tlon-dired-browse-docs)
    ""
    "Misc"
    ("H-s" "sandbox"                        tlon-dired-browse-sandbox)]])

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

;;;;; Files menu

;;;###autoload (autoload 'tlon-files-menu "tlon-dispatch" nil t)
(transient-define-prefix tlon-files-menu ()
  "Files menu."
  :info-manual "(tlon) Files"
  [["Find"
    ("c" "in current repo"                 tlon-find-file-in-repo)
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
