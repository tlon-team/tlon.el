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

;;;;; Browse repo in Magit/Dired

(defmacro tlon-generate-browse-command (name dir backend)
  "Generate a command for browsing repo NAME in BACKEND.
DIR is the directory where the repo is stored. BACKEND is either
`magit' or `dired'."
  (let ((fn-name (intern (format "tlon-%s-browse-%s" backend name)))
        (docstring (format "Browse the %s repository in %s." name
                           (capitalize (symbol-name backend)))))
    `(defun ,fn-name ()
       ,docstring
       (interactive)
       ,(pcase backend
          ('magit `(magit-status ,dir))
          ('dired `(dired ,dir))))))

(dolist (repo tlon-repos)
  (when-let ((abbrev (plist-get repo :abbrev))
	     (dir (plist-get repo :dir)))
    (eval `(tlon-generate-browse-command ,abbrev ,dir magit))
    (eval `(tlon-generate-browse-command ,abbrev ,dir dired))))

(defconst tlon-repo-menu-entries
  '(["Languages"
     ("l d" "de")
     ("l s" "es")
     ("l r" "fr")
     ("l t" "it")]
    ["General"
     ("g c" "babel-core")
     ("g f" "babel-refs")
     ""
     "Uqbar"
     ("q q" "uqbar")
     ("q d" "uqbar-audio-content")
     ("q p" "uqbar-audio-api")
     ""
     ("q a" "uqbar-ar")
     ("q n" "uqbar-en")
     ("q s" "uqbar-es")
     ("q r" "uqbar-fr")
     ("q t" "uqbar-it")
     ("q j" "uqbar-ja")
     ("q k" "uqbar-ko")
     ("q b" "uqbar-sr")
     ("q u" "uqbar-tr")
     ("q z" "uqbar-zh")]
    ["Projects"
     ("8 p" "80k podcast")
     ("8 a" "ai-in-context")
     ("7" "ai-2027")
     ("a a" "aea")
     ("a c" "aea-content")
     ("o" "altruismoeficaz.org")
     ("s s" "bisagra")
     ("s c" "bisagra-content")
     ("b" "boletin")
     ("i" "ea.international")
     ("n" "ea.news")
     ("r" "rational-animations")
     ("t t" "tlon.team")
     ("t c" "tlon.team-content")
     ("u n" "utilitarianism-en")
     ("u s" "utilitarianism-es")]
    ["Meetings"
     ("m l p" "Leo-Pablo")
     ("m f p" "Fede-Pablo")
     ("m f l" "Fede-Leo")
     ("m g" "group")
     ""
     "Clock"
     ("c l" "Leo")
     ("c f" "Fede")
     ("c p" "Pablo")
     ""
     "Emacs"
     ("e" "tlon.el")
     ""
     "Docs"
     ("d" "tlon-docs")
     ""
     "Web"
     ("w" "web-server")
     ""
     "Sandbox"
     ("H-s" "sandbox")])
  "Shared repo menu entries for Magit and Dired browse menus.
Each entry is (KEY LABEL); separators and group headers are strings.")

(defun tlon-dispatch--build-repo-menu-groups (backend &optional extra-entries)
  "Build transient menu groups for BACKEND from `tlon-repo-menu-entries'.
BACKEND is a string like \"magit\" or \"dired\". EXTRA-ENTRIES is an
alist mapping a KEY string to a list of literal entries to insert after
the entry with that key."
  (let ((extra-entries (or extra-entries '())))
    (apply #'vector
           (cl-loop for group in tlon-repo-menu-entries
                    collect
                    (let* ((items (cdr (append group nil)))
                           (header (car (append group nil))))
                      (apply #'vector header
                             (cl-loop for item in items
                                      if (stringp item) collect item
                                      else collect
                                      (let* ((key (nth 0 item))
                                             (label (nth 1 item))
                                             (abbrev (replace-regexp-in-string "[.]" "-" label))
                                             (cmd (intern (format "tlon-%s-browse-%s" backend abbrev))))
                                        (list key label cmd))
                                      and append (alist-get (nth 0 item) extra-entries nil nil #'equal))))))))

;;;###autoload (autoload 'tlon-magit-repo-menu "tlon-dispatch" nil t)
(eval
 `(transient-define-prefix tlon-magit-repo-menu ()
    "Browse a Tlön repo in Magit."
    ,(tlon-dispatch--build-repo-menu-groups
      "magit"
      '(("q q" . (("q Q" "uqbar: pull all" tlon-uqbar-pull-all)))))))

;;;###autoload (autoload 'tlon-dired-repo-menu "tlon-dispatch" nil t)
(eval
 `(transient-define-prefix tlon-dired-repo-menu ()
    "Browse a Tlön repo in Dired."
    ,(tlon-dispatch--build-repo-menu-groups "dired")))

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
  (when repo
    (eval `(tlon-generate-open-file-in-repo-commands ,repo))))

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
