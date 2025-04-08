;;; tlon-search.el --- Miscellaneous search commands -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/
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

;; Miscellaneous search commands.

;;; Code:

(require 'tlon)
(require 'transient)

;;;; Functions

(declare-function magit-log-all "magit-log")
;;;###autoload
(defun tlon-search-commits (search-string &optional repo)
  "Search for SEARCH-STRING in REPO's commit history.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let ((default-directory (or repo default-directory)))
    (magit-log-all (list "--grep" search-string))))

;;;###autoload
(defun tlon-search-commit-diffs (search-string &optional repo)
  "Search for SEARCH-STRING in REPO's commit diff history.
If REPO is nil, use the current repo."
  (interactive "sSearch commit diffs : ")
  (let ((default-directory (or repo default-directory)))
    (magit-log-all `("-S" ,search-string))))

(declare-function consult-ripgrep "consult")
;;;###autoload
(defun tlon-search-files (search-string &optional repo)
  "Search for SEARCH-STRING in REPO files.
If REPO is nil, use the current repo."
  (interactive "sSearch string: ")
  (let ((repo (or repo (tlon-get-repo nil 'include-all))))
    (consult-ripgrep repo search-string)))

(autoload 'goldendict-ng-search-string "goldendict-ng")
;;;###autoload
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

;;;;; Menu

;;;###autoload (autoload 'tlon-search-menu "tlon-dispatch" nil t)
(transient-define-prefix tlon-search-menu ()
  "Search menu."
  ["Search"
   ("c" "commits"                      tlon-search-commits)
   ("d" "commit-diffs"                 tlon-search-commit-diffs)
   ("f" "files"                        tlon-search-files)
   ("i" "issues (current repo)"        forge-search)
   ("I" "issues (all repos)"           tlon-forge-search)
   ("t" "translation"                  tlon-search-for-translation)])

(provide 'tlon-search)
;;; tlon-search.el ends here

