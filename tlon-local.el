;;; tlon-api.el --- Manage local environments -*- lexical-binding: t -*-

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

;; Manage local environments.

;;; Code:

(require 'tlon-core)

;;;; Variables


;;;; Functions

;;;###autoload
(defun tlon-local-run-uqbar (&optional lang)
  "Run the local web server and the Uqbar environment for LANG.
If LANG is nil, prompt the user for a language (ISO 639-1 code)
restricted to Babel project languages. This runs:
- web-server: ./up-dev.sh
- uqbar: ./launch.py start LANG"
  (interactive)
  (let* ((lang (or lang (tlon-select-language 'code t "Language: " t)))
         (ws-dir (tlon-repo-lookup :dir :name "web-server"))
         (uq-dir (tlon-repo-lookup :dir :name "uqbar")))
    (unless (and ws-dir (file-directory-p ws-dir))
      (user-error "`web-server' repo directory not found"))
    (unless (and uq-dir (file-directory-p uq-dir))
      (user-error "`uqbar' repo directory not found"))
    (let* ((buffer (format "*tlon: uqbar start %s*" lang))
           (cmd (mapconcat
                 #'identity
                 (list
                  (format "cd %s && ./up-dev.sh" (shell-quote-argument ws-dir))
                  (format "cd %s && ./launch.py start %s"
                          (shell-quote-argument uq-dir)
                          (shell-quote-argument lang)))
                 " && ")))
      (async-shell-command cmd buffer))))

(provide 'tlon-local)
;;; tlon-local.el ends here

