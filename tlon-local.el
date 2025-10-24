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
(require 'transient)
(require 'url-parse)

;;;; Variables


;;;; Functions

;;;###autoload
(defun tlon-local-run-uqbar (&optional lang)
  "Run the local web server and the Uqbar environment for LANG.
If LANG is nil, prompt the user for a language (ISO 639-1 code).
Steps:
- docker: ensure Docker Desktop is running (start and wait if needed)
- web-server: ./up-dev.sh
- uqbar: ./launch.py start LANG (or stop+start if already running and confirmed)
At the end, open the local site in the default browser."
  (interactive)
  (let* ((lang (or lang (tlon-select-language 'code t "Language: " t)))
         (ws-dir (tlon-repo-lookup :dir :name "web-server"))
         (uq-dir (tlon-repo-lookup :dir :name "uqbar")))
    (unless (and ws-dir (file-directory-p ws-dir))
      (user-error "`web-server' repo directory not found"))
    (unless (and uq-dir (file-directory-p uq-dir))
      (user-error "`uqbar' repo directory not found"))
    (tlon-local--ensure-docker-running)
    (let* ((buffer-name (format "*tlon: uqbar start %s*" lang))
           (local-url (tlon-local--uqbar-local-url lang))
           (uq-running (and local-url (tlon-local--uqbar-running-p local-url)))
           (run-uq (if uq-running
                       (y-or-n-p (format "Uqbar environment for '%s' appears to be running%s. Relaunch it? "
                                         lang
                                         (if local-url (format " at %s" local-url) "")))
                     t))
           (commands '()))
      (push (format "cd %s && ./up-dev.sh" (shell-quote-argument ws-dir)) commands)
      (when run-uq
        (when uq-running
          (push (format "cd %s && ./launch.py stop %s"
                        (shell-quote-argument uq-dir)
                        (shell-quote-argument lang))
                commands))
        (push (format "cd %s && ./launch.py start %s"
                      (shell-quote-argument uq-dir)
                      (shell-quote-argument lang))
              commands))
      (setq commands (nreverse commands))
      (cond
       ;; Nothing to run: just open the site if we know the URL
       ((null commands)
        (if local-url
            (browse-url local-url)
          (message "Nothing to run and local URL unknown")))
       (t
        (let* ((cmd (mapconcat #'identity commands " && "))
               (_win (async-shell-command cmd buffer-name))
               (buf (get-buffer buffer-name)))
          (when-let ((proc (and buf (get-buffer-process buf))))
            (set-process-sentinel
             proc
             (lambda (_p event)
               (when (and (string-prefix-p "finished" event)
                          local-url)
                 (browse-url local-url)))))))))))

;;;; Language-specific commands

;;;###autoload
(defun tlon-local-run-uqbar-ar ()
  "Run the local web server and the Uqbar environment for Arabic."
  (interactive)
  (tlon-local-run-uqbar "ar"))

;;;###autoload
(defun tlon-local-run-uqbar-en ()
  "Run the local web server and the Uqbar environment for English."
  (interactive)
  (tlon-local-run-uqbar "en"))

;;;###autoload
(defun tlon-local-run-uqbar-es ()
  "Run the local web server and the Uqbar environment for Spanish."
  (interactive)
  (tlon-local-run-uqbar "es"))

;;;###autoload
(defun tlon-local-run-uqbar-fr ()
  "Run the local web server and the Uqbar environment for French."
  (interactive)
  (tlon-local-run-uqbar "fr"))

;;;###autoload
(defun tlon-local-run-uqbar-it ()
  "Run the local web server and the Uqbar environment for Italian."
  (interactive)
  (tlon-local-run-uqbar "it"))

;;;###autoload
(defun tlon-local-run-uqbar-ja ()
  "Run the local web server and the Uqbar environment for Japanese."
  (interactive)
  (tlon-local-run-uqbar "ja"))

;;;###autoload
(defun tlon-local-run-uqbar-ko ()
  "Run the local web server and the Uqbar environment for Korean."
  (interactive)
  (tlon-local-run-uqbar "ko"))

;;;###autoload
(defun tlon-local-run-uqbar-tr ()
  "Run the local web server and the Uqbar environment for Turkish."
  (interactive)
  (tlon-local-run-uqbar "tr"))

;;;; Helpers

;;;;; Docker

(defun tlon-local--docker-running-p ()
  "Return non-nil if the Docker daemon responds to `docker info'."
  (and (executable-find "docker")
       (eq 0 (call-process "docker" nil nil nil "info"))))

(defun tlon-local--ensure-docker-running ()
  "Ensure the Docker daemon is running; start it on macOS if needed.
On macOS, try to start Docker Desktop with `open -ga Docker' and wait until
`docker info' succeeds, up to a timeout. On other systems, signal an error."
  (unless (executable-find "docker")
    (user-error "Docker CLI not found"))
  (unless (tlon-local--docker-running-p)
    (cond
     ((eq system-type 'darwin)
      (message "Starting Docker Desktop…")
      (call-process "open" nil nil nil "-ga" "Docker")
      (let ((timeout 90) (elapsed 0))
        (while (and (< elapsed timeout) (not (tlon-local--docker-running-p)))
          (sit-for 1)
          (setq elapsed (1+ elapsed)))
        (unless (tlon-local--docker-running-p)
          (user-error "Docker did not start within %s seconds" timeout))))
     (t
      (user-error "Docker daemon is not running")))))

;;;;; uqbar

(defun tlon-local--uqbar-running-p (url)
  "Return non-nil if the local Uqbar site at URL is reachable (TCP)."
  (when url
    (let* ((parsed (url-generic-parse-url url))
           (host (url-host parsed)))
      (or (tlon-local--tcp-open-p host 443)
          (tlon-local--tcp-open-p host 80)))))

(defun tlon-local--tcp-open-p (host port)
  "Return non-nil if a TCP connection to HOST:PORT succeeds."
  (let (proc ok)
    (setq ok
          (condition-case _err
              (progn
                (setq proc (open-network-stream "tlon-local-probe" nil host port))
                (when proc (delete-process proc))
                t)
            (error nil)))
    ok))

(defun tlon-local--uqbar-local-url (lang)
  "Compute the local development URL for Uqbar in language LANG.
Returns a string like \"https://local-dev.example.org\" or nil if unknown."
  (let* ((prod (tlon-repo-lookup :url :language lang :subproject "uqbar")))
    (when prod
      (let* ((prod-url (if (string-match-p "\\`https?://" prod) prod (concat "https://" prod)))
             (parsed (url-generic-parse-url prod-url))
             (host (url-host parsed)))
        (when host
          (format "https://local-dev.%s" host))))))

;;;; Menu

;;;###autoload (autoload 'tlon-local-menu "tlon-local" nil t)
(transient-define-prefix tlon-local-menu ()
  "`tlon-local' menu."
  [["Run local environment"
    ("q a" "arabic" tlon-local-run-uqbar-ar)
    ("q n" "english" tlon-local-run-uqbar-en)
    ("q s" "spanish" tlon-local-run-uqbar-es)
    ("q r" "french" tlon-local-run-uqbar-fr)
    ("q t" "italian" tlon-local-run-uqbar-it)
    ("q j" "japanese" tlon-local-run-uqbar-ja)
    ("q k" "korean" tlon-local-run-uqbar-ko)
    ("q u" "turkish" tlon-local-run-uqbar-tr)]])

(provide 'tlon-local)
;;; tlon-local.el ends here
