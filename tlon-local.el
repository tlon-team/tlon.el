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
(require 'subr-x)
(require 'json)

;;;; Variables

(defgroup tlon-local nil
  "Manage local environments."
  :group 'tlon)

;;;; Variables (customization)

(defcustom tlon-local-grafana-base-url "http://localhost:3101"
  "Base URL for Grafana in local development."
  :type 'string
  :group 'tlon-local)

(defcustom tlon-local-grafana-loki-proxy "/api/datasources/proxy/1"
  "Path prefix for the Grafana Loki data source proxy.
This usually contains the data source numeric ID or UID."
  :type 'string
  :group 'tlon-local)

(defcustom tlon-local-logs-minutes 60
  "Default lookback window, in minutes, for querying logs."
  :type 'integer
  :group 'tlon-local)

(defcustom tlon-local-logs-limit 1000
  "Default maximum number of log entries to retrieve."
  :type 'integer
  :group 'tlon-local)

(defcustom tlon-local-logs-time-width 24
  "Column width for the timestamp in the logs buffer."
  :type 'integer
  :group 'tlon-local)

(defcustom tlon-local-logs-service-width 14
  "Column width for the service label in the logs buffer."
  :type 'integer
  :group 'tlon-local)

(defvar-local tlon-local--logs-ctx nil
  "Internal context data for `tlon-local-logs' buffers.")

;;;; Functions

;;;###autoload
(defun tlon-local-run-uqbar (&optional lang)
  "Run the local web server and the Uqbar environment for LANG.
If LANG is nil, prompt the user for a language (ISO 639-1 code).
Steps:
- docker: ensure Docker Desktop is running (start and wait if needed)
- web-server: ./up-dev.sh (skipped if already running)
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
           (ws-running (tlon-local--web-server-running-p))
           (commands '())
           (append-command (lambda (command)
                             (append commands
                                     (list (format command
                                                   (shell-quote-argument uq-dir)
                                                   (shell-quote-argument lang)))))))
      (when (not ws-running)
        (setq commands (append commands (list (format "cd %s && ./up-dev.sh" (shell-quote-argument ws-dir))))))
      (when run-uq
        (when uq-running
          (setq commands (funcall append-command "cd %s && ./launch.py stop %s")))
        (setq commands (funcall append-command "cd %s && ./launch.py start %s")))
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
	       "Browse the local site 30 seconds after the process finishes."
               (when (and (string-prefix-p "finished" event)
                          local-url)
                 (run-at-time 30 nil #'browse-url local-url)))))))))))

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

;;;; Logs

;;;###autoload
(defun tlon-local-logs (&optional lang)
  "Show recent Uqbar logs for LANG from the latest content build.
If LANG is nil, prompt for a language. Results are fetched through
Grafana's Loki proxy and filtered to show ERROR and CRITICAL entries."
  (interactive)
  (let* ((lang (or lang (tlon-select-language 'code t "Language: " t)))
         (uq-dir (tlon-repo-lookup :dir :name "uqbar"))
         (label (tlon-local--read-last-update-label uq-dir))
         (query (format "{content_build=\"%s\"} | json | level =~ \"(ERROR|CRITICAL)\""
                        label))
         (end (tlon-local--rfc3339 (current-time)))
         (start (tlon-local--rfc3339
                 (time-subtract (current-time)
                                (seconds-to-time
                                 (* 60 tlon-local-logs-minutes)))))
         (base (string-remove-suffix "/" tlon-local-grafana-base-url))
         (proxy (string-remove-suffix "/" tlon-local-grafana-loki-proxy))
         (url (format "%s%s/loki/api/v1/query_range" base proxy))
         (params `(("query" . ,query)
                   ("limit" . ,(number-to-string tlon-local-logs-limit))
                   ("start" . ,start)
                   ("end" . ,end)
                   ("direction" . "backward"))))
    (tlon-local--http-json
     url params
     (lambda (json)
       (tlon-local--render-logs-buffer json lang label 'errors)))))

;;;###autoload
(defun tlon-local-logs-warnings (&optional lang)
  "Show recent Uqbar WARNING logs for LANG from the latest content build.
If LANG is nil, prompt for a language."
  (interactive)
  (let* ((lang (or lang (tlon-select-language 'code t "Language: " t)))
         (uq-dir (tlon-repo-lookup :dir :name "uqbar"))
         (label (tlon-local--read-last-update-label uq-dir))
         (query (format "{content_build=\"%s\"} | json | level = \"WARNING\"" label))
         (end (tlon-local--rfc3339 (current-time)))
         (start (tlon-local--rfc3339
                 (time-subtract (current-time)
                                (seconds-to-time
                                 (* 60 tlon-local-logs-minutes)))))
         (base (string-remove-suffix "/" tlon-local-grafana-base-url))
         (proxy (string-remove-suffix "/" tlon-local-grafana-loki-proxy))
         (url (format "%s%s/loki/api/v1/query_range" base proxy))
         (params `(("query" . ,query)
                   ("limit" . ,(number-to-string tlon-local-logs-limit))
                   ("start" . ,start)
                   ("end" . ,end)
                   ("direction" . "backward"))))
    (tlon-local--http-json
     url params
     (lambda (json)
       (tlon-local--render-logs-buffer json lang label 'warnings)))))

(defun tlon-local--render-logs-buffer (json lang label kind)
  "Render Loki JSON into a buffer for LANG and build LABEL for KIND."
  (let* ((data (alist-get 'data json))
         (result (and data (alist-get 'result data)))
         (buf (get-buffer-create (format "*tlon: logs %s*" lang))))
    (unless (and data result)
      (user-error "Unexpected Loki response"))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (tlon-local-logs-mode)
        (tlon--setup-visit-file-at-point-buffer)
        (setq-local tlon-local--logs-ctx (list :lang lang :label label :kind kind))
        (insert (format
                 "Uqbar %s logs for %s – content_build=%s (last %dm, limit %d)\n\n"
                 (if (eq kind 'warnings) "WARNING" "ERROR/CRITICAL")
                 lang label tlon-local-logs-minutes tlon-local-logs-limit))
        (let* ((timew tlon-local-logs-time-width)
               (svcw tlon-local-logs-service-width)
               (fmt (format "%%-%ds  %%-%ds  %%s" timew svcw))
               (hdr (format fmt "timestamp" "service" "message")))
          (insert hdr "\n" (make-string (length hdr) ?-) "\n"))
        (dolist (stream result)
          (let* ((labels (alist-get 'stream stream))
                 (values (alist-get 'values stream))
                 (svc (tlon-local--labels-prefix labels)))
            (dolist (v values)
              (let ((ts (tlon-local--ns-to-timestr (nth 0 v)))
                    (line (nth 1 v)))
                (tlon-local--insert-log-row ts svc line lang))))))
      (goto-char (point-min))
      (display-buffer buf))))

(defun tlon-local--insert-log-row (ts svc line lang)
  "Insert a single log row with columns TS, SVC and LINE for LANG.
Continuation lines in LINE are indented under the message column."
  (let* ((timew tlon-local-logs-time-width)
         (svcw tlon-local-logs-service-width)
         (fmt (format "%%-%ds  %%-%ds  %%s\n" timew svcw))
         (indent (make-string (+ timew 2 svcw 2) ?\s))
         (msg (tlon-local--expand-article-ids line lang))
         (parts (and msg (split-string msg "\n"))))
    (insert (format fmt ts (or svc "") (or (car parts) "")))
    (dolist (cont (cdr parts))
      (insert indent cont "\n"))))

(defun tlon-local--expand-article-ids (line lang)
  "Expand article_id slugs in LINE into absolute paths for LANG.
The replacement text includes a `: position 1' suffix to work with
`tlon-visit-file-at-point'."
  (let ((repo (tlon-repo-lookup :dir :name (format "uqbar-%s" lang))))
    (if (not repo)
        line
      (let ((start 0)
            (pattern "\\(article_id=\\)['\"]?\\([^'\" \n]+\\)['\"]?"))
        (save-match-data
          (while (string-match pattern line start)
            (let* ((prefix (match-string 1 line))
                   (slug (match-string 2 line))
                   (path (tlon-local--article-id-to-path slug lang))
                   (replacement (format "%s'%s: position 1'" prefix path)))
              (setq line (replace-match replacement t t line))
              (setq start (+ (match-beginning 0) (length replacement)))))
          line)))))

(defun tlon-local--article-id-to-path (slug lang)
  "Return the absolute markdown file path for article SLUG in LANG."
  (let* ((repo (tlon-repo-lookup :dir :name (format "uqbar-%s" lang)))
         (articles-dir (tlon-get-bare-dir-translation lang "en" "articles"))
         (file (file-name-with-extension slug "md")))
    (file-name-concat repo articles-dir file)))

(define-derived-mode tlon-local-logs-mode special-mode "TLon-Logs"
  "Major mode to view Uqbar logs retrieved via Grafana's Loki proxy."
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (use-local-map (copy-keymap special-mode-map))
  (local-set-key (kbd "g") #'tlon-local-logs-refresh)
  (local-set-key (kbd "o") #'tlon-local-logs-open-in-grafana)
  (local-set-key (kbd "RET") #'tlon-visit-file-at-point))

(defun tlon-local-logs-refresh ()
  "Refresh the logs in the current `tlon-local-logs-mode' buffer."
  (interactive)
  (let* ((ctx tlon-local--logs-ctx)
         (lang (plist-get ctx :lang))
         (kind (plist-get ctx :kind)))
    (if (eq kind 'warnings)
        (tlon-local-logs-warnings lang)
      (tlon-local-logs lang))))

(defun tlon-local-logs-open-in-grafana ()
  "Open the Grafana dashboard for the build in the current buffer."
  (interactive)
  (let* ((ctx tlon-local--logs-ctx)
         (label (plist-get ctx :label))
         (base (string-remove-suffix "/" tlon-local-grafana-base-url))
         (url (format "%s/d/%s/%s?orgId=1&from=now-30d&to=now"
                      base label label)))
    (browse-url url)))

(defun tlon-local--http-json (url params callback)
  "GET URL with URL-encoded PARAMS and call CALLBACK with parsed JSON."
  (let* ((qs (mapconcat
              (lambda (p)
                (concat (url-hexify-string (car p))
                        "="
                        (url-hexify-string (cdr p))))
              params "&"))
         (full (concat url "?" qs)))
    (url-retrieve
     full
     (lambda (status)
       (let ((err (plist-get status :error)))
         (when err
           (kill-buffer)
           (user-error "HTTP error: %s" err)))
       (goto-char (point-min))
       (re-search-forward "^$" nil 'move)
       (let ((json (json-parse-buffer :object-type 'alist :array-type 'list)))
         (kill-buffer)
         (funcall callback json))))))

(defun tlon-local--read-last-update-label (uq-dir)
  "Read last-update-label from UQ-DIR and return its trimmed contents."
  (let ((file (expand-file-name "build-files/last-update-label" uq-dir)))
    (unless (file-readable-p file)
      (user-error "Missing %s" file))
    (string-trim
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

(defun tlon-local--rfc3339 (time)
  "Return TIME formatted as RFC3339 in UTC."
  (let ((system-time-locale "C"))
    (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t)))

(defun tlon-local--ns-to-timestr (ns-str)
  "Convert nanoseconds string NS-STR to an ISO-like UTC time string."
  (let* ((ns (string-to-number ns-str))
         (sec (floor (/ ns 1e9)))
         (ms (floor (mod (/ ns 1e6) 1000)))
         (t0 (seconds-to-time sec)))
    (format "%s.%03dZ"
            (format-time-string "%Y-%m-%dT%H:%M:%S" t0 t) ms)))

(defun tlon-local--labels-prefix (labels)
  "Return a short label prefix from the stream LABELS."
  (let ((svc (alist-get 'compose_service labels))
        (app (alist-get 'application labels)))
    (cond
     (svc (format "%s" svc))
     (app (format "%s" app))
     (t "-"))))

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

;;;;; Web server

(defun tlon-local--web-server-running-p ()
  "Return non-nil if the web server Traefik container is running."
  (let* ((cmd "docker ps --filter \"name=web-server-traefik-1\" --format '{{.Names}}' | grep -q \"^web-server-traefik-1$\" && echo true || echo false")
         (out (string-trim (shell-command-to-string cmd))))
    (string= out "true")))

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
    ("q u" "turkish" tlon-local-run-uqbar-tr)]
   ["Logs"
    ("l r" "range (errors)" tlon-local-logs)
    ("l w" "range (warnings)" tlon-local-logs-warnings)]])

(provide 'tlon-local)
;;; tlon-local.el ends here
