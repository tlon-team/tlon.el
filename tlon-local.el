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

(require 'simple-extras)
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

(defcustom tlon-local-logs-minutes (* 30 24 60)
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

(defcustom tlon-local-logs-source-width 40
  "Column width for the source_filename in the logs buffer."
  :type 'integer
  :group 'tlon-local)

(defcustom tlon-local-rebuild-content-database nil
  "When non-nil, append `--rebuild-content-database' to Uqbar start."
  :type 'boolean
  :group 'tlon-local)

(defcustom tlon-local-content-branch-production nil
  "When non-nil, append `--content-branch=production' to Uqbar start."
  :type 'boolean
  :group 'tlon-local)

(defcustom tlon-local-no-include-testing nil
  "When non-nil, append `--no-include-testing' Uqbar start."
  :type 'boolean
  :group 'tlon-local)

(defcustom tlon-local-enforce-single-env t
  "When non-nil, starting a local environment stops all other running ones."
  :type 'boolean
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
           (append-command (lambda (commands command &optional language)
                             (append commands
                                     (list (format command
                                                   (shell-quote-argument uq-dir)
                                                   (shell-quote-argument (or language lang))))))))
      (when (not ws-running)
        (setq commands (append commands (list (format "cd %s && ./up-dev.sh" (shell-quote-argument ws-dir))))))
      (when run-uq
        (when uq-running
          (setq commands (funcall append-command commands "cd %s && ./launch.py stop %s")))
        (when tlon-local-enforce-single-env
          (setq commands (tlon-local--append-stop-other-envs commands append-command lang)))
        (let* ((opts (string-join
		      (delq nil
			    (list
			     (and tlon-local-rebuild-content-database "--rebuild-content-database")
			     (and tlon-local-content-branch-production "--content-branch=production")
			     (and tlon-local-no-include-testing "--no-include-testing")))
		      " ")))
          (if (string-empty-p opts)
	      (setq commands (funcall append-command commands "cd %s && ./launch.py start %s"))
	    (let* ((uq (shell-quote-argument uq-dir))
		   (la (shell-quote-argument lang))
		   (cmd (format "cd %s && ./launch.py start %s %s" uq la opts)))
	      (setq commands (append commands (list cmd)))))))
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
	       "Browse the local site one minute after the process finishes."
	       (when (and (string-prefix-p "finished" event)
                          local-url)
                 (run-at-time 60 nil #'browse-url local-url)))))))))))

(defun tlon-local--append-stop-other-envs (commands append-command lang)
  "Append stop commands for other running envs to COMMANDS using APPEND-COMMAND.
Skip env in LANG. Return the updated commands."
  (let* ((codes
          (delq nil
                (mapcar (lambda (name)
                          (tlon-lookup tlon-languages-properties :code :name name))
                        tlon-project-languages))))
    (dolist (l codes commands)
      (unless (string= l lang)
        (let ((url (tlon-local--uqbar-local-url l)))
          (when (and url (tlon-local--uqbar-running-p url))
            (setq commands (funcall append-command commands "cd %s && ./launch.py stop %s" l))))))))


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
If LANG is nil, prompt for a language."
  (interactive)
  (let ((lang (or lang (tlon-select-language 'code t "Language: " t))))
    (tlon-local--logs-all lang)))

;;;###autoload
(defun tlon-local-logs-warnings (&optional lang)
  "Deprecated. Use `tlon-local-logs' to show errors and warnings for LANG."
  (interactive)
  (tlon-local-logs lang))

(defun tlon-local--logs-time-range ()
  "Return cons cell (START . END) for the current logs time window in RFC3339."
  (let* ((end (tlon-local--rfc3339 (current-time)))
         (start (tlon-local--rfc3339
                 (time-subtract (current-time)
                                (seconds-to-time (* 60 tlon-local-logs-minutes))))))
    (cons start end)))

(defun tlon-local--loki-base-url ()
  "Return the full Loki query_range endpoint URL."
  (let* ((base (string-remove-suffix "/" tlon-local-grafana-base-url))
         (proxy (string-remove-suffix "/" tlon-local-grafana-loki-proxy)))
    (format "%s%s/loki/api/v1/query_range" base proxy)))

(defun tlon-local--loki-query-range (query limit direction start end callback)
  "Execute a Loki query_range with QUERY and invoke CALLBACK with parsed JSON.
QUERY is the LogQL query string to execute. LIMIT is the maximum number of
entries to return. DIRECTION is the sort order, either \"forward\" or
\"backward\". START is the start time for the query range. END is the end time
for the query range. CALLBACK is a function to invoke with the parsed JSON
response."
  (let* ((url (tlon-local--loki-base-url))
         (params `(("query" . ,query)
                   ("limit" . ,(number-to-string limit))
                   ("start" . ,start)
                   ("end" . ,end)
                   ("direction" . ,direction))))
    (tlon-local--http-json url params callback)))

(defun tlon-local--get-latest-build-label (lang callback)
  "Resolve the newest content_build label for LANG and pass it to CALLBACK."
  (let* ((range (tlon-local--logs-time-range))
         (start (car range))
         (end (cdr range))
         (selector (format "{content_build=~\"uqbar-%s-.*\"}" lang)))
    (tlon-local--loki-query-range
     selector 1 "backward" start end
     (lambda (json)
       (let* ((data (alist-get 'data json))
              (result (and data (alist-get 'result data))))
         (unless (and result (consp result))
           (user-error "No content_build label found for '%s' in the last %dm" lang tlon-local-logs-minutes))
         (let* ((stream (car result))
                (labels (alist-get 'stream stream))
                (label (alist-get 'content_build labels)))
           (unless label
             (user-error "Loki response missing content_build label"))
           (funcall callback label)))))))

(defun tlon-local--logs-all (lang)
  "Fetch and render ERROR/CRITICAL and WARNING logs for LANG in one buffer."
  (let ((lang (or lang (tlon-select-language 'code t "Language: " t))))
    (tlon-local--get-latest-build-label
     lang
     (lambda (label)
       (let* ((range (tlon-local--logs-time-range))
              (start (car range))
              (end (cdr range))
              (errors-filter "| json | level =~ \"(ERROR|CRITICAL)\"")
              (warnings-filter "| json | level = \"WARNING\"")
              (errors-query (format "{content_build=\"%s\"} %s" label errors-filter))
              (warnings-query (format "{content_build=\"%s\"} %s" label warnings-filter)))
         (tlon-local--loki-query-range
          errors-query tlon-local-logs-limit "backward" start end
          (lambda (errors-json)
            (tlon-local--loki-query-range
             warnings-query tlon-local-logs-limit "backward" start end
             (lambda (warnings-json)
               (tlon-local--render-logs-buffer-all
                errors-json warnings-json lang label))))))))))

(defun tlon-local--render-logs-buffer-all (errors-json warnings-json lang label)
  "Render Loki JSON for errors and warnings into a single buffer for LANG.
ERRORS-JSON is the JSON response containing error log entries. WARNINGS-JSON is
the JSON response containing warning log entries. LANG is the language
identifier for the logs. LABEL is the content_build label identifier."
  (let* ((buf (get-buffer-create (format "*tlon: logs %s*" lang)))
         (errors (alist-get 'result (alist-get 'data errors-json)))
         (warnings (alist-get 'result (alist-get 'data warnings-json))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (tlon-local-logs-mode)
        (tlon--setup-visit-file-at-point-buffer)
        (setq-local tlon-local--logs-ctx (list :lang lang :label label :kind 'all))
        (insert (format
                 "Uqbar logs for %s – content_build=%s (last %dm, limit %d)\n\n"
                 lang label tlon-local-logs-minutes tlon-local-logs-limit))
        (insert "ERRORS\n")
        (tlon-local--insert-logs-section errors lang)
        (insert "WARNINGS\n")
        (tlon-local--insert-logs-section warnings lang))
      (goto-char (point-min))
      (hl-line-mode)
      (display-buffer buf))))

(defun tlon-local--insert-logs-section (streams lang)
  "Insert a section table for STREAMS (Loki result list) for LANG."
  (let* ((srcw tlon-local-logs-source-width)
         (fmt (format "%%-%ds  %%s" srcw))
         (hdr (format fmt "source_filename" "message")))
    (insert hdr "\n" (make-string (length hdr) ?-) "\n")
    (dolist (stream streams)
      (let* ((labels (alist-get 'stream stream))
             (src-label (alist-get 'source_filename labels))
             (values (alist-get 'values stream)))
        (dolist (v values)
          (let ((line (nth 1 v)))
            (tlon-local--insert-log-row line src-label lang)))))
    (insert "\n")))

(defun tlon-local--logs (lang kind)
  "Fetch and render logs for LANG. KIND is `errors' or `warnings'."
  (let ((lang (or lang (tlon-select-language 'code t "Language: " t))))
    (tlon-local--get-latest-build-label
     lang
     (lambda (label)
       (let* ((range (tlon-local--logs-time-range))
              (start (car range))
              (end (cdr range))
              (filter (if (eq kind 'warnings)
                          "| json | level = \"WARNING\""
                        "| json | level =~ \"(ERROR|CRITICAL)\""))
              (query (format "{content_build=\"%s\"} %s" label filter)))
         (tlon-local--loki-query-range
          query tlon-local-logs-limit "backward" start end
          (lambda (json)
            (tlon-local--render-logs-buffer json lang label kind))))))))

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
        (let* ((srcw tlon-local-logs-source-width)
               (fmt (format "%%-%ds  %%s" srcw))
               (hdr (format fmt "source_filename" "message")))
          (insert hdr "\n" (make-string (length hdr) ?-) "\n"))
        (dolist (stream result)
          (let* ((labels (alist-get 'stream stream))
                 (src-label (alist-get 'source_filename labels))
                 (values (alist-get 'values stream)))
            (dolist (v values)
              (let ((line (nth 1 v)))
                (tlon-local--insert-log-row line src-label lang))))))
      (goto-char (point-min))
      (hl-line-mode)
      (display-buffer buf))))

(defun tlon-local--insert-log-row (line src-label lang)
  "Insert one row with SOURCE-LABEL and LINE for LANG.
Continuation lines in LINE are indented under the message column."
  (let* ((srcw tlon-local-logs-source-width)
         (fmt (format "%%-%ds  %%s\n" srcw))
         (indent (make-string (+ srcw 2) ?\s))
         (parsed (tlon-local--parse-log-line line))
         (raw-src (or src-label (cdr parsed)))
         (src (tlon-local--expand-source-filename raw-src lang))
         (msg (tlon-local--expand-article-ids (car parsed) lang))
         (parts (and msg (split-string msg "\n"))))
    (insert (format fmt (or src "") (or (car parts) "")))
    (dolist (cont (cdr parts))
      (insert indent cont "\n"))))

(defun tlon-local--parse-log-line (line)
  "Return cons (MESSAGE . SOURCE_FILENAME) parsed from LINE.
If LINE is not JSON, return (LINE . nil)."
  (let ((str (string-trim-right line)))
    (condition-case _err
        (let* ((obj (json-parse-string str :object-type 'alist :array-type 'list))
               (raw-msg (or (alist-get 'message obj) (alist-get 'msg obj) str))
               (msg (if (stringp raw-msg) raw-msg (prin1-to-string raw-msg)))
               (raw-src (alist-get 'source_filename obj))
               (src (and raw-src (if (stringp raw-src) raw-src (prin1-to-string raw-src)))))
          (cons msg src))
      (error (cons str nil)))))

(defun tlon-local--expand-source-filename (src lang)
  "Return abbreviated absolute SRC path for LANG with ': position 1' suffix.
If SRC cannot be resolved, return SRC as-is."
  (when (and src (stringp src) (not (string-empty-p src)))
    (let* ((abs
            (cond
             ((file-name-absolute-p src) src)
             (t
              (let* ((repo (tlon-repo-lookup :dir :subproject "uqbar" :language lang))
                     (rel (if (string-match "\\`[^/]+/\\(.*\\)\\'" src)
                              (match-string 1 src)
                            src)))
                (when repo (file-name-concat repo rel))))))
           (path (and abs (abbreviate-file-name abs))))
      (if path
          (format "%s: position 1" path)
        src))))

(defun tlon-local--expand-article-ids (line lang)
  "Expand article_id slugs in LINE into absolute paths for LANG.
The replacement text includes a `: position 1' suffix to work with
`tlon-visit-file-at-point'."
  (let ((repo (tlon-repo-lookup :dir :subproject "uqbar" :language lang)))
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
  (let* ((repo (tlon-repo-lookup :dir :subproject "uqbar" :language lang))
         (articles-dir (tlon-get-bare-dir-translation lang "en" "articles"))
         (file (file-name-with-extension slug "md")))
    (file-name-concat repo articles-dir file)))

(define-derived-mode tlon-local-logs-mode special-mode "Uqbar Logs"
  "Major mode to view Uqbar logs retrieved via Grafana's Loki proxy."
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (use-local-map (copy-keymap special-mode-map))
  (local-set-key (kbd "k") #'previous-line)
  (local-set-key (kbd "l") #'next-line)
  (local-set-key (kbd "g") #'tlon-local-logs-refresh)
  (local-set-key (kbd "i") #'simple-extras-visual-line-mode-enhanced)
  (local-set-key (kbd "o") #'tlon-local-logs-open-in-grafana)
  (local-set-key (kbd "RET") #'tlon-visit-file-at-point))

(defun tlon-local-logs-refresh ()
  "Refresh the logs in the current `tlon-local-logs-mode' buffer."
  (interactive)
  (let* ((ctx tlon-local--logs-ctx)
         (lang (plist-get ctx :lang)))
    (tlon-local-logs lang)))

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

;;;;; Infix toggles

(transient-define-infix tlon-local-infix-rebuild-content-database ()
  "Toggle the `rebuild-content-database' flag."
  :class 'transient-lisp-variable
  :variable 'tlon-local-rebuild-content-database
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-local-rebuild-content-database)))

(transient-define-infix tlon-local-infix-content-branch-production ()
  "Toggle the `content-branch=production' flag."
  :class 'transient-lisp-variable
  :variable 'tlon-local-content-branch-production
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-local-content-branch-production)))

(transient-define-infix tlon-local-infix-no-include-testing ()
  "Toggle the `no-include-testing' flag."
  :class 'transient-lisp-variable
  :variable 'tlon-local-no-include-testing
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-local-no-include-testing)))

(transient-define-infix tlon-local-infix-enforce-single-env ()
  "Toggle enforcing a single running environment."
  :class 'transient-lisp-variable
  :variable 'tlon-local-enforce-single-env
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-local-enforce-single-env)))

;;;###autoload (autoload 'tlon-local-menu "tlon-local" nil t)
(transient-define-prefix tlon-local-menu ()
  "`tlon-local' menu."
  [["Run environment"
    ("q a" "arabic"                        tlon-local-run-uqbar-ar)
    ("q n" "english"                       tlon-local-run-uqbar-en)
    ("q s" "spanish"                       tlon-local-run-uqbar-es)
    ("q r" "french"                        tlon-local-run-uqbar-fr)
    ("q t" "italian"                       tlon-local-run-uqbar-it)
    ("q j" "japanese"                      tlon-local-run-uqbar-ja)
    ("q k" "korean"                        tlon-local-run-uqbar-ko)
    ("q u" "turkish"                       tlon-local-run-uqbar-tr)
    ""
    "Options"
    ("-r" "rebuild content db"             tlon-local-infix-rebuild-content-database)
    ("-b" "content branch=production"      tlon-local-infix-content-branch-production)
    ("-t" "no testing"                     tlon-local-infix-no-include-testing)
    ("-s" "single env"                     tlon-local-infix-enforce-single-env)]
   ["Show logs"
    ("l" "errors and warnings"           tlon-local-logs)]])

(provide 'tlon-local)
;;; tlon-local.el ends here
