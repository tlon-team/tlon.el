;;; tlon-clock.el --- org-clock functionality -*- lexical-binding: t -*-

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

;; `org-clock' functionality.

;;; Code:

(require 'cl-lib)
(require 'org-clock)
(require 'tlon-core)
(require 'transient)

;;;; Variables

(defconst tlon-key-regexp "`\\(.+?\\)\\(\\.md\\)?`"
  "Regular expression for matching bibtex keys in clocked headings.
The second capture group handles the `.md' extension, which we used previously.")

;;;; Functions

(defun tlon-get-clock ()
  "Return the currently clocked heading."
  (if org-clock-current-task
      (substring-no-properties org-clock-current-task)
    (user-error "No clock running")))

;;;###autoload
(defun tlon-get-clock-key ()
  "Return bibtex key in clocked heading.
Assumes key is enclosed in backticks."
  ;; second capture group handles optional .md extension
  (if (string-match tlon-key-regexp (tlon-get-clock))
      (match-string 1 (tlon-get-clock))
    (user-error "I wasn't able to find a file in clocked heading")))

(declare-function tlon-get-file-from-key "tlon")
(defun tlon-get-clock-file ()
  "Return the file path of the clocked task."
  (let ((key (tlon-get-clock-key)))
    (tlon-get-file-from-key key)))

(defun tlon-open-clock-file ()
  "Open file of clocked task."
  (interactive)
  (find-file (tlon-get-clock-file)))

(defun tlon-get-clock-issue ()
  "Get issue GID from `orgit-forge' link in heading at point."
  (unless org-clock-heading
    (user-error "No clock running"))
  (save-window-excursion
    (org-clock-goto)
    (org-narrow-to-subtree)
    (when (re-search-forward org-link-bracket-re)
      (let ((raw-link (org-link-unescape (match-string-no-properties 1))))
	(string-match "orgit-topic:\\(.+\\)" raw-link)
	(match-string 1 raw-link)))))

(declare-function forge-visit-issue "forge-commands")
(defun tlon-open-clock-issue ()
  "Open the issue from `orgit-forge' link in heading at point."
  (interactive)
  (let ((default-directory (tlon-get-repo))
	(issue (tlon-get-clock-issue)))
    (forge-visit-issue issue)))

(defun tlon-get-clock-action ()
  "Return action in clock.
Assumes action is first word of clocked task."
  ;; as rough validation, we check that the clocked heading contains a file
  (tlon-get-clock-key)
  (let ((action (nth 1 (split-string (tlon-get-clock))))
	(actions (tlon-label-lookup-all :action)))
    (if (member action actions)
	action
      (user-error "I wasn't able to find a relevant action in clocked heading"))))

(defun tlon-get-clock-label ()
  "Return label associated with action in heading at point."
  (let ((label (tlon-label-lookup :label :action (tlon-get-clock-action))))
    label))

(defvar tlon-job-labels)
(defun tlon-get-clock-next-label ()
  "Return label associated with the action after the one in heading at point."
  (tlon-next-value :label (tlon-get-clock-label) tlon-job-labels))

(defun tlon-next-value (property value alist)
  "Return the \"next\" value of PROPERTY with VALUE in ALIST."
  (catch 'found
    (let ((found nil))
      (dolist (item alist)
	(when (and found (not (equal (plist-get item property) value)))
	  (throw 'found (plist-get item property)))
	(when (equal (plist-get item property) value)
	  (setq found t))))
    nil))

(defun tlon-copy-buffer (&optional file)
  "Copy the contents of FILE to the kill ring.
Defaults to the current buffer if no FILE is specified."
  (let ((file (or file (buffer-file-name))))
    (with-current-buffer (find-file-noselect file)
      (copy-region-as-kill (point-min) (point-max)))
    (message "Copied the contents of `%s' to kill ring" (file-name-nondirectory file))))

(defun tlon-copy-region (beg end)
  "Copy the contents between BEG and END to the kill ring."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (copy-region-as-kill (point-min) (point-max))))
  (message "Copied the contents of the region to kill ring"))

(defun tlon-copy-dwim ()
  "Copy the contents of the region or buffer to the kill ring."
  (interactive)
  (if (region-active-p)
      (tlon-copy-region (region-beginning) (region-end))
    (tlon-copy-buffer)))

(declare-function tlon-metadata-in-repos "tlon-yaml")
(declare-function tlon-get-counterpart "tlon-counterpart")
(defun tlon-set-paths-from-clock ()
  "Return paths for original and translation files based on clocked task."
  (let* ((key (tlon-get-clock-key))
	 (metadata (tlon-metadata-in-repos :subtype 'translations))
	 (translation (tlon-metadata-lookup metadata "file" "original_key" key))
	 (original (tlon-get-counterpart translation)))
    (cl-values original translation key)))

(declare-function window-extras-split-if-unsplit "window-extras")
(declare-function winum-select-window-1 "winum")
(declare-function winum-select-window-2 "winum")
(defun tlon-set-windows (original-path translation-path)
  "Open ORIGINAL-PATH and TRANSLATION-PATH in windows 1 and 2."
  (window-extras-split-if-unsplit)
  (winum-select-window-1)
  (find-file original-path)
  (winum-select-window-2)
  (find-file translation-path))

(provide 'tlon-clock)
;;; tlon-clock.el ends here

