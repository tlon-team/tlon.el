;;; tlon-report.el --- Team clock data and reports -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon/blob/main/tlon-report.el

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

;; Team clock data and reports.

;;; Code:

(require 'org-agenda)
(require 'tlon-core)
(require 'transient)

;;;; Functions

;;;;; Create

(autoload 'org-read-date "org")
(autoload 'org-extras-clock-report-insert "org-extras")
(autoload 'org-agenda-files "org")
;;;###autoload
(defun tlon-clock-entry-create (date &optional submit)
  "Create a clock entry for DATE in the user’s clock repo.
If DATE is nil, prompt the user for one, defaulting to today. If SUBMIT is nil,
ask the user if they want to submit the entry. If SUBMIT is `never', do not
submit the entry. Otherwise, submit the entry without prompting the user for
confirmation."
  (interactive (list (org-read-date)))
  (let* ((default-directory (tlon-clock-get-repo))
         (file (tlon-clock-get-file-for-date date))
         (agenda-files (tlon-clock--valid-agenda-files)))
    (find-file file)
    (erase-buffer)
    (insert (format "#+TITLE: %s\n\n" date))
    (let ((org-agenda-files agenda-files))
      (org-extras-clock-report-insert date date 'agenda))
    (save-buffer)
    (pcase submit
      ('never nil)
      ('nil (tlon-clock-entry-submit-prompt file))
      (_ (tlon-clock-entry-submit file)))))

(defun tlon-clock--valid-agenda-files ()
  "Return a sanitized list of agenda files or signal an error.
Remove nils and non-existent files from `org-agenda-files'. Signal an error if
the resulting list is empty."
  (let (result)
    (dolist (f (org-agenda-files))
      (when (and f (stringp f) (file-exists-p f))
        (push f result)))
    (setq result (nreverse result))
    (unless result (user-error "The variable `org-agenda-files' is empty or invalid"))
    result))

(declare-function calendar-extras-get-dates-in-range "calendar-extras")
;;;###autoload
(defun tlon-clock-entry-create-in-range (start end)
  "Create clock entries for dates between START to END in the user’s clock repo."
  (interactive (list (org-read-date nil nil nil "Start date (inclusive):")
		     (org-read-date nil nil nil "End date (inclusive):")))
  (let ((dates (calendar-extras-get-dates-in-range start end)))
    (dolist (date dates)
      (tlon-clock-entry-create date 'never))
    (if (y-or-n-p "Submit all entries? ")
	(tlon-clock-entry-submit-all)
      (message "You can submit the entries later by calling `tlon-clock-entry-submit-all'."))))

;;;;; Submit

(defun tlon-clock-entry-submit-prompt (file)
  "Prompt the user to submit FILE."
  (if (y-or-n-p "Submit? ")
      (tlon-clock-entry-submit file)
    (message "You can submit the entry later by calling `tlon-clock-entry-submit'.")))

(autoload 'magit-extras-track-file "magit-extras")
(autoload 'magit-extras-stage-commit-and-push "magit-extras")
;;;###autoload
(defun tlon-clock-entry-submit (file &optional no-push)
  "Submit the clock entry FILE.
If FILE is nil, prompt the user for a file, defaulting to the one for today.
If NO-PUSH is nil, do not push the commit to the remote repository."
  (interactive (list (read-file-name "File: "
				     (tlon-clock-get-repo) nil nil
				     (format-time-string "%Y-%m-%d"))))
  (let ((default-directory (tlon-clock-get-repo)))
    (unless (file-exists-p file)
      (user-error "File `%s' does not exist" file))
    (magit-extras-track-file file)
    (magit-extras-stage-commit-and-push (format "Add clock entry: %s" (file-name-nondirectory file))
					file no-push)))

(autoload 'magit-extras-get-unstaged-files "magit-extras")
(autoload 'magit-untracked-files "magit-extras")
(declare-function magit-push-current-to-pushremote "magit-push")
;;;###autoload
(defun tlon-clock-entry-submit-all ()
  "Submit all clock files not yet submitted."
  (interactive)
  (let* ((default-directory (tlon-clock-get-repo))
	 (unstaged (magit-extras-get-unstaged-files))
	 (untracked (magit-untracked-files)))
    (dolist (file (append unstaged untracked))
      (tlon-clock-entry-submit file 'no-push))
    (call-interactively #'magit-push-current-to-pushremote)))

;;;;; Report

(autoload 'org-table-align "org-table")
;;;###autoload
(defun tlon-clock-report-create (start-date end-date)
  "Aggregate clock data from your org clock files between START-DATE and END-DATE.
Both dates should be entered as strings in \"YYYY-MM-DD\" format.
The command searches through files in `org-agenda-files' whose names look
like \"YYYY-MM-DD.org\" and expects to find one or more clocktable blocks.
It produces an aggregated report in a new buffer with one table row per
activity (summing times if an activity occurs more than once)."
  (interactive
   (list (read-string "Start date (YYYY-MM-DD): ")
         (read-string "End date (YYYY-MM-DD): ")))
  (let ((activities (make-hash-table :test 'equal))
        (files (tlon-clock-collect-clock-files start-date end-date)))
    (unless files
      (user-error "No org clock files found in the specified date range"))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Look for each clocktable block in the file.
        (while (re-search-forward "^#\\+BEGIN: clocktable\\_>" nil t)
          (let ((block-start (match-beginning 0)))
            (when (re-search-forward "^#\\+END:" nil t)
              (let ((block-end (point)))
                (let ((block-text (buffer-substring-no-properties block-start block-end)))
                  (tlon-clock-parse-clocktable-block block-text activities))))))))
    ;; Now produce the report buffer.
    (let ((report-buffer (get-buffer-create "*Aggregated Clock Report*"))
          total)
      (with-current-buffer report-buffer
        (erase-buffer)
        (insert (format "#+TITLE: Aggregated Clock Report from %s to %s\n\n" start-date end-date))
        (insert "| Headline | Time |\n")
        (insert "|----------+------|\n")
        (setq total 0)
        ;; Collect and sort the activity keys.
        (let (keys)
          (maphash (lambda (k _v) (push k keys)) activities)
          (setq keys (sort keys #'string<))
          (dolist (key keys)
            (let ((mins (gethash key activities)))
              (setq total (+ total mins))
              (insert (format "| %s | %s |\n" key (tlon-clock-minutes-to-time-string mins))))))
        (insert "|----------+------|\n")
        (insert (format "| ALL /Total time/ | %s |\n" (tlon-clock-minutes-to-time-string total)))
	(org-table-align)
        (insert "\n")
	(tlon-clock-insert-summary-stats total start-date end-date)
        (org-mode))
      (display-buffer report-buffer))))

(defun tlon-clock-time-string-to-minutes (time-string)
  "Convert a TIME-STRING of the form \"H:MM\" to minutes."
  (when time-string
    (if (string-match "\\([0-9]+\\):\\([0-9]+\\)" time-string)
        (+ (* 60 (string-to-number (match-string 1 time-string)))
           (string-to-number (match-string 2 time-string)))
      0)))

(defun tlon-clock-minutes-to-time-string (minutes)
  "Convert MINUTES to a formatted time string H:MM."
  (format "%d:%02d" (/ minutes 60) (mod minutes 60)))

(defun tlon-clock-parse-clocktable-block (block-text activities)
  "Parse clocktable BLOCK-TEXT and add activity times into ACTIVITIES.
Each detailed activity row is expected to have no text in its first column.
Rows that include the words \"Total time\" or \"/File time/\" are skipped."
  (dolist (line (split-string block-text "\n" t))
    (when (string-match-p "^|" line)  ; only process table rows
      (let* ((cells (mapcar #'string-trim
                            (split-string
                             ;; remove the leading and trailing vertical bar:
                             (substring line 1 (if (string-suffix-p "|" line)
                                                   -1
                                                 (length line)))
                             "|" t)))
             (col1 (nth 0 cells))
             (headline (nth 1 cells)))
        ;; We want only detailed rows, which have an empty first column;
        ;; also skip summary rows.
        (unless (or (not (string= col1 ""))
		    (or (string-match-p "Total time" headline)
                        (string-match-p "\\/File time\\*" headline)))
          ;; Try to get a time value from column 3 or, if blank, column 4.
          (let ((time-cell (or (nth 2 cells) (nth 3 cells))))
	    (when (and time-cell (not (string= time-cell "")))
	      ;; Remove any extraneous asterisks.
	      (setq time-cell (replace-regexp-in-string "\\*" "" time-cell))
	      (let ((minutes (tlon-clock-time-string-to-minutes time-cell)))
                (when minutes
                  (puthash headline (+ (gethash headline activities 0)
                                       minutes)
                           activities))))))))))

(autoload 'org-string<= "org-macs")
(defun tlon-clock-collect-clock-files (start-date end-date)
  "Return a list of org files between START-DATE and END-DATE."
  (let (result)
    (dolist (file (directory-files (tlon-clock-get-repo) t "\\.org$"))
      (let ((fname (file-name-nondirectory file)))
        (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\.org" fname)
          (let ((file-date (match-string 1 fname)))
            (when (and (org-string<= start-date file-date)
                       (org-string<= file-date end-date))
              (push file result))))))
    result))

(autoload 'org-time-string-to-time "org")
(defun tlon-clock-insert-summary-stats (total start-date end-date)
  "Return summary statistics for TOTAL hours worked between START-DATE END-DATE."
  (let* ((total-hours (/ (float total) 60))
         (start-time (org-time-string-to-time start-date))
         (end-time (org-time-string-to-time end-date))
         (num-days (max 1 (1+ (floor (/ (float-time (time-subtract end-time start-time))
                                        86400)))))
         (hours-per-day (/ total-hours num-days))
         (hours-per-week (* hours-per-day 7)))
    (insert (format "Total days: %d\n" num-days))
    (insert (format "Total hours: %.2f\n" total-hours))
    (insert (format "Hours per day: %.2f\n" hours-per-day))
    (insert (format "Hours per week: %.2f\n" hours-per-week))
    (insert (format "Hours per workday: %.2f\n" (/ hours-per-week 5)))
    (insert "\n")))

;;;;; Misc

(defun tlon-clock-get-file-for-date (date)
  "Return the path to the file of the clock entry for DATE."
  (let* ((date (or date (org-read-date)))
	 (file-name (file-name-with-extension date "org")))
    (file-name-concat (tlon-clock-get-repo) file-name)))

(defun tlon-clock-get-repo ()
  "Return the path to the user’s clock repo."
  (let ((repo-name (concat "clock-"
			   (downcase (tlon-user-lookup :nickname :name user-full-name)))))
    (file-name-concat paths-dir-tlon-repos (file-name-as-directory repo-name))))

(defun tlon-clock-open-entry (date)
  "Open the clock entry for DATE."
  (interactive (list (org-read-date)))
  (find-file (tlon-clock-get-file-for-date date)))

;;;;; Menu

;;;###autoload (autoload 'tlon-clock-menu "tlon-report" nil t)
(transient-define-prefix tlon-clock-menu ()
  "`clock' menu."
  [["Create"
    ("c" "Create entry"               tlon-clock-entry-create)
    ("C" "Create entries in range"    tlon-clock-entry-create-in-range)]
   ["Submit"
    ("s" "Submit entry"               tlon-clock-entry-submit)
    ("S" "Submit all entries"         tlon-clock-entry-submit-all)]
   ["Report"
    ("r" "Report"                     tlon-clock-report-create)]
   ["Misc"
    ("o" "Open entry"                 tlon-clock-open-entry)]])

(provide 'tlon-report)
;;; tlon-report.el ends here
