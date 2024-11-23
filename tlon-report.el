;;; tlon-report.el --- Team clock data and reports -*- lexical-binding: t -*-

;; Copyright (C) 2024

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

(require 'paths)
(require 'tlon-core)

;;;; Functions

;;;;; Create

(autoload 'org-read-date "org")
(autoload 'org-extras-clock-report-insert "org-extras")
;;;###autoload
(defun tlon-clock-entry-create (date &optional submit)
  "Create a clock entry for DATE in the user’s clock repo.
If DATE is nil, prompt the user for one, defaulting to today. If SUBMIT is nil,
ask the user if they want to submit the entry. If SUBMIT is `never', do not
submit the entry. Otherwise, submit the entry without prompting the user for
confirmation."
  (interactive (list (org-read-date)))
  (let* ((default-directory (tlon-clock-get-repo))
	 (file (tlon-clock-get-file-for-date date)))
    (find-file file)
    (erase-buffer)
    (insert (format "#+TITLE: %s\n\n" date))
    (org-extras-clock-report-insert date date 'agenda)
    (save-buffer)
    (pcase submit
      ('never nil)
      ('nil (tlon-clock-entry-submit-prompt file))
      (_ (tlon-clock-entry-submit file)))))

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
(defun tlon-clock-entry-submit (file)
  "Submit the clock entry FILE.
If FILE is nil, prompt the user for a file, defaulting to the one for today."
  (interactive (list (read-file-name "File: "
				     (tlon-clock-get-repo) nil nil
				     (format-time-string "%Y-%m-%d"))))
  (let ((default-directory (tlon-clock-get-repo)))
    (unless (file-exists-p file)
      (user-error "File `%s' does not exist" file))
    (magit-extras-track-file file)
    (magit-extras-stage-commit-and-push (format "Add clock entry: %s" (file-name-nondirectory file)))))

(autoload 'magit-extras-get-unstaged-files "magit-extras")
(autoload 'magit-untracked-files "magit-extras")
;;;###autoload
(defun tlon-clock-entry-submit-all ()
  "Submit all clock files not yet submitted."
  (interactive)
  (let* ((default-directory (tlon-clock-get-repo))
	 (unstaged (magit-extras-get-unstaged-files))
	 (untracked (magit-untracked-files)))
    (dolist (file (append unstaged untracked))
      (tlon-clock-entry-submit file))))

;;;;; Report

;; TODO

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

(provide 'tlon-report)
;;; tlon-report.el ends here
