;;; tlon-babel-clock.el --- Team clock reports -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL:
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

;; Team clock reports.

;;; Code:

(require 'paths)
(require 'magit-extra)
(require 'org-extras)
(require 'tlon-babel-core)

;;;; Functions

;;;;; Clock reports

(defun tlon-babel-clock-entry-create (date)
  "Create a new clock entry for DATE in the user’s clock repo.
If DATE is nil, prompt the user for one, defaulting to today."
  (interactive (list (org-read-date)))
  (let* ((default-directory (tlon-babel-clock-get-repo))
	 (file (tlon-babel-clock-get-file-for-date date)))
    (when (file-exists-p file)
      (user-error "A clock entry for today already exists"))
    (find-file file)
    (insert (format "#+TITLE: %s\n\n" date))
    (org-extras-clock-report-insert date date 'agenda)
    (save-buffer)
    (if (y-or-n-p "Submit? ")
	(tlon-babel-clock-entry-submit file)
      (message "You can submit the entry later by calling `tlon-babel-clock-entry-submit'."))))

(defun tlon-babel-clock-entry-submit (file)
  "Submit the clock entry in FILE.
If FILE is nil, prompt the user for a file, defaulting to the one for today."
  (interactive (list (read-file-name "File: "
				     (tlon-babel-clock-get-repo) nil nil
				     (format-time-string "%Y-%m-%d"))))
  (let ((default-directory (file-name-directory file)))
    (when (not (file-exists-p file))
      (user-error "File `%s' does not exist" file))
    (magit-call-git "add" (expand-file-name file)) ; track file if untracked
    (magit-extras-stage-commit-and-push (format "Add clock entry: %s"
						(file-name-nondirectory file)))))

(defun tlon-babel-clock-get-file-for-date (date)
  "Return the path to the file of the clock entry for DATE."
  (let* ((date (or date (org-read-date)))
	 (file-name (file-name-with-extension date "org")))
    (file-name-concat (tlon-babel-clock-get-repo) file-name)))

(defun tlon-babel-clock-get-repo ()
  "Return the path to the user’s clock repo."
  (let ((repo-name (concat "clock-"
			   (downcase (tlon-babel-user-lookup :nickname :name user-full-name)))))
    (file-name-concat paths-dir-tlon-repos (file-name-as-directory repo-name))))

(defun tlon-babel-clock-open-entry (date)
  "Open the clock entry for DATE."
  (interactive (list (org-read-date)))
  (find-file (tlon-babel-clock-get-file-for-date date)))

;;;;; Menu

;;;###autoload (autoload 'tlon-babel-clock-menu "tlon-babel-clock" nil t)
(transient-define-prefix tlon-babel-clock-menu ()
  "`clock' menu."
  [("e" "Create entry" tlon-babel-clock-entry-create)
   ("o" "Open entry" tlon-babel-clock-open-entry)
   ("s" "Submit entry" tlon-babel-clock-entry-submit)])

(provide 'tlon-babel-clock)
;;; tlon-babel-clock.el ends here
