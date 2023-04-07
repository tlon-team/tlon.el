;;; tlon.el --- A collection of convenience functions to be used by the Tlön team. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.0
;; Package-Requires: ((org "9.1") (dired "1.0") (ox-hugo "1.2.0"))
;; Homepage: https://tlon.team
;; Keywords: convenience tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(defun ps/tlon-meeting-with (person tareas-id meetings-id pending-id)
  "TODO: docstring"
  ;; "[person] > Meetings > Pending for next meeting" org heading in `work-dashboard.org'
  (ps/org-id-goto pending-id)
  (let ((contents (ps/org-get-heading-contents)))
    (ps/org-clear-heading-contents)
    ;; "[person] > Meetings" org heading in `work-dashboard.org'
    (ps/org-id-goto meetings-id)
    (org-narrow-to-subtree)
    (goto-char (point-max))
    (org-insert-heading)
    (insert "Meeting on ")
    (ps/org-time-stamp-inactive-current-time)
    (unless (string= contents "")
      (insert (concat "\nTo discuss:\n" contents "\n"))))
  (forward-line)
  (ps/org-narrow-to-entry-and-children)
  (ps/window-split-if-unsplit)
  (ps/switch-to-last-window)
  ;; "Tareas" org heading in `tareas.org'
  (ps/org-id-goto tareas-id)
  (ps/org-narrow-to-entry-and-children)
  (ps/switch-to-last-window))

(defun ps/tlon-bae-format-file (&optional extension)
  "Prompt the user for bibliographic information and return a file
name based on it."
  (let* ((lastname (read-string "Last name(s) [separated by spaces if more than one author]: "))
         (title (read-string "Title: "))
         (slug-lastname (ps/bibtex-asciify-string (org-hugo-slug lastname)))
         (slug-title (ps/bibtex-asciify-string (org-hugo-slug title)))
         (extension (or extension "md")))
    (file-name-with-extension (concat slug-lastname "--" slug-title) extension)))

(defun ps/tlon-bae-rename-file (&optional extension)
  "Prompt the user for bibliographic information and rename file at
point based on it."
  (interactive)
  (let* ((source-file-path (dired-get-filename)))
    (rename-file
     source-file-path
     (file-name-concat
      (file-name-directory source-file-path)
      (ps/tlon-bae-format-file extension))))
  (revert-buffer))

(defun ps/tlon-bae-create-file (&optional extension)
  "Prompt the user for bibliographic information and create a new
 file based on it in the current directory."
  (interactive)
  (find-file (ps/tlon-bae-format-file extension)))

(provide 'tlon)
;;; tlon.el ends here
