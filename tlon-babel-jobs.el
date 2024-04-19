;;; tlon-babel-jobs.el --- Functions for processing Babel jobs -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon-babel
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

;; Functions for processing Babel jobs.

;;; Code:

(require 'ebib)
(require 'ebib-extras)
(require 'orgit-forge)
(require 'tlon-babel)
(require 'tlon-babel-core)
(require 'tlon-babel-import)
(require 'tlon-babel-forg)
(require 'tlon-babel-split)
(require 'tlon-babel-tts)
(require 'window-extras)

;;;; Main variables

(defconst tlon-babel-job-labels
  '((:label "Awaiting processing"
	    :action "Process"
	    :assignee "worldsaround")
    (:label "Awaiting translation"
	    :action "Translate"
	    :assignee "")
    (:label "Awaiting revision"
	    :action "Revise"
	    :assignee "worldsaround")
    (:label "Awaiting check"
	    :action "Check"
	    :assignee "worldsaround")
    (:label "Awaiting review"
	    :action "Review"
	    :assignee "benthamite")
    (:label "Published"
	    :action "Publish"
	    :assignee ""))
  "List of labels and associated properties.")

(defconst tlon-babel-jobs-manual-processing-id
  "60251C8E-6A6F-430A-9DB3-15158CC82EAE"
  "Org ID of the `processing' heading in `manual.org'.")

(defconst tlon-babel-jobs-id
  "820BEDE2-F982-466F-A391-100235D4C596"
  "Org ID of the `jobs' heading in `jobs.org'.")

;;;; Functions

;;;;; Job phases

;;;###autoload
(defun tlon-babel-jobs-dwim ()
  "Initialize or finalize process based on clocked task."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-clock-in))
  (save-buffer)
  (let* ((action (tlon-babel-jobs-get-action-in-label (tlon-babel-get-clock-label)))
	 (stage (pcase major-mode
		  ('org-mode 'initialize)
		  ('markdown-mode 'finalize)
		  (_ (user-error "I don't know what to do in `%s`" major-mode))))
	 (fun (intern (format "tlon-babel-jobs-%s" stage)))
	 (arg (intern (format "tlon-babel-jobs-%s-%s" stage action))))
    (if (eq stage 'initialize)
	(funcall fun arg)
      (funcall fun))))

(defun tlon-babel-jobs-get-action-in-label (label)
  "Return action associated with LABEL."
  (let ((action (cadr (split-string label))))
    action))

(defun tlon-babel-jobs-initialize (fun)
  "Initialize process associated with FUN.
Runs all the general initialization functions, followed by the specific function
for the process that is being initialized."
  (let* ((key (tlon-babel-get-clock-key))
	 (metadata (tlon-babel-metadata-in-repos :subtype 'translations))
	 (file (tlon-babel-metadata-lookup metadata "file" "original_key" key))
	 (repo (tlon-babel-get-repo-from-file file))
	 (default-directory repo))
    (tlon-babel-check-label-and-assignee repo)
    (tlon-babel-check-branch "main" repo)
    (call-interactively #'magit-pull-from-upstream nil)
    (sleep-for 2)
    (cl-multiple-value-bind
	(original-path translation-path)
	(tlon-babel-set-paths-from-clock)
      (let ((issue (tlon-babel-get-clock-issue)))
	(tlon-babel-set-windows original-path translation-path)
	(write-file translation-path)
	(winum-select-window-2)
	(orgit-topic-open issue)
	(tlon-babel-copy-buffer original-path)
	(funcall fun)))))

(defun tlon-babel-jobs-finalize ()
  "Finalize current stage of translation process."
  (tlon-babel-split-mode -1)
  (save-buffer)
  (cl-multiple-value-bind
      (original-path translation-path original-key)
      (tlon-babel-set-paths-from-clock)
    (let* ((repo (tlon-babel-get-repo))
	   (current-action (tlon-babel-get-clock-action))
	   (next-label (tlon-babel-get-clock-next-label))
	   (next-assignee (tlon-babel-jobs-get-next-assignee)))
      ;; MAYBE: check that it is a repo of `translations' subtype
      ;; thought this would have to exclude “process” stage
      (tlon-babel-check-branch "main" repo)
      (tlon-babel-check-label-and-assignee repo)
      (tlon-babel-check-file
       (when (string= current-action "Process")
	 'original))
      (pcase current-action
	("translate"
	 (unless (y-or-n-p "Have you processed all Jinx and Flycheck warnings, and ran `tlon-babel-manual-fix-all'?")
	   (user-error "Aborted")))
	("check"
	 (tlon-babel-tts-mode -1)
	 (remove-hook 'eww-mode-hook #'tlon-babel-tts-mode)))
      (save-buffer)
      (if (string= current-action "Process")
	  (write-file original-path)
	(write-file translation-path))
      (when (string= current-action "Process")
	(tlon-babel-commit-and-push current-action original-path))
      (tlon-babel-commit-and-push current-action translation-path)
      (tlon-babel-jobs-act-on-issue original-key next-label next-assignee
				    (when (string= current-action "Review")
				      'close))
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'"
	       next-label next-assignee))
    (tlon-babel-jobs-finalize-set-todos)))

(defun tlon-babel-jobs-act-on-issue (original-key label assignee &optional close)
  "Apply LABEL and ASSIGNEE to issue associated with ORIGINAL-KEY.
If CLOSE is non-nil, close the issue."
  (let* ((issue-title (format "Job: `%s" original-key))
	 (issue (tlon-babel-issue-lookup issue-title))
	 (default-directory (tlon-babel-get-repo 'error 'include-all)))
    (tlon-babel-set-labels `(,label) issue)
    (tlon-babel-set-assignee assignee issue)
    (when close
      (tlon-babel-close-issue issue))))

(defun tlon-babel-jobs-get-next-assignee ()
  "Get the next assignee based on the current user and clock label.
This function returns the assignee designated for the next label if the current
user is the user designated for the current label; otherwise, it returns the
substitute assignee."
  (let*
      ((current-user (tlon-babel-user-lookup :github :name user-full-name))
       (current-assignee (tlon-babel-label-lookup :assignee :label (tlon-babel-get-clock-label)))
       (designated-next-assignee (tlon-babel-label-lookup :assignee :label (tlon-babel-get-clock-next-label)))
       (substitute-next-assigne (tlon-babel-user-lookup :substitute :github designated-next-assignee)))
    (if (string= current-user current-assignee)
	designated-next-assignee
      substitute-next-assigne)))

(defun tlon-babel-jobs-finalize-set-todos ()
  "Set relevant TODO statuses during the finalize process."
  (let ((todo (tlon-babel-get-clock))
	(key (tlon-babel-get-clock-key))
	(current-action (tlon-babel-get-clock-action)))
    (tlon-babel-mark-todo-done todo (tlon-babel-get-todos-jobs-file))
    (when (or (string= current-action "Review") (string= current-action "Check"))
      (let ((parent-todo (tlon-babel-get-parent-todo todo)))
	(tlon-babel-mark-todo-done parent-todo (tlon-babel-get-todos-jobs-file))))
    (when (string= current-action "Review")
      (let ((job-todo (format "[cite:@%s]" key)))
	(tlon-babel-mark-todo-done job-todo tlon-babel-file-jobs)
	(tlon-babel-jobs-sort-headings tlon-babel-file-jobs)
	(tlon-babel-commit-and-push "Update"
				    tlon-babel-file-jobs)))))

(defun tlon-babel-jobs-initialize-processing ()
  "Initialize processing."
  (cl-multiple-value-bind
      (original-path)
      (tlon-babel-set-paths-from-clock)
    (tlon-babel-set-windows original-path tlon-babel-file-babel-manual)
    (org-id-goto tlon-babel-jobs-manual-processing-id)
    (org-narrow-to-subtree)
    (org-extras-show-subtree-hide-drawers)
    (winum-select-window-2)
    (let ((issue (tlon-babel-get-clock-issue)))
      (orgit-topic-open issue))))

(defun tlon-babel-jobs-initialize-translation ()
  "Initialize translation."
  (macos-open-app "deepl"))

(defun tlon-babel-jobs-initialize-revision ()
  "Initialize stylistic revision."
  (winum-select-window-2)
  (tlon-babel-split-mode))

(defun tlon-babel-jobs-initialize-check ()
  "Initialize accuracy check."
  ;; we move the buffer displaying the issue to the right, to uncover
  ;; the original file
  (window-extras-buffer-move-right)
  (window-extras-switch-to-last-window)
  (add-hook 'eww-mode-hook #'tlon-babel-tts-mode)
  (markdown-preview)
  (read-aloud-buf))

(defun tlon-babel-jobs-initialize-review ()
  "Initialize review."
  (cl-multiple-value-bind
      (_ translation-path)
      (tlon-babel-set-paths-from-clock)
    (tlon-babel-log-buffer-latest-user-commit-ediff translation-path)
    (winum-select-window-1)
    (setq-local jinx-languages "es")
    (add-file-local-variable 'jinx-languages jinx-languages)
    (setf (alist-get 'jinx-languages file-local-variables-alist) jinx-languages)
    (jinx--load-dicts)
    (jinx--cleanup)
    (goto-char (point-min))
    (save-buffer)
    (files-extras-switch-to-alternate-buffer)))

;;;;; Job creation

;;;###autoload
(defun tlon-babel-create-job ()
  "Create a new job for IDENTIFIER based on Ebib entry at point.
Creating a new job means (1) importing a document and (2) creating a record for
it. A record is (a) an issue in GitHub and (b) a heading in `jobs.org'.

IDENTIFIER can be a URL or a PDF file path."
  (interactive)
  (unless (derived-mode-p 'ebib-entry-mode 'ebib-index-mode)
    (user-error "This command must be run from an Ebib buffer"))
  (if-let ((id (or (ebib-extras-get-field "url")
		   (ebib-extras-get-file "md")))
	   (title (ebib-extras-get-field "title"))
	   (key (ebib-extras-get-field "=key="))
	   (repo (completing-read "Repo: " (tlon-babel-repo-lookup-all :dir :subtype 'translations))))
      (progn
	(tlon-babel-import-document id title)
	(tlon-babel-create-translation-file repo)
	(tlon-babel-create-record-for-job key))
    (user-error "The current Ebib entry seems to be missing one of the following
fields, which are needed to create a new job: `url' or `file',
`title' and `key'")))

(defun tlon-babel-create-record-for-job (&optional key)
  "Create a record based on KEY.
Creates a new record in the repository (with the format `Job: KEY') and a new
heading in the file `jobs.org'. If KEY is not provided, the key in the current
Markdown buffer at point is used."
  (interactive)
  (if-let ((key (or key
		    (pcase major-mode
		      ('markdown-mode (tlon-babel-get-key-in-buffer))
		      ('ebib-entry-mode (ebib--get-key-at-point))))))
      (progn
	(tlon-babel-create-issue-from-key key)
	(tlon-babel-create-heading-for-job key 'commit))
    (user-error "I wasn't able to create a record because I didn't find a key")))

;;;;; `jobs.org'

(defun tlon-babel-create-heading-for-job (&optional key commit)
  "Create a heading based on BibTeX KEY in `jobs.org'.
If KEY is not provided, the key in the Markdown buffer at point is used. If
COMMIT is non-nil, commit the change."
  (interactive)
  (let* ((key (or key (tlon-babel-get-key-in-buffer)))
	 (heading (format "[cite:@%s]" key))
	 (file (tlon-babel-metadata-lookup (tlon-babel-metadata-in-repo) "file" "original_key" key))
	 (repo (tlon-babel-get-repo-from-file file))
	 (repo-abbrev (tlon-babel-repo-lookup :abbrev :dir repo)))
    (with-current-buffer (or (find-buffer-visiting tlon-babel-file-jobs)
			     (find-file-noselect tlon-babel-file-jobs))
      (widen)
      (goto-char (point-min))
      (unless (search-forward heading nil t)
	(re-search-forward tlon-babel-jobs-id nil t)
	(while (and (not (org-at-heading-p)) (not (eobp)))
	  (forward-line))
	(org-insert-heading)
	(insert heading)
	(org-todo 'todo)
	(org-set-tags repo-abbrev)
	(tlon-babel-jobs-sort-headings tlon-babel-file-jobs)
	(save-buffer)))
    (when commit
      (tlon-babel-commit-and-push "Update" tlon-babel-file-jobs))))

(defun tlon-babel-jobs-sort-headings (&optional file)
  "Sort all headings under parent in FILE alphabetically and by TODO order."
  (interactive)
  (with-current-buffer (or (find-buffer-visiting file)
			   (find-file-noselect file))
    (widen)
    (org-up-heading-safe)
    (org-sort-entries nil ?a)
    (org-sort-entries nil ?o)
    (save-buffer)))

(defun tlon-babel-jobs-get-key-in-heading ()
  "Get the key of the currently clocked task."
  (unless (org-at-heading-p)
    (user-error "Not in an org-mode heading"))
  (let ((heading (substring-no-properties (org-get-heading t t t t))))
    (if (string-match "\\[cite:@\\(.+?\\)\\]\\|Job: `\\(.+?\\)\\.md`" heading)
	(or (match-string 1 heading)
	    (match-string 2 heading))
      (user-error "I wasn't able to find a key in clocked heading"))))

(defun tlon-babel-jobs-goto-heading (key)
  "Move point to the heading in `jobs.org' with KEY."
  (with-current-buffer (or (find-buffer-visiting tlon-babel-file-jobs)
			   (find-file-noselect tlon-babel-file-jobs))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
	(when (string= (org-element-property :raw-value headline) (format "[cite:@%s]" key))
	  (goto-char (org-element-property :begin headline)))))))

;;;;; Transient

;;;###autoload (autoload 'tlon-babel-jobs-menu "tlon-babel-jobs" nil t)
(transient-define-prefix tlon-babel-jobs-menu ()
  "`jobs' menu."
  :info-manual "(tlon-babel) Jobs"
  [
   ["Actions"
    ("j" "dwim"                     tlon-babel-jobs-dwim)
    ("c" "create job"               tlon-babel-create-job)]
   ["Add or modify"
    ("g" "glossary"                 tlon-babel-glossary-dwim)
    ("s" "section corresp"          tlon-babel-section-correspondence-dwim)
    ("u" "URL corresp"              tlon-babel-url-correspondence-dwim)]
   ["jobs.org"
    ("r" "create record"            tlon-babel-create-record-for-job)
    ("h" "create heading"           tlon-babel-create-heading-for-job)
    ("t" "sort headings"            tlon-babel-jobs-sort-headings)]])

(provide 'tlon-babel-jobs)
;;; tlon-babel-jobs.el ends here

