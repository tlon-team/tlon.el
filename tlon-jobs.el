;;; tlon-jobs.el --- Functions for processing Babel jobs -*- lexical-binding: t -*-

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

;; Functions for processing Babel jobs.

;;; Code:

(require 'magit)
(require 'tlon-clock)
(require 'tlon-core)
(require 'tlon-forg)
(require 'tlon-import)

;;;; Variables

(defconst tlon-job-labels
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

(defconst tlon-jobs-manual-processing-id
  "60251C8E-6A6F-430A-9DB3-15158CC82EAE"
  "Org ID of the `processing' heading in `manual.org'.")

(defconst tlon-jobs-id
  "820BEDE2-F982-466F-A391-100235D4C596"
  "Org ID of the `jobs' heading in `jobs.org'.")

(defvar tlon-jobs-file nil
  "File containing the jobs.
This variable should not be set manually.")

(defvar tlon-file-url-correspondences
  (file-name-concat (tlon-repo-lookup :dir :name "babel-refs") "url" "url-correspondences.json")
  "File containing the URL correspondences.")

;;;; Functions

;;;;; Job phases

(defun tlon-jobs-get-file ()
  "Get the file containing the jobs `org-mode' ID."
  (tlon-get-or-set-org-var 'tlon-jobs-file tlon-jobs-id))

;;;###autoload
(defun tlon-jobs-start-or-finish-phase ()
  "Initialize or finalize process based on clocked task."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-clock-in))
  (save-buffer)
  (let* ((action (tlon-jobs-get-action-in-label (tlon-get-clock-label)))
	 (stage (pcase major-mode
		  ('org-mode 'initialize)
		  ('markdown-mode 'finalize)
		  (_ (user-error "I don't know what to do in `%s`" major-mode))))
	 (fun (intern (format "tlon-jobs-%s" stage)))
	 (arg (intern (format "tlon-jobs-%s-%s" stage action))))
    (if (eq stage 'initialize)
	(funcall fun arg)
      (funcall fun))))

(defun tlon-jobs-get-action-in-label (label)
  "Return action associated with LABEL."
  (let ((action (cadr (split-string label))))
    action))

(autoload 'orgit-topic-open "orgit-forge")
(autoload 'magit-pull-from-upstream "magit-pull")
(autoload 'tlon-check-branch "tlon")
(autoload 'winum-select-window-2 "winum")
(autoload 'tlon-metadata-in-repos "tlon-yaml")
(autoload 'tlon-get-clock-key "tlon-clock")
(defun tlon-jobs-initialize (fun)
  "Initialize process associated with FUN.
Runs all the general initialization functions, followed by the specific function
for the process that is being initialized."
  (let* ((key (tlon-get-clock-key))
	 (metadata (tlon-metadata-in-repos :subtype 'translations))
	 (file (tlon-metadata-lookup metadata "file" "original_key" key))
	 (repo (tlon-get-repo-from-file file))
	 (default-directory repo))
    (tlon-check-label-and-assignee repo)
    (tlon-check-branch "main" repo)
    (call-interactively #'magit-pull-from-upstream nil)
    (sleep-for 2)
    (cl-multiple-value-bind
	(original-path translation-path)
	(tlon-set-paths-from-clock)
      (let ((issue (tlon-get-clock-issue)))
	(tlon-set-windows original-path translation-path)
	(write-file translation-path)
	(winum-select-window-2)
	(orgit-topic-open issue)
	(tlon-copy-buffer original-path)
	(funcall fun)))))

(autoload 'tlon-check-file "tlon")
(autoload 'tlon-commit-and-push "tlon")
(autoload 'tlon-split-mode "tlon")
(defun tlon-jobs-finalize ()
  "Finalize current stage of translation process."
  (tlon-split-mode -1)
  (save-buffer)
  (cl-multiple-value-bind
      (original-path translation-path original-key)
      (tlon-set-paths-from-clock)
    (let* ((repo (tlon-get-repo))
	   (current-action (tlon-get-clock-action))
	   (next-label (tlon-get-clock-next-label))
	   (next-assignee (tlon-jobs-get-next-assignee)))
      ;; MAYBE: check that it is a repo of `translations' subtype
      ;; thought this would have to exclude “process” stage
      (tlon-check-branch "main" repo)
      (tlon-check-label-and-assignee repo)
      (tlon-check-file
       (when (string= current-action "Process")
	 'original))
      (pcase current-action
	("translate"
	 (unless (y-or-n-p "Have you processed all Jinx and Flycheck warnings, and ran `tlon-manual-fix-all'?")
	   (user-error "Aborted")))
	("check"
	 ;; used to call the two functions below but they are obsolete; revise
	 ;; (tlon-tts-mode -1)
	 ;; (remove-hook 'eww-mode-hook #'tlon-tts-mode)
	 ))
      (save-buffer)
      (if (string= current-action "Process")
	  (write-file original-path)
	(write-file translation-path))
      (when (string= current-action "Process")
	(tlon-commit-and-push current-action original-path))
      (tlon-commit-and-push current-action translation-path)
      (tlon-jobs-act-on-issue original-key next-label next-assignee
			      (when (string= current-action "Review")
				'close))
      (message "Marked as DONE. Set label to `%s' and assignee to `%s'"
	       next-label next-assignee))
    (tlon-jobs-finalize-set-todos)))

(defun tlon-jobs-act-on-issue (original-key label assignee &optional close)
  "Apply LABEL and ASSIGNEE to issue associated with ORIGINAL-KEY.
If CLOSE is non-nil, close the issue."
  (let* ((issue-title (format "Job: `%s" original-key))
	 (issue (tlon-issue-lookup issue-title))
	 (default-directory (tlon-get-repo 'error 'include-all)))
    (tlon-set-labels `(,label) 'phase issue)
    (tlon-set-assignee assignee issue)
    (when close
      (tlon-close-issue issue))))

(defun tlon-jobs-get-next-assignee ()
  "Get the next assignee based on the current user and clock label.
This function returns the assignee designated for the next label if the current
user is the user designated for the current label; otherwise, it returns the
substitute assignee."
  (let*
      ((current-user (tlon-user-lookup :github :name user-full-name))
       (current-assignee (tlon-label-lookup :assignee :label (tlon-get-clock-label)))
       (designated-next-assignee (tlon-label-lookup :assignee :label (tlon-get-clock-next-label)))
       (substitute-next-assigne (tlon-user-lookup :substitute :github designated-next-assignee)))
    (if (string= current-user current-assignee)
	designated-next-assignee
      substitute-next-assigne)))

(defun tlon-jobs-finalize-set-todos ()
  "Set relevant TODO statuses during the finalize process."
  (let ((todo (tlon-get-clock))
	(key (tlon-get-clock-key))
	(current-action (tlon-get-clock-action)))
    (tlon-mark-todo-done todo (tlon-get-todos-jobs-file))
    (when (or (string= current-action "Review") (string= current-action "Check"))
      (let ((parent-todo (tlon-get-parent-todo todo)))
	(tlon-mark-todo-done parent-todo (tlon-get-todos-jobs-file))))
    (when (string= current-action "Review")
      (let ((job-todo (format "[cite:@%s]" key)))
	(tlon-mark-todo-done job-todo (tlon-jobs-get-file))
	(tlon-sort-headings (tlon-jobs-get-file))
	(tlon-commit-and-push "Update"
			      (tlon-jobs-get-file))))))

(defvar tlon-file-babel-manual)
(declare-function org-extras-show-subtree-hide-drawers "org-extras")
(defun tlon-jobs-initialize-processing ()
  "Initialize processing."
  (cl-multiple-value-bind
      (original-path)
      (tlon-set-paths-from-clock)
    (tlon-set-windows original-path tlon-file-babel-manual)
    (org-id-goto tlon-jobs-manual-processing-id)
    (org-narrow-to-subtree)
    (org-extras-show-subtree-hide-drawers)
    (winum-select-window-2)
    (let ((issue (tlon-get-clock-issue)))
      (orgit-topic-open issue))))

(declare-function macos-open-app "macos")
(defun tlon-jobs-initialize-translation ()
  "Initialize translation."
  (macos-open-app "deepl"))

(defun tlon-jobs-initialize-revision ()
  "Initialize stylistic revision."
  (winum-select-window-2)
  (tlon-split-mode))

(declare-function read-aloud-buf "read-aloud")
(declare-function window-extras-buffer-move-right "window-extras")
(declare-function window-extras-switch-to-last-window "window-extras")
(defun tlon-jobs-initialize-check ()
  "Initialize accuracy check."
  ;; we move the buffer displaying the issue to the right, to uncover
  ;; the original file
  (window-extras-buffer-move-right)
  (window-extras-switch-to-last-window)
  ;; (add-hook 'eww-mode-hook #'tlon-tts-mode)
  (markdown-preview)
  (read-aloud-buf))

(declare-function files-extras-switch-to-alternate-buffer "files-extras")
(declare-function jinx--load-dicts "jinx")
(declare-function jinx--cleanup "jinx")
(declare-function tlon-log-buffer-latest-user-commit-ediff "tlon")
(declare-function winum-select-window-1 "winum")
(defun tlon-jobs-initialize-review ()
  "Initialize review."
  (cl-multiple-value-bind
      (_ translation-path)
      (tlon-set-paths-from-clock)
    (tlon-log-buffer-latest-user-commit-ediff translation-path)
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

(declare-function tlon-import-document "tlon-import")
(declare-function ebib-extras-get-field "ebib-extras")
(declare-function ebib-extras-get-file "ebib-extras")

(autoload 'ebib--get-key-at-point "ebib")
(declare-function tlon-get-key-in-buffer "tlon")
(defun tlon-create-record-for-job (&optional key)
  "Create a record based on KEY.
Creates a new record in the repository (with the format `Job: KEY') and a new
heading in the file `jobs.org'. If KEY is not provided, the key in the current
Markdown buffer at point is used."
  (interactive)
  (if-let ((key (or key
		    (pcase major-mode
		      ('markdown-mode (tlon-get-key-in-buffer))
		      ('ebib-entry-mode (ebib--get-key-at-point))))))
      (progn
	(tlon-create-issue-from-key key)
	(tlon-create-heading-for-job key 'commit))
    (user-error "I wasn't able to create a job record because I couldn't find the relevant BibTeX key")))

;;;;; `jobs.org'

(declare-function tlon-metadata-in-repo "tlon-yaml")
(defun tlon-create-heading-for-job (&optional key commit)
  "Create a heading based on BibTeX KEY in `jobs.org'.
If KEY is not provided, the key in the Markdown buffer at point is used. If
COMMIT is non-nil, commit the change."
  (interactive)
  (let* ((key (or key (tlon-get-key-in-buffer)))
	 (heading (format "[cite:@%s]" key))
	 (file (tlon-metadata-lookup (tlon-metadata-in-repo) "file" "original_key" key))
	 (repo (tlon-get-repo-from-file file))
	 (repo-abbrev (tlon-repo-lookup :abbrev :dir repo)))
    (with-current-buffer (or (find-buffer-visiting (tlon-jobs-get-file))
			     (find-file-noselect (tlon-jobs-get-file)))
      (widen)
      (goto-char (point-min))
      (unless (search-forward heading nil t)
	(re-search-forward tlon-jobs-id nil t)
	(while (and (not (org-at-heading-p)) (not (eobp)))
	  (forward-line))
	(org-insert-heading)
	(insert heading)
	(org-todo 'todo)
	(org-set-tags repo-abbrev)
	(tlon-sort-headings (tlon-jobs-get-file))
	(save-buffer)))
    (when commit
      (tlon-commit-and-push "Update" (tlon-jobs-get-file)))))

(defun tlon-jobs-get-key-in-heading ()
  "Get the key of the currently clocked task."
  (unless (org-at-heading-p)
    (user-error "Not in an org-mode heading"))
  (let ((heading (substring-no-properties (org-get-heading t t t t))))
    (if (string-match "\\[cite:@\\(.+?\\)\\]\\|Job: `\\(.+?\\)\\.md`" heading)
	(or (match-string 1 heading)
	    (match-string 2 heading))
      (user-error "I wasn't able to find a key in clocked heading"))))

(autoload 'org-element-map "org-element")
(autoload 'org-element-parse-buffer "org-element")
(autoload 'org-element-property "org-element-ast")
(defun tlon-jobs-goto-heading (key)
  "Move point to the heading in `jobs.org' with KEY."
  (with-current-buffer (or (find-buffer-visiting (tlon-jobs-get-file))
			   (find-file-noselect (tlon-jobs-get-file)))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
	(when (string= (org-element-property :raw-value headline) (format "[cite:@%s]" key))
	  (goto-char (org-element-property :begin headline)))))))

;;;;; section correspondences

(autoload 'citar-select-refs "citar")
(defun tlon-section-correspondence-dwim ()
  "Add a new section correspondence or modify an existing one."
  (interactive)
  (let* ((data (tlon-read-json tlon-file-section-correspondences 'hash-table 'list 'symbol))
	 (selected-key (citar-select-refs)))
    (tlon-section-correspondence-check selected-key)
    (let ((default-value (gethash selected-key data))
	  (new-sectionOriginal (read-string (format "Enter value for key '%s', sectionOriginal: " selected-key)))
	  (new-sectionSpanish (read-string (format "Enter value for key '%s', sectionSpanish: " selected-key)))
	  (new-value (make-hash-table)))
      (puthash "sectionOriginal" new-sectionOriginal new-value)
      (puthash "sectionSpanish" new-sectionSpanish new-value)
      (puthash selected-key
	       (if default-value
		   (append default-value (list new-value))
		 (list new-value))
	       data)
      (with-temp-file tlon-file-section-correspondences
	(insert "{\n")
	(maphash (lambda (k v)
		   (insert (format "  \"%s\": [\n" k))
		   (dolist (item v)
		     (insert (format "    %s,\n" (json-encode item))))
		   (delete-char -2)
		   (insert "\n  ],\n"))
		 data)
	;; Remove last comma
	(goto-char (- (point) 2))
	(delete-char 1)
	(insert "\n}")
	(json-pretty-print (point-min) (point-max))
	(write-file tlon-file-section-correspondences)
	;; (tlon-url-correspondence-commit)
	))))

(declare-function ebib-extras-open-key "ebib-extras")
(declare-function ebib-extras-get-field "ebib-get-field")
(defun tlon-section-correspondence-check (key)
  "Check that selected BibTeX KEY is associated with the original work."
  (save-window-excursion
    (ebib-extras-open-key key)
    (let ((langid (ebib-extras-get-field "langid")))
      (unless (member langid '("english" "american"))
	(unless (y-or-n-p "The BibTeX entry you selected is not in English. In the `section-correspondences.json' file, you should use the BibTeX entry associated with the original work rather than with its translation. Are you sure you want to proceed?")
	  (user-error "Aborted"))))))

;;;;; URL correspondences

(declare-function tlon-edit-json-mapping "tlon-core")
(defun tlon-edit-url-correspondences ()
  "Add or edit a URL correspondence in `tlon-file-url-correspondences`."
  (interactive)
  (tlon-edit-json-mapping tlon-file-url-correspondences "Source URL: " "Target URL: "))

;; TODO: consider adapting `tlon-commit-and-push' instead
(defun tlon-url-correspondence-commit ()
  "Commit modifications in `url-correspondences.json'."
  (let ((default-directory (tlon-repo-lookup :dir :name "es")))
    ;; save all unsaved files in repo
    (magit-save-repository-buffers)
    (call-interactively #'magit-pull-from-upstream nil)
    ;; if there are staged files, we do not commit or push the changes
    (unless (magit-staged-files)
      (tlon-check-branch "main" default-directory)
      (magit-run-git "add" tlon-file-url-correspondences)
      (let ((magit-commit-ask-to-stage nil))
	(magit-commit-create (list "-m" "Update URL correspondences"))))))

(defun tlon-highlight-url-correspondences ()
  "Highlight source URLs in URL correspondences file."
  (interactive)
  ;; Load JSON file
  (let* ((json-data (tlon-read-json tlon-file-url-correspondences 'hash-table 'vector 'symbol))
	 (key-urls (tlon-get-keys json-data))
	 ;; Remove URL prefixes from keys
	 (search-keywords (mapcar (lambda (url)
				    (replace-regexp-in-string "^https?://\\(www\\.\\)?" "" url))
				  key-urls))
	 ;; Build list of keys
	 (keywords-regex (regexp-opt search-keywords 'words))
	 ;; Specify a custom face for highlighting
	 (highlight-face '(:background "#D3FFD2")))

    ;; Remove the previous highlighting
    (with-silent-modifications
      (remove-list-of-text-properties (point-min) (point-max) '(font-lock-face))

      ;; Highlight each occurrence of a key from the JSON file
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward keywords-regex nil t)
	  (add-text-properties (match-beginning 0) (match-end 0)
			       `(font-lock-face ,highlight-face)))))))

;;;;; Misc

(declare-function zotra-extras-add-entry "zotra-extras")
;;;###autoload
(defun tlon-create-bibtex-original ()
  "Add a new original BibTeX entry."
  (interactive)
  (zotra-extras-add-entry nil nil tlon-file-fluid))

(declare-function tlon-yaml-get-key "tlon-yaml")
(declare-function tlon-get-bibtex-key "tlon-yaml")
(declare-function tlon-get-key-at-point "tlon-bib")
(declare-function ebib-extras-get-file-of-key "ebib-extras")
(declare-function bibtex-extras-get-field "bibtex-extras")
(declare-function tlon-get-counterpart-key "tlon-counterpart")
(declare-function tlon-db-move-entry "tlon-db")
(declare-function citar-extras-refresh-bibliography "citar-extras")
;;;###autoload
(defun tlon-move-entries-to-db ()
  "Move entries to the Tlön database."
  (interactive)
  (let* ((key (or (tlon-yaml-get-key "key") (tlon-get-key-at-point) (tlon-get-bibtex-key)))
	 (file (ebib-extras-get-file-of-key key))
	 orig-key trans-key)
    (with-current-buffer (find-file-noselect file)
      (widen)
      (bibtex-search-entry key)
      (apply #'setq (if (bibtex-extras-get-field "translation") trans-key orig-key) key))
    (citar-extras-refresh-bibliography file 'force)
    (if trans-key
	(setq orig-key (tlon-get-counterpart-key key "en"))
      (let ((lang (tlon-select-language 'code 'babel "Translation language: ")))
	(setq trans-key (tlon-get-counterpart-key key lang))))
    (tlon-db-move-entry orig-key)
    (if trans-key
	(tlon-db-move-entry trans-key)
      (message "No translation entry found. Call `M-x tlon-db-move-entry' manually from the translated BibTeX entry."))))

;;;;; Menu

;;;###autoload (autoload 'tlon-jobs-menu "tlon-jobs" nil t)
(transient-define-prefix tlon-jobs-menu ()
  "`jobs' menu."
  :info-manual "(tlon) Jobs"
  [["Job phases"
    ("j" "start or finish phase"                tlon-jobs-start-or-finish-phase)]
   ["Job creation"
    ("1" "add original bibtex entry"            tlon-create-bibtex-original)
    ("2" "add translation bibtex entry"         tlon-create-bibtex-translation)
    ("3" "import original document"             tlon-import-document)
    ("4" "translate original document"          tlon-translate-file)
    ("5" "move entries to db"                   tlon-move-entries-to-db)]
   ["Add or modify"
    ("a s" "section correspondence"             tlon-section-correspondence-dwim)
    ("a u" "URL correspondence"                 tlon-edit-url-correspondences)]
   ["jobs.org"
    ("r" "create record"                        tlon-create-record-for-job)
    ("h" "create heading"                       tlon-create-heading-for-job)
    ("t" "sort headings"                        tlon-sort-headings)]])

(provide 'tlon-jobs)
;;; tlon-jobs.el ends here
