;;; tlon-forg.el --- Forge & org-mode integration -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini

;; Package-Requires: ((forge-extras "0.1"))

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

;; Integration between `forge' and `org-mode'.

;;; Code:

(require 'forge)
(require 'org)
(require 'org-refile)
(require 'org-element-ast)
(require 'ox-md)
(require 'shut-up)
(require 'tlon-core)
(require 'tlon-dispatch)
(require 'forge-extras)
(require 'cl-lib)
(require 'crm)
(require 'seq)
(require 'org-capture)

;;;; Path Helpers

(defun tlon-forg--get-repo-specific-todo-file (issue)
  "Return the full path to the repo-specific Org TODO file for ISSUE.
The file is named REPO-NAME.org inside `paths-dir-tlon-todos'.
Returns nil if `paths-dir-tlon-todos' is not set, not a directory,
or if ISSUE has no repository or repository name."
  (when-let* ((repo (and issue (ignore-errors (forge-get-repository issue)))) ; Ensure issue is valid for repo
	      (repo-name (and repo (oref repo name)))
	      (todos-dir (and (boundp 'paths-dir-tlon-todos)
			      (stringp paths-dir-tlon-todos)
			      (file-directory-p paths-dir-tlon-todos)
			      paths-dir-tlon-todos)))
    (expand-file-name (format "%s.org" repo-name) todos-dir)))

(defun tlon-forg--safe-get-repository (repo-dir)
  "Return the tracked forge repository for REPO-DIR or nil.
Never signals an error – if the directory has not been registered with
forge yet just return nil."
  (let ((default-directory repo-dir))
    (condition-case nil
	(forge-get-repository :tracked)
      (error nil))))

(defun tlon-forg--remote-open-issue-set (repo)
  "Return a hash table keyed by the numbers of *open* issues in REPO.
The information is fetched live from the GitHub REST API, thereby avoiding
Forge’s local cache which might list issues that have been deleted or closed.

Pull requests are filtered out.  The resulting hash table can be queried
efficiently with `gethash'."
  (require 'ghub nil t)
  (let* ((owner    (oref repo owner))
         (name     (oref repo name))
         (per-page 100)
         (numbers  '()))
    (cl-loop for page from 1
             do
             (let* ((resp (ghub-get (format "/repos/%s/%s/issues" owner name)
                                    `((state . "open")
                                      (per_page . ,per-page)
                                      (page . ,page))
                                    :auth 'forge))
                    (count (length resp)))
               ;; Collect pure issues (skip entries that include the `pull_request' key).
               (dolist (it resp)
                 (unless (assoc 'pull_request it)
                   (push (alist-get 'number it) numbers)))
               (when (< count per-page)
                 (cl-return))))
    (let ((ht (make-hash-table :test #'eql)))
      (dolist (n numbers) (puthash n t ht))
      ht)))

(defun tlon-forg--get-repository-from-dir (dir)
  "Return the tracked forge repository for DIR.
This uses a temporary buffer whose `default-directory' is DIR so that
`forge-get-repository' looks only at the directory, instead of falling back to
the repository object associated with whatever Forge buffer happens to be
current."
  (with-temp-buffer
    (let ((default-directory dir))
      (forge-get-repository :tracked))))

;;;; Helper Functions for Repository and Issue Selection

(defun tlon-forg-get-or-select-repository ()
  "Return a `forge-repository' object.
Priority:

1. If current buffer is an Org file named like a known Tlön repo (e.g.,
REPO.org).

2. Current repository via `forge-get-repository :tracked' in
`default-directory'.

3. Prompt from known Tlön repositories via `tlon-get-repo'.

4. List repositories from GitHub via `gh' CLI and prompt.

The selected repository must be locally configured in Tlön for its path to be
found."
  (or
   (tlon-forg--get-repository-from-org-buffer)
   (tlon-forg--get-repository-from-current-directory)
   (tlon-forg--get-repository-from-tlon-config)
   (tlon-forg--get-repository-from-github)))

(defun tlon-forg--get-repository-from-org-buffer ()
  "Try to derive repo from current buffer's filename if it's an Org file."
  (when-let* ((current-file (buffer-file-name))
	      (is-org-file (and current-file (string-suffix-p ".org" current-file))))
    (when is-org-file
      (let* ((base-name (file-name-sans-extension (file-name-nondirectory current-file)))
	     (repo-path (tlon-repo-lookup :dir :name base-name)))
	(when (and repo-path (file-directory-p repo-path))
	  (ignore-errors ; In case forge isn't set up for this repo-path yet
	    (let ((default-directory repo-path))
	      (forge-get-repository :tracked))))))))

(defun tlon-forg--get-repository-from-current-directory ()
  "Try current repository via `forge-get-repository :tracked`."
  (let ((r (ignore-errors (forge-get-repository :tracked))))
    (when (and r
	       ;; accept only if the worktree belongs to a Tlön repo
	       (tlon-get-repo-from-dir (oref r worktree)))
      r)))

(defun tlon-forg--get-repository-from-tlon-config ()
  "Prompt from known Tlön repositories."
  (let ((repo-path (tlon-get-repo nil 'include-all)))
    (when repo-path
      (ignore-errors
	(let ((default-directory repo-path))
	  (forge-get-repository :tracked))))))

(defun tlon-forg--get-repository-from-github ()
  "Fallback to gh list."
  (tlon-message-debug "No repository determined from context or Tlön configuration. Fetching list from GitHub...")
  (let* ((gh-executable (executable-find "gh"))
	 (repo-names
	  (if gh-executable
	      (split-string
	       (shell-command-to-string
		(format "gh repo list %s --json name --jq \".[] | .name\""
			forge-extras-project-owner))
	       "\n" t)
	    (user-error "The 'gh' command-line tool is required but not found")))
	 (selected-repo-name (completing-read "Select repository from GitHub: " repo-names nil t)))
    (when selected-repo-name
      (let ((repo-dir (tlon-repo-lookup :dir :name selected-repo-name)))
	(unless repo-dir
	  (user-error "Repository '%s' not found in local Tlön configuration. Please ensure it's cloned and configured" selected-repo-name))
	(unless (file-directory-p repo-dir)
	  (user-error "Repository directory '%s' for '%s' does not exist" repo-dir selected-repo-name))
	(ignore-errors
	  (let ((default-directory repo-dir))
	    (forge-get-repository :tracked)))))))

(defun tlon-forg-select-issue-from-repo (repo)
  "Prompt the user to select an open issue from REPO.
REPO must be a valid `forge-repository` object.
Ensures topics for REPO are fetched before prompting.
Returns a `forge-issue` object or nil if no issue is selected or on error."
  (when repo
    (let ((default-directory (oref repo worktree))) ; Ensure correct context
      (tlon-message-debug "Fetching issues for %s..." (oref repo name))
      (condition-case err
	  (progn
	    ;; Use forge--pull to synchronously update the repository data, including issues.
	    ;; Passing nil as the callback makes it synchronous.
	    (shut-up (forge--pull repo nil)) ; Pull all data for the repo silently and synchronously.
	    (tlon-message-debug "Fetching issues for %s... done. Please select an issue." (oref repo name))
	    ;; Call the modified tlon-get-issue-number-from-open-issues
	    (let ((selected-issue-number (tlon-get-issue-number-from-open-issues repo)))
	      (if selected-issue-number
		  (let* ((issue-id (caar (forge-sql [:select [id] :from issue
							     :where (and (= repository $s1)
									 (= number $s2))]
						    (oref repo id)
						    selected-issue-number))))
		    (if issue-id
			(forge-get-topic issue-id)
		      (progn
			(message "Error: Could not retrieve details for issue #%s in %s." selected-issue-number (oref repo name))
			nil)))
		nil))) ; No issue number selected, tlon-get-issue-number-from-open-issues returned nil
	((debug error) ; Catch user-error and other errors, provide debug info
	 (message "Error during issue selection/fetching for %s: %s" (oref repo name) err)
	 (message "To debug, try M-x toggle-debug-on-error and run the command again.")
	 nil)))))

;;;; User options

(defgroup tlon-forg ()
  "Integration between `forge' and `org-mode'."
  :group 'tlon)

(defcustom tlon-when-assignee-is-nil 'no-capture
  "What to do when the issue has no assignee.
- `prompt': prompt the user to confirm that it will be assigned to them.
- `change': assign it to the user without prompting for confirmation.
- `warn': capture the issue as is, logging a warning in the `*Messages*' buffer.
- `capture': capture the issue as is, without logging a warning.
- `no-capture', or any other value: do not capture the issue.

The value of this user option can also be set interactively from
`tlon-forg-menu'. When set that way, the value will only persist for the
current session."
  :type '(choice (const :tag "Prompt for confirmation" prompt)
		 (const :tag "Change without prompt" change)
		 (const :tag "Capture with warning" warn)
		 (const :tag "Capture without warning" capture)
		 (const :tag "Do not capture" t))
  :group 'tlon-forg)

(defcustom tlon-when-assignee-is-someone-else 'no-capture
  "What to do when the issue’s assignee is someone else.
- `prompt': prompt the user to confirm that it will be assigned to them.
- `change': assign it to the user without prompting for confirmation.
- `warn': capture the issue as is, logging a warning in the `*Messages*' buffer.
- `capture': capture the issue as is, without logging a warning.
- `no-capture', or any other value: do not capture the issue.

The value of this user option can also be set interactively from
`tlon-forg-menu'. When set that way, the value will only persist for the
current session.

Note that this user option has no effect when the tasks are captured via
`tlon-capture-all-issues'. That command always behaves as if the value of
this variable is `no-capture'."
  :type '(choice (const :tag "Prompt for confirmation" prompt)
		 (const :tag "Change without prompt" change)
		 (const :tag "Capture with warning" warn)
		 (const :tag "Capture without warning" capture)
		 (const :tag "Do not capture" t))
  :group 'tlon-forg)

(defcustom tlon-forg-when-syncing 'prompt
  "What to do when the issue and its associated todo differ.
- `prompt': prompt the user to choose between the issue and the todo.
- `issue': update the todo to match the issue.
- `todo': update the issue to match the todo.

The value of this user option can also be set interactively from
`tlon-forg-menu'. When set that way, the value will only persist for the
current session."
  :type '(choice (const :tag "Prompt for choice" prompt)
		 (const :tag "Update issue" issue)
		 (const :tag "Update todo" todo))
  :group 'tlon-forg)

(defcustom tlon-forg-include-archived nil
  "Whether to include archived issues in capture or sync processes."
  :type 'boolean
  :group 'tlon-forg)

(defcustom tlon-forg-archive-todo-on-close nil
  "Whether to archive the Org TODO item when closing an issue and its TODO.
When non-nil, `tlon-close-issue-and-todo' will archive the corresponding
Org TODO subtree after marking it as DONE."
  :type 'boolean
  :group 'tlon-forg)

(defcustom tlon-forg-sort-after-sync-or-capture nil
  "Whether to sort the buffer after bulk capture or sync operations.
When non-nil, `tlon-capture-all-issues-in-project' and
`tlon-sync-all-issues-in-project' will sort the buffer by status and project
order after completing their operations."
  :type 'boolean
  :group 'tlon-forg)

(defcustom tlon-forg-enforce-user nil
  "If non-nil, the name to be considered that of the current user.
For testing purposes."
  :type 'string
  :group 'tlon-forg)

(defcustom tlon-forg-sync-after-capture-project 'prompt
  "Whether to automatically sync all issues after capturing them.

- t: Always sync after capturing all issues in a project.

- nil: Never sync after capturing all issues in a project.

- `prompt': Prompt the user whether to sync after capturing.

The sync operation will use the cached project item list if available. This
option can be set temporarily via `tlon-forg-menu'."
  :type '(choice (const :tag "Always sync" t)
		 (const :tag "Never sync" nil)
		 (const :tag "Prompt to sync" prompt))
  :group 'tlon-forg)

;;;; Variables

(defconst tlon-todo-statuses
  '(("Todo" . "TODO")
    ("Doing" . "DOING")
    ("Next" . "NEXT")
    ("Waiting" . "WAITING")
    ("Later" . "LATER")
    ("Someday" . "SOMEDAY")
    ("Done" . "DONE"))
  "Alist mapping GitHub Project item statuses (car) to Org TODO keywords (cdr).
The `cdr` values should be present in `org-todo-keywords'.")

(defconst tlon-todo-tags
  '("PendingReview" "Later")
  "List of admissible TODO tags.")

(defconst tlon-forg-sort-by-tags-regexp
  "[[:digit:]]\\{2\\}_[[:digit:]]\\{2\\}")

(defconst tlon-forg-orgit-regexp
  "\\[\\[orgit-topic:\\(?1:[^]]+\\)\\]\\[\\(?2:.*?\\)\\]\\]"
  "Regular expression to match Orgit links in `org-mode' headings.
The first group captures the topic ID, and the second captures the title.")

;;;; Functions

;;;###autoload
(defun tlon-insert-issue-link ()
  "Insert a markdown link to a GitHub issue in a Tlön repository."
  (interactive)
  (forge-extras-insert-issue-markdown-link "tlon-team"))

;;;;; internal helpers for sync

(defun tlon-forg--pandoc-convert (text from to)
  "Convert TEXT between markup formats FROM → TO using pandoc.
FROM and TO are strings accepted by pandoc’s -f/-t switches.
Return TEXT trimmed of trailing newline.
Signal an error if pandoc is not in PATH."
  (unless (executable-find "pandoc")
    (user-error "Pandoc executable not found"))
  (with-temp-buffer
    (insert (or text ""))
    (let ((exit (call-process-region (point-min) (point-max)
				     "pandoc" t t nil
				     "-f" from "-t" to)))
      (when (/= exit 0)
	(user-error "Pandoc conversion failed"))
      (string-trim (buffer-string)))))

(defun tlon-forg-md->org (text)
  "Convert TEXT from Markdown to Org format using pandoc."
  (tlon-forg--pandoc-convert text "markdown" "org"))

(defun tlon-forg-org->md (text)
  "Convert TEXT from Org to Markdown format using `org-export-string-as'."
  (if (or (null text) (string-empty-p text))
      ""
    ;; Convert the string, requesting only the body of the output.
    ;; backend='md, subtreep=nil, info=(list :body-only t)
    (string-trim (org-export-string-as text 'md nil (list :body-only t)))))

(defun tlon-forg--wait-for-issue (number repo-dir &optional forge-repo timeout interval)
  "Return ISSUE NUMBER in REPO-DIR, waiting until it exists locally.
While waiting, repeatedly pulls FORGE-REPO to refresh the database.

TIMEOUT  – seconds to wait (default 10)
INTERVAL – seconds between retries (default 0.5)

Signal a `user-error' if the issue cannot be found in time."
  (let* ((timeout  (or timeout 30))     ; wait up-to 30 s
	 (interval (or interval 1.0))   ; retry every second
	 (forge-repo (or forge-repo
			 (let ((default-directory repo-dir))
			   (forge-get-repository :tracked))))
	 issue)
    (while (and (not issue) (> timeout 0))
      ;; attempt to locate the issue
      (setq issue (tlon-get-issue number repo-dir))
      (unless issue
	;; refresh forge’s local db then retry
	(tlon-forg--pull-sync forge-repo)
	(sleep-for interval)
	(setq timeout (- timeout interval))))
    (or issue
	(user-error "Unable to retrieve newly created issue #%s" number))))

(defun tlon-forg--org-heading-title ()
  "Return the *issue title* encoded in the Org heading at point.
Strips the repo tag, the orgit-link and the “#NNN ” prefix produced by
`tlon-make-todo-name-from-issue'."
  (let* ((el (org-element-at-point))
	 (raw (org-element-property :raw-value el)))
    (let* ((str (string-trim (or raw ""))))
      ;; drop a leading repo tag such as “[repo] ”
      (setq str (replace-regexp-in-string "^\\[[^]]+\\][[:space:]]*" "" str))

      ;; if the headline contains an orgit link, keep only its description
      (when (string-match tlon-forg-orgit-regexp str)
	(setq str (match-string 2 str)))

      ;; discard a leading “#123 ” issue number, if present
      (when (string-match "^#[0-9]+[[:space:]]+\\(.*\\)$" str)
	(setq str (match-string 1 str)))

      str)))

(defun tlon-forg--org-heading-components ()
  "Return plist (:title TITLE :tags TAGS :todo TODO) for the heading at point."
  (let* ((el   (org-element-at-point))
	 (todo (org-element-property :todo-keyword el))
	 (tags (org-element-property :tags el))
	 (title (tlon-forg--org-heading-title)))
    (list :title title :tags tags :todo todo)))

(defun tlon-forg--normalize-tags (raw-tags)
  "Return RAW-TAGS in canonical form or nil.

The canonical form is a list of unique, lowercase, trimmed tag
strings sorted alphabetically.  Empty or nil input yields nil."
  (when raw-tags
    (let* ((cleaned    (mapcar (lambda (tag)
                                 (string-trim (downcase (or tag ""))))
                               raw-tags))
           (non-empty  (cl-remove-if #'string-empty-p cleaned))
           (unique     (delete-dups non-empty)))
      (when unique
        (sort unique #'string<)))))

(defun tlon-forg--valid-tags (tags)
  "Return the down-cased TAGS that are listed in `tlon-todo-tags'."
  (sort (cl-remove-duplicates
	 (cl-remove-if-not
	  (lambda (tag)
	    (member (downcase tag) (mapcar #'downcase tlon-todo-tags)))
	  (mapcar #'downcase tags))
	 :test #'string=)
	#'string<))

(defun tlon-forg--select-with-completing-read (element issue-val todo-val issue-context-string)
  "Prompt via `completing-read' which value of ELEMENT to keep.
Return ?i when user chooses ISSUE value, ?t when they choose TODO value.
ISSUE-VAL and TODO-VAL are the values to be compared, and ELEMENT is a string
describing the element being compared (e.g., \"Titles\").
ISSUE-CONTEXT-STRING provides context about the issue being processed."
  (let* ((choices `((,(format "github issue: %s" issue-val) . ?i)
		    (,(format "org heading:  %s" todo-val) . ?t)))
	 (selection (completing-read
		     (format "For issue '%s':\n%s differ. Choose value to keep: " issue-context-string element)
		     (mapcar #'car choices) nil t)))
    (cdr (assoc selection choices))))

(defun tlon-forg--prompt-element-diff (element issue-val todo-val issue-context-string)
  "Return ?i or ?t according to `tlon-forg-when-syncing' or user choice.
ELEMENT is a string describing the element being compared (e.g., \"Titles\").
ISSUE-VAL and TODO-VAL are the values to be compared.
ISSUE-CONTEXT-STRING provides context about the issue being processed."
  (pcase tlon-forg-when-syncing
    ('issue ?i)
    ('todo  ?t)
    (_ (tlon-forg--select-with-completing-read element issue-val todo-val issue-context-string))))

(defun tlon-forg--sync-title (issue)
  "Reconcile the ISSUE title with the Org heading at point."
  (let* ((issue-title-org (tlon-forg-md->org (oref issue title)))
	 (issue-title (string-trim (replace-regexp-in-string "\n" " " issue-title-org)))
	 (todo-title  (string-trim (tlon-forg--org-heading-title)))
	 (issue-context (tlon-get-issue-name issue)))
    (unless (string= issue-title todo-title)
      (pcase (tlon-forg--prompt-element-diff "Titles" issue-title todo-title issue-context)
	(?i (tlon-update-todo-from-issue (tlon-make-todo-name-from-issue issue)))
	(?t (forge--set-topic-title (forge-get-repository issue) issue
				    (tlon-forg-org->md todo-title)))))))

(defun tlon-forg--sync-status (issue &optional project-item-data)
  "Reconcile the status between ISSUE and the Org heading.
If PROJECT-ITEM-DATA is provided, its :status field is used first, falling
back to a live GraphQL call *only when* the cached value **appears** to be
in‑sync with the Org heading.  This retains the original safety guarantee
while avoiding an unnecessary network round‑trip for every issue, which was
the primary bottleneck when running `tlon-sync-all-issues-in-repo'."
  ;; --- 1. Gather tentative status values ----------------------------
  (let* ((issue-status
          (let ((st (tlon-get-status-in-issue issue nil project-item-data)))
            (when st (upcase st))))
         (todo-status
          (let ((st (org-get-todo-state)))
            (when st (upcase st))))
         (issue-context (tlon-get-issue-name issue)))
    ;; --- 2. *Selective* live re‑validation --------------------------
    ;; Only hit the network when we *think* everything is already in sync.
    (when (and project-item-data (string= issue-status todo-status))
      (setq issue-status
            (let ((st (tlon-get-status-in-issue issue nil nil))) ; unconditional live fetch
              (when st (upcase st)))))
    ;; --- 3. Reconcile -----------------------------------------------
    (unless (string= issue-status todo-status)
      (pcase (tlon-forg--prompt-element-diff
              "Statuses" issue-status todo-status issue-context)
        (?i (org-todo issue-status))
        (?t (tlon-update-issue-from-todo))))))

(defun tlon-forg--sync-tags (issue)
  "Reconcile the tags between ISSUE and the Org heading."
  (let* ((gh-labels-raw (tlon-forg-get-labels issue)) ; Raw labels from GH
	 (org-tags-raw (org-get-tags))               ; Raw tags from Org
	 ;; For comparison, normalize GH tags. For Org tags, use valid tags.
	 (issue-tags-for-compare (tlon-forg--normalize-tags gh-labels-raw))
	 ;; tlon-forg--valid-tags already downcases, sorts, and handles empty/nil.
	 (org-tags-for-compare-and-display (tlon-forg--valid-tags org-tags-raw))
	 (issue-context (tlon-get-issue-name issue)))
    (unless (equal issue-tags-for-compare org-tags-for-compare-and-display)
      (pcase (tlon-forg--prompt-element-diff
	      "Tags"
	      (string-join issue-tags-for-compare ", ") ; Display all GH tags being compared
	      (string-join org-tags-for-compare-and-display ", ") ; Display valid Org tags, consistent with comparison
	      issue-context)
	(?i ;; Update Org TODO from Issue: apply all tags from the issue
	 (org-set-tags (string-join (or gh-labels-raw '()) ":")))
	(?t ;; Update Issue from Org TODO: tlon-update-issue-from-todo handles this.
	 ;; It currently uses raw org tags to set issue labels.
	 (tlon-update-issue-from-todo))))))

(defun tlon-forg--diff-issue-and-todo (issue)
  "Return a list of symbols whose values differ between ISSUE and the Org heading.
Possible symbols are `title', `status' and `tags'."
  (let* ((issue-title-org (tlon-forg-md->org (oref issue title)))
	 (issue-title  (string-trim (replace-regexp-in-string "\n" " " issue-title-org)))
	 (issue-status (let ((st (tlon-get-status-in-issue issue)))
			 (when st (upcase st))))
	 ;; For comparison, use normalized raw tags
	 (issue-tags-for-compare (sort (mapcar #'downcase (tlon-forg-get-labels issue)) #'string<))
	 (org-title    (string-trim (tlon-forg--org-heading-title)))
	 (org-status   (let ((st (org-get-todo-state)))
			 (when st (upcase st))))
	 ;; For comparison, use normalized raw tags
	 (org-tags-for-compare (sort (mapcar #'downcase (org-get-tags)) #'string<))
	 (diff '()))
    (unless (string= issue-title org-title)   (push 'title  diff))
    (unless (string= issue-status org-status) (push 'status diff))
    (unless (equal issue-tags-for-compare org-tags-for-compare) (push 'tags   diff))
    diff))

(defun tlon-forg--report-diff (diff)
  "Return a short human-readable string summarizing DIFF."
  (mapconcat (lambda (sym) (symbol-name sym)) diff ", "))

;;;;; Visit

;; FIXME: this returns nil when called in an issue with no args
(defun tlon-visit-issue (&optional number repo)
  "Visit Github issue.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (interactive)
  (if-let ((issue (tlon-get-issue number repo)))
      (forge-visit-issue issue)
    (if (and number repo)
	(user-error "Could not find or retrieve issue #%s in repository %s" number repo)
      (user-error "Could not find or retrieve issue from current context"))))

(defun tlon-get-issue (&optional number repo)
  "Get Github issue.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (let ((issue-number (or number (tlon-get-issue-number-from-heading)))
	(repo-path (or repo (tlon-get-repo-from-heading))))
    ;; Proceed only if both an issue number and a repository path are found.
    (when (and issue-number repo-path)
      (let ((default-directory repo-path)) ; repo-path is guaranteed to be non-nil here.
	;; Catch errors during Forge operations (e.g., if not a configured Forge repo).
	(condition-case nil
	    (when-let* ((forge-repo (forge-get-repository :tracked)) ; Can return nil if not a forge repo.
			;; Ensure forge-repo is non-nil before trying to use it.
			(issue-id (and forge-repo
				       (caar (forge-sql [:select [id] :from issue
								 :where (and (= repository $s1)
									     (= number $s2))]
							(oref forge-repo id)
							issue-number)))))
	      (when issue-id (forge-get-topic issue-id))) ; Ensure issue-id was found.
	  (error nil)))))) ; If any error occurs, tlon-get-issue returns nil.

(defun tlon-get-issue-buffer (&optional number repo)
  "Get Github issue buffer.
If NUMBER and REPO are nil, follow org link to issue if point is on an `orgit'
link, else get their values from the heading title, if possible."
  (save-window-excursion
    (tlon-visit-issue number repo)
    (current-buffer)))

(defun tlon-visit-todo (&optional pos-file issue)
  "Visit TODO.
If POS-FILE (a cons cell `(POSITION . FILE-PATH)`) is provided, visit TODO at
POSITION in FILE-PATH.
If POS-FILE is nil, ISSUE (a `forge-issue` object) must be provided. The
function will then determine the position and file of the TODO associated with
ISSUE using `tlon-get-todo-position-from-issue`.
If ISSUE is also nil, it defaults to the issue at point."
  (let* ((resolved-pos-file (or pos-file (tlon-get-todo-position-from-issue issue)))
	 (pos (car resolved-pos-file))
	 (file (cdr resolved-pos-file)))
    (if (and pos file)
	(tlon-open-todo file pos)
      (user-error "No TODO found"))))

(defun tlon-open-todo (file position)
  "Open FILE at TODO POSITION."
  (find-file file)
  (widen)
  (org-kill-note-or-show-branches)
  (goto-char position))

(defun tlon-visit-todo-or-capture ()
  "Visit the TODO associated with the current issue, creating one if necessary."
  (if-let ((pos-file (tlon-get-todo-position-from-issue))) ; Returns (POS . FILE)
      (tlon-visit-todo pos-file)
    (tlon-capture-issue)))

;;;;; Capture

;;;###autoload
(defun tlon-capture-issue (&optional issue invoked-from-org-file override-status)
  "Create a new `org-mode' TODO based on ISSUE.
If ISSUE is nil, it attempts to use the issue at point or in the current
buffer. If no issue can be determined from the context, it prompts to select a
repository and then an issue.

INVOKED-FROM-ORG-FILE specifies an Org file path if the command's context
originates from a specific Org file.
OVERRIDE-STATUS, if non-nil, is used as the status string for the new TODO,
bypassing `tlon-get-status-in-issue`.

- For job issues, INVOKED-FROM-ORG-FILE is used for initial capture before
  refiling.
- For non-job issues, TODO is captured to repo-specific Org file or generic
  TODOs file.
If called interactively from an Org buffer and `invoked-from-org-file` is nil,
the current buffer's file is used for job issues' initial capture.
Capture defaults to the template's file if no other target is determined."
  (interactive
   (list nil ; For `issue`
	 (if (and (eq major-mode 'org-mode) (buffer-file-name)) ; For `invoked-from-org-file`
	     (buffer-file-name)
	   nil)
	 nil)) ; For `override-status`
  (let (repo issue-to-capture)
    (if issue
	(setq issue-to-capture issue
	      repo (forge-get-repository issue))
      ;; No explicit issue passed (e.g., interactive call, or programmatic without issue)
      (setq issue-to-capture (ignore-errors (forge-current-topic)))
      (if issue-to-capture
	  (setq repo (forge-get-repository issue-to-capture))
	;; Still no issue (e.g. interactive call from a non-forge, non-org buffer, or org buffer without topic)
	;; `invoked-from-org-file` (the parameter) is already set by interactive spec or caller.
	(setq repo (tlon-forg-get-or-select-repository))
	(unless repo
	  (user-error "Repository selection failed or cancelled. Aborting capture"))
	(setq issue-to-capture (tlon-forg-select-issue-from-repo repo))
	(unless issue-to-capture
	  (user-error "Issue selection failed or cancelled. Aborting capture"))))

    ;; Ensure repo is set if issue-to-capture is set (should be by now)
    (unless repo
      (if issue-to-capture
	  (setq repo (forge-get-repository issue-to-capture))
	(user-error "Cannot determine repository. Aborting capture")))

    ;; Proceed with capture if issue and repo are valid
    (let* ((issue-name (oref issue-to-capture title))
	   (default-directory (oref repo worktree))) ; Set context for subsequent operations
      (when (and (eq (tlon-get-state issue-to-capture) 'open)
		 (tlon-capture-handle-assignee issue-to-capture))
	(tlon-message-debug "Capturing ‘%s’" issue-name)
	(if (tlon-issue-is-job-p issue-to-capture)
	    (tlon-create-job-todo-from-issue issue-to-capture invoked-from-org-file override-status)
	  ;; For non-job issues, determine target file: repo-specific or generic.
	  (let ((preferred-target-file
		 (or (when-let ((rsf (tlon-forg--get-repo-specific-todo-file issue-to-capture)))
		       (and (file-exists-p rsf) rsf))
		     (tlon-get-todos-generic-file))))
	    (tlon-store-todo "tbG" nil issue-to-capture preferred-target-file override-status)))
	;; Sync estimate after capturing the TODO
	(tlon-sync-estimate-from-issue issue-to-capture)))))

;;;###autoload
(defun tlon-capture-all-issues-in-repo (arg)
  "Capture all open issues in a selected repository.
The issues captured are those not assigned to another user. If a repository
cannot be inferred from the current context, the user is prompted to select one.
Before initiating the capture process, this command performs a full pull of the
selected repository to ensure local data reflects its remote state. The window
configuration active before the command was called will be restored after
completion. If any TODOs are captured, the target Org file is opened and
point is moved to the last captured TODO. This command clears the
`org-refile' cache upon completion.

If called with a prefix ARG, the initial pull from forge is omitted."
  (interactive "P")
  (let* ((repo (tlon-forg-get-or-select-repository))
	 (origin-file (if (and (eq major-mode 'org-mode) (buffer-file-name))
			  (buffer-file-name)
			nil)))
    (unless repo
      (user-error "No repository selected or available. Aborting capture-all-issues"))
    ;; Set default-directory for the scope of tlon-pull-silently and its callback
    (let ((default-directory (oref repo worktree)))
      (let ((file-to-open
	     (if arg
		 (tlon-capture-all-issues-in-repo-after-pull repo nil origin-file)
	       (tlon-pull-silently "Pulling issues..."
				   (lambda () (tlon-capture-all-issues-in-repo-after-pull repo nil origin-file))
				   repo))))
	(when (and file-to-open (stringp file-to-open) (file-exists-p file-to-open))
	  (find-file file-to-open)
	  (org-capture-goto-last-stored))))))

;;;###autoload
(defun tlon-capture-all-issues-in-project (arg)
  "Capture all open issues in the GitHub Project.
The issues captured are those not assigned to another user. This command
fetches all issues from the project configured in `forge-extras-project-owner'
and `forge-extras-project-number'.

Before initiating the capture process, this command performs a full pull of
each repository containing issues in the project to ensure local data reflects
its remote state. The window configuration active before the command was called
will be restored after completion. This command clears the `org-refile' cache
upon completion.

If called with a prefix ARG, the initial pull from forge is omitted and the
cached project items list is used instead of fetching a fresh one."
  (interactive "P")
  ;; Inform the user what is happening and how to skip it.
  (unless arg
    (message "Pulling issues from all repositories in project... (use “C-u %s” to skip this pull)"
	     this-command))
  (let* ((original-window-config (current-window-configuration))
	 (project-items (forge-extras-list-project-items-ordered nil nil arg))
	 (issue-repos (make-hash-table :test 'equal))
	 (origin-file (if (and (eq major-mode 'org-mode) (buffer-file-name))
			  (buffer-file-name)
			nil)))

    ;; Group issues by repository
    (dolist (item project-items)
      (when (eq (plist-get item :type) 'issue)
	(let* ((repo-fullname (plist-get item :repo))
	       (repo-parts (split-string repo-fullname "/"))
	       (repo-name (cadr repo-parts))
	       (repo-dir (tlon-repo-lookup :dir :name repo-name)))
	  (when repo-dir
	    (puthash repo-name repo-dir issue-repos)))))
    (if (= (hash-table-count issue-repos) 0)
	(user-error "No repositories with issues found in the project")
      ;; Process each repository
      (let* ((total-repos (hash-table-count issue-repos))
	     (repos-finished 0)
	     (finish
	      (lambda ()
		(setq repos-finished (1+ repos-finished))
		(when (= repos-finished total-repos)
		  ;; restore frame, clear cache, sort, final message
		  (org-refile-cache-clear)
		  (org-refile-cache-clear) ; For capture phase

		  (let ((do-sync (pcase tlon-forg-sync-after-capture-project
				   ('t t)
				   ('nil nil)
				   ('prompt (y-or-n-p "Sync all issues in project after capture? ")))))
		    (when do-sync
		      (message "Proceeding to sync issues in project after capture...")
		      ;; Pass `arg` to tlon-sync-all-issues-in-project to use cached project items.
		      (tlon-sync-all-issues-in-project arg)
		      (message "Sync after capture complete.")))

		  (set-window-configuration original-window-config)
		  (when tlon-forg-sort-after-sync-or-capture
		    (dolist (todos-file (tlon-forg--get-all-todo-files))
		      (when (and todos-file (file-exists-p todos-file))
			(with-current-buffer (find-file-noselect todos-file)
			  (tlon-forg-sort-by-status-and-project-order t)))))
		  (message "Finished capturing issues from %d repositories in project. Subsequent sync (if any) also completed. Refile cache cleared."
			   total-repos)))))
	(maphash
	 (lambda (repo-name repo-dir)
	   (let ((default-directory repo-dir))
	     (let ((forge-repo (tlon-forg--safe-get-repository repo-dir)))
	       (if (null forge-repo)
		   (progn
		     (message "Skipping repository %s: not registered in forge (run `forge-add-repository` there first)" repo-name)
		     (funcall finish))
		 (progn
		   (tlon-message-debug "Processing repository %s..." repo-name)
		   (if arg
		       (progn
			 (tlon-capture-all-issues-in-repo-after-pull forge-repo project-items origin-file)
			 (funcall finish))
		     (tlon-pull-silently
		      (format "Pulling issues from %s..." repo-name)
		      (lambda ()
			(tlon-capture-all-issues-in-repo-after-pull forge-repo project-items origin-file)
			(funcall finish))
		      forge-repo)))))))
	 issue-repos)))))

(defun tlon-pull-silently (&optional message callback repo)
  "Pull all issues from forge for REPO.
If REPO is nil, attempts to determine the current repository. If MESSAGE is
non-nil, display it (if `tlon-debug' is true) while the process is ongoing. If
CALLBACK is non-nil, call it after the process completes. The window
configuration active before the pull started will be restored. Returns the
result of the CALLBACK function, or nil if no callback."
  (when message (tlon-message-debug message))
  (let ((original-window-config (current-window-configuration))
	(forge-repo (or repo (tlon-forg-get-or-select-repository))))
    (unless forge-repo
      (user-error "Cannot determine repository for pull operation"))
    (let ((default-directory (oref forge-repo worktree))) ; Set context for forge--pull
      ;; Perform the forge pull silently and synchronously.
      (shut-up
	(forge--pull forge-repo nil))) ; Pass nil to forge--pull to make it synchronous.
    ;; After the silent pull is complete, execute the callback if provided.
    ;; This ensures the callback runs outside the shut-up context and not from a sentinel.
    (let (callback-result)
      (unwind-protect
	  (when callback
	    (setq callback-result (funcall callback)))
	(set-window-configuration original-window-config))
      callback-result))) ; Return the callback's result

(defun tlon-forg--pull-sync (forge-repo)
  "Run `forge--pull' on FORGE-REPO synchronously and quietly."
  (shut-up (forge--pull forge-repo nil)))   ; nil callback ⇒ wait

(defun tlon-capture-all-issues-in-repo-after-pull (repo &optional project-items invoked-from-org-file)
  "Capture all issues in REPO after `forge-pull' is finished.
REPO must be a valid `forge-repository` object.
If PROJECT-ITEMS is provided, only capture issues that are in the project.
INVOKED-FROM-ORG-FILE is the path to the org file from which the overall
capture operation was initiated, if any.
If any issues are captured, returns the path to the target Org file for
non-job issues if it's valid, otherwise nil."
  (let* ((default-directory (oref repo worktree)) ; Set context
	 (_repo-name (oref repo name)) ; unused variable, kept for context
	 (_owner (oref repo owner)) ; unused variable, kept for context
	 (repo-fullname (format "%s/%s" (oref repo owner) (oref repo name)))
	 (remote-open-set (tlon-forg--remote-open-issue-set repo))
	 (captured-anything-p nil) ; Flag to track if any issue was captured
	 ;; Determine the target file for non-job issues from THIS repo
	 (repo-name-for-file (oref repo name))
	 (safe-todos-dir (when (and (boundp 'paths-dir-tlon-todos)
				    (stringp paths-dir-tlon-todos)
				    (file-directory-p paths-dir-tlon-todos))
			   paths-dir-tlon-todos))
	 (potential-repo-specific-file
	  (when (and repo-name-for-file safe-todos-dir)
	    (expand-file-name (format "%s.org" repo-name-for-file) safe-todos-dir)))
	 (actual-target-file-for-non-jobs
	  (if (and potential-repo-specific-file (file-exists-p potential-repo-specific-file))
	      potential-repo-specific-file
	    (tlon-get-todos-generic-file))))

    (if project-items
	;; Only process issues that are in the project
	(dolist (item project-items)
	  (when (and (eq (plist-get item :type) 'issue)
		     (string= (plist-get item :repo) repo-fullname))
	    (let* ((issue-number (plist-get item :number))
		   (issue-id (caar (forge-sql [:select [id] :from issue
						       :where (and (= repository $s1)
								   (= number $s2))]
					      (oref repo id)
					      issue-number))))
	      (when issue-id
		(let ((issue (forge-get-topic issue-id)))
		  (when (and (gethash (oref issue number) remote-open-set)
		             (not (tlon-get-todo-position-from-issue issue))) ; Capture only if issue is open remotely
		    (tlon-capture-issue issue invoked-from-org-file)
		    (setq captured-anything-p t))))))) ; Mark that we captured something
      ;; Fallback to all issues in repo (original behavior)
      (dolist (issue (tlon-get-issues repo))
	(when (and (gethash (oref issue number) remote-open-set)
	           (not (tlon-get-todo-position-from-issue issue))) ; Capture only if issue is open remotely
	  (tlon-capture-issue issue invoked-from-org-file)
	  (setq captured-anything-p t)))) ; Mark that we captured something

    (org-refile-cache-clear)
    (org-refile-cache-clear)
    (if captured-anything-p
	(progn
	  (tlon-message-debug "Finished capturing issues into %s. Refile cache cleared."
			      (if actual-target-file-for-non-jobs
				  (file-name-nondirectory actual-target-file-for-non-jobs)
				"default location"))
	  ;; Return the file path if it's valid and we captured something
	  (if (and actual-target-file-for-non-jobs (file-exists-p actual-target-file-for-non-jobs))
	      actual-target-file-for-non-jobs
	    nil))
      (progn ; else (not captured-anything-p)
	(tlon-message-debug "No new issues captured for this repository. Refile cache cleared.")
	nil)))) ; Return nil if nothing was captured or target file invalid

;;;###autoload
(defun tlon-capture-all-issues-in-repo-after-pull-by-name (repo-name
                                                           &optional project-items
                                                           invoked-from-org-file)
  "Run `tlon-capture-all-issues-in-repo-after-pull' for repository REPO-NAME.
Interactively, prompt for a repository name among the Tlön-configured
repositories.

PROJECT-ITEMS and INVOKED-FROM-ORG-FILE are forwarded unchanged so this helper
can be used programmatically as a drop-in replacement.

This command can be useful for debugging; unlike
`tlon-capture-all-issues-in-repo', it uses `gh' rather than `forge' to get the
issues."
  (interactive
   (list (completing-read "Repository: "
                          (tlon-repo-lookup-all :name) nil t)))
  (let ((repo-dir (tlon-repo-lookup :dir :name repo-name)))
    (unless repo-dir
      (user-error "Repository '%s' not found in Tlön configuration" repo-name))
    (unless (file-directory-p repo-dir)
      (user-error "Directory '%s' for repository '%s' does not exist"
                  repo-dir repo-name))
    (let ((forge-repo (tlon-forg--safe-get-repository repo-dir)))
      (unless forge-repo
        (user-error "Repository '%s' is not registered with forge" repo-name))
      (tlon-capture-all-issues-in-repo-after-pull
       forge-repo project-items invoked-from-org-file))))

;;;###autoload
(defun tlon-store-todo (template &optional no-action issue target-file override-status)
  "Capture a TODO for ISSUE using `org-capture' TEMPLATE.
TEMPLATE is the key identifying an `org-capture' template. It can be a character
or string understood by `org-capture'.

When NO-ACTION is non-nil, instruct `tlon-make-todo-name-from-issue' to mark the
created headline as a non-action item.

ISSUE is the Forge issue or pull-request object to capture.  If nil, use the
current topic as returned by `forge-current-topic'.

When TARGET-FILE is non-nil, override the template's target location and capture
into the specified Org file instead.

If OVERRIDE-STATUS is non-nil, pass this value to
`tlon-make-todo-name-from-issue' so that it replaces the status component of the
generated headline.

If a TODO for ISSUE already exists the function returns immediately.
The freshly generated headline is also pushed onto the `kill-ring' so
that it can be yanked afterwards."
  (interactive)
  (let* ((issue   (or issue (forge-current-topic))))
    ;; Bail early if the TODO already exists.
    (unless (tlon-get-todo-position-from-issue issue)
      ;; Put the freshly generated TODO text on the kill-ring so the user
      ;; can yank it if desired.
      (kill-new (tlon-make-todo-name-from-issue issue no-action nil override-status))
      ;; Look-up the original template (if any) once – we will need it in
      ;; several branches below.
      (let* ((orig-templates org-capture-templates)
	     (tpl-entry      (assoc template orig-templates))
	     (capture-tpl
	      (cond
	       ;; ──────────────── 1) Caller provided TARGET-FILE ────────────────
	       ((and target-file tpl-entry)
		;; Try to *adapt* existing template, fall back to ad‑hoc.
		(or (tlon-forg--adapt-template-target tpl-entry target-file)
		    (tlon-forg--build-adhoc-template target-file tpl-entry)))
	       ((and target-file (null tpl-entry))
		;; Template key unknown: create an ad‑hoc template from scratch.
		(tlon-forg--build-adhoc-template target-file))
	       ;; ──────────────── 2) No TARGET-FILE ─────────────────────────────
	       (tpl-entry
		;; Re‑use existing template, only guaranteeing :prepend.
		(tlon-forg--ensure-prepend tpl-entry))
	       ;; ──────────────── 3) Ultimate fall‑back ────────────────────────
	       (t nil))))                    ; will call `org-capture' with KEY
	;; Perform the capture.  We shadow `org-capture-templates' *temporarily*
	;; so the global value remains untouched.
	(cond
	 ;; We built a *concrete* template list – push it and call org‑capture
	 ((listp capture-tpl)
	  (let ((org-capture-templates (cons capture-tpl orig-templates)))
	    (org-capture t (car capture-tpl))))
	 ;; No concrete list – simply use the key that the user passed in.
	 (t (org-capture t template)))))))

;;;###autoload
(defun tlon-forg--ensure-prepend (tpl)
  "Return a copy of capture template TPL with `:prepend t' ensured.
This makes the template always insert at the beginning of its target."
  (let ((tpl (copy-sequence tpl)))
    (unless (plist-member (nthcdr 5 tpl) :prepend)
      (nconc tpl '(:prepend t)))
    tpl))

;;;###autoload
(defun tlon-forg--adapt-template-target (template target-file)
  "Attempt to retarget TEMPLATE so that it captures into TARGET-FILE.
When successful, a *new* template list (with `:prepend t' guaranteed)
is returned; otherwise nil is returned so that the caller can fall
back to an ad‑hoc template."
  (let* ((tpl     (copy-sequence template))
	 (target  (nth 3 tpl))
	 (updated (pcase target
		    ;; (file \"~/foo.org\")
		    (`(file ,_)
		     (list 'file+headline target-file (file-name-base target-file)))
		    ;; (file+headline \"~/foo.org\" \"Heading\")
		    (`(file+headline ,_ ,headline)
		     (list 'file+headline target-file headline))
		    ;; (file+olp \"~/foo.org\" \"lvl1\" \"lvl2\" …)
		    (`(file+olp ,_ . ,rest)
		     (cons 'file+olp (cons target-file rest)))
		    (_ nil))))
    (when updated
      (setf (nth 3 tpl) updated)
      (tlon-forg--ensure-prepend tpl))))

(defun tlon-forg--build-adhoc-template (target-file &optional original-tpl)
  "Create a minimal, self‑contained capture template for TARGET-FILE.
Values from ORIGINAL-TPL (description, type, template string) are reused
when available so the user experience stays familiar."
  (let* ((heading       (file-name-base target-file))
	 (description   (or (nth 1 original-tpl)
			    (format "Tlon ad‑hoc capture to %s"
				    (file-name-nondirectory target-file))))
	 (entry-type    (or (nth 2 original-tpl) 'entry))
	 (template-text (or (nth 4 original-tpl) "* TODO %c\n%i")))
    (tlon-forg--ensure-prepend
     (list "tlon-adhoc"               ; key
	   description
	   entry-type
	   (list 'file+headline target-file heading)
	   template-text
	   :immediate-finish t
	   :unnarrowed       t))))

;;;;;; Handling assignee, status, phase

(defun tlon-capture-handle-assignee (issue)
  "Take appropriate action when the user is not the assignee of ISSUE.
The appropriate action is determined by the value of
`tlon-when-assignee-is-nil' and
`tlon-when-assignee-is-someone-else'."
  (let ((capture-p t))
    (unless (tlon-assignee-is-current-user-p issue)
      (let* ((title (oref issue title))
	     (assignee (tlon-get-assignee issue))
	     (cond (if assignee
		       tlon-when-assignee-is-someone-else
		     tlon-when-assignee-is-nil))
	     (warning (if assignee
			  (format "Warning: the assignee of issue `%s' is %s." title assignee)
			(format "Warning: issue `%s' has no assignee." title))))
	(pcase cond
	  ('prompt
	   (if (y-or-n-p (concat warning " Assign to you? "))
	       (tlon-forg-change-assignee issue)
	     (setq capture-p nil)))
	  ('change (tlon-forg-change-assignee issue))
	  ('warn (message warning))
	  ('capture nil)
	  (_ (setq capture-p nil)))))
    capture-p))

(defun tlon-forg-change-assignee (&optional issue)
  "Change the assignee of ISSUE to the current user.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (tlon-set-assignee (tlon-user-lookup :github :name user-full-name) issue)
    (while (not (tlon-assignee-is-current-user-p issue))
      (tlon-pull-silently "Changing assignee...")
      (sleep-for 0.1))))

(defun tlon-capture-handle-phase (issue)
  "Take appropriate action when ISSUE has no valid job phase."
  (unless (tlon-get-phase-in-issue issue)
    (let* ((title (oref issue title))
	   (prompt (format "Warning: issue `%s' has no valid job phase. What should it be?" title))
	   (phase (completing-read prompt (tlon-label-lookup-all :label))))
      (tlon-set-labels `(,phase) 'phase issue))))

;;;;;; Capture jobs

(defun tlon-issue-is-job-p (&optional issue)
  "Return t if ISSUE is a job.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (when (string-match-p "^Job: " (oref issue title))
      t)))

(defun tlon-create-job-todo-from-issue (&optional issue invoked-from-org-file override-status)
  "Create a new `org-mode' job TODO based on ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer.
INVOKED-FROM-ORG-FILE is the file path if called from an Org buffer.
OVERRIDE-STATUS is passed to `tlon-store-or-refile-job-todo'."
  (let ((issue (or issue (forge-current-topic))))
    (tlon-capture-handle-phase issue)
    (tlon-store-or-refile-job-todo issue invoked-from-org-file override-status)))

(defun tlon-store-master-job-todo (&optional set-issue issue _invoked-from-org-file)
  "Create a new job master TODO.
If SET-ISSUE is non-nil, set issue label to `Awaiting processing' and assignee
to the current user. If ISSUE is non-nil, use the issue at point or in the
current buffer. _INVOKED-FROM-ORG-FILE is passed down but not used for
the master TODO itself, which always goes to the jobs file."
  (let* ((issue (or issue (forge-current-topic)))
	 (todo (tlon-make-todo-name-from-issue issue 'no-action 'no-status))
	 (jobs-file (tlon-get-todos-jobs-file)))
    (if-let ((pos (tlon-get-todo-position todo jobs-file)))
	(tlon-visit-todo (cons pos jobs-file))
      (save-window-excursion
	(when set-issue
	  (tlon-set-initial-label-and-assignee))
	;; Master TODOs are always created in the central jobs file,
	;; so invoked-from-org-file is intentionally not passed to this tlon-store-todo call.
	(tlon-store-todo "tbJ" 'master-todo issue)))))

(autoload 'org-extras-refile-goto-latest "org-extras")
(autoload 'org-extras-refile-at-position "org-extras")
(defun tlon-store-or-refile-job-todo (&optional issue invoked-from-org-file override-status)
  "Refile TODO under appropriate heading, or create new master TODO if none exists.
If ISSUE is nil, use the issue at point or in the current buffer.
INVOKED-FROM-ORG-FILE is the file path if called from an Org buffer, used for
initial capture before refiling.
OVERRIDE-STATUS is passed to `tlon-store-todo'."
  (if-let* ((issue (or issue (forge-current-topic)))
	    (pos (tlon-get-todo-position
		  (tlon-make-todo-name-from-issue issue 'no-action 'no-status)
		  (tlon-get-todos-jobs-file)))) ; Master heading is sought in jobs file
      (save-window-excursion
	;; Initial capture respects invoked-from-org-file
	(tlon-store-todo "tbJ" nil issue invoked-from-org-file override-status)
	;; Refile to the jobs file under the master heading
	(let* ((inhibit-message t))
	  (org-extras-refile-at-position pos)
	  (org-extras-refile-goto-latest)))
    (when (y-or-n-p (format "No master TODO found for issue `%s'. Create?" (oref issue title)))
      ;; Pass invoked-from-org-file for consistency, though tlon-store-master-job-todo
      ;; will ignore it for placing the master TODO itself.
      (tlon-store-master-job-todo nil issue invoked-from-org-file)
      ;; This recursive call to tlon-capture-issue must pass invoked-from-org-file.
      (tlon-capture-issue issue invoked-from-org-file))))

;;;;; Sync

;;;###autoload
(defun tlon-sync-issue-and-todo ()
  "With point on either, sync issue and associated TODO."
  (interactive)
  (tlon-todo-issue-funcall
   (lambda ()
     (with-current-buffer (tlon-get-issue-buffer)
       (tlon-sync-issue-and-todo-from-issue)))
   #'tlon-sync-issue-and-todo-from-issue))

;;;###autoload
(defun tlon-sync-all-issues-in-repo (arg)
  "Sync all TODOs with their issues in a selected repository.
Before initiating the synchronization process, this command performs a full pull
of the selected repository to ensure that local data reflects the remote state.
If a repository cannot be inferred from the current context, the user is
prompted to select one. The window configuration active before the command was
called will be restored after completion. This command clears the `org-refile'
cache upon completion.

If called with a prefix ARG, the initial pull from forge is omitted."
  (interactive "P")
  (let ((repo (tlon-forg-get-or-select-repository)))
    (unless repo
      (user-error "No repository selected or available. Aborting sync-all-issues"))
    ;; Set default-directory for the scope of tlon-pull-silently and its callback
    (let ((default-directory (oref repo worktree)))
      (if arg
	  (tlon-sync-all-issues-in-repo-after-pull repo)
	(tlon-pull-silently "Pulling issues..."
			    (lambda () (tlon-sync-all-issues-in-repo-after-pull repo))
			    repo)))))

;;;###autoload
(defun tlon-sync-all-issues-in-project (arg)
  "Sync all TODOs with their issues in the GitHub Project.
This command fetches all issues from the project configured in
`forge-extras-project-owner' and `forge-extras-project-number'.

Before initiating the synchronization process, this command performs a full pull
of each repository containing issues in the project to ensure local data
reflects its remote state. The window configuration active before the command
was called will be restored after completion. This command clears the
`org-refile' cache upon completion.

If called with a prefix ARG, the initial pull from forge is omitted and the
cached project items list is used instead of fetching a fresh one."
  (interactive "P")
  (let* ((original-window-config (current-window-configuration))
	 (project-items (forge-extras-list-project-items-ordered nil nil arg))
	 (issue-repos (make-hash-table :test 'equal)))
    ;; Group issues by repository
    (dolist (item project-items)
      (when (eq (plist-get item :type) 'issue)
	(let* ((repo-fullname (plist-get item :repo))
	       (repo-parts (split-string repo-fullname "/"))
	       (repo-name (cadr repo-parts))
	       (repo-dir (tlon-repo-lookup :dir :name repo-name)))
	  (when repo-dir
	    (puthash repo-name repo-dir issue-repos)))))
    (if (= (hash-table-count issue-repos) 0)
	(user-error "No repositories with issues found in the project")
      ;; Process each repository
      (let* ((total-repos (hash-table-count issue-repos))
	     (repos-finished 0)
	     (finish
	      (lambda ()
		(setq repos-finished (1+ repos-finished))
		(when (= repos-finished total-repos)
		  ;; restore frame, clear cache, sort, final message
		  (org-refile-cache-clear)
		  (set-window-configuration original-window-config)
		  (when tlon-forg-sort-after-sync-or-capture
		    (dolist (todos-file (tlon-forg--get-all-todo-files))
		      (when (and todos-file (file-exists-p todos-file))
			(with-current-buffer (find-file-noselect todos-file)
			  (tlon-forg-sort-by-status-and-project-order t)))))
		  (message "Finished syncing issues from %d repositories in project. Refile cache cleared."
			   total-repos)))))
	(maphash
	 (lambda (repo-name repo-dir)
	   (let ((default-directory repo-dir))
	     (let ((forge-repo (tlon-forg--safe-get-repository repo-dir)))
	       (if (null forge-repo)
		   (progn
		     (message "Skipping repository %s: not registered in forge (run `forge-add-repository` there first)" repo-name)
		     (funcall finish))
		 (progn
		   (message "Processing repository %s..." repo-name)
		   (if arg
		       (progn
			 (tlon-sync-all-issues-in-repo-after-pull forge-repo project-items)
			 (funcall finish))
		     (tlon-pull-silently
		      (format "Pulling issues from %s..." repo-name)
		      (lambda ()
			(tlon-sync-all-issues-in-repo-after-pull forge-repo project-items)
			(funcall finish))
		      forge-repo)))))))
	 issue-repos)))))

(defun tlon-forg--get-all-todo-files ()
  "Return a list of all Org TODO files to be processed.
This includes the generic file, the jobs file, and all *.org files
in `paths-dir-tlon-todos'."
  (let ((files (list (tlon-get-todos-generic-file)
		     (tlon-get-todos-jobs-file))))
    (when (and (boundp 'paths-dir-tlon-todos)
	       (stringp paths-dir-tlon-todos)
	       (file-directory-p paths-dir-tlon-todos))
      (setq files (append files (directory-files paths-dir-tlon-todos t "\\.org$" t)))) ; t for full path
    ;; Ensure absolute paths and remove duplicates
    (delete-dups (mapcar #'expand-file-name files))))

(defun tlon-sync-all-issues-in-repo-after-pull (repo &optional all-project-items)
  "Sync TODOs with their issues in REPO after `forge-pull' is finished.
REPO must be a valid `forge-repository` object.
If ALL-PROJECT-ITEMS (a list of plists representing all items in a GitHub
Project) is provided, it's filtered for items belonging to REPO. Otherwise,
project item data (status, estimate) for all issues in the configured GitHub
Project is fetched once using `forge-extras-list-project-items-ordered` and
then filtered for REPO.
This collected project data is then used to sync each local issue in REPO,
passing specific item data to `tlon-sync-issue-and-todo-from-issue` to
avoid per-issue API calls for status and estimate."
  (let* ((default-directory (oref repo worktree)) ; Set context
	 (repo-name (oref repo name))
	 (repo-fullname (format "%s/%s" (oref repo owner) (oref repo name)))
	 (issue-count 0)
	 (project-data-map (make-hash-table :test 'eql))) ; Keyed by issue number

    ;; Helper to ensure keys are always numeric
    (cl-labels ((tlon--put-project-item (item)
		  (let ((num (plist-get item :number)))
		    (puthash (if (stringp num) (string-to-number num) num)
			     item
			     project-data-map))))
      ;; 1. Populate project-data-map with status and estimate for issues in this repo
      (if all-project-items
	  (progn
	    (tlon-message-debug "Using provided project items data for repository %s..." repo-name)
	    (dolist (item all-project-items)
	      (when (and (eq (plist-get item :type) 'issue)
			 (string= (plist-get item :repo) repo-fullname))
		(tlon--put-project-item item))))
	(progn
	  (tlon-message-debug "Fetching project item data for repository %s via all-project fetch..." repo-name)
	  ;; Fetch all project items for the configured project and then filter
	  ;; The third arg 't' to forge-extras-list-project-items-ordered means use cache if available.
	  ;; This is appropriate as tlon-sync-all-issues-in-repo might be called after tlon-sync-all-issues-in-project.
	  (let ((all-items (forge-extras-list-project-items-ordered nil nil t)))
	    (dolist (item all-items)
	      (when (and (eq (plist-get item :type) 'issue)
			 (string= (plist-get item :repo) repo-fullname))
		(tlon--put-project-item item)))))))
    (tlon-message-debug "Populated project data for %d issues in %s."
			(hash-table-count project-data-map) repo-name)
    ;; 2. Iterate through local issues and sync them
    (let ((local-issues (tlon-get-issues repo)))
      (dolist (issue local-issues)
	(when-let ((pos-file (tlon-get-todo-position-from-issue issue)))
	  (with-current-buffer (find-file-noselect (cdr pos-file))
	    (save-excursion
	      (goto-char (car pos-file))
	      (when (or (not (member org-archive-tag (org-get-tags)))
			tlon-forg-include-archived)
		(tlon-message-debug "Syncing issue #%d in repository %s..."
				    (oref issue number) repo-name)
		;; Get the specific project item data for this issue
		(let ((project-item-data (gethash (oref issue number) project-data-map)))
		  (tlon-sync-issue-and-todo-from-issue issue project-item-data)
		  (setq issue-count (1+ issue-count)))))))))

    (org-refile-cache-clear)
    (tlon-message-debug "Finished syncing %d issues in repository %s. Refile cache cleared."
			issue-count repo-name)))

(defun tlon-sync-issue-and-todo-from-issue (&optional issue project-item-data)
  "Sync ISSUE and associated TODO (name, status, tags, and estimate).
If PROJECT-ITEM-DATA is provided, it is passed to helper functions for
determining status and estimate, avoiding per-issue API calls for these.
If ISSUE is nil, use the issue at point."
  (if-let* ((issue (or issue (forge-current-topic))))
      (let* ((pos-file-pair (tlon-get-todo-position-from-issue issue)))
	(if (not pos-file-pair)
	    (message "No TODO found for issue ‘%s’." (oref issue title))
	  ;; visit the todo heading, compute differences, then act
	  (save-window-excursion
	    (tlon-visit-todo pos-file-pair)
	    ;; element-by-element reconciliation
	    (tlon-forg--sync-title  issue)
	    (tlon-forg--sync-status issue project-item-data)
	    (tlon-forg--sync-tags   issue)))
	;; estimates are handled separately
	(tlon-sync-estimate-from-issue issue project-item-data))
    (user-error "No issue found to sync")))

(defun tlon-sync-issue-and-todo-prompt (issue-name todo-name diff)
  "Prompt the user to reconcile DIFF between ISSUE-NAME and TODO-NAME."
  (let ((choice (pcase tlon-forg-when-syncing
		  ('prompt
		   (read-char-choice
		    (format "The issue and TODO differ (%s). Keep (i)ssue or (t)odo?\n\nissue: `%s'\ntodo:  `%s'"
			    (tlon-forg--report-diff diff)
			    issue-name todo-name)
		    '(?i ?t)))
		  ('issue ?i)
		  ('todo  ?t))))
    (pcase choice
      (?i (tlon-update-todo-from-issue issue-name))
      (?t (tlon-update-issue-from-todo)) ; No longer needs todo-name argument
      (_ (user-error "Aborted")))))

(defun tlon-forg--set-github-project-estimate (issue estimate-value)
  "Set GitHub Project estimate for ISSUE to ESTIMATE-VALUE.
ISSUE is a forge-topic object. ESTIMATE-VALUE is a float."
  (forge-extras-set-project-estimate issue estimate-value))

(defun tlon-forg--set-github-project-status (issue org-status-keyword)
  "Set GitHub Project status for ISSUE to ORG-STATUS-KEYWORD.
ORG-STATUS-KEYWORD is the Org TODO keyword (e.g., \"DOING\")."
  (let* ((target-status (car (cl-rassoc org-status-keyword tlon-todo-statuses
					:test #'string=))))
    (unless target-status
      (user-error "Cannot map Org status %s to a project status" org-status-keyword))
    (forge-extras-set-project-status issue target-status)))

(defun tlon-forg--org-effort-to-hours (str)
  "Convert Org Effort string STR to float hours.
If STR is a plain number assume it is already in hours, otherwise
use `org-duration-to-minutes' and convert minutes → hours."
  (when str
    (let ((s (string-trim str)))
      (if (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" s)
	  (string-to-number s)                ; pure number ⇒ hours
	(/ (float (org-duration-to-minutes s)) 60.0)))))

(defun tlon-forg--set-org-effort (effort-hours)
  "Set Org effort property from EFFORT-HOURS (float)."
  (let ((effort-str (format "%g" effort-hours)))
    (org-entry-put nil "Effort" effort-str)
    (message "Org Effort property updated to %s." effort-str)))

(defun tlon-sync-estimate-from-issue (&optional issue project-item-data)
  "Sync estimate for ISSUE and its associated TODO.
If PROJECT-ITEM-DATA (a plist containing :estimate) is provided, its :estimate
field (float hours) is used for the GitHub estimate. Otherwise, project fields
are fetched via `forge-extras-gh-get-issue-fields`.
If ISSUE is nil, use the issue at point."
  (let ((issue (or issue (forge-current-topic))))
    (unless issue
      (user-error "No issue found to sync estimate for"))
    (let* ((repo (forge-get-repository issue))
	   (pos-file-pair (tlon-get-todo-position-from-issue issue)))
      (if pos-file-pair
	  (save-window-excursion
	    (tlon-visit-todo pos-file-pair)
	    (let ((default-directory (oref repo worktree))) ; For tlon-forg--set-github-project-estimate
	      (let* ((issue-number (oref issue number))
		     (gh-estimate-hours
		      (if project-item-data
			  (plist-get project-item-data :estimate)
			;; Fallback to fetching if project-item-data is not provided
			(when-let* ((repo-name (oref repo name))
				    (gh-fields (condition-case err
						   (forge-extras-gh-get-issue-fields issue-number repo-name)
						 (error (progn
							  (when tlon-debug (message "Error fetching GH fields for #%s: %s" issue-number err))
							  nil))))
				    (parsed-gh-fields (if gh-fields (forge-extras-gh-parse-issue-fields gh-fields) nil)))
			  (plist-get parsed-gh-fields :effort))))
		     ;; Org Effort (float hours) - This part is Org-specific, doesn't need repo context
		     (org-effort-str (org-entry-get nil "Effort" 'inherit))
		     (org-effort-hours (tlon-forg--org-effort-to-hours org-effort-str))
		     (epsilon 0.01) ; Tolerance for float comparison
		     (gh-estimate-significant-p (and gh-estimate-hours (> gh-estimate-hours epsilon)))
		     (org-effort-significant-p (and org-effort-hours (> org-effort-hours epsilon))))
		(cond
		 ;; Case 1: Only GitHub has a significant estimate, Org has none or zero.
		 ((and gh-estimate-significant-p (not org-effort-significant-p))
		  (tlon-message-debug "GitHub issue #%s has estimate %s, Org TODO has none. Updating Org TODO."
				      issue-number gh-estimate-hours)
		  (tlon-forg--set-org-effort gh-estimate-hours))
		 ;; Case 2: Only Org has a significant estimate, GitHub has none or zero.
		 ((and org-effort-significant-p (not gh-estimate-significant-p))
		  (tlon-message-debug "Org TODO has effort %s (%s hours), GitHub issue #%s has none. Updating GitHub issue."
				      org-effort-str org-effort-hours issue-number)
		  (tlon-forg--set-github-project-estimate issue org-effort-hours))
		 ;; Case 3: Both have significant estimates, and they differ.
		 ((and gh-estimate-significant-p org-effort-significant-p
		       (> (abs (- gh-estimate-hours org-effort-hours)) epsilon))
		  (let* ((issue-context (tlon-get-issue-name issue))
			 (choice (tlon-forg--prompt-element-diff
				  "Estimates"
				  (format "%g" gh-estimate-hours)
				  (format "%g" org-effort-hours)
				  issue-context)))
		    (pcase choice
		      (?i (tlon-message-debug "Updating Org TODO to match GitHub estimate.")
			  (tlon-forg--set-org-effort gh-estimate-hours))
		      (?t (tlon-message-debug "Updating GitHub issue to match Org TODO estimate.")
			  (tlon-forg--set-github-project-estimate issue org-effort-hours)))))
		 ;; Case 4: estimates already match – nothing to do, stay silent
		 (t nil)))))
	(message "No TODO found for issue %s to sync estimate." (oref issue title))))))

(defun tlon-update-todo-from-issue (full-new-heading-text)
  "Update TODO to match FULL-NEW-HEADING-TEXT.
FULL-NEW-HEADING-TEXT is the complete heading string after the stars,
including the new TODO keyword, title, and tags.
Preserves existing priority cookie."
  (let ((original-visual-line-mode visual-line-mode))
    (visual-line-mode -1) ; Ensure correct point motion
    (save-window-excursion ; Preserve point for the caller
      (goto-char (org-entry-beginning-position)) ; Ensure we are at the correct headline
      (org-fold-show-subtree) ; Ensure headline is visible for modification

      (let (new-keyword new-title-material)
	(if (string-match "^\\(\\S-\+\\)\\s-+\\(.*\\)$" full-new-heading-text)
	    (progn
	      (setq new-keyword (match-string 1 full-new-heading-text))
	      (setq new-title-material (match-string 2 full-new-heading-text)))
	  ;; Fallback if full-new-heading-text doesn't contain a keyword.
	  ;; Current callers (like tlon-make-todo-name-from-issue) should always provide one.
	  (progn
	    (setq new-keyword (or (nth 1 (org-heading-components)) "TODO")) ; Keep current or default
	    (setq new-title-material full-new-heading-text)))

	;; Set the keyword first. This preserves the priority cookie.
	(org-todo new-keyword)

	;; Now, replace the old title material (after keyword and priority)
	;; with new-title-material.
	(beginning-of-line)
	;; Move point past "** KEYWORD "
	(unless (re-search-forward "^\\*+ +\\S-+\\s-+" nil t)
	  (user-error "Could not find keyword in headline after org-todo"))

	;; Move point past priority cookie, if it exists
	(let ((priority-cookie-string (nth 2 (org-heading-components))))
	  (when priority-cookie-string
	    (unless (re-search-forward (regexp-quote priority-cookie-string) nil t)
	      ;; This error implies inconsistency if priority was expected but not found
	      (message "Warning: Priority cookie '%s' expected but not found after keyword" priority-cookie-string))
	    ;; Move past any space after priority cookie
	    (re-search-forward "\\s-*" nil t)))

	;; Point is now at the start of the old title material.
	;; Delete from point to end of line (old title and tags)
	(delete-region (point) (line-end-position))
	;; Insert the new title material
	(insert new-title-material)

	(when tlon-debug (message "TODO updated to: %s"
				  (org-get-heading t t nil nil))))) ; Log the full new heading
    (visual-line-mode original-visual-line-mode)))

(defun tlon-update-issue-from-todo ()
  "Update the GitHub issue to match the Org TODO heading at point.
This includes title, labels (tags), and project status.
Uses functions from `forge-extras.el` for GitHub Project interactions."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point is not on an Org heading"))
  (let* ((issue (tlon-get-issue)))
    (unless issue
      (user-error "Could not find a corresponding GitHub issue for this TODO"))
    (let* ((repo              (forge-get-repository issue))
	   (default-directory (oref repo worktree))   ; run GH calls from repo
	   (org-info          (tlon-forg--org-heading-components))
	   (org-title         (plist-get org-info :title))
	   (org-tags          (plist-get org-info :tags))
	   (org-todo-keyword  (plist-get org-info :todo))
	   (issue-number      (oref issue number))
	   (repo-name         (oref repo name))
	   (gh-fields         (ignore-errors
				(forge-extras-gh-parse-issue-fields
				 (forge-extras-gh-get-issue-fields
				  issue-number repo-name))))
	   (issue-node-id     (plist-get gh-fields :issue-node-id))
	   (project-item-id   (plist-get gh-fields :project-item-id))
	   (current-title     (oref issue title))
	   (current-labels    (sort (copy-sequence
				     (tlon-forg-get-labels issue)) #'string<))
	   (current-status    (plist-get gh-fields :status)))
      (unless issue-node-id
	(user-error "Cannot retrieve project data for issue #%s" issue-number))
      ;; 1. title
      (unless (string= org-title current-title)
	(tlon-message-debug "Updating issue title from '%s' to '%s'" current-title org-title)
	(forge--set-topic-title repo issue (tlon-forg-org->md org-title)))
      ;; 2. labels
      (let ((wanted-labels (sort (copy-sequence org-tags) #'string<)))
	(unless (equal wanted-labels current-labels)
	  (tlon-message-debug "Updating issue labels from %s to %s"
			      current-labels wanted-labels)
	  (forge--set-topic-labels repo issue wanted-labels)))
      ;; 3. project status
      (when org-todo-keyword
	(let* ((target-status (car (cl-rassoc org-todo-keyword tlon-todo-statuses
					      :test #'string=))))
	  (cond
	   ((null target-status)
	    (tlon-message-debug "TODO keyword '%s' not mapped to a project status; skipping"
				org-todo-keyword))
	   ((string= target-status current-status) nil)   ; already in sync
	   (t
	    (let* ((option-id (cdr (assoc target-status
					  forge-extras-status-option-ids-alist
					  #'string-equal)))) ; Use case-insensitive comparison
	      (if option-id
		  ;; Option ID found, proceed with update
		  (if project-item-id
		      ;; already in project → just update status
		      (forge-extras-gh-update-project-item-status-field
		       forge-extras-project-node-id project-item-id
		       forge-extras-status-field-node-id option-id)
		    ;; not in project → ask to add & then set status
		    (when (y-or-n-p
			   (format "Issue #%s is not in Project %s.  Add it and set status to '%s'? "
				   issue-number forge-extras-project-number target-status))
		      (let ((new-item-id
			     (forge-extras-gh-add-issue-to-project
			      forge-extras-project-node-id issue-node-id)))
			(when new-item-id
			  (forge-extras-gh-update-project-item-status-field
			   forge-extras-project-node-id new-item-id
			   forge-extras-status-field-node-id option-id)))))
		;; Option ID not found
		(tlon-message-debug "Cannot find option id for status '%s'; skipping update of project status."
				    target-status)))))))
      (message "Issue “%s” (#%s) updated successfully." org-title issue-number))))

;;;;; Files

(defvar tlon-todos-jobs-file)
(defun tlon-get-todos-jobs-file ()
  "Get the file containing the jobs `org-mode' ID."
  (tlon-get-or-set-org-var 'tlon-todos-jobs-file
			   paths-tlon-todos-jobs-id))

(defvar tlon-todos-generic-file)
(defun tlon-get-todos-generic-file ()
  "Get the file containing the generic `org-mode' ID."
  (tlon-get-or-set-org-var 'tlon-todos-generic-file
			   paths-tlon-todos-generic-id))

(defun tlon-get-todo-position (string file &optional substring)
  "Return the position of STRING exactly matching TODO heading in FILE.
If SUBSTRING is non-nil, return the position of string matching a TODO heading
substring."
  (if substring
      (tlon-find-headline-substring-in-file string file)
    (org-find-exact-headline-in-buffer string (find-file-noselect file))))

(defun tlon-find-headline-substring-in-file (todo file)
  "Move point to TODO in FILE matching TODO."
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\*+.*" todo) nil t)
	(point)))))

(defun tlon-get-todos-file-from-issue (&optional issue)
  "Get the file where the current issue is or would be stored.
If ISSUE is nil, default to the issue at point."
  (if (tlon-issue-is-job-p issue)
      (tlon-get-todos-jobs-file)
    (tlon-get-todos-generic-file)))

(defun tlon-get-todo-position-from-issue (&optional issue)
  "Get the TODO position and file of ISSUE.
Return (POSITION . FILE) or nil. If the issue is a job, search for its heading
name in the jobs file. Else (generic issue), search first in the repo-specific
file (REPO-NAME.org in `paths-dir-tlon-todos'), then in the generic todos file,
using the `orgit-topic' ID as a substring. If ISSUE is nil, use the issue at
point."
  (when-let ((issue (or issue (forge-current-topic))))
    (if (tlon-issue-is-job-p issue)
	(let ((file (tlon-get-todos-jobs-file)))
	  (when-let ((pos (tlon-get-todo-position
			   (tlon-make-todo-name-from-issue issue nil 'no-status)
			   file)))
	    (cons pos file)))
      (let* ((search-string (format "\\[orgit-topic:%s\\]" (oref issue id)))
	     (repo-specific-file (tlon-forg--get-repo-specific-todo-file issue))
	     (pos-in-repo-file (and repo-specific-file
				    (file-exists-p repo-specific-file)
				    (tlon-get-todo-position search-string repo-specific-file 'substring)))
	     (generic-file (tlon-get-todos-generic-file))
	     (pos-in-generic-file (unless pos-in-repo-file ; Only search generic if not in repo-specific
				    (and generic-file ; Ensure generic file path is valid
					 (file-exists-p generic-file)
					 (tlon-get-todo-position search-string generic-file 'substring)))))
	(cond (pos-in-repo-file (cons pos-in-repo-file repo-specific-file))
	      (pos-in-generic-file (cons pos-in-generic-file generic-file))
	      (t nil))))))

;;;;; Counterpart

;;;###autoload
(defun tlon-visit-counterpart ()
  "Visit the ID associated with TODO, or vice versa."
  (interactive)
  (tlon-todo-issue-funcall #'tlon-visit-issue
			   #'tlon-visit-todo))

;;;###autoload
(defun tlon-visit-counterpart-or-capture ()
  "Visit the issue associated with TODO, or vice versa, creating TODO if necessary."
  (interactive)
  (tlon-todo-issue-funcall #'tlon-visit-issue
			   #'tlon-visit-todo-or-capture))

(defun tlon-todo-issue-funcall (todo-fun issue-fun)
  "Call TODO-FUN or ISSUE-FUN depending on the current major mode."
  (pcase major-mode
    ('org-mode
     (let ((entry-pos (org-entry-beginning-position)))
       (if entry-pos
	   (progn
	     (goto-char entry-pos) ; Move to the start of the headline
	     (funcall todo-fun))
	 (user-error "I could not find an `org-mode` entry at point"))))
    ((or 'forge-topic-mode 'forge-issue-mode 'magit-status-mode)
     (unless (tlon-get-issue-name)
       (user-error "I could not find a GitHub issue at point"))
     (funcall issue-fun))
    (_ (user-error "This command cannot be invoked in `%s`" major-mode))))

;;;;; Get heading elements

(defun tlon-get-element-from-heading (regexp)
  "Get element matching REGEXP from the heading at point."
  (when (org-at-heading-p)
    (let ((heading (substring-no-properties (org-get-heading t t t t))))
      (when (string-match regexp heading)
	(match-string 1 heading)))))

(defun tlon-get-issue-number-from-heading ()
  "Get the GitHub issue number from the `org-mode' heading at point."
  (when-let ((issue-number (tlon-get-element-from-heading "#\\([[:digit:]]\\{1,4\\}\\)")))
    (string-to-number issue-number)))

(defun tlon-get-repo-from-heading ()
  "Return the repository directory indicated in the Org heading at point.
The heading is expected to contain a fragment like \"[REPO-IDENTIFIER]\"
anywhere in the text. The REPO-IDENTIFIER (what appears inside the square
brackets) is first treated as a repository abbreviation, and if that lookup
fails, it's treated as a repository name. The path is found via
`tlon-repo-lookup'."
  (when-let* ((repo-identifier (tlon-get-element-from-heading "\\[\\([^]]+\\)\\]")))
    (or (tlon-repo-lookup :dir :abbrev repo-identifier)
	(tlon-repo-lookup :dir :name repo-identifier))))

(defun tlon-get-issue-number-from-open-issues (&optional forge-repo)
  "Prompt user to select from open issues in FORGE-REPO and return its number.
If FORGE-REPO (a forge-repository object) is nil, the repository is
determined from context or by prompting."
  (let* ((repo-to-use (or forge-repo
			  (let ((repo-path (tlon-get-repo nil 'include-all))) ; Prompts if needed
			    (when repo-path
			      (let ((default-directory repo-path))
				(forge-get-repository :tracked))))))
	 ;; Ensure repo-to-use is valid before proceeding
	 (_ (unless repo-to-use (user-error "Could not determine repository for issue selection")))
	 (default-directory (oref repo-to-use worktree)) ; Set context for oref issues
	 ;; Fetch all issues, but filter for open ones
	 (issue-list (mapcar #'(lambda (issue) ; issue here is a forge-issue object
				 (cons (format "#%d %s"
					       (oref issue number)
					       (oref issue title))
				       (oref issue number)))
			     (cl-remove-if-not (lambda (issue)
						 (string= (oref issue state) "open"))
					       (oref repo-to-use issues))))
	 ;; Let the user select one, requiring a match
	 (selected-issue-number (cdr (assoc (completing-read "Select an issue: " issue-list nil t) issue-list))))
    ;; Return the selected issue number
    selected-issue-number))

(defun tlon-get-issues (&optional repo)
  "Return a list of all open issues in REPO.
If REPO is nil, use the current repository."
  (let* ((repo (or repo (forge-get-repository :tracked)))
	 (repo-id (oref repo id))
	 (issues (forge-sql [:select [id]
				     :from issue
				     :where (= repository $s1)
				     :and  (= state 'open)]
			    repo-id)))
    (mapcar (lambda (issue-id)
	      (closql-get (forge-database) issue-id 'forge-issue))
	    (mapcar #'car issues))))

(defun tlon-forg--issue-exists-p (issue)
  "Return non-nil if ISSUE still exists on the remote GitHub server.
It queries the GitHub REST API for the issue and returns t on success,
or nil if the issue was deleted or any network error occurred."
  (require 'ghub nil t)
  (let* ((repo  (forge-get-repository issue))
         (owner (oref repo owner))
         (name  (oref repo name))
         (num   (oref issue number)))
    (condition-case nil
        (progn
          ;; REST endpoint returns 200 when the issue exists
          (ghub-get (format "/repos/%s/%s/issues/%s" owner name num)
                    nil :auth 'forge)
          t)
      (ghub-http-error nil)
      (error nil))))

(defun tlon-get-latest-issue (&optional repo)
  "Return a list of the most recent open issue in REPO, or nil otherwise.
The list is in the format \\='(NUMBER TITLE). If REPO is nil, use the current
repository."
  (let* ((issues (tlon-get-issues repo))
	 (latest-issue (car (sort issues (lambda (a b)
					   (time-less-p
					    (date-to-time (oref b created))
					    (date-to-time (oref a created))))))))
    (if latest-issue
	(list (oref latest-issue number) (oref latest-issue title))
      nil)))

(defun tlon-count-issues (&optional repo)
  "Return the number of open issues in REPO.
If REPO is nil, use the current repository."
  (length (tlon-get-issues repo)))

;;;;; Set heading elements

(autoload 'org-extras-goto-beginning-of-heading-text "org-extras")
(defun tlon-set-repo-in-heading ()
  "Set the repo in the heading at point if not already present."
  (when (and (org-at-heading-p)
	     (not (tlon-get-repo-from-heading)))
    (let* ((repo-name (completing-read "Select repo: " (tlon-repo-lookup-all :name)))
	   (abbrev-repo (tlon-repo-lookup :abbrev :name repo-name)))
      (org-extras-goto-beginning-of-heading-text)
      (insert (format "[%s] " abbrev-repo)))))

(defun tlon-set-issue-number-in-heading (issue-number)
  "Set ISSUE-NUMBER in heading at point if not already present."
  (unless (tlon-get-issue-number-from-heading)
    (org-extras-goto-beginning-of-heading-text)
    ;; move past repo name
    (re-search-forward "\\[.+?\\] ")
    (insert (format "#%s " (number-to-string issue-number)))))

;;;;; Close issues, todos

;;;###autoload
(defun tlon-close-issue-and-todo ()
  "With point on either, close issue and associated TODO."
  (interactive)
  (tlon-todo-issue-funcall
   #'tlon-close-issue-and-todo-from-issue
   (lambda ()
     (tlon-visit-counterpart)
     (tlon-close-issue-and-todo-from-issue))))

(defun tlon-close-issue-and-todo-from-issue ()
  "With point on issue, close issue and associated TODO."
  (let ((issue (tlon-get-issue)))
    (when issue
      (tlon-close-issue-number (oref issue number) (oref (forge-get-repository issue) worktree))
      (let ((pos-file-pair (tlon-get-todo-position-from-issue issue))
	    message)
	(if pos-file-pair
	    (progn
	      (tlon-visit-todo pos-file-pair)
	      (org-todo "DONE")
	      (if tlon-forg-archive-todo-on-close
		  (progn
		    (org-archive-subtree-default)
		    (setq message "Closed issue and TODO (archived)."))
		(setq message "Closed issue and TODO."))
	      (save-buffer))
	  (setq message "Closed issue, but no corresponding TODO found to mark as DONE."))
	(message message)))))

;; shouldn’t this be done using the orgit-link rather than issue-number?
(defun tlon-close-issue-number (issue-number repo)
  "Close the issue with ISSUE-NUMBER in REPO."
  (tlon-visit-issue issue-number repo)
  (tlon-close-issue))

;;;;; Get tags, status, assignees

(defun tlon-get-labels-of-type (type &optional issue)
  "Return the valid label(s) of TYPE in ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let ((issue (or issue (forge-current-topic))))
    (when-let* ((labels (tlon-forg-get-labels issue)))
      (pcase type
	('tag labels)
	('phase (tlon-get-phase-in-labels labels))
	(_ (user-error "Invalid type"))))))

;;;;;; sort by tag

;;;###autoload
(defun tlon-forg-sort-by-tag ()
  "Sort org entries by the first tag that matches the relevant regexp pattern.
The pattern is defined in `tlon-forg-sort-by-tags-regexp'. Unmatched tags are
sorted to the end."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an `org-mode' buffer"))
  (org-sort-entries nil ?f #'tlon-forg-tag-sorter))

(defun tlon-forg-tag-sorter ()
  "Return the first tag of an org heading that matches the relevant regexp pattern.
The pattern is defined in `tlon-forg-sort-by-tags-regexp'. If no tag matches,
return \"~\", so that the entry is sorted to the end."
  (let ((tags (org-get-tags)))
    (catch 'match
      (dolist (tag tags)
	(when (string-match-p tlon-forg-sort-by-tags-regexp tag)
	  (throw 'match tag)))
      "~")))

;;;###autoload
(defun tlon-forg-sort-by-status-and-project-order (&optional arg)
  "Sort org entries by status and then by GitHub Project order.
Status order is based on `tlon-todo-statuses' (DOING, NEXT, LATER, SOMEDAY),
followed by other active TODO states, then DONE, then items with no status.
Project order is determined by `forge-extras-list-project-items-ordered'.
Entries not linked to an issue, or issues not in the project, are sorted last.

With prefix ARG, use the cached project items list instead of fetching a fresh
one."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an `org-mode' buffer"))
  (let ((project-items (forge-extras-list-project-items-ordered nil nil arg)))
    ;; Ensure we're at a heading before sorting
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward org-heading-regexp nil t)
	(beginning-of-line)
	(org-sort-entries nil ?f (lambda () (tlon-forg-status-and-project-order-sorter project-items)))))))

(defun tlon-forg-status-and-project-order-sorter (project-items)
  "Return a sort key for an Org entry based on status and project order.
PROJECT-ITEMS is a list of plists from
`forge-extras-list-project-items-ordered'. The sort key is a number for proper
comparison in `org-sort-entries'. Lower values sort earlier."
  (let* ((tags (org-get-tags))
	 (archive-priority 2000000000) ; Very high number to ensure archive entries sort last
	 (issue (tlon-get-issue))
	 (max-priority 999)
	 (status-priority max-priority)
	 (project-position max-priority)
	 (issue-number max-priority))
    ;; Check if this heading has the ARCHIVE tag
    (if (member "ARCHIVE" tags)
	archive-priority ; Sort archive entries to the very end
      (if (not issue)
	  max-priority ; Sort items not linked to issues last (but before archive)
	(progn
	  (setq issue-number (oref issue number))
	  ;; Calculate status-priority
	  (let* ((status-keyword (org-get-todo-state))
		 (todo-order (mapcar #'cdr tlon-todo-statuses))
		 (done-keyword "DONE")) ; Assuming "DONE" is the keyword for completed tasks
	    (cond
	     ((null status-keyword) (setq status-priority 100)) ; No status
	     ((string= status-keyword done-keyword) (setq status-priority 99)) ; DONE status
	     (t
	      (let ((pos (cl-position status-keyword todo-order :test #'string=)))
		(if pos
		    (setq status-priority pos)
		  (setq status-priority 50)))))) ; Default for other active states
	  ;; Calculate project-position
	  (when-let* ((repo-obj (forge-get-repository issue))
		      (owner (oref repo-obj owner))
		      (repo-name (oref repo-obj name))
		      (issue-repo-fullname (format "%s/%s" owner repo-name)))
	    (let ((idx -1) (found-idx nil))
	      (cl-block search-loop
		(dolist (item project-items)
		  (setq idx (1+ idx))
		  (when (and (eq (plist-get item :type) 'issue)
			     (string= (plist-get item :repo) issue-repo-fullname)
			     (= (plist-get item :number) issue-number))
		    (setq found-idx idx)
		    (cl-return-from search-loop))))
	      (if found-idx
		  (setq project-position found-idx)
		(setq project-position max-priority)))) ; Issue not in project list
	  ;; Return a single number that encodes all three priorities
	  (+ (* status-priority 1000000) (* project-position 1000) issue-number))))))

;;;;;; status

(defun tlon-get-status-in-issue (&optional issue _upcased project-item-data)
  "Return the GitHub Project status of ISSUE, mapped to an Org TODO keyword.
PROJECT-ITEM-DATA is a plist that may contain :status and :estimate."
  (let* ((issue (or issue (forge-current-topic)))
	 ;; 1. take :status from PROJECT-ITEM-DATA when present
	 (project-status-val (when project-item-data
			       (plist-get project-item-data :status)))
	 org-status)

    ;; 2. if still nil, fetch it on-demand via GraphQL
    (when (null project-status-val)
      (when-let* ((repo (and issue (forge-get-repository issue)))
		  (repo-name (and repo (oref repo name)))
		  (issue-number (and issue (oref issue number))))
	(let* ((raw (condition-case err
			(forge-extras-gh-get-issue-fields issue-number repo-name)
		      (error (progn
			       (when tlon-debug
				 (message "Error fetching GH fields for #%s in %s: %s"
					  issue-number repo-name err))
			       nil))))
	       (parsed (and raw (forge-extras-gh-parse-issue-fields raw))))
	  (setq project-status-val (plist-get parsed :status)))))

    ;; 3. map to org TODO keyword (case-insensitive)
    (when project-status-val
      (setq org-status
	    (cdr (assoc-string (format "%s" project-status-val)
			       tlon-todo-statuses
			       t)))) ; case-insensitive

    ;; 4. fallback to open/closed when mapping failed
    (or org-status
	(if (and issue (eq (oref issue state) 'completed)) "DONE" "TODO"))))

(defun tlon-get-status-in-todo ()
  "Return the status of the `org-mode' heading at point.
The status is returned downcased."
  (when-let ((status (org-get-todo-state)))
    (when (member status (mapcar #'cdr tlon-todo-statuses))
      (downcase status))))

;;;;;; job phases
;; TODO: make nomenclature consistent: should be called `phases' throughout, not `labels'

(defun tlon-get-phase-in-labels (labels)
  "Return the unique valid phase in LABELS.
A label is considered a valid phase if it exists in `tlon-todo-statuses'.
If more than one phase is found, signal an error."
  (when-let ((status (cl-intersection labels
				      (tlon-label-lookup-all :label)
				      :test 'string=)))
    (if (= (length status) 1)
	(car status)
      (user-error "Issue has more than one status"))))

(defun tlon-get-phase-in-issue (&optional issue)
  "Return the unique valid job phase in ISSUE.
A label is valid job phase iff it is a member of `tlon-job-labels'.

If ISSUE is nil, use the issue at point or in the current buffer."
  (tlon-get-labels-of-type 'phase issue))

;;;;;; assignees

(defun tlon-get-assignee (&optional issue)
  "Return assignee of ISSUE.
If the issue has more than one assignee, return the first. If ISSUE is nil, use
the issue at point or in the current buffer."
  (when-let* ((issue (or issue (forge-current-topic)))
	      (assignees (car (closql-dref issue 'assignees)))
	      (assignee-name (nth 1 assignees)))
    assignee-name))

;;;;;; labels

(defun tlon-forg-get-labels (&optional issue)
  "Return the labels of ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (labels (closql-dref issue 'labels))
	 (label-names (mapcar (lambda (label)
				(nth 1 label))
			      labels)))
    label-names))

(defun tlon-get-tags-in-todo ()
  "Return the valid tags in the `org-mode' heading at point.
A tag is valid iff it is a member of `tlon-todo-tags'."
  (when-let ((tags (cl-intersection
		    (cdr (org-get-tags)) tlon-todo-tags :test 'string=)))
    tags))

;;;;; Validate

(defun tlon-assignee-is-current-user-p (issue)
  "Return t iff the assignee of ISSUE is the current user."
  (let ((assignee (tlon-get-assignee issue))
	(user (or
	       tlon-forg-enforce-user
	       (tlon-user-lookup :github :name user-full-name))))
    (string= assignee user)))

(defun tlon-todo-has-valid-status-p ()
  "Return t iff status of TODO at point it is a valid TODO status.
A status is valid iff it is one of the `cdr` values in `tlon-todo-statuses'."
  (when-let ((status (org-get-todo-state)))
    (member status (mapcar #'cdr tlon-todo-statuses))))

;;;###autoload
(defun tlon-forg-check-for-duplicate-todos (&optional file)
  "Check for duplicate TODO entries in FILE.
A duplicate is a heading with an `orgit-topic' ID that appears more than once.
If FILE is nil, use the file associated with the current buffer. If duplicates
are found, signal an error listing the duplicate IDs and their counts.
Otherwise, return nil."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
	 (topic-ids (make-hash-table :test 'equal))
	 (duplicates '()))
    (unless (and file (file-exists-p file))
      (user-error "File not found: %s" file))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward tlon-forg-orgit-regexp nil t)
	  (let ((id (match-string-no-properties 1)))
	    (puthash id (1+ (gethash id topic-ids 0)) topic-ids)))))
    (maphash (lambda (id count)
	       (when (> count 1)
		 (push (cons id count) duplicates)))
	     topic-ids)
    (if duplicates
	(user-error "Duplicate TODOs found in %s:\n%s"
		    (file-name-nondirectory file)
		    (mapconcat (lambda (dup) (format "- ID %s: %d times" (car dup) (cdr dup)))
			       duplicates "\n"))
      (message "No duplicate TODOs found in %s" (file-name-nondirectory file)))))

;;;;; Set tags, status, assignees

(defun tlon-set-labels (labels &optional type issue)
  "Set list of LABELS of TYPE in ISSUE.
If TYPE is `phase' or `status', replace the label of that type if present, while
keeping all other labels. If TYPE is any other value, add LABELS and keep all
current labels.

If ISSUE is nil, use issue at point or in the current buffer."
  (interactive)
  (let* ((issue (or issue (forge-current-topic)))
	 (repo (forge-get-repository issue))
	 (current-labels (tlon-forg-get-labels issue))
	 (status (tlon-get-status-in-issue issue))
	 (phase (tlon-get-phase-in-issue issue))
	 (trimmed-labels (pcase type
			   ('status (remove status current-labels))
			   ('phase (remove phase current-labels))
			   (_ current-labels))))
    (forge--set-topic-labels repo issue (append labels trimmed-labels))))

;; TODO: Cleanup the two functions below
(defun tlon-set-job-label ()
  "Prompt the user to select a job label."
  (let ((label (completing-read "What should be the label? "
				(tlon-label-lookup-all :label))))
    label))

(defun tlon-set-status (&optional prompt)
  "Prompt the user to select a status label.
Use PROMPT as the prompt, defaulting to \"TODO status? \"."
  (let* ((prompt (or prompt "TODO status? "))
	 (label (completing-read prompt (mapcar #'downcase (mapcar #'cdr tlon-todo-statuses)) nil t)))
    label))

(defun tlon-set-assignee (assignee &optional issue)
  "Make ASSIGNEE the assignee of ISSUE.
If ISSUE is nil, use issue at point or in the current buffer."
  (interactive
   (list (tlon-select-assignee)))
  (let* ((issue (or issue (forge-current-topic)))
	 (repo (forge-get-repository issue)))
    (forge--set-topic-assignees repo issue `(,assignee))))

(defun tlon-select-assignee (&optional prompt)
  "Prompt the user to select an ASSIGNEE.
Use PROMPT as the prompt, defaulting to \"Who should be the assignee? \"."
  (let* ((prompt (or prompt "Who should be the assignee? "))
	 (assignee (completing-read prompt
				    (tlon-user-lookup-all :github) nil nil
				    (tlon-user-lookup :github :name user-full-name))))
    assignee))

(defun tlon-set-initial-label-and-assignee ()
  "Set label to `Awaiting processing' and assignee to current user."
  (tlon-set-labels '("Awaiting processing") 'phase)
  (tlon-set-assignee (tlon-user-lookup :github :name user-full-name)))

;;;;; Get elements

(defun tlon-get-element (element &optional issue)
  "Return ELEMENT of ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (when-let ((issue (or issue (forge-current-topic))))
    (eieio-oref issue element)))

(defun tlon-get-first-element (element &optional issue)
  "Return first ELEMENT of ISSUE.
If the issue has more than one element, return the first. If ISSUE is nil, use
the issue at point or in the current buffer."
  (when-let ((issue (or issue (forge-current-topic))))
    (caar (eieio-oref issue element))))

;; TODO: should return all labels, not just first
(defun tlon-get-first-label (&optional issue)
  "Return the first label of the issue at point.
If ISSUE is nil, use the issue at point or in the current buffer."
  (tlon-get-first-element 'labels issue))

(defun tlon-get-state (&optional issue)
  "Return state of ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (when-let ((issue (or issue (forge-current-topic))))
    (oref issue state)))

;;;;; Issue Data Formatting Helpers

(defun tlon-get-issue-name (&optional issue)
  "Get the name of ISSUE.
An issue name is its number followed by its title.
The title part is sanitized to be a single line by replacing newlines with
spaces.

If ISSUE is nil, get the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (number (oref issue number))
	 (title (replace-regexp-in-string "\n" " "
					  (tlon-forg-md->org (oref issue title)))))
    (format "#%s %s" number title)))

(defun tlon-get-issue-link (&optional issue)
  "Get an `org-mode' link to ISSUE.
If ISSUE is nil, get the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (name (tlon-get-issue-name issue))
	 (id (oref issue id)))
    (org-link-make-string (format "orgit-topic:%s" id) name)))

(defun tlon-make-todo-name-from-issue (&optional issue no-action no-status override-status)
  "Construct the name of a TODO from an ISSUE.
For job TODOs, the resulting name will have a name with the form \"[REPO] ACTION
NAME\". ACTION is optional, and used only for job TODOs. For example, if the
TODO is \"[uqbar-es] #591 Job: `Handbook2022ExerciseForRadical`\", and ACTION is
\"Process\", the function returns \"[uqbar-es] Process #591 Job:
`Handbook2022ExerciseForRadical`\".

If ISSUE is nil, use the issue at point or in the current buffer. If NO-ACTION
is non-nil, omit the ACTION element. If NO-STATUS is non-nil, omit the
STATUS element. If OVERRIDE-STATUS is non-nil, use it instead of fetching."
  (let* ((issue (or issue (forge-current-topic)))
	 (action (if (and (tlon-issue-is-job-p issue)
			  (not no-action))
		     (or (tlon-label-lookup :action :label (tlon-get-first-label issue))
			 "")
		   ""))
	 (status (if override-status
		     override-status
		   (tlon-get-status-in-issue issue nil nil)))
	 (tags (tlon-forg-get-labels issue))
	 (repo-name (oref (forge-get-repository issue) name))) ; Get the repository name
    ;; Use repo-name directly, instead of looking up an abbreviation.
    (let ((todo-name (replace-regexp-in-string
		      "[[:space:]]\\{2,\\}"
		      " "
		      (concat
		       (unless no-status (format "%s " (or status "")))
		       (format "[%s] %s %s" repo-name action (tlon-get-issue-link issue))
		       (when tags (format "   :%s:" (mapconcat #'identity tags ":")))))))
      todo-name)))

;;;;; Create issues

(defun tlon-create-issue (title &optional repo body format)
  "Create a new GitHub issue in REPO with TITLE and BODY.
Returns the created issue number.

Optional fourth argument FORMAT can be `:org' (default, convert title from Org
to Markdown) or `:markdown' (title is already Markdown, do not convert)."
  (let* ((repo (or repo (tlon-get-repo 'error 'include-all)))
	 (body (or body ""))
	 (default-directory repo)
	 (repo (tlon-forg--get-repository-from-dir default-directory))
	 (owner (oref repo owner))
	 (reponame (oref repo name))
	 (resource (format "/repos/%s/%s/issues" owner reponame))
	 (md-title (if (eq format :markdown)
		       title                       ; already md
		     (tlon-forg-org->md title)))   ; org → md
	 (data `((title . ,md-title)
		 (body . ,body)))
	 (resp (ghub-post resource data :auth 'forge))
	 (num  (alist-get 'number resp)))
    (message "Created issue #%s with title %s" num title)
    num))

;;;###autoload
(defun tlon-create-new-issue ()
  "Create a GitHub issue from scratch, then capture it as an Org TODO.
The command:

- prompts for a repository, title, status (Org TODO keyword), and optional
  effort estimate in hours;

- creates the issue, labels it with the chosen status, sets the project estimate
  when given;

- captures the freshly-created issue as an Org TODO and leaves point on that
  heading.

- silently adds the issue to the project and updates its project status to match
  the TODO."
  (interactive)
  (let* ((detected-repo (tlon-repo-lookup :name :dir (tlon-get-repo 'no-prompt 'include-all)))
	 (selected-repo (completing-read "Repo: " (tlon-repo-lookup-all :name) nil t detected-repo))
	 (repo-dir  (tlon-repo-lookup :dir :name selected-repo))
	 (forge-repo (let ((default-directory repo-dir))
		       (forge-get-repository :tracked)))
	 (title       (read-string "Issue title: "))
	 (status      (completing-read
		       "Status: "
		       (seq-remove (lambda (s) (string= s "DONE"))
				   (mapcar #'cdr tlon-todo-statuses))
		       nil t "DOING"))
	 (effort-str  (read-string "Effort hours (optional): "))
	 (effort-h    (and (string-match-p "\\S-" effort-str)
			   (string-to-number effort-str)))
	 (assignee    (tlon-select-assignee "Assignee: ")))
    (unless repo-dir
      (user-error "Could not determine repository directory"))
    (unless (and forge-repo (object-p forge-repo))
      (user-error "No tracked Forge repository found for %s" repo-dir))
    (let* ((new-num (tlon-create-issue title repo-dir nil :markdown)))
      ;; Ensure local DB sees new issue + subsequent remote changes (assignee/labels).
      (tlon-forg--pull-sync forge-repo)
      (let* ((issue (tlon-forg--wait-for-issue new-num repo-dir forge-repo)))
	(tlon-set-assignee assignee issue)
	(tlon-set-labels `(,status) nil issue)
	(when effort-h
	  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
	    (tlon-forg--set-github-project-estimate issue effort-h)))
	(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
	  (tlon-forg--set-github-project-status issue status))
	;; Refresh Forge DB so `issue' reflects the assignee/labels changes, and so
	;; Org capture can reliably locate and link the topic.
	(tlon-forg--pull-sync forge-repo)
	(setq issue (tlon-forg--wait-for-issue new-num repo-dir forge-repo))
	;; The assignee has been set on the remote, but the local `issue' object
	;; may still be stale. We let-bind `tlon-when-assignee-is-nil' and
	;; `tlon-when-assignee-is-someone-else' so capture always proceeds.
	(let ((tlon-when-assignee-is-nil 'capture)
	      (tlon-when-assignee-is-someone-else 'capture))
	  (tlon-capture-issue issue nil status))
	;; After capture, locate the TODO by orgit id. Newer Org versions can
	;; sometimes defer writes; force a save and retry once.
	(unless (tlon-get-todo-position-from-issue issue)
	  (when-let ((cap-buf (org-capture-get :buffer)))
	    (with-current-buffer cap-buf
	      (when (buffer-file-name)
		(save-buffer))))
	  (tlon-forg--pull-sync forge-repo))
	(when-let ((pf (tlon-get-todo-position-from-issue issue)))
	  (tlon-visit-todo pf)
	  (org-todo status)
	  (when effort-h
	    (tlon-forg--set-org-effort effort-h))
	  (tlon-visit-counterpart)
	  (other-window 1))))))

(defun tlon-create-issue-in-dir (dir)
  "Create a new issue in the git repository at DIR."
  (let* ((default-directory dir)
	 (repo (forge-get-repository :tracked)))
    (unless repo
      (user-error "No tracked Forge repository found for directory %s" dir))
    (forge--pull repo)
    (magit-status-setup-buffer (oref repo worktree))
    (let ((template (forge--topic-template nil 'forge-issue)))
      (forge-create-issue template))))

;;;###autoload
(defun tlon-report-bug ()
  "Submit a bug report in the `tlon.el' repo."
  (interactive)
  (tlon-create-issue-in-dir (tlon-repo-lookup :dir :name "tlon.el"))
  (tlon-prepopulate-bug-report-buffer))

(defun tlon-prepopulate-bug-report-buffer ()
  "Prepopulate the bug report buffer with relevant headings and information."
  (goto-char (point-max))
  (insert (concat "Bug: \n\n"
		  "## Descripción\n\n\n"
		  "## Pasos para reproducir\n\n1.\n2.\n3.\n\n\n"
		  "## Comentarios\n<!-- Opcional -->\n\n\n"
		  "## Backtrace\n<!-- Si el backtrace es demasiado largo y GitHub lo rechaza, adjuntar como archivo txt -->\n\n\n```elisp\n\n```\n\n"
		  "## Información del sistema\n<!-- No modificar -->\n"
		  "\n- Config version: " (file-name-nondirectory (directory-file-name user-emacs-directory))
		  "\n- Tlon version: " tlon-version
		  "\n- Latest commit: " (tlon-get-latest-commit tlon-package-dir)
		  "\n\n"))
  (goto-char (+ (point-min) 7)))

;;;###autoload
(defun tlon-create-issue-from-todo (&optional debug)
  "Create a GitHub issue from the Org heading at point.

The command replaces _that exact_ heading with a link to the new issue. Three
independent anchors ensure we always get back to the right headline: the `:ID:`
property, the outline path, and a marker. Enable verbose logging with a prefix
argument (\\[universal-argument]), by setting `tlon-debug' to non-nil, or by
passing a non-nil DEBUG."
  (interactive "P")
  (setq tlon-debug (or debug tlon-debug))
  (tlon-ensure-org-mode)
  ;; Abort if this heading already links to an issue
  (when (tlon-get-issue-number-from-heading)
    (user-error "This heading already has an issue"))
  ;;------------------------------------------------------------
  ;; Pin the target *three* different ways
  ;;------------------------------------------------------------
  (let* ((origin-buffer (current-buffer))
	 (marker        (copy-marker (org-entry-beginning-position) t))
	 (id            (org-id-get-create))
	 (outline-path  (org-get-outline-path 'with-self)))
    (tlon--dlog "Pinned headline: ID=%s path=%S marker=%d"
		id outline-path (marker-position marker))
    (unwind-protect
	;;=======================================================
	;; 1. Gather metadata while still on the target headline
	;;=======================================================
	(let* ((repo-dir  (or (tlon-get-repo-from-heading)
			      (tlon-get-repo nil 'include-all)))
	       (heading-str
		(substring-no-properties (org-get-heading t t t t)))
	       (title
		(string-trim
		 (replace-regexp-in-string
		  "\n" " "
		  (replace-regexp-in-string
		   "^\\(TODO\\|NEXT\\|DOING\\|DONE\\|LATER\\|SOMEDAY\\)[[:space:]]+"
		   "" heading-str))))
	       (org-tags (tlon-get-tags-in-todo))
	       (hours
		(tlon-forg--org-effort-to-hours
		 (org-entry-get nil "Effort" 'inherit)))
	       (status (or (org-get-todo-state) "DOING")))
	  (tlon--dlog "Prepared issue title=%S status=%s tags=%S hours=%S"
		      title status org-tags hours)
	  ;;===============================================
	  ;; 2. Network / Forge operations
	  ;;===============================================
	  (save-current-buffer
	    (let* ((default-directory repo-dir)
		   (forge-repo (forge-get-repository :tracked))
		   (new-num    (tlon-create-issue title repo-dir)))
	      (tlon--dlog "GitHub issue #%s created" new-num)
	      (tlon-forg--pull-sync forge-repo)
	      (let ((issue (tlon-forg--wait-for-issue
			    new-num repo-dir forge-repo)))
		(tlon-set-assignee
		 (tlon-user-lookup :github :name user-full-name) issue)
		(tlon-set-labels `(,status ,@org-tags) nil issue)
		(when hours
		  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
		    (tlon-forg--set-github-project-estimate issue hours)))
		(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
		  (tlon-forg--set-github-project-status issue status))
		;;===========================================
		;; 3. Replace the ORIGINAL headline in place
		;;===========================================
		(let ((new-text (tlon-make-todo-name-from-issue issue))
		      (jump ""))
		  (with-current-buffer origin-buffer
		    ;; -------- Step 3a: find the headline --------
		    (cond
		     ;; 1) :ID: is the most reliable
		     ((org-id-goto id)
		      (setq jump "ID"))
		     ;; 2) Outline path lookup (now via org-find-olp)
		     ((let ((pos (org-find-olp outline-path t)))
			(when pos (goto-char pos)))
		      (setq jump "path"))
		     ;; 3) Marker fallback
		     (t
		      (goto-char marker)
		      ;; Make sure an ID exists again
		      (org-id-get-create)
		      (setq jump "marker")))
		    (tlon--dlog "Returned to headline via %s" jump)
		    ;; Verify we are on the intended entry
		    (unless (equal id (org-entry-get nil "ID"))
		      (tlon--dlog
		       "WARNING: ID mismatch! wanted=%s got=%s"
		       id (org-entry-get nil "ID"))
		      (user-error
		       "Cannot locate the original heading – aborting"))
		    ;; -------- Step 3b: actual replacement -----
		    (tlon--replace-headline-text marker new-text)
		    (when hours
		      (tlon-forg--set-org-effort hours))
		    (tlon--dlog "Headline successfully replaced.")))))))
      ;; Always release the marker
      (set-marker marker nil))))

(defun tlon--dlog (fmt &rest args)
  "Log a debug message with FMT and ARGS if `tlon-debug' is non-nil."
  (when tlon-debug
    (apply #'message (concat "[TLON‑DEBUG] " fmt) args)))

(defun tlon--replace-headline-text (marker new-text)
  "Replace the headline starting at MARKER with NEW-TEXT.
MARKER must point to the first star of the headline."
  (with-current-buffer (marker-buffer marker)
    (goto-char marker)
    (org-with-wide-buffer
     (org-back-to-heading t)
     (let* ((level (org-outline-level))
	    (stars (make-string level ?*)))
       (looking-at org-complex-heading-regexp)
       (replace-match (concat stars " " new-text) t t)))))

(defun tlon-create-issue-or-todo ()
  "Create issue from TODO or vice versa."
  (interactive)
  (tlon-todo-issue-funcall #'tlon-create-issue-from-todo
			   #'tlon-capture-issue))

(declare-function tlon-get-key-in-buffer "tlon")
(defun tlon-create-issue-from-key (&optional key)
  "Create an issue based on KEY.
If KEY is not provided, the key in the Markdown buffer at point is used."
  (let ((default-directory (tlon-get-repo 'error))
	(key (or key (tlon-get-key-in-buffer))))
    (tlon-create-issue (format "Job: `%s`" key) default-directory nil :markdown)))

(defun tlon-close-issue (&optional issue)
  "Close ISSUE.
If ISSUE is nil, use the issue at point or in the current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (repo (forge-get-repository issue)))
    (when (eq 'open (oref issue state))
      (forge--set-topic-state repo issue 'completed))))

;;;;; File Operations for Issues

(defvar tlon-key-regexp)
(declare-function tlon-get-file-from-key "tlon")
(defun tlon-get-file-from-issue (&optional issue)
  "Get the file path of ISSUE.
If ISSUE is nil, use the issue at point or in current buffer."
  (let* ((issue (or issue (forge-current-topic)))
	 (name (tlon-get-issue-name issue)))
    (if (string-match tlon-key-regexp name)
	(tlon-get-file-from-key (match-string 1 name))
      (user-error "I wasn't able to find a file at point or in the forge buffer"))))

;;;###autoload
(defun tlon-open-forge-file ()
  "Open the file of the issue at point or in the current buffer."
  (interactive)
  (find-file (tlon-get-file-from-issue)))

(declare-function tlon-open-counterpart "tlon-counterpart")
;;;###autoload
(defun tlon-open-forge-counterpart ()
  "Open the file counterpart of the issue at point or in the current buffer."
  (interactive)
  (tlon-open-counterpart nil (tlon-get-file-from-issue)))

(defun tlon-get-parent-todo (todo)
  "Get parent of TODO in `tlon-todos-jobs-file'."
  (let* ((jobs-file (tlon-get-todos-jobs-file))
	 (pos (tlon-get-todo-position todo jobs-file)))
    (when pos
      (save-window-excursion
	(tlon-visit-todo (cons pos jobs-file))
	(widen)
	(org-up-heading-safe)
	(org-no-properties (org-get-heading))))))

(defun tlon-mark-todo-done (todo file)
  "Mark TODO in FILE as DONE."
  (let ((pos (tlon-get-todo-position todo file)))
    (when pos
      (save-window-excursion
	(tlon-visit-todo (cons pos file))
	(org-todo "DONE")
	(save-buffer)
	(message "Marked `%s' as DONE" todo)))))

(declare-function tlon-get-clock-key "tlon-clock")
(declare-function tlon-get-clock-label "tlon-clock")
;; MAYBE: move to jobs?
(defun tlon-check-label-and-assignee (repo)
  "Check that clocked action, user match label, assignee of issue in REPO."
  (save-window-excursion
    (let* ((default-directory repo)
	   (key (tlon-get-clock-key))
	   (issue-title (format "Job: `%s" key))
	   (issue (tlon-issue-lookup issue-title))
	   (clocked-label (tlon-get-clock-label))
	   (label (tlon-get-first-label issue))
	   (assignee (tlon-user-lookup :name :github (tlon-get-assignee issue))))
      (unless (string= clocked-label label)
	(user-error "The `org-mode' TODO says the label is `%s', but the actual issue label is `%s'"
		    clocked-label label))
      (unless (string= user-full-name assignee)
	(user-error "The `org-mode' TODO says the assignee is `%s', but the actual issue assignee is `%s'"
		    user-full-name assignee))
      t)))

;;;;; misc

(declare-function org-roam-extras-node-find-special "org-roam-extras")
;;;###autoload
(defun tlon-node-find ()
  "Prompt the user to select a Tlön TODO in a list sorted by priority."
  (interactive)
  (org-roam-extras-node-find-special `(:dir ,paths-dir-tlon-todos)))

;;;;; Transient

(defun tlon-symbol-reader (prompt symbols)
  "Return a list of SYMBOLS with PROMPT to be used as an `infix' reader function."
  (let* ((input (completing-read prompt
				 (mapcar 'symbol-name symbols))))
    (intern input)))

(transient-define-infix tlon-when-assignee-is-nil-infix ()
  "Set the value of `tlon-when-assignee-is-nil' in `forg' menu."
  :class 'transient-lisp-variable
  :reader (lambda (prompt _ _)
	    (tlon-symbol-reader prompt '(prompt change warn capture no-capture)))
  :transient t
  :prompt "What to do when the assignee is nil (see docstring for details): "
  :variable 'tlon-when-assignee-is-nil)

(transient-define-infix tlon-when-assignee-is-someone-else-infix ()
  "Set the value of `tlon-when-assignee-is-someone-else' in `forg' menu."
  :class 'transient-lisp-variable
  :reader (lambda (prompt _ _)
	    (tlon-symbol-reader prompt '(prompt change warn capture no-capture)))
  :transient t
  :prompt "What to do when the assignee is someone else (see docstring for details): "
  :variable 'tlon-when-assignee-is-someone-else)

(transient-define-infix tlon-forg-when-syncing-infix ()
  "Set the value of `tlon-forg-when-syncing' in `forg' menu."
  :class 'transient-lisp-variable
  :reader (lambda (prompt _ _)
	    (tlon-symbol-reader prompt '(prompt issue todo)))
  :transient t
  :prompt "What to do when the issue and its associated todo differ (see docstring for details): "
  :variable 'tlon-forg-when-syncing)

(transient-define-infix tlon-infix-toggle-include-archived ()
  "Toggle the value of `tlon-forg-include-archived' in `forg' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-forg-include-archived
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-forg-include-archived)))

(transient-define-infix tlon-infix-toggle-archive-todo-on-close ()
  "Toggle the value of `tlon-forg-archive-todo-on-close' in `forg' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-forg-archive-todo-on-close
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-forg-archive-todo-on-close)))

(transient-define-infix tlon-forg-sync-after-capture-project-infix ()
  "Set the value of `tlon-forg-sync-after-capture-project' in `forg' menu."
  :class 'transient-lisp-variable
  :reader (lambda (prompt _ _)
	    (tlon-symbol-reader prompt '(t nil prompt)))
  :transient t
  :prompt "Sync after capturing all issues in project (see docstring for details): "
  :variable 'tlon-forg-sync-after-capture-project)

(transient-define-infix tlon-infix-toggle-sort-after-sync-or-capture ()
  "Toggle the value of `tlon-forg-sort-after-sync-or-capture' in `forg' menu."
  :class 'transient-lisp-variable
  :variable 'tlon-forg-sort-after-sync-or-capture
  :reader (lambda (_ _ _) (tlon-transient-toggle-variable-value 'tlon-forg-sort-after-sync-or-capture)))

;;;###autoload (autoload 'tlon-forg-menu "tlon-forg" nil t)
(transient-define-prefix tlon-forg-menu ()
  "`forg' menu."
  :info-manual "(tlon) GitHub and org-mode synchronization"
  [["Actions"
    ("n" "new"                                              tlon-create-new-issue)
    ("p" "post"                                             tlon-create-issue-from-todo)
    ("o" "sort"                                             tlon-forg-sort-by-status-and-project-order)
    ("v" "visit"                                            tlon-visit-counterpart)
    ("x" "close"                                            tlon-close-issue-and-todo)]
   ["Capture (issue ↠ todo)"
    ("c c" "capture issue"                                  tlon-capture-issue)
    ("c r" "capture all issues in repo"                     tlon-capture-all-issues-in-repo)
    ("c p" "capture all issues in project"                  tlon-capture-all-issues-in-project)]
   ["Sync (issue ↔ todo)"
    ("s s" "sync issue"                                     tlon-sync-issue-and-todo)
    ("s r" "sync all issues in repo"                        tlon-sync-all-issues-in-repo)
    ("s p" "sync all issues in project"                     tlon-sync-all-issues-in-project)]
   ["Options"
    ("-d" "When issue & todo differ"                        tlon-forg-when-syncing-infix)
    ("-n" "When assignee is nil"                            tlon-when-assignee-is-nil-infix)
    ("-e" "When assignee is someone else"                   tlon-when-assignee-is-someone-else-infix)
    ""
    ("-a" "Include archived"                                tlon-infix-toggle-include-archived)
    ("-A" "Archive on close"                                tlon-infix-toggle-archive-todo-on-close)
    ""
    ("-s" "Sync after project capture"                      tlon-forg-sync-after-capture-project-infix)
    ("-o" "Sort after project capture/sync"                 tlon-infix-toggle-sort-after-sync-or-capture)]])

(provide 'tlon-forg)
;;; tlon-forg.el ends here
