;;; tlon-meet.el --- Manage Tlön meetings -*- lexical-binding: t -*-

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

;; Manage Tlön meetings.

;;; Code:

(require 'org)
(require 'tlon-forg)
(require 'tlon-ai)

;;;; User options

(defgroup tlon-meet nil
  "Manage Tlön meetings."
  :group 'tlon)

(defcustom tlon-meet-diarize-script "diarize.py"
  "Path to the diarization script."
  :type 'string
  :group 'tlon-meet)

(defcustom tlon-meet-summary-prompt
  "Please provide a concise summary of the following conversation transcript. Focus on the key points discussed, decisions made, and any action items or follow-up tasks mentioned. Format the summary with bullet points for each main topic, and include a section at the end titled 'Action Items' that lists specific tasks that were assigned or mentioned.\n\n%s"
  "Prompt template for generating meeting summaries.
The %s will be replaced with the transcript text."
  :type 'string
  :group 'tlon-meet)

;;;; Functions

(defun tlon-create-or-visit-meeting-issue (&optional person-or-group)
  "Create or visit issue for a meeting with PERSON-OR-GROUP."
  (interactive)
  (let ((person-or-group (or person-or-group (tlon-prompt-for-all-other-users t))))
    (if (string= person-or-group "group")
	(tlon-create-or-visit-group-meeting-issue)
      (tlon-create-or-visit-individual-meeting-issue person-or-group))))

(defun tlon-create-or-visit-individual-meeting-issue (person &optional date)
  "Create or visit issue for a meeting with PERSON on DATE."
  (interactive (list (tlon-prompt-for-all-other-users)))
  (let* ((date (or date (org-read-date)))
	 (dir (tlon-get-meeting-repo person user-full-name)))
    (tlon-create-or-visit-meeting-issue-date date dir)))

(defun tlon-create-or-visit-group-meeting-issue (&optional date)
  "Create or visit issue for a group meeting on DATE."
  (interactive)
  (let* ((date (or date (org-read-date)))
	 (dir (tlon-repo-lookup :dir :name "meetings-group")))
    (tlon-create-or-visit-meeting-issue-date date dir)))

(defun tlon-create-or-visit-meeting-issue-date (date dir)
  "Create or visit issue in DIR for a meeting on DATE."
  (let ((default-directory dir))
    (tlon-wait-until-forge-updates)
    (if-let ((issue (tlon-issue-lookup date dir)))
	(forge-visit-issue issue)
      (tlon-create-and-visit-issue date dir))))

(defun tlon-wait-until-forge-updates ()
  "Wait until Forge is finished updating the repo."
  (let* ((get-last-updated (lambda () (oref (forge-get-repository :tracked?) updated)))
	 (last-updated (funcall get-last-updated))
	 (count 0))
    (forge-pull)
    (while (and (< count 25) (string= last-updated (funcall get-last-updated)))
      (sleep-for 0.1)
      (setq count (1+ count)))))

;; TODO: generate the next three functions with function
;;;###autoload
(defun tlon-create-or-visit-meeting-issue-leo-pablo (&optional date)
  "Create or visit issue for a meeting with Leo and Pablo on DATE."
  (interactive)
  (let ((date (or date (org-read-date)))
	(person (pcase user-full-name
		  ("Pablo Stafforini" "Leonardo Picón")
		  ("Leonardo Picón" "Pablo Stafforini")
		  (_ (user-error "This command is only for Leo and Pablo meetings")))))
    (tlon-create-or-visit-individual-meeting-issue person date)))

;;;###autoload
(defun tlon-create-or-visit-meeting-issue-fede-pablo (&optional date)
  "Create or visit issue for a meeting with Fede and Pablo on DATE."
  (interactive)
  (let ((date (or date (org-read-date)))
	(person (pcase user-full-name
		  ("Pablo Stafforini" "Federico Stafforini")
		  ("Federico Stafforini" "Pablo Stafforini")
		  (_ (user-error "This command is only for Fede and Pablo meetings")))))
    (tlon-create-or-visit-individual-meeting-issue person date)))

;;;###autoload
(defun tlon-create-or-visit-meeting-issue-fede-leo (&optional date)
  "Create or visit issue for a meeting with Fede and Leo on DATE."
  (interactive)
  (let ((date (or date (org-read-date)))
	(person (pcase user-full-name
		  ("Federico Stafforini" "Leonardo Picón")
		  ("Leonardo Picón" "Federico Stafforini")
		  (_ (user-error "This command is only for Leo and Fede meetings")))))
    (tlon-create-or-visit-individual-meeting-issue person date)))

(defun tlon-prompt-for-all-other-users (&optional group)
  "Ask the user to select from a list of all users except himself.
If GROUP is non-nil, include the \"group\" option in the prompt."
  (completing-read "Person: "
		   (let ((people
			  (cl-remove-if (lambda (user)
					  (string= user user-full-name))
					(tlon-user-lookup-all :name))))
		     (if group
			 (append people '("group"))
		       people))))

;; TODO: create `tlon-issue-lookup-all', analogous to `tlon-lookup-all'
(defun tlon-get-meeting-repo (participant1 participant2)
  "Get directory of meeting repo for PARTICIPANT1 and PARTICIPANT2."
  (catch 'found
    (dolist (repo tlon-repos)
      (when (and
	     (eq 'meetings (plist-get repo :subtype))
	     (member participant1 (plist-get repo :participants))
	     (member participant2 (plist-get repo :participants)))
	(throw 'found (plist-get repo :dir))))))

(defun tlon-create-and-visit-issue (title dir)
  "Create an issue with TITLE in DIR and visit it."
  (with-temp-buffer
    (cd dir)
    (when (forge-get-repository :tracked?)
      (tlon-create-issue title dir)
      (forge-pull)
      (while (not (tlon-issue-lookup title dir))
	(sleep-for 0.1))
      (forge-visit-issue (tlon-issue-lookup title dir))
      (format "*forge: %s %s*"
	      (oref (forge-get-repository :tracked?) slug)
	      (oref (forge-current-topic) slug)))))

;; TODO: generalize to all possible meetings
(defun tlon-discuss-issue-in-meeting ()
  "Create a reminder to discuss the current issue in a meeting.
We should try to follow the rule of avoiding prolonged discussions in the issue
tracker, and instead conduct these discussions in person or over a call. This
function tried to be a nudge in that direction."
  (interactive)
  (unless (derived-mode-p 'forge-issue-mode)
    (user-error "This command can only be invoked in Forge issue buffers"))
  (let (backlink)
    (save-excursion
      (let* ((repo-name (oref (forge-get-repository :tracked?) name))
	     (issue-number (oref (forge-current-issue) number))
	     (link (format "tlon-team/%s#%s" repo-name issue-number)))
	(switch-to-buffer (tlon-create-or-visit-meeting-issue))
	(let* ((repo-name (oref (forge-get-repository :tracked?) name))
	       (issue-number (oref (forge-current-issue) number)))
	  (setq backlink (format "tlon-team/%s#%s" repo-name issue-number))
	  (goto-char (point-max))
	  (forward-line -1)
	  (forge-edit-post)
	  (while (not (derived-mode-p 'forge-post-mode))
	    (sleep-for 0.1))
	  (goto-char (point-max))
	  (insert (format "- Discutir %s." link))
	  (forge-post-submit))))
    (forge-create-post)
    (while (not (derived-mode-p 'forge-post-mode))
      (sleep-for 0.1))
    (insert (format "A discutir en %s." backlink))
    (forge-post-submit)))

;;;;; Advice org

;;;###autoload
(defun tlon-set-meeting-buffers (&optional _ _)
  "Open Zoom link and Create or visit appropriate meeting issue."
  (org-open-at-point)
  (let ((heading (org-get-heading t t t t)))
    (cond
     ((string-match "Leo<>Pablo" heading)
      (tlon-create-or-visit-meeting-issue-leo-pablo (format-time-string "%Y-%m-%d")))
     ((string-match "Fede<>Pablo" heading)
      (tlon-create-or-visit-meeting-issue-fede-pablo (format-time-string "%Y-%m-%d")))
     ((string-match "Group meeting" heading)
      (tlon-create-or-visit-group-meeting-issue (format-time-string "%Y-%m-%d"))))))

;;;;; clock-in

(declare-function org-clocking-p "org-clock")
(defun tlon-goto-meeting-when-clocking-in ()
  "Go to the meeting issue when clocking in."
  (when (and (derived-mode-p 'org-mode)
	     (org-clocking-p))
    (tlon-set-meeting-buffers)))

;;;;; diarize & summarize

;;;###autoload
(defun tlon-meet-diarize-and-summarize (audio-file)
  "Diarize AUDIO-FILE and create an AI summary of the conversation.
This function runs the diarization script on the audio file, then uses AI to
generate a summary of the conversation. The summary is saved in the appropriate
meetings repository with the filename format \"yyyy-mm-dd-summary.org\"."
  (interactive "fSelect audio file: ")
  (let* ((default-directory (file-name-directory audio-file))
         (audio-filename (file-name-nondirectory audio-file))
         (date (or (and (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" audio-filename)
                        (match-string 1 audio-filename))
                   (format-time-string "%Y-%m-%d")))
         (transcript-file (concat (file-name-sans-extension audio-file) "-transcript.txt"))
         (buffer (get-buffer-create "*Diarization Output*"))
         (process-name "diarize-process"))
    
    ;; Show the output buffer
    (display-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Starting diarization of %s...\n\n" audio-filename)))
    
    ;; Run the diarization script
    (make-process
     :name process-name
     :buffer buffer
     :command (list "python" tlon-meet-diarize-script "-a" audio-file)
     :sentinel
     (lambda (_process event)
       (when (string= event "finished\n")
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert "\nDiarization complete. Generating summary...\n"))
         
         ;; Check if transcript file exists
         (if (file-exists-p transcript-file)
             (tlon-meet--generate-summary transcript-file date)
           (with-current-buffer buffer
             (goto-char (point-max))
             (insert (format "\nError: Transcript file %s not found.\n" transcript-file)))))))))

(defun tlon-meet--generate-summary (transcript-file date)
  "Generate a summary from TRANSCRIPT-FILE for meeting on DATE."
  (with-temp-buffer
    (insert-file-contents transcript-file)
    (let ((transcript (buffer-string))
          (buffer (get-buffer "*Diarization Output*")))
      
      ;; Use AI to generate summary
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "Reading transcript and generating AI summary...\n"))
      
      (tlon-make-gptel-request
       (format tlon-meet-summary-prompt "%s")
       transcript
       (lambda (response info)
         (if response
             (tlon-meet--save-summary response date buffer)
           (with-current-buffer buffer
             (goto-char (point-max))
             (insert (format "\nError generating summary: %s\n"
                             (plist-get info :status))))))))))

(defun tlon-meet--save-summary (summary date output-buffer)
  "Save SUMMARY for meeting on DATE to appropriate repo.
Updates OUTPUT-BUFFER with progress messages."
  (let* ((meeting-repos (tlon-lookup-all tlon-repos :dir :subtype 'meetings))
         (repo (tlon-meet--determine-repo date meeting-repos))
         (summary-file (expand-file-name "meeting-summaries.org" repo)))
    
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (insert (format "\nSaving summary to %s\n" summary-file)))
    
    ;; Create or append to the summaries file
    (with-temp-buffer
      (when (file-exists-p summary-file)
        (insert-file-contents summary-file))
      
      ;; If file is empty or doesn't exist, add the header
      (when (= (buffer-size) 0)
        (insert "#+TITLE: Meeting Summaries\n")
        (insert "#+OPTIONS: toc:t num:nil\n\n"))
      
      ;; Go to end of file to append
      (goto-char (point-max))
      
      ;; Add a section for this meeting
      (insert (format "\n* Meeting on %s\n\n" date))
      (insert summary)
      
      ;; Save the file
      (write-region (point-min) (point-max) summary-file))
    
    ;; Commit the changes
    (let ((default-directory repo))
      (with-current-buffer output-buffer
        (goto-char (point-max))
        (insert "Committing changes...\n"))
      
      (call-process "git" nil output-buffer t "add" (file-name-nondirectory summary-file))
      (call-process "git" nil output-buffer t "commit" "-m"
                    (format "Add AI-generated summary for meeting on %s" date))
      
      (with-current-buffer output-buffer
        (goto-char (point-max))
        (insert "\nSummary added to meeting-summaries.org successfully!\n")
        (insert (format "Summary file: %s\n" summary-file))))))

(defun tlon-meet--determine-repo (date meeting-repos)
  "Determine which meeting repository to use for DATE.
MEETING-REPOS is a list of meeting repository directories."
  ;; For now, just prompt the user to select the appropriate repo
  ;; This could be enhanced to automatically determine based on calendar data
  (let* ((repo-names (mapcar (lambda (repo)
                               (cons (file-name-nondirectory repo) repo))
                             meeting-repos))
         (selected (completing-read
                    (format "Select meeting repository for %s: " date)
                    repo-names nil t)))
    (cdr (assoc selected repo-names))))

;;;;; Menu

;;;###autoload (autoload 'tlon-meet-menu "tlon-meet.el" nil t)
(transient-define-prefix tlon-meet-menu ()
  "`meet' menu."
  ["Meetings"
   ("l p" "Leo-Pablo"                  tlon-create-or-visit-meeting-issue-leo-pablo)
   ("f p" "Fede-Pablo"                 tlon-create-or-visit-meeting-issue-fede-pablo)
   ("f l" "Fede-Leo"                   tlon-create-or-visit-meeting-issue-fede-leo)
   ("g"   "group"                      tlon-create-or-visit-group-meeting-issue)
   ("i"   "discuss issue in meeting"   tlon-discuss-issue-in-meeting)
   ("d"   "diarize and summarize"      tlon-meet-diarize-and-summarize)])

(provide 'tlon-meet)
;;; tlon-meet.el ends here

