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
(require 'tlon-core)

;;;; Variables

(defconst tlon-meet-format-transcript-prompt
  "Please format the the following conversation transcript. You should replace SPEAKER_nn with the actual speaker name. In this conversation, the speakers are %s.\n\nIn addition, break the text into paragraphs, in a way that is optimized for reading (the kind of format typical of a newspaper or magazine interview).\n\nPlease process the entire file, not just its beginning. In other word, don't say things like \"The conversation continues for a while\", but rather include the text of all the conversation, properly formatted.\n\n%s"
  "Prompt template for formatting meeting summaries.
The %s will be replaced with the participant names and the transcript text.")

(defconst tlon-meet-summarize-transcript-prompt
  "Please provide a concise summary of the following conversation transcript. Focus on the key points discussed, decisions made, and any action items or follow-up tasks mentioned. Format the summary with bullet points for each main topic, and include a section at the end titled 'Action Items' that lists specific tasks that were assigned or mentioned.\n\nUse org-mode syntax: ‘-’ for the bullet points and asterisks for the headings. Use leading two asterisks for the heading with the meeting date, and three leading asterisks for the subheading with the action items (note that the asterisks go only at the beginning; i.e. do not put asterisks at the end of the heading as well).\n\n%s"
  "Prompt template for generating meeting summaries.
The %s will be replaced with the transcript text.")

;;;; User options

(defgroup tlon-meet nil
  "Manage Tlön meetings."
  :group 'tlon)

(defcustom tlon-meet-recordings-directory "~/My Drive/Meet Recordings/"
  "Directory where Google Meet recordings are stored."
  :type 'directory
  :group 'tlon-meet)

(defcustom tlon-zoom-recordings-directory "~/Documents/Zoom/"
  "Directory where Zoom recordings are stored."
  :type 'directory
  :group 'tlon-meet)

(defcustom tlon-default-conference-app 'meet
  "Default conference app for meetings."
  :type '(choice (const :tag "Google Meet" meet)
		 (const :tag "Zoom" zoom)))

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
  (when-let ((repo (forge-get-repository :tracked?)))
    (let* ((get-last-updated (lambda () (oref repo updated)))
           (last-updated (funcall get-last-updated))
           (count 0))
      (forge-pull)
      (while (and (< count 25) (string= last-updated (funcall get-last-updated)))
        (sleep-for 0.1)
        (setq count (1+ count))))))

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

;;;;; transcribe & summarize

(defun tlon-meet--get-audio-file ()
  "Prompt user for an audio file from configured recording directories."
  (let ((default-dir (pcase tlon-default-conference-app
                       ('meet tlon-meet-recordings-directory)
                       ('zoom tlon-zoom-recordings-directory)
                       (_ default-directory))))
    (read-file-name "Select audio file: " default-dir)))

(defun tlon-meet--get-transcript-file (&optional extension)
  "Prompt user for a transcript file.
Default EXTENSION is \".md\"."
  (read-file-name "Select transcript file: " paths-dir-downloads nil t (or extension ".md")))

(defun tlon-meet--get-date-from-filename (filename)
  "Extract date (YYYY-MM-DD) from FILENAME or return current date."
  (or (and (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" filename)
           (match-string 1 filename))
      (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun tlon-meet-transcribe (audio-file participants &optional callback)
  "Transcribe AUDIO-FILE using whisperx and format the result.
Prompts for PARTICIPANTS. Runs `whisperx' on the audio file with diarization
enabled (language hardcoded to \"es\"). Saves a [basename].txt file.
Then, calls `tlon-meet-format-transcript' to generate a formatted Markdown file.
If CALLBACK is provided, call it with the transcript file path after initiating
formatting."
  (interactive (list (tlon-meet--get-audio-file)
                     (completing-read-multiple "Participants: " (tlon-user-lookup-all :nickname))))
  (let* ((audio-filename (file-name-nondirectory audio-file))
         ;; whisperx outputs [basename].txt by default
         (transcript-file (concat (file-name-sans-extension audio-file) ".txt"))
         (buffer (get-buffer-create "*Diarization Output*"))
         (process-name "whisperx-diarize-process")
         (hf-token (auth-source-pass-get "whisperX" (concat "chrome/huggingface.co/" (getenv "PERSONAL_EMAIL"))))
         (whisperx-command (list "whisperx" (expand-file-name audio-file)
                                 "--diarize"
                                 "--language" "es"
                                 "--hf_token" hf-token)))
    ;; Show the output buffer
    (display-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Starting diarization of %s using whisperx...\n\n" audio-filename))
      (insert (format "Running command: %s\n\n" (string-join whisperx-command " "))))
    ;; Run the whisperx command
    (make-process
     :name process-name
     :buffer buffer
     :command whisperx-command
     :sentinel
     (lambda (process event)
       (let ((output-buffer (process-buffer process)))
         (cond
          ((string-match "\\`exited abnormally" event) ;; Check for abnormal exit
           (with-current-buffer output-buffer
             (goto-char (point-max))
             (insert (format "\n\nError: Diarization script failed.\nEvent: %s\nSee buffer output above for details." event))))
          ((string= event "finished\n") ;; Original success condition
           (with-current-buffer output-buffer
             (goto-char (point-max))
             (insert "\nDiarization complete. Checking for transcript...\n"))
           ;; Check if transcript file exists
           (if (file-exists-p transcript-file)
               (progn
                 (with-current-buffer output-buffer
                   (goto-char (point-max))
                   (insert (format "Transcript file found: %s. Starting formatting...\n" transcript-file)))
                 ;; Call the formatting function
                 (tlon-meet-format-transcript transcript-file participants)
                 ;; Call the formatting function
                 (tlon-meet-format-transcript transcript-file participants)
                 ;; Call the original callback if provided (e.g., for summarization)
                 ;; Pass the expected *output* path of the formatted transcript (.md)
                 (when callback
                   (let ((formatted-transcript-file (concat (file-name-sans-extension transcript-file) ".md")))
                     (funcall callback formatted-transcript-file)))
                 )
             (with-current-buffer output-buffer
               (goto-char (point-max))
               (insert (format "\n\nError: Transcript file %s not found after successful diarization.\n" transcript-file)))))
          ;; Ignore other events like "sent signal..." or process output lines
          (t nil)))))))

;;;###autoload
(defun tlon-meet-summarize-transcript (transcript-file &optional participants)
  "Generate AI summary for formatted TRANSCRIPT-FILE (.md) and save to meeting repo.
If PARTICIPANTS (list of nicknames) are provided (e.g., when called from
`tlon-meet-transcribe-and-summarize'), determine the repository automatically.
Otherwise, prompt the user to select the meeting repository.
Reads the formatted Markdown transcript, calls the AI, and saves the summary to
\"meeting-summaries.org\" and copies the transcript to
\"transcripts/YYYY-MM-DD.md\" in the determined repository."
  ;; Prompt for .md file when called interactively
  (interactive (list (tlon-meet--get-transcript-file ".md") nil))
  (let* ((date (tlon-meet--get-date-from-filename transcript-file))
         (repo (if participants
                   (let* ((full-names (mapcar (lambda (nick) (tlon-user-lookup :name :nickname nick)) participants))
                          (num-participants (length full-names)))
                     (cond
                      ((= num-participants 2)
                       (or (tlon-get-meeting-repo (car full-names) (cadr full-names))
                           (user-error "Could not find meeting repo for participants: %s" (tlon-concatenate-list full-names))))
                      ((> num-participants 2)
                       (tlon-repo-lookup :dir :name "meetings-group"))
                      (t (user-error "Cannot determine meeting repo with %d participants" num-participants))))
                 ;; Else (no participants provided), prompt the user
                 (let* ((meeting-repos (tlon-lookup-all tlon-repos :dir :subtype 'meetings))
                        (repo-choices (mapcar (lambda (r)
						(cons (file-name-nondirectory (directory-file-name r)) r))
					      meeting-repos))
                        (selected-name (completing-read "Select meeting repository: " repo-choices nil t)))
                   (cdr (assoc selected-name repo-choices)))))
         (output-buffer (get-buffer-create "*Meeting Summary Output*")))
    (unless repo (user-error "No meeting repository selected or determined"))
    (display-buffer output-buffer)
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert (format "Generating summary for transcript: %s\n" transcript-file))
      (insert (format "Meeting Date: %s\n" date))
      (insert (format "Target Repository: %s\n" repo)))
    (tlon-meet--generate-and-save-summary transcript-file date repo output-buffer)))

(defun tlon-meet--generate-and-save-summary (transcript-md-file date repo output-buffer)
  "Helper to generate summary and save files.
Reads TRANSCRIPT-MD-FILE, calls AI, saves summary and copies transcript to REPO
for meeting on DATE. Updates OUTPUT-BUFFER."
  (with-temp-buffer
    (insert-file-contents transcript-md-file) ; Read the .md file
    (let ((transcript-content (buffer-string)))
      ;; Use AI to generate summary
      (with-current-buffer output-buffer
        (goto-char (point-max))
        (insert "Reading formatted transcript and generating AI summary...\n"))
      (tlon-make-gptel-request
       (format tlon-meet-summarize-transcript-prompt transcript-content) ; Pass transcript content
       nil ; No extra string needed
       (lambda (response info)
         (if response
             ;; Pass the *input* transcript file path to the save function
             (tlon-meet--save-summary-and-transcript response transcript-md-file date repo output-buffer)
           (with-current-buffer output-buffer
             (goto-char (point-max))
             (insert (format "\nError generating summary: %s\n"
                             (plist-get info :status))))))))))

(defun tlon-meet--save-summary-and-transcript (summary input-transcript-file date repo output-buffer)
  "Save SUMMARY and copy INPUT-TRANSCRIPT-FILE for meeting on DATE to REPO.
Updates OUTPUT-BUFFER with progress messages. INPUT-TRANSCRIPT-FILE should be
the formatted Markdown (.md) file."
  (let* ((summary-file (expand-file-name "meeting-summaries.org" repo))
         ;; Save the transcript in the 'transcripts' subdirectory with YYYY-MM-DD.md format
         (transcript-dir (expand-file-name "transcripts" repo))
         (repo-transcript-file (expand-file-name (format "%s.md" date) transcript-dir)))
    ;; Ensure the transcripts directory exists
    (make-directory transcript-dir t)
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (insert (format "\nSaving summary to %s\n" summary-file))
      (insert (format "Saving transcript to %s\n" repo-transcript-file)))
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
      ;; Save the summary file
      (write-region (point-min) (point-max) summary-file))
    ;; Copy the input (formatted) transcript file to the repo
    (copy-file input-transcript-file repo-transcript-file t) ; t = overwrite if exists
    ;; Commit the changes
    (let ((default-directory repo))
      (with-current-buffer output-buffer
        (goto-char (point-max))
        (insert "Committing changes...\n"))
      ;; Add both the summary file and the transcript file (relative to repo root)
      (call-process "git" nil output-buffer t "add" (file-name-nondirectory summary-file))
      (call-process "git" nil output-buffer t "add" (file-relative-name repo-transcript-file repo))
      (call-process "git" nil output-buffer t "commit" "-m"
                    (format "Add AI summary and transcript for meeting on %s" date))
      (with-current-buffer output-buffer
        (goto-char (point-max))
        (insert "\nSummary and transcript added successfully!\n")
        (insert (format "Summary file: %s\n" summary-file))
        (insert (format "Transcript file: %s\n" repo-transcript-file))))))

;;;###autoload
(defun tlon-meet-format-transcript-command (transcript-file participants)
  "Interactively format TRANSCRIPT-FILE using PARTICIPANTS.
Prompts for transcript file and participants, then calls the AI formatter."
  (interactive (list (tlon-meet--get-transcript-file)
                     (completing-read-multiple "Participants: " (tlon-user-lookup-all :nickname))))
  (tlon-meet-format-transcript transcript-file participants))

(defun tlon-meet-format-transcript (transcript-file participants)
  "Generate AI formatted version for TRANSCRIPT-FILE using PARTICIPANTS and save as Markdown.
Reads the transcript, calls the AI with PARTICIPANTS, and saves the formatted
result to a Markdown file (.md) with the same base name in the same directory.
This function is intended to be called programmatically."
  (message "Formatting transcript: %s..." transcript-file)
  (tlon-meet--generate-and-save-formatted-transcript-md transcript-file participants))

(defun tlon-meet--generate-and-save-formatted-transcript-md (transcript-file participants)
  "Helper to generate formatted transcript and save it as Markdown.
Reads TRANSCRIPT-FILE, uses PARTICIPANTS list, calls AI, and saves the result
to a .md file."
  (with-temp-buffer
    (insert-file-contents transcript-file)
    (let ((transcript-content (buffer-string))
          (participants-string (tlon-concatenate-list participants)))
      (message "Reading transcript and generating AI formatted version...")
      (tlon-make-gptel-request
       (format tlon-meet-format-transcript-prompt participants-string transcript-content)
       nil ; No extra string needed
       (lambda (response info)
         (if response
             (let ((output-file (concat (file-name-sans-extension transcript-file) ".md")))
               (with-temp-buffer
                 (insert response)
                 (write-region (point-min) (point-max) output-file))
               (message "Formatted transcript saved to: %s" output-file))
           (message "Error formatting transcript: %s" (plist-get info :status))))))))

;;;###autoload
(defun tlon-meet-transcribe-and-summarize (audio-file participants)
  "Transcribe AUDIO-FILE, format it, and then create an AI summary.
Prompts for PARTICIPANTS. Runs `tlon-meet-transcribe' (which handles
transcription via whisperx and formatting via AI) and, upon success,
automatically runs `tlon-meet-summarize-transcript' on the resulting
formatted Markdown (.md) transcript file."
  (interactive (list (tlon-meet--get-audio-file)
                     (completing-read-multiple "Participants: " (tlon-user-lookup-all :nickname))))
  (tlon-meet-transcribe audio-file participants
			;; Callback function to run after transcription and formatting start
			(lambda (formatted-transcript-file) ; Receives the .md path
			  (message "Transcription/Formatting started. Starting summarization for %s" formatted-transcript-file)
			  ;; Call summarize non-interactively with the .md file and participants
			  (tlon-meet-summarize-transcript formatted-transcript-file participants))))

;;;;; Menu

;;;###autoload (autoload 'tlon-meet-menu "tlon-meet.el" nil t)
(transient-define-prefix tlon-meet-menu ()
  "`meet' menu."
  [["Meetings"
    ("l p" "Leo-Pablo"                  tlon-create-or-visit-meeting-issue-leo-pablo)
    ("f p" "Fede-Pablo"                 tlon-create-or-visit-meeting-issue-fede-pablo)
    ("f l" "Fede-Leo"                   tlon-create-or-visit-meeting-issue-fede-leo)
    ("g"   "group"                      tlon-create-or-visit-group-meeting-issue)]
   ["Processing"
    ("i"   "discuss issue in meeting"   tlon-discuss-issue-in-meeting)
    ("t"   "transcribe audio"           tlon-meet-transcribe)
    ("s"   "summarize transcript"       tlon-meet-summarize-transcript)
    ("a"   "transcribe, format & summarize" tlon-meet-transcribe-and-summarize)]])

(provide 'tlon-meet)
;;; tlon-meet.el ends here

