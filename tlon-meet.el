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
(require 'filenotify)

;;;; Variables

(defconst tlon-meet-format-transcript-prompt
  "Please format the following conversation transcript. The transcript uses placeholders like `SPEAKER_00`, `SPEAKER_01`, etc. You need to replace these placeholders with the actual speaker names. The participants in this conversation are: %s. Analyze the conversation content to determine which speaker corresponds to each placeholder (`SPEAKER_00`, `SPEAKER_01`, etc.) and replace the placeholder accordingly. Use *only* the names provided. Do not use 'Nil', 'Unknown Speaker', or any other placeholder if you are unsure; make your best guess based on the context.\n\nIn addition, break the text into paragraphs, in a way that is optimized for reading (the kind of format typical of a newspaper or magazine interview).\n\nPlease process the entire file, not just its beginning. In other words, don't say things like \"The conversation continues for a while\", but rather include the text of all the conversation, properly formatted.\n\nIMPORTANT: Do not include any introductory or concluding remarks like 'Okay, here is the formatted transcript...'. Just return the formatted transcript itself, starting directly with the content.\n\n%s"
  "Prompt template for formatting meeting summaries.
The first %s will be replaced with the participant names, the second %s with the
transcript text.")

(defconst tlon-meet-cleanup-transcript-prompt
  "Please review the following conversation transcript, which was automatically generated and may contain errors. Correct any mistakes you find, paying attention to context to ensure the meaning is accurate. For example, if the transcript says 'Perdón por la demográfica' in Spanish, but the context suggests 'Perdón por la demora' (Sorry for the delay), please make the correction. Maintain the original speaker attributions (e.g., 'Pablo Stafforini:') and the overall paragraph structure.\n\nIMPORTANT: Do not add any introductory or concluding remarks like 'Here is the corrected transcript...'. Just return the corrected transcript content itself.\n\n%s"
  "Prompt template for cleaning up meeting transcripts.
The %s will be replaced with the formatted transcript text.")

(defconst tlon-meet-summarize-transcript-prompt
  "Please provide a concise summary of the following conversation transcript. Focus on the key points discussed, decisions made, and any action items or follow-up tasks mentioned. Format the summary with bullet points for each main topic, and include a section at the end titled 'Action Items' that lists specific tasks that were assigned or mentioned.\n\nUse org-mode syntax: ‘-’ for the bullet points and asterisks for the headings. Use leading two asterisks for the heading with the meeting date, and three leading asterisks for the subheading with the action items (note that the asterisks go only at the beginning; i.e. do not put asterisks at the end of the heading as well).\n\nIMPORTANT: Do not wrap your response in ```org blocks or any other code fences. Just provide the Org-mode content directly, starting with the `** Meeting on YYYY-MM-DD` heading (replace YYYY-MM-DD with the actual date).\n\n%s"
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

(defvar tlon-meet--recording-watch-descriptor nil
  "File notification descriptor for the recordings directory watch.")

;;;; Functions

;;;;; Meeting Issue Management

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

;;;;; Transcription, Formatting, Summarization

(defun tlon-meet--infer-details-from-filename (filename)
  "Attempt to infer participants and date from FILENAME.
Filename is expected to be like \"Person1<>Person2 - YYYY MM DD ...\" or \"Group
- YYYY MM DD ...\". Returns a plist `(:participants (\"nick1\" \"nick2\") :date
\"YYYY-MM-DD\")' or nil if inference fails."
  (let* ((basename (file-name-nondirectory filename))
         (participants nil)
         (date nil))
    (cond
     ;; Case 1: Person1<>Person2 format
     ((string-match "^\\(.*?\\)<>\\(.*?\\) +- +\\([0-9]\\{4\\}\\)[ -]\\([0-9]\\{2\\}\\)[ -]\\([0-9]\\{2\\}\\)" basename)
      (let* ((name1 (string-trim (match-string 1 basename)))
             (name2 (string-trim (match-string 2 basename))))
        (when (and name1 name2)
          (setq participants (list name1 name2))
          (setq date (format "%s-%s-%s" (match-string 3 basename) (match-string 4 basename) (match-string 5 basename))))))
     ;; Case 2: Group format (assuming "Group" or similar indicates group meeting)
     ;; Add more specific group indicators if needed
     ((string-match "^\\(Group\\|Grupo\\) +- +\\([0-9]\\{4\\}\\)[ -]\\([0-9]\\{2\\}\\)[ -]\\([0-9]\\{2\\}\\)" basename)
      ;; For group meetings, we might not know all participants from the filename.
      ;; We can infer it's a group meeting and the date.
      ;; The function calling this might need special handling for 'group'.
      ;; For now, let's return 'group' as a special participant marker.
      (setq participants '("group")) ; Special marker
      (setq date (format "%s-%s-%s" (match-string 2 basename) (match-string 3 basename) (match-string 4 basename)))))
    ;; Return plist if successful
    (when (and participants date)
      (list :participants participants :date date))))

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
(defun tlon-meet-transcribe-audio (audio-file participants &optional callback)
  "Transcribe AUDIO-FILE using whisperx and format the result for PARTICIPANTS.
Runs `whisperx' on the audio file with diarization enabled (language hardcoded
to \"es\"). Saves a [basename].txt file in the same directory as AUDIO-FILE,
deleting other output files (.vtt, .srt, .tsv, .json). Then, calls
`tlon-meet-format-transcript' to generate a formatted Markdown file using the
provided PARTICIPANTS list. If CALLBACK is provided, call it with the path to
the final cleaned transcript file after the formatting and cleanup steps are
complete.
Interactively, prompts for AUDIO-FILE and PARTICIPANTS, using filename
inference for initial participant suggestion."
  (interactive (tlon-meet--get-file-and-participants)) ; Gets file and confirmed participants
  (let* ((audio-dir (file-name-directory (expand-file-name audio-file)))
         (audio-filename (file-name-nondirectory audio-file))
         ;; whisperx outputs [basename].txt by default in the same directory as the audio file
         (transcript-file (concat (file-name-sans-extension (expand-file-name audio-file)) ".txt"))
         (buffer (get-buffer-create "*Diarization Output*"))
         (process-name "whisperx-diarize-process")
         (hf-token (auth-source-pass-get "whisperX" (concat "chrome/huggingface.co/" (getenv "PERSONAL_EMAIL")))))
    ;; Note: transcript-file path calculation remains above, assuming output lands in audio-dir
    ;; Show the output buffer only if called interactively
    (when (called-interactively-p 'any)
      (display-buffer buffer))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Starting diarization of %s using whisperx...\n\n" audio-filename)))
    ;; Run the whisperx command, ensuring output goes to audio-dir by setting CWD
    (let ((default-directory audio-dir)) ; Set CWD for the process
      ;; Define command inside let, so paths/args are relative to CWD if needed
      (let ((whisperx-command (list "whisperx" (expand-file-name audio-file) ; Absolute path for input
                                    "--diarize"
                                    "--language" "es"
                                    "--hf_token" hf-token
                                    "--output_dir" "."))) ; Output to CWD (which is audio-dir)
        ;; Log the actual command being run
        (with-current-buffer buffer
          (insert (format "Running command in %s: %s\n\n" default-directory (string-join whisperx-command " "))))
        ;; Start the process
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
                   (progn ; Transcript file exists
                     (let ((base-name (file-name-sans-extension transcript-file)))
                       ;; Delete unwanted files, ignoring errors if they don't exist
                       (dolist (ext '(".vtt" ".srt" ".tsv" ".json"))
                         (ignore-errors (delete-file (concat base-name ext))))
                       (with-current-buffer output-buffer
                         (goto-char (point-max))
                         (insert (format "Transcript file found: %s. Cleaned up other formats. Starting formatting...\n" transcript-file)))
                       ;; Call the formatting function, passing the original callback along
                       (tlon-meet-format-transcript transcript-file participants callback)))
                 ;; Else (transcript file not found)
                 (with-current-buffer output-buffer
                   (goto-char (point-max))
                   (insert (format "\n\nError: Transcript file %s not found after successful diarization.\n" transcript-file)))))
              ;; Ignore other events like "sent signal..."
              (t nil))))))))) ; Close lambda, make-process, inner let, outer let

(defun tlon-meet-cleanup-transcript (transcript-md-file &optional callback)
  "Clean up TRANSCRIPT-MD-FILE using AI.
Reads the formatted Markdown transcript, sends it to the AI with a cleanup
prompt, and overwrites the file with the corrected version. If CALLBACK is
provided, call it with the path to the cleaned .md file after successful
saving."
  (let ((output-buffer (get-buffer "*Diarization Output*"))) ; Get the output buffer
    (when output-buffer
      (with-current-buffer output-buffer
        (goto-char (point-max))
        (insert (format "Initiating AI cleanup for: %s\n" transcript-md-file)))))
  (message "Cleaning up transcript: %s..." transcript-md-file)
  (with-temp-buffer
    (insert-file-contents transcript-md-file)
    (let ((transcript-content (buffer-string)))
      (message "Reading formatted transcript and requesting AI cleanup...")
      (tlon-make-gptel-request
       (format tlon-meet-cleanup-transcript-prompt transcript-content)
       nil ; No extra string needed
       (lambda (response info)
         (if response
             (progn
               (let ((output-buffer (get-buffer "*Diarization Output*"))) ; Log success
                 (when output-buffer
                   (with-current-buffer output-buffer
                     (goto-char (point-max))
                     (insert "AI cleanup successful. Saving cleaned file...\n"))))
               (with-temp-buffer
                 (insert response)
                 (write-region (point-min) (point-max) transcript-md-file)) ; Overwrite original
               (message "Transcript cleaned up and saved to: %s" transcript-md-file)
               ;; If a callback was provided, call it now with the cleaned file path
               (when callback
                 (let ((output-buffer (get-buffer "*Diarization Output*"))) ; Log before final callback
                   (when output-buffer
                     (with-current-buffer output-buffer
                       (goto-char (point-max))
                       (insert "Cleanup complete. Triggering next step (e.g., summarization)...\n"))))
                 (funcall callback transcript-md-file)))
           (progn
             (let ((output-buffer (get-buffer "*Diarization Output*"))) ; Log failure
               (when output-buffer
                 (with-current-buffer output-buffer
                   (goto-char (point-max))
                   (insert (format "Error during AI cleanup: %s. Proceeding with uncleaned file...\n" (plist-get info :status))))))
             (message "Error cleaning up transcript: %s. Proceeding with uncleaned version." (plist-get info :status))
             ;; Call callback even on error, passing the uncleaned file path
             (when callback
               (let ((output-buffer (get-buffer "*Diarization Output*"))) ; Log before final callback (on error)
                 (when output-buffer
                   (with-current-buffer output-buffer
                     (goto-char (point-max))
                     (insert "Cleanup failed. Triggering next step (e.g., summarization) with uncleaned file...\n"))))
               (funcall callback transcript-md-file)))))))))

;;;###autoload
(defun tlon-meet-summarize-transcript (transcript-file &optional participants)
  "Generate AI summary for formatted TRANSCRIPT-FILE and save to meetings repo.
If PARTICIPANTS (list of nicknames) are provided (either interactively with
inference or non-interactively), determine the repository automatically based
on them. Otherwise (e.g., interactive call where inference failed or was
overridden, or non-interactive call without participants), prompt the user to
select the meeting repository. Reads the formatted Markdown transcript, calls
the AI, saves the summary to \"meeting-summaries.org\", copies the transcript
to \"transcripts/YYYY-MM-DD.md\" in the determined repository, commits, pushes,
and deletes the original transcript file."
  (interactive
   (let* ((file (tlon-meet--get-transcript-file ".md"))
          (details (tlon-meet--infer-details-from-filename file))
          (inferred-participants (plist-get details :participants)))
     ;; If called interactively, we still need to confirm participants
     ;; to reliably determine the repo without prompting later.
     ;; Use the inferred ones as default if available.
     (list file
           (if inferred-participants
               (completing-read-multiple "Participants (confirm for repo): "
                                         (tlon-user-lookup-all :nickname)
                                         nil nil inferred-participants)
             (completing-read-multiple "Participants (for repo): "
                                       (tlon-user-lookup-all :nickname))))))
  (let* ((date (or (plist-get (tlon-meet--infer-details-from-filename transcript-file) :date)
                   (tlon-meet--get-date-from-filename transcript-file))) ; Fallback date extraction
         (repo (if participants
                   (let* ((is-group (equal participants '("group"))) ; Check for special group marker
                          (full-names (unless is-group (mapcar (lambda (nick) (tlon-user-lookup :name :nickname nick)) participants)))
                          (num-participants (length full-names))) ; Will be 0 if is-group is true
                     (cond
                      (is-group ; Handle the 'group' marker
                       (tlon-repo-lookup :dir :name "meetings-group"))
                      ((= num-participants 2)
                       (or (tlon-get-meeting-repo (car full-names) (cadr full-names))
                           (user-error "Could not find meeting repo for participants: %s" (mapconcat #'identity participants ", "))))
                      ((> num-participants 2) ; Assumed group meeting if > 2 explicit participants
                       (tlon-repo-lookup :dir :name "meetings-group"))
                      ;; Handle cases like 0 or 1 participant if needed, or error
                      (t (user-error "Cannot determine meeting repo for participants: %s" (mapconcat #'identity participants ", ")))))
                 ;; Else (no participants provided), prompt the user
                 (let* ((meeting-repos (tlon-lookup-all tlon-repos :dir :subtype 'meetings))
                        (repo-choices (mapcar (lambda (r)
						(cons (file-name-nondirectory (directory-file-name r)) r))
					      meeting-repos))
                        (selected-name (completing-read "Select meeting repository: " repo-choices nil t)))
                   (cdr (assoc selected-name repo-choices)))))
         (output-buffer (get-buffer-create "*Meeting Summary Output*")))
    (unless repo (user-error "No meeting repository selected or determined"))
    ;; Show the output buffer only if called interactively
    (when (called-interactively-p 'any)
      (display-buffer output-buffer))
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
  "Save SUMMARY, copy INPUT-TRANSCRIPT-FILE to REPO, and delete original.
Saves SUMMARY to `meeting-summaries.org`, copies INPUT-TRANSCRIPT-FILE (the
formatted .md file) to `transcripts/[date].md` in REPO, commits/pushes changes,
and then deletes the original INPUT-TRANSCRIPT-FILE. DATE is the meeting date.
Updates OUTPUT-BUFFER with progress messages."
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
      ;; Commit
      (let ((commit-success (= 0 (call-process "git" nil output-buffer t "commit" "-m"
                                                (format "Add AI summary and transcript for meeting on %s" date)))))
        (with-current-buffer output-buffer
          (goto-char (point-max))
          (if commit-success
              (progn
                (insert "\nCommit successful. Pushing changes...\n")
                ;; Push changes
                (if (= 0 (call-process "git" nil output-buffer t "push"))
                    (progn
                      (insert "Push successful.\n")
                      ;; Delete the original input transcript file after successful commit and push
                      (ignore-errors (delete-file input-transcript-file))
                      (insert (format "Deleted original transcript file: %s\n" input-transcript-file)))
                  (insert "Error: Push failed. See buffer output.\n")))
            (insert "\nError: Commit failed. See buffer output.\n"))))
      (with-current-buffer output-buffer
        (goto-char (point-max))
        (insert "\nSummary and transcript processing complete.\n")
        (insert (format "Summary file: %s\n" summary-file))
        (insert (format "Transcript file: %s\n" repo-transcript-file))))))

(defun tlon-meet-format-transcript (transcript-file participants &optional callback)
  "Generate AI formatted version for TRANSCRIPT-FILE using PARTICIPANTS.
Reads the transcript (.txt), calls the AI with PARTICIPANTS, saves the formatted
result to a Markdown file (.md), deletes the original .txt file, then calls
`tlon-meet-cleanup-transcript' to refine the .md file using AI. If CALLBACK is
provided, it's called after the cleanup step with the path to the cleaned .md
file."
  (message "Formatting transcript: %s..." transcript-file)
  (tlon-meet--generate-and-save-formatted-transcript-md transcript-file participants callback))

(defun tlon-meet--generate-and-save-formatted-transcript-md (transcript-file participants &optional callback)
  "Helper to generate formatted transcript and save it as Markdown.
Reads TRANSCRIPT-FILE, uses PARTICIPANTS list, calls AI, saves the result to a
.md file, and calls CALLBACK if provided."
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
               (message "Formatted transcript saved to: %s" output-file)
               ;; Delete the original .txt file
               (ignore-errors (delete-file transcript-file))
               (message "Deleted original transcript file: %s" transcript-file)
               ;; Call the cleanup function, passing the original callback along
               (tlon-meet-cleanup-transcript output-file callback))
          (message "Error formatting transcript: %s" (plist-get info :status))
          ;; If formatting failed, but a callback exists, call it anyway?
          ;; Or maybe not, as there's no formatted file to process.
          ;; Let's stick to only calling callback on success for now.
          ))))))

;;;###autoload
(defun tlon-meet-transcribe-and-summarize (audio-file participants)
  "Transcribe AUDIO-FILE, format it, clean it, and create an AI summary.
Runs `tlon-meet-transcribe-audio' (which handles transcription via whisperx, AI
formatting, and AI cleanup) using the provided PARTICIPANTS list. Upon success
\\=(after cleanup), automatically runs `tlon-meet-summarize-transcript' on the
resulting formatted and cleaned Markdown (.md) transcript file, using the same
PARTICIPANTS list to determine the repository. Interactively, prompts for
AUDIO-FILE and PARTICIPANTS, using filename inference for initial participant
suggestion."
  (interactive (tlon-meet--get-file-and-participants)) ; Gets file and confirmed participants
  (message "Starting transcription and summarization for %s with participants: %s"
           audio-file (mapconcat #'identity participants ", "))
  (tlon-meet-transcribe-audio audio-file participants
                              ;; Callback function to run after transcription, formatting, AND cleanup
                              (lambda (cleaned-transcript-file) ; Receives the cleaned .md path
                                (message "Transcription/Formatting/Cleanup complete. Starting summarization for %s" cleaned-transcript-file)
                                ;; Call summarize non-interactively with the .md file and participants
                                (tlon-meet-summarize-transcript cleaned-transcript-file participants))))

(defun tlon-meet--get-file-and-participants ()
  "Prompt user for audio file and participants, inferring participants first.
Returns a list: (AUDIO-FILE SELECTED-PARTICIPANTS)."
  (let* ((audio-file (tlon-meet--get-audio-file))
         (details (tlon-meet--infer-details-from-filename audio-file))
         (inferred-participants (plist-get details :participants))
         (all-nicknames (tlon-user-lookup-all :nickname))
         (prompt "Participants: ")
         (selected-participants
          (if inferred-participants
              ;; If inferred, use as initial input, allow confirmation/correction
              (completing-read-multiple prompt all-nicknames nil t inferred-participants nil inferred-participants)
            ;; If not inferred, prompt normally
            (completing-read-multiple prompt all-nicknames nil t))))
    (list audio-file selected-participants)))

;;;;; File Monitoring

(defun tlon-meet--handle-new-recording (event)
  "Handle file creation events in the recording directory.
This function is intended as a callback for `file-notify-add-watch'.
It checks if the EVENT list indicates a file creation (`created` action),
attempts to infer details from the filename (third element of EVENT), and if
successful, calls `tlon-meet-transcribe-and-summarize' non-interactively."
  ;; EVENT is (DESCRIPTOR ACTION FILE [FILE1])
  (let ((action (cadr event))
        (filename (caddr event)))
    (when (and (eq action 'created) filename) ; Check for 'created' action and non-nil filename
      (let ((details (tlon-meet--infer-details-from-filename filename)))
        (if details
            (let ((participants (plist-get details :participants)))
            (message "New recording detected: %s. Participants: %s. Starting processing..."
                     filename (mapconcat #'identity participants ", "))
            ;; Run the processing slightly delayed to ensure file is fully written
            (run-with-idle-timer
             0 nil #'tlon-meet-transcribe-and-summarize filename participants))
        (message "New file detected: %s. Could not infer meeting details. Manual processing required." filename)))))

(defun tlon-meet-watch-recordings ()
  "Start monitoring the default recording directory for new files.
Uses `file-notify-add-watch' to monitor the directory specified by
`tlon-default-conference-app'. When a new file is created, attempts
to automatically process it using `tlon-meet-transcribe-and-summarize'."
  (if tlon-meet--recording-watch-descriptor
      (message "Already monitoring recordings directory.")
    (let ((dir-to-watch (pcase tlon-default-conference-app
                          ('meet tlon-meet-recordings-directory)
                          ('zoom tlon-zoom-recordings-directory)
                          (_ (progn (message "Unknown conference app type.") nil)))))
      (when (and dir-to-watch (file-directory-p dir-to-watch))
        (message "Starting to monitor %s for new recordings..." dir-to-watch)
        (condition-case err
            (setq tlon-meet--recording-watch-descriptor
                  (file-notify-add-watch dir-to-watch
                                         '(change) ; Watch for creation/deletion/change
                                         #'tlon-meet--handle-new-recording))
          (file-notify-error (message "Error starting file monitor: %s" err)))
        (if tlon-meet--recording-watch-descriptor
            (message "Monitoring started for %s." dir-to-watch)
          (message "Failed to start monitoring for %s." dir-to-watch)))
      (unless dir-to-watch
        (message "Could not determine or access directory to watch.")))))

(defun tlon-meet-unwatch-recordings ()
  "Stop monitoring the recording directory for new files."
  (if tlon-meet--recording-watch-descriptor
      (progn
        (message "Stopping monitoring for recordings...")
        (file-notify-rm-watch tlon-meet--recording-watch-descriptor) ; Use rm-watch
        (setq tlon-meet--recording-watch-descriptor nil)
        (message "Monitoring stopped."))
    (message "Not currently monitoring recordings directory.")))

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
    ("t"   "transcribe audio"           tlon-meet-transcribe-audio)
    ("s"   "summarize transcript"       tlon-meet-summarize-transcript)
    ("a"   "transcribe & summarize"     tlon-meet-transcribe-and-summarize)]])

(when (string= (system-name) "Pablos-MacBook-Pro.local")
  (tlon-meet-watch-recordings))

(provide 'tlon-meet)
;;; tlon-meet.el ends here

