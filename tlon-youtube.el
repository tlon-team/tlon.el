;;; tlon-youtube.el --- YouTube integration for Tlön. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.0
;; URL: https://github.com/tlon-team/tlon
;; Keywords: multimedia, api, youtube
;; Package-Requires: ((emacs "27.1"))
;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides functionality to process and upload podcast episodes to
;; YouTube.

;;; Code:

(require 'tlon-core)
(require 'tlon)
(require 'cl-lib)
(require 'paths)
(require 'transient)
(require 'url)
(require 'json)
(require 'oauth2-auto)

;;;; Constants

(defconst tlon-youtube-thumbnail-command-template
  "magick -density %d -size %dx%d -define gradient:angle=135 gradient:'#f8f9fa-#e9ecef' -font %s -size %dx%d -background none -fill '#2c3e50' -stroke '#34495e' -strokewidth %d -gravity center caption:'%s' -geometry +0%d -composite -font %s -pointsize %d -fill '#5d6d7e' -stroke '#5d6d7e' -strokewidth 1 -gravity center -annotate +0+%d '%s' \\( %s -density %d -background none -trim -resize %dx%d \\) -gravity southeast -geometry +%d+%d -composite -font %s -pointsize %d -fill '#7f8c8d' -gravity southwest -annotate +%d+%d '%s' -resize %dx%d -quality 95 %s"
  "ImageMagick command template for generating YouTube thumbnails.

Format arguments (in order):
1. DPI (300)
2. Scaled canvas width
3. Scaled canvas height
4. Font path
5. Text area width
6. Text area height
7. Stroke width
8. Title text
9. Title Y offset
10. Font path (author)
11. Author pointsize
12. Author Y offset
13. Author text
14. Logo path
15. Logo DPI
16. Logo width
17. Logo height
18. Logo padding X
19. Logo padding Y
20. Monospace font path (URL)
21. URL pointsize
22. URL padding X
23. URL padding Y
24. URL text
25. Final width
26. Final height
27. Output file path")

;;;; User options

(defgroup tlon-youtube ()
  "`tlon-youtube' user options."
  :group 'tlon)

(defcustom tlon-youtube-video-resolution '(1280 . 720)
  "Video resolution (WIDTH . HEIGHT) for generated videos.
Common resolutions:
- 720p:  (1280 . 720)
- 1080p: (1920 . 1080)
- 1440p: (2560 . 1440)
- 4K (2160p): (3840 . 2160)"
  :type '(cons integer integer)
  :group 'tlon-youtube)

(defcustom tlon-youtube-api-key nil
  "YouTube Data API v3 key for API requests.
Get this from the Google Cloud Console."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "API Key"))
  :group 'tlon-youtube)

(defcustom tlon-youtube-client-id
  (auth-source-pass-get "desktop-client-id" (concat "tlon/core/console.cloud.google.com/" tlon-email-shared))
  "OAuth 2.0 client ID for YouTube API authentication.
Get this from the Google Cloud Console."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "Client ID"))
  :group 'tlon-youtube)

(defcustom tlon-youtube-client-secret
  (auth-source-pass-get "desktop-client-secret" (concat "tlon/core/console.cloud.google.com/" tlon-email-shared))
  "OAuth 2.0 client secret for YouTube API authentication.
Get this from the Google Cloud Console."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "Client Secret"))
  :group 'tlon-youtube)

(defcustom tlon-youtube-default-privacy "private"
  "Default privacy setting for uploaded videos.
Valid values are: \"private\", \"unlisted\", \"public\"."
  :type '(choice (const "private")
                 (const "unlisted")
                 (const "public"))
  :group 'tlon-youtube)

;;;; Functions

;;;;; Create

;;;;;; Video resolution

(defconst tlon-youtube-resolution-choices
  '(("720p (1280x720)"   . (1280 . 720))
    ("1080p (1920x1080)" . (1920 . 1080))
    ("1440p (2560x1440)" . (2560 . 1440))
    ("4K (2160p) (3840x2160)" . (3840 . 2160)))
  "Alist of predefined resolution choices for YouTube videos.
The car is the display string and the cdr is the (WIDTH . HEIGHT) cons cell")

(defun tlon-youtube-read-resolution-choice (prompt obj _history)
  "Reader function for `tlon-youtube-video-resolution`.
PROMPT is the prompt string. OBJ is the transient infix object. _HISTORY is the
history list (unused). Allows selecting from predefined resolutions."
  (let* ((val-from-obj (if (and obj (slot-boundp obj 'variable))
                           (ignore-errors (symbol-value (oref obj variable)))
                         nil))
         (initial-value (if (consp val-from-obj)
                            val-from-obj
                          (if (consp tlon-youtube-video-resolution)
                              tlon-youtube-video-resolution
                            '(1280 . 720))))
         (choices (mapcar #'car tlon-youtube-resolution-choices))
         (found-pair (cl-find-if (lambda (pair) (equal (cdr pair) initial-value))
                                 tlon-youtube-resolution-choices))
         (current-selection-str
          (if found-pair
              (car found-pair)
            (car choices)))
         (selection (completing-read prompt choices nil t nil nil current-selection-str)))
    (cond
     ((or (null selection) (string-empty-p selection)) initial-value)
     (t
      (let ((choice-pair (assoc selection tlon-youtube-resolution-choices)))
        (cdr choice-pair))))))

(transient-define-infix tlon-youtube-video-resolution-infix ()
  "Set the video resolution for YouTube videos."
  :class 'transient-lisp-variable
  :variable 'tlon-youtube-video-resolution
  :reader #'tlon-youtube-read-resolution-choice
  :transient t)

;;;;;; Wavelength video

(defun tlon-youtube-generate-wavelength-video ()
  "Generate a video with an animated wavelength from an audio file.
The command runs asynchronously and opens the video upon completion.
Uses `seewav` to generate the animation. The resolution is determined
by `tlon-youtube-video-resolution`. Prompts the user to select an
audio file from the \"uqbar-audio\" repository. The output video is
saved in `paths-dir-downloads` with a \".mp4\" extension, using
the original audio file name."
  (interactive)
  (unless (executable-find "seewav")
    (user-error "`seewav' is not installed or not in your PATH"))
  (let* ((uqbar-audio-dir (tlon-repo-lookup :dir :name "uqbar-audio"))
         (width (car tlon-youtube-video-resolution))
         (height (cdr tlon-youtube-video-resolution)))
    (unless uqbar-audio-dir
      (user-error "Could not find the 'uqbar-audio' repository directory"))
    (let* ((selected-audio-file (read-file-name "Select audio file: " uqbar-audio-dir))
           (audio-file (expand-file-name selected-audio-file))
           (video-file-name (file-name-with-extension (file-name-base selected-audio-file) "mp4"))
           (video-file (expand-file-name (file-name-concat paths-dir-downloads video-file-name)))
           (process-name "seewav-generate-video")
           (output-buffer (generate-new-buffer (format "*%s-output*" process-name)))
           (command `("seewav"
                      "-W" ,(number-to-string width)
                      "-H" ,(number-to-string height)
                      ,audio-file
                      ,video-file)))
      (message "Generating video in the background...")
      (let ((process (apply #'start-process process-name output-buffer command)))
        (set-process-sentinel
         process
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let ((output-buf (process-buffer proc)))
               (if (and (zerop (process-exit-status proc)) (file-exists-p video-file))
                   (progn
                     (message "Successfully generated video: %s. Opening it now..." video-file)
                     (tlon-browse-file video-file)
                     (when (buffer-live-p output-buf)
                       (kill-buffer output-buf)))
                 (progn
                   (message "Failed to generate video. Check the `%s' buffer for details." (buffer-name output-buf))
                   (pop-to-buffer output-buf)))))))))))

;;;;;; Thumbnail

(defun tlon-youtube-generate-thumbnail ()
  "Generate a thumbnail for the video.
Prompts the user for a title and author(s), and uses the logo from
the \"tlon.team-content\" repository to create a thumbnail image."
  (interactive)
  (let* ((width (car tlon-youtube-video-resolution))
         (height (cdr tlon-youtube-video-resolution))
         (logo-path (expand-file-name "images/ea-logo-transparent.png"
                                      (tlon-repo-lookup :dir :name "tlon.team-content")))
	 (font-dir (expand-file-name "~/Library/Fonts/"))
         (font-path (file-name-concat font-dir "GilliusADF-Regular.otf"))
         (monospace-font-path (file-name-concat font-dir "SauceCodeProNerdFontMono-Italic.ttf"))
         (title (read-string "Enter video title: "))
         (authors (read-string "Enter author(s): "))
         (author-text authors)
         ;; Use high DPI for text rendering to prevent pixelation
         (thumbnail-file (expand-file-name "thumbnail.png" paths-dir-downloads))
         (dpi 300) ; Standard screen DPI is 72
         (scale-factor (/ dpi 72.0))
         (scaled-width (round (* width scale-factor)))
         (scaled-height (round (* height scale-factor)))
         (text-width (round (* scaled-width 0.8)))
         (text-height (round (* scaled-height 0.4)))
         (title-y-offset (round (* scaled-height -0.18)))
         (authors-pointsize (round (* scaled-height 0.035)))
         (authors-y-offset (round (* scaled-height 0.12)))
         (logo-size (round (* scaled-height 0.18)))
         (logo-padding (round (* scaled-width 0.03)))
         (stroke-width (round (* 2 scale-factor)))
         (url-pointsize (round (* scaled-height 0.013)))
         (url-padding-x (round (* scaled-width 0.02)))
         (url-padding-y (round (* scaled-height 0.035)))
         (url-text "altruismoeficaz.net")
         (command (format tlon-youtube-thumbnail-command-template
                          dpi scaled-width scaled-height
                          (shell-quote-argument font-path)
                          text-width text-height
                          stroke-width
                          (tlon-youtube--sanitize-draw-string title)
                          title-y-offset
                          (shell-quote-argument font-path)
                          authors-pointsize
                          authors-y-offset
                          (tlon-youtube--sanitize-draw-string author-text)
                          (shell-quote-argument logo-path)
                          dpi logo-size logo-size
                          logo-padding logo-padding
                          (shell-quote-argument monospace-font-path)
                          url-pointsize
                          url-padding-x url-padding-y
                          url-text
                          width height
                          (shell-quote-argument thumbnail-file))))
    (unless (file-exists-p font-path)
      (user-error "Font file not found: %s" font-path))
    (unless (file-exists-p logo-path)
      (user-error "Logo file not found: %s" logo-path))
    (shell-command command)
    (if (file-exists-p thumbnail-file)
        (message "Successfully generated thumbnail: %s" thumbnail-file)
      (user-error "Failed to generate thumbnail. Check *Messages* buffer for convert output"))))

(defun tlon-youtube--sanitize-draw-string (str)
  "Sanitize STR for use in ImageMagick -draw text command.
Replaces single quotes with escaped single quotes (e.g., ' -> \\\\')."
  (replace-regexp-in-string "'" "\\\\'" str t t))

;;;;; Upload

;;;;;; Upload video

(defun tlon-youtube--prepare-upload-request (video-file title description privacy)
  "Prepare resumable upload commands for YouTube upload.
Returns a list of (INIT-COMMAND UPLOAD-COMMAND METADATA-FILE)."
  (let* ((metadata-file (make-temp-file "youtube-metadata-" nil ".json"))
         (metadata (json-encode
                    `((snippet . ((title . ,title)
                                  (description . ,description)))
                      (status . ((privacyStatus . ,privacy))))))
         (access-token (tlon-youtube--get-access-token))
         (init-url "https://www.googleapis.com/upload/youtube/v3/videos?uploadType=resumable&part=snippet,status")
         (init-command `("curl" "-v" "-X" "POST"
                         "--data" ,metadata
                         "-H" ,(format "Authorization: Bearer %s" access-token)
                         "-H" "Content-Type: application/json; charset=UTF-8"
                         "-H" "X-Upload-Content-Type: video/mp4"
                         "-H" ,(format "X-Upload-Content-Length: %d" (file-attribute-size (file-attributes video-file)))
                         "-D" "-"  ; Include response headers in output
                         ,init-url))
         (upload-command `("curl" "-v" "-X" "PUT"
                           "--data-binary" ,(format "@%s" video-file)
                           "-H" "Content-Type: video/mp4"
                           "UPLOAD_URL_PLACEHOLDER")))
    ;; Write metadata to file
    (with-temp-file metadata-file
      (insert metadata))
    (list init-command upload-command metadata-file)))

(defun tlon-youtube-upload-video ()
  "Upload a video file to YouTube.
Prompts for video file, title, description, and privacy setting."
  (interactive)
  (unless (and tlon-youtube-client-id tlon-youtube-client-secret)
    (user-error "YouTube API credentials not configured. Set `tlon-youtube-client-id` and `tlon-youtube-client-secret`"))
  (let* ((video-file (read-file-name "Select video file: " paths-dir-downloads nil t nil
                                     (lambda (name) (string-match-p "\\.mp4\\'" name))))
         (title (read-string "Video title: "))
         (description (read-string "Video description: "))
         (privacy (completing-read "Privacy setting: "
                                   '("private" "unlisted" "public")
                                   nil t nil nil tlon-youtube-default-privacy)))
    (unless (file-exists-p video-file)
      (user-error "Video file does not exist: %s" video-file))
    (tlon-youtube--upload-video-file video-file title description privacy)))

(defun tlon-youtube--upload-video-file (video-file title description privacy)
  "Upload VIDEO-FILE to YouTube with TITLE, DESCRIPTION, and PRIVACY setting."
  (unless (executable-find "curl")
    (user-error "`curl' is not installed or not in your PATH"))
  (let* ((request-data (tlon-youtube--prepare-upload-request
                        video-file title description privacy))
         (init-command (nth 0 request-data))
         (upload-command (nth 1 request-data))
         (metadata-file (nth 2 request-data)))
    (message "Metadata file: %s" metadata-file)
    (message "Step 1 - Initialize upload: %s"
             (string-join (mapcar #'shell-quote-argument init-command) " "))
    
    ;; Step 1: Initialize the resumable upload
    (let* ((init-process-name "youtube-upload-init")
           (init-output-buffer (generate-new-buffer (format "*%s-output*" init-process-name)))
           (init-process (apply #'start-process init-process-name init-output-buffer init-command)))
      (set-process-sentinel
       init-process
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let ((exit-status (process-exit-status proc))
                 (output-buf (process-buffer proc)))
             (with-current-buffer output-buf
               (let* ((full-output (buffer-string))
                      (upload-url (tlon-youtube--extract-upload-url full-output)))
                 (cond
                  ((and (zerop exit-status) upload-url)
                   (message "Upload URL obtained: %s" upload-url)
                   ;; Step 2: Upload the actual video file
                   (let ((final-upload-command (cl-substitute upload-url "UPLOAD_URL_PLACEHOLDER" upload-command :test #'string=)))
                     (message "Step 2 - Upload video file: %s"
                              (string-join (mapcar #'shell-quote-argument final-upload-command) " "))
                     (tlon-youtube--execute-video-upload final-upload-command metadata-file)))
                  (t
                   (message "Failed to initialize upload (exit code %d). Check `%s' for details." exit-status (buffer-name output-buf))
                   (message "Metadata file kept for debugging: %s" metadata-file)
                   (pop-to-buffer output-buf))))))))))))

(defun tlon-youtube--extract-upload-url (curl-output)
  "Extract the upload URL from CURL-OUTPUT containing response headers."
  (when (string-match "^Location: \\(https://[^\r\n]+\\)" curl-output)
    (match-string 1 curl-output)))

(defun tlon-youtube--execute-video-upload (upload-command metadata-file)
  "Execute the video UPLOAD-COMMAND and handle the response."
  (let* ((upload-process-name "youtube-video-upload")
         (upload-output-buffer (generate-new-buffer (format "*%s-output*" upload-process-name)))
         (upload-process (apply #'start-process upload-process-name upload-output-buffer upload-command)))
    (set-process-sentinel
     upload-process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((exit-status (process-exit-status proc))
               (output-buf (process-buffer proc)))
           (with-current-buffer output-buf
             (let* ((full-output (buffer-string))
                    (json-start-pos (save-excursion
                                      (goto-char (point-max))
                                      (search-backward "{" nil t)))
                    (json-response (if json-start-pos
                                       (buffer-substring-no-properties json-start-pos (point-max))
                                     full-output))
                    (response-data (condition-case nil (json-read-from-string json-response) (error nil)))
                    (video-id (and response-data (cdr (assoc 'id response-data))))
                    (error-info (and response-data (cdr (assoc 'error response-data)))))
               (cond
                (video-id
                 (message "Video uploaded successfully! Video ID: %s" video-id)
                 (when (file-exists-p metadata-file) (delete-file metadata-file))
                 (kill-buffer output-buf))
                (error-info
                 (let ((error-code (cdr (assoc 'code error-info)))
                       (error-message (cdr (assoc 'message error-info))))
                   (message "YouTube API Error %s: %s" error-code error-message))
                 (message "Metadata file kept for debugging: %s" metadata-file)
                 (pop-to-buffer output-buf))
                ((not (zerop exit-status))
                 (message "Video upload failed with exit code %d. Check `%s' for full `curl -v' output." exit-status (buffer-name output-buf))
                 (message "Metadata file kept for debugging: %s" metadata-file)
                 (pop-to-buffer output-buf))
                (t
                 (message "Upload completed but no JSON response found. Check `%s' for output." (buffer-name output-buf))
                 (message "Metadata file kept for debugging: %s" metadata-file)
                 (pop-to-buffer output-buf)))))))))))

(defun tlon-youtube-prepare-upload-command ()
  "Prepare and display the curl commands for a YouTube video upload.
This command prepares the metadata and constructs the `curl`
commands for the two-step resumable upload process, but does not
execute them. It prints the commands to the *Messages* buffer so
they can be run manually in a terminal for debugging."
  (interactive)
  (unless (and tlon-youtube-client-id tlon-youtube-client-secret)
    (user-error "YouTube API credentials not configured. Set `tlon-youtube-client-id` and `tlon-youtube-client-secret`"))
  (let* ((video-file (read-file-name "Select video file: " paths-dir-downloads nil t nil
                                     (lambda (name) (string-match-p "\\.mp4\\'" name))))
         (title (read-string "Video title: "))
         (description (read-string "Video description: "))
         (privacy (completing-read "Privacy setting: "
                                   '("private" "unlisted" "public")
                                   nil t nil nil tlon-youtube-default-privacy)))
    (unless (file-exists-p video-file)
      (user-error "Video file does not exist: %s" video-file))
    (let* ((request-data (tlon-youtube--prepare-upload-request
                          video-file title description privacy))
           (init-command (nth 0 request-data))
           (upload-command (nth 1 request-data))
           (metadata-file (nth 2 request-data))
           (init-shell-command (string-join (mapcar #'shell-quote-argument init-command) " "))
           (upload-shell-command (string-join (mapcar #'shell-quote-argument upload-command) " ")))
      (message "Manual curl commands prepared for resumable upload:")
      (message "")
      (message "Step 1 - Initialize upload (run this first):")
      (message "%s" init-shell-command)
      (message "")
      (message "Step 2 - Upload video file (replace UPLOAD_URL_PLACEHOLDER with Location header from step 1):")
      (message "%s" upload-shell-command)
      (message "")
      (message "Metadata file: %s" metadata-file)
      (message "Video file: %s" video-file))))

;;;;;; Upload thumbnail

(defun tlon-youtube-upload-thumbnail ()
  "Upload a thumbnail to an existing YouTube video.
Prompts for thumbnail file and video ID."
  (interactive)
  (unless (and tlon-youtube-client-id tlon-youtube-client-secret)
    (user-error "YouTube API credentials not configured"))
  (let* ((thumbnail-file (read-file-name "Select thumbnail file: " paths-dir-downloads nil t nil
                                         (lambda (name) (string-match-p "\\.\\(png\\|jpg\\|jpeg\\)\\'" name))))
         (video-id (read-string "YouTube video ID: ")))
    (unless (file-exists-p thumbnail-file)
      (user-error "Thumbnail file does not exist: %s" thumbnail-file))
    (tlon-youtube--upload-thumbnail-file thumbnail-file video-id)))

(defun tlon-youtube--upload-thumbnail-file (thumbnail-file video-id)
  "Upload THUMBNAIL-FILE to YouTube video with VIDEO-ID."
  (unless (executable-find "curl")
    (user-error "`curl' is not installed or not in your PATH"))
  (unless (file-exists-p thumbnail-file)
    (user-error "Thumbnail file does not exist: %s" thumbnail-file))
  (unless (file-readable-p thumbnail-file)
    (user-error "Thumbnail file is not readable: %s" thumbnail-file))
  (let* ((access-token (tlon-youtube--get-access-token))
         (url (format "https://www.googleapis.com/upload/youtube/v3/thumbnails/set?videoId=%s" video-id))
         (request-body-file (make-temp-file "youtube-thumbnail-" nil ".png")))
    (copy-file thumbnail-file request-body-file t)
    (let* ((process-name "youtube-thumbnail-upload")
           (output-buffer (generate-new-buffer (format "*%s-output*" process-name)))
           (command `("curl" "-s" "-X" "POST"
                      "--data-binary" ,(format "@%s" request-body-file)
                      "-H" ,(format "Authorization: Bearer %s" access-token)
                      "-H" "Content-Type: image/png"
                      ,url)))
      (let ((process (apply #'start-process process-name output-buffer command)))
        (set-process-sentinel
         process
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (unwind-protect
                 (with-current-buffer (process-buffer proc)
                   (let* ((json-response (buffer-string))
                          (response-data (condition-case nil
                                             (json-read-from-string json-response)
                                           (error nil)))
                          (error-info (and response-data (cdr (assoc 'error response-data)))))
                     (cond
                      (error-info
                       (let ((error-code (cdr (assoc 'code error-info)))
                             (error-message (cdr (assoc 'message error-info))))
                         (message "YouTube API Error %s: %s" error-code error-message)))
                      (response-data
                       (message "Thumbnail uploaded successfully!"))
                      (t
                       (message "Thumbnail upload completed. Response: %s" json-response)))))
               (delete-file request-body-file)
               (kill-buffer (process-buffer proc))))))))))

;;;;;; Authorize upload

(defun tlon-youtube-authorize ()
  "Force re-authorization for YouTube API access.
This is useful if the stored tokens are invalid or have been revoked."
  (interactive)
  (unless (and tlon-youtube-client-id tlon-youtube-client-secret)
    (user-error "YouTube API credentials not configured"))
  (require 'oauth2-auto)
  (message "Starting authorization process... Please check your browser.")
  (oauth2-auto-poll-promise (oauth2-auto-force-reauth tlon-email-shared 'tlon-youtube))
  (message "Authorization process completed."))

(defun tlon-youtube--get-access-token ()
  "Get a valid OAuth 2.0 access token for YouTube API using oauth2-auto."
  (unless (and tlon-youtube-client-id tlon-youtube-client-secret)
    (user-error "YouTube API credentials not configured"))
  (require 'oauth2-auto)
  (oauth2-auto-access-token-sync tlon-email-shared 'tlon-youtube))

;;;;; Menu

;;;###autoload (autoload 'tlon-youtube-menu "tlon-youtube" nil t)
(transient-define-prefix tlon-youtube-menu ()
  "YouTube menu."
  [["Generate"
    ("g" "Generate wavelength video" tlon-youtube-generate-wavelength-video)
    ("t" "Generate video thumbnail" tlon-youtube-generate-thumbnail)]
   ["Upload"
    ("u" "Upload video to YouTube" tlon-youtube-upload-video)
    ("T" "Upload thumbnail to YouTube" tlon-youtube-upload-thumbnail)]
   ["Debug"
    ("P" "Prepare upload command" tlon-youtube-prepare-upload-command)]
   ["Options"
    ("r" "Video Resolution" tlon-youtube-video-resolution-infix)
    ("a" "Authorize YouTube API" tlon-youtube-authorize)]])

(defun tlon-youtube--oauth2-auto-setup ()
  "Setup OAuth2 authentication for YouTube using oauth2-auto."
  (add-to-list
   'oauth2-auto-additional-providers-alist
   `(tlon-youtube
     (authorize_url . "https://accounts.google.com/o/oauth2/v2/auth")
     (token_url . "https://oauth2.googleapis.com/token")
     (scope . "https://www.googleapis.com/auth/youtube.upload")
     (client_id . ,tlon-youtube-client-id)
     (client_secret . ,tlon-youtube-client-secret))))

(if (and tlon-youtube-client-id tlon-youtube-client-secret)
    (tlon-youtube--oauth2-auto-setup)
  (unless noninteractive
    (warn "tlon-youtube: must set `tlon-youtube-client-id' and `tlon-youtube-client-secret' for this package to work.")))

(provide 'tlon-youtube)
;;; tlon-youtube.el ends here
