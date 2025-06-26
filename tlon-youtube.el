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
(require 'cl-lib)
(require 'paths) ; For paths-dir-downloads
(require 'transient)

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

;;;; Functions

(defun tlon-youtube-generate-wavelength-video ()
  "Generate a video with an animated wavelength from an audio file.
Uses `seewav` to generate the animation. The resolution is determined
by `tlon-youtube-video-resolution`. Prompts the user to select an
audio file from the \"uqbar-audio\" repository. The output video is
saved in `paths-dir-downloads` with a \".mp4\" extension, using
the original audio file name."
  (interactive)
  (let* ((uqbar-audio-dir (tlon-repo-lookup :dir :name "uqbar-audio"))
         (width (car tlon-youtube-video-resolution))
         (height (cdr tlon-youtube-video-resolution)))
    (unless uqbar-audio-dir
      (user-error "Could not find the 'uqbar-audio' repository directory"))
    (let* ((selected-audio-file (read-file-name "Select audio file: " uqbar-audio-dir))
           (audio-file (expand-file-name selected-audio-file))
           (video-file-name (file-name-with-extension (file-name-base selected-audio-file) "mp4"))
           (video-file (expand-file-name (file-name-concat paths-dir-downloads video-file-name)))
           (command (format "seewav -W %d -H %d %s %s"
                            width height
                            (shell-quote-argument audio-file)
                            (shell-quote-argument video-file))))
      (message "Generating %dx%d video with `seewav'..." width height)
      (shell-command command)
      (if (file-exists-p video-file)
          (message "Successfully generated video: %s" video-file)
        (user-error "Failed to generate video. Check *Messages* buffer for seewav output")))))

(defun tlon-youtube-generate-thumbnail ()
  "Generate a thumbnail for the video.
Prompts the user for a title and author(s), and uses the logo from
the \"tlon.team-content\" repository to create a thumbnail image."
  (interactive)
  (let* ((width (car tlon-youtube-video-resolution))
         (height (cdr tlon-youtube-video-resolution))
         (logo-path (expand-file-name "images/ea-logo-transparent.png"
                                      (tlon-repo-lookup :dir :name "tlon.team-content")))
         (font-path (expand-file-name "fonts/GilliusADF-Regular.otf"
                                      (tlon-repo-lookup :dir :name "tlon.team-content")))
         (title (read-string "Enter video title: "))
         (authors (read-string "Enter author(s): "))
         (thumbnail-file (expand-file-name "thumbnail.png" paths-dir-downloads))
         ;; Calculate dynamic sizes and positions - scale up for high density
         (scale-factor 4) ; Render at 4x size then downsample
         (scaled-width (* width scale-factor))
         (scaled-height (* height scale-factor))
         (title-pointsize (* (round (* height 0.08)) scale-factor))
         (title-y-offset (* (round (* height -0.12)) scale-factor))
         (authors-pointsize (* (round (* height 0.05)) scale-factor))
         (authors-y-offset (* (round (* height 0.18)) scale-factor))
         (logo-size (* (round (* height 0.15)) scale-factor))
         (logo-padding (* (round (* width 0.03)) scale-factor))
         (command (format "convert -size %dx%d -define gradient:angle=135 gradient:'#f8f9fa-#e9ecef' \
-font '%s' -pointsize %d -fill '#2c3e50' -stroke '#34495e' -strokewidth %d \
-gravity center -draw \"text 0,%d '%s'\" \
-font '%s' -pointsize %d -fill '#5d6d7e' -stroke none \
-draw \"text 0,%d 'by %s'\" \
\\( '%s' -background none -trim -resize %dx%d \\) \
-gravity southeast -geometry +%d+%d -composite \
-resize %dx%d -quality 95 '%s'"
                          scaled-width scaled-height
                          font-path title-pointsize scale-factor
                          title-y-offset (tlon-youtube--sanitize-draw-string title)
                          font-path authors-pointsize
                          authors-y-offset (tlon-youtube--sanitize-draw-string authors)
                          logo-path logo-size logo-size
                          logo-padding logo-padding
                          width height
                          thumbnail-file)))
    (message "Generating %dx%d thumbnail..." width height)
    (shell-command command)
    (if (file-exists-p thumbnail-file)
        (message "Successfully generated thumbnail: %s" thumbnail-file)
      (user-error "Failed to generate thumbnail. Check *Messages* buffer for convert output"))))

(defun tlon-youtube--sanitize-draw-string (str)
  "Sanitize STR for use in ImageMagick -draw text command.
Replaces single quotes with escaped single quotes (e.g., ' -> \\\\')."
  (replace-regexp-in-string "'" "\\\\'" str t t))

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
                           (ignore-errors (symbol-value (oref obj 'variable)))
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

;;;; Menu

;;;###autoload (autoload 'tlon-youtube-menu "tlon-youtube" nil t)
(transient-define-prefix tlon-youtube-menu ()
  "YouTube menu."
  [["Actions"
    ("g" "Generate wavelength video" tlon-youtube-generate-wavelength-video)
    ("t" "Generate video thumbnail" tlon-youtube-generate-thumbnail)]
   ["Options"
    ("r" "Video Resolution" tlon-youtube-video-resolution-infix)]])

(provide 'tlon-youtube)
;;; tlon-youtube.el ends here
