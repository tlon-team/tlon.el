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
         (font-path (expand-file-name "~/Library/Fonts/GilliusADF-Regular.otf"))
         (title (read-string "Enter video title: "))
         (authors (read-string "Enter author(s): "))
         (language (tlon-select-language 'code))
         (authorship-pattern (tlon-lookup tlon-authorship-pattern :pattern :language language))
         (author-text (format authorship-pattern authors))
         (thumbnail-file (expand-file-name "thumbnail.png" paths-dir-downloads))
         ;; Use high DPI for text rendering
         (dpi 300)
         (scale-factor (/ dpi 72.0)) ; Standard screen DPI is 72
         (scaled-width (round (* width scale-factor)))
         (scaled-height (round (* height scale-factor)))
         (title-pointsize (round (* height 0.05 scale-factor)))
         (title-y-offset (round (* height -0.12 scale-factor)))
         (authors-pointsize (round (* height 0.035 scale-factor)))
         (authors-y-offset (round (* height 0.18 scale-factor)))
         (logo-size (round (* height 0.15 scale-factor)))
         (logo-padding (round (* width 0.03 scale-factor)))
         (stroke-width (round (* 2 scale-factor))))
    ;; Debug output
    (message "=== THUMBNAIL DEBUG INFO ===")
    (message "Target size: %dx%d" width height)
    (message "Scaled size: %dx%d (DPI: %d, scale: %.2f)" scaled-width scaled-height dpi scale-factor)
    (message "Title pointsize: %d (scaled from %.1f)" title-pointsize (* height 0.08))
    (message "Font path: %s" font-path)
    (message "Font exists: %s" (if (file-exists-p font-path) "YES" "NO"))
    (message "Logo path: %s" logo-path)
    (message "Logo exists: %s" (if (file-exists-p logo-path) "YES" "NO"))
    ;; Check if font exists
    (unless (file-exists-p font-path)
      (user-error "Font file not found: %s" font-path))
    ;; Check if logo exists
    (unless (file-exists-p logo-path)
      (user-error "Logo file not found: %s" logo-path))
    (let* ((title-lines (tlon-youtube--wrap-text title (* scaled-width 0.8) (/ title-pointsize scale-factor)))
           (line-height (round (* title-pointsize 1.2)))
           (total-title-height (* (length title-lines) line-height))
           (title-start-y (- title-y-offset (/ total-title-height 2)))
           (title-draw-commands
            (mapconcat
             (lambda (line-info)
               (let ((line (car line-info))
                     (y-pos (cdr line-info)))
                 (format "-draw \"text 0,%d '%s'\""
                         y-pos (tlon-youtube--sanitize-draw-string line))))
             (cl-loop for line in title-lines
                      for i from 0
                      collect (cons line (+ title-start-y (* i line-height))))
             " "))
           (command (format "convert -density %d -size %dx%d -define gradient:angle=135 gradient:'#f8f9fa-#e9ecef' -font %s -pointsize %d -fill '#2c3e50' -stroke '#34495e' -strokewidth %d -gravity center %s -font %s -pointsize %d -fill '#5d6d7e' -stroke none -draw \"text 0,%d '%s'\" \\( %s -density %d -background none -trim -resize %dx%d \\) -gravity southeast -geometry +%d+%d -composite -resize %dx%d -quality 95 %s"
                            dpi scaled-width scaled-height
                            (shell-quote-argument font-path) title-pointsize
                            stroke-width title-draw-commands
                            (shell-quote-argument font-path) authors-pointsize
                            authors-y-offset (tlon-youtube--sanitize-draw-string author-text)
                            (shell-quote-argument logo-path) dpi logo-size logo-size
                            logo-padding logo-padding
                            width height
                            (shell-quote-argument thumbnail-file))))
      (message "Command: %s" command)
      (message "Generating %dx%d thumbnail at %d DPI..." width height dpi)
      (let ((result (shell-command command)))
        (message "Shell command result: %d" result)
        (if (file-exists-p thumbnail-file)
            (progn
              (message "Successfully generated thumbnail: %s" thumbnail-file)
              (message "File size: %d bytes" (file-attribute-size (file-attributes thumbnail-file))))
          (user-error "Failed to generate thumbnail. Check *Messages* buffer for convert output"))))))

(defun tlon-youtube--sanitize-draw-string (str)
  "Sanitize STR for use in ImageMagick -draw text command.
Replaces single quotes with escaped single quotes (e.g., ' -> \\\\')."
  (replace-regexp-in-string "'" "\\\\'" str t t))

(defun tlon-youtube--wrap-text (text max-width pointsize)
  "Wrap TEXT to fit within MAX-WIDTH pixels at POINTSIZE.
Returns a list of lines that will fit within the specified width.
Uses a rough approximation of character width."
  (let* ((char-width (* pointsize 0.6)) ; Approximate character width
         (chars-per-line (max 1 (floor (/ max-width char-width))))
         (words (split-string text))
         (lines '())
         (current-line ""))
    (dolist (word words)
      (let ((test-line (if (string-empty-p current-line)
                           word
                         (concat current-line " " word))))
        (if (<= (length test-line) chars-per-line)
            (setq current-line test-line)
          (when (not (string-empty-p current-line))
            (push current-line lines))
          (setq current-line word))))
    (when (not (string-empty-p current-line))
      (push current-line lines))
    (reverse lines)))

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
