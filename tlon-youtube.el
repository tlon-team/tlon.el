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

;; Ensure tlon-authorship-pattern is available
(defvar tlon-authorship-pattern)

;;;; Constants

(defconst tlon-youtube-thumbnail-command-template
  "magick -density %d -size %dx%d -define gradient:angle=135 gradient:'#f8f9fa-#e9ecef' \\( -size %dx%d -background none -font %s -pointsize %d -fill '#2c3e50' -stroke '#34495e' -strokewidth %d %s \\) -gravity center -geometry +0%d -composite \\( -font %s -pointsize %d -fill '#5d6d7e' -stroke none -gravity center -draw \"text 0,%d '%s'\" -size %dx%d xc:none \\) -gravity center -composite \\( %s -density %d -background none -trim -resize %dx%d \\) -gravity southeast -geometry +%d+%d -composite -resize %dx%d -quality 95 %s"
  "ImageMagick command template for generating YouTube thumbnails.

This template creates a thumbnail with the following components:
- `-density %d`: Sets rendering DPI for high-quality text (300 DPI)
- `-size %dx%d`: Canvas dimensions (scaled width x scaled height)
- `-define gradient:angle=135 gradient:'#f8f9fa-#e9ecef'`: Diagonal gradient background
- First subcommand \\(...\\): Title text rendering
  - `-size %dx%d`: Text area dimensions (80% of canvas width x 60% of canvas height)
  - `-background none`: Transparent background for text area
  - `-font %s`: Font file path (GilliusADF-Regular.otf)
  - `-pointsize %d`: Title font size (scaled)
  - `-fill '#2c3e50'`: Dark blue-gray text color
  - `-stroke '#34495e'`: Darker stroke outline for better contrast
  - `-strokewidth %d`: Stroke width (scaled)
  - `-gravity center`: Center text alignment
  - `%s`: Caption text with automatic wrapping
- `-gravity center -geometry +0%d -composite`: Position title text on canvas
- Second subcommand \\(...\\): Author text rendering
  - `-font %s`: Same font file
  - `-pointsize %d`: Author font size (smaller than title)
  - `-fill '#5d6d7e'`: Medium gray color for hierarchy
  - `-stroke none`: No stroke for author text
  - `-draw \"text 0,%d '%s'\"`: Draw author text at specified Y offset
  - `-size %dx%d xc:none`: Transparent canvas for author text
- Third subcommand \\(...\\): Logo processing
  - `%s`: Logo file path
  - `-density %d`: Logo rendering DPI
  - `-background none`: Preserve logo transparency
  - `-trim`: Remove excess whitespace
  - `-resize %dx%d`: Scale logo to specified dimensions
- `-gravity southeast -geometry +%d+%d -composite`: Position logo in bottom-right
- `-resize %dx%d`: Final downsampling to target resolution
- `-quality 95`: High JPEG quality
- `%s`: Output file path

Format arguments (in order):
1. DPI (300)
2. Scaled canvas width
3. Scaled canvas height
4. Text area width
5. Text area height
6. Font path
7. Title pointsize
8. Stroke width
9. Title caption text
10. Title Y offset
11. Font path (author)
12. Author pointsize
13. Author Y offset
14. Author text
15. Canvas width (for author area)
16. Canvas height (for author area)
17. Logo path
18. Logo DPI
19. Logo width
20. Logo height
21. Logo padding X
22. Logo padding Y
23. Final width
24. Final height
25. Output file path")

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
         (author-text (string-trim-right (format authorship-pattern authors) "\\."))
         (thumbnail-file (expand-file-name "thumbnail.png" paths-dir-downloads))
         ;; Use high DPI for text rendering
         (dpi 300)
         (scale-factor (/ dpi 72.0)) ; Standard screen DPI is 72
         (scaled-width (round (* width scale-factor)))
         (scaled-height (round (* height scale-factor)))
         (title-pointsize (round (* height 0.035 scale-factor)))
         (title-y-offset (round (* height -0.12 scale-factor)))
         (authors-pointsize (round (* height 0.035 scale-factor)))
         (authors-y-offset (round (* height 0.18 scale-factor)))
         (logo-size (round (* height 0.15 scale-factor)))
         (logo-padding (round (* width 0.03 scale-factor)))
         (stroke-width (round (* 2 scale-factor))))
    (unless (file-exists-p font-path)
      (user-error "Font file not found: %s" font-path))
    (unless (file-exists-p logo-path)
      (user-error "Logo file not found: %s" logo-path))
    (let* ((text-width (round (* scaled-width 0.8)))
           (text-height (round (* scaled-height 0.8)))
           (command (format tlon-youtube-thumbnail-command-template
                            dpi scaled-width scaled-height
                            text-width text-height
                            (shell-quote-argument font-path) title-pointsize
                            stroke-width (format "caption:'%s'" (tlon-youtube--sanitize-draw-string title))
                            title-y-offset
                            (shell-quote-argument font-path) authors-pointsize
                            authors-y-offset (tlon-youtube--sanitize-draw-string author-text)
                            scaled-width scaled-height
                            (shell-quote-argument logo-path) dpi logo-size logo-size
                            logo-padding logo-padding
                            width height
                            (shell-quote-argument thumbnail-file))))
      (message "Generating %dx%d thumbnail at %d DPI..." width height dpi)
      (let ((result (shell-command command)))
        (message "Shell command result: %d" result)
        (if (file-exists-p thumbnail-file)
            (message "Successfully generated thumbnail: %s" thumbnail-file)
          (user-error "Failed to generate thumbnail. Check *Messages* buffer for convert output"))))))

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
