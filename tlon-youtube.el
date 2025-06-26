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

;;;; Functions

(defun tlon-youtube-generate-wavelength-video ()
  "Generate a 4K video (3840x2160) with an animated wavelength from an audio file using `seewav`.
Prompts the user to select an audio file from the \"uqbar-audio\"
repository. The output video is saved in `paths-dir-downloads`
with a `.mp4` extension, using the original audio file name."
  (interactive)
  (let* ((uqbar-audio-dir (tlon-repo-lookup :dir :name "uqbar-audio")))
    (unless uqbar-audio-dir
      (user-error "Could not find the 'uqbar-audio' repository directory."))
    (let* ((selected-audio-file (read-file-name "Select audio file: " uqbar-audio-dir))
           ;; Expand the selected audio file path
           (audio-file (expand-file-name selected-audio-file))
           (video-file-name (concat (file-name-nondirectory (file-name-sans-extension selected-audio-file)) ".mp4"))
           ;; Construct and expand the output video file path
           (video-file (expand-file-name (file-name-concat paths-dir-downloads video-file-name)))
           (command (format "seewav -W 3840 -H 2160 %s %s"
                            (shell-quote-argument audio-file)
                            (shell-quote-argument video-file))))
      (message "Generating 4K video with seewav...")
      (shell-command command)
      (if (file-exists-p video-file) ; Use the expanded path for checking
          (message "Successfully generated video: %s" video-file)
        (user-error "Failed to generate video. Check *Messages* buffer for seewav output")))))

;;;; Menu

;;;###autoload (autoload 'tlon-youtube-menu "tlon-youtube" nil t)
(transient-define-prefix tlon-youtube-menu ()
  "YouTube menu."
  [["Actions"
    ("g" "generate wavelength video" tlon-youtube-generate-wavelength-video)]])

(provide 'tlon-youtube)
;;; tlon-youtube.el ends here
