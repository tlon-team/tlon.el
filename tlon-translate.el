;;; tlon-translate.el --- Translate files -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon

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

;; This module provides functionality for translating entire files using
;; different translation engines.

;;; Code:

(require 'tlon-core)
(require 'tlon-counterpart)
(require 'tlon-deepl)
(require 'transient)

(declare-function tlon-get-repo-from-file "tlon-core")
(declare-function tlon-repo-lookup "tlon-core")
(declare-function tlon-select-language "tlon-core")
(declare-function tlon-get-counterpart-in-translations "tlon-counterpart")
(declare-function tlon-get-counterpart-dir "tlon-counterpart")
(declare-function tlon-metadata-lookup "tlon-core")
(declare-function tlon-metadata-in-repo "tlon-yaml")

;;;; User options

(defgroup tlon-translate nil
  "File translation functionality for Tlön."
  :group 'tlon)

(defcustom tlon-translate-engine 'deepl
  "The translation engine to use for file translation."
  :group 'tlon-translate
  :type '(choice (const :tag "DeepL" deepl)))

;;;; Variables

(defconst tlon-translate--engine-choices
  '(("DeepL" . deepl))
  "Alist of translation engine display names and their symbols.")

;;;; Commands

;;;###autoload
(defun tlon-translate-file (&optional file lang)
  "Translate FILE into LANG using `tlon-translate-engine'.
If FILE is not provided, prompt for one, defaulting to the current buffer's
file. If LANG is not provided, prompt for a target language."
  (interactive
   (list (read-file-name "Translate file: " nil nil t
			 (file-relative-name (buffer-file-name) default-directory))
         (tlon-select-language 'code 'babel "Target language: " 'require-match)))
  (let* ((source-file file)
         (target-lang-code lang)
         (target-file
          (let ((candidate (tlon-translate--get-counterpart-for-language source-file target-lang-code)))
            (if (and candidate (file-exists-p candidate))
                (if (y-or-n-p (format "Overwrite existing counterpart %s?" candidate))
                    candidate
                  (read-file-name "Save translation to: " (file-name-directory candidate)))
              (or candidate
                  (let ((counterpart-dir (tlon-get-counterpart-dir source-file target-lang-code)))
                    (read-file-name "Save translation to: " counterpart-dir)))))))
    (when target-file
      (tlon-translate--do-translate source-file target-file target-lang-code))))

(defun tlon-translate--do-translate (source-file target-file target-lang-code)
  "Translate SOURCE-FILE to TARGET-FILE into TARGET-LANG-CODE."
  (pcase tlon-translate-engine
    ('deepl
     (let* ((source-repo (tlon-get-repo-from-file source-file))
            (source-lang-code (tlon-repo-lookup :language :dir source-repo))
            (text (with-temp-buffer
                    (insert-file-contents source-file)
                    (buffer-string)))
            (translated-text nil))
       (tlon-deepl-translate text target-lang-code source-lang-code
                             (lambda ()
                               (setq translated-text (tlon-translate--get-deepl-translation-from-buffer)))
                             t)
       (when translated-text
         (with-temp-file target-file
           (insert translated-text))
         (message "Translated %s to %s" source-file target-file))))
    (_ (user-error "Unsupported translation engine: %s" tlon-translate-engine))))

(defun tlon-translate--get-counterpart-for-language (file lang-code)
  "Return the counterpart of FILE for LANG-CODE."
  (let* ((repo (tlon-get-repo-from-file file))
         (subtype (tlon-repo-lookup :subtype :dir repo)))
    (pcase subtype
      ('originals
       (tlon-translate--get-translation-from-original file lang-code))
      ('translations
       (if (string= (tlon-repo-lookup :language :dir repo) lang-code)
           file
         (when-let ((original-file (tlon-get-counterpart-in-translations file)))
           (tlon-translate--get-translation-from-original original-file lang-code)))))))

(defun tlon-translate--get-translation-from-original (original-file lang-code)
  "Get translation of ORIGINAL-FILE for LANG-CODE."
  (let* ((original-repo (tlon-get-repo-from-file original-file))
         (target-repo-dir
          (tlon-repo-lookup :dir
                            :subproject (tlon-repo-lookup :subproject :dir original-repo)
                            :language lang-code))
         (counterpart-file nil))
    (when target-repo-dir
      (setq counterpart-file
            (tlon-metadata-lookup (tlon-metadata-in-repo target-repo-dir)
                                  "file"
                                  "original_path"
                                  (file-relative-name original-file original-repo)))
      (unless counterpart-file
        (when-let* ((fallback-dir (tlon-get-counterpart-dir original-file lang-code))
                    (fallback-path (file-name-concat fallback-dir
                                                     (file-name-nondirectory original-file))))
          (setq counterpart-file fallback-path))))
    counterpart-file))

(defun tlon-translate--get-deepl-translation-from-buffer ()
  "Parse DeepL JSON response in current buffer and return translation."
  (goto-char (point-min))
  (when (re-search-forward "^{\\|\\[{" nil t)
    (goto-char (match-beginning 0)))
  (let* ((json-array-type 'list)
         (json-key-type 'string)
         (json-object-type 'alist)
         (json-data (json-read))
         (translations (alist-get "translations" json-data nil nil #'string=))
         (first-translation (car translations)))
    (when first-translation
      (alist-get "text" first-translation nil nil #'string=))))

;;;;; Menu

;;;###autoload (autoload 'tlon-translate-menu "tlon-translate" nil t)
(transient-define-prefix tlon-translate-menu ()
  "`tlon-translate' menu."
  [["Translate"
    ("f" "Translate file" tlon-translate-file)]
   ["Options"
    ("e" "Engine" tlon-translate-engine-infix)]])

(defun tlon-translate-engine-reader (prompt _initval _arg)
  "PROMPT the user to select a translation engine."
  (let* ((current-value tlon-translate-engine)
         (current-label (or (car (rassoc current-value tlon-translate--engine-choices))
                            (format "%s (Unknown)" current-value)))
         (prompt (format "%s (current: %s): " prompt current-label))
         (selection (completing-read prompt tlon-translate--engine-choices nil t)))
    (cdr (assoc selection tlon-translate--engine-choices))))

(transient-define-infix tlon-translate-engine-infix ()
  "Select the translation engine (`tlon-translate-engine')."
  :class 'transient-lisp-variable
  :variable 'tlon-translate-engine
  :reader 'tlon-translate-engine-reader
  :prompt "Translation Engine: ")

(provide 'tlon-translate)
;;; tlon-translate.el ends here
