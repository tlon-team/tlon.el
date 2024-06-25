;;; tlon-deepl.el --- Support for DeepL API calls -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/tlon-team/tlon
;; Version: 0.1

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

;; Support for DeepL API calls.

;;; Code:

(require 'tlon-glossary)
(require 'tlon-core)
(require 'url)

;;;; Variables

(defconst tlon-deepl-key
  (auth-source-pass-get "key" (concat "tlon/babel/deepl.com/" (getenv "WORK_EMAIL")))
  "The DeepL API key.")

(defconst tlon-deepl-url-prefix
  "https://api.deepl.com/v2/"
  "URL common to all DeepL API calls.")

(defconst tlon-deepl-parameters
  '((translate . ("POST"
		  "translate"
		  tlon-deepl-translate-callback
		  tlon-deepl-translate-encode))
    (glossary-create . ("POST"
			"glossaries"
			tlon-deepl-glossary-create-callback
			tlon-deepl-glossary-create-encode))
    (glossary-get . ("GET"
		     "glossaries"
		     tlon-deepl-glossary-get-callback))
    (glossary-delete . ("DELETE"
			tlon-deepl-glossary-delete-formatter
			tlon-deepl-glossary-delete-callback)))
  "Alist of API calls and their parameters.")

(defvar tlon-deepl-glossaries nil
  "A list of glossaries retrieved from the DeepL API.")

(defvar tlon-deepl-source-language nil
  "Source language of the current API request.")

(defvar tlon-deepl-target-language nil
  "Target language of the current API request.")

(defvar tlon-deepl-text nil
  "The text to be translated in the current API request.")

;;;; Functions

;;;###autoload
(defun tlon-deepl-diff (&optional translation deepl)
  "Run an `ediff' session for a TRANSLATION and the DEEPL translation of it.
If TRANSLATION is nil, use the current buffer. If DEEPL is nil, prompt the user
for a file."
  (interactive)
  (let ((translation (or translation
			 (if-let ((file (buffer-file-name)))
			     file
			   (user-error "Current buffer is not visiting a file"))))
	(deepl (or deepl (read-file-name "DeepL translation: "))))
    (ediff-files deepl translation)))

;;;###autoload
(defun tlon-deepl-select-glossary ()
  "Select an existing DeepL glossary."
  (interactive)
  (let* ((glossaries (tlon-deepl-get-list-of-glossaries))
	 (selection (completing-read "Glossary: " glossaries nil t)))
    (alist-get selection glossaries nil nil #'string=)))

(defun tlon-deepl-get-list-of-glossaries ()
  "Return a cons cell of glossaries and their metadata."
  (let (cons)
    (dolist (glossary tlon-deepl-glossaries cons)
      (let* ((data (mapcar (lambda (key)
			     (alist-get key glossary nil nil #'string=))
			   '(glossary_id name source_lang target_lang entry_count)))
	     (id (car data)))
	(push
	 (cons (apply #'format "%2$20s %3$4s %4$4s %5$3s %1$40s" data) id) cons)))))

;;;;; API

(defun tlon-deepl-request-wrapper (type &optional callback)
  "Wrapper for API requests of TYPE.
If CALLBACK is nil, use the default callback for TYPE."
  (cl-destructuring-bind (method url-suffix-or-fun default-callback &optional json)
      (alist-get type tlon-deepl-parameters)
    (let ((callback (or callback default-callback))
	  (url (if (functionp url-suffix-or-fun)
		   (funcall url-suffix-or-fun)
		 (concat tlon-deepl-url-prefix url-suffix-or-fun)))
	  (url-request-method method)
	  (url-request-extra-headers
	   `(("Content-Type" . "application/json")
	     ("Authorization" . ,(concat "DeepL-Auth-Key " tlon-deepl-key))
	     ("User-Agent" . "YourApp/1.2.3")))
	  (url-request-data (when json (funcall json))))
      (url-retrieve url
		    (lambda (_) (funcall callback))
		    nil t))))

;;;;; Translation

;;;###autoload
(defun tlon-deepl-translate (&optional text target-lang source-lang callback)
  "Translate TEXT from SOURCE-LANG into TARGET-LANG and execute CALLBACK.
If SOURCE-LANG is nil, use \"en\". If CALLBACK is nil, execute
`tlon-deepl-translate-callback'."
  (interactive)
  (let ((text (or text
		  (read-string "Text to translate: "
			       (when (region-active-p)
				 (buffer-substring-no-properties (region-beginning) (region-end))))))
	(target-lang (or target-lang (tlon-select-language 'code 'babel))))
    (setq tlon-deepl-text text
	  tlon-deepl-source-language (or source-lang "en")
	  tlon-deepl-target-language target-lang)
    (tlon-deepl-request-wrapper 'translate callback)))

;;;###autoload
(defun tlon-deepl-translate-and-copy (&optional text target-lang source-lang)
  "Translate TEXT from SOURCE-LANG into TARGET-LANG and copy the translation.
If SOURCE-LANG is nil, use \"en\"."
  (interactive)
  (tlon-deepl-translate text target-lang source-lang
			(lambda ()
			  (tlon-deepl-translate-callback 'copy))))

(defun tlon-deepl-translate-callback (&optional copy)
  "Print the translated text.
If COPY is non-nil, copy the translation to the kill ring instead."
  (setq tlon-deepl-source-language nil
	tlon-deepl-target-language nil
	tlon-deepl-text nil)
  (goto-char (point-min))
  (search-forward "\n\n")
  (let* ((json-array-type 'list)
	 (json-key-type 'string)
	 (json (cadar (json-read)))
	 (translation (alist-get "text" json nil nil #'string=))
	 (decoded (tlon-deepl-fix-encoding translation)))
    (when copy
      (kill-new decoded))
    (message (concat (when copy "Copied to kill ring: " ) (replace-regexp-in-string "%" "%%" decoded)))))

(defun tlon-deepl-fix-encoding (string)
  "Fix encoding in STRING.
The encoding in misinterpreted as ISO-8859-1 when it's actually UTF-8."
  (decode-coding-string (encode-coding-string string 'iso-8859-1) 'utf-8))

(defun tlon-deepl-fix-encoding-persistent ()
  "Fix persistent encoding issues in the current buffer."
  (interactive)
  (unless (derived-mode-p 'bib-mode)
    (user-error "This command must be run in BibTeX mode"))
  (save-excursion
    (goto-char (point-min))
    (dolist (cons '(("\\\\\\\\\\\\\\\\3\\\\\\\\255" . "í")
		    ("\\\\\\\\303\\\\\\\\241" . "á")
		    ("\\\\\\\\303\\\\\\\\263" . "ó")
		    ("\\\\\\\\342\\\\\\\\200\\\\\\\\223" . "—")
		    ("\\\\\\\\342\\\\\\\\200\\\\\\\\235" . "’")
		    ("\\\\\\\\3\\\\\\\\263" . "ó")
		    ("\\\\\\\\3\\\\\\\\3" . "ó")
		    ;; ("\\\\\\\\303\\\\\\e\\251" . "é")
		    ("\\\\\\\\263" . "ó")
		    ("\\\\\\\\342\\\\\\\\200\\\\\\\\234" . "‘")
		    ("\\\\\\\\303\\\\\\\\251" . "é")
		    ("\\\\\\\\\Resumen generado por" . "Resumen generado por")
		    ("\\\\\\\\303\\\\\\\\255" . "í")
		    ("\\\\\\\\303\\\\\\\\272" . "ú")
		    ("\\\\\\\\303\\\\\\\\257" . "ï")
		    ("\\\\\\\\342\\\\\\\\200\\\\\\\\211" . " ")
		    ("\\\\\\\\342\\\\\\\\210\\\\\\\\222" . "-")
		    ("\\\\\\\\302\\\\\\\\261" . "±")
		    ("\\\\\\\\3303ó" . "ó")
		    ("¾3" . "ó")
		    ("ó30ó" . "ó")
		    ("\\\\\\\\%" . "%")
		    ("\\\\\\\\3" . "ó")
		    ("ó42\\\\\\\\200\\\\\\\\230" . "“")
		    ("ó42\\\\\\\\200\\\\\\\\231" . "”")
		    ("ó03\\\\\\\\1" . "é")
		    ("ó03ó" . "ó")
		    ("u00f3" . "ó")
		    ("Ã³" . "ó")
		    ("Ã330ó" . "ó")
		    ("Ã©" . "é")
		    ("ó42\\\\\\\\200\\\\\\\\224" . "—")
		    ("ó\\\\\\\\241" . "á")
		    ("ó03\\\\\\\\261" . "ñ")
		    ("ó\\\\\\\\1" . "é")
		    ("ó42\\\\\\\\200\\\\\\\\242" . "•")
		    ("342\\\\\\\\200\\\\\\\\234" . "“")))
      (goto-char (point-min))
      (let ((search (car cons))
	    (replace (cdr cons)))
	(while (re-search-forward search nil t)
	  (replace-match replace t t))))))

(defun tlon-deepl-translate-encode ()
  "Return a JSON representation of the text to be translated to language."
  (let ((id (tlon-deepl-get-language-glossary tlon-deepl-target-language))
	(text (list (encode-coding-string tlon-deepl-text 'utf-8))))
    (when (and (not id) (string= tlon-deepl-source-language "en"))
      (user-error "No glossary found for %s" tlon-deepl-target-language))
    (json-encode `(("text" . ,text)
		   ("source_lang" . ,tlon-deepl-source-language)
		   ("target_lang" . ,tlon-deepl-target-language)
		   ("glossary_id" . ,id)))))

(defun tlon-deepl-get-language-glossary (language)
  "Return the glossary ID for LANGUAGE.
Since we only have glossaries for English as the source language, return nil
when the source language is not English."
  (when (string= "en" tlon-deepl-source-language)
    (tlon-lookup tlon-deepl-glossaries "glossary_id" "target_lang" language)))

;;;;;; Glossaries

;;;;;;; Get glossary

;;;###autoload
(defun tlon-deepl-get-glossaries ()
  "Retrieve and display list of glossaries from the DeepL API."
  (interactive)
  (tlon-deepl-request-wrapper 'glossary-get))

(defun tlon-deepl-glossary-get-callback ()
  "Callback for `tlon-deepl-get-glossaries'."
  (goto-char (point-min))
  (search-forward "\n\n")
  (let ((json-array-type 'list)
	(json-key-type 'string))
    (setq tlon-deepl-glossaries (cdar (json-read))))
  (message "Read glossaries from DeepL API."))

(tlon-deepl-get-glossaries)

;;;;;;; Create glossary

;;;###autoload
(defun tlon-deepl-glossary-create (language)
  "Create a DeepL glossary for LANGUAGE."
  (interactive (list (tlon-select-language 'code 'babel)))
  (tlon-extract-glossary language 'deepl-api)
  (setq tlon-deepl-target-language language)
  (tlon-deepl-request-wrapper 'glossary-create))

(defun tlon-deepl-glossary-create-encode ()
  "Return a JSON representation of the glossary to be created."
  (let* ((extension "tsv")
	 (file (tlon-glossary-make-file (upcase tlon-deepl-target-language) extension))
	 (name (file-name-base file))
	 entries)
    (with-temp-buffer
      (insert-file-contents file)
      (setq entries (encode-coding-string (buffer-string) 'utf-8))   ;; Encode entries in UTF-8
      (json-encode `(("name" . ,name)
		     ("source_lang" . "en")
		     ("target_lang" . ,tlon-deepl-target-language)
		     ("entries" . ,entries)
		     ("entries_format" . ,extension))))))

(defun tlon-deepl-glossary-create-callback ()
  "Callback for `tlon-deepl-glossary-create'."
  (setq tlon-deepl-target-language nil)
  (goto-char (point-min))
  (search-forward "\n\n")
  (let ((response (json-read)))
    (tlon-deepl-get-glossaries)
    (message "Response: %s" response)))

;;;;;;; Delete glossary

;;;###autoload
(defun tlon-deepl-glossary-delete ()
  "Delete a DeepL glossary."
  (interactive)
  (tlon-deepl-request-wrapper 'glossary-delete))

(defun tlon-deepl-glossary-delete-formatter ()
  "URL formatter for `tlon-deepl-glossary-delete'."
  (concat tlon-deepl-url-prefix "glossaries/" (tlon-deepl-select-glossary)))

(defun tlon-deepl-glossary-delete-callback ()
  "Callback for `tlon-deepl-glossary-delete'."
  (tlon-deepl-get-glossaries)
  (message "Deleted glossary."))

;;;;; Menu

;;;###autoload (autoload 'tlon-deepl-menu "tlon-deepl" nil t)
(transient-define-prefix tlon-deepl-menu ()
  "DeepL menu."
  ["Translate"
   ("t" "Translate" tlon-deepl-translate)
   ("H-t" "Translate and copy" tlon-deepl-translate-and-copy)
   ""
   "Glossaries"
   ("l" "List" tlon-deepl-select-glossary)
   ("g" "Retrieve" tlon-deepl-get-glossaries)
   ("c" "Create" tlon-deepl-glossary-create)
   ("d" "Delete" tlon-deepl-glossary-delete)
   ""
   ("e" "Ediff" tlon-deepl-diff)])

(provide 'tlon-deepl)
;;; tlon-deepl.el ends here
