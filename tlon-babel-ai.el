;;; tlon-babel-ai.el --- AI functionality for the Babel project -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon-babel
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

;; AI functionality for the Babel project.

;;; Code:

(require 'gptel)
(require 'gptel-extras)
(require 'tlon-babel)

;;;; Functions

;;;;; Translation

;;;###autoload
(defun tlon-babel-ai-rewrite ()
  "Docstring."
  (interactive)
  (let* ((text (if (region-active-p)
		   (buffer-substring-no-properties (region-beginning) (region-end))
		 (read-string "Text to rewrite: "))))
    (gptel-request
     (format "Por favor, genera las mejores diez variantes del siguiente texto castellano:\n\n```\n%s\n```\n\n. Por favor, devuelve todas las variantes en una única linea, separadas por '|'. No insertes un espacio ni antes ni después de '|'. No agregues ningún comentario aclaratorio: solo necesito la lista de variantes. A modo de ejemplo, para la expresión 'búsqueda de poder' el texto a devolver sería: 'ansia de poder|ambición de poder|búsqueda de autoridad|sed de poder|afán de poder|aspiración de poder|anhelo de poder|deseo de control|búsqueda de dominio|búsqueda de control' (esta lista solo pretende ilustrar el formato en que debes presentar tu respuesta). Gracias!" text)
     :callback
     (lambda (response info)
       (if (not response)
	   (message "gptel-quick failed with message: %s" (plist-get info :status))
	 (let* ((variants (split-string response "|"))
		(variant (completing-read "Variant: " variants)))
	   (delete-region (region-beginning) (region-end))
	   (kill-new variant)))))))

;;;###autoload
(defun tlon-babel-ai-translate (text)
  "Return ten alternative translations of TEXT."
  (interactive "sText to translate: ")
  (gptel-request
   (format "Generate the best ten Spanish translations of the following English text:\n\n```\n%s\n```\n\nPlease return each translation on the same line, separated by '|'. Do not add a space either before or after the '|'. Do not precede your answer by 'Here are ten Spanish translations' or any comments of that sort: just return the translations. An example return string for the word 'very beautiful' would be: 'muy bello|muy bonito|muy hermoso|muy atractivo' (etc). Thanks!" text)
   :callback
   (lambda (response info)
     (if (not response)
	 (message "gptel-quick failed with message: %s" (plist-get info :status))
       (let ((translations (split-string response "|")))
	 (kill-new (completing-read "Translation: " translations)))))))

(defun tlon-babel-ai-translate-file (file)
  "Translate FILE."
  (let* ((counterpart (tlon-babel-get-counterpart file))
	 (filename (file-name-nondirectory counterpart))
	 (target-path (concat
		       (file-name-sans-extension filename)
		       "--ai-translated.md"))
	 (text (with-temp-buffer
		 (insert-file-contents file)
		 (buffer-string))))
    (gptel-request
     (format "Translate the following text into Spanish:\n\n```\n%s\n```\n\n" text)
     :callback
     (lambda (response info)
       (if (not response)
	   (message "gptel-quick failed with message: %s" (plist-get info :status))
	 (with-temp-buffer
	   (insert response)
	   (write-region (point-min) (point-max) target-path)))))))

;;;;; Summarization

;;;###autoload
(defun tlon-babel-ai-summarize (model)
  "Summarize and copy the summary to the kill ring using AI MODEL.
If region is active, summarize the region; otherwise, prompt for a file to
summarize."
  (interactive (list (completing-read "Model: " gptel-extras-backends)))
  (gptel-extras-model-config model)
  (let ((string
	 (if (region-active-p)
	     (buffer-substring-no-properties (region-beginning) (region-end))
	   (let* ((current-file (buffer-file-name))
		  (selected-file (read-file-name "Select file to summarize (if you would like to summarize a region, run this command with an active region): " nil current-file nil (file-name-nondirectory current-file))))
	     (with-temp-buffer
	       (insert-file-contents selected-file)
	       (buffer-string))))))
    (message "Generating summary. This may take 5–30 seconds, depending on length...")
    (gptel-request
     (format "Por favor, genera un resumen del siguiente artículo:\n\n```\n%s\n```\n\n. El resumen debe tener solamente un párrafo y no es necesario que mencione datos bibliográficos de la obra reusmida (como título o autor). Escribe el resumen afirmando directamente lo que el artículo sostiene, en lugar de utilizar giros como ‘El artículo sostiene que...’. Por ejemplo, en lugar de escribir ‘El artículo cuenta que la humanidad luchó contra la viruela durante siglos...’, escribe ‘La humanidad luchó contra la viruela durante siglos...’"
	     string)
     :callback
     (lambda (response info)
       (if (not response)
	   (message "`gptel' failed with message: %s" (plist-get info :status))
	 (kill-new response)
	 (message "Copied AI-generated summary to the kill ring:\n\n%s" response))))))

(provide 'tlon-babel-ai)
;;; tlon-babel-ai.el ends here

