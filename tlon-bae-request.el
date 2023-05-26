;;; tlon-bae-request.el --- Request convenience functions for the Tlön BAE project. -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1.0
;; Homepage: https://tlon.team
;; Keywords: convenience tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(require 'request)

(defconst tlon-bae-request-eaf-api-url
  "https://forum.effectivealtruism.org/graphql"
  "URL for the EAF GraphQL API endpoint.")

(defvar tlon-bae-eaf-entities
  '(post tag)
  "List of entities supported by the EAF GraphQL API.")

;;; queries

(defun tlon-bae-request-query-eaf-post (id)
  "Return an EA Forum GraphQL query for post whose ID is ID."
  (concat "{\"query\":\"{\\n  post(\\n    input: {\\n      selector: {\\n        _id: \\\""
	  id
	  "\\\"\\n      }\\n    }\\n  ) {\\n    result {\\n      _id\\n      postedAt\\n      url\\n      canonicalSource\\n      title\\n      contents {\\n        markdown\\n        ckEditorMarkup\\n      }\\n      slug\\n      commentCount\\n      htmlBody\\n      baseScore\\n      voteCount\\n      pageUrl\\n      legacyId\\n      question\\n      tableOfContents\\n      user {\\n        username\\n        displayName\\n        slug\\n        bio\\n      }\\n      coauthors {\\n        _id\\n        username\\n        displayName\\n        slug\\n      }\\n    }\\n  }\\n}\\n\"}"))

(defun tlon-bae-request-query-eaf-tag (slug)
  "Return an EA Forum GraphQL query for tag whose slug is SLUG."
  (concat "{\"query\":\"{\\n  tag(input: { selector: { slug: \\\""
	  slug
	  "\\\" } }) {\\n    result {\\n      name\\n      description {\\n        html\\n      }\\n      parentTag {\\n        name\\n      }\\n    }\\n  }\\n}\\n\"}"))

;;; extractors

(defun tlon-bae-request-extract-html-eaf-post (json-response)
  (let* ((data (cdr (assoc 'data json-response)))
	 (post (cdr (assoc 'post data)))
	 (result (cdr (assoc 'result post)))
	 (html (cdr (assoc 'htmlBody result))))
    html))

(defun tlon-bae-request-extract-title-eaf-post (json-response)
  (let* ((data (cdr (assoc 'data json-response)))
	 (post (cdr (assoc 'post data)))
	 (result (cdr (assoc 'result post)))
	 (title (cdr (assoc 'title result))))
    (tlon-bae-shorten-title title)))

(defun tlon-bae-request-extract-html-eaf-tag (json-response)
  (let* ((data (cdr (assoc 'data json-response)))
	 (tag (cdr (assoc 'tag data)))
	 (result (cdr (assoc 'result tag)))
	 (description (cdr (assoc 'description result)))
	 (html (cdr (assoc 'html description))))
    html))

(defun tlon-bae-request-extract-title-eaf-tag (json-response)
  (let* ((data (cdr (assoc 'data json-response)))
	 (tag (cdr (assoc 'tag data)))
	 (result (cdr (assoc 'result tag)))
	 (title (cdr (assoc 'name result))))
    (tlon-bae-shorten-title title)))

;;; file handling

(defun tlon-bae-request-save-html-to-file (html)
  "Save the HTML string HTML to a temporary file."
  (let ((filename (make-temp-file "tlon-bae-request-" nil ".html")))
    (with-temp-file filename
      (insert html))
    filename))

;;; request functions

(defun tlon-bae-request-deferred-eaf (entity identifier key)
  "Run a deferred EAF request for KEY in ENTITY with IDENTIFIER.
  ENTITY should be one of the elements in
  `tlon-bae-eaf-entities'. IDENTIFIER should be a string returned
  by `tlon-bae-validate-identifier'; see its docstring
  for details."
  (setq identifier (tlon-bae-eaf-validate-identifier entity identifier))
  (let* ((query-function (intern (concat
				  "tlon-bae-request-query-eaf-"
				  (symbol-name entity))))
	 (query (funcall query-function identifier))
	 (return-function (intern (concat "tlon-bae-request-extract-"
					  (symbol-name key)
					  "-eaf-"
					  (symbol-name entity))))
	 output)
    (deferred:$
     (request-deferred
      tlon-bae-request-eaf-api-url
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data query
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (let ((results (cdr (assoc 'data data))))
		    (message "Success: %S" results))))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error: %S" error-thrown)))
      )
     (deferred:nextc it
		     (lambda (response)
		       (funcall return-function (request-response-data response)))))))

(defun tlon-bae-request--eaf-export-to-markdown (entity identifier)
  "Export the EAF entity ENTITY with identifier IDENTIFIER to Markdown."
  (deferred:nextc (tlon-bae-request-deferred-eaf entity identifier 'title)
		  (lambda (title)
		    (let* ((md-file (tlon-bae-format-file title "md" entity)))
		      (deferred:nextc (tlon-bae-request-deferred-eaf entity identifier 'html)
				      (lambda (html)
					(let* ((html-file (tlon-bae-request-save-html-to-file html)))
					  (shell-command
					   (format "pandoc -s '%s' -t markdown -o '%s'" html-file md-file))
					  (with-current-buffer (find-file-noselect md-file)
					    (tlon-bae-markdown-eaf-cleanup))
					  (tlon-bae-create-job md-file))))))))

(defun tlon-bae-request-eaf-export-to-markdown (entity identifier)
  "Export the EAF entity ENTITY with identifier IDENTIFIER to Markdown."
  (interactive
   (list (intern (completing-read "Entity: " tlon-bae-eaf-entities))
	 (read-string "Identifier (URL, ID or slug): ")))
  (tlon-bae-request--eaf-export-to-markdown entity identifier))

(provide 'tlon-bae-request)
;;; tlon-bae-request.el ends here
