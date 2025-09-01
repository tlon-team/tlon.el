;;; tlon-import.el --- Import functions for Tlön -*- lexical-binding: t -*-

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

;; Import functions for Tlön.

;;; Code:

(require 'request)
(require 'tlon-cleanup)
(require 'tlon-core)
(require 'tlon-fix)

;;;; Variables

;;;;; EAF API

(defconst tlon-import-eaf-api-url
  "https://forum.effectivealtruism.org/graphql"
  "URL for the EA Forum GraphQL API endpoint.")

(defconst tlon-import-eaf-objects
  '(post tag)
  "List of entities supported by the EAF GraphQL API.")

;;;;; LessWrong API

(defconst tlon-import-lw-api-url
  "https://lesswrong.com/graphql"
  "URL for the LessWrong GraphQL API endpoint.")

;;;;; Pandoc

(defconst tlon-pandoc-to-markdown
  "-t markdown-raw_html-native_divs-native_spans-fenced_divs-bracketed_spans-header_attributes-fenced_code_blocks --wrap=none --strip-comments '%s' -o '%s'"
  "Pandoc command to convert HTML to Markdown.")

(defconst tlon-pandoc-convert-from-file
  (format "pandoc %s" tlon-pandoc-to-markdown)
  "Command to convert from HTML file to Markdown.")

(defconst tlon-pandoc-convert-from-url
  (format "pandoc -r html %s" tlon-pandoc-to-markdown)
  "Command to convert from URL to Markdown.")

;;;;; Executables

(defvar tlon-pdf2md
  (file-name-concat paths-dir-external-repos "pdf2md/lib/pdf2md-cli.js")
  "Path to `pdf2md-cli.js' executable.")

(defvar tlon-pdftotext
  "pdftotext"
  "Path to `pdftotext' executable.")

;;;; Functions

;;;;; General import

(declare-function simple-extras-string-is-url-p "simple-extras")
(declare-function ebib-extras-open-key "ebib-extras")
(declare-function tlon-yaml-insert-field "tlon-yaml")
(declare-function tlon-yaml-suggest-tags "tlon-yaml")
(declare-function tlon-images-download-from-markdown "tlon-images")
;;;###autoload
(defun tlon-import-document ()
  "Import a new document from a URL or a PDF file."
  (interactive)
  (if (derived-mode-p 'ebib-entry-mode 'ebib-index-mode)
      (when-let ((key (ebib-extras-get-field "translation")))
	(ebib-extras-open-key key)
	(unless (y-or-n-p (message "You ran this command from a translation BibTeX entry, whose original is %s. Shall we import this original? " key))
	  (user-error "Aborted")))
    (unless (y-or-n-p "This command should ideally be run from the bibtex entry to import. Continue and enter details manually? ")
      (user-error "Aborted")))
  (let* ((ebib-data (tlon-get-import-details-from-ebib))
	 (identifier (or (nth 0 ebib-data) (read-string "Identifier (URL or PDF path): ")))
	 (title (or (nth 1 ebib-data) (read-string "Title: ")))
	 (key (nth 2 ebib-data)))
    (if (simple-extras-string-is-url-p identifier)
	(tlon-import-html identifier title)
      (tlon-import-pdf (expand-file-name identifier)))
    (tlon-yaml-insert-field "key" key))
  (let ((file (buffer-file-name)))
    (tlon-yaml-suggest-tags file)
    (tlon-images-download-from-markdown file)))

(declare-function ebib-extras-get-field "ebib-extras")
(declare-function ebib-extras-get-file "ebib-extras")
(declare-function ebib--get-key-at-point "ebib")
(defun tlon-get-import-details-from-ebib ()
  "Get the relevant details for importing a document from the current BibTeX entry."
  (when (derived-mode-p 'ebib-entry-mode 'ebib-index-mode)
    (if-let ((identifier (or (ebib-extras-get-field "url")
			     (ebib-extras-get-file "md")))
	     (title (ebib-extras-get-field "title"))
	     (key (ebib--get-key-at-point)))
	(list identifier title key)
      (user-error "The current Ebib entry seems to be missing one of the following
fields, which are needed to create a new job: `url' or `file',
`title' and `key'"))))

;;;;;; HTML import

(defun tlon-import-html (url &optional title)
  "Import the HTML in URL and convert it to Markdown.
TITLE optionally specifies the title of the file to be imported."
  (if-let ((id-or-slug (tlon-import-eaf-get-id-or-slug-from-identifier url)))
      (tlon-import-eaf-html id-or-slug title)
    (tlon-import-convert-html-to-markdown url title)))

;;;;;;; EAF

;; TODO: make it also work with LessWrong
(defun tlon-import-eaf-html (id-or-slug &optional title)
  "Import the HTML of EAF entity with ID-OR-SLUG and convert it to MD.
TITLE optionally specifies the title of the entity to be imported.
Delegates to specific handlers for articles and tags."
  (if-let* ((response (tlon-import-eaf-request id-or-slug)))
      (let ((type (tlon-import-eaf-get-type id-or-slug)))
        (pcase-exhaustive type
	  ('article (tlon-import-eaf--process-article response title))
	  ('tag (tlon-import-eaf--process-tag response title))))
    (user-error "EAF API returned no response")))

;; (declare-function delete-tlon-yaml-insert-field "tlon-yaml")
(defun tlon-import-eaf--common-processing (entity-title bare-dir-name html-content)
  "Common logic to process EAF entity HTML content.
ENTITY-TITLE is the title for the document.
BARE-DIR-NAME is the target subdirectory (e.g., \"articles\", \"tags\").
HTML-CONTENT is the raw HTML string."
  (let* ((target (tlon-import-set-target entity-title bare-dir-name))
         (html-file (tlon-import-save-html-to-file html-content)))
    (shell-command
     (format tlon-pandoc-convert-from-file html-file target))
    (with-current-buffer (find-file-noselect target)
      (tlon-cleanup-common)
      (tlon-cleanup-eaf)
      (tlon-autofix-all)
      ;; (delete-tlon-yaml-insert-field "type" (substring bare-dir-name 0 -1)) ;; Use bare-dir-name if type is needed
      (save-buffer))
    (find-file target)))

(defun tlon-import-eaf--process-article (response &optional title)
  "Process an EAF article from API RESPONSE. Optional TITLE override."
  (let ((article-title (or title (tlon-import-eaf-get-article-title response))))
    (if-let ((html (tlon-import-eaf-get-article-html response))) ; Directly use HTML
        (tlon-import-eaf--common-processing article-title "articles" html)
      (if (y-or-n-p "EAF API returned no contents for article. Import as normal URL?")
          (let ((url (tlon-import-eaf-get-article-url response)))
            (tlon-import-convert-html-to-markdown url article-title))
        (message "Aborted.")))))

(defun tlon-import-eaf--process-tag (response &optional title)
  "Process an EAF tag from API RESPONSE. Optional TITLE override."
  (let ((tag-title (or title (tlon-import-eaf-get-tag-title response))))
    (if-let ((html (tlon-import-eaf-get-tag-html response)))
	(tlon-import-eaf--common-processing tag-title "tags" html)
      (if (y-or-n-p "EAF API returned no HTML description for tag. Import as normal URL?")
          (let ((url (tlon-import-eaf-get-tag-url response)))
            (tlon-import-convert-html-to-markdown url tag-title))
        (message "Aborted.")))))

;;;;;;; non-EAF

(defun tlon-import-save-html-to-file (html)
  "Save the HTML string HTML to a temporary file."
  (let ((filename (make-temp-file "tlon-request-" nil ".html")))
    (with-temp-file filename
      (insert html))
    filename))

(declare-function tlon-select-bare-dir "tlon-counterpart")
(defun tlon-import-convert-html-to-markdown (source &optional title)
  "Convert HTML text in SOURCE to Markdown.
SOURCE can be a URL or a file path. If TITLE is not provided, prompt the user
for one."
  (let* ((bare-dir (tlon-select-bare-dir "en"))
	 (target (tlon-import-set-target title bare-dir))
	 (pandoc (if (simple-extras-string-is-url-p source)
		     tlon-pandoc-convert-from-url
		   tlon-pandoc-convert-from-file)))
    (shell-command
     (format pandoc source target))
    (with-current-buffer (find-file-noselect target)
      (tlon-cleanup-common)
      (tlon-autofix-all)
      ;; (delete-tlon-yaml-insert-field "type" (substring bare-dir 0 -1))
      )
    (find-file target)))

(declare-function tlon-set-file-from-title "tlon")
(defun tlon-import-set-target (&optional title bare-dir)
  "Set the target file path for the imported document.
If TITLE is nil, prompt the user for one. BARE-DIR specifies the bare-dir of
entity being imported (e.g., article or tag)."
  (let* ((dir (tlon-repo-lookup :dir :name "uqbar-en")))
    (read-string "Save file in: "
		 (tlon-set-file-from-title title (file-name-concat dir bare-dir)))))

;;;;;; PDF import

;; TODO: cleanup two functions below
(defun tlon-import-pdf (path &optional title)
  "Import the PDF in PATH to TARGET and convert it to Markdown.
This command requires the user to supply values for the header and footer
elements to be excluded from the conversion, which are different for each PDF.
To determine these values, measure the distance between the top/bottom of the
PDF (which will open in the other window) and note the number of pixels until
the end of the header/footer. (You can measure the number of pixels between two
points by taking a screenshot: note the numbers next to the pointer.) Then enter
these values when prompted.

If TITLE is nil, prompt the user for one."
  (find-file-other-window path)
  (let ((target (read-string "Save file in: " (tlon-set-file-from-title title)))
	(header (read-string "Header: "))
	(footer (read-string "Footer: ")))
    (unless (executable-find "pdftotext")
      (user-error "`pdftotext' not found. Please install it (`brew install poppler') and set `tlon-pdftotext' to its path"))
    (shell-command (format "'%s' -margint %s -marginb %s '%s' '%s'"
			   tlon-pdftotext header footer path target))
    (find-file target)))

(defun tlon-convert-pdf (source &optional destination)
  "Convert PDF in SOURCE to Markdown in DESTINATION.
If DESTINATION is nil, return the Markdown string."
  (shell-command-to-string (format "%s '%s' '%s'"
				   tlon-pdftotext
				   (expand-file-name source)
				   (if destination
				       (expand-file-name destination)
				     "-"))))

;;;;; EAF API

(defun tlon-import-eaf-article-query (id)
  "Return an EA Forum GraphQL query for post whose ID is ID."
  (concat "{\"query\":\"{\\n  post(\\n    input: {\\n      selector: {\\n        _id: \\\""
	  id
	  "\\\"\\n      }\\n    }\\n  ) {\\n    result {\\n      _id\\n      postedAt\\n      url\\n      canonicalSource\\n      title\\n      contents {\\n        markdown\\n        ckEditorMarkup\\n      }\\n      slug\\n      commentCount\\n      htmlBody\\n      baseScore\\n      voteCount\\n      pageUrl\\n      legacyId\\n      question\\n      tableOfContents\\n      author\\n      user {\\n        username\\n        displayName\\n        slug\\n        bio\\n      }\\n      coauthors {\\n        _id\\n        username\\n        displayName\\n        slug\\n      }\\n    }\\n  }\\n}\\n\"}"))

(defun tlon-import-eaf-tag-query (slug)
  "Return an EA Forum GraphQL query for tag whose slug is SLUG."
  (concat "{\"query\":\"{\\n  tag(input: { selector: { slug: \\\""
	  slug
	  "\\\" } }) {\\n    result {\\n      name\\n      slug\\n      description {\\n        html\\n      }\\n      parentTag {\\n        name\\n      }\\n    }\\n  }\\n}\\n\"}"))

(defun tlon-import-eaf-request (id-or-slug &optional async)
  "Run an EAF request for ID-OR-SLUG.
If ASYNC is t, run the request asynchronously."
  (let* ((type (tlon-import-eaf-get-type id-or-slug))
	 (fun (pcase-exhaustive type
		('article 'tlon-import-eaf-article-query)
		('tag 'tlon-import-eaf-tag-query)))
	 (query (funcall fun id-or-slug))
	 response)
    (request
      tlon-import-eaf-api-url
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data query
      :parser 'json-read
      :sync (not async)
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq response (cdr (assoc 'data data)))))
      :error (cl-function
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "Error: %S" error-thrown))))
    response))

(defun tlon-import-eaf-get-article-result (response)
  "Get article details from EA Forum API RESPONSE."
  (let* ((article (cdr (assoc 'post response)))
	 (result (cdr (assoc 'result article))))
    result))

(defun tlon-import-eaf-get-article-url (response)
  "Get article URL from EA Forum API RESPONSE."
  (let* ((result-data (tlon-import-eaf-get-article-result response)))
    (cdr (assoc 'pageUrl result-data))))

(defun tlon-import-eaf-get-article-html (response)
  "Get article HTML from EA Forum API RESPONSE."
  (let* ((result (tlon-import-eaf-get-article-result response))
	 (html (cdr (assoc 'htmlBody result))))
    html))

(defun tlon-import-eaf-get-article-title (response)
  "Get article title from EA Forum API RESPONSE."
  (let* ((result (tlon-import-eaf-get-article-result response))
	 (title (cdr (assoc 'title result))))
    title))

(defun tlon-import-eaf-get-tag-result (response)
  "Get tag details from EA Forum API RESPONSE."
  (let* ((tag (cdr (assoc 'tag response)))
	 (result (cdr (assoc 'result tag))))
    result))

(defun tlon-import-eaf-get-tag-url (response)
  "Construct tag URL from EA Forum API RESPONSE.
The URL is constructed using the tag's slug."
  (let* ((result (tlon-import-eaf-get-tag-result response))
         (slug (cdr (assoc 'slug result))))
    (if slug
        (format "https://forum.effectivealtruism.org/topics/%s" slug)
      nil)))

(defun tlon-import-eaf-get-tag-html (response)
  "Get tag HTML from EA Forum API RESPONSE."
  (let* ((result (tlon-import-eaf-get-tag-result response))
	 (description (cdr (assoc 'description result)))
	 (html (cdr (assoc 'html description))))
    html))

(defun tlon-import-eaf-get-tag-title (response)
  "Get tag title from EA Forum API RESPONSE."
  (let* ((result (tlon-import-eaf-get-tag-result response))
	 (title (cdr (assoc 'name result))))
    (tlon-import-eaf-shorten-title title)))

(defun tlon-import-eaf-shorten-title (title)
  "Return a shortened version of TITLE."
  (string-match "\\([[:alnum:] ,'‘’“”@#$%*\\^`~&\"]*\\)" title)
  (match-string 1 title))

;;;;; EAF validation

(defun tlon-eaf-base-regexp (url)
  "Return t if URL is an EAF URL, nil otherwise."
  (not (null (string-match tlon-eaf-base-regexp url))))

(defun tlon-import-eaf-article-id-p (identifier)
  "Return t if IDENTIFIER is a post ID, nil otherwise."
  (not (null (string-match (format "^%s$" tlon-eaf-id-regexp) identifier))))

(defun tlon-import-eaf-tag-slug-p (identifier)
  "Return t if IDENTIFIER is a tag slug, nil otherwise."
  (not (null (string-match (format "^%s$" tlon-eaf-tag-slug-regexp) identifier))))

(defun tlon-import-eaf-get-id-or-slug-from-identifier (identifier)
  "Return the EAF post ID or tag slug from IDENTIFIER, if found.
IDENTIFIER can be an URL, a post ID or a tag slug."
  (interactive "sURL: ")
  (if (simple-extras-string-is-url-p identifier)
      (or (tlon-import-eaf-get-id-from-identifier identifier)
	  (tlon-import-eaf-get-slug-from-identifier identifier))
    ;; return id or slug if identifier is an id or slug
    (pcase identifier
      ((pred tlon-import-eaf-article-id-p) identifier)
      ((pred tlon-import-eaf-tag-slug-p) identifier))))

;; TODO: make it work with LW, AF, if it doesn’t already
(defun tlon-import-eaf-get-id-from-identifier (identifier)
  "Return the EAF post ID from IDENTIFIER, if found."
  (when (or (string-match tlon-eaf-url-post-collection identifier)
	    (string-match tlon-eaf-url-post-canonical identifier))
    (match-string-no-properties 2 identifier)))

(defun tlon-import-eaf-get-slug-from-identifier (identifier)
  "Return the EAF tag slug from IDENTIFIER, if found."
  (when (string-match (format "^.+?forum.effectivealtruism.org/topics/%s"
			      tlon-eaf-tag-slug-regexp)
		      identifier)
    (match-string-no-properties 1 identifier)))

(defun tlon-import-eaf-get-type (id-or-slug)
  "Return the EAF type in ID-OR-SLUG."
  (let ((type (cond ((tlon-import-eaf-article-id-p id-or-slug)
		     'article)
		    ((tlon-import-eaf-tag-slug-p id-or-slug)
		     'tag)
		    (t (user-error "Not an ID or slug: %S" id-or-slug)))))
    type))

(provide 'tlon-import)
;;; tlon-import.el ends here
