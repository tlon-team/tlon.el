;;; tlon-babel-core.el --- Core Babel functionality  -*- lexical-binding: t -*-

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

;; Core Babel functionality.

;;; Code:

(require 'bibtex-extras)
(require 'forge-core)
(require 'paths)

;;;; Variables

(defconst tlon-babel-repos
  `((:name "babel-core"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-core"
	   :type meta
	   :key "b c")
    (:name "babel-refs"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-refs"
	   :type meta
	   :subtype biblio
	   :key "b r")
    (:name "babel-es"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-es"
	   :type meta
	   :language "es"
	   :key "b s")
    (:name "babel-fr"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-fr"
	   :type meta
	   :language "fr"
	   :key "b f")
    (:name "babel-it"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-it"
	   :type meta
	   :language "it"
	   :key "b t")
    (:name "babel-de"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-de"
	   :type meta
	   :language "de"
	   :key "b d")
    (:name "babel-issues"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-issues"
	   :type development
	   :subtype issues
	   :key "b i")
    (:name "uqbar-issues"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-issues"
	   :type development
	   :subtype issues
	   :key "q i")
    (:name "uqbar-front"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-front"
	   :type development
	   :subtype front
	   :key "q f")
    (:name "uqbar-api"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-api"
	   :type development
	   :subtype api
	   :key "q a")
    (:name "uqbar-en"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-en"
	   :type content
	   :subtype originals
	   :language "en"
	   :key "q n")
    (:name "uqbar-es"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-es"
	   :type content
	   :subtype translations
	   :language "es"
	   :key "q s"
	   :url "https://altruismoeficaz.net/")
    (:name "uqbar-fr"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-fr"
	   :type content
	   :subtype translations
	   :language "fr"
	   :key "q f"
	   :url "")
    (:name "uqbar-it"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-it"
	   :type content
	   :subtype translations
	   :language "it"
	   :key "q t"
	   :url "")
    (:name "utilitarianism-en"
	   :project "babel"
	   :subproject "utilitarianism"
	   :abbrev "utilitarianism-en"
	   :type content
	   :subtype originals
	   :key "u n")
    (:name "utilitarianism-es"
	   :project "babel"
	   :subproject "utilitarianism"
	   :abbrev "utilitarianism-es"
	   :type content
	   :subtype translations
	   :language "es"
	   :key "u s"
	   :url "https://utilitarismo.net/")
    (:name "essays-en"
	   :project "babel"
	   :subproject "essays"
	   :abbrev "essays-en"
	   :type content
	   :subtype originals
	   :key "l n")
    (:name "essays-es"
	   :project "babel"
	   :subproject "essays"
	   :abbrev "essays-es"
	   :type content
	   :subtype translations
	   :language "es"
	   :key "l s")
    (:name "ea.news-issues"
	   :project "other"
	   :subproject "ea.news"
	   :abbrev "ean-issues"
	   :type development
	   :subtype issues
	   :key "e i")
    (:name "ea.news-front"
	   :project "other"
	   :subproject "ea.news"
	   :abbrev "ean-front"
	   :type development
	   :subtype front
	   :key "f")
    (:name "ea.news-api"
	   :project "other"
	   :subproject "ea.news"
	   :abbrev "ean-api"
	   :type development
	   :subtype api
	   :key "a")
    (:name "ea.international"
	   :project "other"
	   :subproject "ea.international"
	   :abbrev "ea-international"
	   :type meta
	   :key "i")
    (:name "bisagra-dev"
	   :project "other"
	   :subproject "bisagra"
	   :abbrev "bisagra-dev"
	   :type development
	   :key "d")
    (:name "bisagra-content"
	   :project "other"
	   :subproject "bisagra"
	   :abbrev "bisagra-content"
	   :type content
	   :key "c")
    (:name "boletin"
	   :project "other"
	   :subproject "boletin"
	   :abbrev "boletin"
	   :type misc
	   :key "a")
    (:name "meetings-leo-pablo"
	   :abbrev "meetings-leo-pablo"
	   :subtype meetings
	   :participants ("Pablo Stafforini" "Leonardo Picón"))
    (:name "meetings-fede-pablo"
	   :abbrev "meetings-fede-pablo"
	   :subtype meetings
	   :participants ("Pablo Stafforini" "Federico Stafforini"))
    (:name "meetings-fede-leo"
	   :abbrev "meetings-fede-leo"
	   :subtype meetings
	   :participants ("Federico Stafforini" "Leonardo Picón"))
    (:name "tlon-docs"
	   :project "other"
	   :subproject "docs"
	   :abbrev "docs"
	   :type docs)
    (:name "tlon-site"
	   :project "tlon"
	   :subproject "docs"
	   :abbrev "tlon-site"
	   :type development)
    (:name "sandbox"
	   :abbrev "sandbox"
	   :type test))
  "List of repos and associated properties.
The `:name' property is the full name of the repo, as it appears in the URL. The
`:abbrev' property is an abbreviated form of the name, used, for example, for
creating `org-mode' TODOs.")

;;;;; To sort

(defvar tlon-babel-users
  '((:name "Pablo Stafforini"
	   :git "Pablo Stafforini"
	   :github "benthamite"
	   :substitute "worldsaround")
    (:name "Federico Stafforini"
	   :git "Federico Stafforini"
	   :github "fstafforini")
    (:name "Leonardo Picón"
	   :git "cartago"
	   :github "worldsaround"
	   :substitute "benthamite"))
  "Property list of users and associated properties.
The special property `:substitute' is used to determine which user should
perform a given phase of the translation process when the designated user is not
the actual user.")

(defvar tlon-babel-translation-language "es"
  "The current translation language.")

(defconst tlon-babel-core-bare-dirs
  '((("en" . "articles")
     ("es" . "articulos"))
    (("en" . "tags")
     ("es" . "temas"))
    (("en". "authors")
     ("es" . "autores"))
    (("en" . "collections")
     ("es" . "colecciones")))
  "Alist of bare directories and associated translations.")

(defconst tlon-babel-languages
  '(("english" . "en")
    ("spanish" . "es")
    ("italian" . "it")
    ("french" . "fr")
    ("german" . "de")
    ("portuguese" . "pt")
    ("arabic" . "ar"))
  "Alist of languages and associated names.")

(defconst tlon-babel-locales
  '(("english" . "en-US")
    ("spanish" . "es-ES")
    ("italian" . "it-IT")
    ("french" . "fr-FR")
    ("german" . "de-DE")
    ("portuguese" . "pt-PT")
    ("arabic"  "ar-AR"))
  "Alist of languages and associated locales.")

;;;; Functions

(defun tlon-babel-set-dir (repo)
  "Set the `:directory' property for REPO in `tlon-babel-repos'."
  (let* ((dir (file-name-as-directory
	       (file-name-concat paths-dir-tlon-repos
				 (plist-get repo :name)))))
    (plist-put repo :dir dir)))

(mapc #'tlon-babel-set-dir tlon-babel-repos)

;;;;; Ger repo

(defun tlon-babel-get-repo (&optional no-prompt include-all)
  "Get Babel repository path.
If the current directory matches any of the directories in
`tlon-babel-repos', return it. Else, prompt the user to select a repo from
that list, unless NO-PROMPT is non-nil. In that case, signal an error if its
value is `error', else return nil. If INCLUDE-ALL is non-nil, include all repos.
In that case, matching will be made against repos with any value for the
property `:type'."
  (if-let ((current-repo (tlon-babel-get-repo-from-file)))
      current-repo
    (if no-prompt
	(when (eq no-prompt 'error)
	  (user-error "Not in a recognized Babel repo"))
      (let* ((content (tlon-babel-repo-lookup-all :name :subtype 'translations))
	     (all (tlon-babel-repo-lookup-all :name)))
	(tlon-babel-repo-lookup :dir :name
				(completing-read "Select repo: "
						 (if include-all all content)))))))

(defun tlon-babel-get-repo-from-file (&optional file)
  "Return the repo to which FILE belongs.
If FILE is nil, use the current buffer's file name."
  (let* ((file (or file (tlon-babel-core-buffer-file-name) default-directory))
	 (directory-path (file-name-directory file)))
    (catch 'found
      (dolist (dir (tlon-babel-repo-lookup-all :dir))
	(when (string-prefix-p (file-name-as-directory dir)
			       directory-path)
	  (throw 'found dir))))))

;;;;; Lookup

(defun tlon-babel-lookup (list key &rest pairs)
  "Return the first value of KEY in LIST matching all PAIRS.
PAIRS is an even-sized list of <key value> tuples."
  (cl-loop for entry in list
           when (tlon-babel-all-pairs-in-entry-p pairs entry)
           return (tlon-babel-get-value-in-entry key entry)))

(defun tlon-babel-lookup-all (list key &rest pairs)
  "Return all unique values of KEY in LIST matching all PAIRS.
PAIRS is expected to be an even-sized list of <key value> tuples."
  (let (results)
    (cl-loop for entry in list
             do (when (tlon-babel-all-pairs-in-entry-p pairs entry)
		  (when-let* ((result (tlon-babel-get-value-in-entry key entry))
			      (flat-result (if (listp result) result (list result))))
		    (dolist (r flat-result)
		      (push r results)))))
    (delete-dups (nreverse results))))

(defun tlon-babel-all-pairs-in-entry-p (pairs entry)
  "Return t iff all PAIRS are found in ENTRY.
PAIRS is an even-sized list of <key value> tuples."
  (cl-loop for (key val) on pairs by #'cddr
           always (equal val (tlon-babel-get-value-in-entry key entry))))

(defun tlon-babel-get-value-in-entry (key entry)
  "Return the value of KEY in ENTRY, or nil if not found."
  (if (stringp key)
      (alist-get key entry nil nil 'string=)
    (plist-get entry key)))

(defun tlon-babel-metadata-lookup (metadata key &rest key-value)
  "Return the value of KEY in METADATA matching all KEY-VALUE pairs."
  (apply #'tlon-babel-lookup metadata key key-value))

(defun tlon-babel-metadata-lookup-all (metadata key &rest key-value)
  "Return all unique values of KEY in METADATA matching alll KEY-VALUE pairs."
  (apply #'tlon-babel-lookup-all metadata key key-value))

(defun tlon-babel-repo-lookup (key &rest key-value)
  "Return the value of KEY in repos matching all KEY-VALUE pairs."
  (apply #'tlon-babel-lookup tlon-babel-repos key key-value))

(defun tlon-babel-repo-lookup-all (key &rest key-value)
  "Return all unique values of KEY in repos matching all KEY-VALUE pairs."
  (apply #'tlon-babel-lookup-all tlon-babel-repos key key-value))

(defun tlon-babel-user-lookup (key &rest key-value)
  "Return the value of KEY in users matching all KEY-VALUE pairs."
  (apply #'tlon-babel-lookup tlon-babel-users key key-value))

(defun tlon-babel-user-lookup-all (key &rest key-value)
  "Return all unique values of KEY in users matching all KEY-VALUE pairs."
  (apply #'tlon-babel-lookup-all tlon-babel-users key key-value))

(defun tlon-babel-label-lookup (key &rest key-value)
  "Return the value of KEY in labels matching all KEY-VALUE pairs."
  (apply #'tlon-babel-lookup tlon-babel-job-labels key key-value))

(defun tlon-babel-label-lookup-all (key &rest key-value)
  "Return all values of KEY in labels matching all KEY-VALUE pairs."
  (apply #'tlon-babel-lookup-all tlon-babel-job-labels key key-value))

(defun tlon-babel-core-buffer-file-name ()
  "Return name of file BUFFER is visiting, handling `git-dirs' path."
  (when-let ((file (buffer-file-name)))
    (replace-regexp-in-string "git-dirs/"
			      (file-relative-name paths-dir-tlon-repos "~/")
			      (buffer-file-name))))

(defun tlon-babel-issue-lookup (string &optional dir)
  "Return the first issue in DIR whose title includes STRING.
If DIR is nil, use the current repository."
  (let* ((string (concat "%" string "%"))
	 (default-directory (or dir default-directory))
	 (repo (forge-get-repository t))
	 (issue-id (caar (emacsql (forge-db)
				  [:select [number]
					   :from 'issue
					   :where (and (= repository $s1)
						       (like title $s2))]
				  (oref repo id)
				  string))))
    (when issue-id
      (forge-get-issue repo issue-id))))

;;;;; Get region pos

(defun tlon-babel-get-delimited-region-pos (begin &optional end)
  "Get the position of the region delimited by BEGIN and END.
If END is nil, use BEGIN also as the end delimiter."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward begin nil t)
	(let* ((begin-pos (match-beginning 0))
	       (end-pos (when (re-search-forward (or end begin) nil t)
			  (match-end 0))))
	  (when (and begin-pos end-pos)
	    (cons begin-pos end-pos)))))))

;;;;; Misc
;; this function will eventually be deleted once we migrate to a system of English-only directory names

(defun tlon-babel-get-bare-dir-translation (target-lang source-lang bare-dir)
  "For BARE-DIR in SOURCE-LANG, get its translation into TARGET-LANG."
  (let (result)
    (dolist (outer tlon-babel-core-bare-dirs result)
      (dolist (inner outer)
	(when (and (equal (cdr inner) bare-dir)
		   (equal (car inner) source-lang))
	  (setq result (cdr (assoc target-lang outer))))))))

(defun tlon-babel-concatenate-list (list)
  "Concatenate LIST into a string with commas and `and' as appropriate."
  (if (cdr list)
      (let ((all-but-last (mapconcat #'identity (butlast list) ", "))
	    (last (car (last list))))
	(format "%s and %s" all-but-last last))
    (car list)))

;;;;; language

(defun tlon-babel-validate-language (language)
  "If LANGUAGE is a valid language, return it.
The validation is case-insensitive, but the returned language is in lowercase."
  (let ((language (downcase language)))
    (when (member language (mapcar #'car bibtex-extras-valid-languages))
      language)))

(defun tlon-babel-get-two-letter-code (language)
  "Return the two-letter code for LANGUAGE."
  (if (= (length language) 2)
      language
    (when-let* ((downcased (downcase language))
		(code-raw (alist-get downcased bibtex-extras-valid-languages nil nil #'string=)))
      (string-limit code-raw 2))))

(defun tlon-babel-select-language (&optional format babel multiple)
  "Prompt the user to select a LANGUAGE and return it in FORMAT.
If FORMAT is `two-letter', return the two-letter code of the language (e.g.
\"es\"). If it is `locale', return the predefined locale for that language (e.g.
\"en-US\"). Otherwise, return the original selection (e.g. \"english\").

By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project.

If MULTIPLE is non-nil, allow the user to select multiple languages. In that
case, the return value will be a list of strings rather than a string."
  (let* ((selection (if multiple
			(tlon-babel-read-multiple-languages babel)
		      (tlon-babel-read-language babel))))
    (pcase format
      ('two-letter (tlon-babel-get-formatted-languages selection format))
      ('locale (tlon-babel-get-formatted-languages selection format))
      (_ selection))))

(defun tlon-babel-get-formatted-languages (selection format)
  "Return the language SELECTION in appropriate FORMAT.
FORMAT must be either `two-letter' or `locale'. SELECTION is either a string or
a list of strings representing languages in English."
  (let ((languages (pcase format
		     ('locale tlon-babel-locales)
		     ('two-letter bibtex-extras-valid-languages))))
    (if (listp selection)
	(mapcar (lambda (language)
		  "Return the two-letter code or the locale for LANGUAGE."
		  (alist-get language languages nil nil #'string=))
		selection)
      (alist-get selection languages nil nil #'string=))))

(defun tlon-babel-read-language (&optional babel)
  "Read a language from a list of languages.
By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project."
  (let* ((language-candidates (if babel tlon-babel-languages bibtex-extras-valid-languages)))
    (completing-read "Language: " language-candidates nil t)))

(defun tlon-babel-read-multiple-languages (&optional babel)
  "Read a list of languages from a list of languages.
By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project."
  (let* ((language-candidates (if babel tlon-babel-languages bibtex-extras-valid-languages))
	 (language-selection (completing-read-multiple "Languages (comma-separated): "
						       (append '("*all*") language-candidates))))
    (if (member "*all*" language-selection) (mapcar 'car language-candidates) language-selection)))

;;;;; json

(defun tlon-babel-parse-json (file &optional object-type array-type key-type)
  "Parse JSON FILE using array TYPE.
OBJECT-TYPE must be one of `alist' (default), `plist' or `hash-table'.
ARRAY-TYPE must be one of `list' (default) or `vector'. KEY-TYPE must be one of
`string' (default), `symbol' or `keyword'."
  (let ((json-object-type (or object-type 'alist))
	(json-array-type (or array-type 'list))
	(json-key-type (or key-type 'string))
	(json-false :json-false))
    (json-read-file file)))

(defun tlon-babel-write-data (file data)
  "Write DATA to a JSON FILE."
  (with-temp-file file
    (insert (json-encode data))
    (json-pretty-print-buffer)))

(defun tlon-babel-get-keys (data)
  "Get keys from hash table DATA."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) data)
    keys))

;;;;; tags

(defun tlon-babel-make-tag-search-pattern (pair &optional format)
  "Construct a regexp match pattern with FORMAT for PAIR of tags."
  (let ((format (or format "\\(?1:%s\\(?2:\\(.\\|\n\\)*?\\)%s\\)")))
    (format format
	    (regexp-quote (car pair))
	    (regexp-quote (cdr pair)))))

(defun tlon-babel-make-tag-replace-pattern (pair &optional format)
  "Construct a replace with FORMAT for PAIR of tags."
  (let ((format (or format "%s%%s%s")))
    (format format
	    (car pair)
	    (cdr pair))))

(provide 'tlon-babel-core)
;;; tlon-babel-core.el ends here
