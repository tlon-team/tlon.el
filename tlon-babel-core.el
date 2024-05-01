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
    (:name "bisagra-api"
	   :project "other"
	   :subproject "bisagra"
	   :abbrev "bisagra-api"
	   :type development
	   :key "a")
    (:name "bisagra-front"
	   :project "other"
	   :subproject "bisagra"
	   :abbrev "bisagra-front"
	   :type development
	   :key "f")
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
    (:name "clock-pablo"
	   :abbrev "clock-pablo"
	   :subtype clock)
    (:name "clock-fede"
	   :abbrev "clock-fede"
	   :subtype clock)
    (:name "clock-leo"
	   :abbrev "clock-leo"
	   :subtype clock)
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

(defcustom tlon-babel-email-shared
  "tlon.shared@gmail.com"
  "Tlön shared gmail address."
  :type 'string
  :group 'tlon-core)

;;;;; To sort

(defvar tlon-babel-users
  '((:name "Pablo Stafforini"
	   :git "Pablo Stafforini"
	   :github "benthamite"
	   :substitute "worldsaround"
	   :nickname "Pablo")

    (:name "Federico Stafforini"
	   :git "Federico Stafforini"
	   :github "fstafforini"
	   :nickname "Fede")
    (:name "Leonardo Picón"
	   :git "cartago"
	   :github "worldsaround"
	   :substitute "benthamite"
	   :nickname "Leo"))
  "Property list of users and associated properties.
The special property `:substitute' is used to determine which user should
perform a given phase of the translation process when the designated user is not
the actual user.")

(defvar tlon-babel-translation-language "es"
  "The current translation language.")

(defconst tlon-babel-core-bare-dirs
  '((("en" . "articles")
     ("es" . "articulos")
     ("fr" . "articles")
     ("it" . "articoli"))
    (("en" . "tags")
     ("es" . "temas")
     ("fr" . "sujets")
     ("it" . "temi"))
    (("en". "authors")
     ("es" . "autores")
     ("fr" . "auteurs")
     ("it" . "autori"))
    (("en" . "collections")
     ("es" . "colecciones")
     ("fr" . "collections")
     ("it" . "collezioni")))
  "Alist of bare directories and associated translations.")

(defconst tlon-babel-project-languages
  '("english" "spanish" "italian" "french" "german" "portuguese" "arabic")
  "A list of languages all languages in the Babel project.")

(defconst tlon-babel-languages-properties
  '((:name "albanian" :code "sq" :locale "sq_AL")
    (:name "american" :code "en-US" :locale "en_US")
    (:name "amharic" :code "am" :locale "am_ET")
    (:name "arabic" :code "ar" :locale "ar_SA")
    (:name "argentinian" :code "es-AR" :locale "es_AR")
    (:name "armenian" :code "hy" :locale "hy_AM")
    (:name "assamese" :code "as" :locale "as_IN")
    (:name "asturian" :code "ast" :locale "ast_ES")
    (:name "austrian" :code "de-AT" :locale "de_AT")
    (:name "australian" :code "en-AU" :locale "en_AU")
    (:name "basque" :code "eu" :locale "eu_ES")
    (:name "belarusian" :code "be" :locale "be_BY")
    (:name "bengali" :code "bn" :locale "bn_BD")
    (:name "bosnian" :code "bs" :locale "bs_BA")
    (:name "breton" :code "br" :locale "br_FR")
    (:name "british" :code "en-GB" :locale "en_GB")
    (:name "bulgarian" :code "bg" :locale "bg_BG")
    (:name "canadian" :code "en-CA" :locale "en_CA")
    (:name "catalan" :code "ca" :locale "ca_ES")
    (:name "chinese" :code "zh" :locale "zh_CN")
    (:name "coptic" :code "cop" :locale "cop_EG")
    (:name "croatian" :code "hr" :locale "hr_HR")
    (:name "czech" :code "cs" :locale "cs_CZ")
    (:name "danish" :code "da" :locale "da_DK")
    (:name "dutch" :code "nl" :locale "nl_NL")
    (:name "english" :code "en" :locale "en_GB")
    (:name "esperanto" :code "eo" :locale "eo_EO")
    (:name "estonian" :code "et" :locale "et_EE")
    (:name "finnish" :code "fi" :locale "fi_FI")
    (:name "french" :code "fr" :locale "fr_FR")
    (:name "galician" :code "gl" :locale "gl_ES")
    (:name "georgian" :code "ka" :locale "ka_GE")
    (:name "german" :code "de" :locale "de_DE")
    (:name "greek" :code "el" :locale "el_GR")
    (:name "hebrew" :code "he" :locale "he_IL")
    (:name "hindi" :code "hi" :locale "hi_IN")
    (:name "hungarian" :code "hu" :locale "hu_HU")
    (:name "icelandic" :code "is" :locale "is_IS")
    (:name "interlingua" :code "ia" :locale "ia_IA")
    (:name "irish" :code "ga" :locale "ga_IE")
    (:name "italian" :code "it" :locale "it_IT")
    (:name "japanese" :code "ja" :locale "ja_JP")
    (:name "kannada" :code "kn" :locale "kn_IN")
    (:name "korean" :code "ko" :locale "ko_KR")
    (:name "lao" :code "lo" :locale "lo_LA")
    (:name "latin" :code "la" :locale "la_VA")
    (:name "latvian" :code "lv" :locale "lv_LV")
    (:name "lithuanian" :code "lt" :locale "lt_LT")
    (:name "macedonian" :code "mk" :locale "mk_MK")
    (:name "malayalam" :code "ml" :locale "ml_IN")
    (:name "marathi" :code "mr" :locale "mr_IN")
    (:name "mexican" :code "es-MX" :locale "es_MX")
    (:name "mongolian" :code "mn" :locale "mn_MN")
    (:name "naustrian" :code "de-AT" :locale "de_AT")
    (:name "newzealand" :code "en-NZ" :locale "en_NZ")
    (:name "ngerman" :code "de-DE" :locale "de_DE")
    (:name "nko" :code "nqo" :locale "nqo_GN")
    (:name "norwegian" :code "no" :locale "no_NO")
    (:name "oriya" :code "or" :locale "or_IN")
    (:name "persian" :code "fa" :locale "fa_IR")
    (:name "polish" :code "pl" :locale "pl_PL")
    (:name "portuguese" :code "pt" :locale "pt_PT")
    (:name "romanian" :code "ro" :locale "ro_RO")
    (:name "russian" :code "ru" :locale "ru_RU")
    (:name "sanskrit" :code "sa" :locale "sa_IN")
    (:name "serbian" :code "sr" :locale "sr_RS")
    (:name "slovak" :code "sk" :locale "sk_SK")
    (:name "slovenian" :code "sl" :locale "sl_SI")
    (:name "spanish" :code "es" :locale "es_ES")
    (:name "spanish" :code "es" :locale "es_ES")
    (:name "swedish" :code "sv" :locale "sv_SE")
    (:name "swissgerman" :code "de-CH" :locale "de_CH")
    (:name "tamil" :code "ta" :locale "ta_IN")
    (:name "telugu" :code "te" :locale "te_IN")
    (:name "thai" :code "th" :locale "th_TH")
    (:name "turkish" :code "tr" :locale "tr_TR")
    (:name "turkmen" :code "tk" :locale "tk_TM")
    (:name "ukenglish" :code "en-GB" :locale "en_GB")
    (:name "ukrainian" :code "uk" :locale "uk_UA")
    (:name "urdu" :code "ur" :locale "ur_PK")
    (:name "vietnamese" :code "vi" :locale "vi_VN")
    (:name "welsh" :code "cy" :locale "cy_GB")
    (:name "afrikaans" :code "af" :locale "af_ZA"))
  "Plist of valid `langid' names, locales and ISO 639-1 codes.")

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

(defvar tlon-babel-job-labels)
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

(declare-function emacsql "emacsql")
(declare-function forge-db "forge-db")
(declare-function forge-get-repository "forge-core")
(declare-function forge-get-issue "forge-core")
(defun tlon-babel-issue-lookup (string &optional dir)
  "Return the first issue in DIR whose title includes STRING.
If DIR is nil, use the current repository."
  (let* ((string (concat "%" string "%"))
	 (default-directory (or dir default-directory))
	 (repo (forge-get-repository :tracked))
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
    (when (member language (mapcar #'car tlon-babel-languages))
      language)))

(defun tlon-babel-get-iso-code (language)
  "Return the two-letter ISO 639-1 code for LANGUAGE."
  (if (= (length language) 2)
      language
    (when-let* ((downcased (downcase language))
		(code-raw (tlon-babel-lookup tlon-babel-languages-properties :code :name downcased)))
      (string-limit code-raw 2))))

(defun tlon-babel-select-language (&optional format babel multiple)
  "Prompt the user to select a LANGUAGE and return it in FORMAT.
If FORMAT is `code', return the two-letter code of the language (e.g.
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
      ((or 'code 'locale) (tlon-babel-get-formatted-languages selection format))
      (_ selection))))

(defun tlon-babel-get-formatted-languages (selection format)
  "Return the language SELECTION in appropriate FORMAT.
SELECTION is either a string or a list of strings representing languages in
English. FORMAT must be either `code' or `locale'."
  (let* ((property (pcase format ('locale :locale) ('code :code)))
	 (fun (lambda (language)
		(tlon-babel-lookup tlon-babel-languages-properties property :name language))))
    (if (listp selection)
	(mapcar (lambda (language)
		  (funcall fun language))
		selection)
      (funcall fun selection))))

(defun tlon-babel-read-language (&optional babel)
  "Read a language from a list of languages.
By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project."
  (let* ((language-candidates (if babel tlon-babel-languages tlon-babel-languages)))
    (completing-read "Language: " language-candidates nil t)))

(defun tlon-babel-read-multiple-languages (&optional babel)
  "Read a list of languages from a list of languages.
By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project."
  (let* ((language-candidates (if babel tlon-babel-languages tlon-babel-languages))
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
