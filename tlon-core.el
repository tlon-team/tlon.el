;;; tlon-core.el --- Core Babel functionality  -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; Homepage: https://github.com/tlon-team/tlon
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

(require 'elpaca)
(require 'paths)

;;;; User options

(defgroup tlon-core ()
  "`tlon-core' user options."
  :group 'emacs)

(defcustom tlon-debug nil
  "Whether to display debug messages."
  :type 'boolean
  :group 'tlon-core)

;;;; Variables

(defvar elpaca-repos-directory)
(defconst tlon-package-dir
  (file-name-concat elpaca-repos-directory "tlon/")
  "Directory where the Tlön package is located.")

(defconst tlon-repos
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
    (:name "meetings-group"
	   :abbrev "meetings-group"
	   :subtype meetings
	   :participants ("Pablo Stafforini" "Leonardo Picón" "Federico Stafforini"))
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
    (:name "tlon-front"
	   :project "tlon"
	   :abbrev "tlon-front"
	   :type development)
    (:name "tlon-content"
	   :project "tlon"
	   :abbrev "tlon-content"
	   :type development)
    (:name "tlon.el"
	   :project "tlon"
	   :abbrev "tlon.el"
	   :type emacs)
    (:name "sandbox"
	   :abbrev "sandbox"
	   :type test))
  "List of repos and associated properties.
The `:name' property is the full name of the repo, as it appears in the URL. The
`:abbrev' property is an abbreviated form of the name, used, for example, for
creating `org-mode' TODOs.")

(defcustom tlon-email-shared
  "tlon.shared@gmail.com"
  "Tlön shared gmail address."
  :type 'string
  :group 'tlon-core)

;;;;; Paths

(defvar tlon-refs-dir
  paths-dir-babel-refs
  "Directory of the `babel-refs' repo.")

(defvar tlon-bibtex-dir
  (file-name-concat tlon-refs-dir "bib/")
  "Directory where BibTeX files are stored.")

(defvar tlon-file-fluid
  (file-name-concat tlon-bibtex-dir "fluid.bib")
  "File containing the fluid bibliography.")

(defvar tlon-file-stable
  (file-name-concat tlon-bibtex-dir "stable.bib")
  "File containing the stable bibliography.")

(defvar tlon-bibliography-files
  `(,tlon-file-fluid
    ,tlon-file-stable)
  "List of bibliography files.")

;;;;; Numbers

(defconst tlon-number-separated-by-separator
  "\\(?1:[[:digit:]]\\{1,4\\}\\(?:\\(?2:%s\\)[[:digit:]]\\{3\\}\\)*\\(?:\\(?3:%s\\)[[:digit:]]+\\)?\\)"
  "Pattern to match numbers separated by separator.
The first placeholder is for the thousands separator, the second one for the
decimal separator.

We allow for up to four (rather than three) digits before the first separator to
accommodate cases when the thousands separator is not used, such as years or
amounts between 1000 and 9999.")

(defconst tlon-default-thousands-separator " "
  "The default thousands separator.")

(defconst tlon-language-specific-thousands-separator
  '((:language "en" :separator ",")
    (:language "es" :separator ".")
    (:language "fr" :separator " ")
    (:language "it" :separator "."))
  "List of language-specific thousands separators.
These are used in TTS processes to ensure that the numbers are pronounced
correctly.")

;;;;; To sort

(defvar tlon-users
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

(defvar tlon-translation-language "es"
  "The current translation language.")

(defconst tlon-core-bare-dirs
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

(defconst tlon-project-languages
  '("english" "spanish" "italian" "french")
  "A list of languages all languages in the Babel project.")

(defconst tlon-project-target-languages
  (remove "english" tlon-project-languages)
  "A list of languages all languages in the Babel project minus English.")

(defconst tlon-languages-properties
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

(defun tlon-set-dir (repo)
  "Set the `:directory' property for REPO in `tlon-repos'."
  (let* ((dir (file-name-as-directory
	       (file-name-concat paths-dir-tlon-repos
				 (plist-get repo :name)))))
    (plist-put repo :dir dir)))

(mapc #'tlon-set-dir tlon-repos)

;;;;; Get repo

(defun tlon-get-repo (&optional no-prompt include-all)
  "Get Babel repository path.
If the current directory matches any of the directories in
`tlon-repos', return it. Else, prompt the user to select a repo from
that list, unless NO-PROMPT is non-nil. In that case, signal an error if its
value is `error', else return nil. If INCLUDE-ALL is non-nil, include all repos.
In that case, matching will be made against repos with any value for the
property `:type'."
  (if-let ((current-repo (tlon-get-repo-from-file)))
      current-repo
    (if no-prompt
	(when (eq no-prompt 'error)
	  (user-error "Not in a recognized Babel repo"))
      (let* ((content (tlon-repo-lookup-all :name :subtype 'translations))
	     (all (tlon-repo-lookup-all :name)))
	(tlon-repo-lookup :dir :name
			  (completing-read "Select repo: "
					   (if include-all all content)))))))

(declare-function files-extras-buffer-file-name "files-extras")
(defun tlon-get-repo-from-file (&optional file)
  "Return the repo to which FILE belongs.
If FILE is nil, use the current buffer's file name."
  (let* ((file (or file (files-extras-buffer-file-name) default-directory))
	 (directory-path (file-name-directory file)))
    (catch 'found
      (dolist (dir (tlon-repo-lookup-all :dir))
	(when (string-prefix-p (file-name-as-directory dir)
			       directory-path)
	  (throw 'found dir))))))

(declare-function files-extras-get-nth-directory "files-extras")
(defun tlon-get-repo-from-dir (dir)
  "Return the repo to which DIR belongs.
This function is meant to handles cases in which the `.git' directory of a repo
is outside that repo’s directory."
  (let ((dir-cands
	 (mapcar (lambda (repo-dir)
		   (string-trim
		    (files-extras-get-nth-directory (file-relative-name dir repo-dir))
		    nil "/"))
		 (list paths-dir-tlon-repos paths-dir-split-git))))
    (catch 'found
      (dolist (dir-cand dir-cands)
	(when-let ((repo-dir (tlon-repo-lookup :dir :name dir-cand)))
	  (throw 'found repo-dir))))))

;;;;; Get language

;; TODO: we also have `tlon-translation-language'; think about what to do
(defun tlon-get-language (&optional error)
  "Get the current language.
If ERROR is non-nil, signal an error if the language is not found. Otherwise,
return nil."
  (let ((repo (tlon-get-repo (when error 'error))))
    (tlon-repo-lookup :language :dir repo)))

;;;;; Lookup

;;;;;; Common

(defun tlon-all-pairs-in-entry-p (pairs entry case-insensitive)
  "Return t iff all PAIRS are found in ENTRY.
PAIRS is an even-sized list of <key value> tuples. If CASE-INSENSITIVE is
non-nil, match regardless of case."
  (let ((fun (if case-insensitive 'cl-equalp 'equal)))
    (cl-loop for (key val) on pairs by #'cddr
	     always (funcall fun val (tlon-get-value-in-entry key entry)))))

(defun tlon-get-value-in-entry (key entry)
  "Return the value of KEY in ENTRY, or nil if not found."
  (if (stringp key)
      (alist-get key entry nil nil 'string=)
    (plist-get entry key)))

;;;;;; Lookup one

(defun tlon-lookup-builder (list key case-insensitive &rest pairs)
  "Return the first value of KEY in LIST matching all PAIRS.
PAIRS is an even-sized list of <key value> tuples."
  (cl-loop for entry in list
	   when (tlon-all-pairs-in-entry-p pairs entry case-insensitive)
	   return (tlon-get-value-in-entry key entry)))

(defun tlon-lookup (list key &rest pairs)
  "Return the first value of KEY in LIST matching all PAIRS.
PAIRS is an even-sized list of <key value> tuples."
  (apply 'tlon-lookup-builder list key nil pairs))

(defun tlon-lookup-case-insensitive (list key &rest pairs)
  "Return the first value of KEY in LIST matching all PAIRS, regardless of case.
PAIRS is an even-sized list of <key value> tuples."
  (apply 'tlon-lookup-builder list key t pairs))

;;;;;; Lookup all

(defun tlon-lookup-all-builder (list key case-insensitive &rest pairs)
  "Return all unique values of KEY in LIST matching all PAIRS.
PAIRS is expected to be an even-sized list of <key value> tuples."
  (let (results)
    (cl-loop for entry in list
	     do (when (tlon-all-pairs-in-entry-p pairs entry case-insensitive)
		  (when-let* ((result (tlon-get-value-in-entry key entry))
			      (flat-result (if (listp result) result (list result))))
		    (dolist (r flat-result)
		      (push r results)))))
    (delete-dups (nreverse results))))

(defun tlon-lookup-all (list key &rest pairs)
  "Return all unique values of KEY in LIST matching all PAIRS.
PAIRS is expected to be an even-sized list of <key value> tuples."
  (apply #'tlon-lookup-all-builder list key nil pairs))

(defun tlon-lookup-all-case-insensitive (list key &rest pairs)
  "Return all unique values of KEY in LIST matching all PAIRS, regardless of case.
PAIRS is expected to be an even-sized list of <key value> tuples."
  (apply #'tlon-lookup-all-builder list key t pairs))

;;;;;; Metadata lookup

(defun tlon-metadata-lookup (metadata key &rest key-value)
  "Return the value of KEY in METADATA matching all KEY-VALUE pairs."
  (apply #'tlon-lookup metadata key key-value))

(defun tlon-metadata-lookup-all (metadata key &rest key-value)
  "Return all unique values of KEY in METADATA matching alll KEY-VALUE pairs."
  (apply #'tlon-lookup-all metadata key key-value))

;;;;;; Repo lookup

(defun tlon-repo-lookup (key &rest key-value)
  "Return the value of KEY in repos matching all KEY-VALUE pairs."
  (apply #'tlon-lookup tlon-repos key key-value))

(defun tlon-repo-lookup-all (key &rest key-value)
  "Return all unique values of KEY in repos matching all KEY-VALUE pairs."
  (apply #'tlon-lookup-all tlon-repos key key-value))

;;;;;; User lookup

(defun tlon-user-lookup (key &rest key-value)
  "Return the value of KEY in users matching all KEY-VALUE pairs."
  (apply #'tlon-lookup tlon-users key key-value))

(defun tlon-user-lookup-all (key &rest key-value)
  "Return all unique values of KEY in users matching all KEY-VALUE pairs."
  (apply #'tlon-lookup-all tlon-users key key-value))

;;;;;; Label lookup

(defvar tlon-job-labels)
(defun tlon-label-lookup (key &rest key-value)
  "Return the value of KEY in labels matching all KEY-VALUE pairs."
  (apply #'tlon-lookup tlon-job-labels key key-value))

(defun tlon-label-lookup-all (key &rest key-value)
  "Return all values of KEY in labels matching all KEY-VALUE pairs."
  (apply #'tlon-lookup-all tlon-job-labels key key-value))

(declare-function emacsql "emacsql")
(declare-function forge-db "forge-db")
(declare-function forge-get-repository "forge-core")
(declare-function forge-get-issue "forge-core")
(defun tlon-issue-lookup (string &optional dir)
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

(defun tlon-get-delimited-region-pos (begin &optional end)
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

;;;;; Bare dirs

(defun tlon-get-bare-dir-translation (target-lang source-lang bare-dir)
  "For BARE-DIR in SOURCE-LANG, get its translation into TARGET-LANG."
  (let (result)
    (dolist (outer tlon-core-bare-dirs result)
      (dolist (inner outer)
	(when (and (equal (cdr inner) bare-dir)
		   (equal (car inner) source-lang))
	  (setq result (cdr (assoc target-lang outer))))))))

(defun tlon-get-bare-dir (&optional file)
  "Get the bare directory of FILE.
A file’s bare directory is its directory minus its repository. For example, the
bare directory of `~/Dropbox/repos/uqbar-es/autores/' is `autores'.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer."
  (when-let* ((file (or file (buffer-file-name)))
	      (repo (tlon-get-repo-from-file file)))
    (directory-file-name (file-name-directory (file-relative-name file repo)))))

(defun tlon-select-bare-dir (lang)
  "Set the bare dir in LANG."
  (let* ((bare-dirs (tlon-lookup-all tlon-core-bare-dirs lang)))
    (completing-read "Type: " bare-dirs)))

;;;;; Misc

(defun tlon-concatenate-list (list)
  "Concatenate LIST into a string with commas and `and' as appropriate."
  (if (cdr list)
      (let ((all-but-last (mapconcat #'identity (butlast list) ", "))
	    (last (car (last list))))
	(format "%s and %s" all-but-last last))
    (car list)))

;;;;; language

(defun tlon-validate-language (language &optional format)
  "If LANGUAGE is a valid language, return it.
The validation is case-insensitive, but the returned language is in lowercase.
If FORMAT is `code', validate against the ISO 639-1 code. Otherwise,
validate a list of natural languages."
  (let ((language (downcase language))
	(format (pcase format
		  ('code :code)
		  (_ :name))))
    (when (member language (mapcar (lambda (language)
				     (plist-get language format))
				   tlon-languages-properties))
      language)))

(defun tlon-get-iso-code (language)
  "Return the two-letter ISO 639-1 code for LANGUAGE."
  (if (= (length language) 2)
      language
    (when-let* ((downcased (downcase language))
		(code-raw (tlon-lookup tlon-languages-properties :code :name downcased)))
      (string-limit code-raw 2))))

(defun tlon-select-language (&optional format babel multiple)
  "Prompt the user to select a LANGUAGE and return it in FORMAT.
If FORMAT is `code', return the two-letter code of the language (e.g.
\"es\"). If it is `locale', return the predefined locale for that language (e.g.
\"en-US\"). Otherwise, return the original selection (e.g. \"english\").

By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project.

If MULTIPLE is non-nil, allow the user to select multiple languages. In that
case, the return value will be a list of strings rather than a string."
  (let* ((selection (if multiple
			(tlon-read-multiple-languages babel)
		      (tlon-read-language babel))))
    (pcase format
      ((or 'code 'locale) (tlon-get-formatted-languages selection format))
      (_ selection))))

(defun tlon-get-formatted-languages (selection format)
  "Return the language SELECTION in appropriate FORMAT.
SELECTION is either a string or a list of strings representing languages in
English. FORMAT must be either `code' or `locale'."
  (let* ((property (pcase format ('locale :locale) ('code :code)))
	 (fun (lambda (language)
		(tlon-lookup tlon-languages-properties property :name language))))
    (if (listp selection)
	(mapcar (lambda (language)
		  (funcall fun language))
		selection)
      (funcall fun selection))))

(defun tlon-read-language (&optional babel)
  "Read a language from a list of languages.
By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project."
  (let* ((language-candidates (tlon-get-language-candidates babel)))
    (completing-read "Language: " language-candidates nil t)))

(defun tlon-read-multiple-languages (&optional babel)
  "Read a list of languages from a list of languages.
By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project."
  (let* ((language-candidates (tlon-get-language-candidates babel))
	 (language-selection (completing-read-multiple "Languages (comma-separated): "
						       (append '("*all*") language-candidates))))
    (if (member "*all*" language-selection) (mapcar 'car language-candidates) language-selection)))

(defun tlon-get-language-candidates (babel)
  "Return a list of language candidates.
If BABEL is nil, return all valid BibTeX languages; otherwise, return candidates
languages in the Babel project only."
  (if babel
      tlon-project-languages
    (tlon-lookup-all tlon-languages-properties :name)))

;;;;; json

(defun tlon-read-json (&optional file object-type array-type key-type)
  "Read JSON substring or FILE using array TYPE.
OBJECT-TYPE must be one of `alist' (default), `plist' or `hash-table'.
ARRAY-TYPE must be one of `list' (default) or `vector'. KEY-TYPE must be one of
`string' (default), `symbol' or `keyword'."
  (let ((json-object-type (or object-type 'alist))
	(json-array-type (or array-type 'list))
	(json-key-type (or key-type 'string))
	(json-false :json-false))
    (if file
	(json-read-file file)
      (json-read))))

(defun tlon-write-data (file data)
  "Write DATA to a JSON FILE."
  (with-temp-file file
    (insert (json-encode data))
    (json-pretty-print-buffer)))

(defun tlon-get-keys (data)
  "Get keys from hash table DATA."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) data)
    keys))

;;;;; tags

;; TODO: create pattern directly fro `tlon-tag-specs'
(defun tlon-make-tag-replace-pattern (pair &optional format)
  "Construct a replace with FORMAT for PAIR of tags."
  (let ((format (or format "%s%%s%s")))
    (format format (car pair) (cdr pair))))

;;;;; vars

(defun tlon-get-or-set-org-var (var id)
  "Get the value of VAR if set, else set it to the file containing `org-mode' ID."
  (or (symbol-value var) (set var (tlon-get-file-with-org-id id))))

(declare-function org-roam-id-find "org-roam-id")
(defun tlon-get-file-with-org-id (id)
  "Return the file containing the heading with the given `org-mode' ID."
  (when-let ((location (org-roam-id-find id)))
    (car location)))

;;;;; org-mode

(defun tlon-ensure-org-mode ()
  "Throw an error if the current buffer is not in `org-mode'."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an `org-mode' buffer")))

(declare-function org-up-heading-safe "org")
(declare-function org-sort-entries "org")
(defun tlon-sort-headings (&optional file)
  "Sort all headings under parent in FILE alphabetically and by TODO order."
  (interactive)
  (with-current-buffer (or (find-buffer-visiting file)
			   (find-file-noselect file))
    (widen)
    (org-up-heading-safe)
    (org-sort-entries nil ?a)
    (org-sort-entries nil ?o)
    (save-buffer)))

;;;;; transient

(defun tlon-transient-read-symbol-choice (prompt choices)
  "Return a list of CHOICES with PROMPT to be used as an `infix' reader function."
  (let* ((input (completing-read prompt (mapcar 'symbol-name choices))))
    (intern input)))

(defun tlon-transient-read-number-choice (prompt choices)
  "Return a list of CHOICES with PROMPT to be used as an `infix' reader function."
  (let* ((input (completing-read prompt (mapcar 'number-to-string choices))))
    (string-to-number input)))

(defun tlon-transient-read-string-choice (prompt choices)
  "Return a list of CHOICES with PROMPT to be used as an `infix' reader function."
  (let* ((input (completing-read prompt choices)))
    (substring-no-properties input)))

(defun tlon-transient-toggle-variable-value (var-name)
  "Toggle the value of the VAR-NAME."
  (if (symbol-value var-name)
      (set var-name nil)
    (set var-name t)))

;;;;; numbers

(defun tlon-get-separator (type &optional language)
  "Return the decimal separator for LANGUAGE.
TYPE is either `thousands' or `decimal'. If LANGUAGE is nil, use the language of
the current repository."
  (when-let* ((language (or language (tlon-get-language)))
	      (separators '("," "."))
	      (en (pcase type ('thousands ",") ('decimal ".")))
	      (rest (lambda () (car (remove en separators)))))
    (pcase language
      ("en" en)
      ((or "es" "fr" "it" "pt") (funcall rest)))))

(defun tlon-get-decimal-separator (&optional language)
  "Return the decimal separator for LANGUAGE.
If LANGUAGE is nil, use the language of the current repository."
  (tlon-get-separator 'decimal language))

(defun tlon-get-thousands-separator (&optional language)
  "Return the thousands separator for LANGUAGE.
If LANGUAGE is nil, use the language of the current repository."
  (tlon-get-separator 'thousands language))

(defun tlon-get-number-separator-pattern (&optional thousands decimal bounded)
  "Return the pattern that matches a number with THOUSANDS and DECIMAL separators.
If THOUSANDS or DECIMAL are nil, infer them from the language of the current
repository. If BOUNDED is non-nil, match only text at the beginning or end of a
word."
  (when-let* ((thousands (or thousands (tlon-get-thousands-separator)))
	      (decimal (or decimal (tlon-get-decimal-separator)))
	      (pattern (format tlon-number-separated-by-separator (regexp-quote thousands) (regexp-quote decimal))))
    (if bounded (format "\\b%s\\b" pattern) pattern)))

(defun tlon-string-to-number (string &optional thousands decimal)
  "Convert STRING with THOUSANDS and DECIMAL separators into a number.
If THOUSANDS or DECIMAL are nil, infer them from the language of the current
repository."
  (let* ((thousands (or thousands (tlon-get-thousands-separator)))
	 (decimal (or decimal (tlon-get-decimal-separator)))
	 (fixed-thousands (replace-regexp-in-string (regexp-quote thousands) "" string))
	 (fixed-decimals (replace-regexp-in-string (regexp-quote decimal) "." fixed-thousands)))
    (string-to-number fixed-decimals)))

;;;;; search

(declare-function elgrep "elgrep")
(defun tlon-grep (string extension)
  "Perform a ripgrep search for STRING."
  (interactive (list (read-string "Search string: ")
		     (completing-read "Extension: " '("md" "el" "") nil t)))
  (let* ((core-settings '(:interactive t :recursive t))
	 (no-context-settings '(:c-beg-only t :c-end-only t))
	 (context (y-or-n-p "Context?"))
	 (settings (append core-settings (unless context no-context-settings)))
	 (extension (unless (string-empty-p extension) (format "\\.%s" extension))))
    (apply #'elgrep nil extension string settings)))

(provide 'tlon-core)
;;; tlon-core.el ends here
