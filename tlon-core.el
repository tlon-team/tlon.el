;;; tlon-core.el --- Core Tlön functionality  -*- lexical-binding: t -*-

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

;; Core Tlön functionality.

;;; Code:

(require 'elpaca)
(require 'json)
(require 'paths)

;;;; User options

(defgroup tlon-core ()
  "`tlon-core' user options."
  :group 'emacs)

(defcustom tlon-debug nil
  "Whether to display debug messages."
  :type 'boolean
  :group 'tlon-core)

(defmacro tlon-message-debug (format-string &rest args)
  "Display a message using FORMAT-STRING and ARGS if `tlon-debug' is non-nil."
  `(when tlon-debug
     (message ,format-string ,@args)))

;;;; Variables

;;;;; Files

(defconst tlon-package-dir
  (file-name-concat user-emacs-directory "elpaca/repos/tlon/")
  "Directory where the `tlon' package is located.")

;;;;; Repos

;; `abbrev' is for naming Emacs symbols, perhaps for other things
(defconst tlon-repos
  `((:name "babel-core"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-core"
	   :type meta
	   :key "g c"
	   :help t)
    (:name "babel-refs"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "babel-refs"
	   :type meta
	   :subtype biblio
	   :key "g r"
	   :help t)
    (:name "es"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "es"
	   :type meta
	   :language "es"
	   :key "l e s"
	   :help t)
    (:name "fr"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "fr"
	   :type meta
	   :language "fr"
	   :key "l f r"
	   :help t)
    (:name "it"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "it"
	   :type meta
	   :language "it"
	   :key "l i t"
	   :help t)
    (:name "de"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "de"
	   :type meta
	   :language "de"
	   :key "l d"
	   :help t)
    (:name "ru"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "ru"
	   :type meta
	   :language "ru"
	   :key "l r u"
	   :help t)
    (:name "polish-translations"
	   :project "babel"
	   :subproject "babel"
	   :abbrev "polish-translations"
	   :type content
	   :subtype translations
	   :language "pl"
	   :help nil)
    (:name "uqbar"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar"
	   :type development
	   :key "q q"
	   :help t)
    (:name "uqbar-audio-api"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-audio-api"
	   :type development
	   :subtype audio
	   :key "q p"
	   :help t)
    (:name "uqbar-audio-content"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-audio-content"
	   :type content
	   :subtype audio
	   :key "q d"
	   :help t)
    (:name "uqbar-ar"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-ar"
	   :type content
	   :subtype translations
	   :language "ar"
	   :key "q a"
	   :url "https://ihsanfaal.org"
	   :help t)
    (:name "uqbar-en"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-en"
	   :type content
	   :subtype originals
	   :language "en"
	   :key "q n"
	   :help t)
    (:name "uqbar-es"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-es"
	   :type content
	   :subtype translations
	   :language "es"
	   :key "q s"
	   :url "https://altruismoeficaz.net/"
	   :help t)
    (:name "uqbar-fr"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-fr"
	   :type content
	   :subtype translations
	   :language "fr"
	   :key "q f"
	   :url "https://altruismeefficace.net"
	   :help t)
    (:name "uqbar-it"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-it"
	   :type content
	   :subtype translations
	   :language "it"
	   :key "q t"
	   :url "https://altruismoefficace.net"
	   :help t)
    (:name "uqbar-ko"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-ko"
	   :type content
	   :subtype translations
	   :language "ko"
	   :key "q k"
	   :url "eakorea.org"
	   :help t)
    (:name "uqbar-ja"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-ja"
	   :type content
	   :subtype translations
	   :language "ja"
	   :key "q j"
	   :url ""
	   :help t)
    (:name "uqbar-ru"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-ru"
	   :type content
	   :subtype translations
	   :language "ru"
	   :key "q u"
	   :help t)
    (:name "uqbar-tr"
	   :project "babel"
	   :subproject "uqbar"
	   :abbrev "uqbar-tr"
	   :type content
	   :subtype translations
	   :language "tr"
	   :key "q u"
	   :url "https://efektifaltruizm.org"
	   :help t)
    (:name "web-server"
	   :project "tlon"
	   :abbrev "web-server"
	   :type development
	   :help t)
    (:name "utilitarianism-en"
	   :project "babel"
	   :subproject "utilitarianism"
	   :abbrev "utilitarianism-en"
	   :type content
	   :subtype originals
	   :key "u n"
	   :help t)
    (:name "utilitarianism-es"
	   :project "babel"
	   :subproject "utilitarianism"
	   :abbrev "utilitarianism-es"
	   :type content
	   :subtype translations
	   :language "es"
	   :key "u s"
	   :url "https://utilitarismo.net/"
	   :help t)
    (:name "ea.news"
	   :project "other"
	   :subproject "ea.news"
	   :abbrev "ea-news"
	   :type development
	   :subtype issues
	   :key "n n"
	   :help t)
    (:name "ea.international"
	   :project "other"
	   :subproject "ea.international"
	   :abbrev "ea-international"
	   :type meta
	   :key "i"
	   :help t)
    ;; (:name "80k-website"
    ;; :project "other"
    ;; :subproject "80k-website"
    ;; :abbrev "80k-website"
    ;; :type content
    ;; :key "w"
    ;; :help t)
    (:name "effectivealtruism.org"
	   :project "other"
	   :abbrev "effectivealtruism-org"
	   :type development
	   :help t)
    (:name "80k-podcast"
	   :project "other"
	   :subproject "80k-podcast"
	   :abbrev "80k-podcast"
	   :type content
	   :key "p"
	   :help t)
    (:name "AI-2027"
	   :project "other"
	   :subproject "AI-2027"
	   :abbrev "ai-2027"
	   :type content
	   :key "7"
	   :help t)
    (:name "aea"
	   :project "other"
	   :subproject "aea"
	   :abbrev "aea"
	   :type development
	   :key "r c"
	   :help t)
    (:name "aea-content"
	   :project "other"
	   :subproject "aea"
	   :abbrev "aea-content"
	   :type content
	   :key "r c"
	   :help t)
    (:name "altruismoeficaz.org"
	   :project "other"
	   :abbrev "altruismoeficaz-org"
	   :type content
	   :key "o"
	   :help t)
    (:name "bisagra"
	   :project "other"
	   :subproject "bisagra"
	   :abbrev "bisagra"
	   :type development
	   :key "s s"
	   :help t)
    (:name "bisagra-content"
	   :project "other"
	   :subproject "bisagra"
	   :abbrev "bisagra-content"
	   :type content
	   :key "s c"
	   :help t)
    (:name "boletin"
	   :project "other"
	   :subproject "boletin"
	   :abbrev "boletin"
	   :type misc
	   :key "a"
	   :help t)
    (:name "rational-animations"
	   :project "other"
	   :subproject "rational-animations"
	   :abbrev "rational-animations"
	   :type content
	   :key "r"
	   :help t)
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
	   :type docs
	   :help t)
    (:name "marketing"
	   :project "tlon"
	   :abbrev "marketing"
	   :type meta
	   :help t)
    (:name "tlon.el"
	   :project "tlon"
	   :abbrev "tlon-el"
	   :type development
	   :subtype emacs
	   :dir ,tlon-package-dir)
    (:name "tlon.team"
	   :project "tlon"
	   :abbrev "tlon-team"
	   :type development
	   :help t)
    (:name "tlon.team-content"
	   :project "tlon"
	   :abbrev "tlon-team-content"
	   :type content
	   :help t)
    (:name "tlon-generic"
	   :project "tlon"
	   :abbrev "tlon-generic"
	   :type meta
	   :subtype issues
	   :help nil)
    (:name "essays-en"
	   :project "babel"
	   :subproject "essays"
	   :abbrev "essays-en"
	   :type content
	   :subtype originals
	   :inactive t
	   :key "l n")
    (:name "essays-es"
	   :project "babel"
	   :subproject "essays"
	   :abbrev "essays-es"
	   :type content
	   :subtype translations
	   :language "es"
	   :inactive t
	   :key "l s")
    (:name "tlon-generic"
	   :project "tlon"
	   :abbrev "tlon-generic"
	   :type misc)
    (:name "sandbox"
	   :abbrev "sandbox"
	   :type test))
  "List of repos and associated properties.
The `:name' property is the full name of the repo, as it appears in the URL. The
`:abbrev' property is an abbreviated form of the name, used, for example, for
creating `org-mode' TODOs.

If `:help' is non-nil, include this repo in the list of directories scanned by
`tlon-ai-get-documentation-files'.")

;;;;; Email

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

(defvar tlon-file-db
  (file-name-concat tlon-bibtex-dir "db.bib")
  "File containing the db bibliography.")

(defvar tlon-file-db-upstream
  (file-name-concat tlon-bibtex-dir "db-upstream.bib")
  "File containing the db upstream bibliography.")

(defvar tlon-bibliography-files
  `(,tlon-file-fluid
    ,tlon-file-stable
    ,tlon-file-db)
  "List of Tlön bibliography files.")

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

(defconst tlon-language-separators
  '((:language "ar" :thousands " " :decimal ".")
    (:language "de" :thousands "." :decimal ",")
    (:language "en" :thousands "," :decimal ".")
    (:language "es" :thousands " " :decimal ",")
    (:language "fr" :thousands " " :decimal ",")
    (:language "it" :thousands "." :decimal ",")
    (:language "ja" :thousands "," :decimal ".")
    (:language "ko" :thousands "," :decimal ".")
    (:language "ru" :thousands " " :decimal ",")
    (:language "tr" :thousands "." :decimal ","))
  "Language-specific thousands and decimal separators.")

;;;;; EAF validation

(defconst tlon-eaf-base-regexp
  "forum\\.effectivealtruism\\.org"
  "Regular expression for matching the base EAF URL.")

(defconst tlon-eaf-id-regexp
  "[[:alnum:]]\\{17\\}"
  "Regular expression for matching EAF post or comment IDs.")

(defconst tlon-eaf-url-post-comment-id
  (format "\\?commentId=%s" tlon-eaf-id-regexp)
  "Regular expression for capturing the `commentid' element in post URLs.")

(defconst tlon-eaf-url-post-canonical
  (format "\\(?1:%s/posts/\\(?2:%s\\)\\)\\(?:/[-[:alnum:]]*\\)?\\(?3:%s\\)?"
	  tlon-eaf-base-regexp tlon-eaf-id-regexp tlon-eaf-url-post-comment-id)
  "Regular expression for validating canonical URLs of EAF posts.
The first group captures the entire canonical URL, the second group captures the
post ID.

The concatenated string following the first capture group was obtained from
`browse-url-button-regexp'. This is needed because the URLs may contain a slug
after the proper ID.")

(defconst tlon-eaf-url-post-collection
  (format "\\(?1:%s/s/\\(?3:%s\\)/p/\\(?2:%2$s\\)\\)" tlon-eaf-base-regexp tlon-eaf-id-regexp)
  "Regular expression for validating URLs of posts embedded in a collection.
The first group captures the entire URL, the second group captures the post ID
and the third group captures the collection ID.")

(defconst tlon-eaf-tag-slug-regexp
  "\\([[:alnum:]-]*\\)"
  "Regular expression for matching tag slugs.")

;;;;; Authorhip

(defconst tlon-authorship-pattern
  '((:language "ar" :pattern "بواسطة %s.")
    (:language "de" :pattern "von %s.")
    (:language "en" :pattern "by %s.")
    (:language "es" :pattern "por %s.")
    (:language "fr" :pattern "par %s.")
    (:language "it" :pattern "di %s.")
    (:language "ja" :pattern "%sによって".)
    (:language "ko" :pattern "%s에 의해.")
    (:language "ru" :pattern "от %s.")
    (:language "tr" :pattern "tarafından %s."))
  "Pattern to use when listing the author(s) of a work.
For example, in English, the pattern is `by %s', where `%s' is replaced by the
author name(s).")

;;;;; Conjuncts

(defconst tlon-tts-conjuncts
  '((:language "en" :conjunct "and")
    (:language "es" :conjunct "y")
    (:language "fr" :conjunct "et")
    (:language "it" :conjunct "e")
    (:language "de" :conjunct "und")
    (:language "ar" :conjunct "و")
    (:language "ko" :conjunct "그리고")
    (:language "ja" :conjunct "と"))
  "List of language-specific conjunctions.")

(defconst tlon-numerals-sans-separator
  "\\([ \\$£€][[:digit:]]\\{1,3\\}\\)\\([[:digit:]]\\{3\\}[\\.,:;!\\? ]\\)"
  "Regular expression matching numerals without thousands separators.")

;;;;; Image file names

(defconst tlon-figure-names
  '((:language "ar" :name "شكل")
    (:language "de" :name "abbildung")
    (:language "en" :name "figure")
    (:language "es" :name "figura")
    (:language "fr" :name "figure")
    (:language "it" :name "figura")
    (:language "ja" :name "図")
    (:language "ko" :name "그림")
    (:language "ru" :name "рисунок")
    (:language "tr" :name "şekil"))
  "List of language-specific names for figures.")

(defconst tlon-image-dirs
  '((:language "ar" :name "صور")
    (:language "de" :name "bilder")
    (:language "en" :name "images")
    (:language "es" :name "imagenes")
    (:language "fr" :name "images")
    (:language "it" :name "immagini")
    (:language "ja" :name "画像")
    (:language "ko" :name "이미지")
    (:language "ru" :name "изображения")
    (:language "tr" :name "resimler"))
  "List of language-specific names for image directories.")

;;;;; Abstracts

(defconst tlon-max-abstract-length 300
  "Maximum length of an abstract, in words.")

(defconst tlon-min-abstract-length 45
  "Minimum length of an abstract, in words.")

(defconst tlon-synopsis-length 2000
  "Maximum length of a synopsis, in words.")

;;;;; YAML

(defconst tlon-yaml-delimiter "---\n"
  "Delimiter for YAML metadata.")

;;;;; Users

(defvar tlon-users
  '((:name "Federico Stafforini"
	   :git "Federico Stafforini"
	   :github "fstafforini"
	   :nickname "Fede")
    (:name "Leonardo Picón"
	   :git "cartago"
	   :github "worldsaround"
	   :substitute "benthamite"
	   :nickname "Leo")
    (:name "Pablo Stafforini"
	   :git "Pablo Stafforini"
	   :github "benthamite"
	   :substitute "worldsaround"
	   :nickname "Pablo"))
  "Property list of users and associated properties.
The special property `:substitute' is used to determine which user should
perform a given phase of the translation process when the designated user is not
the actual user.")

;;;;; Translation

(defvar tlon-translation-language "es"
  "The current translation language.")

;;;;; Bare directory translations

(defconst tlon-core-bare-dirs
  '((("ar" . "مجلات")
     ("en" . "articles")
     ("es" . "articulos")
     ("fr" . "articles")
     ("it" . "articoli")
     ("ja" . "記事")
     ("ko" . "기사")
     ("ru" . "статьи")
     ("tr" . "makaleler"))
    (("ar" . "الأوسمة")
     ("en" . "tags")
     ("es" . "temas")
     ("fr" . "sujets")
     ("it" . "soggetti")
     ("ja" . "タグ")
     ("ko" . "태그")
     ("ru" . "теги")
     ("tr" . "etiketler"))
    (("ar" . "المؤلفون")
     ("en" . "authors")
     ("es" . "autores")
     ("fr" . "auteurs")
     ("it" . "autori")
     ("ja" . "著者")
     ("ko" . "저자")
     ("ru" . "авторы")
     ("tr" . "yazarlar"))
    (("ar" . "مجموعات")
     ("en" . "collections")
     ("es" . "colecciones")
     ("fr" . "collections")
     ("it" . "collezioni")
     ("ja" . "コレクション")
     ("ko" . "컬렉션")
     ("ru" . "коллекции")
     ("tr" . "koleksiyonlar")))
  "Alist of bare directories and associated translations.")

;;;;; Languages

(defconst tlon-project-languages
  '("arabic"
    "english"
    "french"
    "italian"
    "japanese"
    "korean"
    "polish"
    "russian"
    "spanish"
    "turkish")
  "A list of languages all languages in the Babel project.")

(defconst tlon-project-target-languages
  (remove "english" tlon-project-languages)
  "A list of languages all languages in the Babel project minus English.")

(defconst tlon-languages-properties
  '((:name "albanian" :standard "albanian" :code "sq" :locale "sq_AL")
    (:name "american" :standard "english" :code "en" :locale "en_US" :iso-639-2 "eng")
    (:name "amharic" :standard "amharic" :code "am" :locale "am_ET")
    (:name "arabic" :standard "arabic" :code "ar" :locale "ar_SA" :iso-639-2 "ara")
    (:name "argentinian" :standard "spanish" :code "es" :locale "es_AR")
    (:name "armenian" :standard "armenian" :code "hy" :locale "hy_AM")
    (:name "assamese" :standard "assamese" :code "as" :locale "as_IN")
    (:name "asturian" :standard "asturian" :code "ast" :locale "ast_ES")
    (:name "austrian" :standard "german" :code "de" :locale "de_AT")
    (:name "australian" :standard "english" :code "en" :locale "en_AU")
    (:name "basque" :standard "basque" :code "eu" :locale "eu_ES")
    (:name "belarusian" :standard "belarusian" :code "be" :locale "be_BY")
    (:name "bengali" :standard "bengali" :code "bn" :locale "bn_BD")
    (:name "bosnian" :standard "bosnian" :code "bs" :locale "bs_BA")
    (:name "breton" :standard "breton" :code "br" :locale "br_FR")
    (:name "british" :standard "english" :code "en" :locale "en_GB")
    (:name "bulgarian" :standard "bulgarian" :code "bg" :locale "bg_BG")
    (:name "burmese" :standard "burmese" :code "my" :locale "my_MM" :iso-639-2 "mya")
    (:name "canadian" :standard "english" :code "en" :locale "en_CA")
    (:name "catalan" :standard "catalan" :code "ca" :locale "ca_ES")
    (:name "chinese" :standard "chinese" :code "zh" :locale "zh_CN" :iso-639-2 "zho")
    (:name "coptic" :standard "coptic" :code "cop" :locale "cop_EG")
    (:name "croatian" :standard "croatian" :code "hr" :locale "hr_HR")
    (:name "czech" :standard "czech" :code "cs" :locale "cs_CZ")
    (:name "danish" :standard "danish" :code "da" :locale "da_DK")
    (:name "dutch" :standard "dutch" :code "nl" :locale "nl_NL")
    (:name "english" :standard "english" :code "en" :locale "en_GB" :iso-639-2 "eng")
    (:name "esperanto" :standard "esperanto" :code "eo" :locale "eo_EO")
    (:name "estonian" :standard "estonian" :code "et" :locale "et_EE")
    (:name "finnish" :standard "finnish" :code "fi" :locale "fi_FI")
    (:name "french" :standard "french" :code "fr" :locale "fr_FR" :iso-639-2 "fra")
    (:name "galician" :standard "galician" :code "gl" :locale "gl_ES")
    (:name "georgian" :standard "georgian" :code "ka" :locale "ka_GE")
    (:name "german" :standard "german" :code "de" :locale "de_DE" :iso-639-2 "deu")
    (:name "greek" :standard "greek" :code "el" :locale "el_GR" :iso-639-2 "ell")
    (:name "hebrew" :standard "hebrew" :code "he" :locale "he_IL")
    (:name "hindi" :standard "hindi" :code "hi" :locale "hi_IN")
    (:name "hungarian" :standard "hungarian" :code "hu" :locale "hu_HU")
    (:name "icelandic" :standard "icelandic" :code "is" :locale "is_IS")
    (:name "indonesian" :standard "indonesian" :code "in" :locale "id_ID" :iso-639-2 "ind")
    (:name "interlingua" :standard "interlingua" :code "ia" :locale "ia_IA")
    (:name "irish" :standard "irish" :code "ga" :locale "ga_IE")
    (:name "italian" :standard "italian" :code "it" :locale "it_IT" :iso-639-2 "ita")
    (:name "japanese" :standard "japanese" :code "ja" :locale "ja_JP" :iso-639-2 "jpn")
    (:name "kannada" :standard "kannada" :code "kn" :locale "kn_IN")
    (:name "korean" :standard "korean" :code "ko" :locale "ko_KR" :iso-639-2 "kor")
    (:name "kurdish" :standard "kurdish" :code "ku" :locale :"ku_IQ" :iso-639-2 "kur")
    (:name "lao" :standard "lao" :code "lo" :locale "lo_LA")
    (:name "latin" :standard "latin" :code "la" :locale "la_VA" :iso-639-2 "lat")
    (:name "latvian" :standard "latvian" :code "lv" :locale "lv_LV")
    (:name "lithuanian" :standard "lithuanian" :code "lt" :locale "lt_LT")
    (:name "macedonian" :standard "macedonian" :code "mk" :locale "mk_MK")
    (:name "malayalam" :standard "malayalam" :code "ml" :locale "ml_IN")
    (:name "marathi" :standard "marathi" :code "mr" :locale "mr_IN")
    (:name "mexican" :standard "spanish" :code "es" :locale "es_MX")
    (:name "mongolian" :standard "mongolian" :code "mn" :locale "mn_MN")
    (:name "naustrian" :standard "austrian" :code "de" :locale "de_AT")
    (:name "newzealand" :standard "english" :code "en" :locale "en_NZ")
    (:name "ngerman" :standard "german" :code "de" :locale "de_DE")
    (:name "nko" :standard "nko" :code "nqo" :locale "nqo_GN")
    (:name "norwegian" :standard "norwegian" :code "no" :locale "no_NO")
    (:name "oriya" :standard "oriya" :code "or" :locale "or_IN")
    (:name "persian" :standard "persian" :code "fa" :locale "fa_IR")
    (:name "polish" :standard "polish" :code "pl" :locale "pl_PL")
    (:name "portuguese" :standard "portuguese" :code "pt" :locale "pt_PT" :iso-639-2 "por")
    (:name "romanian" :standard "romanian" :code "ro" :locale "ro_RO")
    (:name "russian" :standard "russian" :code "ru" :locale "ru_RU" :iso-639-2 "rus")
    (:name "sanskrit" :standard "sanskrit" :code "sa" :locale "sa_IN")
    (:name "serbian" :standard "serbian" :code "sr" :locale "sr_RS")
    (:name "slovak" :standard "slovak" :code "sk" :locale "sk_SK")
    (:name "slovenian" :standard "slovenian" :code "sl" :locale "sl_SI")
    (:name "somali" :standard "somali" :code "so")
    (:name "spanish" :standard "spanish" :code "es" :locale "es_ES" :iso-639-2 "spa")
    (:name "swedish" :standard "swedish" :code "sv" :locale "sv_SE")
    (:name "swissgerman" :standard "german" :code "de" :locale "de_CH")
    (:name "tamil" :standard "tamil" :code "ta" :locale "ta_IN")
    (:name "telugu" :standard "telugu" :code "te" :locale "te_IN")
    (:name "thai" :standard "thai" :code "th" :locale "th_TH")
    (:name "turkish" :standard "turkish" :code "tr" :locale "tr_TR")
    (:name "turkmen" :standard "turkmen" :code "tk" :locale "tk_TM")
    (:name "ukenglish" :standard "english" :code "en" :locale "en_GB")
    (:name "ukrainian" :standard "ukrainian" :code "uk" :locale "uk_UA")
    (:name "urdu" :standard "urdu" :code "ur" :locale "ur_PK")
    (:name "vietnamese" :standard "vietnamese" :code "vi" :locale "vi_VN")
    (:name "welsh" :standard "welsh" :code "cy" :locale "cy_GB")
    (:name "afrikaans" :standard "afrikaans" :code "af" :locale "af_ZA"))
  "Plist of valid `langid' names, locales and ISO 639-1 codes.")

;;;;; Site names

(defconst tlon-site-data
  '((:language "es" :name "Altruismo Eficaz" :url "https://altruismoeficaz.net/")
    ;; TODO: add `:url' property for all sites, based on actual URL
    ;; TODO: ensure `:name' property values are accurate
    (:language "ar" :name "الإيثار الفعال")
    (:language "de" :name "Effektiver Altruismus")
    (:language "fr" :name "Altruisme Efficace")
    (:language "it" :name "Altruismo Efficace")
    (:language "ja" :name "エフェクティブ・アルトルイズム")
    (:language "ko" :name "이펙티브 알트루이즘"))
  "List of language-specific names for the uqbar site.")

;;;; Functions

(defun tlon-set-dir (repo)
  "Set the `:directory' property for REPO in `tlon-repos' if not already set."
  (unless (plist-get repo :dir)
    (let* ((dir (file-name-as-directory
		 (file-name-concat paths-dir-tlon-repos
				   (plist-get repo :name)))))
      (plist-put repo :dir dir))))

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
(defun tlon-get-repo-from-file (&optional file error)
  "Return the repo to which FILE belongs.
If FILE is nil, use the current buffer's file name. If ERROR is non-nil, signal
an error if the repo is not found. Otherwise, return nil."
  (let* ((file (or file (files-extras-buffer-file-name) default-directory)))
    (if-let ((repo (tlon-get-repo-from-dir (file-name-directory file))))
	repo
      (when error (user-error "Not in a recognized Babel repo")))))

(declare-function files-extras-get-nth-directory "files-extras")
(defun tlon-get-repo-from-dir (dir)
  "Return the repo to which DIR belongs.
This function correctly handles cases in which the `.git' directory of a repo is
outside that repo’s directory."
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

;;;###autoload
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

;;;###autoload
(defun tlon-lookup-all (list key &rest pairs)
  "Return all unique values of KEY in LIST matching all PAIRS.
PAIRS is expected to be an even-sized list of <key value> tuples."
  (apply #'tlon-lookup-all-builder list key nil pairs))

(defun tlon-lookup-all-case-insensitive (list key &rest pairs)
  "Return all unique values of KEY in LIST matching all PAIRS, regardless of case.
PAIRS is expected to be an even-sized list of <key value> tuples."
  (apply #'tlon-lookup-all-builder list key t pairs))

;;;;;; Metadata lookup

;;;###autoload
(defun tlon-metadata-lookup (metadata key &rest key-value)
  "Return the value of KEY in METADATA matching all KEY-VALUE pairs."
  (apply #'tlon-lookup metadata key key-value))

;;;###autoload
(defun tlon-metadata-lookup-all (metadata key &rest key-value)
  "Return all unique values of KEY in METADATA matching alll KEY-VALUE pairs."
  (apply #'tlon-lookup-all metadata key key-value))

;;;;;; Repo lookup

;;;###autoload
(defun tlon-repo-lookup (key &rest key-value)
  "Return the value of KEY in repos matching all KEY-VALUE pairs."
  (apply #'tlon-lookup tlon-repos key key-value))

;;;###autoload
(defun tlon-repo-lookup-all (key &rest key-value)
  "Return all unique values of KEY in repos matching all KEY-VALUE pairs."
  (apply #'tlon-lookup-all tlon-repos key key-value))

;;;;;; User lookup

;;;###autoload
(defun tlon-user-lookup (key &rest key-value)
  "Return the value of KEY in users matching all KEY-VALUE pairs."
  (apply #'tlon-lookup tlon-users key key-value))

;;;###autoload
(defun tlon-user-lookup-all (key &rest key-value)
  "Return all unique values of KEY in users matching all KEY-VALUE pairs."
  (apply #'tlon-lookup-all tlon-users key key-value))

;;;;;; Label lookup

(defvar tlon-job-labels)
;;;###autoload
(defun tlon-label-lookup (key &rest key-value)
  "Return the value of KEY in labels matching all KEY-VALUE pairs."
  (apply #'tlon-lookup tlon-job-labels key key-value))

;;;###autoload
(defun tlon-label-lookup-all (key &rest key-value)
  "Return all values of KEY in labels matching all KEY-VALUE pairs."
  (apply #'tlon-lookup-all tlon-job-labels key key-value))

(declare-function forge-get-repository "forge-core")
(declare-function forge-get-issue "forge-core")
(autoload 'forge-sql "forge-db")
;;;###autoload
(defun tlon-issue-lookup (string &optional dir)
  "Return the first issue in DIR whose title includes STRING.
If DIR is nil, use the current repository."
  (when-let* ((string (concat "%" string "%"))
	      (default-directory (or dir default-directory))
	      (repo (forge-get-repository :tracked))
	      (repo-id (eieio-oref repo 'id))
	      (issue-number (caar (forge-sql [:select [number] :from issue
						      :where (and (= repository $s1)
								  (like title $s2))]
					     repo-id
					     string))))
    (forge-get-issue repo issue-number)))

;;;;; Get region pos

(defun tlon-get-delimited-region-pos (begin &optional end exclude-delims)
  "Get the position of the region delimited by BEGIN and END.
If END is nil, use BEGIN also as the end delimiter. If EXCLUDE-DELIMS is
non-nil, exclude the delimiters when returning the region position."
  (let ((begin-fun (if exclude-delims 'match-end 'match-beginning))
	(end-fun (if exclude-delims 'match-beginning 'match-end)))
    (save-restriction
      (widen)
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward begin nil t)
	  (let* ((begin-pos (funcall begin-fun 0))
		 (end-pos (when (re-search-forward (or end begin) nil t)
			    (funcall end-fun 0))))
	    (when (and begin-pos end-pos)
	      (cons begin-pos end-pos))))))))

;;;;; Bare dirs

(defun tlon-get-bare-dir (&optional file in-english)
  "Get the bare directory of FILE.
A file’s bare directory is its directory minus its repository. For example, the
bare directory of `~/Dropbox/repos/uqbar-es/autores/' is `autores'.

If FILE is nil, return the counterpart repo of the file visited by the current
buffer. If IN-ENGLISH is non-nil, return the English translation of the bare
directory."
  (when-let* ((file (or file (buffer-file-name)))
	      (repo (tlon-get-repo-from-file file)))
    (when-let* ((relative-path (file-relative-name file repo))
		(dir-name (file-name-directory relative-path))
		(bare-dir (directory-file-name dir-name)))
      (if in-english
	  (tlon-get-bare-dir-translation "en" (tlon-get-language-in-file) bare-dir)
	bare-dir))))

(defun tlon-get-bare-dir-translation (target-lang source-lang bare-dir)
  "For BARE-DIR in SOURCE-LANG, get its translation into TARGET-LANG."
  (let (result)
    (dolist (outer tlon-core-bare-dirs result)
      (dolist (inner outer)
	(when (and (equal (cdr inner) bare-dir)
		   (equal (car inner) source-lang))
	  (setq result (cdr (assoc target-lang outer))))))))

(defun tlon-select-bare-dir (lang)
  "Set the bare dir in LANG."
  (let* ((bare-dirs (tlon-lookup-all tlon-core-bare-dirs lang)))
    (completing-read "Type: " bare-dirs)))

;;;;; Misc

(defun tlon-concatenate-list (list)
  "Concatenate LIST into a string with commas and a conjunct, as appropriate."
  (if (cdr list)
      (let ((all-but-last (mapconcat #'identity (butlast list) ", "))
	    (last (car (last list)))
	    (conjunct (tlon-lookup tlon-tts-conjuncts :conjunct :language (tlon-get-language-in-file))))
	(format "%s %s %s" all-but-last conjunct last))
    (car list)))

;;;;; language

;; TODO: we also have `tlon-translation-language'; think about what to do
(defun tlon-get-language-in-file (&optional file error)
  "Get the two-letter ISO 639-1 language code in FILE.
If FILE is nil, use the current buffer's file name. If ERROR is non-nil, signal
an error if the language is not found. Otherwise, return nil."
  (when-let* ((file (or file (buffer-file-name)))
	      (repo (tlon-get-repo-from-file file error)))
    (tlon-repo-lookup :language :dir repo)))

(declare-function ebib-extras-get-field "ebib-extras")
(declare-function bibtex-extras-get-field "bibtex-extras")
(defun tlon-get-language-in-mode ()
  "Return the language in the current major mode.
The language is returned in standard format."
  (pcase major-mode
    ('ebib-entry-mode (ebib-extras-get-field "langid"))
    ('bibtex-mode (bibtex-extras-get-field "langid"))
    ('markdown-mode (let ((code (tlon-get-language-in-file (buffer-file-name))))
		      (tlon-lookup tlon-languages-properties :standard :code code)))))

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

;;;###autoload
(defun tlon-select-language (&optional format babel prompt require-match initial-input
				       additional-langs excluded-langs multiple)
  "Prompt the user to select a LANGUAGE and return it in FORMAT.
If FORMAT is `code', return the two-letter code of the language (e.g. \"es\"),
if found. If it is `locale', return the predefined locale for that
language (e.g. \"en-US\"), if found. Otherwise, return the original
selection (e.g. \"english\").

By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project.

PROMPT, REQUIRE-MATCH and INITIAL-INPUT behave as those arguments do in
`completing-read'; check its docstring for details. If PROMPT is nil, use
\"Language: \".

ADDITIONAL-LANGS and EXCLUDED-LANGS are lists of languages to add to or to
exclude from the default list of languages, respectively.

If MULTIPLE is non-nil, allow the user to select multiple languages. In that
case, the return value will be a list of strings rather than a string."
  (let* ((selection (if multiple
			(tlon-read-multiple-languages babel)
		      (tlon-read-language
		       babel prompt require-match initial-input additional-langs excluded-langs))))
    (pcase format
      ((or 'code 'locale) (or (tlon-get-formatted-languages selection format) selection))
      (_ selection))))

(defun tlon-get-formatted-languages (selection format)
  "Return the language SELECTION in appropriate FORMAT.
SELECTION is either a string or a list of strings representing languages in
English. FORMAT must be either `code' or `locale'."
  (let* ((property (pcase format ('locale :locale) ('code :code)))
	 (fun (lambda (language)
		(if (equal language "default")
		    "default" ; Return "default" directly if selected
		  (tlon-lookup tlon-languages-properties property :name language)))))
    (if (listp selection)
	(mapcar (lambda (language)
		  (funcall fun language))
		selection)
      (funcall fun selection))))

;;;###autoload
(defun tlon-read-language (&optional babel prompt require-match initial-input additional-langs excluded-langs)
  "Read a language from a list of languages.
By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project. Prompt user using PROMPT; if nil,
use \"Language: \". REQUIRE-MATCH and INITIAL-INPUT behave as those arguments do
in `completing-read'; check its docstring for details. ADDITIONAL-LANGS and
EXCLUDED-LANGS are lists of languages to add to or to exclude from the default
list of languages, respectively."
  (let ((prompt (or prompt "Language: "))
	(language-candidates (tlon-get-language-candidates babel additional-langs excluded-langs)))
    (completing-read prompt language-candidates nil require-match initial-input)))

(defun tlon-read-multiple-languages (&optional babel prompt)
  "Read a list of languages from a list of languages.
By default, offer all valid BibTeX languages; if BABEL is non-nil, restrict the
candidates to languages in the Babel project. Prompt user using PROMPT; if nil,
use \"Languages (comma-separated, or ‘default’): \"."
  (let* ((language-candidates (tlon-get-language-candidates babel))
	 (prompt (or prompt "Languages (comma-separated, or 'default'): ")))
    (completing-read-multiple prompt (append '("default") language-candidates))))

(defun tlon-get-language-candidates (babel &optional additional-langs excluded-langs)
  "Return a list of language candidates.
If BABEL is nil, return all valid BibTeX languages; otherwise, return candidates
languages in the Babel project only. ADDITIONAL-LANGS and EXCLUDED-LANGS are
lists of languages to add to or to exclude from the default list of languages,
respectively."
  (let ((base-langs (if babel
			tlon-project-languages
		      (tlon-lookup-all tlon-languages-properties :name))))
    (cl-remove-if (lambda (lang)
		    (member lang excluded-langs))
		  (append base-langs additional-langs))))

;;;;; json

(defun tlon-read-json (&optional file object-type array-type key-type)
  "Read JSON substring or FILE using array TYPE.
OBJECT-TYPE must be one of `alist' (default), `plist' or `hash-table'.
ARRAY-TYPE must be one of `list' (default) or `vector'. KEY-TYPE must be one of
`string' (default), `symbol' or `keyword'."
  (condition-case err
      (let ((json-object-type (or object-type 'alist))
	    (json-array-type (or array-type 'list))
	    (json-key-type (or key-type 'string))
	    (json-false :json-false))
	(if file
	    (json-read-file file)
	  ;; Try to find and parse just the JSON part
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward "{" nil t)
	      (goto-char (match-beginning 0))
	      (json-read)))))
    (error
     (message "JSON parsing error in %s: %s"
	      (or file "buffer") err)
     nil)))

(defun tlon-write-data (file data)
  "Write DATA to a JSON FILE."
  (with-temp-file file
    (insert (json-encode data))
    (json-pretty-print-buffer)))

(defun tlon-get-keys (data)
  "Get keys from hash table JSON DATA."
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) data)
    keys))

;;;;; JSON editing

(defun tlon-edit-json-mapping (file outer-key-prompt inner-value-prompt)
  "Edit a JSON file structured as {outer_key: {lang_code_or_default: inner_value}}.
Prompts for OUTER-KEY, one or more LANG-CODEs (or \"default\"),
and INNER-VALUE. Updates FILE accordingly.
OUTER-KEY-PROMPT is the prompt string for the outer key.
INNER-VALUE-PROMPT is the prompt string for the inner value."
  (let* ((json-data (or (tlon-read-json file 'hash-table)
			(make-hash-table :test 'equal)))
	 (outer-keys (tlon-get-keys json-data))
	 (chosen-outer-key (completing-read outer-key-prompt outer-keys nil nil nil nil (car outer-keys)))
	 (inner-alist (gethash chosen-outer-key json-data (make-hash-table :test 'equal)))
	 ;; Prompt for multiple languages or "default"
	 (chosen-langs (tlon-select-language 'code 'babel "Language(s) (or 'default'): "
					     'require-match nil '("default") nil 'multiple))
	 ;; Prioritize non-default language for existing value prompt
	 (prompt-lang (or (cl-find-if-not (lambda (l) (equal l "default")) chosen-langs)
			  (car chosen-langs)))
	 (existing-value (when prompt-lang
			   (gethash prompt-lang inner-alist)))
	 (chosen-inner-value (read-string (format "%s for '%s' in %s: "
						  inner-value-prompt
						  chosen-outer-key
						  (string-join chosen-langs ", "))
					  existing-value)))
    ;; Update the inner hash-table for all selected languages/keys
    (dolist (lang chosen-langs)
      (puthash lang chosen-inner-value inner-alist))
    ;; Update the main data structure
    (puthash chosen-outer-key inner-alist json-data)
    ;; Write back to the file
    (tlon-write-data file json-data)
    (message "Updated '%s' for key '%s' in language(s)/key(s) %s in %s"
	     chosen-inner-value chosen-outer-key (string-join chosen-langs ", ") file)))

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
(declare-function org-id-find-id-file "org-id")
(defun tlon-get-file-with-org-id (id)
  "Return the file containing the heading with the given `org-mode' ID."
  (if-let ((location (org-roam-id-find id)))
      (car location)
    (file-truename (org-id-find-id-file id))))

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
  (set var-name (if (symbol-value var-name)
		    nil
		  t)))

;;;;; numbers

(defun tlon-get-separator (type &optional language)
  "Return the separator for LANGUAGE.
TYPE is either `thousands' or `decimal'. If LANGUAGE is nil, use the language of
the current repository."
  (when-let* ((language (or language (tlon-get-language-in-file)))
	      (separator-key (pcase type ('thousands :thousands) ('decimal :decimal)))
	      (separator (tlon-lookup tlon-language-separators separator-key :language language)))
    separator))

(defun tlon-get-decimal-separator (&optional language)
  "Return the decimal separator for LANGUAGE.
If LANGUAGE is nil, use the language of the current repository."
  (tlon-get-separator 'decimal language))

(defun tlon-get-thousands-separator (&optional language)
  "Return the thousands separator for LANGUAGE.
If LANGUAGE is nil, use the language of the current repository."
  (tlon-get-separator 'thousands language))

(defun tlon-get-number-separator-pattern (&optional lang thousands decimal bounded)
  "Return pattern matching a number with THOUSANDS and DECIMAL separators in LANG.
If LANG is nil, obtain it from the file visited by the current buffer. If
THOUSANDS or DECIMAL are nil, infer them from the language of the current
repository. If BOUNDED is non-nil, match only text at the beginning or end of a
word."
  (when-let* ((thousands (or thousands (tlon-get-thousands-separator lang)))
	      (decimal (or decimal (tlon-get-decimal-separator lang)))
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
  "Perform a ripgrep search for STRING in files with EXTENSION."
  (interactive (list (read-string "Search string: ")
		     (completing-read "Extension: " '("" "el" "md" "org") nil t)))
  (let* ((core-settings '(:interactive t :recursive t))
	 (no-context-settings '(:c-beg-only t :c-end-only t))
	 (context (y-or-n-p "Context?"))
	 (context-lines (when context (read-number "Context lines: " 0)))
	 (settings (append core-settings
			   (unless context no-context-settings)
			   (when context (list :c-end context-lines))))
	 (extension (unless (string-empty-p extension) (format "\\.%s" extension))))
    (apply #'elgrep nil extension string settings)))

;;;;; checks

(autoload 'magit-get-current-branch "magit-git")
(defun tlon-check-branch (branch repo)
  "Throw an error unless current buffer is in REPO branch BRANCH."
  (let ((default-directory repo))
    (unless (string= (magit-get-current-branch) branch)
      (user-error "Please switch to the branch `%s' before proceeding" branch)
      t)))

(declare-function tlon-get-clock-key "tlon-clock")
(declare-function tlon-metadata-in-repo "tlon-yaml")
(defun tlon-check-file (&optional original)
  "Throw an error unless current file matches file in clock.
If ORIGINAL is non-nil, check that current file matches original; otherwise,
check that current file matches translation."
  (let* ((key (tlon-get-clock-key))
	 (field (if original "original_path" "file"))
	 (expected-file (file-name-nondirectory
			 (tlon-metadata-lookup (tlon-metadata-in-repo) field "original_key" key)))
	 (actual-file (file-name-nondirectory
		       (buffer-file-name))))
    (if (string= expected-file actual-file)
	t
      (user-error "Current file does not match file in clock"))))

;; TODO: the two functions below appear to check for the same thing, except that
;; one ignores changes to the file itself. They should be merged into a single
;; function.
(defun tlon-check-staged-or-unstaged (file)
  "Check if there are staged or unstaged modifications in repo involving FILE."
  (catch 'found
    (dolist (flag '("staged" ""))
      (let ((git-command (format "git diff --%s --name-only %s" flag file)))
	(when (not (string-empty-p (shell-command-to-string git-command)))
	  (throw 'found t))))))

(autoload 'magit-git-str "magit-git")
(defun tlon-ensure-no-uncommitted-changes (file)
  "Throw an error if there are uncommitted modifications in repo of FILE.
FILE is excluded from the check."
  (let* ((default-directory (tlon-get-repo-from-file file))
	 (all-changes (magit-git-str "diff" "HEAD" "--" "."))
	 (filtered-changes (magit-git-str "diff" "HEAD" "--" file)))
    (unless (string= all-changes filtered-changes)
      (let ((repo-name (tlon-repo-lookup :name :dir (tlon-get-repo-from-file file))))
	(user-error "There are uncommitted changes in repo `%s'" repo-name)))))

(declare-function tlon-yaml-get-key "tlon-yaml")
(declare-function simple-extras-slugify "simple-extras")
(defun tlon-check-file-title-match  (&optional file)
  "Check that FILE matches its title.
If FILE is nil, check the current buffer.

NOTE: This function is no longer run automatically, since its functionality is
now performed by the backend, which provides more comprehensive and robust
logging."
  (when-let* ((file (or file (buffer-file-name)))
	      (base (file-name-base file))
	      (title (tlon-yaml-get-key "title" file))
	      (slugified-title (simple-extras-slugify title)))
    (unless (or
	     (string= base slugified-title)
	     ;; for articles with duplicate titles
	     (string-match-p (concat "^" (regexp-quote slugified-title) "-[0-9]+$") base))
      (error "The file `%s' does not match its title" title))))

(defun tlon-check-file-type-match (&optional file)
  "Check that FILE matches its tile.
Return an error if the file is not in a subdirectory of the repository whose
name, or its translation, is the value of the file’s `type' metadata field. For
example, if the FILE is of type `article', the function will throw an error if
FILE is located in `uqbar-es/temas/FILE' or `uqbar-es/imagenes/articulos/FILE',
but will not throw an error if it is located in `uqbar-en/articles/FILE' or
`uqbar-es/articulos/FILE'."
  (let* ((file (or file (buffer-file-name)))
	 (repo (tlon-get-repo-from-file file))
	 (lang (tlon-repo-lookup :language :dir repo))
	 (dir-raw (file-name-directory (file-relative-name file repo)))
	 (dir-lang (tlon-get-bare-dir-translation
		    "en" lang (directory-file-name dir-raw)))
	 (type (tlon-yaml-get-key "type" file)))
    (unless (string-match type dir-lang) ; we use `string-match' instead of `string=' to handle plurals
      (user-error "The file `%s' does not match its type" file))))

(defvar tlon-md-image-sans-alt)
(defun tlon-check-image-alt-text ()
  "Check if all images have alt text, else signal an error."
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward tlon-md-image-sans-alt nil t)
	(setq pos (match-beginning 0))))
    (when pos
      (goto-char pos)
      (user-error "There are images without alt text. Use `tlon-md-insert-alt-text' to fix them"))))

;;;;; visit file at point

(defun tlon-visit-file-at-point ()
  "Visit the file referenced on the current line and jump to its position."
  (interactive)
  (let* ((line (buffer-substring-no-properties (line-beginning-position)
					       (line-end-position))))
    (if (and line (string-match "^\\([^:\n]+\\): .* position \\([0-9]+\\)\\b" line))
	(let ((file (match-string 1 line))
	      (pos (string-to-number (match-string 2 line))))
	  (find-file file)
	  (goto-char (min pos (point-max)))
	  (recenter))
      (user-error "No navigable entry on this line"))))

(defun tlon--setup-visit-file-at-point-buffer ()
  "Prepare the *tlon-unbalanced* buffer and bind RET to navigate to entries."
  (special-mode)
  (use-local-map
   (let ((map (make-sparse-keymap)))
     (set-keymap-parent map special-mode-map)
     (define-key map (kbd "RET") #'tlon-visit-file-at-point)
     map)))

(provide 'tlon-core)
;;; tlon-core.el ends here
