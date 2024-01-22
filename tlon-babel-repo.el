;;; tlon-babel-repo.el --- Set main repo properties -*- lexical-binding: t -*-

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

;; Set main repo properties.

;;; Code:

;;;; Variables

(defconst tlon-babel-repo-props
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
    (:name "utilitarismo-en"
	   :project "babel"
	   :subproject "utilitarismo"
	   :abbrev "utilitarismo-en"
	   :type content
	   :subtype originals
	   :key "u n")
    (:name "utilitarismo-es"
	   :project "babel"
	   :subproject "utilitarismo"
	   :abbrev "utilitarismo-es"
	   :type content
	   :subtype translations
	   :key "u s"
	   :url "https://utilitarismo.net/")
    (:name "ensayos-en"
	   :project "babel"
	   :subproject "ensayos"
	   :abbrev "ensayos-en"
	   :type content
	   :subtype originals
	   :key "l n")
    (:name "ensayos-es"
	   :project "babel"
	   :subproject "ensayos"
	   :abbrev "ensayos-es"
	   :type content
	   :subtype translations
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
    (:name "bisagra"
	   :project "other"
	   :subproject "bisagra"
	   :abbrev "bisagra"
	   :type misc
	   :key "b")
    (:name "tlon-docs"
	   :project "other"
	   :subproject "docs"
	   :abbrev "docs"
	   :type docs
	   :key "d"))
  "List of repos and associated properties.
The `:name' property is the full name of the repo, as it appears in the URL. The
`:abbrev' property is an abbreviated form of the name, used, for example, for
creating `org-mode' TODOs.")

;;;; Functions

(defun tlon-babel-repo-set-dir (repo)
  "Set the `:directory' property for REPO in `tlon-babel-repo-props'."
  (let* ((dir (file-name-as-directory
	       (file-name-concat tlon-core-repo-dirs
				 (plist-get repo :name)))))
    (plist-put repo :dir dir)))

(mapc #'tlon-babel-repo-set-dir tlon-babel-repo-props)

(provide 'tlon-babel-repo)
;;; tlon-babel-repo.el ends here

