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

(defcustom tlon-babel-core-repo-dir
  (file-name-concat paths-dir-dropbox "repos/")
  "Directory where the Tlön repositories are stored."
  :type 'directory
  :group 'tlon-babel)

(defconst tlon-babel-core-repos
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

;;;;; To sort

(defconst tlon-babel-core-labels
  '((:label "Awaiting processing"
	    :action "Process"
	    :assignee "worldsaround")
    (:label "Awaiting translation"
	    :action "Translate"
	    :assignee "")
    (:label "Awaiting revision"
	    :action "Revise"
	    :assignee "worldsaround")
    (:label "Awaiting check"
	    :action "Check"
	    :assignee "worldsaround")
    (:label "Awaiting review"
	    :action "Review"
	    :assignee "benthamite")
    (:label "Published"
	    :action "Publish"
	    :assignee ""))
  "List of labels and associated properties.")

(defvar tlon-babel-core-users
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

;;;; Functions

(defun tlon-babel-core-set-dir (repo)
  "Set the `:directory' property for REPO in `tlon-babel-core-repos'."
  (let* ((dir (file-name-as-directory
	       (file-name-concat tlon-babel-core-repo-dir
				 (plist-get repo :name)))))
    (plist-put repo :dir dir)))

(mapc #'tlon-babel-core-set-dir tlon-babel-core-repos)

;;;;; Extra funs to sort

(defun tlon-babel-core-plist-lookup (list prop &rest props-values)
  "Return the value of PROP in LIST matching one or more PROPS-VALUES pairs.
If multiple matches are found, return the first match."
  (cl-loop for plist in list
	   when (cl-loop for (prop value) on props-values by #'cddr
			 always (equal (plist-get plist prop) value))
	   return (plist-get plist prop)))

(defun tlon-babel-core-repo-lookup (prop &rest props-values)
  "Return the value of PROP in repos matching one or more PROPS-VALUES pairs."
  (apply #'tlon-babel-core-plist-lookup tlon-babel-core-repos prop props-values))

(defun tlon-babel-core-user-lookup (prop &rest props-values)
  "Return the value of PROP in users matching one or more PROPS-VALUES pairs."
  (apply #'tlon-babel-core-plist-lookup tlon-babel-core-users prop props-values))

(defun tlon-babel-core-label-lookup (prop &rest props-values)
  "Return the value of PROP in labels matching one or more PROPS-VALUES pairs.."
  (apply #'tlon-babel-core-plist-lookup tlon-babel-core-labels prop props-values))

(defun tlon-babel-get-property-core-of-repo (prop repo)
  "Return the value of PROP in REPO."
  (tlon-babel-core-plist-lookup tlon-babel-core-repos prop :dir repo))

(defun tlon-babel-get-property-of-core-repo-name (prop repo-name)
  "Return the value of PROP in REPO-NAME.
REPO-NAME is named in its abbreviated form, i.e. the value of `:abbrev' rather
than `:name'."
  (tlon-babel-core-plist-lookup tlon-babel-core-repos prop :abbrev repo-name))

(defun tlon-babel-get-property-core-of-user (prop user)
  "Return the value of PROP in USER."
  (tlon-babel-core-plist-lookup tlon-babel-core-users prop :name user))

(defun tlon-babel-get-property-core-of-label (prop user)
  "Return the value of PROP in USER."
  (tlon-babel-core-plist-lookup prop tlon-babel-core-users :name user))

(defun tlon-babel-get-property-core-of-plists (prop plist &optional target-prop target-value)
  "Return a list of all PROP values in PLIST.
Optionally, return only the subset of values such that TARGET-PROP matches
TARGET-VALUE."
  (let ((result '()))
    (dolist (plist plist)
      (let* ((value1 (plist-get plist prop #'string=))
	     (target-value-test (when target-prop (plist-get plist target-prop #'string=))))
	(when value1
	  (if target-prop
	      (when (string= target-value target-value-test)
		(setq result (append result (list value1))))
	    (setq result (append result (list value1)))))))
    result))

(defun tlon-babel-get-property-core-of-repos (prop &optional target-prop target-value)
  "Return a list of all PROP values in `tlon-babel-core-repos'.
Optionally, return only the subset of values such that TARGET-PROP matches
TARGET-VALUE."
  (tlon-babel-get-property-core-of-plists prop tlon-babel-core-repos target-prop target-value))

(defun tlon-babel-get-property-core-of-users (prop &optional target-prop target-value)
  "Return a list of all PROP values in PLIST `tlon-babel-core-users'.
Optionally, return only the subset of values such that TARGET-PROP matches
TARGET-VALUE."
  (tlon-babel-get-property-core-of-plists prop tlon-babel-core-users target-prop target-value))

(defun tlon-babel-get-property-core-of-labels (prop &optional target-prop target-value)
  "Return a list of all PROP values in PLIST `tlon-babel-core-labels'.
Optionally, return only the subset of values such that TARGET-PROP matches
TARGET-VALUE."
  (tlon-babel-get-property-core-of-plists prop tlon-babel-core-labels target-prop target-value))

(provide 'tlon-babel-core)
;;; tlon-babel-core.el ends here
