;;; tlon-init.el --- Convenience functions to manage Tlön's Emacs config -*- lexical-binding: t -*-

;; Author: Federico Stafforini & Pablo Stafforini
;; Version: 1.0.0
;; Homepage: https://github.com/tlon-team/tlon-init
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

;; Convenience functions to manage Tlön's Emacs config

;;; Code:

(require 'cus-edit)
(require 'ob-tangle)
(require 'paths)

;;;; User options

(defgroup tlon-init ()
  "Convenience functions to manage Tlön's Emacs config."
  :group 'emacs)

(defcustom tlon-init-post-init-hook nil
  "Hook run at the end of the user’s config file.
If the user is Pablo, it is run at the end of `config.org'. Otherwise, it is run
at the end the user’s personal config file, e.g. `config-leonardo.org' if the
user is Leo.

The advantage of this hook over `elpaca-after-init-hook' is that the latter will
always load at the end of `config.org', even when the user is not Pablo."
  :type 'hook
  :group 'tlon-init)

(defcustom tlon-init-post-build-hook nil
  "Hook run after building a profile."
  :type 'hook
  :group 'tlon-init)

(defcustom tlon-init-post-deploy-hook nil
  "Hook run after deploying a profile."
  :type 'hook
  :group 'tlon-init)

;;;; Variables

(defvar chemacs-profiles)
(defvar chemacs-profile-name)
(defvar chemacs-profiles-path)

;;;;; Files

(defvar tlon-init-file-early-init
  (file-name-concat user-emacs-directory "early-init.el")
  "Path to `early-init.el'.")

(defvar tlon-init-file-user-init
  (file-name-concat user-emacs-directory "init.el")
  "Path to `init.el'.")

(defvar tlon-init-file-late-init
  (file-name-concat user-emacs-directory "late-init.el")
  "Path to `late-init.el'.")

(defvar tlon-init-file-paths-override
  (file-name-concat user-emacs-directory "paths-override.el")
  "Path to `paths-override.el'.")

(defvar tlon-init-file-code-override
  (file-name-concat user-emacs-directory "code-override.el")
  "Path to `code-override.el'.")

(defvar tlon-init-file-excluded-packages
  (file-name-concat user-emacs-directory "excluded-packages.el")
  "Path to `excluded-packages.el'.")

;;;;; Other

(defvar tlon-init-excluded-packages '()
  "List of packages to be excluded in the tangle process.")

(defvar tlon-init-code-overrides '()
  "Alist of code overrides for each package.")

(defvar tlon-init-boot-as-if-not-pablo nil
  "If non-nil, boot as if the current machine is not Pablo’s.
This variable allows Pablo to test other people’s configs from his own computer.
It should be set in `init.el'.")

(defvar tlon-init-chemacs-profile-name ""
  "The name of the active Chemacs profile.")

;;;; Functions

;;;;; Profile names

(defun tlon-init-chemacs-actual-profile-name ()
  "Return the actual name of the active Chemacs profile, handling defaults.
If the profile is the default one, return its name, not \"default\"."
  (if (string= chemacs-profile-name "default")
      (tlon-init-get-default-profile-name)
    chemacs-profile-name))

(defun tlon-init-get-default-profile-name ()
  "Find the name of the default Chemacs profile."
  (let ((default-dir (cdar (alist-get "default" chemacs-profiles nil nil #'string=))))
    ;; Iterate over the profiles to find a match with default-dir (excluding "default").
    (cl-loop for (name . attrs) in (remove (assoc "default" chemacs-profiles) chemacs-profiles)
             if (equal (cdr (assoc 'user-emacs-directory attrs)) default-dir)
             return name)))

(setq tlon-init-chemacs-profile-name (tlon-init-chemacs-actual-profile-name))

;;;;; Functions used in code blocks

(declare-function org-get-heading "org")
;; TODO: consider removing the first argument
(defun tlon-init-tangle-conditionally (&optional package tangle-to-early-init bisect)
  "Tangle PACKAGE unless listed in `tlon-init-excluded-packages'.
By default, tangle to `init.el'. If TANGLE-TO-EARLY-INIT is non-nil, tangle to
`early-init.el' instead.

BISECT is a reserved argument for a functionality that has not yet been
developed."
  (let ((package (or package (intern (org-get-heading t t t t)))))
    (if bisect
	(tlon-init-process-for-bisection package)
      (tlon-init-get-tangle-target package tangle-to-early-init))))

(defun tlon-init-get-tangle-target (package early-init)
  "Return the file to which code block for PACKAGE should be tangled.
If EARLY-INIT is non-nil, return the early init file; else, return the main init
file."
  (if (member package tlon-init-excluded-packages)
      "no"
    (if early-init
	tlon-init-file-early-init
      tlon-init-file-user-init)))

(defun tlon-init-override-code (key code-block)
  "Return CODE-BLOCK of KEY in `tlon-init-code-overrides'.
When KEY is not present in `tlon-init-code-overrides', return the default,
non-overridden code. The variable `tlon-init-code-overrides' is populated during
the init process.

The syntax for the KEY parameter is `:{package-name}' where `{package-name}' is
the name of the package. Examples: `:general', `:embark', `:hydra'.

Example usage:

\(tlon-init-override-code
 :embark
 \='(
  (use-package embark
   ;; Default, non-overridden code goes here,
   ;; in this case, the full use-package call
   )))

If `:embark' is found within `tlon-init-code-overrides' in this example, the
default will be overridden by that code."
  (with-temp-buffer
    (dolist (row (alist-get key tlon-init-code-overrides code-block))
      (insert (prin1-to-string row)))
    (eval-buffer)))

;;;;;

(defun tlon-init-read-file (fname)
  "Read FNAME and return its contents."
  (when fname
    (with-temp-buffer
      (insert-file-contents fname)
      (goto-char (point-min))
      (condition-case err
	  (read (current-buffer))
	(error
	 (error "Failed to parse %s: %s" fname (error-message-string err)))))))

(defun tlon-init-available-init-dirs (&optional include-default)
  "Return Alist of Chemacs profiles and associated init locations.
If INCLUDE-DEFAULT is non-nil, include the ‘default’ profile."
  ;; update `chemacs-profiles' in case a new profile was added
  ;; this is just the `defvar' of `chemacs-profiles' copied from chemacs.el
  (setq chemacs-profiles
	(with-temp-buffer
	  (insert-file-contents chemacs-profiles-path)
	  (goto-char (point-min))
	  (condition-case err
	      (read (current-buffer))
	    (error
	     (error "Failed to parse %s: %s" chemacs-profiles-path (error-message-string err))))))
  ;; now return an alist of profile names and their associated init file locations
  (let (target-directories)
    (dolist (chemacs-profile chemacs-profiles target-directories)
      (when (or include-default (not (string= (car chemacs-profile) "default")))
	(push
	 (cons
	  (car chemacs-profile)
	  (cdadr chemacs-profile))
	 target-directories)))))

(defun tlon-init-machine-pablo-p ()
  "Return t if Pablo's machine is the current machine, and nil otherwise.
Set `tlon-init-boot-as-if-not-pablo' to t in `init.el' to test from Pablo's
machine"
  (and (string= (system-name) "Pablos-MacBook-Pro.local")
       (not tlon-init-boot-as-if-not-pablo)))

(defun tlon-init-load-excluded-packages-file (init-dir)
  "Load the excluded packages list for INIT-DIR."
  (unless (tlon-init-machine-pablo-p)
    (if (file-regular-p tlon-init-file-excluded-packages)
	(load-file tlon-init-file-excluded-packages)
      (user-error "`excluded-packages.el' not present in init directory `%s'" init-dir))
    (message "tlon-init: Loaded excluded packages for Chemacs profile `%s'." tlon-init-chemacs-profile-name)))

(defun tlon-init-build (init-dir)
  "Build or rebuild a profile in INIT-DIR."
  (interactive (list (tlon-init-profile-dir
		      (completing-read
		       "Select Chemacs profile to build: "
		       (tlon-init-available-init-dirs)
		       nil t))))
  (unless (string-equal major-mode "org-mode")
    (user-error "Error: cannot build init from a buffer that is not visiting an `org-mode' file"))
  (tlon-init-set-babel-paths init-dir)
  ;; conditionally tangle extra config file, pass 1: get excluded packages only
  (tlon-init-tangle-extra-config-file)
  (tlon-init-load-excluded-packages-file init-dir)
  ;; tangle `config.org'
  (tlon-init-tangle)
  ;; conditionally tangle extra config file, pass 2: get the rest of extra config
  (unless (tlon-init-machine-pablo-p)
    (tlon-init-tangle-extra-config-file))
  (run-hooks 'tlon-init-post-build-hook))

;;;;; org-babel

(defun tlon-init-set-babel-paths (init-dir)
  "Set the paths for the `org-babel' code blocks relative to INIT-DIR."
  (setq tlon-init-file-paths-override (file-name-concat init-dir "paths-override.el")
	tlon-init-file-code-override (file-name-concat init-dir "code-override.el")
	tlon-init-file-excluded-packages (file-name-concat init-dir "excluded-packages.el")
	tlon-init-file-early-init (file-name-concat init-dir "early-init.el")
	tlon-init-file-user-init (file-name-concat init-dir "init.el")
	tlon-init-file-late-init (file-name-concat init-dir "late-init.el")))

(defun tlon-init-tangle ()
  "Tangle the current buffer."
  (widen)
  (save-buffer)
  (org-babel-tangle)
  (message "tlon-init: Tangled init files to Chemacs profile `%s'." tlon-init-file-user-init))

(defun tlon-init-tangle-extra-config-file ()
  "Tangle extra config file.
For Pablo, the extra config file is the same as the main config
file (`paths-file-config'). For all other users, the extra config file is the
file with the name `config-{user-first-name}.org'."
  (let* ((user-first-name (downcase (car (split-string user-full-name))))
	 (extra-config-file (if (tlon-init-machine-pablo-p)
				paths-file-config
			      (file-name-concat paths-dir-dotemacs
						(concat "config-" user-first-name ".org")))))
    (if (file-exists-p extra-config-file)
	(with-current-buffer (or (find-file-noselect extra-config-file)
				 (find-buffer-visiting extra-config-file))
	  (tlon-init-tangle))
      (user-error "Extra config file for user %s not found" user-first-name))))

;;;;; Startup

(defun tlon-init-startup ()
  "Start up Emacs with `tlon-init' config."
  (message "tlon-init: Running startup...")
  (tlon-init-load-paths)
  (tlon-init-load-code-overrides)
  (tlon-init-load-excluded-packages-file user-emacs-directory)
  (tlon-init-defer-load-late-init))

(defun tlon-init-run-post-init-hook ()
  "Run `tlon-init-post-init-hook'."
  (when (tlon-init-machine-pablo-p)
    (dolist (hook tlon-init-post-init-hook)
      (message "tlon-init: Running `%s'." (symbol-name hook)))
    (message "tlon-init: Running of hooks in `tlon-init-post-init-hook' complete")
    (run-hooks 'tlon-init-post-init-hook)))

(defun tlon-init-load-code-overrides ()
  "Load or re-load code overrides and from the currently booted init profile."
  (unless (tlon-init-machine-pablo-p)
    (setq tlon-init-code-overrides
	  (tlon-init-read-file tlon-init-file-code-override))
    (message "tlon-init: Loaded code overrides for Chemacs profile `%s'." tlon-init-chemacs-profile-name)))

(defun tlon-init-defer-load-late-init ()
  "Load `late-init.el' file."
  (unless (tlon-init-machine-pablo-p)
    (add-hook 'elpaca-after-init-hook #'tlon-init-load-late-init)
    (message "tlon-init: Added `tlon-init-load-late-init' to `elpaca-after-init-hook'.")))

(defun tlon-init-load-late-init ()
  "Load `late-init.el'."
  (load tlon-init-file-late-init)
  (message "tlon-init: Loaded `late-init.el' for Chemacs profile `%s'." tlon-init-chemacs-profile-name))

(defun tlon-init-load-paths ()
  "Set paths from the currently booted init profile."
  (interactive)
  (unless (tlon-init-machine-pablo-p)
    (tlon-init-load-default-paths)
    (tlon-init-load-override-paths)
    (message "tlon-init: Loaded paths for Chemacs profile `%s'." tlon-init-chemacs-profile-name)))

(defun tlon-init-load-default-paths ()
  "Set paths in `paths.el', overriding them with `paths-override.el’ if present."
  (dolist (row (tlon-init-get-variables-and-values 'paths))
    (set (car row)
	 (tlon-init-eval-value-when-possible
	  (alist-get (car row) (tlon-init-read-file tlon-init-file-paths-override) (cdr row))))
    (message "tlon-init: Set `%s' to `%s'." (car row) (symbol-value (car row)))))

(defun tlon-init-load-override-paths ()
  "Set paths in `paths-override.el' not present in `paths.el'."
  (dolist (row (tlon-init-read-file tlon-init-file-paths-override))
    (unless (boundp (car row))
      (set (car row)
	   (tlon-init-eval-value-when-possible (cdr row)))
      (message "tlon-init: Set `%s' to `%s'." (car row) (symbol-value (car row))))))

(defun tlon-init-get-variables-and-values (group)
  "Return a list of lists of all variables and corresponding values in GROUP."
  (let (result)
    (dolist (member (custom-group-members group nil))
      (when (eq (cadr member) 'custom-variable)
	(let ((option (car member)))
	  (push `(,option . ,(symbol-value option)) result))))
    (nreverse result)))

(defun tlon-init-eval-value-when-possible (value)
  "Evaluate variable VALUE when possible, else return unevaluated VALUE."
  (condition-case _
      (eval value)
    (error value)))

(defun tlon-init-profile-dir (profile-name)
  "Return the directory of the Chemacs profile PROFILE-NAME."
  (alist-get profile-name (tlon-init-available-init-dirs t) nil nil 'string=))

;;;;; Profile management

(defun tlon-init-create-profile (profile-name &optional overwrite)
  "Create a new Chemacs profile named PROFILE-NAME.
This adds a new profile to `~/.emacs-profiles.el' and creates a directory in the
Chemacs profiles directory. The directory will have PROFILE-NAME as its name. If
profile already exists, throw a user error message, unless OVERWRITE is non-nil."
  (let ((profile-dir (file-name-concat
		      (file-name-directory (directory-file-name user-emacs-directory))
		      profile-name)))
    (when (string-match file-name-invalid-regexp profile-name)
      (user-error "Invalid profile name"))
    (when (and (tlon-init-profile-exists-p profile-name)
	       (not overwrite))
      (user-error "Profile already exists"))
    (make-directory profile-dir t)
    (tlon-init-act-on-chemacs-profiles profile-name profile-dir 'create)
    (message "tlon-init: Created new Chemacs profile `%s'. Default profile is `%s'."
	     profile-name
	     (file-name-nondirectory (tlon-init-profile-dir "default")))
    profile-name))

(defun tlon-init-delete-profile (profile-name)
  "Delete a Chemacs profile with name PROFILE-NAME."
  (interactive
   (list (completing-read "Chemacs profile name to delete: "
			  (mapcar 'car (tlon-init-available-init-dirs)))))
  (when (string= profile-name tlon-init-chemacs-profile-name)
    (unless (y-or-n-p (format "You have chosen to delete profile %s, which is currently active. Proceed? "
			      profile-name))
      (user-error "Aborted")))
  (let ((profile-dir (tlon-init-profile-dir profile-name)))
    ;; first delete profile dir, if it exists
    (when (and (file-exists-p profile-dir)
	       (y-or-n-p (format "Are you sure you want to delete the directory '%s'? "
				 profile-dir)))
      (delete-directory profile-dir t t))
    ;; then delete profile from `~/.emacs-profiles.el'
    (tlon-init-act-on-chemacs-profiles profile-name profile-dir 'delete)
    (message "tlon-init: Deleted Chemacs profile '%s'." profile-name)
    (when (string= profile-dir (tlon-init-profile-dir "default"))
      (call-interactively 'tlon-init-set-default-profile))))

(defun tlon-init-set-default-profile (profile-name)
  "Set the default Chemacs profile to PROFILE-NAME."
  (interactive
   (list (completing-read "Chemacs profile name to set as new default: "
			  (mapcar 'car (tlon-init-available-init-dirs)))))
  (let ((profile-dir (tlon-init-profile-dir profile-name)))
    (when (not (file-exists-p profile-dir))
      (user-error "Profile does not exist"))
    (tlon-init-act-on-chemacs-profiles profile-name profile-dir 'set-default)
    (message "tlon-init: Set default Chemacs profile to '%s'." profile-name)))

(defun tlon-init-deploy-profile (profile-name)
  "Deploy PROFILE-NAME."
  (interactive "sProfile name: ")
  (when (tlon-init-profile-exists-p profile-name)
    (unless (y-or-n-p (format "Profile `%s' already exists. Redeploy? " profile-name))
      (user-error "Aborted")))
  (tlon-init-create-profile profile-name t)
  (if (and (boundp 'paths-file-config)
	   (y-or-n-p " Build init files?"))
      (with-current-buffer (or (find-file-noselect paths-file-config)
			       (find-buffer-visiting paths-file-config))
	(tlon-init-build (tlon-init-profile-dir profile-name)))
    (run-hooks 'tlon-init-post-deploy-hook)
    (message (format "Deployed profile '%s'." profile-name))))

(defun tlon-init-profile-exists-p (profile-name)
  "Return non-nil if Chemacs profile PROFILE-NAME exists."
  (when-let ((profile-dir (tlon-init-profile-dir profile-name)))
    (file-directory-p profile-dir)))

(defun tlon-init-act-on-chemacs-profiles (profile-name &optional profile-dir action)
  "Create, delete or set PROFILE-NAME as default in PROFILE-DIR.
When ACTION is `'set-default', set PROFILE-NAME as default. When ACTION is
`'create', create PROFILE-NAME. Otherwise, delete PROFILE-NAME."
  (let* ((emacs-profiles (file-truename "~/.emacs-profiles.el"))
	 (regex-pattern "(\"%s\" . ((user-emacs-directory . \"%s\")))")
	 (default (format regex-pattern "default" (tlon-init-profile-dir "default")))
	 (regex-search (pcase action
			 ((or 'create 'set-default) (format regex-pattern "default" ".+?"))
			 (_ (format regex-pattern profile-name ".+?"))))
	 (regex-replace (pcase action
			  ('create (concat default "\n" (format regex-pattern profile-name profile-dir)))
			  ('set-default (format regex-pattern "default" profile-dir))
			  (_ ""))))
    (with-current-buffer (or (find-buffer-visiting emacs-profiles)
			     (find-file-noselect emacs-profiles))
      (goto-char (point-min))
      (re-search-forward regex-search nil t)
      (replace-match regex-replace)
      (delete-blank-lines)
      (save-buffer))))

;;;;; Bisection

(defun tlon-init-process-for-bisection (package)
  "<explain behavior> PACKAGE."
  package
  (user-error "This function is not yet defined"))

(provide 'tlon-init)

;;; tlon-init.el ends here
