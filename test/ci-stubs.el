;;; ci-stubs.el --- Minimal stubs for CI dependencies -*- lexical-binding: t -*-

;;; Commentary:

;; Provides minimal stubs for packages that are not available in CI but are
;; required at load time by tlon modules.  Only used by the `test-ci' Makefile
;; target; local development uses real packages via elpaca.

;;; Code:

(require 'cl-lib)

;;;; elpaca

(defvar elpaca-directory "/tmp/elpaca/")
(defvar elpaca-repos-directory "/tmp/elpaca/repos/")
(provide 'elpaca)

;;;; paths

(defvar paths-dir-tlon-repos "/tmp/tlon-repos/")
(defvar paths-dir-tlon-todos "/tmp/tlon-todos/")
(defvar paths-dir-downloads "/tmp/downloads/")
(defvar paths-dir-babel-refs "/tmp/babel-refs/")
(defvar paths-dir-external-repos "/tmp/external-repos/")
(defvar paths-dir-split-git "/tmp/split-git/")
(defvar paths-dir-dotemacs "/tmp/dotemacs/")
(defvar paths-files-bibliography-all nil)
(defvar paths-tlon-todos-jobs-id "stub-jobs-id")
(defvar paths-tlon-todos-generic-id "stub-generic-id")
(provide 'paths)

;;;; gptel (AI interface)

(defvar gptel-model nil)
(defvar gptel-backend nil)
(defvar gptel-default-mode 'org-mode)
(defvar gptel-expert-commands nil)
(defvar gptel--known-backends nil)
(defun gptel-request (&rest _) nil)
(defun gptel-make-openai (&rest _) nil)
(provide 'gptel)
(provide 'gptel-extras)

;;;; citar (bibliography)

(defvar citar-bibliography nil)
(defun citar-get-entry (&rest _) nil)
(defun citar-select-refs (&rest _) nil)
(defun citar-extras-refresh-bibliography (&rest _) nil)
(provide 'citar)
(provide 'citar-cache)

;;;; ebib (bibliography manager)

(defvar ebib--databases nil)
(provide 'ebib)

;;;; org-element-ast (Emacs 30+ only; Emacs 29 has these in org-element)

(unless (featurep 'org-element-ast)
  (require 'org-element nil t)
  (provide 'org-element-ast))

;;;; tlon-dispatch (transient menus)

;; tlon-dispatch is no longer stubbed.  It requires transient, which is
;; installed from MELPA by `make deps'.  The generated browse commands
;; reference magit-status, which is defined as a no-op below.

;;;; magit / forge

(defun magit-status (&optional _dir) nil)
(provide 'magit)
(provide 'magit-extras)
(provide 'forge)
(provide 'forge-core)
(provide 'forge-search)
(provide 'forge-extras)

;;;; markdown-mode

(defvar markdown-regex-heading-1-atx "^# ")
(defvar markdown-regex-heading-2-atx "^## ")
(defvar markdown-regex-heading-atx "^#+ ")
(defvar markdown-mode-map (make-sparse-keymap))
(define-derived-mode markdown-mode text-mode "Markdown")
(provide 'markdown-mode)
(provide 'markdown-mode-extras)

;;;; request (HTTP)

(defun request (&rest _args) nil)
(provide 'request)

;;;; shut-up

(defmacro shut-up (&rest body)
  "Execute BODY with output suppressed."
  (declare (indent 0))
  `(let ((inhibit-message t)) ,@body))
(provide 'shut-up)

;;;; other stubs

(provide 'simple-extras)
(provide 'files-extras)
(provide 'bibtex-extras)
(provide 'vc-extras)
(provide 'reverso)
(provide 'read-aloud)
(defvar oauth2-auto-additional-providers-alist nil)
(provide 'oauth2-auto)
(provide 'doi-utils)
(provide 'threads)

;;; ci-stubs.el ends here
