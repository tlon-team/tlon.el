;;; tlon-dispatch-test.el --- Tests for tlon-dispatch -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the dispatch menu infrastructure in tlon-dispatch.el.
;; The critical test here is the consistency check: every menu entry
;; must resolve to a command that actually exists.  This is the test
;; that would have caught the "80k podcast" and "Leo-Pablo" bugs
;; where the refactored menu derived wrong command names.

;;; Code:

(require 'ert)
(require 'tlon-dispatch)

;;;; tlon-dispatch--build-repo-menu-groups: abbrev derivation

(ert-deftest tlon-dispatch-abbrev-dots-replaced ()
  "Dots in labels are replaced with hyphens in derived abbrevs."
  (let* ((tlon-repo-menu-entries
          '(["Test" ("k" "foo.bar")]))
         (groups (tlon-dispatch--build-repo-menu-groups "dired")))
    (should (equal 'tlon-dired-browse-foo-bar
                   (nth 2 (nth 1 (append (aref groups 0) nil)))))))

(ert-deftest tlon-dispatch-abbrev-spaces-replaced ()
  "Spaces in labels are replaced with hyphens in derived abbrevs."
  (let* ((tlon-repo-menu-entries
          '(["Test" ("k" "foo bar")]))
         (groups (tlon-dispatch--build-repo-menu-groups "dired")))
    (should (equal 'tlon-dired-browse-foo-bar
                   (nth 2 (nth 1 (append (aref groups 0) nil)))))))

(ert-deftest tlon-dispatch-explicit-abbrev-overrides-label ()
  "A three-element entry uses the explicit abbrev, not the label."
  (let* ((tlon-repo-menu-entries
          '(["Test" ("k" "Leo-Pablo" "meetings-leo-pablo")]))
         (groups (tlon-dispatch--build-repo-menu-groups "dired")))
    (should (equal 'tlon-dired-browse-meetings-leo-pablo
                   (nth 2 (nth 1 (append (aref groups 0) nil)))))))

(ert-deftest tlon-dispatch-label-preserved-in-output ()
  "The display label is preserved in the output, not the abbrev."
  (let* ((tlon-repo-menu-entries
          '(["Test" ("k" "Leo-Pablo" "meetings-leo-pablo")]))
         (groups (tlon-dispatch--build-repo-menu-groups "dired")))
    (should (equal "Leo-Pablo"
                   (nth 1 (nth 1 (append (aref groups 0) nil)))))))

(ert-deftest tlon-dispatch-key-preserved-in-output ()
  "The key binding is preserved in the output."
  (let* ((tlon-repo-menu-entries
          '(["Test" ("m l p" "Leo-Pablo" "meetings-leo-pablo")]))
         (groups (tlon-dispatch--build-repo-menu-groups "dired")))
    (should (equal "m l p"
                   (nth 0 (nth 1 (append (aref groups 0) nil)))))))

;;;; tlon-dispatch--build-repo-menu-groups: backend parameter

(ert-deftest tlon-dispatch-magit-backend ()
  "Backend string is included in the generated command name."
  (let* ((tlon-repo-menu-entries
          '(["Test" ("k" "babel-core")]))
         (groups (tlon-dispatch--build-repo-menu-groups "magit")))
    (should (equal 'tlon-magit-browse-babel-core
                   (nth 2 (nth 1 (append (aref groups 0) nil)))))))

;;;; tlon-dispatch--build-repo-menu-groups: separators and headers

(ert-deftest tlon-dispatch-string-separators-pass-through ()
  "Bare strings (separators and sub-headers) pass through unchanged."
  (let* ((tlon-repo-menu-entries
          '(["Group" ("k" "foo") "" "Sub-header" ("j" "bar")]))
         (groups (tlon-dispatch--build-repo-menu-groups "dired"))
         (items (cdr (append (aref groups 0) nil))))
    ;; items: (("k" "foo" CMD) "" "Sub-header" ("j" "bar" CMD))
    (should (equal "" (nth 1 items)))
    (should (equal "Sub-header" (nth 2 items)))))

(ert-deftest tlon-dispatch-group-header-preserved ()
  "The group header (first element of the vector) is preserved."
  (let* ((tlon-repo-menu-entries
          '(["My Header" ("k" "foo")]))
         (groups (tlon-dispatch--build-repo-menu-groups "dired")))
    (should (equal "My Header"
                   (aref (aref groups 0) 0)))))

;;;; tlon-dispatch--build-repo-menu-groups: extra entries

(ert-deftest tlon-dispatch-extra-entries-injected ()
  "Extra entries are injected after the matching key."
  (let* ((tlon-repo-menu-entries
          '(["Test" ("q q" "uqbar") ("q d" "uqbar-audio")]))
         (extra '(("q q" . (("q Q" "uqbar: pull all" tlon-uqbar-pull-all)))))
         (groups (tlon-dispatch--build-repo-menu-groups "dired" extra))
         (items (cdr (append (aref groups 0) nil))))
    ;; items: (("q q" "uqbar" CMD) ("q Q" "uqbar: pull all" CMD) ("q d" ...))
    (should (equal "q Q" (nth 0 (nth 1 items))))
    (should (equal "uqbar: pull all" (nth 1 (nth 1 items))))))

(ert-deftest tlon-dispatch-extra-entries-no-match-silent ()
  "Extra entries with no matching key are silently dropped."
  (let* ((tlon-repo-menu-entries
          '(["Test" ("k" "foo")]))
         (extra '(("nonexistent" . (("x" "extra" some-cmd)))))
         (groups (tlon-dispatch--build-repo-menu-groups "dired" extra))
         (items (cdr (append (aref groups 0) nil))))
    (should (= 1 (length items)))))

;;;; tlon-dispatch--build-repo-menu-groups: multiple groups

(ert-deftest tlon-dispatch-multiple-groups ()
  "Multiple groups produce a vector with the right number of elements."
  (let* ((tlon-repo-menu-entries
          '(["A" ("k" "foo")]
            ["B" ("j" "bar")]))
         (groups (tlon-dispatch--build-repo-menu-groups "dired")))
    (should (= 2 (length groups)))
    (should (equal "A" (aref (aref groups 0) 0)))
    (should (equal "B" (aref (aref groups 1) 0)))))

;;;; Consistency: all menu commands must resolve to defined functions

(ert-deftest tlon-dispatch-all-dired-commands-defined ()
  "Every command referenced by the dired repo menu is defined."
  (let ((groups (tlon-dispatch--build-repo-menu-groups "dired"))
        (missing nil))
    (cl-loop for group across groups do
             (dolist (item (cdr (append group nil)))
               (when (listp item)
                 (let ((cmd (nth 2 item)))
                   (unless (fboundp cmd)
                     (push cmd missing))))))
    (should (null missing))))

(ert-deftest tlon-dispatch-all-magit-commands-defined ()
  "Every command referenced by the magit repo menu is defined."
  (let ((groups (tlon-dispatch--build-repo-menu-groups
                 "magit"
                 '(("q q" . (("q Q" "uqbar: pull all" tlon-uqbar-pull-all))))))
        (missing nil))
    (cl-loop for group across groups do
             (dolist (item (cdr (append group nil)))
               (when (listp item)
                 (let ((cmd (nth 2 item)))
                   (unless (fboundp cmd)
                     (push cmd missing))))))
    (should (null missing))))

;;;; tlon-generate-browse-command: generated commands exist

(ert-deftest tlon-dispatch-browse-commands-exist-for-repos-with-dir ()
  "Every repo with :abbrev and :dir has both magit and dired browse commands."
  (let ((missing nil))
    (dolist (repo tlon-repos)
      (when-let ((abbrev (plist-get repo :abbrev))
                 (dir (plist-get repo :dir)))
        (let ((magit-cmd (intern (format "tlon-magit-browse-%s" abbrev)))
              (dired-cmd (intern (format "tlon-dired-browse-%s" abbrev))))
          (unless (fboundp magit-cmd)
            (push magit-cmd missing))
          (unless (fboundp dired-cmd)
            (push dired-cmd missing)))))
    (should (null missing))))

;;;; Menu entries vs tlon-repos consistency

(ert-deftest tlon-dispatch-menu-abbrevs-match-repo-abbrevs ()
  "Every abbrev derived from menu entries matches an :abbrev in tlon-repos."
  (let ((repo-abbrevs (tlon-repo-lookup-all :abbrev))
        (mismatches nil))
    (dolist (group tlon-repo-menu-entries)
      (dolist (item (cdr (append group nil)))
        (when (listp item)
          (let ((abbrev (or (nth 2 item)
                            (replace-regexp-in-string "[. ]" "-" (nth 1 item)))))
            (unless (member abbrev repo-abbrevs)
              (push (cons (nth 1 item) abbrev) mismatches))))))
    (should (null mismatches))))

(provide 'tlon-dispatch-test)
;;; tlon-dispatch-test.el ends here
