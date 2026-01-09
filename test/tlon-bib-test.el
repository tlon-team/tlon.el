;;; tlon-bib-test.el --- Tests for tlon-bib -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'tlon-bib)

(ert-deftest tlon-bib-populate-url-field-slugifies-unicode-punctuation ()
  "Ensure `tlon-bib-populate-url-field' generates URLs with clean slugs.
This is a regression test for titles containing non-ASCII punctuation (e.g.,
Japanese brackets \"【】\" and fullwidth colon \"：\"), which should not appear
verbatim in the generated URL slug."
  (let ((tlon-languages-properties '(("japanese" (:code . "ja"))))
        (tlon-core-bare-dirs '(("ja" ("en" ("articles" . "記事")))))
        (captured-url nil))
    (cl-letf (((symbol-function 'tlon-ensure-bib) (lambda () t))
              ((symbol-function 'tlon-get-key-at-point) (lambda () "Todd2016WantToDoJa"))
              ((symbol-function 'tlon-lookup)
               (lambda (list key &rest pairs)
                 (cond
                  ;; Resolve language code from langid name.
                  ((eq list tlon-languages-properties)
                   (pcase (list key pairs)
                     (`(:code (:name "japanese")) "ja")
                     (_ nil)))
                  ;; Resolve bare dir path for articles.
                  ;;
                  ;; In `tlon-bib-populate-url-field' this is called as:
                  ;;   (tlon-lookup tlon-core-bare-dirs lang "en" "articles")
                  ;; so KEY is the language code and PAIRS is ("en" "articles").
                  ((eq list tlon-core-bare-dirs)
                   (pcase (list key pairs)
                     (`("ja" ("en" "articles")) "記事")
                     (_ nil)))
                  (t nil))))
              ((symbol-function 'tlon-repo-lookup)
               (lambda (&rest _args) "https://eanotes.jp"))
              ;; Pretend we're in `bibtex-mode' without requiring bibtex buffers.
              ((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (memq 'bibtex-mode modes)))
              ((symbol-function 'bibtex-narrow-to-entry) (lambda () nil))
              ((symbol-function 'bibtex-beginning-of-entry) (lambda () nil))
              ;; Provide get/set-field pair for the function under test.
              ((symbol-function 'bibtex-extras-get-field)
               (lambda (field)
                 (pcase field
                   ("langid" "japanese")
                   ("title" "【キャリアガイド】第4部：良いことをしたい")
                   ("url" nil)
                   (_ nil))))
              ((symbol-function 'bibtex-set-field)
               (lambda (field value &rest _args)
                 (when (string= field "url")
                   (setq captured-url value))))
              ;; Avoid interactive prompt just in case.
              ((symbol-function 'y-or-n-p) (lambda (&rest _args) t)))
      (with-temp-buffer
        (setq major-mode 'bibtex-mode)
        (tlon-bib-populate-url-field)
        (should (equal captured-url
                       "https://eanotes.jp/記事/キャリアガイド-第4部-良いことをしたい"))))))

(provide 'tlon-bib-test)
;;; tlon-bib-test.el ends here
