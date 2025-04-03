;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")
((emacs-lisp-mode . (;; Aspell does not currently support Korean & Arabic, so we just disable it
		     (eval . (jinx-mode -1))
		     ((fill-column . 80)))))
