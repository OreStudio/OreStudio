;; Directory Local Variables         -*- no-byte-compile: t; -*-
;;; For more information see (info "(emacs) Directory Variables")
((nil . ((eval . (setq-local
                  org-roam-directory (expand-file-name (locate-dominating-file
                                                        default-directory ".dir-locals.el"))))
         (eval . (setq-local
                  org-roam-db-location (expand-file-name ".org-roam.db"
                                                         org-roam-directory)))
         (eval . (load (expand-file-name
                        "projects/ores.lisp/ores-babel.el"
                        (locate-dominating-file
                         (or buffer-file-name default-directory)
                         ".dir-locals.el"))
                       :no-error :no-message)))))
