;; Publish only the site's static attachments (css, js, images).
;; Diagnostic: the built site was serving without /OreStudio/assets/style.css.
(setq ores/site-setup-only t)
(load-file (expand-file-name "projects/ores.lisp/src/ores-build-site.el"))
(dolist (proj '("site:style" "site:js" "site:images"))
  (message "publishing %s" proj)
  (org-publish proj t))
(message "attachment publish done")
