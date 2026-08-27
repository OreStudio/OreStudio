;;; ores-build-page.el --- Publish a single org file to HTML. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;;; Commentary:
;;
;; Publishes one org file to the site output using the exact configuration
;; `ores-build-site.el' defines, so a page rebuilt here is byte-identical to the
;; page a full build would produce.
;;
;; The site script performs its work at load time and then exits.  Binding
;; `ores/site-setup-only' before loading it stops it after the configuration,
;; so no whole-site publish, org-roam sync or web-app deployment runs here.
;; The publish cache the full build wrote is reused, so this script inherits its
;; state rather than rebuilding it, and writes it back afterwards so a later
;; full build sees the page as published.  The named files are republished
;; whether or not their timestamps have moved, since the caller has asked for
;; them by name.
;;
;; Links resolve against the id location map written by the last full build,
;; which is loaded rather than rebuilt: rebuilding it means parsing every org
;; file in the repository, and that cost is the whole reason this script exists.
;; A page linking to a document created since the last full build will therefore
;; fail to resolve that link, and the failure is reported rather than hidden.
;;
;; Usage:
;;
;;   emacs -Q --script projects/ores.lisp/src/ores-build-page.el -- <file.org>...
;;   emacs -Q --script projects/ores.lisp/src/ores-build-page.el -- --all
;;
;; With `--all', every page whose source has changed since the last build is
;; republished, and unchanged pages are skipped.  Named files are republished
;; regardless.
;;
;;; Code:
(defvar ores/page-files
  (cdr (member "--" command-line-args-left))
  "Org files to publish, taken from the command line after a `--' separator.")

(unless ores/page-files
  (message "usage: ores-build-page.el -- <file.org> [file.org...]")
  (kill-emacs 2))

(defvar ores/site-setup-only t
  "Tell `ores-build-site.el' to define its configuration and stop.")

(load-file (expand-file-name "projects/ores.lisp/src/ores-build-site.el"))

(defvar ores/page-all (equal ores/page-files '("--all"))
  "Non-nil when every changed page is to be republished.")

(org-publish-initialize-cache "site:pages")

(if ores/page-all
    ;; Let the publish cache decide: unchanged pages are skipped.
    (progn
      (setq org-publish-use-timestamps-flag t)
      (org-publish-all nil)
      (org-publish-write-cache-file)
      (message "ores-build-page: incremental publish complete.")
      (kill-emacs 0))
  ;; A file named on the command line is republished whether or not its
  ;; timestamp has moved.  The caller asked for it, and the usual reason to ask
  ;; is that the rendering rather than the source has changed.
  (setq org-publish-use-timestamps-flag nil))

(let ((project (assoc "site:pages" org-publish-project-alist))
      (failed 0))
  (dolist (file ores/page-files)
    (let ((path (expand-file-name file)))
      (cond
       ((not (file-readable-p path))
        (message "ores-build-page: cannot read %s" path)
        (setq failed (1+ failed)))
       (t
        (message "ores-build-page: publishing %s" file)
        (condition-case err
            (org-publish-file path project t)
          (error
           (message "ores-build-page: FAILED %s: %s" file
                    (error-message-string err))
           (setq failed (1+ failed))))))))
  (org-publish-write-cache-file)
  (message "ores-build-page: %d file(s), %d failure(s)."
           (length ores/page-files) failed)
  (kill-emacs (if (> failed 0) 1 0)))

(provide 'ores-build-page)
;;; ores-build-page.el ends here
