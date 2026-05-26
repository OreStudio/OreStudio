;;; .build-site.el --- Build product site. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Marco Craveiro
;;
;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Maintainer: Marco Craveiro <marco.craveiro@gmail.com>
;; URL: https://github.com/MASD-Project/progen/blob/main/.build-site.el
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
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Builds the site under the build directory.
;;
;;; Code:
(require 'package)
(require 'org)
(require 'org-id)
(require 'ob-shell)
(require 'oc-bibtex)
(require 'ox-publish)
(require 'org-element)

;; Prevent Emacs from entering debugger or showing full stack traces
(setq debug-on-error nil)

;; Optionally, also suppress debug-on-quit and debug-on-signal if needed
(setq debug-on-quit nil)

(defvar site-github-user "OreStudio"
  "GitHub username or organization name.")

(defvar site-github-repo "OreStudio"
  "GitHub repository name.")

(defvar site-github-branch "main"
  "The default branch to link to (e.g., 'main', 'master').")

(org-link-set-parameters
 "proj"
 :export (lambda (path desc backend)
           "Export `proj:' links to full GitHub URLs for HTML backend."
           (when (eq backend 'html)
             (let ((new-url (format "https://github.com/%s/%s/blob/%s/%s"
                                    site-github-user
                                    site-github-repo
                                    site-github-branch
                                    path)))
               (format "<a href=\"%s\">%s</a>" new-url (or desc new-url))))))

(setq org-id-locations-file (expand-file-name "./.org-id-locations-file"))
(org-id-update-id-locations (directory-files-recursively "." "\\.org$"))
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Initialize the package system
(package-initialize)
(condition-case err
    (package-refresh-contents)
  (error (message "Warning: could not refresh package archives (%s); using cached contents"
                  (error-message-string err))))

;; Install dependencies
(package-install 'htmlize)
(package-install 'citeproc)
(package-install 'org-roam)

;; Load the publishing system
(require 'ox-publish)

;; Stylesheet uses absolute /OreStudio/ path so it resolves both in
;; production (orestudio.github.io/OreStudio/) and under the local
;; preview server, which symlinks the build output to /OreStudio (see
;; build/scripts/serve-site.sh).
(defvar html-header "<link rel=\"stylesheet\" href=\"/OreStudio/assets/style.css\">
<link rel=\"icon\" href=\"/OreStudio/assets/images/modern-icon.png\">
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.10.0/highlight.min.js\"></script>
<script>var hlf=function(){Array.prototype.forEach.call(document.querySelectorAll(\"pre.src\"),function(t){var e;e=t.getAttribute(\"class\"),e=e.replace(/src-(\w+)/,\"src-$1 $1\"),t.setAttribute(\"class\",e),hljs.highlightBlock(t)})};addEventListener(\"DOMContentLoaded\",hlf);</script>
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.10.0/styles/googlecode.min.css\" />
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css\">")

;; Disable babel evaluation during export. ob-shell is still loaded for
;; syntax highlighting, but we never want CI to execute src blocks.
;; Developers refresh #+RESULTS: manually via the org-babel-refresh recipe.
(setq org-export-use-babel nil)

;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head html-header)

;; Make org-roam id-links scroll to the right place. Org rewrites
;; [[id:UUID]] links to `…#ID-UUID' on the href side, but does not stamp
;; a matching id attribute on the target heading (it only does that for
;; :CUSTOM_ID:). The advice below prepends an empty `<a id="ID-UUID"/>'
;; anchor to every exported heading that carries an :ID:. Browsers
;; scroll to the anchor; the heading's auto-generated org-html id stays
;; intact for the table of contents.
(defun ores/org-html-headline-id-anchor (orig-fun headline contents info)
  "Prepend an `ID-<uuid>' anchor when HEADLINE has an :ID: property."
  (let ((rendered (funcall orig-fun headline contents info))
        (uuid (org-element-property :ID headline)))
    (if uuid
        (concat (format "<a id=\"ID-%s\" class=\"id-anchor\"></a>\n" uuid)
                rendered)
      rendered)))

(advice-add 'org-html-headline :around #'ores/org-html-headline-id-anchor)

(defvar site-html-preamble "<header id='site-header'>
  <nav id='site-nav'>
    <a href='/OreStudio/readme.html'>Home</a>
    <a href='/OreStudio/doc/orientation.html'>Orientation</a>
    <a href='/OreStudio/doc/identity/product_identity.html'>Product</a>
    <a href='/OreStudio/projects/modeling/system_model.html'>Architecture</a>
    <a href='/OreStudio/doc/llm/llm.html'>LLMs</a>
    <a href='/OreStudio/doc/manual/manuals.html'>Manuals</a>
    <a href='/OreStudio/doc/downloads.html'>Downloads</a>
    <a href='/OreStudio/doc/agile/agile.html'>Agile</a>
    <a href='/OreStudio/doc/developer.html'>Developer</a>
    <a href='/OreStudio/doc/agile/versions/versions.html'>Roadmap</a>
    <a href='/OreStudio/graph/index.html'>Knowledge Graph</a>
    <a href='https://github.com/OreStudio/OreStudio' aria-label='GitHub' title='GitHub'><i class='fab fa-github'></i></a>
  </nav>
</header>")

;; Define the publishing project
(setq org-publish-project-alist
      `(
        ("site:pages"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\)/\\|external/org-roam-ui"
         :publishing-function org-html-publish-to-html
         :publishing-directory "./build/output/site/OreStudio"
         :html-preamble ,site-html-preamble
         :with-author nil
         :with-creator t
         :with-toc t
         :section-numbers nil
         :time-stamp-file nil)
        ("site:images"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\)/\\|external/org-roam-ui"
         :base-extension "png\\|jpe?g\\|gif\\|svg"
         :publishing-directory "./build/output/site/OreStudio/"
         :publishing-function org-publish-attachment)
        ("site:style"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\)/\\|external/org-roam-ui"
         :base-extension "css"
         :publishing-directory "./build/output/site/OreStudio/"
         :publishing-function org-publish-attachment)
        ("site:pdf"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\)/\\|external/org-roam-ui"
         :base-extension "pdf"
         :publishing-directory "./build/output/site/OreStudio/"
         :publishing-function org-publish-attachment)
        ("site:main" :components("site:pages" "site:images" "site:style" "site:pdf"))))

(defun ores-inject-site-nav (index-file preamble)
  "Inject site CSS and PREAMBLE nav into the org-roam-ui INDEX-FILE."
  (when (file-exists-p index-file)
    (let* ((content (with-temp-buffer
                      (insert-file-contents index-file)
                      (buffer-string)))
           (css-tag "<link rel=\"stylesheet\" href=\"/OreStudio/assets/style.css\">")
           (fa-tag  "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css\">")
           ;; Fix graph-page layout: float the header and undo the body flex/padding
           ;; from style.css (org-roam-ui sizes its canvas to window inner dimensions).
           (fix-tag "<style>#site-header{position:fixed;top:0;left:0;right:0;z-index:9999;}body{display:block;padding:0;align-items:unset;}</style>")
           (patched (replace-regexp-in-string
                     "</head>"
                     (concat css-tag fa-tag fix-tag "</head>")
                     (replace-regexp-in-string
                      "<body>"
                      (concat "<body>" preamble)
                      content))))
      (with-temp-file index-file
        (insert patched)))
    (message "Injected site nav into %s" index-file)))

(defun ores-deploy-org-roam-ui (site-dir)
  "Copy pre-built org-roam-ui static files to SITE-DIR/graph/."
  (let ((src (expand-file-name "./external/org-roam-ui"))
        (dst (expand-file-name "graph" site-dir)))
    (if (not (file-directory-p src))
        (message "Warning: external/org-roam-ui not found; skipping")
      (make-directory dst t)
      (copy-directory src dst nil t t)
      (ores-inject-site-nav (expand-file-name "index.html" dst) site-html-preamble)
      (message "org-roam-ui files copied to %s" dst))))

(condition-case err
    (progn
      ;; Sync org-roam DB before publishing so id: links are resolvable
      (setq org-roam-directory (expand-file-name "./"))
      (setq org-roam-db-location (expand-file-name "./.org-roam.db"))
      (setq org-roam-file-exclude-regexp
            "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\)/\\|external/org-roam-ui")
      (require 'org-roam)
      (condition-case roam-err
          (org-roam-db-sync)
        (error (message "Warning: org-roam DB sync failed: %s"
                        (error-message-string roam-err))))
      (org-publish-all nil)
      (ores-deploy-org-roam-ui "./build/output/site/OreStudio")
      (condition-case roam-err
          (progn
            (load-file (expand-file-name "./projects/ores.lisp/src/ores-org-roam-export.el"))
            (ores/org-roam-export-graph-json
             (expand-file-name "./.org-roam.db")
             (expand-file-name "./build/output/site/OreStudio/graph/graphdata.json")
             (expand-file-name "./")
             "/OreStudio/"))
        (error (message "Warning: org-roam graph export skipped: %s"
                        (error-message-string roam-err))))
      (message "Build succeeded.")
      (kill-emacs 0))
  (error
   (message "Build failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide '.build-site)
;;; .build-site.el ends here
