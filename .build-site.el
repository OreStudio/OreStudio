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
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)
(package-install 'citeproc)

;; Load the publishing system
(require 'ox-publish)

;; Stylesheet uses absolute /OreStudio/ path so it resolves both in
;; production (orestudio.github.io/OreStudio/) and under the local
;; preview server, which symlinks the build output to /OreStudio (see
;; build/scripts/serve-site.sh).
(defvar html-header "<link rel=\"stylesheet\" href=\"/OreStudio/assets/style.css\">
<link rel=\"icon\" href=\"/OreStudio/assets/images/modern-icon.png\">
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.10.0/highlight.min.js\"></script>
<script>var hlf=function(){Array.prototype.forEach.call(document.querySelectorAll(\"pre.src\"),function(t){var e;e=t.getAttribute(\"class\"),e=e.replace(/src-(\w+)/,\"src-$1 $1\"),console.log(e),t.setAttribute(\"class\",e),hljs.highlightBlock(t)})};addEventListener(\"DOMContentLoaded\",hlf);</script>
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

(defvar site-html-preamble "<header id='site-header'>
  <nav id='site-nav'>
    <a href='/OreStudio/readme.html'>Home</a>
    <a href='/OreStudio/doc/doc.html'>Documentation</a>
    <a href='/OreStudio/doc/v2/compass.html'>V2 Compass</a>
    <a href='/OreStudio/doxygen/html/index.html'>Doxygen</a>
    <a href='https://github.com/OreStudio/OreStudio' aria-label='GitHub' title='GitHub'><i class='fab fa-github'></i></a>
  </nav>
</header>")

;; Define the publishing project
(setq org-publish-project-alist
      `(
        ("site:pages"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\)/"
         :publishing-function org-html-publish-to-html
         :publishing-directory "./build/output/site"
         :html-preamble ,site-html-preamble
         :with-author nil
         :with-creator t
         :with-toc t
         :section-numbers nil
         :time-stamp-file nil)
        ("site:images"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\)/"
         :base-extension "png\\|jpg\\|gif\\|svg"
         :publishing-directory "./build/output/site/"
         :publishing-function org-publish-attachment)
        ("site:style"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\)/"
         :base-extension "css"
         :publishing-directory "./build/output/site/"
         :publishing-function org-publish-attachment)
        ("site:main" :components("site:pages" "site:images" "site:style"))))

(condition-case err
    (progn
      (org-publish-all t)
      (message "Build succeeded.")
      (kill-emacs 0))
  (error
   (message "Build failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide '.build-site)
;;; .build-site.el ends here
