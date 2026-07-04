;;; ores-build-site.el --- Build product site. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Marco Craveiro
;;
;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Maintainer: Marco Craveiro <marco.craveiro@gmail.com>
;; URL: https://github.com/OreStudio/OreStudio
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
(require 'seq)
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

(let ((src-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "ores-link-types.el" src-dir))
  ;; Package bootstrap lives in its own file so CI can pre-warm the cache
  ;; out of band (see CTestSite.cmake): a cold install byte-compiles dozens
  ;; of files, and that output must not be scraped into the CDash build.
  (load-file (expand-file-name "ores-site-packages.el" src-dir)))

(setq org-id-locations-file (expand-file-name "./.org-id-locations-file"))
(setq org-publish-timestamp-directory (expand-file-name "./build/output/org-timestamps/"))
(make-directory org-publish-timestamp-directory t)
(org-id-update-id-locations
 (seq-remove (lambda (f) (string-match-p "/\\.claude/worktrees/" f))
             (directory-files-recursively "." "\\.org$")))

;; Ensure the site's package dependencies are present. A no-op when the cache
;; is already warm (the CI pre-warm step, or a developer's local .packages).
(ores-site-ensure-packages)

;; Load the publishing system
(require 'ox-publish)

;; Stylesheet uses absolute /OreStudio/ path so it resolves both in
;; production (orestudio.github.io/OreStudio/) and under the local
;; preview server (see compass site serve).
(defvar html-header "<link rel=\"stylesheet\" href=\"/OreStudio/assets/style.css\">
<link rel=\"icon\" href=\"/OreStudio/assets/images/modern-icon.png\">
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.10.0/highlight.min.js\"></script>
<script>var hlf=function(){Array.prototype.forEach.call(document.querySelectorAll(\"pre.src\"),function(t){var e;e=t.getAttribute(\"class\"),e=e.replace(/src-(\w+)/,\"src-$1 $1\"),t.setAttribute(\"class\",e),hljs.highlightBlock(t)})};addEventListener(\"DOMContentLoaded\",hlf);</script>
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.10.0/styles/atom-one-dark.min.css\" />
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css\">")

;; Disable babel evaluation during export. ob-shell is still loaded for
;; syntax highlighting, but we never want CI to execute src blocks.
;; Developers refresh #+RESULTS: manually via the org-babel-refresh recipe.
(setq org-export-use-babel nil)

;; Never interpret a_b / a^b as subscript/superscript on export:
;; snake_case identifiers (is_temporal, valid_from) are pervasive in
;; prose and tables, and render as nonsense subscripts otherwise.
(setq org-export-with-sub-superscripts nil)

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
    <a href='/OreStudio/index.html'>Home</a>
    <div class='nav-group'>
      <span class='nav-group-label' tabindex='0'>Project</span>
      <ul class='nav-dropdown'>
        <li><a href='/OreStudio/readme.html'>README</a></li>
        <li><a href='/OreStudio/doc/orientation.html'>Orientation</a></li>
        <li><a href='/OreStudio/doc/compass.html'>Compass</a></li>
        <li><a href='/OreStudio/doc/identity/product_identity.html'>Product Identity</a></li>
        <li><a href='/OreStudio/doc/roadmap.html'>Roadmap</a></li>
        <li><a href='/OreStudio/projects/modeling/system_model.html'>System Model</a></li>
      </ul>
    </div>
    <div class='nav-group'>
      <span class='nav-group-label' tabindex='0'>Modeling</span>
      <ul class='nav-dropdown'>
        <li><a href='/OreStudio/doc/knowledge/masd/masd.html'>Methodology</a></li>
        <li><a href='/OreStudio/doc/knowledge/masd/logical_space.html'>Logical Space</a></li>
        <li><a href='/OreStudio/doc/knowledge/masd/physical_space.html'>Physical Space</a></li>
        <li><a href='/OreStudio/doc/knowledge/masd/technical_space.html'>Technical Space</a></li>
        <li><a href='/OreStudio/doc/knowledge/masd/facet.html'>Facet</a></li>
        <li><a href='/OreStudio/doc/knowledge/masd/variability.html'>Variability</a></li>
        <li><a href='/OreStudio/projects/modeling/technical_spaces.html'>ORE Studio Model</a></li>
        <li><a href='/OreStudio/projects/modeling/technical_space_cpp.html'>C++ Technical Space</a></li>
        <li><a href='/OreStudio/projects/modeling/technical_space_sql.html'>SQL Technical Space</a></li>
        <li><a href='/OreStudio/projects/modeling/technical_spaces_other.html'>Other Tech Spaces</a></li>
        <li><a href='/OreStudio/projects/modeling/variability_model.html'>Variability Model</a></li>
      </ul>
    </div>
    <div class='nav-group'>
      <span class='nav-group-label' tabindex='0'>Developer</span>
      <ul class='nav-dropdown'>
        <li><a href='/OreStudio/doc/developer.html'>Guide</a></li>
        <li><a href='/OreStudio/doc/recipes/recipes.html'>Recipes</a></li>
        <li><a href='/OreStudio/doc/agile/agile.html'>Agile</a></li>
        <li><a href='/OreStudio/agile/index.html'>Agile Board</a></li>
        <li><a href='/OreStudio/doc/llm/llm.html'>LLMs</a></li>
      </ul>
    </div>
    <div class='nav-group'>
      <span class='nav-group-label' tabindex='0'>Resources</span>
      <ul class='nav-dropdown'>
        <li><a href='/OreStudio/doc/manual/manuals.html'>Manuals</a></li>
        <li><a href='/OreStudio/doc/knowledge/knowledge.html'>Knowledge</a></li>
        <li><a href='/OreStudio/graph/index.html'>Knowledge Graph</a></li>
      </ul>
    </div>
    <a href='/OreStudio/doc/downloads.html'>Downloads</a>
    <a href='https://github.com/OreStudio/OreStudio' aria-label='GitHub' title='GitHub'><i class='fab fa-github'></i></a>
  </nav>
</header>")

(defvar ores/repo-root (expand-file-name "./"))

(defun ores/site-preamble (info)
  "Return site preamble with a per-page Edit on GitHub button.
Injects an edit link into the nav using :input-file from INFO."
  (let* ((input-file (plist-get info :input-file))
         (rel-path   (when input-file
                       (file-relative-name input-file ores/repo-root)))
         (edit-url   (when rel-path
                       (concat "https://github.com/OreStudio/OreStudio/edit/main/"
                               rel-path)))
         (edit-btn   (when edit-url
                       (format "<a class='edit-on-github' href='%s' \
title='Edit this page on GitHub' aria-label='Edit this page on GitHub'>\
<i class='fas fa-pencil-alt'></i></a>"
                               edit-url))))
    (if edit-btn
        (replace-regexp-in-string "</nav>" (concat edit-btn "</nav>")
                                  site-html-preamble)
      site-html-preamble)))

;; Define the publishing project
(setq org-publish-project-alist
      `(
        ("site:pages"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\)/\\|projects/ores.org-js"
         :publishing-function org-html-publish-to-html
         :publishing-directory "./build/output/site/OreStudio"
         :html-preamble ores/site-preamble
         :with-author nil
         :with-creator t
         :with-toc t
         :section-numbers nil
         ;; Never interpret a_b / a^b as subscript/superscript: snake_case
         ;; identifiers (is_temporal, valid_from) are pervasive in prose.
         :with-sub-superscript nil
         :time-stamp-file nil)
        ("site:images"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\)/\\|projects/ores.org-js"
         :base-extension "png\\|jpe?g\\|gif\\|svg"
         :publishing-directory "./build/output/site/OreStudio/"
         :publishing-function org-publish-attachment)
        ("site:style"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\)/\\|projects/ores.org-js"
         :base-extension "css"
         :publishing-directory "./build/output/site/OreStudio/"
         :publishing-function org-publish-attachment)
        ("site:pdf"
         :recursive t
         :base-directory "./"
         :exclude "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\)/\\|projects/ores.org-js"
         :base-extension "pdf"
         :publishing-directory "./build/output/site/OreStudio/"
         :publishing-function org-publish-attachment)
        ("site:main" :components("site:pages" "site:images" "site:style" "site:pdf"))))

(defun ores-inject-site-nav (index-file preamble fix-tag)
  "Inject site CSS, PREAMBLE nav, and app-specific FIX-TAG into INDEX-FILE."
  (when (file-exists-p index-file)
    (let* ((content (with-temp-buffer
                      (insert-file-contents index-file)
                      (buffer-string)))
           (css-tag "<link rel=\"stylesheet\" href=\"/OreStudio/assets/style.css\">")
           (fa-tag  "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css\">")
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

(defun ores-deploy-web-app (src-dir site-dir app-name fix-tag)
  "Copy the static web app at SRC-DIR to SITE-DIR/APP-NAME/ and inject nav.
The ores.org-js apps (graph, agile) all deploy through here so they are
managed uniformly: copy, then patch index.html with the site chrome."
  (let ((src (expand-file-name src-dir))
        (dst (expand-file-name app-name site-dir)))
    (if (not (file-directory-p src))
        (message "Warning: %s not found; skipping" src-dir)
      (make-directory dst t)
      (copy-directory src dst nil t t)
      (ores-inject-site-nav (expand-file-name "index.html" dst)
                            site-html-preamble fix-tag)
      (message "%s deployed to %s" app-name dst))))

(defun ores-deploy-web-apps (site-dir)
  "Deploy every ores.org-js app into SITE-DIR."
  ;; Graph: float the header and undo the body flex/padding from
  ;; style.css (org-roam-ui sizes its canvas to window inner dimensions).
  (ores-deploy-web-app
   "./external/org-roam-ui" site-dir "graph"
   "<style>#site-header{position:fixed;top:0;left:0;right:0;z-index:9999;}body{display:block;padding:0;align-items:unset;}#__next{margin-top:56px;}</style>")
  ;; Agile board: undo the site body flex/padding; the app styles itself.
  (ores-deploy-web-app
   "./projects/ores.org-js/agile" site-dir "agile"
   "<style>body{display:block;padding:0;align-items:unset;}</style>"))

(condition-case err
    (progn
      ;; Sync org-roam DB before publishing so id: links are resolvable
      (setq org-roam-directory (expand-file-name "./"))
      (setq org-roam-db-location (expand-file-name "./.org-roam.db"))
      (setq org-roam-file-exclude-regexp
            "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\)/\\|projects/ores.org-js")
      (require 'org-roam)
      (condition-case roam-err
          (org-roam-db-sync)
        (error (message "Warning: org-roam DB sync failed: %s"
                        (error-message-string roam-err))))
      (org-publish-all nil)
      (ores-deploy-web-apps "./build/output/site/OreStudio")
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

(provide 'ores-build-site)
;;; ores-build-site.el ends here
