;;; .build-site.el --- Build product site.
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
(require 'oc-bibtex)
(require 'ox-publish)
(require 'org-element)

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

(defvar html-header "<link id=\"pagestyle\" rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.10.0/highlight.min.js\"></script>
<script>var hlf=function(){Array.prototype.forEach.call(document.querySelectorAll(\"pre.src\"),function(t){var e;e=t.getAttribute(\"class\"),e=e.replace(/src-(\w+)/,\"src-$1 $1\"),console.log(e),t.setAttribute(\"class\",e),hljs.highlightBlock(t)})};addEventListener(\"DOMContentLoaded\",hlf);</script>
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.10.0/styles/googlecode.min.css\" />")

;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head html-header)

;; Define the publishing project
(setq org-publish-project-alist
      '(
        ("site:pages"
         :recursive t
         :base-directory "./"
         :publishing-function org-html-publish-to-html
         :publishing-directory "./build/output/site"
         :with-author nil
         :with-creator t
         :with-toc t
         :section-numbers nil
         :time-stamp-file nil)
        ("site:images"
         :recursive t
         :base-directory "./"
         :base-extension "png\\|jpg\\|gif\\|svg"
         :publishing-directory "./build/output/site/"
         :publishing-function org-publish-attachment)
        ("site:main" :components("site:pages" "site:images"))))

;; Generate the site output
(org-publish-all t)

(message "Build complete!")

(provide '.build-site)
;;; .build-site.el ends here
