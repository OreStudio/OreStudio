;;; ores-build-help.el --- Export the user manual as Qt-help HTML. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Marco Craveiro
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
;; Exports the single-file user manual (doc/manual/user_guide/user_manual.org,
;; which #+includes every chapter) to ONE self-contained HTML file suitable
;; for compilation into a Qt help collection (.qch) by qhelpgenerator.
;;
;; Unlike the site export, the output is help-friendly:
;;   - assets are RELATIVE (images/NAME.png) and copied alongside the HTML,
;;     not absolute /OreStudio/ URLs that break inside a .qch;
;;   - a plain stylesheet renders in QTextBrowser, which supports only a
;;     subset of HTML/CSS — no dark web theme, web fonts, highlight.js, or
;;     font-awesome;
;;   - no site preamble/nav.
;;
;; Output: build/output/help/user_manual.html + build/output/help/images/.
;;
;;; Code:
(require 'org)
(require 'org-id)
(require 'ox-html)

(setq debug-on-error nil)
(setq debug-on-quit nil)

(defvar ores/help-root
  (if (or load-file-name buffer-file-name)
      (expand-file-name "../../../"
                        (file-name-directory (or load-file-name buffer-file-name)))
    default-directory)
  "Repository root, captured when this script is loaded.")

(defvar ores/help-output-dir
  (expand-file-name "build/output/help/" ores/help-root)
  "Directory the help HTML and its assets are written to.")

(defvar ores/help-manual
  (expand-file-name "doc/manual/user_guide/user_manual.org" ores/help-root)
  "The single-file manual to export.")

;; Load the shared proj: link type, then override its HTML export below so
;; images become relative and are collected for copying.
(load-file (expand-file-name
            "ores-link-types.el"
            (file-name-directory (or load-file-name buffer-file-name))))

(defvar ores/help-images nil
  "Alist of (REPO-RELATIVE-PATH . BASENAME) for images to copy.")

(defun ores/help--proj-export (path desc backend _info)
  "Export a proj: link for the Qt-help HTML build.
Images under the repo become relative images/BASENAME and are
recorded for copying; everything else falls back to a GitHub URL."
  (when (org-export-derived-backend-p backend 'html)
    (if (ores/proj--image-p path)
        (let ((base (file-name-nondirectory path)))
          (cl-pushnew (cons path base) ores/help-images :test #'equal)
          (format "<img src=\"images/%s\" alt=\"%s\"/>"
                  base
                  (or (and (stringp desc) (> (length desc) 0) desc) base)))
      (let ((url (ores/proj--github-url path)))
        (format "<a href=\"%s\">%s</a>" url (or desc url))))))

(org-link-set-parameters "proj" :export #'ores/help--proj-export)

;; Plain stylesheet: QTextBrowser supports a subset of CSS 2.1, so keep to
;; basic selectors — no custom properties, flexbox, or box-shadow.
(defvar ores/help-style "<style>
body { font-family: sans-serif; color: #222222; line-height: 1.5;
       margin: 1em 2em; max-width: 50em; }
h1, h2, h3, h4, h5 { color: #1a3a5a; font-weight: bold; }
h1 { font-size: 1.7em; }
h2 { font-size: 1.4em; border-bottom: 1px solid #cccccc; }
a { color: #1560b0; text-decoration: none; }
table { border-collapse: collapse; margin: 1em 0; }
th, td { border: 1px solid #aaaaaa; padding: 4px 9px; text-align: left; }
th { background-color: #eef2f6; }
pre { background-color: #f4f4f4; border: 1px solid #dddddd; padding: 8px;
      white-space: pre-wrap; }
code { background-color: #f4f4f4; }
blockquote { border-left: 3px solid #cccccc; margin-left: 0;
             padding-left: 1em; color: #555555; }
img { max-width: 100%; }
.figure { margin: 1em 0; }
.figure p { margin: 0; }
</style>")

(defun ores/build-help ()
  "Export the manual to self-contained Qt-help HTML."
  ;; Resolve id: links across the whole repo so export does not abort on
  ;; cross-document references (they point outside the manual; that is fine
  ;; for help — they just will not be live).
  (setq org-id-locations-file
        (expand-file-name "./.org-id-locations-file" ores/help-root))
  (org-id-update-id-locations
   (directory-files-recursively ores/help-root "\\.org$"))

  (make-directory (expand-file-name "images" ores/help-output-dir) t)

  ;; Plain, deterministic HTML for QTextBrowser.
  (let ((org-html-head ores/help-style)
        (org-html-head-include-default-style nil)
        (org-html-head-include-scripts nil)
        (org-html-htmlize-output-type nil)   ; no syntax-highlight spans
        (org-html-validation-link nil)
        (org-html-preamble nil)
        (org-html-postamble nil)
        (org-export-with-sub-superscripts nil)
        (org-export-use-babel nil)
        (org-export-with-toc t)
        (org-export-with-section-numbers t)
        (default-directory (file-name-directory ores/help-manual))
        (out (expand-file-name "user_manual.html" ores/help-output-dir)))
    (setq ores/help-images nil)
    (with-current-buffer (find-file-noselect ores/help-manual)
      (org-export-to-file 'html out))
    ;; Copy every referenced image next to the HTML.
    (let ((copied 0) (missing 0))
      (dolist (pair ores/help-images)
        (let ((src (expand-file-name (car pair) ores/help-root))
              (dst (expand-file-name (concat "images/" (cdr pair))
                                     ores/help-output-dir)))
          (if (file-readable-p src)
              (progn (copy-file src dst t) (setq copied (1+ copied)))
            (message "Warning: help image missing: %s" src)
            (setq missing (1+ missing)))))
      (message "ores-build-help: wrote %s (%d images copied, %d missing)"
               out copied missing))))

(condition-case err
    (progn
      (ores/build-help)
      (message "Help build succeeded.")
      (kill-emacs 0))
  (error
   (message "Help build failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide 'ores-build-help)
;;; ores-build-help.el ends here
