;;; ores-link-types.el --- Custom org-mode link types for ORE Studio. -*- lexical-binding: t; -*-
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
;; Defines the `proj:' org-mode link type, which maps a repository-root-relative
;; path to the correct output for each export backend:
;;
;;   HTML  — images: <img src="/OreStudio/path">; other: <a href="GitHub blob URL">
;;   LaTeX — images: delegates to org-latex--inline-image (full figure/caption
;;           support) via path resolution advice; other: \href{GitHub URL}{desc}
;;   Emacs — :follow opens the file; org-display-inline-images displays images
;;           inline via after-advice
;;
;; The `proj:' type replaces relative file: image paths in manual chapters and
;; documentation, removing the need for ../../../ prefix chains.
;;
;;; Code:
(require 'org)

(defvar ores/repo-root
  (cond
   (load-file-name
    (expand-file-name "../../../"
                      (file-name-directory load-file-name)))
   ((and (fboundp 'project-current)
         (project-current nil))
    (project-root (project-current nil)))
   (t default-directory))
  "Repository root, captured when ores-link-types is loaded.")

(defvar ores/proj-github-user "OreStudio"
  "GitHub user or organisation for proj: GitHub URL export.")

(defvar ores/proj-github-repo "OreStudio"
  "GitHub repository name for proj: GitHub URL export.")

(defvar ores/proj-github-branch "main"
  "Default branch for proj: GitHub blob URLs.")

(defvar ores/proj-site-root "/OreStudio/"
  "Absolute URL prefix for the published site (trailing slash required).")

(defun ores/proj--github-url (path)
  "Return the GitHub blob URL for PATH (relative to repo root)."
  (format "https://github.com/%s/%s/blob/%s/%s"
          ores/proj-github-user ores/proj-github-repo
          ores/proj-github-branch path))

(defun ores/proj--image-p (path)
  "Return non-nil if PATH is an image file by extension."
  (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|svg\\)$" path))

;; Register proj: as an inline image type so that:
;; - org-html-paragraph wraps proj: images in <figure> with caption.
;; - org-latex-link takes the `imagep' path (org-latex--inline-image)
;;   rather than the custom-protocol path, giving full figure support.
(add-to-list 'org-html-inline-image-rules
             (cons "proj" "\\.\\(?:png\\|jpe?g\\|gif\\|svg\\)$"))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-inline-image-rules
               (cons "proj" "\\.\\(?:png\\|jpe?g\\|gif\\)$")))

(org-link-set-parameters
 "proj"
 :export
 (lambda (path desc backend _info)
   (cond
    ;; HTML: emit <img> for images so org-html-paragraph wraps it in
    ;; <figure>; emit <a href> blob URL for everything else.
    ((org-export-derived-backend-p backend 'html)
     (if (ores/proj--image-p path)
         (format "<img src=\"%s%s\" alt=\"%s\"/>"
                 ores/proj-site-root path
                 (or (and (stringp desc) (> (length desc) 0) desc)
                     (file-name-nondirectory path))))
       (let ((url (ores/proj--github-url path)))
         (format "<a href=\"%s\">%s</a>" url (or desc url)))))
    ;; LaTeX: return nil for images so org-latex-link falls through to
    ;; the imagep branch and calls org-latex--inline-image (which handles
    ;; figure, marginfigure, caption, and width correctly).  For non-image
    ;; links emit a hyperref URL.
    ((org-export-derived-backend-p backend 'latex)
     (unless (ores/proj--image-p path)
       (format "\\href{%s}{%s}"
               (ores/proj--github-url path)
               (or desc path))))))
 :follow
 (lambda (path _arg)
   (let ((abs (expand-file-name path ores/repo-root)))
     (if (file-exists-p abs)
         (find-file abs)
       (browse-url (ores/proj--github-url path))))))

;; Advice: resolve proj: paths to absolute file: paths before
;; org-latex--inline-image runs.
;;
;; We mutate the ORIGINAL link object in-place rather than copying it so
;; that (eq link node) inside org-latex--inline-image's float-determination
;; code succeeds.  That check skips the original link when mapping over the
;; parent's contents, allowing float to reach the #+caption clause and
;; correctly become 'figure.  unwind-protect restores the original values.
(defun ores/latex--inline-image-resolve-proj (orig-fun link info)
  "Resolve `proj:' paths to absolute paths for LaTeX image export."
  (if (string= (org-element-property :type link) "proj")
      (let* ((orig-path (org-element-property :path link))
             (abs-path  (expand-file-name orig-path ores/repo-root)))
        (org-element-put-property link :path abs-path)
        (org-element-put-property link :type "file")
        (unwind-protect
            (funcall orig-fun link info)
          (org-element-put-property link :path orig-path)
          (org-element-put-property link :type "proj")))
    (funcall orig-fun link info)))

(with-eval-after-load 'ox-latex
  (advice-add 'org-latex--inline-image
              :around #'ores/latex--inline-image-resolve-proj))

;; Advice: display proj: image links as inline images in org buffers.
;; Emacs 30.2's org-display-inline-images only handles file: and
;; attachment: links; this after-advice handles proj: by resolving paths
;; against ores/repo-root and creating overlays in the same way.
(defun ores/display-proj-inline-images (&optional _include-linked _refresh beg end)
  "Display `proj:' image links as inline images."
  (when (display-graphic-p)
    (let ((end (or end (point-max)))
          (file-extension-re (image-file-name-regexp)))
      (org-with-point-at (or beg (point-min))
        (while (re-search-forward "\\[\\[proj:\\([^]]+\\)\\]" end t)
          (let* ((path (match-string 1))
                 (link (org-element-lineage
                        (save-match-data (org-element-context))
                        '(link) t)))
            (when (and link
                       (string= (org-element-property :type link) "proj")
                       (string-match-p file-extension-re path))
              (let* ((abs (expand-file-name path ores/repo-root))
                     (width (org-display-inline-image--width link)))
                (when (file-readable-p abs)
                  (let ((image (org--create-inline-image abs width)))
                    (when image
                      (let ((ov (make-overlay
                                 (org-element-begin link)
                                 (progn
                                   (goto-char (org-element-end link))
                                   (unless (eolp) (skip-chars-backward " \t"))
                                   (point)))))
                        (image-flush image)
                        (overlay-put ov 'display image)
                        (overlay-put ov 'face 'default)
                        (overlay-put ov 'org-image-overlay t)
                        (overlay-put ov 'modification-hooks
                                     (list 'org-display-inline-remove-overlay))
                        (when (boundp 'image-map)
                          (overlay-put ov 'keymap image-map))
                        (push ov org-inline-image-overlays)))))))))))))

(advice-add 'org-display-inline-images
            :after #'ores/display-proj-inline-images)

(provide 'ores-link-types)
;;; ores-link-types.el ends here
