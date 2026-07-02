;;; ores-build-release-notes.el --- Export a sprint's release_notes.org to GitHub-flavoured Markdown. -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Keywords: publish

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Batch-exports <sprint-dir>/release_notes.org to
;; <sprint-dir>/release_notes.md using GitHub-flavoured Markdown
;; (ox-gfm), for use as the body of a `gh release create --notes-file`.
;;
;; Usage:
;;   emacs -Q --script projects/ores.lisp/src/ores-build-release-notes.el -- <sprint-dir>
;;
;; <sprint-dir> is repo-root-relative, e.g. doc/agile/versions/v0/sprint_21.
;;
;; House settings: no table of contents, no section numbers (matches
;; ores-build-skills.el). `proj:' links (see ores-link-types.el) are
;; resolved locally rather than via that file, because it has no gfm/md
;; branch: images become raw.githubusercontent.com URLs (so they render
;; inline in a GitHub release body); everything else becomes a
;; github.com/.../blob/main/... URL.
;;
;;; Code:
(require 'package)
(require 'org)
(require 'ox-publish)

(setq debug-on-error nil)
(setq debug-on-quit nil)

(defvar ores/release-notes-repo-root
  (expand-file-name default-directory)
  "Repository root — batch scripts run with CMAKE_SOURCE_DIR as cwd.")

(defvar ores/release-notes-github-user "OreStudio")
(defvar ores/release-notes-github-repo "OreStudio")
(defvar ores/release-notes-github-branch "main")

(defun ores/release-notes--image-p (path)
  (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|svg\\)$" path))

(defun ores/release-notes--raw-url (path)
  (format "https://raw.githubusercontent.com/%s/%s/%s/%s"
          ores/release-notes-github-user ores/release-notes-github-repo
          ores/release-notes-github-branch path))

(defun ores/release-notes--blob-url (path)
  (format "https://github.com/%s/%s/blob/%s/%s"
          ores/release-notes-github-user ores/release-notes-github-repo
          ores/release-notes-github-branch path))

(with-eval-after-load 'ol
  (org-link-set-parameters
   "proj"
   :export (lambda (path desc _format)
             (if (ores/release-notes--image-p path)
                 (format "![%s](%s)" (or desc (file-name-nondirectory path))
                         (ores/release-notes--raw-url path))
               (format "[%s](%s)" (or desc path)
                       (ores/release-notes--blob-url path))))))

;; Suppress the TOC even if #+options: toc:t is set in-buffer — in-buffer
;; options take precedence over project/explicit :with-toc settings.
(add-hook 'org-export-before-processing-hook
          (lambda (backend)
            (when (eq backend 'gfm)
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "\\(^#\\+options:.*\\)\\btoc:t\\b" nil t)
                  (replace-match "\\1toc:nil"))))))

(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install 'ox-gfm)
(require 'ox-gfm)

(let* ((args (if (equal (car command-line-args-left) "--")
                  (cdr command-line-args-left)
                command-line-args-left))
       (sprint-dir-rel (car args)))
  (unless sprint-dir-rel
    (message "Usage: emacs -Q --script ores-build-release-notes.el -- <sprint-dir>")
    (kill-emacs 1))
  (let* ((sprint-dir (expand-file-name sprint-dir-rel ores/release-notes-repo-root))
         (org-file (expand-file-name "release_notes.org" sprint-dir)))
    (unless (file-exists-p org-file)
      (message "release_notes.org not found: %s" org-file)
      (kill-emacs 1))
    (condition-case err
        (with-current-buffer (find-file-noselect org-file)
          (let ((org-export-with-toc nil)
                (org-export-with-section-numbers nil)
                (org-export-with-author nil)
                (org-export-with-sub-superscripts nil)
                (org-export-with-broken-links 'mark))
            (org-gfm-export-to-markdown))
          (message "Wrote %s"
                   (expand-file-name "release_notes.md" sprint-dir)))
      (error
       (message "Release notes export failed: %s" (error-message-string err))
       (kill-emacs 1)))))

(provide 'ores-build-release-notes)
;;; ores-build-release-notes.el ends here
