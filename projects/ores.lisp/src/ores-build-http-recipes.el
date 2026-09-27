;;; ores-build-http-recipes.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Keywords: publish

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generates the Hurl library from the HTTP recipes.  Each recipe under
;; doc/recipes/http/ is the single source of truth for one resource's HTTP
;; surface; its hurl src block(s) are tangled into a runnable .hurl artefact
;; in the library folder (projects/ores.http/scripts/library/).  The recipe
;; stays the thing a human reads and the generator writes; the .hurl is
;; generated, carries a "do not edit" header, and is committed so the harness
;; can run it without a build step.
;;
;; This is the HTTP twin of ores-build-recipe-scripts.el, and differs from it
;; in two ways.  The language is `hurl' rather than `ores-shell', and there is
;; nothing to strip: a shell recipe ends its block with `exit' so an interactive
;; run terminates, and a Hurl file has no such line.
;;
;; Every generated recipe names a target per block, because one recipe
;; documents one resource and exports one file per endpoint.  With no
;; TARGET-FILE the tangle honours each block's own name, and this script moves
;; what it produced into the library, so the directory stays the build's
;; business and the recipe states only a name.

;;; Code:
(require 'org)
(require 'ob-core)
(require 'ob-tangle)

(setq debug-on-error nil)
(setq debug-on-quit nil)

(setq org-id-locations-file (expand-file-name "./.org-id-locations-file"))
(setq package-user-dir (expand-file-name "./.packages"))

;; Load the ORE Studio babel environment for ores/repo-root.
(load-file (expand-file-name "projects/ores.lisp/src/ores-babel.el"))

(defvar ores/--http-recipes-root
  (or (ores/repo-root)
      (file-name-as-directory (expand-file-name ".")))
  "Absolute path to the project root for this tangle run.")

(defvar ores/--http-recipes-source-dir
  (expand-file-name "doc/recipes/http/" ores/--http-recipes-root)
  "Directory holding the HTTP recipes — the single source of truth.")

(defvar ores/--http-recipes-library-dir
  (expand-file-name "projects/ores.http/scripts/library/"
                    ores/--http-recipes-root)
  "Library folder receiving the generated .hurl artefacts.")

(defun ores/--http-recipe-keyword (recipe-file keyword)
  "Return the value of #+KEYWORD: in RECIPE-FILE, or nil."
  (with-temp-buffer
    (insert-file-contents recipe-file)
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^#\\+" (regexp-quote keyword) ":[ \t]*\\(.*\\)$") nil t)
      (string-trim (match-string 1)))))

(defun ores/--http-recipe-has-block-p (recipe-file)
  "Non-nil if RECIPE-FILE contains at least one hurl src block.

Used to skip the hand-written prose recipes and the inventory index, so no
empty category folder is created for them."
  (with-temp-buffer
    (insert-file-contents recipe-file)
    (goto-char (point-min))
    (re-search-forward "^[ \t]*#\\+begin_src[ \t]+hurl\\b" nil t)))

(defun ores/--http-recipe-category (recipe-file)
  "Folder a RECIPE-FILE's requests belong in: its category filetag.

The recipe filetags read =:recipe:http:<category>:...=; the third component
groups requests (accounts, tags, …) into sub-folders of the library,
matching the recipe inventory.  Recipes without a category land in
=general/=."
  (let ((tags (ores/--http-recipe-keyword recipe-file "filetags")))
    (if (and tags (string-match ":recipe:http:\\([^:]+\\):" tags))
        (match-string 1 tags)
      "general")))

(defun ores/--http-recipe-block-tangles (recipe-file)
  "The :tangle targets RECIPE-FILE's hurl blocks declare."
  (with-temp-buffer
    (insert-file-contents recipe-file)
    (goto-char (point-min))
    (let (targets)
      (while (re-search-forward
              "^[ \t]*#\\+begin_src[ \t]+hurl\\b[^\n]*" nil t)
        (let ((header (match-string 0)))
          (when (string-match ":tangle[ \t]+\\([^ \t\n]+\\)" header)
            (push (match-string 1 header) targets))))
      (nreverse targets))))

(defun ores/--http-recipe-section-title (recipe-file target)
  "The heading of the section in RECIPE-FILE whose block tangles to TARGET.

A per-block request exports one section, so the section's own heading is what
tells a reader of the file which endpoint it calls."
  (with-temp-buffer
    (insert-file-contents recipe-file)
    (goto-char (point-min))
    (let ((heading nil)
          (found nil))
      (while (and (not found) (not (eobp)))
        (cond
         ((looking-at "^\\*+ +\\(.*\\)[ \t]*$")
          (setq heading (string-trim (match-string 1))))
         ((looking-at "^[ \t]*#\\+begin_src[ \t]+hurl\\b")
          (let* ((line (buffer-substring (line-beginning-position)
                                         (line-end-position)))
                 (at (string-match ":tangle[ \t]+\\([^ \t\n]+\\)" line)))
            (when (and at (equal (match-string 1 line) target))
              (setq found heading)))))
        (forward-line 1))
      (or found ""))))

(defun ores/--http-recipe-header (recipe-file section)
  "Return the self-documenting banner for RECIPE-FILE, in Hurl comments.

Leads with the section's heading so a reader of the file sees which endpoint
it calls, then the generated-file warning.  These are =#= comment lines, which
Hurl skips."
  (let* ((rel (file-relative-name recipe-file ores/--http-recipes-root))
         (title (ores/--http-recipe-keyword recipe-file "title")))
    (concat
     (when (and section (not (string-empty-p section)))
       (concat "# " section "\n"))
     (when (and (not (and section (not (string-empty-p section))))
                title (not (string-empty-p title)))
       (concat "# " title "\n"))
     "#\n"
     "# GENERATED from " rel " — do not edit by hand.\n"
     "# Regenerate with: ./compass.sh build --direct tangle_http_recipes\n"
     "#\n")))

(defun ores/--http-prepend-generated-header (script-file recipe-file section)
  "Prepend the self-documenting banner for RECIPE-FILE to SCRIPT-FILE."
  (with-temp-buffer
    (insert (ores/--http-recipe-header recipe-file section))
    (insert-file-contents script-file)
    (write-region (point-min) (point-max) script-file)))

(condition-case err
    (progn
      (make-directory ores/--http-recipes-library-dir t)
      (let ((recipes (directory-files-recursively
                      ores/--http-recipes-source-dir "\\.org\\'"))
            (generated 0))
        (dolist (recipe recipes)
          ;; Only recipes with a hurl block yield a file; this also keeps empty
          ;; category folders from being created.
          (when (ores/--http-recipe-has-block-p recipe)
            (let* ((category (ores/--http-recipe-category recipe))
                   (dir (expand-file-name category
                                          ores/--http-recipes-library-dir))
                   (block-tangles (ores/--http-recipe-block-tangles recipe)))
              (make-directory dir t)
              (if (null block-tangles)
                  (error "HTTP recipe names no :tangle target: %s" recipe)
                (org-babel-tangle-file recipe nil "hurl")
                (dolist (raw block-tangles)
                  (let* ((produced (expand-file-name
                                    raw (file-name-directory recipe)))
                         (dest (expand-file-name
                                (file-name-nondirectory raw) dir)))
                    (when (file-exists-p produced)
                      (ores/--http-prepend-generated-header
                       produced recipe
                       (ores/--http-recipe-section-title recipe raw))
                      (rename-file produced dest t)
                      (setq generated (1+ generated))
                      (message "Generated %s/%s" category
                               (file-name-nondirectory dest)))))))))
        (message "Generated %d file(s) in %s"
                 generated ores/--http-recipes-library-dir)))
  (error
   (message "HTTP recipes tangle failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide 'ores-build-http-recipes)
;;; ores-build-http-recipes.el ends here
