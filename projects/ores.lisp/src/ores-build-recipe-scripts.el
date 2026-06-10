;;; ores-build-recipe-scripts.el --- -*- lexical-binding: t; -*-

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

;; Generates the ores-shell script library from the shell recipes.  Each
;; recipe under doc/recipes/shell/ is the single source of truth for one
;; piece of shell usage; its ores-shell src block(s) are tangled into a
;; runnable .ores artefact in the library folder
;; (projects/ores.shell/scripts/library/).  The recipe stays the thing a
;; human edits; the .ores is generated, carries a "do not edit" header,
;; and is committed so the Qt script panel and `compass shell -f' can
;; load it directly.
;;
;; The output lands "elsewhere" from the source (in the library folder,
;; not next to the recipe): org-babel-tangle-file's TARGET-FILE argument
;; redirects every ores-shell block in a recipe to the chosen path, and
;; the LANG-RE argument keeps the `sh' runner blocks out.
;;
;; One recipe == one script: a recipe is expected to carry exactly one
;; ores-shell block. Because TARGET-FILE redirects every matching block
;; to the same file, a recipe with two ores-shell blocks would have them
;; *concatenated* into one .ores (in document order). Recipes with no
;; ores-shell block at all (e.g. the inventory index) are skipped, so no
;; empty category folder is created for them.

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

(defvar ores/--recipe-scripts-root
  (or (ores/repo-root)
      (file-name-as-directory (expand-file-name ".")))
  "Absolute path to the project root for this tangle run.")

(defvar ores/--recipe-scripts-source-dir
  (expand-file-name "doc/recipes/shell/" ores/--recipe-scripts-root)
  "Directory holding the shell recipes — the single source of truth.")

(defvar ores/--recipe-scripts-library-dir
  (expand-file-name "projects/ores.shell/scripts/library/"
                    ores/--recipe-scripts-root)
  "Library folder receiving the generated .ores artefacts.")

(defun ores/--recipe-keyword (recipe-file keyword)
  "Return the value of #+KEYWORD: in RECIPE-FILE, or nil."
  (with-temp-buffer
    (insert-file-contents recipe-file)
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^#\\+" (regexp-quote keyword) ":[ \t]*\\(.*\\)$") nil t)
      (string-trim (match-string 1)))))

(defun ores/--recipe-has-shell-block-p (recipe-file)
  "Non-nil if RECIPE-FILE contains at least one ores-shell src block.

Used to skip prose-only recipes (e.g. the inventory index) so no empty
category folder is created for them."
  (with-temp-buffer
    (insert-file-contents recipe-file)
    (goto-char (point-min))
    (re-search-forward "^[ \t]*#\\+begin_src[ \t]+ores-shell\\b" nil t)))

(defun ores/--recipe-category (recipe-file)
  "Folder a RECIPE-FILE's script belongs in: its category filetag.

The recipe filetags read =:recipe:shell:<category>:...=; the third
component groups scripts (accounts, provisioning, tenants, …) into
sub-folders of the library, matching the recipe inventory.  Recipes
without a category land in =general/=."
  (let ((tags (ores/--recipe-keyword recipe-file "filetags")))
    (if (and tags (string-match ":recipe:shell:\\([^:]+\\):" tags))
        (match-string 1 tags)
      "general")))

(defun ores/--recipe-script-header (recipe-file)
  "Return the self-documenting banner for RECIPE-FILE, in ores-shell comments.

Leads with the recipe's title and description so a reader of the script
sees what it does, then the generated-file warning.  These are =#=
comment lines, which the shell's load command skips."
  (let* ((rel (file-relative-name recipe-file ores/--recipe-scripts-root))
         (title (ores/--recipe-keyword recipe-file "title"))
         (desc (ores/--recipe-keyword recipe-file "description"))
         ;; Many legacy recipes set description = title; don't print it twice.
         (desc (and desc (not (string-empty-p desc))
                    (not (equal desc title)) desc)))
    (concat
     (when (and title (not (string-empty-p title))) (concat "# " title "\n"))
     (when desc (concat "# " desc "\n"))
     "#\n"
     "# GENERATED from " rel " — do not edit by hand.\n"
     "# Regenerate with: cmake --build <preset> --target tangle_shell_scripts\n"
     "#\n")))

(defun ores/--strip-trailing-exit (script-file)
  "Remove a trailing =exit= line from SCRIPT-FILE in place.

Recipes end their ores-shell block with =exit= so org-babel's REPL
terminates when the recipe is executed in Emacs.  A library script is
run via the shell's load command (or the Qt panel), where =exit= would
close the whole shell — so it is dropped from the generated artefact."
  (with-temp-buffer
    (insert-file-contents script-file)
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (delete-region (point) (point-max))
    (beginning-of-line)
    (when (looking-at "[ \t]*exit[ \t]*$")
      (delete-region (point) (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max)))
    (goto-char (point-max))
    (insert "\n")
    (write-region (point-min) (point-max) script-file)))

(defun ores/--prepend-generated-header (script-file recipe-file)
  "Prepend the self-documenting banner for RECIPE-FILE to SCRIPT-FILE."
  (with-temp-buffer
    (insert (ores/--recipe-script-header recipe-file))
    (insert-file-contents script-file)
    (write-region (point-min) (point-max) script-file)))

(condition-case err
    (progn
      (make-directory ores/--recipe-scripts-library-dir t)
      ;; Recipes are partitioned into category sub-folders that mirror the
      ;; library, so recurse; the category for each script is still taken
      ;; from its filetag, not its directory.
      (let ((recipes (directory-files-recursively
                      ores/--recipe-scripts-source-dir "\\.org\\'"))
            (generated 0))
        (unless recipes
          (error "No shell recipes found in %s"
                 ores/--recipe-scripts-source-dir))
        (dolist (recipe recipes)
          ;; Only recipes with an ores-shell block yield a script; this
          ;; also keeps empty category folders from being created.
          (when (ores/--recipe-has-shell-block-p recipe)
            (let* ((category (ores/--recipe-category recipe))
                   (dir (expand-file-name category
                                          ores/--recipe-scripts-library-dir))
                   (target (expand-file-name
                            (concat (file-name-base recipe) ".ores") dir)))
              (make-directory dir t)
              ;; TARGET-FILE redirects every block to the category folder;
              ;; LANG-RE "ores-shell" keeps the sh runner blocks out.
              (org-babel-tangle-file recipe target "ores-shell")
              (when (file-exists-p target)
                (ores/--strip-trailing-exit target)
                (ores/--prepend-generated-header target recipe)
                (setq generated (1+ generated))
                (message "Generated %s/%s" category
                         (file-name-nondirectory target))))))
        (message "Generated %d script(s) in %s"
                 generated ores/--recipe-scripts-library-dir)))
  (error
   (message "recipe scripts tangle failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide 'ores-build-recipe-scripts)
;;; ores-build-recipe-scripts.el ends here
