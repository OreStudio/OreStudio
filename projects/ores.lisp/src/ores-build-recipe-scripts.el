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

(defun ores/--recipe-script-header (recipe-file)
  "Return the generated-file banner for RECIPE-FILE, in ores-shell comments."
  (let ((rel (file-relative-name recipe-file ores/--recipe-scripts-root)))
    (concat
     "# GENERATED from " rel " — do not edit by hand.\n"
     "# Regenerate with: cmake --build <preset> --target tangle_shell_scripts\n"
     "#\n")))

(defun ores/--prepend-generated-header (script-file recipe-file)
  "Prepend the generated banner for RECIPE-FILE to SCRIPT-FILE in place."
  (with-temp-buffer
    (insert (ores/--recipe-script-header recipe-file))
    (insert-file-contents script-file)
    (write-region (point-min) (point-max) script-file)))

(condition-case err
    (progn
      (make-directory ores/--recipe-scripts-library-dir t)
      (let ((recipes (directory-files
                      ores/--recipe-scripts-source-dir t "\\.org\\'"))
            (generated 0))
        (unless recipes
          (error "No shell recipes found in %s"
                 ores/--recipe-scripts-source-dir))
        (dolist (recipe recipes)
          (let ((target (expand-file-name
                         (concat (file-name-base recipe) ".ores")
                         ores/--recipe-scripts-library-dir)))
            ;; TARGET-FILE redirects every block to the library folder;
            ;; LANG-RE "ores-shell" keeps the sh runner blocks out. The
            ;; file is only written when the recipe has an ores-shell
            ;; block, so prose-only recipes are skipped silently.
            (org-babel-tangle-file recipe target "ores-shell")
            (when (file-exists-p target)
              (ores/--prepend-generated-header target recipe)
              (setq generated (1+ generated))
              (message "Generated %s" (file-name-nondirectory target)))))
        (message "Generated %d script(s) in %s"
                 generated ores/--recipe-scripts-library-dir)))
  (error
   (message "recipe scripts tangle failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide 'ores-build-recipe-scripts)
;;; ores-build-recipe-scripts.el ends here
