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
;; and is committed so `compass shell -f' can
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

(defun ores/--recipe-block-tangles (recipe-file)
  "The :tangle targets RECIPE-FILE's ores-shell blocks declare.

Empty for the recipes that state the whole document as one script, which
take the TARGET-FILE path instead.  A recipe that names a target per
block is a literate document: one section per command, each section
exporting its own script, so the target is the script's name and this
script owns its directory."
  (with-temp-buffer
    (insert-file-contents recipe-file)
    (goto-char (point-min))
    (let (targets)
      (while (re-search-forward
              "^[ \t]*#\\+begin_src[ \t]+ores-shell\\b[^\n]*" nil t)
        (let ((header (match-string 0)))
          (when (string-match ":tangle[ \t]+\\([^ \t\n]+\\)" header)
            (push (match-string 1 header) targets))))
      (nreverse targets))))

(defun ores/--recipe-section-title (recipe-file target)
  "The heading of the section in RECIPE-FILE whose block tangles to TARGET.

A per-block script exports one section, so the section's own heading is
what tells a reader of the script which command it runs."
  (with-temp-buffer
    (insert-file-contents recipe-file)
    (goto-char (point-min))
    (let ((heading nil)
          (found nil))
      (while (and (not found) (not (eobp)))
        (cond
         ((looking-at "^\\*+ +\\(.*\\)[ \t]*$")
          (setq heading (string-trim (match-string 1))))
         ((looking-at "^[ \t]*#\\+begin_src[ \t]+ores-shell\\b")
          ;; The match data is read back against the same string, so the
          ;; heading scan above cannot be mistaken for the block's target.
          (let* ((line (buffer-substring (line-beginning-position)
                                         (line-end-position)))
                 (at (string-match ":tangle[ \t]+\\([^ \t\n]+\\)" line)))
            (when (and at (equal (match-string 1 line) target))
              (setq found heading)))))
        (forward-line 1))
      (or found ""))))

(defun ores/--recipe-script-header (recipe-file &optional section)
  "Return the self-documenting banner for RECIPE-FILE, in ores-shell comments.

Leads with the recipe's title and description so a reader of the script
sees what it does, then the generated-file warning.  These are =#=
comment lines, which the shell's load command skips.  SECTION names the
heading a per-block script exports, which leads the banner in place of a
description the recipe already used for the whole document."
  (let* ((rel (file-relative-name recipe-file ores/--recipe-scripts-root))
         (title (ores/--recipe-keyword recipe-file "title"))
         (desc (ores/--recipe-keyword recipe-file "description"))
         ;; Many legacy recipes set description = title; don't print it twice.
         (desc (and desc (not (string-empty-p desc))
                    (not (equal desc title)) desc)))
    (concat
     (cond
      ((and section (not (string-empty-p section)))
       (concat "# " section "\n"))
      ((and title (not (string-empty-p title)))
       (concat "# " title "\n")))
     (when (and (not section) desc) (concat "# " desc "\n"))
     "#\n"
     "# GENERATED from " rel " — do not edit by hand.\n"
     "# Regenerate with: ./compass.sh build --direct tangle_shell_scripts\n"
     "#\n")))

(defun ores/--strip-trailing-exit (script-file)
  "Remove a trailing =exit= line from SCRIPT-FILE in place.

Recipes end their ores-shell block with =exit= so org-babel's REPL
terminates when the recipe is executed in Emacs.  A library script is
run via the shell's load command, where =exit= would
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

(defun ores/--prepend-generated-header (script-file recipe-file &optional section)
  "Prepend the self-documenting banner for RECIPE-FILE to SCRIPT-FILE.

SECTION names the heading a per-block script exports, when the script is
one section of a literate recipe rather than the whole document."
  (with-temp-buffer
    (insert (ores/--recipe-script-header recipe-file section))
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
                   (block-tangles (ores/--recipe-block-tangles recipe)))
              (make-directory dir t)
              (if block-tangles
                  ;; A literate recipe names the script each section exports.
                  ;; With no TARGET-FILE the tangle honours those names, and
                  ;; this script moves what it produced into the library, so
                  ;; the directory stays the build's business and the recipe
                  ;; states only a name. Without TARGET-FILE a block that
                  ;; declares no :tangle is skipped, which is why the two
                  ;; modes are exclusive rather than layered.
                  (progn
                    (org-babel-tangle-file recipe nil "ores-shell")
                    (dolist (raw block-tangles)
                      (let* ((produced (expand-file-name
                                        raw (file-name-directory recipe)))
                             (dest (expand-file-name
                                    (file-name-nondirectory raw) dir)))
                        (when (file-exists-p produced)
                          (ores/--strip-trailing-exit produced)
                          (ores/--prepend-generated-header
                           produced recipe
                           (ores/--recipe-section-title recipe raw))
                          (rename-file produced dest t)
                          (setq generated (1+ generated))
                          (message "Generated %s/%s" category
                                   (file-name-nondirectory dest))))))
                ;; TARGET-FILE redirects every block to one file in the
                ;; category folder; LANG-RE "ores-shell" keeps the sh runner
                ;; blocks out.
                (let ((target (expand-file-name
                               (concat (file-name-base recipe) ".ores") dir)))
                  (org-babel-tangle-file recipe target "ores-shell")
                  (when (file-exists-p target)
                    (ores/--strip-trailing-exit target)
                    (ores/--prepend-generated-header target recipe)
                    (setq generated (1+ generated))
                    (message "Generated %s/%s" category
                             (file-name-nondirectory target))))))))
        (message "Generated %d script(s) in %s"
                 generated ores/--recipe-scripts-library-dir)))
  (error
   (message "recipe scripts tangle failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide 'ores-build-recipe-scripts)
;;; ores-build-recipe-scripts.el ends here
