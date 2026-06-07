;;; ores-build-shell-scripts.el --- -*- lexical-binding: t; -*-

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

;; Tangles the ores-shell script library: every .org file under
;; projects/ores.shell/scripts/ (except the README) is a literate
;; script whose ores-shell src blocks carry :tangle targets naming the
;; sibling .ores artefacts consumed by the shell's load command.

;;; Code:
(require 'org)
(require 'ob-core)
(require 'project)

(setq debug-on-error nil)
(setq debug-on-quit nil)

(setq org-id-locations-file (expand-file-name "./.org-id-locations-file"))
(setq package-user-dir (expand-file-name "./.packages"))

;; Load the ORE Studio babel environment for ores/repo-root.
(load-file (expand-file-name "projects/ores.lisp/src/ores-babel.el"))

;; Resolve the project root.
(defvar ores/--shell-scripts-root
  (or (ores/repo-root)
      (file-name-as-directory (expand-file-name ".")))
  "Absolute path to the project root for this tangle run.")

(defvar ores/--shell-scripts-dir
  (expand-file-name "projects/ores.shell/scripts/"
                    ores/--shell-scripts-root)
  "Directory holding the literate scripts and their tangled artefacts.")

(condition-case err
    (let ((docs (seq-remove
                 (lambda (doc)
                   (string= (file-name-nondirectory doc) "README.org"))
                 (directory-files ores/--shell-scripts-dir t "\\.org\\'"))))
      (unless docs
        (error "No literate scripts found in %s" ores/--shell-scripts-dir))
      (dolist (doc docs)
        (org-babel-tangle-file doc)
        (message "Tangled %s" (file-name-nondirectory doc)))
      (message "Tangled %d script(s) in %s"
               (length docs) ores/--shell-scripts-dir))
  (error
   (message "shell scripts tangle failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide 'ores-build-shell-scripts)
;;; ores-build-shell-scripts.el ends here
