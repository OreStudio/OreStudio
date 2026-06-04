;;; ores-build-clang-format.el --- -*- lexical-binding: t; -*-

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

;; Tangles doc/knowledge/architecture/clang_format_config.org to .clang-format.
;; All #+begin_src yaml blocks in the document are concatenated in order and
;; written to .clang-format at the project root.

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
(defvar ores/--clang-format-root
  (or (ores/repo-root)
      (file-name-as-directory (expand-file-name ".")))
  "Absolute path to the project root for this tangle run.")

;; Inject the tangle target into org-babel's yaml defaults so the src blocks
;; in clang_format_config.org need no explicit :tangle directive.
(setq org-babel-default-header-args:yaml
      `((:tangle . ,(expand-file-name ".clang-format"
                                      ores/--clang-format-root))))

(condition-case err
    (progn
      (org-babel-tangle-file
       (expand-file-name "doc/knowledge/architecture/clang_format_config.org"
                         ores/--clang-format-root))
      (message "Tangled .clang-format to %s"
               (expand-file-name ".clang-format" ores/--clang-format-root)))
  (error
   (message "clang-format tangle failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide 'ores-build-clang-format)
;;; ores-build-clang-format.el ends here
