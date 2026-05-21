;;; ores-babel.el --- Org babel environment for ORE Studio recipes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>

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
;; Makes =#+begin_src sh= blocks in ORE Studio recipes "just work" when
;; you hit C-c C-c, no matter which doc/recipes/*.org file the recipe
;; lives in.
;;
;; What it does:
;;   - Sets the default cwd for shell babel blocks to the repo root, so
;;     paths like `./projects/ores.codegen/doc_list.sh` resolve
;;     consistently no matter where the org file lives in the tree.
;;   - Sets the default :results to verbatim so multi-line shell output
;;     renders cleanly in #+RESULTS: blocks.
;;
;; Public API:
;;
;;   `ores/repo-root'  -> absolute path of the repo root
;;   `ores/build-dir'  -> absolute path of build/
;;   `ores/bin'        -> path of NAME inside the most recently built bin/
;;
;; Loaded by `.dir-locals.el' at the repo root; runs an org-mode-hook
;; that applies the per-buffer defaults.

;;; Code:

(require 'project)

(defun ores/repo-root ()
  "Return the absolute path of the ORE Studio repo root.

Uses `project-current' (the same mechanism `ores-env.el' relies on),
so it picks up the marker that identifies the project (typically
.git or .dir-locals.el)."
  (when-let* ((proj (project-current nil)))
    (file-name-as-directory (expand-file-name (project-root proj)))))

(defun ores/build-dir ()
  "Return the absolute path of the ORE Studio build directory."
  (when-let* ((root (ores/repo-root)))
    (expand-file-name "build/" root)))

(defun ores/bin (name)
  "Return the absolute path of NAME inside the most-recent build's bin dir.

Searches under =build/output/<preset>/bin/= for the preset whose bin
directory was modified most recently, then appends NAME. Returns nil
if no bin directory exists yet (e.g. you have not built)."
  (when-let* ((root (ores/repo-root))
              (bins (file-expand-wildcards
                     (expand-file-name "build/output/*/bin/" root))))
    (let ((latest (car (sort bins
                             (lambda (a b)
                               (time-less-p
                                (file-attribute-modification-time
                                 (file-attributes b))
                                (file-attribute-modification-time
                                 (file-attributes a))))))))
      (expand-file-name name latest))))

(defun ores/-apply-babel-defaults ()
  "Apply the ORE Studio babel defaults to the current org buffer."
  (when (derived-mode-p 'org-mode)
    (when-let* ((root (ores/repo-root)))
      (setq-local org-babel-default-header-args:sh
                  `((:dir     . ,root)
                    (:results . "verbatim"))))))

(add-hook 'org-mode-hook #'ores/-apply-babel-defaults)

;; Apply immediately when this file is loaded inside an already-open
;; org buffer (the .dir-locals.el path).
(ores/-apply-babel-defaults)

(provide 'ores-babel)
;;; ores-babel.el ends here
