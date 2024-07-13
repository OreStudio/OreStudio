;;; setup-prodigy.el --- Sets up prodigy for this project.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Keywords: local

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
;;; Code:
(defun ores/path-to-ui ()
  "Return the path to the qt user interface directory."
  (let* ((pr (project-current t))
         (root (project-root pr))
         (path (concat root "build/output/linux-clang-release/projects/ores.qt/")))
    path))

(setq prodigy-services nil)
(prodigy-define-tag :name 'ores)
(prodigy-define-tag :name 'ui)

(prodigy-define-service
  :name "ORE Studio Qt."
  :cwd (ores/path-to-ui)
  :command (concat (ores/path-to-ui) "ores.qt")
  :tags '(ores ui)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(provide 'setup-prodigy)
;;; setup-prodigy.el ends here
