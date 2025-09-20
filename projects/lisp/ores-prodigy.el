;;; ores-prodigy.el --- Sets up prodigy for this project.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Marco Craveiro

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
(require 'project)
(autoload 'prodigy-define-tag "prodigy")
(autoload 'prodigy-define-service "prodigy")
(defvar prodigy-services)

(defun ores/path-to-output ()
  "Return the path to the qt user interface directory."
  (let* ((pr (project-current t))
         (root (project-root pr))
         (path (concat root "build/output")))
    path))

(setq prodigy-services nil)
(prodigy-define-tag :name 'ores)
(prodigy-define-tag :name 'ui)
(prodigy-define-tag :name 'debug)
(prodigy-define-tag :name 'release)

(prodigy-define-service
  :name "ORE Studio QT - Debug"
  :cwd (concat (ores/path-to-output) "/linux-clang-debug/projects/ores.qt")
  :command (concat (ores/path-to-output) "/linux-clang-debug/projects/ores.qt/ores.qt")
  :tags '(ores ui debug)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio QT - Release"
  :cwd (concat (ores/path-to-output) "/linux-clang-debug/projects/ores.qt")
  :command (concat (ores/path-to-output) "/linux-clang-release/projects/ores.qt/ores.qt")
  :tags '(ores ui release)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio Service - Debug"
  :cwd (concat (ores/path-to-output) "/linux-clang-debug/projects/ores.service")
  :command (concat (ores/path-to-output) "/linux-clang-debug/projects/ores.service/ores.service")
  :tags '(ores debug)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio Service - Release"
  :cwd (concat (ores/path-to-output) "/linux-clang-debug/projects/ores.service")
  :command (concat (ores/path-to-output) "/linux-clang-release/projects/ores.service/ores.service")
  :tags '(ores release)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(provide 'ores-prodigy)
;;; ores-prodigy.el ends here
