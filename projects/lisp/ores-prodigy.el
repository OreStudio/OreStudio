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

(defun ores/path-to-publish ()
  "Return the path to the qt user interface directory."
  (let* ((pr (project-current t))
         (root (project-root pr))
         (path (concat root "build/output/linux-clang-debug/publish")))
    path))

(defun ores/setup-environment ()
  (let ((pwd (auth-source-pick-first-password
              :host "localhost"
              :user "ores")))
    (list (list "ORES_SERVICE_DB_PASSWORD" pwd))))

(defvar ores/service-environment (ores/setup-environment)
  "Environment to run services in.")

(setq prodigy-services nil)
(prodigy-define-tag :name 'ores)
(prodigy-define-tag :name 'ui)
(prodigy-define-tag :name 'debug)
(prodigy-define-tag :name 'release)
(prodigy-define-tag
  :name 'dev-service
  :env (ores/setup-environment))

(prodigy-define-service
  :name "ORE Studio QT - Debug"
  :cwd (concat (ores/path-to-publish) "/bin")
  :command (concat (ores/path-to-publish) "/bin/ores.qt")
  :tags '(ores ui debug)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio QT - Release"
  :cwd (concat (ores/path-to-publish) "/bin")
  :command (concat (ores/path-to-publish) "/bin/ores.qt")
  :tags '(ores ui release)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio Service - Debug"
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log")
  :cwd (concat (ores/path-to-publish) "/bin")
  :command (concat (ores/path-to-publish) "/bin/ores.service")
  :tags '(ores debug dev-service)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio Service - Release"
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log")
  :cwd (concat (ores/path-to-publish) "/bin")
  :command (concat (ores/path-to-publish) "/bin/ores.service")
  :tags '(ores release dev-service)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(provide 'ores-prodigy)
;;; ores-prodigy.el ends here
