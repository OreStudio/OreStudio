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

(defun ores--get-build-output-path (build-type)
  "Return the path to the build output directory for BUILD-TYPE.
BUILD-TYPE should be either 'debug or 'release.
This is an internal helper function."
  (let* ((pr (project-current t))
         (root (expand-file-name (project-root pr)))
         (build-dir (if (eq build-type 'release)
                        "linux-clang-release"
                      "linux-clang-debug")))
    (concat root "build/output/" build-dir)))

(defun ores/path-to-publish (build-type)
  "Return the path to the publish directory for BUILD-TYPE.
BUILD-TYPE should be either 'debug or 'release."
  (concat (ores--get-build-output-path build-type) "/publish"))

(defun ores/path-to-wt-resources (build-type)
  "Return the path to the Wt resources directory for BUILD-TYPE.
BUILD-TYPE should be either 'debug or 'release."
  (concat (ores--get-build-output-path build-type)
          "/vcpkg_installed/x64-linux/share/Wt/resources"))

(defcustom ores/database-name "ores_nameless_sea"
  "Database name for ORES services.
This should be set to an instance database name like 'ores_autumn_sound'."
  :type 'string
  :group 'ores)

(defcustom ores/database-user "ores"
  "Database user for ORES services."
  :type 'string
  :group 'ores)

(defun ores/setup-environment (app-prefix)
  "Set up environment variables for ORES services.
APP-PREFIX is the application prefix (e.g., \"SERVICE\", \"HTTP_SERVER\", \"WT\").
Retrieves database password from auth-source and combines with
configured database name and user."
  (let ((pwd (auth-source-pick-first-password
              :host "localhost"
              :user ores/database-user)))
    (list
     (list (concat "ORES_" app-prefix "_DB_USER") ores/database-user)
     (list (concat "ORES_" app-prefix "_DB_PASSWORD") pwd)
     (list (concat "ORES_" app-prefix "_DB_DATABASE") ores/database-name))))

(setq prodigy-services nil)
(prodigy-define-tag :name 'ores)
(prodigy-define-tag :name 'ui)
(prodigy-define-tag :name 'debug)
(prodigy-define-tag :name 'release)
(prodigy-define-tag
  :name 'comms-service
  :env (ores/setup-environment "SERVICE"))
(defun ores/setup-http-server-environment ()
  "Set up environment variables for the HTTP server.
Includes database credentials and JWT secret from auth-source."
  (let ((jwt-secret (auth-source-pick-first-password
                     :host "ores-jwt"
                     :user "http-server")))
    `(("ORES_HTTP_SERVER_JWT_SECRET" ,jwt-secret)
      ,@(ores/setup-environment "HTTP_SERVER"))))

(prodigy-define-tag
  :name 'http-server
  :env (ores/setup-http-server-environment))
(prodigy-define-tag
  :name 'wt-server
  :env (ores/setup-environment "WT"))

(prodigy-define-service
  :name "ORE Studio QT - Debug"
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--compression-enabled")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.qt")
  :tags '(ores ui debug)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio QT - Release"
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--compression-enabled")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.qt")
  :tags '(ores ui release)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio Comms Service - Debug"
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.comms.service")
  :tags '(ores debug comms-service)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio Comms Service - Release"
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.comms.service")
  :tags '(ores release comms-service)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio HTTP Server - Debug"
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.http.server")
  :tags '(ores debug http-server)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio HTTP Server - Release"
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.http.server")
  :tags '(ores release http-server)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio WT Server - Debug"
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.wt")
  :tags '(ores debug wt-server)
  :env `(("WT_RESOURCES_DIR" ,(ores/path-to-wt-resources 'debug))
         ,@(ores/setup-environment "WT"))
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "ORE Studio WT Server - Release"
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.wt")
  :tags '(ores release wt-server)
  :env `(("WT_RESOURCES_DIR" ,(ores/path-to-wt-resources 'release))
         ,@(ores/setup-environment "WT"))
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(provide 'ores-prodigy)
;;; ores-prodigy.el ends here
