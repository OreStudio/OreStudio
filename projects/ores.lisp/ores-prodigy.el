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
(require 'cl-lib)
(require 'prodigy)

(setq prodigy-list-format
      [("Marked" 6 t :right-align t)
       ("Name" 60 t)
       ("Status" 15 t)
       ("Tags" 25 nil)])

(autoload 'prodigy-define-tag "prodigy")
(autoload 'prodigy-define-service "prodigy")
(defvar prodigy-services)

(defvar ores/checkout-label
  (let* ((pr (project-current t))
         (root (directory-file-name (expand-file-name (project-root pr))))
         (dir-name (file-name-nondirectory root)))
    (if (string-prefix-p "OreStudio." dir-name)
        (substring dir-name (length "OreStudio."))
      dir-name))
  "The checkout label derived from the project directory name.")

(defvar ores/checkout-tag (intern ores/checkout-label)
  "The tag symbol for the current checkout.")

;; Remove existing services for THIS checkout only, to allow reloading without
;; duplicates while preserving services from other checkouts.
(when (boundp 'prodigy-services)
  (setq prodigy-services
        (cl-delete-if (lambda (service)
                        (memq ores/checkout-tag (plist-get service :tags)))
                      prodigy-services)))

(defconst ores/port-bases
  '( ("remote" . 50000)
     ("local1" . 51000)
     ("local2" . 52000)
     ("local3" . 53000)
     ("local4" . 54000)
     ("local5" . 55000))
  "Alist mapping checkout labels to base port numbers.")

(defun ores/get-port (service-type build-type)
  "Return the port for SERVICE-TYPE and BUILD-TYPE in the current checkout.
SERVICE-TYPE is one of 'http, 'wt, or 'binary.
BUILD-TYPE is one of 'debug or 'release."
  (let* ((base (alist-get ores/checkout-label ores/port-bases 50000 nil #'string=))
         (offset (cond
                  ((and (eq service-type 'http) (eq build-type 'debug)) 0)
                  ((and (eq service-type 'http) (eq build-type 'release)) 1)
                  ((and (eq service-type 'wt) (eq build-type 'debug)) 2)
                  ((and (eq service-type 'wt) (eq build-type 'release)) 3)
                  ((and (eq service-type 'binary) (eq build-type 'debug)) 4)
                  ((and (eq service-type 'binary) (eq build-type 'release)) 5)
                  (t 0))))
    (+ base offset)))

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

(defcustom ores/database-name "ores_frosty_leaf"
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

(prodigy-define-tag :name 'ores)
(prodigy-define-tag :name 'ui)
(prodigy-define-tag :name 'debug)
(prodigy-define-tag :name 'release)
(prodigy-define-tag :name ores/checkout-tag)

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
  :name (format "ORE Studio QT - Debug - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--compression-enabled")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.qt")
  :tags `(ores ui debug ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio QT - Release - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--compression-enabled")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.qt")
  :tags `(ores ui release ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio QT Blue - Debug - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.blue.log" "--compression-enabled" "--instance-name" "Blue Debug" "--instance-color" "2196F3")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.qt")
  :tags `(ores ui debug ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio QT Blue - Release - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.blue.log" "--compression-enabled" "--instance-name" "Blue Release" "--instance-color" "2196F3")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.qt")
  :tags `(ores ui release ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio QT Red - Debug - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.red.log" "--compression-enabled" "--instance-name" "Red Debug" "--instance-color" "F44336")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.qt")
  :tags `(ores ui debug ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio QT Red - Release - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.red.log" "--compression-enabled" "--instance-name" "Red Release" "--instance-color" "F44336")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.qt")
  :tags `(ores ui release ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio QT Green - Debug - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.green.log" "--compression-enabled" "--instance-name" "Green Debug" "--instance-color" "4CAF50")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.qt")
  :tags `(ores ui debug ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio QT Green - Release - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.green.log" "--compression-enabled" "--instance-name" "Green Release" "--instance-color" "4CAF50")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.qt")
  :tags `(ores ui release ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio Comms Service - Debug - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args `("--log-enabled" "--log-level" "trace" "--log-directory" "../log"
          "--port" ,(number-to-string (ores/get-port 'binary 'debug)))
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.comms.service")
  :tags `(ores debug comms-service ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio Comms Service - Release - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args `("--log-enabled" "--log-level" "trace" "--log-directory" "../log"
          "--port" ,(number-to-string (ores/get-port 'binary 'release)))
  :command (concat (ores/path-to-publish 'release) "/bin/ores.comms.service")
  :tags `(ores release comms-service ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio HTTP Server - Debug - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args `("--log-enabled" "--log-level" "trace" "--log-directory" "../log"
          "--port" ,(number-to-string (ores/get-port 'http 'debug)))
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.http.server")
  :tags `(ores debug http-server ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio HTTP Server - Release - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args `("--log-enabled" "--log-level" "trace" "--log-directory" "../log"
          "--port" ,(number-to-string (ores/get-port 'http 'release)))
  :command (concat (ores/path-to-publish 'release) "/bin/ores.http.server")
  :tags `(ores release http-server ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio WT Server - Debug - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args `("--log-enabled" "--log-level" "trace" "--log-directory" "../log"
          "--http-address" "0.0.0.0" "--docroot" "."
          "--http-port" ,(number-to-string (ores/get-port 'wt 'debug)))
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.wt")
  :tags `(ores debug wt-server ,ores/checkout-tag)
  :env `(("WT_RESOURCES_DIR" ,(ores/path-to-wt-resources 'debug))
         ,@(ores/setup-environment "WT"))
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (format "ORE Studio WT Server - Release - %s" ores/checkout-label)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args `("--log-enabled" "--log-level" "trace" "--log-directory" "../log"
          "--http-address" "0.0.0.0" "--docroot" "."
          "--http-port" ,(number-to-string (ores/get-port 'wt 'release)))
  :command (concat (ores/path-to-publish 'release) "/bin/ores.wt")
  :tags `(ores release wt-server ,ores/checkout-tag)
  :env `(("WT_RESOURCES_DIR" ,(ores/path-to-wt-resources 'release))
         ,@(ores/setup-environment "WT"))
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(provide 'ores-prodigy)
;;; ores-prodigy.el ends here
