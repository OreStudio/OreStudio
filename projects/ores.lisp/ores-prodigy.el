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
       ("Name" 35 t)
       ("Port" 8 t)
       ("Env" 8 t)
       ("Config" 8 t)
       ("Status" 10 t)])

(autoload 'prodigy-define-tag "prodigy")
(autoload 'prodigy-define-service "prodigy")
(defvar prodigy-services)

(defconst ores/checkout-label
  (let* ((pr (project-current t))
         (root (directory-file-name (expand-file-name (project-root pr))))
         (dir-name (file-name-nondirectory root)))
    (if (string-prefix-p "OreStudio." dir-name)
        (substring dir-name (length "OreStudio."))
      dir-name))
  "The checkout label derived from the project directory name.")

(defconst ores/checkout-tag (intern ores/checkout-label)
  "The tag symbol for the current checkout.")

(defun ores/prodigy ()
  "Open Prodigy buffer for current project's checkout label."
  (interactive)
  (let ((label ores/checkout-label))
    (if label
        (cunene/prodigy-filter-by-tag (intern label))
      (user-error "Not in an OreStudio project?"))))

;; Optional: bind to a key
(define-key global-map (kbd "C-x p 8") #'ores/prodigy)

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

(defconst ores/database-name
  (concat "ores_dev_" ores/checkout-label)
  "Database name for ORES services.
Automatically derived from the checkout label (e.g., ores_dev_local2).
Each environment gets its own isolated database.")

(defconst ores/database-users
  '(("SERVICE" . "ores_comms_user")
    ("HTTP_SERVER" . "ores_http_user")
    ("WT" . "ores_wt_user"))
  "Alist mapping application prefixes to their database users.
Each service has its own dedicated database user for fine-grained access control.")

(defun ores/get-database-user (app-prefix)
  "Get the database user for APP-PREFIX.
Returns the service-specific user or falls back to ores_cli_user."
  (or (cdr (assoc app-prefix ores/database-users))
      "ores_cli_user"))

(defun ores/setup-environment (app-prefix)
  "Set up environment variables for ORES services.
APP-PREFIX is the application prefix (e.g., \"SERVICE\", \"HTTP_SERVER\", \"WT\").
Retrieves database password from auth-source using the service-specific user
and combines with configured database name."
  (let* ((db-user (ores/get-database-user app-prefix))
         (pwd (auth-source-pick-first-password
               :host "localhost"
               :user db-user)))
    (list
     (list (concat "ORES_" app-prefix "_DB_USER") db-user)
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

(defun ores/service-name (base config)
  "Generate unique service name from BASE, CONFIG (debug/release) and checkout."
  (let ((cfg-char (if (eq config 'debug) "D" "R")))
    (format "%s [%s:%s]" base cfg-char ores/checkout-label)))

(prodigy-define-service
  :name (ores/service-name "ORE Studio QT" 'debug)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--compression-enabled")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.qt")
  :tags `(ores ui debug ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio QT" 'release)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--compression-enabled")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.qt")
  :tags `(ores ui release ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio QT Blue" 'debug)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.blue.log" "--compression-enabled" "--instance-name" "Blue Debug" "--instance-color" "2196F3")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.qt")
  :tags `(ores ui debug ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio QT Blue" 'release)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.blue.log" "--compression-enabled" "--instance-name" "Blue Release" "--instance-color" "2196F3")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.qt")
  :tags `(ores ui release ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio QT Red" 'debug)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.red.log" "--compression-enabled" "--instance-name" "Red Debug" "--instance-color" "F44336")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.qt")
  :tags `(ores ui debug ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio QT Red" 'release)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.red.log" "--compression-enabled" "--instance-name" "Red Release" "--instance-color" "F44336")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.qt")
  :tags `(ores ui release ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio QT Green" 'debug)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.green.log" "--compression-enabled" "--instance-name" "Green Debug" "--instance-color" "4CAF50")
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.qt")
  :tags `(ores ui debug ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio QT Green" 'release)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log" "--log-filename" "ores.qt.green.log" "--compression-enabled" "--instance-name" "Green Release" "--instance-color" "4CAF50")
  :command (concat (ores/path-to-publish 'release) "/bin/ores.qt")
  :tags `(ores ui release ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio Comms Service" 'debug)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args `("--log-enabled" "--log-level" "trace" "--log-directory" "../log"
          "--port" ,(number-to-string (ores/get-port 'binary 'debug)))
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.comms.service")
  :tags `(ores debug comms-service ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio Comms Service" 'release)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args `("--log-enabled" "--log-level" "trace" "--log-directory" "../log"
          "--port" ,(number-to-string (ores/get-port 'binary 'release)))
  :command (concat (ores/path-to-publish 'release) "/bin/ores.comms.service")
  :tags `(ores release comms-service ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio HTTP Server" 'debug)
  :cwd (concat (ores/path-to-publish 'debug) "/bin")
  :args `("--log-enabled" "--log-level" "trace" "--log-directory" "../log"
          "--port" ,(number-to-string (ores/get-port 'http 'debug)))
  :command (concat (ores/path-to-publish 'debug) "/bin/ores.http.server")
  :tags `(ores debug http-server ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio HTTP Server" 'release)
  :cwd (concat (ores/path-to-publish 'release) "/bin")
  :args `("--log-enabled" "--log-level" "trace" "--log-directory" "../log"
          "--port" ,(number-to-string (ores/get-port 'http 'release)))
  :command (concat (ores/path-to-publish 'release) "/bin/ores.http.server")
  :tags `(ores release http-server ,ores/checkout-tag)
  :stop-signal 'sigint
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name (ores/service-name "ORE Studio WT Server" 'debug)
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
  :name (ores/service-name "ORE Studio WT Server" 'release)
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

;; =============================================================================
;; Color-code services by checkout in prodigy buffer
;; =============================================================================

;; Clean up any stale hooks from previous loads
(remove-hook 'prodigy-mode-hook #'ores/prodigy-setup-font-lock)
(remove-hook 'prodigy-mode-hook #'ores/prodigy-add-font-lock)

;; Environment faces - proced-inspired color scheme
;; Using face-spec-set with face-defface-spec to ensure colors update on reload
(defface ores/prodigy-local1-face '((t)) "Face for local1." :group 'ores)
(face-spec-set 'ores/prodigy-local1-face '((t :foreground "#8a2be2")) 'face-defface-spec)

(defface ores/prodigy-local2-face '((t)) "Face for local2." :group 'ores)
(face-spec-set 'ores/prodigy-local2-face '((t :foreground "#5085ef")) 'face-defface-spec)

(defface ores/prodigy-local3-face '((t)) "Face for local3." :group 'ores)
(face-spec-set 'ores/prodigy-local3-face '((t :foreground "#ded93e")) 'face-defface-spec)

(defface ores/prodigy-local4-face '((t)) "Face for local4." :group 'ores)
(face-spec-set 'ores/prodigy-local4-face '((t :foreground "#6d5cc3")) 'face-defface-spec)

(defface ores/prodigy-local5-face '((t)) "Face for local5." :group 'ores)
(face-spec-set 'ores/prodigy-local5-face '((t :foreground "#1e90ff")) 'face-defface-spec)

(defface ores/prodigy-remote-face '((t)) "Face for remote." :group 'ores)
(face-spec-set 'ores/prodigy-remote-face '((t :foreground "DeepSkyBlue")) 'face-defface-spec)

;; Configuration faces - proced-inspired
(defface ores/prodigy-debug-face '((t)) "Face for debug." :group 'ores)
(face-spec-set 'ores/prodigy-debug-face '((t :foreground "#5085ef")) 'face-defface-spec)

(defface ores/prodigy-release-face '((t)) "Face for release." :group 'ores)
(face-spec-set 'ores/prodigy-release-face '((t :foreground "#5085bf")) 'face-defface-spec)

;; Name and port faces - proced-inspired
(defface ores/prodigy-name-face '((t)) "Face for name." :group 'ores)
(face-spec-set 'ores/prodigy-name-face '((t :foreground "DeepSkyBlue")) 'face-defface-spec)

(defface ores/prodigy-port-face '((t)) "Face for port." :group 'ores)
(face-spec-set 'ores/prodigy-port-face '((t :foreground "#40e0d0")) 'face-defface-spec)

;; Force recalculation of all faces
(dolist (face '(ores/prodigy-local1-face ores/prodigy-local2-face ores/prodigy-local3-face
                ores/prodigy-local4-face ores/prodigy-local5-face ores/prodigy-remote-face
                ores/prodigy-debug-face ores/prodigy-release-face
                ores/prodigy-name-face ores/prodigy-port-face))
  (face-spec-recalc face nil))

(defvar ores/prodigy-env-faces
  '((local1 . ores/prodigy-local1-face)
    (local2 . ores/prodigy-local2-face)
    (local3 . ores/prodigy-local3-face)
    (local4 . ores/prodigy-local4-face)
    (local5 . ores/prodigy-local5-face)
    (remote . ores/prodigy-remote-face))
  "Alist mapping environment tags to faces.")

(defvar ores/prodigy-config-faces
  '((debug . ores/prodigy-debug-face)
    (release . ores/prodigy-release-face))
  "Alist mapping configuration tags to faces.")

(defun ores/prodigy-get-env (service)
  "Extract environment (local1, local2, etc.) from SERVICE tags."
  (let ((tags (plist-get service :tags)))
    (cl-find-if (lambda (tag)
                  (assq tag ores/prodigy-env-faces))
                tags)))

(defun ores/prodigy-get-config (service)
  "Extract configuration (debug, release) from SERVICE tags."
  (let ((tags (plist-get service :tags)))
    (cond ((memq 'debug tags) 'debug)
          ((memq 'release tags) 'release)
          (t nil))))

(defun ores/prodigy-colorize-env (env)
  "Return colorized string for ENV."
  (if env
      (let ((face (alist-get env ores/prodigy-env-faces)))
        (propertize (symbol-name env) 'face face))
    ""))

(defun ores/prodigy-colorize-config (config)
  "Return colorized string for CONFIG."
  (if config
      (let ((face (alist-get config ores/prodigy-config-faces)))
        (propertize (symbol-name config) 'face face))
    ""))

(defun ores/prodigy-get-port (service)
  "Extract port number from SERVICE args.
Looks for --port or --http-port arguments and returns the following value.
Returns \"-\" if no port is found."
  (let ((args (plist-get service :args)))
    (if args
        (let ((port-idx (or (cl-position "--port" args :test #'string=)
                            (cl-position "--http-port" args :test #'string=))))
          (if port-idx
              (nth (1+ port-idx) args)
            "N/A"))
      "N/A")))

(defun ores/prodigy-normalize-name (name)
  "Normalize NAME to prodigy's internal ID format.
Converts to lowercase and replaces spaces with hyphens."
  (downcase (replace-regexp-in-string " " "-" name)))

(defun ores/prodigy-find-service-by-id (id)
  "Find service plist by ID (normalized name)."
  (cl-find-if (lambda (s)
                (string= (ores/prodigy-normalize-name (plist-get s :name)) id))
              prodigy-services))

(defun ores/prodigy-strip-name-suffix (name)
  "Strip the [D:env] or [R:env] suffix from NAME for display."
  (if (string-match "\\(.+\\) \\[.+\\]$" name)
      (match-string 1 name)
    name))

;; Custom column functions for our extended format
;; These use propertize like prodigy-status-col does

(defun ores/prodigy-name-col (service)
  "Return SERVICE name column with suffix stripped and color."
  (let ((name (ores/prodigy-strip-name-suffix (plist-get service :name))))
    (propertize name 'face 'ores/prodigy-name-face)))

(defun ores/prodigy-port-col (service)
  "Return SERVICE port column with color."
  (let ((port (ores/prodigy-get-port service)))
    (propertize port 'face 'ores/prodigy-port-face)))

(defun ores/prodigy-env-col (service)
  "Return SERVICE env column with color."
  (let ((env (ores/prodigy-get-env service)))
    (if env
        (let ((face (alist-get env ores/prodigy-env-faces)))
          (propertize (symbol-name env) 'face face))
      "")))

(defun ores/prodigy-config-col (service)
  "Return SERVICE config column with color."
  (let ((config (ores/prodigy-get-config service)))
    (if config
        (let ((face (alist-get config ores/prodigy-config-faces)))
          (propertize (symbol-name config) 'face face))
      "")))

;; Override prodigy-list-entries to add our custom columns
;; Uses same pattern as original prodigy to preserve text properties
(defun prodigy-list-entries ()
  "Create entries for the service list with ORES custom columns."
  (-map
   (lambda (service)
     (list
      (prodigy-service-id service)
      (apply 'vector
             (--map
              (funcall it service)
              '(prodigy-marked-col
                ores/prodigy-name-col
                ores/prodigy-port-col
                ores/prodigy-env-col
                ores/prodigy-config-col
                prodigy-status-col)))))
   (prodigy-services)))

(provide 'ores-prodigy)
;;; ores-prodigy.el ends here
