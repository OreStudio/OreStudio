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
(require 'json)
(require 'transient)
(require 'prodigy)
(require 'ores-env)

(autoload 'prodigy-define-tag "prodigy")
(autoload 'prodigy-define-service "prodigy")
(defvar prodigy-services)

;; =============================================================================
;; Checkout identification
;; =============================================================================

(defconst ores/checkout-label
  (let* ((root (directory-file-name (ores/checkout-root)))
         (dir  (file-name-nondirectory root)))
    (if (string-prefix-p "OreStudio." dir)
        (substring dir (length "OreStudio."))
      dir))
  "The checkout label derived from the project directory name.")

(defconst ores/checkout-tag (intern ores/checkout-label)
  "The tag symbol for the current checkout.")

(defconst ores/project-root
  (expand-file-name (project-root (project-current t)))
  "The root directory of the current project.")

;; =============================================================================
;; CMake preset parsing
;; =============================================================================

(defun ores/parse-cmake-presets ()
  "Return list of non-hidden configure preset names from CMakePresets.json."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (data (with-temp-buffer
                 (insert-file-contents (concat ores/project-root "CMakePresets.json"))
                 (json-read)))
         (presets (alist-get 'configurePresets data)))
    (delq nil
          (mapcar (lambda (p)
                    (unless (eq (alist-get 'hidden p) t)
                      (alist-get 'name p)))
                  presets))))

;; =============================================================================
;; Preset utilities
;; =============================================================================

(defun ores/preset-build-type (preset)
  "Return 'debug or 'release from PRESET name string."
  (cond ((string-match-p "debug" preset) 'debug)
        ((string-match-p "release" preset) 'release)
        (t 'debug)))

(defun ores/preset-code (preset)
  "Return 3-char code for PRESET, e.g. 'cdn' for linux-clang-debug-ninja."
  (let* ((parts (split-string (replace-regexp-in-string "^linux-" "" preset) "-"))
         (compiler (pcase (car parts) ("clang" "c") ("gcc" "g") (_ "?")))
         (build    (cond ((member "debug"   parts) "d")
                         ((member "release" parts) "r")
                         (t "?")))
         (tool     (cond ((member "ninja" parts) "n")
                         ((member "make"  parts) "m")
                         (t "?"))))
    (concat compiler build tool)))

(defun ores/preset-output-path (preset)
  "Return path to build output directory for PRESET."
  (concat ores/project-root "build/output/" preset))

(defun ores/preset-publish-path (preset)
  "Return path to publish directory for PRESET."
  (concat (ores/preset-output-path preset) "/publish"))

(defun ores/preset-wt-resources (preset)
  "Return path to Wt resources directory for PRESET."
  (concat (ores/preset-output-path preset)
          "/vcpkg_installed/x64-linux/share/Wt/resources"))

;; =============================================================================
;; Port allocation
;; =============================================================================

(defconst ores/port-bases
  '(("remote" . 50000)
    ("local1" . 51000)
    ("local2" . 52000)
    ("local3" . 53000)
    ("local4" . 54000)
    ("local5" . 55000))
  "Alist mapping checkout labels to base port numbers.")

(defun ores/get-port (service-type build-type)
  "Return port for SERVICE-TYPE and BUILD-TYPE in the current checkout."
  (let* ((base (alist-get ores/checkout-label ores/port-bases 50000 nil #'string=))
         (offset (cond
                  ((and (eq service-type 'http)    (eq build-type 'debug))   0)
                  ((and (eq service-type 'http)    (eq build-type 'release)) 1)
                  ((and (eq service-type 'wt)      (eq build-type 'debug))   2)
                  ((and (eq service-type 'wt)      (eq build-type 'release)) 3)
                  (t 0))))
    (+ base offset)))

;; =============================================================================
;; NATS domain service discovery
;; =============================================================================

(defun ores/nats-domain-services ()
  "Return alist of (binary-name . display-name) for all NATS domain services.
Discovered from projects/ores.*.service directories at the checkout root."
  (let ((projects-dir (expand-file-name "projects" (ores/checkout-root)))
        result)
    (dolist (entry (directory-files projects-dir nil "^ores\\..+\\.service$"))
      (let* ((component (replace-regexp-in-string
                         "^ores\\.\\(.+\\)\\.service$" "\\1" entry))
             (display (concat component " service")))
        (push (cons entry display) result)))
    (nreverse result)))


;; =============================================================================
;; Tags
;; =============================================================================

(prodigy-define-tag :name 'ores)
(prodigy-define-tag :name 'ui)
(prodigy-define-tag :name 'debug)
(prodigy-define-tag :name 'release)
(prodigy-define-tag :name ores/checkout-tag)
(prodigy-define-tag :name 'nats-service)
(prodigy-define-tag :name 'http-server)
(prodigy-define-tag :name 'wt-server)

;; =============================================================================
;; Dynamic service registration
;; =============================================================================

(defun ores/service-name (base _preset)
  "Generate unique service name from BASE.
Uses BASE directly; build type and checkout are already visible as tags."
  base)

(defun ores/define-services-for-preset (preset)
  "Register all prodigy services for PRESET."
  (let* ((build-type  (ores/preset-build-type preset))
         (build-tag   (if (eq build-type 'debug) 'debug 'release))
         (preset-tag  (intern preset))
         (bin         (concat (ores/preset-publish-path preset) "/bin"))
         (common-tags `(ores ,build-tag ,ores/checkout-tag ,preset-tag))
         (common-args '("--log-enabled" "--log-level" "trace" "--log-directory" "../log"))
         (nats-service-args '("--log-to-console")))
    (prodigy-define-tag :name preset-tag)

    ;; Services subscribe using a subject prefix to isolate this checkout from
    ;; others sharing the same NATS broker. The matching namespace value is
    ;; stored in the connection manager and applied by the client at login.
    (let ((nats-args `("--nats-url" "nats://localhost:4222"
                       "--nats-subject-prefix"
                       ,(format "ores.dev.%s" ores/checkout-label))))

      ;; NATS domain services
      (dolist (svc (ores/nats-domain-services))
        (prodigy-define-service
          :name    (ores/service-name (format "ORE Studio %s" (cdr svc)) preset)
          :cwd     bin
          :command (concat bin "/" (car svc))
          :args    `(,@common-args ,@nats-service-args ,@nats-args)
          :tags    `(,@common-tags nats-service)
          :env     (ores/load-dotenv)
          :on-output (lambda (&rest args)
                       (when (string-match-p "Service ready"
                                             (plist-get args :output))
                         (prodigy-set-status (plist-get args :service) 'ready)))
          :stop-signal 'sigint
          :kill-process-buffer-on-stop t))

      (let ((qt-on-output (lambda (&rest args)
                            (when (string-match-p "Starting ORE Studio Qt"
                                                  (plist-get args :output))
                              (prodigy-set-status (plist-get args :service) 'ready))))
            (qt-args `(,@common-args "--log-to-console")))

        ;; QT
        (prodigy-define-service
          :name      (ores/service-name "ORE Studio QT" preset)
          :cwd       bin
          :command   (concat bin "/ores.qt")
          :args      qt-args
          :tags      `(ores ui ,build-tag ,ores/checkout-tag ,preset-tag)
          :env       (ores/load-dotenv)
          :on-output qt-on-output
          :stop-signal 'sigint
          :kill-process-buffer-on-stop t)

        ;; QT Blue
        (prodigy-define-service
          :name      (ores/service-name "ORE Studio QT Blue" preset)
          :cwd       bin
          :command   (concat bin "/ores.qt")
          :args      `(,@qt-args "--log-filename" "ores.qt.blue.log"
                       "--instance-name" "Blue" "--instance-color" "2196F3")
          :tags      `(ores ui ,build-tag ,ores/checkout-tag ,preset-tag)
          :env       (ores/load-dotenv)
          :on-output qt-on-output
          :stop-signal 'sigint
          :kill-process-buffer-on-stop t)

        ;; QT Red
        (prodigy-define-service
          :name      (ores/service-name "ORE Studio QT Red" preset)
          :cwd       bin
          :command   (concat bin "/ores.qt")
          :args      `(,@qt-args "--log-filename" "ores.qt.red.log"
                       "--instance-name" "Red" "--instance-color" "F44336")
          :tags      `(ores ui ,build-tag ,ores/checkout-tag ,preset-tag)
          :env       (ores/load-dotenv)
          :on-output qt-on-output
          :stop-signal 'sigint
          :kill-process-buffer-on-stop t)

        ;; QT Green
        (prodigy-define-service
          :name      (ores/service-name "ORE Studio QT Green" preset)
          :cwd       bin
          :command   (concat bin "/ores.qt")
          :args      `(,@qt-args "--log-filename" "ores.qt.green.log"
                       "--instance-name" "Green" "--instance-color" "4CAF50")
          :tags      `(ores ui ,build-tag ,ores/checkout-tag ,preset-tag)
          :env       (ores/load-dotenv)
          :on-output qt-on-output
          :stop-signal 'sigint
          :kill-process-buffer-on-stop t)))

    ;; HTTP Server
    (prodigy-define-service
      :name    (ores/service-name "ORE Studio HTTP Server" preset)
      :cwd     bin
      :command (concat bin "/ores.http.server")
      :args    `(,@common-args
                 "--port" ,(number-to-string (ores/get-port 'http build-type)))
      :tags    `(,@common-tags http-server)
      :env     (ores/load-dotenv)
      :stop-signal 'sigint
      :kill-process-buffer-on-stop t)

    ;; WT Server
    (prodigy-define-service
      :name    (ores/service-name "ORE Studio WT Server" preset)
      :cwd     bin
      :command (concat bin "/ores.wt")
      :args    `(,@common-args
                 "--http-address" "0.0.0.0" "--docroot" "."
                 "--http-port" ,(number-to-string (ores/get-port 'wt build-type)))
      :tags    `(,@common-tags wt-server)
      :env     `(("WT_RESOURCES_DIR" ,(concat (ores/preset-publish-path preset)
                                               "/../../vcpkg_installed/x64-linux/share/Wt/resources"))
                 ,@(ores/load-dotenv))
      :stop-signal 'sigint
      :kill-process-buffer-on-stop t)))

(defun ores/remove-preset-services (preset)
  "Remove all prodigy services registered for PRESET."
  (let ((preset-tag (intern preset)))
    (setq prodigy-services
          (cl-delete-if (lambda (svc)
                          (memq preset-tag (plist-get svc :tags)))
                        prodigy-services))))

;; =============================================================================
;; Active preset tracking
;; =============================================================================

(defun ores/active-presets ()
  "Return list of preset names currently registered as services for this checkout."
  (let (active)
    (dolist (preset (ores/parse-cmake-presets))
      (when (cl-find-if (lambda (svc)
                          (memq (intern preset) (plist-get svc :tags)))
                        prodigy-services)
        (push preset active)))
    (nreverse active)))

;; =============================================================================
;; Interactive commands
;; =============================================================================

(defun ores/add-presets-interactive ()
  "Interactively select presets to add services for."
  (interactive)
  (let* ((all       (ores/parse-cmake-presets))
         (active    (ores/active-presets))
         (available (cl-set-difference all active :test #'string=)))
    (if (null available)
        (user-error "All available presets are already active")
      (let ((choices (completing-read-multiple "Add presets: " available nil t)))
        (dolist (preset choices)
          (ores/define-services-for-preset preset))
        (message "Added services for: %s" (mapconcat #'identity choices ", "))))))

(defun ores/remove-presets-interactive ()
  "Interactively select presets to remove services for."
  (interactive)
  (let ((active (ores/active-presets)))
    (if (null active)
        (user-error "No active presets to remove")
      (let ((choices (completing-read-multiple "Remove presets: " active nil t)))
        (dolist (preset choices)
          (ores/remove-preset-services preset))
        (message "Removed services for: %s" (mapconcat #'identity choices ", "))))))

(defun ores/prodigy ()
  "Open Prodigy buffer filtered to the current checkout."
  (interactive)
  (cunene/prodigy-filter-by-tag ores/checkout-tag))

;; =============================================================================
;; Transient menu
;; =============================================================================

(defun ores/services-menu--header ()
  "Return a dynamic header string showing the currently active presets."
  (let ((active (ores/active-presets)))
    (if active
        (format "Active [%s]: %s"
                ores/checkout-label
                (mapconcat #'identity active ", "))
      (format "No active presets [%s]" ores/checkout-label))))

(transient-define-prefix ores/services-menu ()
  "Manage ORE Studio prodigy services."
  [:description ores/services-menu--header
   ("a" "Add services"    ores/add-presets-interactive)
   ("r" "Remove services" ores/remove-presets-interactive)
   ("s" "Show Prodigy"    ores/prodigy)])

(define-key global-map (kbd "C-x p 8") #'ores/services-menu)

;; =============================================================================
;; Startup: clean stale services
;; =============================================================================

;; Remove any services for this checkout registered in a prior load,
;; so reloading the file does not produce duplicates.
(when (boundp 'prodigy-services)
  (setq prodigy-services
        (cl-delete-if (lambda (svc)
                        (memq ores/checkout-tag (plist-get svc :tags)))
                      prodigy-services)))

(provide 'ores-prodigy)
;;; ores-prodigy.el ends here
