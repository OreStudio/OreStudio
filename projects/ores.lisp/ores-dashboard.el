;;; ores-dashboard.el --- ORE Studio development console  -*- lexical-binding: t; -*-

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
;; One-stop development console for an ORE Studio checkout.
;;
;; Each invocation of `ores/dashboard' creates (or refreshes) a buffer named
;; "ORES Dashboard - <label>" where <label> is ORES_CHECKOUT_LABEL from .env.
;; The buffer is bound to the checkout's .env at open time via buffer-local
;; variables, so two Emacs instances on local1 and local2 never share state.
;;
;; Public API:
;;   `ores/dashboard'        - open (or refresh) the dashboard
;;   `ores/dashboard-reload' - re-read .env and redraw the current buffer

;;; Code:

(require 'ores-env)
(require 'ores-shell)
(require 'ores-db)

;; ---------------------------------------------------------------------------
;; Buffer-local state
;; ---------------------------------------------------------------------------

(defvar-local ores/dashboard--env nil
  "Alist of (KEY . VALUE) from the checkout .env.
Set after `ores/dashboard-mode' init so kill-all-local-variables does not wipe it.")

(defvar-local ores/dashboard--root nil
  "Checkout root path, paired with `ores/dashboard--env'.")

;; ---------------------------------------------------------------------------
;; Mode
;; ---------------------------------------------------------------------------

(defvar ores/dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'ores/dashboard-reload)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `ores/dashboard-mode'.")

(define-derived-mode ores/dashboard-mode special-mode "ORES-Dashboard"
  "Major mode for the ORE Studio development console.
All interaction is via buttons; the buffer is read-only between renders.")

;; ---------------------------------------------------------------------------
;; Entry points
;; ---------------------------------------------------------------------------

(defun ores/dashboard ()
  "Open the ORE Studio dashboard for the current checkout."
  (interactive)
  (let* ((env   (ores/load-dotenv))
         (label (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "unknown"))
         (root  (ores/checkout-root))
         (buf   (get-buffer-create (format "ORES Dashboard - %s" label))))
    (with-current-buffer buf
      (ores/dashboard-mode)
      (setq ores/dashboard--env  env)
      (setq ores/dashboard--root root)
      (let ((inhibit-read-only t))
        (ores/dashboard--render env root)))
    (pop-to-buffer buf)))

(defun ores/dashboard-reload ()
  "Re-read .env and redraw the current dashboard buffer."
  (interactive)
  (let ((env  (ores/load-dotenv))
        (root (ores/checkout-root)))
    (setq ores/dashboard--env  env)
    (setq ores/dashboard--root root)
    (let ((inhibit-read-only t))
      (ores/dashboard--render env root))
    (message "ORES Dashboard reloaded.")))

;; ---------------------------------------------------------------------------
;; Rendering helpers
;; ---------------------------------------------------------------------------

(defun ores/dashboard--section (title)
  "Insert a section heading for TITLE."
  (insert "\n")
  (insert (propertize (format "  %s\n" title)
                      'face '(:weight bold :underline t)))
  (insert "\n"))

(defun ores/dashboard--button (label action)
  "Insert a clickable button with LABEL that calls ACTION."
  (insert "    ")
  (insert-button label 'action action 'follow-link t)
  (insert "\n"))

(defun ores/dashboard--compile (cmd buf-label &optional root)
  "Run CMD in a compilation buffer named *ores-<buf-label>*.
Optional ROOT sets `default-directory'."
  (let ((default-directory (or root default-directory)))
    (compilation-start cmd nil (lambda (_) (format "*ores-%s*" buf-label)))))

;; ---------------------------------------------------------------------------
;; Render
;; ---------------------------------------------------------------------------

(defun ores/dashboard--render (env root)
  "Erase and redraw the dashboard using ENV alist and checkout ROOT."
  (erase-buffer)
  (let* ((label     (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "unknown"))
         (preset    (or (cdr (assoc "ORES_PRESET"          env)) "linux-clang-debug-ninja"))
         (site-port (or (cdr (assoc "ORES_SITE_PORT"       env)) "8000"))
         (scripts   (expand-file-name "build/scripts" root))
         (compass   (expand-file-name "projects/ores.compass/compass.sh" root)))

    ;; --- Header ---
    (insert "\n")
    (insert (propertize (format "  ORES Dashboard — %s\n" label)
                        'face '(:height 1.4 :weight bold)))
    (insert (propertize (format "  Preset: %s    (g to reload)\n" preset)
                        'face 'shadow))

    ;; --- Environment ---
    (ores/dashboard--section "Environment")
    (ores/dashboard--button "Init environment"
      (let ((r root) (p preset))
        (lambda (_)
          (let ((script (expand-file-name "build/scripts/init-environment.sh" r)))
            (ores/dashboard--compile
             (format "%s -y --preset %s" script p) "init-environment" r)))))
    (ores/dashboard--button "Reload environment"
      (lambda (_) (ores/dashboard-reload)))
    (ores/dashboard--button "Diff .env vs .env.old"
      (let ((r root))
        (lambda (_)
          (let ((old (expand-file-name ".env.old" r))
                (cur (expand-file-name ".env"     r)))
            (if (file-exists-p old)
                (diff old cur)
              (user-error "No .env.old found — run init-environment.sh at least twice"))))))

    ;; --- Database ---
    (ores/dashboard--section "Database")
    (ores/dashboard--button "Recreate environment"
      (let ((e env))
        (lambda (_)
          (let ((env-label (or (cdr (assoc "ORES_CHECKOUT_LABEL" e)) "local1")))
            (unless (yes-or-no-p (format "DROP and recreate ores_dev_%s? " env-label))
              (user-error "Aborted"))
            (ores-db/--run-recreate-env env-label)))))
    (ores/dashboard--button "Browse databases"
      (lambda (_) (ores-db/list-databases)))
    (ores/dashboard--button "Setup SQL connections"
      (lambda (_) (ores-db/setup-connections)))

    ;; --- Services ---
    (ores/dashboard--section "Services")
    (dolist (spec '(("Start services"  "start-services.sh"  "start-services")
                    ("Start client"    "start-client.sh"    "start-client")
                    ("Service status"  "status-services.sh" "service-status")
                    ("Stop services"   "stop-services.sh"   "stop-services")))
      (let ((lbl    (nth 0 spec))
            (script (expand-file-name (nth 1 spec) scripts))
            (buf    (nth 2 spec))
            (r      root))
        (ores/dashboard--button lbl
          (let ((s script) (b buf) (rd r))
            (lambda (_) (ores/dashboard--compile s b rd))))))

    ;; --- Compass ---
    (ores/dashboard--section "Compass")
    (dolist (spec '(("compass where"  "where"  "compass-where")
                    ("compass list"   "list"   "compass-list")
                    ("compass status" "status" "compass-status")))
      (let ((lbl  (nth 0 spec))
            (cmd  (nth 1 spec))
            (buf  (nth 2 spec))
            (c    compass)
            (r    root))
        (ores/dashboard--button lbl
          (let ((s (format "%s %s" c cmd)) (b buf) (rd r))
            (lambda (_) (ores/dashboard--compile s b rd))))))
    (ores/dashboard--button "Open compass.org"
      (let ((r root))
        (lambda (_) (find-file (expand-file-name "doc/compass.org" r)))))

    ;; --- Build ---
    (ores/dashboard--section "Build")
    (ores/dashboard--button "Configure"
      (let ((p preset) (r root))
        (lambda (_)
          (ores/dashboard--compile (format "cmake --preset %s" p) "configure" r))))
    (dolist (spec '(("Build" "all"  "build")
                    ("Test"  "test" "test")))
      (let ((lbl (nth 0 spec))
            (tgt (nth 1 spec))
            (buf (nth 2 spec))
            (p   preset)
            (r   root))
        (ores/dashboard--button lbl
          (let ((cmd (format "cmake --build --preset %s --target %s" p tgt))
                (b buf) (rd r))
            (lambda (_) (ores/dashboard--compile cmd b rd))))))

    ;; --- Site ---
    (ores/dashboard--section "Site")
    (ores/dashboard--button "Rebuild site"
      (let ((p preset) (r root))
        (lambda (_)
          (ores/dashboard--compile
           (format "cmake --build --preset %s --target deploy_site" p)
           "rebuild-site" r))))
    (ores/dashboard--button "Start site"
      (let ((r root))
        (lambda (_)
          (let* ((script (expand-file-name "build/scripts/serve-site.sh" r))
                 (default-directory r))
            (start-process "ores-serve-site" "*ores-serve-site*" script)
            (message "ORE Studio site server started.")))))
    (ores/dashboard--button "Stop site"
      (let ((port site-port))
        (lambda (_)
          (shell-command (format "fuser -k %s/tcp 2>/dev/null; true" port))
          (message "ORE Studio site server stopped."))))

    ;; --- Skills ---
    (ores/dashboard--section "Skills")
    (ores/dashboard--button "Open skills (doc/llm/skills/)"
      (let ((r root))
        (lambda (_) (dired (expand-file-name "doc/llm/skills" r)))))
    (ores/dashboard--button "Deploy skills"
      (let ((p preset) (r root))
        (lambda (_)
          (ores/dashboard--compile
           (format "cmake --build --preset %s --target deploy_skills" p)
           "deploy-skills" r))))

    ;; --- Bookmarks ---
    (ores/dashboard--section "Bookmarks")
    (let* ((log-dir (expand-file-name
                     (format "build/output/%s/publish/log" preset) root))
           (bin-dir (expand-file-name
                     (format "build/output/%s/publish/bin" preset) root)))
      (ores/dashboard--button (format "Log dir  %s" log-dir)
        (let ((d log-dir))
          (lambda (_) (if (file-directory-p d) (dired d)
                        (user-error "Log dir not found: %s" d)))))
      (ores/dashboard--button (format "Bin dir  %s" bin-dir)
        (let ((d bin-dir))
          (lambda (_) (if (file-directory-p d) (dired d)
                        (user-error "Bin dir not found: %s" d))))))

    ;; --- Links ---
    (ores/dashboard--section "Links")
    (dolist (spec `(("User manual"   "doc/manual/user_guide/user_manual.org")
                    ("Recipes index" "doc/recipes/recipes.org")
                    ("Agile index"   "doc/agile/agile.org")))
      (let ((lbl  (nth 0 spec))
            (path (expand-file-name (nth 1 spec) root)))
        (ores/dashboard--button lbl
          (let ((f path))
            (lambda (_) (find-file f))))))

    ;; --- Shell ---
    (ores/dashboard--section "Shell")
    (ores/dashboard--button "Open ORE Studio shell"
      (let ((e env) (r root))
        (lambda (_)
          (let* ((p   (or (cdr (assoc "ORES_PRESET" e)) ""))
                 (bin (expand-file-name
                       (format "build/output/%s/publish/bin/ores.shell" p) r)))
            (if (file-executable-p bin)
                (progn (setq ores-shell-last-program bin) (ores-shell))
              (user-error "ores.shell not found at %s — build first" bin))))))

    (insert "\n")))

(provide 'ores-dashboard)
;;; ores-dashboard.el ends here
