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
;; Per-checkout development console for ORE Studio.
;;
;; Opens a two-panel layout: dashboard fills the top 2/3 of the frame, a
;; persistent output window takes the bottom 1/3.  Every action (compilation,
;; file open, dired, diff) targets the output window — the dashboard stays
;; visible at all times.
;;
;; Usage:
;;   M-x ores/dashboard   — open (or refresh) the dashboard
;;   g                    — reload .env and redraw
;;   q                    — quit

;;; Code:

(require 'ores-env)
(require 'ores-shell)
(require 'ores-db)

;; ---------------------------------------------------------------------------
;; Faces
;; ---------------------------------------------------------------------------

(defface ores/dashboard-title-face
  '((t (:height 1.4 :weight bold)))
  "Dashboard main title.")

(defface ores/dashboard-subtitle-face
  '((t (:inherit shadow)))
  "Dashboard preset / hint line.")

(defface ores/dashboard-border-face
  '((((background dark))  (:foreground "#4a4a5a"))
    (((background light)) (:foreground "#b0b0c0")))
  "Box-drawing border characters.")

(defface ores/dashboard-item-face
  '((((background dark))  (:foreground "#d8dee9"))
    (((background light)) (:foreground "#2e3440")))
  "Clickable item label.")

(defface ores/dashboard-item-hover-face
  '((t (:inherit highlight)))
  "Item label on mouse-over.")

(defface ores/dashboard-quicklink-face
  '((((background dark))  (:foreground "#a3be8c" :underline t))
    (((background light)) (:foreground "#007020" :underline t)))
  "Quick-link labels in the horizontal bar below the banner.")

(defface ores/dashboard-quicklink-hover-face
  '((t (:inherit highlight :underline t)))
  "Quick-link label on mouse-over.")

;; ---------------------------------------------------------------------------
;; Button activation
;; ---------------------------------------------------------------------------

(defvar ores/dashboard--button-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'ores/dashboard--activate)
    (define-key m [mouse-1]   #'ores/dashboard--activate)
    m))

(defun ores/dashboard--activate (&optional _event)
  "Call the action stored at point."
  (interactive)
  (when-let ((action (get-text-property (point) 'ores/action)))
    (funcall action nil)))

;; ---------------------------------------------------------------------------
;; Buffer-local state
;; ---------------------------------------------------------------------------

(defvar-local ores/dashboard--env          nil)
(defvar-local ores/dashboard--root         nil)
(defvar-local ores/dashboard--output-window nil
  "The window below the dashboard where all actions display their output.")

;; ---------------------------------------------------------------------------
;; Mode
;; ---------------------------------------------------------------------------

(defvar ores/dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'ores/dashboard-reload)
    (define-key m (kbd "q") #'quit-window)
    m))

(define-derived-mode ores/dashboard-mode special-mode "ORES-Dashboard"
  "Major mode for the ORE Studio development console.")

;; ---------------------------------------------------------------------------
;; Window helpers
;; ---------------------------------------------------------------------------

(defun ores/dashboard--output-win (dash-buf)
  "Return the live output window for DASH-BUF, or nil."
  (when (buffer-live-p dash-buf)
    (let ((win (buffer-local-value 'ores/dashboard--output-window dash-buf)))
      (when (window-live-p win) win))))

(defun ores/dashboard--display (target-buf dash-buf)
  "Show TARGET-BUF in DASH-BUF's output window; fall back to pop-to-buffer."
  (if-let ((out (ores/dashboard--output-win dash-buf)))
      (set-window-buffer out target-buf)
    (pop-to-buffer target-buf)))

(defun ores/dashboard--setup-layout (buf)
  "Full-frame two-panel layout: BUF on top (2/3), output window below (1/3)."
  (switch-to-buffer buf)
  (delete-other-windows)
  (let* ((out-h   (max 10 (/ (window-total-height) 3)))
         (out-win (split-window-below (- (window-total-height) out-h))))
    (with-current-buffer buf
      (setq ores/dashboard--output-window out-win))
    ;; Leave focus on the dashboard
    (select-window (get-buffer-window buf))))

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
      (setq ores/dashboard--env  env
            ores/dashboard--root root)
      (let ((inhibit-read-only t))
        (ores/dashboard--render env root)))
    (ores/dashboard--setup-layout buf)))

(defun ores/dashboard-reload ()
  "Re-read .env and redraw the current dashboard buffer."
  (interactive)
  (let ((env  (ores/load-dotenv))
        (root (ores/checkout-root)))
    (setq ores/dashboard--env  env
          ores/dashboard--root root)
    (let ((inhibit-read-only t))
      (ores/dashboard--render env root))
    (message "ORES Dashboard reloaded.")))

;; ---------------------------------------------------------------------------
;; Icon helpers
;; ---------------------------------------------------------------------------

(defun ores/dashboard--group-icon (icon-fn icon-arg color)
  "Return a nerd-icons glyph for a group header in COLOR."
  (if (and (display-graphic-p) (fboundp icon-fn))
      (or (ignore-errors
            (concat (funcall icon-fn icon-arg
                             :face `(:foreground ,(or color "#88c0d0") :weight bold))
                    " "))
          "")
    ""))

(defun ores/dashboard--mkitem (label icon-fn icon-name action)
  "Build an item (label . action) with a nerd-icon prefix in label."
  (cons (concat (if (and (display-graphic-p) (fboundp icon-fn))
                    (or (ignore-errors (concat (funcall icon-fn icon-name) " "))
                        "  ")
                  "  ")
                label)
        action))

;; ---------------------------------------------------------------------------
;; Column geometry
;; ---------------------------------------------------------------------------

(defconst ores/dashboard--ncols 3)
(defconst ores/dashboard--gap   2)

(defun ores/dashboard--col-width ()
  (max 30 (/ (- (window-total-width)
                (* (1- ores/dashboard--ncols) ores/dashboard--gap)
                2)
             ores/dashboard--ncols)))

;; ---------------------------------------------------------------------------
;; Group → cells
;;
;; Group spec: (title icon-fn icon-arg items color)
;;
;; Returns exactly TARGET-H cells; blank │ │ rows pad inside the box border so
;; all boxes in a row share the same bottom border line.
;;
;; Cell: (text . btn-specs)   btn-spec: (char-offset char-len action)
;; ---------------------------------------------------------------------------

(defun ores/dashboard--rpad (str width)
  (let ((w (string-width str)))
    (cond ((= w width) str)
          ((> w width) (truncate-string-to-width str width))
          (t (concat str (make-string (- width w) ?\s))))))

(defun ores/dashboard--border (l m r width)
  (propertize (concat (string l) (make-string (- width 2) m) (string r))
              'face 'ores/dashboard-border-face))

(defun ores/dashboard--group-cells (title icon-fn icon-arg items col-width color target-h)
  (let* ((iw         (- col-width 2))
         (icon       (ores/dashboard--group-icon icon-fn icon-arg color))
         (title-face `(:foreground ,(or color "#88c0d0") :weight bold))
         (extra      (max 0 (- target-h (+ 4 (length items)))))
         cells)

    (push (cons (ores/dashboard--border ?┌ ?─ ?┐ col-width) nil) cells)

    (let* ((hdr   (concat " " icon title))
           (inner (propertize (ores/dashboard--rpad hdr iw) 'face title-face)))
      (push (cons (concat (propertize "│" 'face 'ores/dashboard-border-face)
                          inner
                          (propertize "│" 'face 'ores/dashboard-border-face))
                  nil)
            cells))

    (push (cons (ores/dashboard--border ?├ ?─ ?┤ col-width) nil) cells)

    (dolist (item items)
      (let* ((label  (car item))
             (action (cdr item))
             (prefix " ")
             (max-l  (- iw (length prefix) 1))
             (lbl    (ores/dashboard--rpad label max-l))
             (pad    (make-string (max 0 (- iw (length prefix) (string-width lbl) 1)) ?\s))
             (off    (+ 1 (length prefix)))
             (len    (string-width lbl)))
        (push (cons
               (concat (propertize "│" 'face 'ores/dashboard-border-face)
                       prefix lbl pad " "
                       (propertize "│" 'face 'ores/dashboard-border-face))
               (when action (list (list off len action))))
              cells)))

    (let ((blank (concat (propertize "│" 'face 'ores/dashboard-border-face)
                         (make-string iw ?\s)
                         (propertize "│" 'face 'ores/dashboard-border-face))))
      (dotimes (_ extra)
        (push (cons blank nil) cells)))

    (push (cons (ores/dashboard--border ?└ ?─ ?┘ col-width) nil) cells)

    (nreverse cells)))

;; ---------------------------------------------------------------------------
;; Row rendering
;; ---------------------------------------------------------------------------

(defun ores/dashboard--render-row (groups col-width)
  (let* ((gap      ores/dashboard--gap)
         (ncols    (length groups))
         (target-h (apply #'max
                          (mapcar (lambda (g) (+ 4 (length (nth 3 g)))) groups)))
         (all-cells (mapcar (lambda (g)
                              (ores/dashboard--group-cells
                               (nth 0 g) (nth 1 g) (nth 2 g) (nth 3 g)
                               col-width (nth 4 g) target-h))
                            groups)))

    (dotimes (i target-h)
      (cl-loop for gcells in all-cells
               for gi    from 0
               do
               (let* ((cell      (nth i gcells))
                      (cell-text (car cell))
                      (btn-specs (cdr cell))
                      (col-start (point)))
                 (insert cell-text)
                 (dolist (spec btn-specs)
                   (let* ((off (nth 0 spec))
                          (len (nth 1 spec))
                          (act (nth 2 spec))
                          (s   (+ col-start off))
                          (e   (+ s len)))
                     (add-text-properties s e
                       `(face        ores/dashboard-item-face
                         mouse-face  ores/dashboard-item-hover-face
                         ores/action ,act
                         keymap      ,ores/dashboard--button-map
                         help-echo   "RET or click to activate")))))
               (when (< gi (1- ncols))
                 (insert (make-string gap ?\s))))
      (delete-region (save-excursion (skip-chars-backward " ") (point)) (point))
      (insert "\n"))
    (insert "\n")))

;; ---------------------------------------------------------------------------
;; Image banner
;; ---------------------------------------------------------------------------

(defun ores/dashboard--insert-image (root)
  (when (and (display-graphic-p) (image-type-available-p 'png))
    (let ((path (expand-file-name "assets/images/splash-screen.png" root)))
      (when (file-readable-p path)
        (let* ((avail-px  (- (window-pixel-width) 80))
               (img       (create-image path 'png nil :width avail-px))
               (img-h-px  (cdr (image-size img t)))
               (lines     (max 1 (ceiling img-h-px (frame-char-height))))
               (margin    (max 0 (/ (- (window-total-width)
                                       (/ avail-px (frame-char-width)))
                                    2))))
          (insert (make-string margin ?\s))
          (insert (propertize " " 'display img))
          (insert (make-string (max 0 (- lines 1)) ?\n))
          (insert "\n"))))))

;; ---------------------------------------------------------------------------
;; Quick-links bar
;; ---------------------------------------------------------------------------

(defun ores/dashboard--insert-quicklinks (root dash-buf)
  (let ((links '(("Orientation" . "doc/orientation.org")
                 ("Manual"      . "doc/manual/user_guide/user_manual.org")
                 ("Compass"     . "doc/compass.org")
                 ("Agile"       . "doc/agile/agile.org")))
        (sep "    "))
    (insert "  ")
    (dolist (link links)
      (let ((label (car link))
            (path  (expand-file-name (cdr link) root)))
        (insert (propertize (format "[ %s ]" label)
                            'face       'ores/dashboard-quicklink-face
                            'mouse-face 'ores/dashboard-quicklink-hover-face
                            'ores/action (let ((f path) (db dash-buf))
                                           (lambda (_)
                                             (ores/dashboard--display
                                              (find-file-noselect f) db)))
                            'keymap     ores/dashboard--button-map
                            'help-echo  (format "Open %s" (cdr link))))
        (insert sep)))
    (insert "\n\n")))

;; ---------------------------------------------------------------------------
;; Compilation helper — output goes to the dashboard's output window
;; ---------------------------------------------------------------------------

(defun ores/dashboard--compile (label cmd buf-suffix root dash-buf)
  "Run CMD in *ores-LABEL-BUF-SUFFIX*; display result in DASH-BUF's output window."
  (let* ((default-directory (or root default-directory))
         (buf-name (format "*ores-%s-%s*" label buf-suffix))
         (comp-buf (compilation-start cmd nil (lambda (_) buf-name))))
    (ores/dashboard--display comp-buf dash-buf)))

;; ---------------------------------------------------------------------------
;; Group builders — each returns (title icon-fn icon-arg items color)
;; ---------------------------------------------------------------------------

(defun ores/dashboard--env-group (env root dash-buf)
  (let* ((label  (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local"))
         (preset (or (cdr (assoc "ORES_PRESET"         env)) "linux-clang-debug-ninja")))
    (list "Environment" 'nerd-icons-codicon "nf-cod-settings_gear"
          (list
           (ores/dashboard--mkitem
            "Init environment" 'nerd-icons-codicon "nf-cod-play"
            (let ((lbl label) (p preset) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "%s -y --preset %s"
                         (expand-file-name "build/scripts/init-environment.sh" r) p)
                 "init-environment" r db))))
           (ores/dashboard--mkitem
            "Edit environment" 'nerd-icons-codicon "nf-cod-edit"
            (let ((r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--display (find-file-noselect (expand-file-name ".env" r)) db))))
           (ores/dashboard--mkitem
            "Reload environment" 'nerd-icons-codicon "nf-cod-refresh"
            (lambda (_) (ores/dashboard-reload)))
           (ores/dashboard--mkitem
            "Diff .env vs .env.old" 'nerd-icons-codicon "nf-cod-diff"
            (let ((r root) (db dash-buf))
              (lambda (_)
                (let ((old (expand-file-name ".env.old" r))
                      (cur (expand-file-name ".env" r)))
                  (if (file-exists-p old)
                      (ores/dashboard--display (diff-no-select old cur) db)
                    (user-error "No .env.old — run init-environment.sh twice")))))))
          "#a3be8c")))

(defun ores/dashboard--db-group (env root dash-buf)
  (let ((label (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local")))
    (list "Database" 'nerd-icons-codicon "nf-cod-database"
          (list
           (ores/dashboard--mkitem
            "Recreate environment" 'nerd-icons-codicon "nf-cod-trash"
            (let ((lbl label) (r root))
              (lambda (_)
                (unless (yes-or-no-p (format "DROP and recreate ores_dev_%s? " lbl))
                  (user-error "Aborted"))
                (ores-db/--run-recreate-env lbl r))))
           (ores/dashboard--mkitem
            "Browse databases" 'nerd-icons-codicon "nf-cod-table"
            (lambda (_) (ores-db/list-databases)))
           (ores/dashboard--mkitem
            "Setup SQL connections" 'nerd-icons-codicon "nf-cod-plug"
            (lambda (_) (ores-db/setup-connections))))
          "#88c0d0")))

(defun ores/dashboard--services-group (env root dash-buf)
  (let ((label (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local")))
    (list "Services" 'nerd-icons-faicon "nf-fa-server"
          (mapcar
           (lambda (spec)
             (ores/dashboard--mkitem
              (nth 0 spec) (nth 3 spec) (nth 4 spec)
              (let ((lbl label)
                    (s   (expand-file-name (nth 1 spec)
                                           (expand-file-name "build/scripts" root)))
                    (sfx (nth 2 spec))
                    (r   root)
                    (db  dash-buf))
                (lambda (_) (ores/dashboard--compile lbl s sfx r db)))))
           '(("Start services" "start-services.sh"  "start-services"
              nerd-icons-codicon "nf-cod-run-above")
             ("Start client"   "start-client.sh"    "start-client"
              nerd-icons-codicon "nf-cod-run-below")
             ("Service status" "status-services.sh" "service-status"
              nerd-icons-codicon "nf-cod-pulse")
             ("Stop services"  "stop-services.sh"   "stop-services"
              nerd-icons-codicon "nf-cod-debug-stop")))
          "#ebcb8b")))

(defun ores/dashboard--compass-group (env root dash-buf)
  (let* ((label   (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local"))
         (compass (expand-file-name "projects/ores.compass/compass.sh" root)))
    (list "Compass" 'nerd-icons-faicon "nf-fa-compass"
          (append
           (mapcar
            (lambda (spec)
              (ores/dashboard--mkitem
               (nth 0 spec) (nth 3 spec) (nth 4 spec)
               (let ((lbl label)
                     (cmd (format "%s %s" compass (nth 1 spec)))
                     (sfx (nth 2 spec))
                     (r   root)
                     (db  dash-buf))
                 (lambda (_) (ores/dashboard--compile lbl cmd sfx r db)))))
            '(("compass where"  "where"  "compass-where"
               nerd-icons-codicon "nf-cod-location")
              ("compass list"   "list"   "compass-list"
               nerd-icons-codicon "nf-cod-list-tree")
              ("compass status" "status" "compass-status"
               nerd-icons-codicon "nf-cod-pulse")))
           (list
            (ores/dashboard--mkitem
             "Open compass.org" 'nerd-icons-codicon "nf-cod-file"
             (let ((r root) (db dash-buf))
               (lambda (_)
                 (ores/dashboard--display
                  (find-file-noselect (expand-file-name "doc/compass.org" r)) db))))))
          "#d08770")))

(defun ores/dashboard--build-group (env root dash-buf)
  (let* ((label  (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local"))
         (preset (or (cdr (assoc "ORES_PRESET"         env)) "linux-clang-debug-ninja")))
    (list "Build" 'nerd-icons-codicon "nf-cod-tools"
          (list
           (ores/dashboard--mkitem
            "Configure" 'nerd-icons-codicon "nf-cod-gear"
            (let ((lbl label) (p preset) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "cmake --preset %s" p) "configure" r db))))
           (ores/dashboard--mkitem
            "Build" 'nerd-icons-codicon "nf-cod-play"
            (let ((lbl label) (p preset) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "cmake --build --preset %s" p) "build" r db))))
           (ores/dashboard--mkitem
            "Test" 'nerd-icons-codicon "nf-cod-beaker"
            (let ((lbl label) (p preset) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "cmake --build --preset %s --target test" p) "test" r db)))))
          "#bf616a")))

(defun ores/dashboard--site-group (env root dash-buf)
  (let* ((label  (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local"))
         (preset (or (cdr (assoc "ORES_PRESET"         env)) "linux-clang-debug-ninja"))
         (port   (or (cdr (assoc "ORES_SITE_PORT"      env)) "8000")))
    (list "Site" 'nerd-icons-codicon "nf-cod-globe"
          (list
           (ores/dashboard--mkitem
            "Rebuild site" 'nerd-icons-codicon "nf-cod-refresh"
            (let ((lbl label) (p preset) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "cmake --build --preset %s --target deploy_site" p)
                 "rebuild-site" r db))))
           (ores/dashboard--mkitem
            "Start site" 'nerd-icons-codicon "nf-cod-play"
            (let ((r root))
              (lambda (_)
                (let ((script (expand-file-name "build/scripts/serve-site.sh" r))
                      (default-directory r))
                  (start-process "ores-serve-site" "*ores-serve-site*" script)
                  (message "Site server started.")))))
           (ores/dashboard--mkitem
            "Stop site" 'nerd-icons-codicon "nf-cod-debug-stop"
            (let ((pt port))
              (lambda (_)
                (shell-command (format "fuser -k %s/tcp 2>/dev/null; true" pt))
                (message "Site server stopped.")))))
          "#b48ead")))

(defun ores/dashboard--skills-group (env root dash-buf)
  (let* ((label  (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local"))
         (preset (or (cdr (assoc "ORES_PRESET"         env)) "linux-clang-debug-ninja")))
    (list "Skills" 'nerd-icons-codicon "nf-cod-sparkle"
          (list
           (ores/dashboard--mkitem
            "Open skills folder" 'nerd-icons-codicon "nf-cod-folder-opened"
            (let ((r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--display
                 (dired-noselect (expand-file-name "doc/llm/skills" r)) db))))
           (ores/dashboard--mkitem
            "Deploy skills" 'nerd-icons-codicon "nf-cod-cloud-upload"
            (let ((lbl label) (p preset) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "cmake --build --preset %s --target deploy_skills" p)
                 "deploy-skills" r db)))))
          "#8fbcbb")))

(defun ores/dashboard--bookmarks-group (env root dash-buf)
  (let* ((p       (or (cdr (assoc "ORES_PRESET" env)) ""))
         (log-dir (expand-file-name (format "build/output/%s/publish/log" p) root))
         (bin-dir (expand-file-name (format "build/output/%s/publish/bin" p) root)))
    (list "Bookmarks" 'nerd-icons-codicon "nf-cod-bookmark"
          (list
           (ores/dashboard--mkitem
            "Log directory" 'nerd-icons-codicon "nf-cod-output"
            (let ((d log-dir) (db dash-buf))
              (lambda (_)
                (if (file-directory-p d)
                    (ores/dashboard--display (dired-noselect d) db)
                  (user-error "Log dir not found: %s" d)))))
           (ores/dashboard--mkitem
            "Bin directory" 'nerd-icons-codicon "nf-cod-folder-opened"
            (let ((d bin-dir) (db dash-buf))
              (lambda (_)
                (if (file-directory-p d)
                    (ores/dashboard--display (dired-noselect d) db)
                  (user-error "Bin dir not found: %s" d))))))
          "#5e81ac")))

(defun ores/dashboard--links-group (_env root dash-buf)
  (list "Links" 'nerd-icons-codicon "nf-cod-link"
        (mapcar (lambda (spec)
                  (ores/dashboard--mkitem
                   (nth 0 spec) 'nerd-icons-codicon (nth 2 spec)
                   (let ((f  (expand-file-name (nth 1 spec) root))
                         (db dash-buf))
                     (lambda (_)
                       (ores/dashboard--display (find-file-noselect f) db)))))
                '(("User manual"   "doc/manual/user_guide/user_manual.org" "nf-cod-book")
                  ("Recipes index" "doc/recipes/recipes.org"               "nf-cod-library")
                  ("Agile index"   "doc/agile/agile.org"                   "nf-cod-project")))
        "#e5e9f0"))

(defun ores/dashboard--shell-group (env root _dash-buf)
  (list "Shell" 'nerd-icons-codicon "nf-cod-terminal"
        (list
         (ores/dashboard--mkitem
          "Open ORE Studio shell" 'nerd-icons-codicon "nf-cod-terminal"
          (let ((e env) (r root))
            (lambda (_)
              (let* ((p   (or (cdr (assoc "ORES_PRESET" e)) ""))
                     (bin (expand-file-name
                           (format "build/output/%s/publish/bin/ores.shell" p) r)))
                (if (file-executable-p bin)
                    (progn (setq ores-shell-last-program bin) (ores-shell))
                  (user-error "ores.shell not found at %s — build first" bin)))))))
        "#a3be8c"))

;; ---------------------------------------------------------------------------
;; Main render
;; ---------------------------------------------------------------------------

(defun ores/dashboard--render (env root)
  "Erase and fully redraw the dashboard using ENV and ROOT."
  (erase-buffer)
  (let* ((label    (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "unknown"))
         (preset   (or (cdr (assoc "ORES_PRESET"          env)) "unknown"))
         (cw       (ores/dashboard--col-width))
         (dash-buf (current-buffer)))

    (ores/dashboard--insert-image root)
    (ores/dashboard--insert-quicklinks root dash-buf)

    (let ((margin "  "))
      (insert margin
              (propertize (format "ORES Dashboard — %s\n" label)
                          'face 'ores/dashboard-title-face))
      (insert margin
              (propertize (format "Preset: %s    (g to reload)\n\n" preset)
                          'face 'ores/dashboard-subtitle-face)))

    (ores/dashboard--render-row
     (list (ores/dashboard--env-group      env root dash-buf)
           (ores/dashboard--db-group       env root dash-buf)
           (ores/dashboard--services-group env root dash-buf))
     cw)

    (ores/dashboard--render-row
     (list (ores/dashboard--compass-group env root dash-buf)
           (ores/dashboard--build-group   env root dash-buf)
           (ores/dashboard--site-group    env root dash-buf))
     cw)

    (ores/dashboard--render-row
     (list (ores/dashboard--skills-group    env root dash-buf)
           (ores/dashboard--bookmarks-group env root dash-buf)
           (ores/dashboard--links-group     env root dash-buf))
     cw)

    (ores/dashboard--render-row
     (list (ores/dashboard--shell-group env root dash-buf))
     cw)))

(provide 'ores-dashboard)
;;; ores-dashboard.el ends here
