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
;; Opens a buffer named "ORES Dashboard - <label>" (from ORES_CHECKOUT_LABEL)
;; with a banner image, then 3-column rows of box-bordered command groups.
;; All state is buffer-local; multiple checkouts can have independent
;; dashboards open simultaneously.
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

(defface ores/dashboard-group-title-face
  '((((background dark))  (:foreground "#88c0d0" :weight bold))
    (((background light)) (:foreground "#2060a8" :weight bold)))
  "Group heading inside a box.")

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

(defvar-local ores/dashboard--env  nil)
(defvar-local ores/dashboard--root nil)

;; ---------------------------------------------------------------------------
;; Mode
;; ---------------------------------------------------------------------------

(defvar ores/dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'ores/dashboard-reload)
    (define-key m (kbd "q") #'quit-window)
    m))

(define-derived-mode ores/dashboard-mode special-mode "ORES-Dashboard"
  "Major mode for the ORE Studio development console.
All interaction is via clickable items; \\[ores/dashboard-reload] reloads .env.")

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
    (pop-to-buffer buf)))

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
;; Icon helper
;; ---------------------------------------------------------------------------

(defun ores/dashboard--icon (fn name)
  "Return a nerd-icons glyph string via FN for NAME, or empty if unavailable."
  (if (and (display-graphic-p) (fboundp fn))
      (concat (funcall fn name :face 'ores/dashboard-group-title-face) " ")
    ""))

;; ---------------------------------------------------------------------------
;; Column geometry
;; ---------------------------------------------------------------------------

(defconst ores/dashboard--ncols 3
  "Number of group columns per row.")

(defconst ores/dashboard--gap 2
  "Character gap between adjacent group boxes.")

(defun ores/dashboard--col-width ()
  "Return box width for each group column."
  (max 30 (/ (- (window-total-width)
                (* (1- ores/dashboard--ncols) ores/dashboard--gap)
                2)               ; 1-char left margin each side
             ores/dashboard--ncols)))

;; ---------------------------------------------------------------------------
;; Group → lines
;;
;; A "group" is: (title icon-fn icon-arg items)
;; items: list of (label . action)  action may be nil for non-clickable rows.
;;
;; Returns a list of cells: (full-text . btn-specs)
;; btn-specs: list of (char-offset char-len action)
;; ---------------------------------------------------------------------------

(defun ores/dashboard--rpad (str width)
  "Return STR right-padded (or truncated) to WIDTH characters."
  (let ((w (string-width str)))
    (cond ((= w width) str)
          ((> w width) (truncate-string-to-width str width))
          (t (concat str (make-string (- width w) ?\s))))))

(defun ores/dashboard--border (l m r width)
  "Build a border line: L + M*(width-2) + R, all with border face."
  (propertize (concat (string l) (make-string (- width 2) m) (string r))
              'face 'ores/dashboard-border-face))

(defun ores/dashboard--group-cells (title icon-fn icon-arg items col-width)
  "Build cells for one group box of COL-WIDTH chars.
Returns list of (text . btn-specs)."
  (let* ((iw   (- col-width 2))   ; inner width between │ chars
         (icon (ores/dashboard--icon icon-fn icon-arg))
         cells)

    ;; ┌────────────────────┐
    (push (cons (ores/dashboard--border ?┌ ?─ ?┐ col-width) nil) cells)

    ;; │ icon Title         │  (no button)
    (let* ((hdr (concat " " icon title))
           (inner (propertize (ores/dashboard--rpad hdr iw)
                              'face 'ores/dashboard-group-title-face)))
      (push (cons (concat (propertize "│" 'face 'ores/dashboard-border-face)
                          inner
                          (propertize "│" 'face 'ores/dashboard-border-face))
                  nil)
            cells))

    ;; ├────────────────────┤
    (push (cons (ores/dashboard--border ?├ ?─ ?┤ col-width) nil) cells)

    ;; │  label             │  (button if action non-nil)
    (dolist (item items)
      (let* ((label  (car item))
             (action (cdr item))
             (prefix "  ")
             (max-l  (- iw (length prefix) 1))
             (lbl    (ores/dashboard--rpad label max-l))
             (pad    (make-string (- iw (length prefix) (string-width lbl) 1) ?\s))
             ;; char offset of the label within the full cell string:
             ;; 1 (│) + length(prefix) = 3
             (off (+ 1 (length prefix)))
             (len (string-width lbl)))
        (push (cons
               (concat (propertize "│" 'face 'ores/dashboard-border-face)
                       prefix lbl pad " "
                       (propertize "│" 'face 'ores/dashboard-border-face))
               (when action (list (list off len action))))
              cells)))

    ;; └────────────────────┘
    (push (cons (ores/dashboard--border ?└ ?─ ?┘ col-width) nil) cells)

    (nreverse cells)))

;; ---------------------------------------------------------------------------
;; Row rendering
;; ---------------------------------------------------------------------------

(defun ores/dashboard--render-row (groups col-width)
  "Insert GROUPS side by side, each box COL-WIDTH chars wide."
  (let* ((gap     ores/dashboard--gap)
         (ncols   (length groups))
         ;; Build cells for each group
         (all-cells (mapcar (lambda (g)
                              (ores/dashboard--group-cells
                               (nth 0 g) (nth 1 g) (nth 2 g) (nth 3 g) col-width))
                            groups))
         (max-h (apply #'max (mapcar #'length all-cells)))
         (blank (make-string col-width ?\s)))

    ;; Pad each group to max-h rows
    (setq all-cells
          (mapcar (lambda (cells)
                    (append cells
                            (make-list (- max-h (length cells))
                                       (cons blank nil))))
                  all-cells))

    (dotimes (i max-h)
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
      (insert "\n"))
    (insert "\n")))

;; ---------------------------------------------------------------------------
;; Image banner
;; ---------------------------------------------------------------------------

(defun ores/dashboard--insert-image (root)
  "Insert the ORE Studio banner image centred at the top, if displayable."
  (when (and (display-graphic-p) (image-type-available-p 'png))
    (let ((path (expand-file-name "projects/modeling/ores.png" root)))
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

(defun ores/dashboard--insert-quicklinks (root)
  "Insert a horizontal bar of quick-access file links below the banner."
  (let ((links '(("Orientation" . "doc/manual/user_guide/user_manual.org")
                 ("Compass"     . "doc/compass.org")
                 ("Agile"       . "doc/agile/agile.org")))
        (sep   "    "))
    (insert "  ")
    (dolist (link links)
      (let ((label (car link))
            (path  (expand-file-name (cdr link) root)))
        (insert (propertize (format "[ %s ]" label)
                            'face       'ores/dashboard-quicklink-face
                            'mouse-face 'ores/dashboard-quicklink-hover-face
                            'ores/action (let ((f path))
                                           (lambda (_) (find-file f)))
                            'keymap     ores/dashboard--button-map
                            'help-echo  (format "Open %s" (cdr link))))
        (insert sep)))
    (insert "\n\n")))

;; ---------------------------------------------------------------------------
;; Helper: run a shell command in a compilation buffer
;; ---------------------------------------------------------------------------

(defun ores/dashboard--compile (cmd buf-name &optional root)
  "Run CMD in *ores-<buf-name>*, with default-directory set to ROOT."
  (let ((default-directory (or root default-directory)))
    (compilation-start cmd nil (lambda (_) (format "*ores-%s*" buf-name)))))

;; ---------------------------------------------------------------------------
;; Group builders — each returns (title icon-fn icon-arg items)
;; ---------------------------------------------------------------------------

(defun ores/dashboard--env-group (env root)
  (list "Environment" 'nerd-icons-codicon "nf-cod-settings_gear"
        (list
         (cons "Init environment"
               (let ((r root)
                     (p (or (cdr (assoc "ORES_PRESET" env)) "linux-clang-debug-ninja")))
                 (lambda (_)
                   (ores/dashboard--compile
                    (format "%s -y --preset %s"
                            (expand-file-name "build/scripts/init-environment.sh" r) p)
                    "init-environment" r))))
         (cons "Reload environment"
               (lambda (_) (ores/dashboard-reload)))
         (cons "Diff .env vs .env.old"
               (let ((r root))
                 (lambda (_)
                   (let ((old (expand-file-name ".env.old" r))
                         (cur (expand-file-name ".env" r)))
                     (if (file-exists-p old)
                         (diff old cur)
                       (user-error "No .env.old — run init-environment.sh twice")))))))))

(defun ores/dashboard--db-group (env _root)
  (list "Database" 'nerd-icons-codicon "nf-cod-database"
        (list
         (cons "Recreate environment"
               (let ((e env))
                 (lambda (_)
                   (let ((lbl (or (cdr (assoc "ORES_CHECKOUT_LABEL" e)) "local1")))
                     (unless (yes-or-no-p (format "DROP and recreate ores_dev_%s? " lbl))
                       (user-error "Aborted"))
                     (ores-db/--run-recreate-env lbl)))))
         (cons "Browse databases"
               (lambda (_) (ores-db/list-databases)))
         (cons "Setup SQL connections"
               (lambda (_) (ores-db/setup-connections))))))

(defun ores/dashboard--services-group (env root)
  (list "Services" 'nerd-icons-faicon "nf-fa-server"
        (mapcar (lambda (spec)
                  (cons (nth 0 spec)
                        (let ((s (expand-file-name (nth 1 spec)
                                                   (expand-file-name "build/scripts" root)))
                              (b (nth 2 spec))
                              (r root))
                          (lambda (_) (ores/dashboard--compile s b r)))))
                '(("Start services"  "start-services.sh"  "start-services")
                  ("Start client"    "start-client.sh"    "start-client")
                  ("Service status"  "status-services.sh" "service-status")
                  ("Stop services"   "stop-services.sh"   "stop-services")))))

(defun ores/dashboard--compass-group (env root)
  (let ((compass (expand-file-name "projects/ores.compass/compass.sh" root)))
    (list "Compass" 'nerd-icons-faicon "nf-fa-compass"
          (append
           (mapcar (lambda (spec)
                     (cons (nth 0 spec)
                           (let ((cmd (format "%s %s" compass (nth 1 spec)))
                                 (buf (nth 2 spec))
                                 (r root))
                             (lambda (_) (ores/dashboard--compile cmd buf r)))))
                   '(("compass where"  "where"  "compass-where")
                     ("compass list"   "list"   "compass-list")
                     ("compass status" "status" "compass-status")))
           (list (cons "Open compass.org"
                       (let ((r root))
                         (lambda (_)
                           (find-file (expand-file-name "doc/compass.org" r))))))))))

(defun ores/dashboard--build-group (env root)
  (let ((p (or (cdr (assoc "ORES_PRESET" env)) "linux-clang-debug-ninja")))
    (list "Build" 'nerd-icons-codicon "nf-cod-tools"
          (list
           (cons "Configure"
                 (let ((preset p) (r root))
                   (lambda (_)
                     (ores/dashboard--compile
                      (format "cmake --preset %s" preset) "configure" r))))
           (cons "Build"
                 (let ((preset p) (r root))
                   (lambda (_)
                     (ores/dashboard--compile
                      (format "cmake --build --preset %s" preset) "build" r))))
           (cons "Test"
                 (let ((preset p) (r root))
                   (lambda (_)
                     (ores/dashboard--compile
                      (format "cmake --build --preset %s --target test" preset)
                      "test" r))))))))

(defun ores/dashboard--site-group (env root)
  (let ((p    (or (cdr (assoc "ORES_PRESET"    env)) "linux-clang-debug-ninja"))
        (port (or (cdr (assoc "ORES_SITE_PORT" env)) "8000")))
    (list "Site" 'nerd-icons-codicon "nf-cod-globe"
          (list
           (cons "Rebuild site"
                 (let ((preset p) (r root))
                   (lambda (_)
                     (ores/dashboard--compile
                      (format "cmake --build --preset %s --target deploy_site" preset)
                      "rebuild-site" r))))
           (cons "Start site"
                 (let ((r root))
                   (lambda (_)
                     (let* ((script (expand-file-name "build/scripts/serve-site.sh" r))
                            (default-directory r))
                       (start-process "ores-serve-site" "*ores-serve-site*" script)
                       (message "Site server started.")))))
           (cons "Stop site"
                 (let ((pt port))
                   (lambda (_)
                     (shell-command (format "fuser -k %s/tcp 2>/dev/null; true" pt))
                     (message "Site server stopped."))))))))

(defun ores/dashboard--skills-group (env root)
  (let ((p (or (cdr (assoc "ORES_PRESET" env)) "linux-clang-debug-ninja")))
    (list "Skills" 'nerd-icons-codicon "nf-cod-sparkle"
          (list
           (cons "Open skills folder"
                 (let ((r root))
                   (lambda (_)
                     (dired (expand-file-name "doc/llm/skills" r)))))
           (cons "Deploy skills"
                 (let ((preset p) (r root))
                   (lambda (_)
                     (ores/dashboard--compile
                      (format "cmake --build --preset %s --target deploy_skills" preset)
                      "deploy-skills" r))))))))

(defun ores/dashboard--bookmarks-group (env root)
  (let* ((p       (or (cdr (assoc "ORES_PRESET" env)) ""))
         (log-dir (expand-file-name (format "build/output/%s/publish/log" p) root))
         (bin-dir (expand-file-name (format "build/output/%s/publish/bin" p) root)))
    (list "Bookmarks" 'nerd-icons-codicon "nf-cod-bookmark"
          (list
           (cons "Log directory"
                 (let ((d log-dir))
                   (lambda (_)
                     (if (file-directory-p d) (dired d)
                       (user-error "Log dir not found: %s" d)))))
           (cons "Bin directory"
                 (let ((d bin-dir))
                   (lambda (_)
                     (if (file-directory-p d) (dired d)
                       (user-error "Bin dir not found: %s" d)))))))))

(defun ores/dashboard--links-group (_env root)
  (list "Links" 'nerd-icons-codicon "nf-cod-link"
        (mapcar (lambda (spec)
                  (cons (nth 0 spec)
                        (let ((f (expand-file-name (nth 1 spec) root)))
                          (lambda (_) (find-file f)))))
                '(("User manual"   "doc/manual/user_guide/user_manual.org")
                  ("Recipes index" "doc/recipes/recipes.org")
                  ("Agile index"   "doc/agile/agile.org")))))

(defun ores/dashboard--shell-group (env root)
  (list "Shell" 'nerd-icons-codicon "nf-cod-terminal"
        (list
         (cons "Open ORE Studio shell"
               (let ((e env) (r root))
                 (lambda (_)
                   (let* ((p   (or (cdr (assoc "ORES_PRESET" e)) ""))
                          (bin (expand-file-name
                                (format "build/output/%s/publish/bin/ores.shell" p) r)))
                     (if (file-executable-p bin)
                         (progn (setq ores-shell-last-program bin) (ores-shell))
                       (user-error "ores.shell not found at %s — build first" bin)))))))))

;; ---------------------------------------------------------------------------
;; Main render
;; ---------------------------------------------------------------------------

(defun ores/dashboard--render (env root)
  "Erase and fully redraw the dashboard using ENV and ROOT."
  (erase-buffer)
  (let* ((label  (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "unknown"))
         (preset (or (cdr (assoc "ORES_PRESET"          env)) "unknown"))
         (cw     (ores/dashboard--col-width)))

    ;; Banner image
    (ores/dashboard--insert-image root)

    ;; Quick-links bar: Orientation · Compass · Agile
    (ores/dashboard--insert-quicklinks root)

    ;; Title
    (let ((margin "  "))
      (insert margin
              (propertize (format "ORES Dashboard — %s\n" label)
                          'face 'ores/dashboard-title-face))
      (insert margin
              (propertize (format "Preset: %s    (g to reload)\n\n" preset)
                          'face 'ores/dashboard-subtitle-face)))

    ;; Row 1: Environment | Database | Services
    (ores/dashboard--render-row
     (list (ores/dashboard--env-group      env root)
           (ores/dashboard--db-group       env root)
           (ores/dashboard--services-group env root))
     cw)

    ;; Row 2: Compass | Build | Site
    (ores/dashboard--render-row
     (list (ores/dashboard--compass-group env root)
           (ores/dashboard--build-group   env root)
           (ores/dashboard--site-group    env root))
     cw)

    ;; Row 3: Skills | Bookmarks | Links
    (ores/dashboard--render-row
     (list (ores/dashboard--skills-group    env root)
           (ores/dashboard--bookmarks-group env root)
           (ores/dashboard--links-group     env root))
     cw)

    ;; Row 4: Shell (single group)
    (ores/dashboard--render-row
     (list (ores/dashboard--shell-group env root))
     cw)))

(provide 'ores-dashboard)
;;; ores-dashboard.el ends here
