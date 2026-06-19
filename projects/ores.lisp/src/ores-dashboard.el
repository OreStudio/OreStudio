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
;; Opens a single-panel layout: dashboard fills the full frame.  The
;; output window (1/3 of frame height) is created lazily when the first
;; action is triggered — the dashboard stays visible at all times.
;;
;; Usage:
;;   M-x ores/dashboard   — open (or refresh) the dashboard
;;   g                    — reload .env and redraw
;;   q                    — quit

;;; Code:

(require 'ores-env)
(require 'ores-shell)
(require 'ores-db)
(require 'transient)
(require 'sql)

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

;; Per-group title faces (Nord palette) — named faces are more reliable
;; than anonymous face specs across themes.
(defface ores/dashboard-group-env-face
  '((t (:foreground "#a3be8c" :weight bold)))
  "Group title face for Environment card.")

(defface ores/dashboard-group-db-face
  '((t (:foreground "#88c0d0" :weight bold)))
  "Group title face for Database card.")

(defface ores/dashboard-group-services-face
  '((t (:foreground "#ebcb8b" :weight bold)))
  "Group title face for Services card.")

(defface ores/dashboard-group-compass-face
  '((t (:foreground "#d08770" :weight bold)))
  "Group title face for Compass card.")

(defface ores/dashboard-group-build-face
  '((t (:foreground "#bf616a" :weight bold)))
  "Group title face for Build card.")

(defface ores/dashboard-group-site-face
  '((t (:foreground "#b48ead" :weight bold)))
  "Group title face for Site card.")

(defface ores/dashboard-group-skills-face
  '((t (:foreground "#8fbcbb" :weight bold)))
  "Group title face for Skills card.")

(defface ores/dashboard-group-bookmarks-face
  '((t (:foreground "#5e81ac" :weight bold)))
  "Group title face for Bookmarks card.")

(defface ores/dashboard-group-links-face
  '((t (:foreground "#e5e9f0" :weight bold)))
  "Group title face for Links card.")

(defface ores/dashboard-group-shell-face
  '((t (:foreground "#a3be8c" :weight bold)))
  "Group title face for Shell card.")

(defface ores/dashboard-group-nats-face
  '((t (:foreground "#81a1c1" :weight bold)))
  "Group title face for NATS card.")

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
;; Item shortcut keys — global unique a-z A-Z assigned during render
;; ---------------------------------------------------------------------------

(defconst ores/dashboard--key-alphabet
  ;; g and q are reserved for mode-level bindings (reload / quit-window).
  "abcdefhijklmnoprstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun ores/dashboard--alloc-key ()
  "Return the next shortcut character for a dashboard item, or nil when exhausted."
  (let ((idx ores/dashboard--next-key-idx))
    (when (< idx (length ores/dashboard--key-alphabet))
      (cl-incf ores/dashboard--next-key-idx)
      (aref ores/dashboard--key-alphabet idx))))

;; ---------------------------------------------------------------------------
;; Buffer-local state
;; ---------------------------------------------------------------------------

(defvar-local ores/dashboard--env          nil)
(defvar-local ores/dashboard--root         nil)
(defvar-local ores/dashboard--item-keys    nil)
(defvar-local ores/dashboard--next-key-idx 0)
;; ---------------------------------------------------------------------------
;; Mode
;; ---------------------------------------------------------------------------

(defvar ores/dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'ores/dashboard-reload)
    (define-key m (kbd "q") #'quit-window)
    m))

(define-derived-mode ores/dashboard-mode special-mode "ORES-Dashboard"
  "Major mode for the ORE Studio development console."
  ;; Whitespace-mode highlights entire lines and trailing spaces, which
  ;; overrides the character-level face properties used for card title colors.
  (when (fboundp 'whitespace-mode)
    (whitespace-mode -1))
  (setq-local show-trailing-whitespace nil))

;; ---------------------------------------------------------------------------
;; Window helpers — output buffers are displayed full-frame
;; ---------------------------------------------------------------------------

(defun ores/dashboard--display (target-buf _dash-buf)
  "Show TARGET-BUF full-frame (same as dashboard layout)."
  (switch-to-buffer target-buf)
  (delete-other-windows))

(defun ores/dashboard--setup-layout (buf)
  "Full-frame layout: BUF fills the frame.  Output window is created on demand."
  (switch-to-buffer buf)
  (delete-other-windows))

;; ---------------------------------------------------------------------------
;; Entry points
;; ---------------------------------------------------------------------------

(defun ores/dashboard ()
  "Open the ORE Studio dashboard for the current checkout."
  (interactive)
  (let* ((env   (ores/load-dotenv))
         (label (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "unknown"))
         (root  (ores/checkout-root))
         (buf   (get-buffer-create (format "ORE Studio Dashboard - %s" label))))
    (with-current-buffer buf
      (ores/dashboard-mode)
      (setq ores/dashboard--env  env
            ores/dashboard--root root
            default-directory    root)
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
    (message "ORE Studio Dashboard reloaded.")))

;; ---------------------------------------------------------------------------
;; Icon helpers
;; ---------------------------------------------------------------------------

(defun ores/dashboard--group-icon (icon-fn icon-arg)
  "Return a nerd-icons glyph string for a group header, or empty string."
  (if (and (display-graphic-p) (fboundp icon-fn))
      (or (ignore-errors (concat (funcall icon-fn icon-arg) " ")) "")
    ""))

(defun ores/dashboard--mkitem (label icon-fn icon-name action)
  "Build an item (label . action) with a nerd-icon prefix in label.
Falls back to two spaces when icons are unavailable, preserving alignment."
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
;; Group spec: (title icon-fn icon-arg items title-face)
;;   title-face — a named face symbol for the group title text
;;
;; ores/dashboard--group-cells builds exactly TARGET-H cells so all boxes in
;; a row share the same bottom border line (blank │ │ rows added inside).
;;
;; Cell: (text . btn-specs)    btn-spec: (char-offset char-len action)
;; ---------------------------------------------------------------------------

(defun ores/dashboard--border (l m r width)
  (propertize (concat (string l) (make-string (- width 2) m) (string r))
              'face 'ores/dashboard-border-face))

(defun ores/dashboard--group-cells (title icon-fn icon-arg items col-width title-face target-h)
  "Build exactly TARGET-H cells for a group box of COL-WIDTH chars."
  (let* ((iw       (- col-width 2))
         (icon     (ores/dashboard--group-icon icon-fn icon-arg))
         (icon-w   (string-width icon))
         (max-t    (max 0 (- iw 1 icon-w)))
         (title-d  (if (> (string-width title) max-t)
                       (truncate-string-to-width title max-t)
                     title))
         (title-s  (propertize title-d 'face title-face))
         (hdr-w    (+ 1 icon-w (string-width title-d)))
         (hdr-pad  (make-string (max 0 (- iw hdr-w)) ?\s))
         (extra    (max 0 (- target-h (+ 4 (length items)))))
         cells)

    ;; ┌────────────────────┐
    (push (cons (ores/dashboard--border ?┌ ?─ ?┐ col-width) nil) cells)

    ;; │ icon Title...      │
    (push (cons (concat (propertize "│" 'face 'ores/dashboard-border-face)
                        " " icon title-s hdr-pad
                        (propertize "│" 'face 'ores/dashboard-border-face))
                nil)
          cells)

    ;; ├────────────────────┤
    (push (cons (ores/dashboard--border ?├ ?─ ?┤ col-width) nil) cells)

    ;; │  item              │
    (dolist (item items)
      (let* ((label  (car item))
             (action (cdr item))
             (key-ch (when action (ores/dashboard--alloc-key)))
             (hint   (if key-ch
                         (propertize (format " (%c)" key-ch) 'face 'shadow)
                       ""))
             (label  (concat label hint))
             (prefix " ")
             (max-l  (- iw (length prefix) 1))
             (lbl    (if (> (string-width label) max-l)
                         (truncate-string-to-width label max-l)
                       label))
             (pad    (make-string (max 0 (- iw (length prefix) (string-width lbl) 1)) ?\s))
             (off    (+ 1 (length prefix)))
             (len    (string-width lbl)))
        (when (and action key-ch ores/dashboard--item-keys)
          (let ((act action))
            (define-key ores/dashboard--item-keys
              (kbd (string key-ch))
              (lambda () (interactive) (funcall act nil)))))
        (push (cons
               (concat (propertize "│" 'face 'ores/dashboard-border-face)
                       prefix lbl pad " "
                       (propertize "│" 'face 'ores/dashboard-border-face))
               (when action (list (list off len action))))
              cells)))

    ;; blank │ │ rows inside box for equal row height
    (let ((blank (concat (propertize "│" 'face 'ores/dashboard-border-face)
                         (make-string iw ?\s)
                         (propertize "│" 'face 'ores/dashboard-border-face))))
      (dotimes (_ extra) (push (cons blank nil) cells)))

    ;; └────────────────────┘
    (push (cons (ores/dashboard--border ?└ ?─ ?┘ col-width) nil) cells)

    (nreverse cells)))

;; ---------------------------------------------------------------------------
;; Row rendering
;; ---------------------------------------------------------------------------

(defun ores/dashboard--render-row (groups col-width)
  "Insert GROUPS side by side, each box COL-WIDTH chars wide, all equal height."
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
      ;; strip trailing spaces — avoids whitespace-mode false positives
      (delete-region (save-excursion (skip-chars-backward " ") (point)) (point))
      (insert "\n"))
    (insert "\n")))

;; ---------------------------------------------------------------------------
;; Image banner — natural file size, no scaling, centered
;; ---------------------------------------------------------------------------

(defun ores/dashboard--insert-image (root)
  "Insert the ORE Studio banner image at its natural size, horizontally centered."
  (when (and (display-graphic-p) (image-type-available-p 'png))
    (let ((path (expand-file-name "assets/images/modern-icon.png" root)))
      (when (file-readable-p path)
        (let* ((img      (create-image path 'png nil))
               (img-w-px (car (image-size img t)))
               (char-w   (frame-char-width))
               (img-cols (ceiling img-w-px char-w))
               (margin   (max 0 (/ (- (window-total-width) img-cols) 2))))
          (insert (make-string margin ?\s))
          (insert (propertize " " 'display img))
          (insert "\n\n"))))))

;; ---------------------------------------------------------------------------
;; Quick-links bar — centered, no trailing separator
;; ---------------------------------------------------------------------------

(defun ores/dashboard--insert-quicklinks (root dash-buf)
  "Insert a centered horizontal bar of quick-access file links below the banner."
  (let* ((links '(("Orientation" . "doc/orientation.org")
                  ("Manual"      . "doc/manual/user_guide/user_manual.org")
                  ("Compass"     . "doc/compass.org")
                  ("Agile"       . "doc/agile/agile.org")))
         (sep "    ")
         (link-strs
          (mapcar (lambda (link)
                    (let ((label (car link))
                          (path  (expand-file-name (cdr link) root)))
                      (propertize (format "[ %s ]" label)
                                  'face       'ores/dashboard-quicklink-face
                                  'mouse-face 'ores/dashboard-quicklink-hover-face
                                  'ores/action (let ((f path) (db dash-buf))
                                                 (lambda (_)
                                                   (ores/dashboard--display
                                                    (find-file-noselect f) db)))
                                  'keymap     ores/dashboard--button-map
                                  'help-echo  (format "Open %s" (cdr link)))))
                  links))
         (bar    (mapconcat #'identity link-strs sep))
         (bar-w  (string-width bar))
         (margin (max 0 (/ (- (window-total-width) bar-w) 2))))
    (insert (make-string margin ?\s) bar "\n\n")))

;; ---------------------------------------------------------------------------
;; Compilation helper
;; ---------------------------------------------------------------------------

(defun ores/dashboard--compile (label cmd buf-suffix root dash-buf)
  "Run CMD in *ores-LABEL-BUF-SUFFIX*; show result in DASH-BUF's output window."
  (let* ((default-directory (or root default-directory))
         (buf-name (format "*ores-%s-%s*" label buf-suffix))
         (comp-buf (compilation-start cmd nil (lambda (_) buf-name))))
    (ores/dashboard--display comp-buf dash-buf)))

;; ---------------------------------------------------------------------------
;; Service runner — persistent pipe-based process buffer
;;
;; compass services start runs synchronously for ~2 minutes (waits for NATS and
;; the controller), then exits — the services continue as background jobs.
;; compilation-start uses a PTY; when the PTY master closes, the kernel
;; sends SIGHUP to the foreground process group, killing the services.
;;
;; Fix: run the script via make-process with :connection-type 'pipe.  A pipe
;; never sends SIGHUP, so background children survive the script's exit.
;; The buffer stays open and the full startup log streams in in real time.
;; ---------------------------------------------------------------------------

(defun ores/dashboard--service-filter (proc string)
  "Insert process output into the services buffer, bypassing read-only."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))))))

(defun ores/dashboard--run-services (label command root dash-buf)
  "Run COMMAND (an argv list) in a persistent buffer via a pipe.
Background services survive the script's exit because pipes do not deliver
SIGHUP on close — no setsid needed.  COMMAND must be a list of program +
arguments — make-process does not involve a shell, so a single command
string would be treated as one executable path."
  (let* ((buf-name (format "*ores:%s:services*" label))
         (buf      (get-buffer-create buf-name)))
    (when (process-live-p (get-buffer-process buf))
      (user-error "Services already running in %s" buf-name))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (special-mode)
      (setq-local default-directory root))
    (make-process
     :name            (format "ores-services-%s" label)
     :buffer          buf
     :command         command
     :connection-type 'pipe
     :filter          #'ores/dashboard--service-filter
     :noquery         t
     :sentinel        (lambda (proc event)
                        (when (buffer-live-p (process-buffer proc))
                          (with-current-buffer (process-buffer proc)
                            (let ((inhibit-read-only t))
                              (goto-char (point-max))
                              (insert (format "\n[%s]\n" (string-trim event))))))))
    (ores/dashboard--display buf dash-buf)))

(defun ores/dashboard--run-client (label cmd-list root dash-buf)
  "Run CMD-LIST in a persistent buffer via a pipe; show it in the dashboard.
Uses make-process so the Qt window survives without needing setsid."
  (let* ((buf-name (format "*ores:%s:client*" label))
         (buf      (get-buffer-create buf-name)))
    (when (get-buffer-process buf)
      (user-error "Client already running in %s — kill it first" buf-name))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (special-mode)
      (setq-local default-directory root))
    (make-process
     :name            (format "ores-client-%s" label)
     :buffer          buf
     :command         cmd-list
     :connection-type 'pipe
     :filter          #'ores/dashboard--service-filter
     :noquery         t
     :sentinel        (lambda (proc event)
                        (when (buffer-live-p (process-buffer proc))
                          (with-current-buffer (process-buffer proc)
                            (let ((inhibit-read-only t))
                              (goto-char (point-max))
                              (insert (format "\n[%s]\n" (string-trim event))))))))
    (ores/dashboard--display buf dash-buf)))

;; ---------------------------------------------------------------------------
;; Start-client transient
;; ---------------------------------------------------------------------------

(defun ores/dashboard--detach-prefix ()
  "Return a shell prefix that detaches child processes from Emacs's process group.
On Linux `setsid' is required — signals an error if missing.
On macOS the shell's process management is sufficient; no prefix is used.
On other systems `setsid' is used when available, otherwise an error is raised."
  (cond
   ((executable-find "setsid") "setsid ")
   ((eq system-type 'darwin)   "")
   (t (user-error "setsid not found — install util-linux (apt install util-linux)"))))

(defvar ores/dashboard--client-context nil
  "List (root label dash-buf) passed into the start-client transient.")

(defun ores/dashboard--start-client-do ()
  "Launch ores.qt using the options chosen in the transient."
  (interactive)
  (unless ores/dashboard--client-context
    (user-error "No client context — invoke Start client from the dashboard"))
  (let* ((ctx      ores/dashboard--client-context)
         (root     (nth 0 ctx))
         (label    (nth 1 ctx))
         (dashbuf  (nth 2 ctx))
         (args     (transient-args 'ores/dashboard--start-client-transient))
         (script   (expand-file-name "projects/ores.compass/compass.sh" root))
         (arg-list (cl-mapcan (lambda (a)
                     (if (string-match "\\`\\(--[^=]+\\)=\\(.*\\)\\'" a)
                         (list (match-string 1 a) (match-string 2 a))
                       (list a)))
                   (or args '())))
         (cmd-list (append (list "bash" script "client") arg-list)))
    (ores/dashboard--run-client label cmd-list root dashbuf)))

(transient-define-prefix ores/dashboard--start-client-transient ()
  "Start the ORE Studio Qt client."
  ["Instance"
   ("-c" "Colour"    "--colour="    :choices ("red" "green" "blue"))
   ("-n" "Name"      "--name=")
   ("-l" "Log level" "--log-level=" :choices ("trace" "debug" "info" "warn" "error"))]
  ["Actions"
   ("s" "Start" ores/dashboard--start-client-do)])

;; ---------------------------------------------------------------------------
;; Group builders — each returns (title icon-fn icon-arg items title-face)
;; All use nerd-icons-faicon (Font Awesome 4) for reliable icon availability.
;; Colors are Nord palette hues via named deffaces above.
;; ---------------------------------------------------------------------------

(defun ores/dashboard--env-group (env root dash-buf)
  (let* ((label  (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local"))
         (preset (or (cdr (assoc "ORES_PRESET"         env)) "linux-clang-debug-ninja")))
    (list "Environment" 'nerd-icons-faicon "nf-fa-cog"
          (list
           (ores/dashboard--mkitem
            "Init environment" 'nerd-icons-faicon "nf-fa-play"
            (let ((lbl label) (p preset) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "%s env init -y --preset %s"
                         (expand-file-name "projects/ores.compass/compass.sh" r) p)
                 "init-environment" r db))))
           (ores/dashboard--mkitem
            "Edit environment" 'nerd-icons-faicon "nf-fa-edit"
            (let ((r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--display
                 (find-file-noselect (expand-file-name ".env" r)) db))))
           (ores/dashboard--mkitem
            "Reload environment" 'nerd-icons-faicon "nf-fa-refresh"
            (lambda (_) (ores/dashboard-reload)))
           (ores/dashboard--mkitem
            "Diff .env vs .env.old" 'nerd-icons-faicon "nf-fa-exchange"
            (let ((r root) (db dash-buf))
              (lambda (_)
                (let ((old (expand-file-name ".env.old" r))
                      (cur (expand-file-name ".env" r)))
                  (if (file-exists-p old)
                      (ores/dashboard--display (diff-no-select old cur) db)
                    (user-error "No .env.old — run compass env init twice")))))))
          'ores/dashboard-group-env-face)))

(defun ores/dashboard--db-group (env root dash-buf)
  (let* ((label   (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local"))
         (pgpw    (cdr (assoc "PGPASSWORD" env)))
         (pg-host (or (cdr (assoc "PGHOST" env)) "localhost"))
         (db-name (or (cdr (assoc "ORES_DATABASE_NAME" env))
                      (format "ores_dev_%s" label))))
    (list "Database" 'nerd-icons-faicon "nf-fa-database"
          (list
           (ores/dashboard--mkitem
            "Open database connection" 'nerd-icons-faicon "nf-fa-plug"
            (let ((lbl label) (db dash-buf) (pw pgpw) (dbn db-name))
              (lambda (_)
                (ores-db/setup-connections)
                ;; Ensure a postgres superuser connection exists for this label
                (let ((pg-name (format "ores-%s-postgres" lbl)))
                  (unless (assoc pg-name sql-connection-alist)
                    (push (list pg-name
                                (list 'sql-product  ''postgres)
                                (list 'sql-user     "postgres")
                                (list 'sql-password pw)
                                (list 'sql-server   "localhost")
                                (list 'sql-database dbn))
                          sql-connection-alist)))
                (let* ((prefix (format "ores-%s-" lbl))
                       (conns  (seq-filter (lambda (e) (string-prefix-p prefix (car e)))
                                           sql-connection-alist))
                       (names  (mapcar #'car conns)))
                  (if (null names)
                      (user-error "No SQL connections for %s — check .env" lbl)
                    (let* ((choice (if (= (length names) 1)
                                       (car names)
                                     (completing-read (format "Service [%s]: " lbl)
                                                      names nil t)))
                           (svc      (string-remove-prefix prefix choice))
                           (buf-name (format "*ores-%s-db-%s*" lbl svc)))
                      (condition-case err
                          (progn
                            (let* ((conn-tail (cdr (assoc choice sql-connection-alist)))
                                   (conn-pw   (cadr (assq 'sql-password conn-tail)))
                                   (process-environment
                                    (if conn-pw
                                        (cons (format "PGPASSWORD=%s" conn-pw)
                                              process-environment)
                                      process-environment)))
                              ;; sql-get-login always calls read-passwd for postgres
                              ;; even when sql-password is set. Return it silently.
                              (cl-letf (((symbol-function 'read-passwd)
                                         (lambda (_p &optional _c default)
                                           (or default ""))))
                                (save-window-excursion (sql-connect choice buf-name))))
                            (when-let ((buf (get-buffer buf-name)))
                              (ores/dashboard--display buf db)))
                        (error
                         (user-error "Cannot connect: %s"
                                     (error-message-string err))))))))))
           (ores/dashboard--mkitem
            "Kill connections" 'nerd-icons-faicon "nf-fa-times_circle"
            (let ((r root) (pw pgpw) (h pg-host) (dbn db-name) (db dash-buf))
              (lambda (_)
                (unless (yes-or-no-p (format "Kill all connections to %s? " dbn))
                  (user-error "Aborted"))
                (let* ((script (expand-file-name
                                "projects/ores.sql/utility/kill_db_connections.sh" r))
                       (process-environment
                        (if pw (cons (format "PGPASSWORD=%s" pw) process-environment)
                          process-environment))
                       (buf-name (format "*ores-db-kill-%s*" dbn))
                       (comp-buf (compilation-start
                                  (format "%s --host %s %s" script h dbn)
                                  nil (lambda (_) buf-name))))
                  (ores/dashboard--display comp-buf db)))))
           (ores/dashboard--mkitem
            "Teardown DB" 'nerd-icons-faicon "nf-fa-bomb"
            (let ((r root) (pw pgpw) (h pg-host) (dbn db-name) (db dash-buf))
              (lambda (_)
                (unless (yes-or-no-p (format "Teardown %s (kill + drop)? " dbn))
                  (user-error "Aborted"))
                (let* ((script (expand-file-name
                                "projects/ores.sql/teardown_database.sh" r))
                       (process-environment
                        (if pw (cons (format "PGPASSWORD=%s" pw) process-environment)
                          process-environment))
                       (buf-name "*ores-db-teardown*")
                       (comp-buf (compilation-start
                                  (format "%s -y --host %s %s" script h dbn)
                                  nil (lambda (_) buf-name))))
                  (ores/dashboard--display comp-buf db)))))
           (ores/dashboard--mkitem
            "Recreate DB" 'nerd-icons-faicon "nf-fa-recycle"
            (let ((r root) (dbn db-name) (db dash-buf))
              (lambda (_)
                (unless (yes-or-no-p (format "DROP and recreate %s? " dbn))
                  (user-error "Aborted"))
                (let ((comp-buf (let ((default-directory r))
                                  (ores-db/--run-recreate-db dbn))))
                  (when comp-buf
                    (ores/dashboard--display comp-buf db))))))
           (ores/dashboard--mkitem
            "Recreate environment" 'nerd-icons-faicon "nf-fa-trash"
            (let ((lbl label) (r root))
              (lambda (_)
                (unless (yes-or-no-p (format "DROP and recreate ores_dev_%s? " lbl))
                  (user-error "Aborted"))
                (ores-db/--run-recreate-env lbl r))))
           (ores/dashboard--mkitem
            "Open connections DB (SQLite)" 'nerd-icons-faicon "nf-fa-table"
            (let ((sqlite-db (expand-file-name "~/.local/share/ores.qt/connections.db"))
                  (sqliterc  (expand-file-name "projects/ores.sql/utility/sqliterc.sql" root))
                  (dash      dash-buf))
              (lambda (_)
                (let* ((buf-name "*ores-connections-db*")
                       (sql-sqlite-options `("-init" ,sqliterc))
                       (sql-database       sqlite-db))
                  (save-window-excursion (sql-sqlite buf-name))
                  (when-let ((buf (get-buffer buf-name)))
                    (ores/dashboard--display buf dash)))))))
          'ores/dashboard-group-db-face)))

(defun ores/dashboard--services-group (env root dash-buf)
  (let ((label (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local")))
    (list "Services" 'nerd-icons-faicon "nf-fa-server"
          (list
           (ores/dashboard--mkitem
            "Start services" 'nerd-icons-faicon "nf-fa-play"
            (let ((lbl label)
                  (s   (list "bash"
                             (expand-file-name "projects/ores.compass/compass.sh" root)
                             "services" "start"))
                  (r   root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--run-services lbl s r db))))
           (ores/dashboard--mkitem
            "Start client" 'nerd-icons-faicon "nf-fa-play_circle"
            (let ((lbl label) (r root) (db dash-buf))
              (lambda (_)
                (setq ores/dashboard--client-context (list r lbl db))
                (ores/dashboard--start-client-transient))))
           (ores/dashboard--mkitem
            "Service status" 'nerd-icons-faicon "nf-fa-info_circle"
            (let ((lbl label)
                  (s   (concat (expand-file-name "projects/ores.compass/compass.sh" root) " services status"))
                  (r   root) (db dash-buf))
              (lambda (_) (ores/dashboard--compile lbl s "service-status" r db))))
           (ores/dashboard--mkitem
            "Stop services" 'nerd-icons-faicon "nf-fa-stop"
            (let ((lbl label)
                  (s   (concat (expand-file-name "projects/ores.compass/compass.sh" root) " services stop"))
                  (r   root) (db dash-buf))
              (lambda (_) (ores/dashboard--compile lbl s "stop-services" r db))))
           (ores/dashboard--mkitem
            "Clear logs" 'nerd-icons-faicon "nf-fa-trash"
            (let ((lbl label)
                  (s   (concat (expand-file-name "projects/ores.compass/compass.sh" root) " services clear-logs"))
                  (r   root) (db dash-buf))
              (lambda (_) (ores/dashboard--compile lbl s "clear-logs" r db)))))
          'ores/dashboard-group-services-face)))

(defun ores/dashboard--compass-group (env root dash-buf)
  (let* ((label   (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local"))
         (compass (expand-file-name "projects/ores.compass/compass.sh" root)))
    (list "Compass" 'nerd-icons-faicon "nf-fa-compass"
          (mapcar
           (lambda (spec)
             (ores/dashboard--mkitem
              (nth 0 spec) 'nerd-icons-faicon (nth 3 spec)
              (let ((lbl label)
                    (cmd (format "%s %s" compass (nth 1 spec)))
                    (sfx (nth 2 spec))
                    (r   root)
                    (db  dash-buf))
                (lambda (_) (ores/dashboard--compile lbl cmd sfx r db)))))
           '(("compass where"  "where"  "compass-where"  "nf-fa-map_marker")
             ("compass list"   "list"   "compass-list"   "nf-fa-list")
             ("compass status" "status" "compass-status" "nf-fa-info_circle")
             ("compass fleet"  "fleet"  "compass-fleet"  "nf-fa-sitemap")))
          'ores/dashboard-group-compass-face)))

(defun ores/dashboard--build-group (env root dash-buf)
  (let* ((label  (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local"))
         (preset (or (cdr (assoc "ORES_PRESET"         env)) "linux-clang-debug-ninja")))
    (list "Build" 'nerd-icons-faicon "nf-fa-cogs"
          (list
           (ores/dashboard--mkitem
            "Configure" 'nerd-icons-faicon "nf-fa-wrench"
            (let ((lbl label) (p preset) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "cmake --preset %s" p) "configure" r db))))
           (ores/dashboard--mkitem
            "Build" 'nerd-icons-faicon "nf-fa-play"
            (let ((lbl label) (p preset) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "cmake --build --preset %s" p) "build" r db))))
           (ores/dashboard--mkitem
            "Test" 'nerd-icons-faicon "nf-fa-flask"
            (let ((lbl label) (p preset) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "cmake --build --preset %s --target test" p) "test" r db)))))
          'ores/dashboard-group-build-face)))

(defun ores/dashboard--site-group (env root dash-buf)
  (let* ((label  (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local"))
         (port   (or (cdr (assoc "ORES_SITE_PORT"      env)) "8000")))
    (list "Site" 'nerd-icons-faicon "nf-fa-globe"
          (list
           (ores/dashboard--mkitem
            "Rebuild site" 'nerd-icons-faicon "nf-fa-refresh"
            (let ((lbl label) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "%s build --direct site"
                         (shell-quote-argument (expand-file-name "projects/ores.compass/compass.sh" r)))
                 "rebuild-site" r db))))
           (ores/dashboard--mkitem
            "Start site" 'nerd-icons-faicon "nf-fa-play"
            (let ((lbl label) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile
                 lbl
                 (format "%s site serve" (shell-quote-argument (expand-file-name "projects/ores.compass/compass.sh" r)))
                 "serve-site" r db))))
           (ores/dashboard--mkitem
            "Stop site" 'nerd-icons-faicon "nf-fa-stop"
            (let ((pt port))
              (lambda (_)
                (shell-command (format "fuser -k %s/tcp 2>/dev/null; true" pt))
                (message "Site server stopped.")))))
          'ores/dashboard-group-site-face)))

(defun ores/dashboard--skills-group (env root dash-buf)
  (let* ((label  (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local")))
    (list "Skills" 'nerd-icons-faicon "nf-fa-magic"
          (list
           (ores/dashboard--mkitem
            "Open skills folder" 'nerd-icons-faicon "nf-fa-folder_open"
            (let ((r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--display
                 (dired-noselect (expand-file-name "doc/llm/skills" r)) db))))
           (ores/dashboard--mkitem
            "Deploy skills" 'nerd-icons-faicon "nf-fa-upload"
            (let ((lbl label) (r root) (db dash-buf))
              (lambda (_)
                (ores/dashboard--compile lbl
                 (format "%s build --direct skills"
                         (shell-quote-argument (expand-file-name "projects/ores.compass/compass.sh" r)))
                 "deploy-skills" r db)))))
          'ores/dashboard-group-skills-face)))

(defun ores/dashboard--bookmarks-group (env root dash-buf)
  (let* ((p       (or (cdr (assoc "ORES_PRESET" env)) ""))
         (log-dir (expand-file-name (format "build/output/%s/publish/log" p) root))
         (bin-dir (expand-file-name (format "build/output/%s/publish/bin" p) root)))
    (list "Bookmarks" 'nerd-icons-faicon "nf-fa-bookmark"
          (list
           (ores/dashboard--mkitem
            "Log directory" 'nerd-icons-faicon "nf-fa-folder_open"
            (let ((d log-dir) (db dash-buf))
              (lambda (_)
                (if (file-directory-p d)
                    (ores/dashboard--display (dired-noselect d) db)
                  (user-error "Log dir not found: %s" d)))))
           (ores/dashboard--mkitem
            "Bin directory" 'nerd-icons-faicon "nf-fa-folder_open"
            (let ((d bin-dir) (db dash-buf))
              (lambda (_)
                (if (file-directory-p d)
                    (ores/dashboard--display (dired-noselect d) db)
                  (user-error "Bin dir not found: %s" d))))))
          'ores/dashboard-group-bookmarks-face)))

(defun ores/dashboard--links-group (_env root dash-buf)
  (list "Links" 'nerd-icons-faicon "nf-fa-link"
        (mapcar (lambda (spec)
                  (ores/dashboard--mkitem
                   (nth 0 spec) 'nerd-icons-faicon (nth 2 spec)
                   (let ((f  (expand-file-name (nth 1 spec) root))
                         (db dash-buf))
                     (lambda (_)
                       (ores/dashboard--display (find-file-noselect f) db)))))
                '(("User manual"   "doc/manual/user_guide/user_manual.org" "nf-fa-book")
                  ("Recipes index" "doc/recipes/recipes.org"               "nf-fa-list_ul")
                  ("Agile index"   "doc/agile/agile.org"                   "nf-fa-tasks")))
        'ores/dashboard-group-links-face))

(defun ores/dashboard--shell-group (env root dash-buf)
  (let ((label (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local")))
    (list "Shell" 'nerd-icons-faicon "nf-fa-terminal"
          (list
           (ores/dashboard--mkitem
            "Open ORE Studio shell" 'nerd-icons-faicon "nf-fa-terminal"
            (let ((r root) (lbl label) (db dash-buf))
              (lambda (_)
                ;; compass shell resolves the binary, NATS connection and
                ;; login from the checkout's .env.
                (let* ((compass  (expand-file-name "compass.sh" r))
                       (buf-name (format "*ores-%s-shell*" lbl))
                       (buffer   (get-buffer-create buf-name)))
                  (unless (file-executable-p compass)
                    (user-error "compass.sh not found at %s" compass))
                  (with-current-buffer buffer
                    (unless (eq major-mode 'ores-shell-mode)
                      (ores-shell-mode)))
                  (unless (comint-check-proc buffer)
                    (let ((default-directory r))
                      (make-comint-in-buffer "ores-shell" buffer compass nil
                                             "shell" "--log-enabled")))
                  (ores/dashboard--display buffer db))))))
          'ores/dashboard-group-shell-face)))

(defun ores/dashboard--nats-group (env root dash-buf)
  (let ((label (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "local")))
    (list "NATS" 'nerd-icons-faicon "nf-fa-exchange"
          (list
           (ores/dashboard--mkitem
            "Init NATS" 'nerd-icons-faicon "nf-fa-play"
            (let ((lbl label) (r root) (db dash-buf))
              (lambda (_)
                (let* ((script   (expand-file-name "build/scripts/init-nats.sh" r))
                       (buf-name (format "*ores-%s-nats-init*" lbl))
                       (comp-buf (compilation-start script nil (lambda (_) buf-name))))
                  (ores/dashboard--display comp-buf db)))))
           (ores/dashboard--mkitem
            "Init NATS + provision" 'nerd-icons-faicon "nf-fa-play_circle"
            (let ((lbl label) (r root) (db dash-buf))
              (lambda (_)
                (let* ((script   (expand-file-name "build/scripts/init-nats.sh" r))
                       (buf-name (format "*ores-%s-nats-init-provision*" lbl))
                       (comp-buf (compilation-start
                                  (format "%s --provision" script)
                                  nil (lambda (_) buf-name))))
                  (ores/dashboard--display comp-buf db))))))
          'ores/dashboard-group-nats-face)))

;; ---------------------------------------------------------------------------
;; Main render
;; ---------------------------------------------------------------------------

(defun ores/dashboard--render (env root)
  "Erase and fully redraw the dashboard using ENV and ROOT."
  (erase-buffer)
  (setq ores/dashboard--item-keys    (make-sparse-keymap)
        ores/dashboard--next-key-idx 0)
  (let* ((label    (or (cdr (assoc "ORES_CHECKOUT_LABEL" env)) "unknown"))
         (preset   (or (cdr (assoc "ORES_PRESET"          env)) "unknown"))
         (cw       (ores/dashboard--col-width))
         (dash-buf (current-buffer)))

    (ores/dashboard--insert-image root)
    (ores/dashboard--insert-quicklinks root dash-buf)

    (let ((margin "  "))
      (insert margin
              (propertize (format "ORE Studio Dashboard — %s\n" label)
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
     (list (ores/dashboard--shell-group env root dash-buf)
           (ores/dashboard--nats-group  env root dash-buf))
     cw)

    (use-local-map
     (make-composed-keymap ores/dashboard--item-keys ores/dashboard-mode-map))))

(provide 'ores-dashboard)
;;; ores-dashboard.el ends here
