;;; ores-database.el --- Database connectivity -*- lexical-binding: t; -*-

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
(require 'sql)
(require 'transient)
(require 'project)
(require 'ores-env)

;; Disable sql-postgres login prompts - we provide connection URI directly
(setq sql-postgres-login-params nil)

(defconst ores-db/hosts '("localhost" "192.168.1.22")
  "List of hosts to scan for ORES databases.")

(defun ores-db/database-roles ()
  "Return a list of database roles available for connection.
Derives roles from *_DB_USER entries in the checkout .env, with
postgres prepended.  Falls back to an empty list if .env is missing."
  (cons "postgres"
        (when (file-exists-p (ores/dotenv-file))
          (delq nil
                (mapcar (lambda (pair)
                          (when (string-suffix-p "_DB_USER" (car pair))
                            (cdr pair)))
                        (ores/load-dotenv))))))


(defvar-local ores-db/marked-ids nil
  "List of marked database IDs in the current buffer.")

(defvar-local ores-db/project-root nil
  "The project root for this database list buffer.")

(defun ores-db/--get-password-from-dotenv (env-var-name)
  "Look up ENV-VAR-NAME in the checkout .env and return its value."
  (cdr (assoc env-var-name (ores/load-dotenv))))

(defun ores-db/setup-connections ()
  "Populate `sql-connection-alist' from the checkout .env file.

Creates one SQL connection per NATS service, named ores-<label>-<svc>-service.
Call this once after starting Emacs; call again to refresh after re-running
init-environment.sh."
  (interactive)
  (let* ((pairs  (ores/load-dotenv))
         (label  (or (cdr (assoc "ORES_CHECKOUT_LABEL" pairs)) "unknown"))
         (count  0))
    ;; Remove stale entries for this label before re-adding
    (setq sql-connection-alist
          (seq-remove (lambda (e)
                        (string-prefix-p (format "ores-%s-" label) (car e)))
                      sql-connection-alist))
    (dolist (svc '("iam" "refdata" "dq" "variability" "assets"
                   "synthetic" "scheduler" "reporting" "telemetry" "trading"))
      (let* ((prefix  (upcase (replace-regexp-in-string "-" "_" svc)))
             (user-k  (format "ORES_%s_SERVICE_DB_USER"     prefix))
             (pw-k    (format "ORES_%s_SERVICE_DB_PASSWORD"  prefix))
             (db-k    (format "ORES_%s_SERVICE_DB_DATABASE"  prefix))
             (user    (or (cdr (assoc user-k pairs))
                          (format "ores_%s_service" svc)))
             (pw      (cdr (assoc pw-k pairs)))
             (db      (or (cdr (assoc db-k pairs)) "ores_unknown"))
             (name    (format "ores-%s-%s-service" label svc)))
        (push (list name
                    (list 'sql-product  ''postgres)
                    (list 'sql-user     user)
                    (list 'sql-password pw)
                    (list 'sql-database db)
                    (list 'sql-server   "localhost"))
              sql-connection-alist)
        (cl-incf count)))
    (message "[ores-db] Added %d SQL connections for checkout '%s'" count label)))

(defun ores-db/database-list-discovery ()
  "Query all hosts in \='ores-db/hosts' for databases starting with \='ores_'.
Uses `ores-db/project-root' if set (in database list buffers), otherwise
falls back to the current project."
  (let* ((root (or ores-db/project-root
                   (when-let ((pr (project-current)))
                     (project-root pr))))
         (script-path (expand-file-name "projects/ores.sql/utility/list_databases.sh" root))
         all-dbs)
    (dolist (host ores-db/hosts)
      (let* ((pw   (ores-db/--get-password-from-dotenv "PGPASSWORD"))
             (port "5432")
             (process-environment (cons (concat "PGPASSWORD=" pw) process-environment))
             (cmd (format "%s -h %s -p %s" script-path host port))
             (output (if (and pw (file-executable-p script-path))
                         (shell-command-to-string cmd)
                       ""))
             (lines (split-string output "\n" t)))
        (dolist (line lines)
          (let* ((fields (split-string line "\t"))
                 (db (nth 0 fields))
                 (created (or (nth 1 fields) "")))
            (when db
              (push (list db host created) all-dbs))))))
    (nreverse all-dbs)))

;; Define keymap BEFORE mode (Emacs looks for it during define-derived-mode)
;; Use defconst so reloading the file actually redefines the keymap
(defconst ores-db/mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ores-db/ui-connect-at-point)
    (define-key map (kbd "c")   #'ores-db/ui-connect-at-point)
    (define-key map (kbd "s")   #'ores-db/show-connections-at-point)
    (define-key map (kbd "k")   #'ores-db/kill-connections-at-point)
    (define-key map (kbd "x")   #'ores-db/drop-at-point)
    (define-key map (kbd "X")   #'ores-db/teardown-at-point)
    (define-key map (kbd "d")   #'ores-db/recreate-current-env)
    (define-key map (kbd "r")   #'ores-db/recreate-at-point)
    (define-key map (kbd "e")   #'ores-db/recreate-env-database)
    (define-key map (kbd "R")   #'ores-db/recreate-all)
    (define-key map (kbd "n")   #'ores-db/create-whimsical)
    (define-key map (kbd "i")   #'ores-db/init-environment)
    (define-key map (kbd "N")   #'ores-db/init-nats)
    (define-key map (kbd "D")   #'ores-db/diff-environment)
    (define-key map (kbd "v")   #'ores-db/setup-connections)
    (define-key map (kbd "V")   #'ores-db/show-env-vars)
    (define-key map (kbd "g")   #'ores-db/ui-refresh)
    (define-key map (kbd "m")   #'ores-db/mark)
    (define-key map (kbd "u")   #'ores-db/unmark)
    (define-key map (kbd "U")   #'ores-db/unmark-all)
    (define-key map (kbd "t")   #'ores-db/toggle-marks)
    (define-key map (kbd "?")   #'ores-db/menu)
    (define-key map (kbd "h")   #'ores-db/menu)
    map)
  "Keymap for `ores-db/mode`.")

(define-derived-mode ores-db/mode tabulated-list-mode "ORES-DB"
  "Major mode for browsing ORES databases.
Each project gets its own buffer with independent state."
  (setq tabulated-list-format [("M" 1 t)
                               ("Database Name" 30 t)
                               ("Host" 15 t)
                               ("Created" 20 t)])
  (setq tabulated-list-padding 2)
  (setq ores-db/marked-ids nil)
  (setq ores-db/project-root nil)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(defun ores-db/ui-refresh ()
  "Fetch databases and refresh the tabulated list."
  (interactive)
  (let ((db-records (ores-db/database-list-discovery))
        (old-marks ores-db/marked-ids))
    (setq tabulated-list-entries
          (mapcar (lambda (rec)
                    (let* ((db (nth 0 rec))
                           (host (nth 1 rec))
                           (created (nth 2 rec))
                           (id (cons db host))
                           (mark (if (member id old-marks) "*" " ")))
                      ;; ID is (db . host), columns are mark, db, host, created
                      (list id (vector mark db host created))))
                  db-records))
    (tabulated-list-print t)))

(defun ores-db/list-databases ()
  "Switch to the ORES database browser buffer.
The buffer is project-scoped, so different projects can have their own
database lists with independent current database selections."
  (interactive)
  (let* ((pr (project-current))
         (root (when pr (project-root pr)))
         (env (ores-db/current-environment))
         (buf-name (if env
                       (format "*ORES Databases [%s]*" env)
                     "*ORES Databases*"))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (ores-db/mode)
      (setq ores-db/project-root root)
      (ores-db/ui-refresh))
    (switch-to-buffer buf)))

(defun ores-db/mark ()
  "Mark the database at point."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (unless (member id ores-db/marked-ids)
      (push id ores-db/marked-ids))
    (ores-db/ui-refresh)
    (forward-line 1)))

(defun ores-db/unmark ()
  "Unmark the database at point."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (setq ores-db/marked-ids (delete id ores-db/marked-ids))
    (ores-db/ui-refresh)
    (forward-line 1)))

(defun ores-db/unmark-all ()
  "Unmark all databases."
  (interactive)
  (setq ores-db/marked-ids nil)
  (ores-db/ui-refresh)
  (message "All marks cleared."))

(defun ores-db/toggle-marks ()
  "Toggle marks on all databases."
  (interactive)
  (let ((all-ids (mapcar #'car tabulated-list-entries)))
    (setq ores-db/marked-ids
          (seq-difference all-ids ores-db/marked-ids))
    (ores-db/ui-refresh)))

(defun ores-db/get-marked-or-at-point ()
  "Return list of marked IDs, or list with just the ID at point."
  (or (and ores-db/marked-ids (copy-sequence ores-db/marked-ids))
      (when-let ((id (tabulated-list-get-id)))
        (list id))))


(defun ores-db/database-connect-to (db-name role host)
  "Construct a connection URI using DB-NAME ROLE HOST.
Then launch a unique `sql-postgres` session.
The session's working directory is set to ores.sql for easy script access."
  (let* ((pw   (ores-db/--get-password-from-dotenv "PGPASSWORD"))
         (port "5432")
         (connection-uri (format "postgresql://%s:%s@%s:%s/%s"
                                 role
                                 (or (and pw (url-hexify-string pw)) "")
                                 host port db-name))
         (buf-name (format "%s - %s@%s" db-name role host))
         (sql-dir (ores-db/sql-scripts-directory)))

    (if (get-buffer buf-name)
        (pop-to-buffer buf-name)
      (let ((sql-database connection-uri)
            (default-directory (or sql-dir default-directory)))
        (sql-postgres buf-name)))))

(defun ores-db/ui-connect-at-point ()
  "Connect to the database/host at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db-name (car id))
         (host (cdr id)))
    (if id
        (let ((role (completing-read (format "Connect to %s on %s as: " db-name host)
                                     (ores-db/database-roles) nil t)))
          (ores-db/database-connect-to db-name role host))
      (message "No database at point."))))

(defun ores-db/drop-at-point ()
  "Drop the database at point using drop_database.sh."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db-name (car id))
         (host (cdr id)))
    (if (not id)
        (message "No database at point.")
      (unless (yes-or-no-p (format "Drop database '%s' on %s? " db-name host))
        (user-error "Aborted"))
      (let* ((sql-dir (ores-db/sql-scripts-directory))
             (script-path (expand-file-name "drop_database.sh" sql-dir))
             (postgres-pw (ores-db/--get-password-from-dotenv "PGPASSWORD"))
             (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                        process-environment)))
        (if (not (file-exists-p script-path))
            (user-error "Script not found: %s" script-path)
          (compilation-start
           (format "%s -y --host %s %s" script-path host db-name)
           nil
           (lambda (_) (format "*ores-db-drop-%s*" db-name))))))))

(defun ores-db/teardown-at-point ()
  "Teardown marked databases or database at point: kill connections and drop."
  (interactive)
  (let ((ids (ores-db/get-marked-or-at-point)))
    (if (not ids)
        (message "No database at point or marked.")
      (let* ((count (length ids))
             (names (mapcar #'car ids))
             (prompt (if (= count 1)
                         (format "Teardown '%s' (kill connections + drop)? " (car names))
                       (format "Teardown %d databases (kill connections + drop)?\n%s "
                               count (string-join names ", ")))))
        (unless (yes-or-no-p prompt)
          (user-error "Aborted"))
        (let* ((sql-dir (ores-db/sql-scripts-directory))
               (script-path (expand-file-name "teardown_database.sh" sql-dir))
               ;; Use first host's password (assumes same password for all hosts)
               (host (cdar ids))
               (postgres-pw (ores-db/--get-password-from-dotenv "PGPASSWORD"))
               (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                          process-environment))
               ;; Build a single command that runs all teardowns sequentially
               (commands (mapcar (lambda (id)
                                   (format "%s -y --host %s %s"
                                           script-path (cdr id) (car id)))
                                 ids))
               (full-command (string-join commands " && ")))
          (if (not (file-exists-p script-path))
              (user-error "Script not found: %s" script-path)
            (compilation-start
             full-command
             nil
             (lambda (_) "*ores-db-teardown*"))
            (setq ores-db/marked-ids nil)))))))

(defun ores-db/show-connections-at-point ()
  "Show all connections to the database at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db-name (car id))
         (host (cdr id)))
    (if (not id)
        (message "No database at point.")
      (let* ((postgres-pw (ores-db/--get-password-from-dotenv "PGPASSWORD"))
             (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                        process-environment))
             (sql "SELECT pid, usename AS user, client_addr AS client, state, backend_start::timestamp(0) AS connected_since, LEFT(query, 60) AS current_query FROM pg_stat_activity WHERE datname = '%s' ORDER BY backend_start;"))
        (compilation-start
         (format "psql -h %s -U postgres -c \"%s\"" host (format sql db-name))
         nil
         (lambda (_) (format "*ores-db-connections-%s*" db-name)))))))

(defun ores-db/kill-connections-at-point ()
  "Kill all connections to the database at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db-name (car id))
         (host (cdr id)))
    (if (not id)
        (message "No database at point.")
      (let* ((sql-dir (ores-db/sql-scripts-directory))
             (script-path (expand-file-name "utility/kill_db_connections.sh" sql-dir))
             (postgres-pw (ores-db/--get-password-from-dotenv "PGPASSWORD"))
             (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                        process-environment)))
        (if (not (file-exists-p script-path))
            (user-error "Script not found: %s" script-path)
          (compilation-start
           (format "%s --host %s %s" script-path host db-name)
           nil
           (lambda (_) (format "*ores-db-kill-%s*" db-name))))))))


(defun ores-db/show-env-vars ()
  "Display all ORES_ environment variables in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*ORES Environment*"))
        (vars (seq-filter (lambda (v) (string-prefix-p "ORES_" v))
                          process-environment)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "ORES Environment Variables\n")
      (insert "===========================\n\n")
      (if vars
          (dolist (var (sort vars #'string<))
            (insert var "\n"))
        (insert "(no ORES_ variables set)\n"))
      (insert "\n")
      ;; Also show PGPASSWORD if set
      (when-let ((pw (getenv "PGPASSWORD")))
        (insert (format "PGPASSWORD=%s\n" pw)))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buf)))


;;; ==========================================================================
;;; Environment Management
;;; ==========================================================================

(defconst ores-db/environments
  '("local1" "local2" "local3" "local4" "local5" "remote")
  "Available ORES development environments.")

(defun ores-db/current-environment ()
  "Get the current environment from the project directory name.
Returns the environment label (e.g., 'local2') or nil if not in an OreStudio project."
  (when-let* ((pr (project-current))
              (root (directory-file-name (expand-file-name (project-root pr))))
              (dir-name (file-name-nondirectory root)))
    (if (string-prefix-p "OreStudio." dir-name)
        (substring dir-name (length "OreStudio."))
      dir-name)))

(defun ores-db/sql-scripts-directory ()
  "Get the path to the ores.sql/projects directory.
Uses `ores-db/project-root' if set (in database list buffers), otherwise
falls back to the current project."
  (when-let ((root (or ores-db/project-root
                       (when-let ((pr (project-current)))
                         (project-root pr)))))
    (expand-file-name "projects/ores.sql" root)))

(defun ores-db/sql-scripts-directory-for-env (environment)
  "Get the SQL scripts directory for a given ENVIRONMENT.
Assumes all OreStudio environments are siblings under the same parent directory."
  (when-let* ((root (or ores-db/project-root
                        (when-let ((pr (project-current)))
                          (project-root pr))))
              (root-clean (directory-file-name (expand-file-name root)))
              (parent (file-name-directory root-clean)))
    (expand-file-name (format "OreStudio.%s/projects/ores.sql" environment) parent)))

(defun ores-db/run-script (script-name buffer-name &optional args sql-dir)
  "Run SCRIPT-NAME from ores.sql directory in compilation mode.
BUFFER-NAME is the compilation buffer name.
ARGS is an optional list of arguments to pass to the script.
SQL-DIR overrides the default scripts directory when provided."
  (let* ((sql-dir (or sql-dir (ores-db/sql-scripts-directory)))
         (script-path (expand-file-name script-name sql-dir))
         (postgres-pw (ores-db/--get-password-from-dotenv "PGPASSWORD"))
         (default-directory sql-dir)
         (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                    process-environment)))
    (if (not (file-executable-p script-path))
        (user-error "Script not found or not executable: %s" script-path)
      (compilation-start
       (if args
           (format "%s %s" script-path (string-join args " "))
         script-path)
       nil
       (lambda (_) buffer-name)))))

(defun ores-db/run-sql (sql-file buffer-name)
  "Run SQL-FILE from ores.sql directory via psql in compilation mode.
BUFFER-NAME is the compilation buffer name."
  (let* ((sql-dir (ores-db/sql-scripts-directory))
         (sql-path (expand-file-name sql-file sql-dir))
         (postgres-pw (ores-db/--get-password-from-dotenv "PGPASSWORD"))
         (default-directory sql-dir)
         (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                    process-environment)))
    (if (not (file-exists-p sql-path))
        (user-error "SQL file not found: %s" sql-path)
      (compilation-start
       (format "psql -h localhost -U postgres -f %s" sql-path)
       nil
       (lambda (_) buffer-name)))))

(defun ores-db/recreate-env-database (environment &optional skip-validation)
  "Recreate the database for ENVIRONMENT.
If SKIP-VALIDATION is non-nil, skip SQL input validation for faster execution."
  (interactive
   (let* ((current-env (ores-db/current-environment))
          (env (completing-read
                (format "Environment to create/recreate%s: "
                        (if current-env (format " (current: %s)" current-env) ""))
                ores-db/environments nil t nil nil current-env)))
     (list env current-prefix-arg)))
  (unless (yes-or-no-p (format "This will DROP and recreate ores_dev_%s. Are you sure? " environment))
    (user-error "Aborted"))
  (let* ((target-dir (ores-db/sql-scripts-directory-for-env environment))
         (args (list "-e" environment "-y" "-k")))
    (unless (and target-dir (file-directory-p target-dir))
      (user-error "SQL scripts directory not found for environment '%s': %s"
                  environment target-dir))
    (when skip-validation
      (setq args (append args '("--no-sql-validation"))))
    (ores-db/run-script "recreate_env.sh"
                        (format "*ores-db-recreate-%s*" environment)
                        args
                        target-dir)))

(defun ores-db/recreate-current-env (&optional skip-validation)
  "Recreate the database for the current environment.
If SKIP-VALIDATION is non-nil (prefix arg), skip SQL input validation."
  (interactive "P")
  (if-let ((env (ores-db/current-environment)))
      (ores-db/recreate-env-database env skip-validation)
    (user-error "Not in an OreStudio project")))

(defun ores-db/recreate-at-point (&optional skip-validation)
  "Recreate the database at point in the database list.
If SKIP-VALIDATION is non-nil (prefix arg), skip SQL input validation."
  (interactive "P")
  (let* ((id (tabulated-list-get-id))
         (db-name (car id)))
    (unless id
      (user-error "No database at point"))
    (unless (yes-or-no-p (format "This will DROP and recreate %s. Are you sure? " db-name))
      (user-error "Aborted"))
    (cond
     ((string-prefix-p "ores_dev_" db-name)
      (let ((env (substring db-name (length "ores_dev_"))))
        (ores-db/recreate-env-database env skip-validation)))
     (t
      (user-error "Don't know how to recreate %s" db-name)))))

(defun ores-db/recreate-all ()
  "Recreate ALL ORES databases (nuclear option).
This runs recreate_database.sh which drops everything and recreates from scratch."
  (interactive)
  (unless (yes-or-no-p "This will DROP ALL ORES databases and recreate from scratch. Are you SURE? ")
    (user-error "Aborted"))
  (ores-db/run-script "recreate_database.sh" "*ores-db-recreate-all*" '("-y")))

(defconst ores-db/presets
  '("linux-clang-debug-make"
    "linux-clang-release-make"
    "linux-clang-debug-ninja"
    "linux-clang-release-ninja"
    "macos-clang-debug-ninja"
    "macos-clang-release-ninja")
  "Known build presets for preset selection prompt.")

(defun ores-db/init-environment ()
  "Run build/scripts/init-environment.sh -y to regenerate the .env file.
Reads ORES_PRESET from the existing .env if available; otherwise prompts."
  (interactive)
  (let* ((root (or ores-db/project-root
                   (when-let ((pr (project-current)))
                     (project-root pr))))
         (script-path (expand-file-name "build/scripts/init-environment.sh" root)))
    (unless root
      (user-error "Not in an OreStudio project"))
    (unless (file-executable-p script-path)
      (user-error "Script not found or not executable: %s" script-path))
    (let* ((existing-preset (cdr (assoc "ORES_PRESET" (ores/load-dotenv))))
           (default-preset (or existing-preset (car ores-db/presets)))
           (preset (completing-read (format "Build preset (default: %s): " default-preset)
                                    ores-db/presets nil nil nil nil default-preset))
           (default-directory root))
      (compilation-start
       (format "%s -y --preset %s" script-path preset)
       nil
       (lambda (_) "*ores-db-init-environment*")))))

(defun ores-db/init-nats (&optional provision)
  "Run build/scripts/init-nats.sh to generate the per-environment NATS config.
With prefix argument PROVISION, also provision JetStream streams (requires
NATS to already be running)."
  (interactive "P")
  (let* ((root (or ores-db/project-root
                   (when-let ((pr (project-current)))
                     (project-root pr))))
         (script-path (expand-file-name "build/scripts/init-nats.sh" root)))
    (unless root
      (user-error "Not in an OreStudio project"))
    (unless (file-executable-p script-path)
      (user-error "Script not found or not executable: %s" script-path))
    (let ((default-directory root))
      (compilation-start
       (if provision
           (format "%s --provision" script-path)
         script-path)
       nil
       (lambda (_) "*ores-nats-init*")))))

(defun ores-db/diff-environment ()
  "Show a diff buffer comparing .env.old with the current .env."
  (interactive)
  (let* ((root (or ores-db/project-root
                   (when-let ((pr (project-current)))
                     (project-root pr))))
         (env-file (expand-file-name ".env" root))
         (env-old  (expand-file-name ".env.old" root)))
    (unless root
      (user-error "Not in an OreStudio project"))
    (unless (file-exists-p env-old)
      (user-error "No .env.old found — run init-environment.sh at least twice"))
    (unless (file-exists-p env-file)
      (user-error "No .env found"))
    (diff env-old env-file)))

(defun ores-db/create-whimsical ()
  "Create a new database instance with a whimsical name.
Uses two-phase creation: postgres creates the database, ores_ddl_user sets up schema."
  (interactive)
  (let* ((sql-dir (ores-db/sql-scripts-directory))
         (host "localhost")
         (postgres-pw (ores-db/--get-password-from-dotenv "PGPASSWORD"))
         (ddl-pw (ores-db/--get-password-from-dotenv "ORES_DB_DDL_PASSWORD"))
         (default-directory sql-dir)
         (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                    process-environment))
         ;; Generate a whimsical name by querying any existing ORES database
         ;; or use a default pattern
         (db-name (string-trim
                   (shell-command-to-string
                    (format "psql -h %s -U postgres -At -c \"SELECT 'ores_' || substr(md5(random()::text), 1, 12);\"" host)))))
    (compilation-start
     (format "PGPASSWORD=%s psql -h %s -U postgres -v db_name=%s -f %s && PGPASSWORD=%s psql -h %s -U ores_ddl_user -d %s -f %s"
             postgres-pw host db-name
             (expand-file-name "create_database.sql" sql-dir)
             ddl-pw host db-name
             (expand-file-name "setup_schema.sql" sql-dir))
     nil
     (lambda (_) "*ores-db-create-whimsical*"))))

;;; ==========================================================================
;;; Transient Menu
;;; ==========================================================================

(transient-define-prefix ores-db/menu ()
  "ORES Database Management menu."
  [:description
   (lambda ()
     (format "ORES Database Management [%s]"
             (or (ores-db/current-environment) "no project")))
   ["Browse"
    ("l" "List databases" ores-db/list-databases)
    ("g" "Refresh list" ores-db/ui-refresh)
    ("c" "Connect at point" ores-db/ui-connect-at-point)
    ("s" "Show connections" ores-db/show-connections-at-point)
    ("k" "Kill connections" ores-db/kill-connections-at-point)
    ("x" "Drop at point" ores-db/drop-at-point)
    ("X" "Teardown (kill+drop)" ores-db/teardown-at-point)]
   ["Mark"
    ("m" "Mark" ores-db/mark)
    ("u" "Unmark" ores-db/unmark)
    ("U" "Unmark all" ores-db/unmark-all)
    ("t" "Toggle marks" ores-db/toggle-marks)]
   ["Create/Recreate"
    ("n" "New whimsical database" ores-db/create-whimsical)
    ("d" "Recreate DB only (current env)" ores-db/recreate-current-env)
    ("r" "Recreate DB only (at point)" ores-db/recreate-at-point)
    ("e" "Recreate DB only (choose env)" ores-db/recreate-env-database)
    ("R" "Full recreate: roles+users+DB+data" ores-db/recreate-all)]
   ["Utilities"
    ("i" "Init environment" ores-db/init-environment)
    ("N" "Regen NATS config (C-u = +provision)" ores-db/init-nats)
    ("D" "Diff .env vs .env.old" ores-db/diff-environment)
    ("v" "Setup SQL connections" ores-db/setup-connections)
    ("V" "Show env vars" ores-db/show-env-vars)
    ("q" "Quit" transient-quit-one)]])

(provide 'ores-db)
;;; ores-db.el ends here
