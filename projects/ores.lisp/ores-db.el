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

;; Disable sql-postgres login prompts - we provide connection URI directly
(setq sql-postgres-login-params nil)

(defconst ores-db/hosts '("localhost" "192.168.1.22")
  "List of hosts to scan for ORES databases.")

(defconst ores-db/database-roles
  '("postgres" "ores_ddl_user" "ores_cli_user" "ores_wt_user" "ores_comms_user" "ores_http_user"
    "ores_test_ddl_user" "ores_test_dml_user" "ores_readonly_user")
  "Available roles to assume when connecting to an ORES database.")

(defvar-local ores-db/marked-ids nil
  "List of marked database IDs in the current buffer.")

(defvar-local ores-db/project-root nil
  "The project root for this database list buffer.")

(defun ores-db/database--get-credential (user host key)
  "Get a specific KEY (:secret or :port) for USER at HOST from auth-source."
  (let* ((match (auth-source-search :host host :user user :max 1))
         (val (plist-get (car match) key)))
    (cond ((and (eq key :secret) (functionp val)) (funcall val))
          (val (format "%s" val)) ;; Ensure port is a string
          (t nil))))

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
      (let* ((pw   (ores-db/database--get-credential "postgres" host :secret))
             (port (or (ores-db/database--get-credential "postgres" host :port) "5432"))
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
    (define-key map (kbd "v")   #'ores-db/set-env-vars)
    (define-key map (kbd "E")   #'ores-db/unset-env-vars)
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
  (let* ((pw   (ores-db/database--get-credential role host :secret))
         ;; Try to get port from auth-source, fallback to "5432"
         (port (or (ores-db/database--get-credential role host :port) "5432"))
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
                                     ores-db/database-roles nil t)))
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
             (postgres-pw (ores-db/database--get-credential "postgres" host :secret))
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
               (postgres-pw (ores-db/database--get-credential "postgres" host :secret))
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
      (let* ((postgres-pw (ores-db/database--get-credential "postgres" host :secret))
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
             (postgres-pw (ores-db/database--get-credential "postgres" host :secret))
             (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                        process-environment)))
        (if (not (file-exists-p script-path))
            (user-error "Script not found: %s" script-path)
          (compilation-start
           (format "%s --host %s %s" script-path host db-name)
           nil
           (lambda (_) (format "*ores-db-kill-%s*" db-name))))))))

(defun ores-db/set-env-vars (host)
  "Export all ORES passwords for HOST to environment variables.
Uses ORES_DB_<APP>_PASSWORD convention for service users to avoid
conflicting with application CLI environment variable mappers.
Also sets ORES_TEST_DB_DATABASE and ORES_TEST_DB_HOST for test infrastructure."
  (interactive (list (completing-read "Host: " ores-db/hosts nil t)))
  (let ((count 0)
        (env (ores-db/current-environment)))
    ;; Set test database connection info (use current environment's database)
    (setenv "ORES_TEST_DB_DATABASE" (if env (concat "ores_dev_" env) "ores"))
    (setenv "ORES_TEST_DB_HOST" host)
    (setq count (+ count 2))

    (dolist (role ores-db/database-roles)
      (let ((pw (ores-db/database--get-credential role host :secret)))
        (when pw
          (setq count (1+ count))
          (cond
           ((string= role "postgres")
            (setenv "PGPASSWORD" pw))
           ;; Test users for C++ test infrastructure
           ((string= role "ores_test_ddl_user")
            (setenv "ORES_TEST_DB_DDL_PASSWORD" pw))
           ((string= role "ores_test_dml_user")
            (setenv "ORES_TEST_DB_PASSWORD" pw))
           ;; Service users: ores_<app>_user -> ORES_DB_<APP>_PASSWORD
           ;; e.g., ores_cli_user -> ORES_DB_CLI_PASSWORD
           ((string-match "^ores_\\([a-z]+\\)_user$" role)
            (let ((app (upcase (match-string 1 role))))
              (setenv (concat "ORES_DB_" app "_PASSWORD") pw)))
           ;; Fallback for any other roles
           (t
            (setenv (concat "ORES_DB_" (upcase (replace-regexp-in-string "[-_]" "_" role)) "_PASSWORD")
                    pw))))))
    (message "[ORES] Exported %d environment variables for %s." count host)))

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

(defun ores-db/unset-env-vars ()
  "Unset all ORES environment variables."
  (interactive)
  (let ((count 0))
    ;; Unset test database connection info
    (setenv "ORES_TEST_DB_DATABASE" nil)
    (setenv "ORES_TEST_DB_HOST" nil)
    (setq count (+ count 2))

    (dolist (role ores-db/database-roles)
      (cond
       ((string= role "postgres")
        (setenv "PGPASSWORD" nil)
        (setq count (1+ count)))
       ((string= role "ores_test_ddl_user")
        (setenv "ORES_TEST_DB_DDL_PASSWORD" nil)
        (setq count (1+ count)))
       ((string= role "ores_test_dml_user")
        (setenv "ORES_TEST_DB_PASSWORD" nil)
        (setq count (1+ count)))
       ((string-match "^ores_\\([a-z]+\\)_user$" role)
        (let ((app (upcase (match-string 1 role))))
          (setenv (concat "ORES_DB_" app "_PASSWORD") nil)
          (setq count (1+ count))))
       (t
        (setenv (concat "ORES_DB_" (upcase (replace-regexp-in-string "[-_]" "_" role)) "_PASSWORD") nil)
        (setq count (1+ count)))))
    (message "[ORES] Unset %d environment variables." count)))

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

(defun ores-db/run-script (script-name buffer-name &optional args)
  "Run SCRIPT-NAME from ores.sql directory in compilation mode.
BUFFER-NAME is the compilation buffer name.
ARGS is an optional list of arguments to pass to the script."
  (let* ((sql-dir (ores-db/sql-scripts-directory))
         (script-path (expand-file-name script-name sql-dir))
         (postgres-pw (ores-db/database--get-credential "postgres" "localhost" :secret))
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
         (postgres-pw (ores-db/database--get-credential "postgres" "localhost" :secret))
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
  (let ((args (list "-e" environment "-y" "-k")))
    (when skip-validation
      (setq args (append args '("--no-sql-validation"))))
    (ores-db/run-script "recreate_env.sh"
                        (format "*ores-db-recreate-%s*" environment)
                        args)))

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

(defun ores-db/create-whimsical ()
  "Create a new database instance with a whimsical name.
Uses two-phase creation: postgres creates the database, ores_ddl_user sets up schema."
  (interactive)
  (let* ((sql-dir (ores-db/sql-scripts-directory))
         (host "localhost")
         (postgres-pw (ores-db/database--get-credential "postgres" host :secret))
         (ddl-pw (ores-db/database--get-credential "ores_ddl_user" host :secret))
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
    ("d" "Recreate current env" ores-db/recreate-current-env)
    ("r" "Recreate at point" ores-db/recreate-at-point)
    ("e" "Recreate environment..." ores-db/recreate-env-database)
    ("R" "Recreate ALL (nuclear)" ores-db/recreate-all)]
   ["Utilities"
    ("v" "Export env vars" ores-db/set-env-vars)
    ("E" "Unset env vars" ores-db/unset-env-vars)
    ("V" "Show env vars" ores-db/show-env-vars)
    ("q" "Quit" transient-quit-one)]])

(provide 'ores-db)
;;; ores-db.el ends here
