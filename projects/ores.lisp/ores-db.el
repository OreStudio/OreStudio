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

(defconst ores-db/hosts '("localhost" "192.168.1.22")
  "List of hosts to scan for ORES databases.")

(defconst ores-db/database-roles
  '("postgres" "ores_ddl_user" "ores_cli_user" "ores_wt_user" "ores_comms_user" "ores_http_user"
    "ores_test_ddl_user" "ores_test_dml_user" "ores_readonly_user")
  "Available roles to assume when connecting to an ORES database.")

(defun ores-db/database--get-credential (user host key)
  "Get a specific KEY (:secret or :port) for USER at HOST from auth-source."
  (let* ((match (auth-source-search :host host :user user :max 1))
         (val (plist-get (car match) key)))
    (cond ((and (eq key :secret) (functionp val)) (funcall val))
          (val (format "%s" val)) ;; Ensure port is a string
          (t nil))))

(defun ores-db/database--get-password (user &optional host)
  "Get password for USER at HOST from auth-source."
  (let* ((host (or host "localhost"))
         (match (auth-source-search :host host :user user :max 1))
         (secret (plist-get (car match) :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun ores-db/database-list-discovery ()
  "Query all hosts in \='ores-db/hosts' for databases starting with \='ores_'."
  (let (all-dbs)
    (dolist (host ores-db/hosts)
      (let* ((pw   (ores-db/database--get-credential "postgres" host :secret))
             (port (or (ores-db/database--get-credential "postgres" host :port) "5432"))
             (process-environment (cons (concat "PGPASSWORD=" pw) process-environment))
             ;; Added -p flag for the specific port
             (cmd (format "psql -U postgres -h %s -p %s -At -c \"SELECT datname FROM pg_database WHERE datname LIKE 'ores_%%' ORDER BY datname;\""
                          host port))
             (results (if pw (split-string (shell-command-to-string cmd) "\n" t) nil)))
        (dolist (db results)
          (push (list db host) all-dbs))))
    (nreverse all-dbs)))

;; Define keymap BEFORE mode (Emacs looks for it during define-derived-mode)
;; Use defconst so reloading the file actually redefines the keymap
(defconst ores-db/mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ores-db/ui-connect-at-point)
    (define-key map (kbd "c")   #'ores-db/ui-connect-at-point)
    (define-key map (kbd "k")   #'ores-db/drop-at-point)
    (define-key map (kbd "d")   #'ores-db/recreate-current-env)
    (define-key map (kbd "r")   #'ores-db/recreate-at-point)
    (define-key map (kbd "e")   #'ores-db/recreate-env-database)
    (define-key map (kbd "R")   #'ores-db/recreate-all)
    (define-key map (kbd "n")   #'ores-db/create-whimsical)
    (define-key map (kbd "v")   #'ores-db/set-env-vars)
    (define-key map (kbd "u")   #'ores-db/unset-env-vars)
    (define-key map (kbd "V")   #'ores-db/show-env-vars)
    (define-key map (kbd "g")   #'ores-db/ui-refresh)
    (define-key map (kbd "?")   #'ores-db/menu)
    (define-key map (kbd "h")   #'ores-db/menu)
    map)
  "Keymap for `ores-db/mode`.")

(define-derived-mode ores-db/mode tabulated-list-mode "ORES-DB"
  "Major mode for browsing ORES databases."
  (setq tabulated-list-format [("Database Name" 30 t)
                               ("Host" 20 t)
                               ("Status" 10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun ores-db/ui-refresh ()
  "Fetch databases and refresh the tabulated list."
  (interactive)
  (let ((db-records (ores-db/database-list-discovery)))
    (setq tabulated-list-entries
          (mapcar (lambda (rec)
                    (let ((db (car rec))
                          (host (cadr rec)))
                      ;; ID is (db . host), columns are db, host, status
                      (list (cons db host) (vector db host "available"))))
                  db-records))
    (tabulated-list-print t)))

(defun ores-db/list-databases ()
  "Switch to the ORES database browser buffer."
  (interactive)
  (let ((buf (get-buffer-create "*ORES Databases*")))
    (with-current-buffer buf
      (ores-db/mode)
      (ores-db/ui-refresh))
    (switch-to-buffer buf)))


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
      (let* ((sql-dir (ores-db/sql-scripts-directory))
             (script-path (expand-file-name "drop_database.sh" sql-dir))
             (postgres-pw (ores-db/database--get-credential "postgres" host :secret))
             (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                        process-environment)))
        (if (not (file-exists-p script-path))
            (user-error "Script not found: %s" script-path)
          (compilation-start
           (format "%s --host %s %s" script-path host db-name)
           nil
           (lambda (_) (format "*ores-db-drop-%s*" db-name))))))))

(defun ores-db/set-env-vars (host)
  "Export all ORES passwords for HOST to environment variables.
Uses ORES_DB_<APP>_PASSWORD convention for service users to avoid
conflicting with application CLI environment variable mappers."
  (interactive (list (completing-read "Host: " ores-db/hosts nil t)))
  (let ((count 0))
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
    (message "[ORES] Exported %d passwords for %s to environment." count host)))

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
  "Unset all ORES password environment variables."
  (interactive)
  (let ((count 0))
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
    (message "[ORES] Unset %d password environment variables." count)))

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
  "Get the path to the ores.sql/projects directory."
  (when-let ((pr (project-current)))
    (expand-file-name "projects/ores.sql" (project-root pr))))

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
  (let ((sql-dir (ores-db/sql-scripts-directory)))
    ;; Set PGPASSWORD and run the script
    (let* ((postgres-pw (ores-db/database--get-credential "postgres" "localhost" :secret))
           (script-path (expand-file-name "recreate_env.sh" sql-dir))
           (default-directory sql-dir)
           (args (list "-e" environment "-y"))
           (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                       process-environment)))
      (when skip-validation
        (setq args (append args '("--no-sql-validation"))))
      (if (not (file-executable-p script-path))
          (user-error "Script not found or not executable: %s" script-path)
        (compilation-start
         (format "%s %s" script-path (string-join args " "))
         nil
         (lambda (_) (format "*ores-db-recreate-%s*" environment)))))))

(defun ores-db/recreate-template (&optional skip-validation)
  "Recreate the ores_template database.
If SKIP-VALIDATION is non-nil, skip SQL input validation for faster execution."
  (let ((sql-dir (ores-db/sql-scripts-directory)))
    ;; Set PGPASSWORD and run the script
    (let* ((postgres-pw (ores-db/database--get-credential "postgres" "localhost" :secret))
           (script-path (expand-file-name "recreate_template.sh" sql-dir))
           (default-directory sql-dir)
           (args '("-y"))
           (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                       process-environment)))
      (when skip-validation
        (setq args (append args '("--no-sql-validation"))))
      (if (not (file-executable-p script-path))
          (user-error "Script not found or not executable: %s" script-path)
        (compilation-start
         (format "%s %s" script-path (string-join args " "))
         nil
         (lambda (_) "*ores-db-recreate-template*"))))))

(defun ores-db/recreate-admin (&optional skip-validation)
  "Recreate the ores_admin database.
If SKIP-VALIDATION is non-nil, skip SQL input validation for faster execution."
  (let ((sql-dir (ores-db/sql-scripts-directory)))
    (let* ((postgres-pw (ores-db/database--get-credential "postgres" "localhost" :secret))
           (script-path (expand-file-name "recreate_admin.sh" sql-dir))
           (default-directory sql-dir)
           (args '("-y"))
           (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                       process-environment)))
      (when skip-validation
        (setq args (append args '("--no-sql-validation"))))
      (if (not (file-executable-p script-path))
          (user-error "Script not found or not executable: %s" script-path)
        (compilation-start
         (format "%s %s" script-path (string-join args " "))
         nil
         (lambda (_) "*ores-db-recreate-admin*"))))))

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
     ((string= db-name "ores_template")
      (ores-db/recreate-template skip-validation))
     ((string= db-name "ores_admin")
      (ores-db/recreate-admin skip-validation))
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
  (let ((sql-dir (ores-db/sql-scripts-directory)))
    (let* ((postgres-pw (ores-db/database--get-credential "postgres" "localhost" :secret))
           (script-path (expand-file-name "recreate_database.sh" sql-dir))
           (default-directory sql-dir)
           (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                       process-environment)))
      (if (not (file-executable-p script-path))
          (user-error "Script not found or not executable: %s" script-path)
        (compilation-start
         (format "%s -y" script-path)
         nil
         (lambda (_) "*ores-db-recreate-all*"))))))

(defun ores-db/create-whimsical ()
  "Create a new database instance with a whimsical name.
Uses create_instance.sql to create from ores_template."
  (interactive)
  (let ((sql-dir (ores-db/sql-scripts-directory)))
    (let* ((postgres-pw (ores-db/database--get-credential "postgres" "localhost" :secret))
           (sql-path (expand-file-name "create_instance.sql" sql-dir))
           (default-directory sql-dir)
           (process-environment (cons (concat "PGPASSWORD=" postgres-pw)
                                       process-environment)))
      (if (not (file-exists-p sql-path))
          (user-error "Script not found: %s" sql-path)
        (compilation-start
         (format "psql -h localhost -U postgres -f %s" sql-path)
         nil
         (lambda (_) "*ores-db-create-whimsical*"))))))

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
    ("k" "Drop at point" ores-db/drop-at-point)]
   ["Create/Recreate"
    ("n" "New whimsical database" ores-db/create-whimsical)
    ("d" "Recreate current env" ores-db/recreate-current-env)
    ("r" "Recreate at point" ores-db/recreate-at-point)
    ("e" "Recreate environment..." ores-db/recreate-env-database)
    ("R" "Recreate ALL (nuclear)" ores-db/recreate-all)]
   ["Utilities"
    ("v" "Export env vars" ores-db/set-env-vars)
    ("u" "Unset env vars" ores-db/unset-env-vars)
    ("V" "Show env vars" ores-db/show-env-vars)
    ("q" "Quit" transient-quit-one)]])

(provide 'ores-db)
;;; ores-db.el ends here
