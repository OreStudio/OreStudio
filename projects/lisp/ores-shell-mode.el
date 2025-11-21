;;; ores-shell-mode.el --- trivial major mode for my command-line tool -*- lexical-binding: t; -*-

;; Author: Your Name <you@example.com>
;; Version: 0.2
;; Package-Requires: ((emacs "24.3"))
;; Keywords: languages, ores-shell
;; URL: https://github.com/OreStudio/OreStudio

;;; Commentary:

;; A tiny major mode that highlights the simple protocol used by
;; "ores-client".  Commands (connect, login, accounts, create, exit) are
;; displayed in a **bold, bright face** so they are instantly
;; distinguishable from arguments.

;;; Code:
(require 'font-lock)

;; ----------------------------------------------------------------------
;; 1. Syntax table (unchanged)
;; ----------------------------------------------------------------------
(defvar ores-shell-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    table)
  "Syntax table for `ores-shell-mode'.")

;; ----------------------------------------------------------------------
;; 2. Command keywords – the list you may extend later
;; ----------------------------------------------------------------------
(defconst ores-client-keywords
  '("connect" "login" "accounts" "create" "exit" "help"  "currencies")
  "Top-level command keywords for ores-client.")

;; ----------------------------------------------------------------------
;; 3. **Custom face for commands**
;; ----------------------------------------------------------------------
(defface ores-client-command-face
  '((t (:foreground "#00ffff"   ; bright coral red
        :weight bold
        :underline nil)))
  "Face used for the command words (connect, login, accounts, …)."
  :group 'ores-client)

;; ----------------------------------------------------------------------
;; 4. Font-lock rules
;; ----------------------------------------------------------------------
(defconst ores-client-font-lock-keywords
  `(
    ;; ---- COMMAND WORDS -------------------------------------------------
    (,(regexp-opt ores-client-keywords 'words) . 'ores-client-command-face)

    ;; ---- Sub-command under "accounts" ----------------------------------
    ( "\\<accounts[ \t]+\\(create\\)\\>" 1 font-lock-function-name-face)

    ;; ---- Numbers -------------------------------------------------------
    ( "\\<[0-9]+\\(?:\\.[0-9]+\\)?\\>" . font-lock-constant-face)

    ;; ---- Double-quoted strings -----------------------------------------
    ( "\"\\([^\\\"]\\|\\\\.\\)*\"" . font-lock-string-face)

    ;; ---- Email addresses (loose pattern) -------------------------------
    ( "\\<[[:alnum:]_.-]+@[[:alnum:]_.-]+\\.[[:alpha:]]+\\>" . font-lock-type-face)

    ;; ---- localhost -----------------------------------------------------
    ( "\\<localhost\\>" . font-lock-builtin-face)
    )
  "Font-lock keywords for `ores-shell-mode'.")

;; ----------------------------------------------------------------------
;; 5. Major mode definition
;; ----------------------------------------------------------------------
(define-derived-mode ores-shell-mode prog-mode "Ores-Client"
  "Major mode for editing Ores-Client script files.

\\{ores-shell-mode-map}"
  :syntax-table ores-shell-mode-syntax-table
  (setq font-lock-defaults '(ores-client-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ores-client\\'" . ores-shell-mode))

(provide 'ores-shell-mode)
;;; ores-shell-mode.el ends here
