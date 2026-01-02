;;; ores-shell.el --- Shell for ORE Studio     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Marco Craveiro

;; Author: Your Name <you@example.com>
;; Version: 0.2
;; Package-Requires: ((emacs "24.3"))
;; Keywords: shells
;; URL: https://github.com/OreStudio/OreStudio

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

;; Comint-based mode for the ORE Studio shell.

;;; Code:
(require 'font-lock)
(require 'comint)

(defvar ores-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'ores-shell-send-input)
    (define-key map "\C-c\C-d" 'ores-shell-disconnect)
    map)
  "Keymap for `ores-shell-mode'.")

(defvar ores-shell-process nil
  "The current ores-shell process.")

(defvar ores-shell-buffer-name "*ores-shell*"
  "Name of the ores-shell buffer.")

(defvar ores-shell-prompt "ores-shell> "
  "Prompt for ores-shell.")
;; (setq ores-shell-program "~/Development/OreStudio/OreStudio.local1/build/output/linux-clang-debug/publish/bin/ores.shell")
(defvar ores-shell-program "./ores.shell"
  "Path to the ORE Studio shell executable.")

(defvar ores-shell-last-program nil
  "Last used ORE Studio shell executable path.")

(defconst ores-shell-keywords
  '("connect" "login" "accounts" "create" "exit" "help" "currencies")
  "Top-level command keywords for ores-client.")

(defface ores-shell-command-face
  '((t (:foreground "#00ffff"   ; bright coral red
        :weight bold
        :underline nil)))
  "Face used for the command words (connect, login, accounts, …)."
  :group 'ores-client)

(defconst ores-shell-font-lock-keywords
  `(
    ;; ---- COMMAND WORDS -------------------------------------------------
    (,(regexp-opt ores-shell-keywords 'words) . 'ores-client-command-face)

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

(defun ores-shell-send-input ()
  "Send input to ORE Studio shell process."
  (interactive)
  (comint-send-input))

(defun ores-shell-disconnect ()
  "Send disconnect command to ores-shell process."
  (interactive)
  (when (comint-check-proc (current-buffer))
    (comint-send-string (current-buffer) "disconnect\n")))

(defun ores-shell--initialize ()
  "Initialize the comint buffer for ores-shell."
  (unless (comint-check-proc (current-buffer))
    (let ((proc (get-buffer-process (current-buffer))))
      (setq ores-shell-process proc)
      (ores-shell-mode))))

(defun ores-shell--setup-comint ()
  "Set up comint for ores-shell."
  (setq comint-process-echoes nil)
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-regexp (concat "^" (regexp-quote ores-shell-prompt)))
  (setq comint-prompt-read-only t)
  (setq comint-input-autoexpand nil)
  (add-hook 'comint-preoutput-filter-functions 'ores-shell--filter-output nil t))

(defun ores-shell--filter-output (output)
  "Filter OUTPUT from ores-shell process."
  (replace-regexp-in-string "\r" "" output))

(defun ores-shell (&optional prefix)
  "Start an ores-shell session.
With PREFIX argument (\\[universal-argument]), always ask for path.
Otherwise use last path if set, or ask for path if not set."
  (interactive "P")
  (let* ((always-ask prefix)
         (program (cond
                   (always-ask
                    (read-file-name "Path to ORE Studio shell executable: "
                                    nil
                                    (or ores-shell-last-program
                                        ores-shell-program)
                                    t))
                   (ores-shell-last-program
                    ores-shell-last-program)
                   (t
                    (read-file-name "Path to ores-shell executable: "
                                    nil
                                    ores-shell-program
                                    t))))
         (buffer (get-buffer-create ores-shell-buffer-name)))

    (unless program
      (error "No executable path provided"))

    (setq program (expand-file-name program))

    (unless (file-executable-p program)
      (error "Client executable not found or not executable: %s" program))

    (setq ores-shell-last-program program)

    ;; Switch to buffer and ensure it's in ores-shell-mode BEFORE starting process
    (with-current-buffer buffer
      (unless (eq major-mode 'ores-shell-mode)
        (ores-shell-mode)))  ; this sets comint variables appropriately

    (pop-to-buffer buffer)

    (unless (comint-check-proc buffer)
      (let ((log-dir (expand-file-name "../log" (file-name-directory program))))
        (make-comint-in-buffer "ores-shell" buffer program nil
                               "--log-enabled" "--log-level" "trace"
                               "--log-directory" log-dir))
      (message "ORES client started. Type 'help' for available commands."))))

(provide 'ores-shell)
;;; ores-shell.el ends here
