;;; ores-client-shell.el --- c                       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Keywords: client

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
(require 'comint)

(defvar ores-client-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'ores-client-shell-send-input)
    (define-key map "\C-c\C-d" 'ores-client-shell-disconnect)
    map)
  "Keymap for `ores-client-shell-mode'.")

(defvar ores-client-shell-process nil
  "The current ores-client-shell process.")

(defvar ores-client-shell-buffer-name "*ores-client-shell*"
  "Name of the ores-client-shell buffer.")

(defvar ores-client-shell-prompt "ores-client-shell> "
  "Prompt for ores-client-shell.")
;; (setq ores-client-shell-program "~/Development/OreStudio/OreStudio.local1/build/output/linux-clang-debug/publish/bin/ores.client")
(defvar ores-client-shell-program "./ores.client"
  "Path to the ores-client-shell executable.")

(defvar ores-client-shell-last-program nil
  "Last used ores-client executable path.")

(defun ores-client-shell-send-input ()
  "Send input to ores-client-shell process."
  (interactive)
  (comint-send-input))

(defun ores-client-shell-disconnect ()
  "Send disconnect command to ores-client-shell process."
  (interactive)
  (when (comint-check-proc (current-buffer))
    (comint-send-string (current-buffer) "disconnect\n")))

(defun ores-client-shell--initialize ()
  "Initialize the comint buffer for ores-client-shell."
  (unless (comint-check-proc (current-buffer))
    (let ((proc (get-buffer-process (current-buffer))))
      (setq ores-client-shell-process proc)
      (ores-client-shell-mode))))

(defun ores-client-shell--setup-comint ()
  "Set up comint for ores-client-shell."
  (setq comint-process-echoes nil)
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-regexp (concat "^" (regexp-quote ores-client-shell-prompt)))
  (setq comint-prompt-read-only t)
  (setq comint-input-autoexpand nil)
  (add-hook 'comint-preoutput-filter-functions 'ores-client-shell--filter-output nil t))

(defun ores-client-shell--filter-output (output)
  "Filter OUTPUT from ores-client-shell process."
  (replace-regexp-in-string "\r" "" output))

(define-derived-mode ores-client-shell-mode comint-mode "Ores-Client-Shell"
  "Major mode for interacting with ORES client.

Major commands:
\\[comint-send-input] - Send input
\\[ores-client-shell-disconnect] - Send disconnect command
\\[comint-interrupt-subjob] - Interrupt current command

Example session:
  ores-client> connect localhost 55555 test
  ores-client> accounts create newuser3 123 567 newuser3@example.com 1
  ores-client> help
  ores-client> exit"
  :syntax-table nil :abbrev-table nil
  (setq comint-prompt-regexp (concat "^" (regexp-quote ores-client-shell-prompt)))
  (setq comint-prompt-read-only t)
  (setq-local paragraph-start comint-prompt-regexp)
  (ores-client-shell--setup-comint))

(defun ores-client (&optional prefix)
  "Start an ores-client session.
With PREFIX argument (\\[universal-argument]), always ask for path.
Otherwise use last path if set, or ask for path if not set."
  (interactive "P")
  (let* ((always-ask prefix)
         (program (cond
                   (always-ask
                    (read-file-name "Path to ores-client executable: "
                                    nil
                                    (or ores-client-shell-last-program
                                        ores-client-shell-program)
                                    t))
                   (ores-client-shell-last-program
                    ores-client-shell-last-program)
                   (t
                    (read-file-name "Path to ores-client executable: "
                                    nil
                                    ores-client-shell-program
                                    t))))
         (buffer (get-buffer-create ores-client-shell-buffer-name)))

    (unless program
      (error "No executable path provided"))

    (setq program (expand-file-name program))

    (unless (file-executable-p program)
      (error "Client executable not found or not executable: %s" program))

    (setq ores-client-shell-last-program program)

    (pop-to-buffer buffer)

    (unless (comint-check-proc buffer)
      (make-comint-in-buffer "ores-client" buffer program nil)
      (ores-client-shell-mode)
      (message "ORES client started. Type 'help' for available commands."))))

(provide 'ores-client-shell)
;;; ores-client-shell.el ends here
