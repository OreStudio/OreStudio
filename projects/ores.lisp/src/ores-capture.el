;;; ores-capture.el --- Product-backlog capture for ORE Studio. -*- lexical-binding: t; -*-

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
;; Two entry points for creating v2 capture docs in
;; doc/agile/product_backlog/inbox/ of the current checkout:
;;
;;   `ores/setup-capture-templates' - registers key "c" in
;;     `org-capture-templates'.  Selecting it creates a fresh
;;     timestamped file in inbox/ and immediately expands the
;;     v2-capture yasnippet so you fill in title, description, What,
;;     Why etc. via TAB.  Requires yasnippet to be active.
;;
;;   `ores/capture' - interactive command that calls compass.sh.
;;     Useful for LLM-driven and CLI-style captures.
;;
;; Setup (call once at startup):
;;
;;   (with-eval-after-load 'org
;;     (ores/setup-capture-templates))
;;
;; Suggested direct key binding:
;;
;;   (global-set-key (kbd "C-c o c") #'ores/capture)
;;
;; Public API:
;;
;;   `ores/setup-capture-templates'  - register in org-capture
;;   `ores/capture'                  - create a capture via compass.sh

;;; Code:

(require 'ores-env)

;; ---------------------------------------------------------------------------
;; Inbox path
;; ---------------------------------------------------------------------------

(defun ores/capture--inbox-dir ()
  "Return the absolute path to product_backlog/inbox/ for the current checkout."
  (expand-file-name "doc/agile/product_backlog/inbox" (ores/checkout-root)))

;; ---------------------------------------------------------------------------
;; org-capture target: create a fresh timestamped file in inbox/
;; ---------------------------------------------------------------------------

(defun ores/capture--setup-inbox-file ()
  "Create a timestamped org file in inbox/ and position point at start.
Used as the (function ...) target in `org-capture-templates'."
  (let ((file (expand-file-name
               (format-time-string "capture_%Y%m%d_%H%M%S.org")
               (ores/capture--inbox-dir))))
    (find-file file)
    (goto-char (point-min))))

;; ---------------------------------------------------------------------------
;; org-capture hook: expand the v2-capture yasnippet into the buffer
;; ---------------------------------------------------------------------------

(defun ores/capture--yas-expand ()
  "Expand the v2-capture snippet in the current org-capture buffer.
Reads the snippet body from the canonical snippet file so that the
yasnippet (used for \"cap\" TAB) and org-capture share one template."
  (unless (fboundp 'yas-expand-snippet)
    (user-error "yasnippet is not loaded; load yas-global-mode before using this capture template"))
  (let* ((snippet-file (expand-file-name
                        "projects/ores.lisp/snippets/org-mode/v2-capture"
                        (ores/checkout-root)))
         (body (with-temp-buffer
                 (insert-file-contents snippet-file)
                 (goto-char (point-min))
                 ;; Skip yasnippet header lines; body starts after "# --"
                 (when (re-search-forward "^# --\n" nil t)
                   (buffer-substring-no-properties (point) (point-max))))))
    (when body
      (yas-expand-snippet body))))

;; ---------------------------------------------------------------------------
;; Template registration
;; ---------------------------------------------------------------------------

(defun ores/setup-capture-templates ()
  "Register key \"c\" (Capture inbox) in `org-capture-templates'.
Creates a fresh timestamped file in inbox/ and expands the v2-capture
yasnippet for interactive filling.  Call once after `org' is loaded."
  (interactive)
  (add-to-list 'org-capture-templates
    '("c" "Capture (inbox)" plain
      (function ores/capture--setup-inbox-file)
      ""
      :hook ores/capture--yas-expand
      :unnarrowed t)))

;; ---------------------------------------------------------------------------
;; Direct command (compass-backed, suitable for LLM and CLI use)
;; ---------------------------------------------------------------------------

(defun ores/capture (note)
  "Create a capture in the product backlog inbox for the current checkout.
NOTE is the freeform text passed to `compass capture --note'.
Opens the created file for further editing."
  (interactive "sCapture note: ")
  (let* ((root   (ores/checkout-root))
         (script (expand-file-name "projects/ores.compass/compass.sh" root))
         (cmd    (format "cd %s && %s capture --note %s"
                         (shell-quote-argument root)
                         (shell-quote-argument script)
                         (shell-quote-argument note)))
         (output (shell-command-to-string cmd))
         (file   (when (string-match "Created: \\(.+\\.org\\)$" output)
                   (expand-file-name (match-string 1 output) root))))
    (if file
        (progn
          (message "Capture created: %s" file)
          (find-file file))
      (user-error "compass capture failed:\n%s" output))))

(provide 'ores-capture)
;;; ores-capture.el ends here
