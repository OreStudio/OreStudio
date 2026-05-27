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
;; org-capture target: prompt for title, create slug-named file in inbox/
;; ---------------------------------------------------------------------------

(defvar-local ores/capture--pending-title nil
  "Title entered during target setup; used to pre-fill the yasnippet $1 field.")

(defun ores/capture--title-to-slug (title)
  "Convert TITLE to a lowercase underscore slug."
  (let* ((s (downcase title))
         (s (replace-regexp-in-string "[[:space:]]+" "_" s))
         (s (replace-regexp-in-string "[^a-z0-9_]" "" s))
         (s (replace-regexp-in-string "_+" "_" s))
         (s (replace-regexp-in-string "^_+\\|_+$" "" s)))
    s))

(defun ores/capture--setup-inbox-file ()
  "Prompt for a title, create a slug-named org file in inbox/, and position point.
Stores the title buffer-locally so `ores/capture--yas-expand' can pre-fill $1."
  (let* ((title (read-string "Capture title: "))
         (slug  (ores/capture--title-to-slug title))
         (file  (expand-file-name (concat slug ".org")
                                  (ores/capture--inbox-dir))))
    (find-file file)
    (setq-local ores/capture--pending-title title)
    (goto-char (point-min))))

;; ---------------------------------------------------------------------------
;; org-capture hook: expand the v2-capture yasnippet into the buffer
;; ---------------------------------------------------------------------------

(defun ores/capture--yas-expand ()
  "Expand the v2-capture snippet in the current org-capture buffer.
Pre-fills the $1 tab stop with the title from `ores/capture--pending-title'
so the user sees it ready to confirm or edit.  Reads the snippet body from
the canonical snippet file so org-capture and \"cap\" TAB share one template."
  (unless (fboundp 'yas-expand-snippet)
    (user-error "yasnippet is not loaded; load yas-global-mode before using this capture template"))
  (let* ((snippet-file (expand-file-name
                        "projects/ores.lisp/snippets/org-mode/v2-capture"
                        (ores/checkout-root)))
         (raw (with-temp-buffer
                (insert-file-contents snippet-file)
                (goto-char (point-min))
                (when (re-search-forward "^# --\n" nil t)
                  (buffer-substring-no-properties (point) (point-max)))))
         (title (or ores/capture--pending-title "Title"))
         ;; Substitute the captured title as the editable default for $1.
         (body  (replace-regexp-in-string
                 (regexp-quote "${1:Title}")
                 (format "${1:%s}" title)
                 raw)))
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
