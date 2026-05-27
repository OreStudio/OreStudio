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
;;     slug-named file in inbox/ and immediately expands the
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
;; State variables
;; ---------------------------------------------------------------------------

(defvar ores/capture--pending-file nil
  "Inbox path for the capture in progress; deleted on abort if already written.")

(defvar ores/capture--snippet-body nil
  "Pre-built snippet body with the title substituted; set in target setup,
consumed by the :hook so no project-root lookup is needed at hook time.")

;; ---------------------------------------------------------------------------
;; org-capture target: prompt for title, build snippet, return inbox path
;; ---------------------------------------------------------------------------

(defun ores/capture--title-to-slug (title)
  "Convert TITLE to a lowercase underscore slug."
  (let* ((s (downcase title))
         (s (replace-regexp-in-string "[[:space:]]+" "_" s))
         (s (replace-regexp-in-string "[^a-z0-9_]" "" s))
         (s (replace-regexp-in-string "_+" "_" s))
         (s (replace-regexp-in-string "^_+\\|_+$" "" s)))
    s))

(defun ores/capture--inbox-file ()
  "Prompt for title, pre-build the yasnippet body, return the inbox file path.
Everything that needs the project root happens here, while we are still in
the correct project context.  The :hook only needs to call yas-expand-snippet."
  (let* ((root  (ores/checkout-root))
         (title (read-string "Capture title: "))
         (slug  (ores/capture--title-to-slug title))
         (_     (when (string= "" slug)
                  (user-error "Capture title must contain alphanumeric characters to form a valid filename")))
         (file  (expand-file-name
                 (concat slug ".org")
                 (expand-file-name "doc/agile/product_backlog/inbox" root)))
         (snip  (expand-file-name
                 "projects/ores.lisp/snippets/org-mode/v2-capture" root))
         (raw   (if (file-exists-p snip)
                    (with-temp-buffer
                      (insert-file-contents snip)
                      (goto-char (point-min))
                      (when (re-search-forward "^# --\n" nil t)
                        (buffer-substring-no-properties (point) (point-max))))
                  (user-error "Capture snippet template not found: %s" snip)))
         (body  (when raw
                  (replace-regexp-in-string
                   (regexp-quote "${1:Title}")
                   (format "${1:%s}" title)
                   raw))))
    (setq ores/capture--pending-file file
          ores/capture--snippet-body body)
    file))

;; ---------------------------------------------------------------------------
;; Finalization hooks
;; ---------------------------------------------------------------------------

(defun ores/capture--prepare-finalize ()
  "On abort, mark buffer unmodified so Emacs does not prompt to save."
  (when (and org-note-abort ores/capture--pending-file)
    (set-buffer-modified-p nil)))

(defun ores/capture--finalize ()
  "On abort, delete the inbox file if an auto-save already wrote it."
  (when (and org-note-abort ores/capture--pending-file)
    (when (file-exists-p ores/capture--pending-file)
      (delete-file ores/capture--pending-file)))
  (setq ores/capture--pending-file  nil
        ores/capture--snippet-body  nil))

(add-hook 'org-capture-prepare-finalize-hook #'ores/capture--prepare-finalize)
(add-hook 'org-capture-after-finalize-hook   #'ores/capture--finalize)

;; ---------------------------------------------------------------------------
;; org-capture :hook — expand the pre-built snippet body
;; ---------------------------------------------------------------------------

(defun ores/capture--yas-expand ()
  "Expand the pre-built v2-capture snippet body in the org-capture buffer.
The body (with title already substituted) was built in `ores/capture--inbox-file'
so no project-root lookup is needed here."
  (unless (fboundp 'yas-expand-snippet)
    (user-error "yasnippet is not loaded; load yas-global-mode before using this capture template"))
  (when ores/capture--snippet-body
    (yas-expand-snippet ores/capture--snippet-body)))

;; ---------------------------------------------------------------------------
;; Template registration
;; ---------------------------------------------------------------------------

(defun ores/setup-capture-templates ()
  "Register key \"c\" (Capture inbox) in `org-capture-templates'.
The v2-capture snippet body is built in the target function while still in
project context, then expanded by the :hook.  On abort, the inbox file is
cleaned up by `ores/capture--finalize'.
Call once after `org' is loaded."
  (interactive)
  (add-to-list 'org-capture-templates
    '("c" "Capture (inbox)" plain
      (file ores/capture--inbox-file)
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
         (script (expand-file-name "projects/ores.compass/compass.sh" root)))
    (unless (file-executable-p script)
      (user-error "Compass script not found or not executable: %s" script))
    (let* ((cmd    (format "cd %s && %s capture --note %s"
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
        (user-error "compass capture failed:\n%s" output)))))

(provide 'ores-capture)
;;; ores-capture.el ends here
