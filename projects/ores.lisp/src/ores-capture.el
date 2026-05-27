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
;; Provides two entry points for creating v2 capture docs in
;; doc/agile/product_backlog/inbox/ of the current checkout:
;;
;;   `ores/capture'                - interactive command; calls compass.sh.
;;                                   Good for LLM-driven and CLI-style use.
;;   `ores/setup-capture-templates' - registers a capture template under key
;;                                   "c" in BOTH `org-capture-templates' (for
;;                                   M-x org-capture / C-c c) and
;;                                   `org-roam-capture-templates' (for M-x
;;                                   org-roam-capture / C-c n c).
;;
;; Call `ores/setup-capture-templates' once at startup, e.g.:
;;
;;   (with-eval-after-load 'org
;;     (ores/setup-capture-templates))
;;
;; Suggested key binding for `ores/capture':
;;
;;   (global-set-key (kbd "C-c o c") #'ores/capture)
;;
;; Public API:
;;
;;   `ores/capture'                  - create a capture via compass.sh
;;   `ores/setup-capture-templates'  - register in org-capture and org-roam-capture
;;   `ores/setup-capture-snippets'   - load the yasnippet v2-capture snippet
;;
;; Yasnippet usage: open (or create) a .org file in inbox/, type "cap" and
;; press TAB.  Tab stops: $1 title, $2 description, $3 What, $4 Why, $5
;; References, $0 See also.  UUID and dates are filled automatically.

;;; Code:

(require 'ores-env)

;; ---------------------------------------------------------------------------
;; Inbox path
;; ---------------------------------------------------------------------------

(defun ores/capture--inbox-dir ()
  "Return the absolute path to product_backlog/inbox/ for the current checkout."
  (expand-file-name "doc/agile/product_backlog/inbox" (ores/checkout-root)))

;; ---------------------------------------------------------------------------
;; org-roam capture template
;; ---------------------------------------------------------------------------

(defconst ores/capture--head-template
  (concat ":PROPERTIES:\n"
          ":ID: %(upcase (org-id-new))\n"
          ":END:\n"
          "#+title: ${title}\n"
          "#+description: \n"
          "#+type: capture\n"
          "#+version: 2\n"
          "#+level: cross\n"
          "#+filetags: :inbox:capture:\n"
          "#+created: %(format-time-string \"%Y-%m-%d\")\n"
          "#+updated: %(format-time-string \"%Y-%m-%d\")\n"
          "\n"
          "This page is a "
          "[[id:671F18E4-E09C-4B3B-BD24-D33DF8AE38A6][capture]] "
          "in the *inbox* bucket of the product backlog "
          "— a pre-sprint idea, not yet pulled into a sprint as a story.\n\n")
  "Frontmatter inserted at the top of every new inbox capture file.")

(defconst ores/capture--body-template
  "* What\n\n%?\n\n* Why\n\n\n\n* References\n\n-\n\n* See also\n\n-\n"
  "Body template for a new inbox capture; %? marks the initial cursor position.")

;; ---------------------------------------------------------------------------
;; org-capture target function (used when org-roam is not available)
;; ---------------------------------------------------------------------------

(defun ores/capture--title-to-slug (title)
  "Convert TITLE to a lowercase underscore-separated slug."
  (string-trim
   (replace-regexp-in-string "[^a-z0-9]+" "_" (downcase title))
   "_" "_"))

(defun ores/capture--org-capture-target ()
  "Set up a new v2 inbox capture file as the `org-capture' target.
Prompts for a title, writes the frontmatter, and positions point at
the end of the buffer so org-capture appends the body template."
  (let* ((title (read-string "Capture title: "))
         (slug  (ores/capture--title-to-slug title))
         (date  (format-time-string "%Y-%m-%d"))
         (id    (upcase (org-id-new)))
         (file  (expand-file-name (concat slug ".org")
                                  (ores/capture--inbox-dir))))
    (find-file file)
    (when (= (buffer-size) 0)
      (insert ":PROPERTIES:\n"
              ":ID: " id "\n"
              ":END:\n"
              "#+title: " title "\n"
              "#+description: \n"
              "#+type: capture\n"
              "#+version: 2\n"
              "#+level: cross\n"
              "#+filetags: :inbox:capture:\n"
              "#+created: " date "\n"
              "#+updated: " date "\n"
              "\n"
              "This page is a "
              "[[id:671F18E4-E09C-4B3B-BD24-D33DF8AE38A6][capture]] "
              "in the *inbox* bucket of the product backlog "
              "— a pre-sprint idea, not yet pulled into a sprint as a story.\n\n"))
    (goto-char (point-max))))

;; ---------------------------------------------------------------------------
;; Template registration
;; ---------------------------------------------------------------------------

(defun ores/setup-capture-templates ()
  "Register key \"c\" (Capture inbox) in org-capture and org-roam-capture.
Call once at startup, after `org' is loaded.  org-roam registration is
skipped silently if org-roam is not present."
  (interactive)
  ;; Standard org-capture (M-x org-capture / C-c c)
  (add-to-list 'org-capture-templates
    `("c" "Capture (inbox)" plain
      (function ores/capture--org-capture-target)
      ,ores/capture--body-template
      :unnarrowed t))
  ;; org-roam-capture (M-x org-roam-capture / C-c n c) — optional
  (when (boundp 'org-roam-capture-templates)
    (add-to-list 'org-roam-capture-templates
      `("c" "Capture (inbox)" plain
        ,ores/capture--body-template
        :target (file+head
                 "%(expand-file-name \"${slug}.org\" (ores/capture--inbox-dir))"
                 ,ores/capture--head-template)
        :unnarrowed t))))

;; ---------------------------------------------------------------------------
;; Direct command (compass-backed, suitable for interactive and LLM use)
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

;; ---------------------------------------------------------------------------
;; Yasnippet integration
;; ---------------------------------------------------------------------------

(defun ores/setup-capture-snippets ()
  "Register the ores.lisp snippet directory with yasnippet and reload.
Adds projects/ores.lisp/snippets/ to `yas-snippet-dirs' so the
v2-capture snippet (trigger: \"cap\") is available in org-mode buffers."
  (interactive)
  (let ((snippet-dir (expand-file-name "projects/ores.lisp/snippets"
                                       (ores/checkout-root))))
    (if (not (fboundp 'yas-load-directory))
        (user-error "yasnippet is not loaded; load yas-global-mode before calling this")
      (unless (member snippet-dir yas-snippet-dirs)
        (add-to-list 'yas-snippet-dirs snippet-dir))
      (yas-load-directory snippet-dir)
      (message "ores-capture: loaded snippets from %s" snippet-dir))))

(provide 'ores-capture)
;;; ores-capture.el ends here
