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
;;   `ores/setup-capture-templates' - registers an org-roam capture template
;;                                   (key "c") so `org-roam-capture' presents
;;                                   "Capture (inbox)" as an option.
;;
;; Call `ores/setup-capture-templates' once after org-roam is loaded, e.g.:
;;
;;   (with-eval-after-load 'org-roam
;;     (ores/setup-capture-templates))
;;
;; Suggested key binding for `ores/capture':
;;
;;   (global-set-key (kbd "C-c o c") #'ores/capture)
;;
;; Public API:
;;
;;   `ores/capture'                  - create a capture via compass.sh
;;   `ores/setup-capture-templates'  - register org-roam template

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

(defun ores/setup-capture-templates ()
  "Register the inbox capture template with `org-roam-capture-templates'.
Call this once after org-roam has been loaded."
  (add-to-list 'org-roam-capture-templates
    `("c" "Capture (inbox)" plain
      ,ores/capture--body-template
      :target (file+head
               ;; %(…) is evaluated at capture time, so ores/checkout-root
               ;; resolves against the active project.
               "%(expand-file-name \"${slug}.org\" (ores/capture--inbox-dir))"
               ,ores/capture--head-template)
      :unnarrowed t)))

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

(provide 'ores-capture)
;;; ores-capture.el ends here
