;;; .build-plan.el --- version_zero.org             -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Keywords: c

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

;; Builds Task Juggler reports.

;;; Code:
(require 'org)
(require 'org-id)
(require 'oc-bibtex)
(require 'ox-publish)
(require 'org-element)

;; Prevent Emacs from entering debugger or showing full stack traces
(setq debug-on-error nil)

;; Optionally, also suppress debug-on-quit and debug-on-signal if needed
(setq debug-on-quit nil)

(add-to-list 'load-path "build/elisp")
(require 'ox-taskjuggler)

;; Create a custom publishing function
(defun ores/org-publish-to-taskjuggler (plist filename pub-dir)
  "Publish an Org file to TaskJuggler format."
  (let ((output-file (expand-file-name
                      (concat (file-name-base filename) ".tjp")
                      pub-dir)))
    (message "Publishing %s to %s" filename output-file)
    ;; Use the export function - check the actual signature
    (with-current-buffer (find-file-noselect filename)
      (let ((org-export-show-temporary-export-buffer nil))
        (org-taskjuggler-export)))))

(setq org-id-locations-file (expand-file-name "./.org-id-locations-file"))
(org-id-update-id-locations (directory-files-recursively "." "\\.org$"))

;; Define only the TaskJuggler project
(setq org-publish-project-alist
      '(
        ("plan"
         :base-directory "./doc/agile/v0/"
         :publishing-directory "./build/output/taskjuggler/"
         :publishing-function ores/org-publish-to-taskjuggler
         :base-extension "dummy"
         :include ("version_zero.org")
         :recursive nil)
        ))

;; Generate the site output
;; Wrap publishing in error handler to avoid backtraces
(condition-case err
    (progn
      (org-publish-all t)
      (message "Build complete!"))
  (error
   (message "Build failed: %s" (error-message-string err))))

(message "Build complete!")

(provide '.build-plan)
;;; .build-plan.el ends here
