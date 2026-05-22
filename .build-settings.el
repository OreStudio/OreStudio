;;; .build-settings.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Keywords: publish

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

;; Tangles doc/llm/claude_code_settings.org to .claude/settings.json.
;; Loads ores-babel.el so that ores/repo-root is available for path resolution;
;; falls back to default-directory (the project root set by the CMake
;; WORKING_DIRECTORY) if project detection returns nil in batch mode.

;;; Code:
(require 'org)
(require 'ob-core)
(require 'project)

(setq debug-on-error nil)
(setq debug-on-quit nil)

(setq org-id-locations-file (expand-file-name "./.org-id-locations-file"))
(setq package-user-dir (expand-file-name "./.packages"))

;; Load the ORE Studio babel environment, which defines ores/repo-root and
;; companions.  The path is relative to the project root (our CWD).
(load-file (expand-file-name "projects/ores.lisp/ores-babel.el"))

;; Resolve the project root.  ores/repo-root uses project-current; fall back
;; to default-directory (which equals CMAKE_SOURCE_DIR) if batch mode leaves
;; project detection unable to find the .git root.
(defvar ores/--settings-root
  (or (ores/repo-root)
      (file-name-as-directory (expand-file-name ".")))
  "Absolute path to the project root for this tangle run.")

;; Inject the tangle target into org-babel's JSON defaults.  The master block
;; in claude_code_settings.org carries no :tangle directive; this default
;; supplies it as an absolute path derived from ores/repo-root.  Blocks that
;; carry an explicit :tangle no are not affected (explicit args override
;; defaults).
(setq org-babel-default-header-args:json
      `((:tangle . ,(expand-file-name ".claude/settings.json"
                                      ores/--settings-root))))

(condition-case err
    (progn
      (org-babel-tangle-file
       (expand-file-name "doc/llm/claude_code_settings.org"
                         ores/--settings-root))
      (message "Settings deployed to %s"
               (expand-file-name ".claude/settings.json" ores/--settings-root)))
  (error
   (message "Settings deployment failed: %s" (error-message-string err))))

(provide '.build-settings)
;;; .build-settings.el ends here
