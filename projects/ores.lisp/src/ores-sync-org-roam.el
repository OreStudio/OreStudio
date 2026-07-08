;;; ores-sync-org-roam.el --- Sync the org-roam database. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Marco Craveiro
;;
;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Maintainer: Marco Craveiro <marco.craveiro@gmail.com>
;; URL: https://github.com/OreStudio/OreStudio
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Regenerates .org-roam.db from the repository's org files. This is the
;; standalone counterpart of the sync that ores-build-site.el performs as a
;; side effect of publishing the site; run it via `compass build --direct
;; org-roam-db-sync` (or the equivalent org_roam_db_sync CMake target), or
;; `compass index --org-roam-db-sync` when only the database needs refreshing.
;;
;;; Code:
(require 'package)

(setq debug-on-error nil)
(setq debug-on-quit nil)

;; Same package bootstrap as ores-build-site.el: packages live in
;; ./.packages so CI and local runs share the cache.
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'org-roam)
  (condition-case err
      (package-refresh-contents)
    (error (message "Warning: could not refresh package archives (%s); using cached contents"
                    (error-message-string err))))
  (package-install 'org-roam))

(condition-case err
    (progn
      (setq org-roam-directory (expand-file-name "./"))
      (setq org-roam-db-location (expand-file-name "./.org-roam.db"))
      (setq org-roam-file-exclude-regexp
            "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\|\\.claude/worktrees\\)/\\|projects/ores.org-js")
      (require 'org-roam)
      (org-roam-db-sync)
      (message "org-roam DB sync succeeded: %s" org-roam-db-location)
      (kill-emacs 0))
  (error
   (message "org-roam DB sync failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide 'ores-sync-org-roam)
;;; ores-sync-org-roam.el ends here
