;;; ores-batch-packages.el --- Keep batch package installs off the user's cache. -*- lexical-binding: t; -*-
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
;; Every batch script in this directory points `package-user-dir' at the
;; checkout's ./.packages. Emacs 30's `package--quickstart-maybe-refresh' then
;; deletes the *user's* quickstart cache under ~/.emacs.d, because
;; `package-quickstart' is nil under `emacs -Q'; `delete-file' signals when HOME
;; is read-only, so the install aborts before it finishes. That cache describes
;; the user's own package dir, not this project-local one, so a batch install
;; must leave it alone.
;;
;; Load this before the script's first `package-install'. It lives in one file
;; so a new batch script inherits the fix instead of re-deriving it, and so the
;; advice cannot drift between the scripts that install packages.
;;
;;; Code:
(require 'package)

;; The function is internal, so the advice is guarded: a future Emacs that
;; renames or drops it must leave this fix inert, not turn it into a load-time
;; error in every script that loads this file.
(when (fboundp 'package--quickstart-maybe-refresh)
  (advice-add 'package--quickstart-maybe-refresh :override #'ignore))

(provide 'ores-batch-packages)
;;; ores-batch-packages.el ends here
