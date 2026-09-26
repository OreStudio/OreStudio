;;; ores-build-dsh-settings.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Keywords: publish

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tangles doc/llm/dsh_settings.org into .dsh/ and installs the result into
;; ~/.dsh, which is where the DSH harness reads it.  Loads ores-babel.el so
;; that ores/repo-root is available for path resolution; falls back to
;; default-directory (the project root set by the CMake WORKING_DIRECTORY) if
;; project detection returns nil in batch mode.
;;
;; The install is a copy, never a symbolic link into the checkout.  A confined
;; command can write the checkout, so a link would let a confined command widen
;; the sandbox that confines it.  Copying puts the live rules outside the
;; workspace where the sandbox cannot reach them, so they change only when a
;; person runs this script.

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
(load-file (expand-file-name "projects/ores.lisp/src/ores-babel.el"))

;; Resolve the project root.  ores/repo-root uses project-current; fall back
;; to default-directory (which equals CMAKE_SOURCE_DIR) if batch mode leaves
;; project detection unable to find the .git root.
(defvar ores/--dsh-root
  (or (ores/repo-root)
      (file-name-as-directory (expand-file-name ".")))
  "Absolute path to the project root for this tangle run.")

;; The tangle targets are declared on the master blocks in dsh_settings.org as
;; relative paths, so they resolve against this run's working directory, which
;; is the project root here and under the CMake target alike.  The sections
;; that carry an explicit :tangle no are fragments and are not written.

(defun ores/--dsh-install ()
  "Copy the tangled rules into ~/.dsh and report whether the patch uses them.

The copy is what makes the rules live; see the Commentary for why it is not a
symbolic link.  cordis.patch.yml is reported on rather than written, because
the harness manages part of that file itself."
  (let* ((generated (expand-file-name ".dsh" ores/--dsh-root))
         (live (expand-file-name "~/.dsh"))
         (list-src (expand-file-name "sandbox-writable-paths" generated))
         (list-dst (expand-file-name "sandbox-writable-paths" live))
         (runner-src (expand-file-name "bin/dsh-bwrap-writable-ores.sh" generated))
         (runner-dst (expand-file-name "bin/dsh-bwrap-writable-ores.sh" live))
         (patch (expand-file-name "cordis.patch.yml" live)))
    (make-directory (file-name-directory runner-dst) t)
    (copy-file list-src list-dst t)
    (copy-file runner-src runner-dst t)
    (set-file-modes runner-dst #o755)
    (message "Installed %s" list-dst)
    (message "Installed %s" runner-dst)
    (if (and (file-readable-p patch)
             (with-temp-buffer
               (insert-file-contents patch)
               (goto-char (point-min))
               (re-search-forward "dsh-bwrap-writable-ores\\.sh" nil t)))
        (message "The home patch selects the runner: %s" patch)
      (message (concat "WARNING: %s does not select the runner."
                       " Add the block from doc/llm/dsh_settings.org."))
      (message "         Until it does, no confined command receives these grants."))))

(condition-case err
    (progn
      (make-directory (expand-file-name ".dsh/bin" ores/--dsh-root) t)
      (org-babel-tangle-file
       (expand-file-name "doc/llm/dsh_settings.org"
                         ores/--dsh-root))
      (message "DSH settings tangled to %s"
               (expand-file-name ".dsh" ores/--dsh-root))
      (ores/--dsh-install))
  (error
   (message "DSH settings deployment failed: %s" (error-message-string err))
   (kill-emacs 1)))

(provide 'ores-build-dsh-settings)
;;; ores-build-dsh-settings.el ends here
