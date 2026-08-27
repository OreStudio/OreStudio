;;; ores-org-ids.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Marco Craveiro

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

;; Single home for the org-id location map: the set of files that make
;; it up, the scan that builds it, and the staleness test that decides
;; whether a scan is needed.
;;
;; Every build target that resolves an [[id:...]] link needs this map
;; loaded in its own Emacs process.  Before this file existed each one
;; carried its own copy of the scan, and the copies had drifted: the
;; site build skipped .claude/worktrees/ while the skills and plan
;; builds scanned everything, so the map's contents depended on which
;; target wrote it last.
;;
;; Loaded as a library, this file only defines.  Run as a script it
;; rescans, which is what `compass index --org-ids' invokes:
;;
;;   (setq ores/org-ids-library-only t)
;;   (load-file ".../ores-org-ids.el")
;;   (ores/org-id-ensure)

;;; Code:
(require 'org-id)
(require 'seq)

(defconst ores/org-id-excluded-dirs
  '("build" "vcpkg" "external" "tmp" "node_modules")
  "Directories excluded from the id scan, relative to the scan root.
Third-party trees and generated output: vcpkg is a submodule of some
three and a half thousand directories, external holds vendored sources,
and build is our own output.  None of them holds a document we author,
and a README or NEWS file in one of them carrying an :ID: would be
indexed as though it were ours.

Matched against the path relative to the scan root, never against a bare
directory name.  Anchoring is what keeps doc/knowledge/external -- which
does hold pages we author and link to -- out of the exclusion.

Directories whose name begins with a dot are excluded separately, which
covers .git, .claude, .packages and .vcpkg without naming them.")

(defvar ores/org-id--scan-root nil
  "Root of the scan in progress, bound by `ores/org-id-files'.
`directory-files-recursively' passes its predicate only a directory
name, so the root it must be made relative to travels here instead.")

(defun ores/org-id--descend-p (dir)
  "Return non-nil when the id scan should descend into DIR.
Applied to directories rather than to files, so a dotfile that is real
content -- .journal.org in the repository root -- is still scanned."
  (let ((name (file-name-nondirectory (directory-file-name dir))))
    (not (or (string-prefix-p "." name)
             (and ores/org-id--scan-root
                  (member (file-relative-name (directory-file-name dir)
                                              ores/org-id--scan-root)
                          ores/org-id-excluded-dirs))))))

(defun ores/org-id-files (&optional root)
  "Return every org file under ROOT that contributes to the id location map.
ROOT defaults to `default-directory', which is the repository root for a
whole-repo scan.  The manual and help builds pass a subtree instead,
since each resolves links only within its own document set."
  (let ((ores/org-id--scan-root
         (directory-file-name (expand-file-name (or root default-directory)))))
    (directory-files-recursively
     ores/org-id--scan-root "\\.org\\'" nil #'ores/org-id--descend-p)))

(defun ores/org-id-rescan (&optional root)
  "Rebuild the id location map from ROOT and write it to disk.
`org-id-update-id-locations' reports every file it visits, which is five
thousand lines for a whole-repo scan.  That is suppressed here: compass
commands must not be piped, so a caller cannot filter it out, and the
scan takes about three seconds."
  (let* ((files (ores/org-id-files root))
         (count (length files)))
    (message "ores-org-ids: scanning %d file(s)%s" count
             (if root (format " under %s" root) ""))
    (let ((inhibit-message t))
      (org-id-update-id-locations files))
    (message "ores-org-ids: %d id(s) in %s"
             (hash-table-count org-id-locations)
             (abbreviate-file-name org-id-locations-file))))

(defun ores/org-id-scan-suppressed-p ()
  "Return non-nil when the caller has asked for the scan to be skipped.
Set by `compass build --no-index', for a caller that knows no page has
been added or renamed and wants none of the scan's cost."
  (member (getenv "ORES_SKIP_ORG_ID_SCAN") '("1" "true" "yes")))

(defun ores/org-id-ensure (&optional root)
  "Rescan unless the caller has suppressed it, in which case load.
This is what a full build target calls.  There is deliberately no
staleness test: the map needs rebuilding when an id is added, moved or
renamed, and a file modification time cannot distinguish that from an
edit to a page's prose.  Guessing wrong is a failed build, so the full
builds -- which are infrequent and already pay for a scan -- simply
scan.  The cheap path is `org-id-locations-load', taken by callers that
know the map is warm."
  (if (ores/org-id-scan-suppressed-p)
      (progn
        (message "ores-org-ids: scan suppressed, loading map as-is")
        (org-id-locations-load))
    (ores/org-id-rescan root)))

(unless (bound-and-true-p ores/org-ids-library-only)
  (setq org-id-locations-file (expand-file-name "./.org-id-locations-file"))
  (ores/org-id-rescan))

(provide 'ores-org-ids)
;;; ores-org-ids.el ends here
