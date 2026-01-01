;;; .build-skills.el --- -*- lexical-binding: t; -*-

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

;; Builds the Claude skills.

;;; Code:
(require 'package)
(require 'org)
(require 'org-id)
(require 'oc-bibtex)
(require 'ox-publish)
(require 'org-element)

(setq org-id-locations-file (expand-file-name "./.org-id-locations-file"))
(org-id-update-id-locations (directory-files-recursively "." "\\.org$"))
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'ox-gfm)

;; Load the publishing system
(require 'ox-publish)

;; Define the publishing project
(setq org-publish-project-alist
      '(
        ("skills:tangle"
         :recursive t
         :base-directory "./doc/skills/"
         :exclude "claude_code_skills.org"
         :publishing-function org-babel-tangle-publish
         :publishing-directory "./.claude/skills/")
        ("skills:export"
         :recursive t
         :base-directory "./doc/skills/"
         :exclude "claude_code_skills.org"
         :publishing-function org-gfm-publish-to-gfm
         :publishing-directory "./.claude/skills/"
         :with-author nil
         :with-creator t
         :with-toc t
         :section-numbers nil
         :time-stamp-file nil)
        ("skills:main" :components("skills:tangle" "skills:export"))
        ))

;; Generate the site output
(org-publish-all t)

;; Claude Code requires SKILL.md (uppercase) for skill discovery.
;; Rename all skill.md files to SKILL.md after publishing.
(dolist (file (directory-files-recursively "./.claude/skills/" "^skill\\.md$"))
  (let ((new-name (concat (file-name-directory file) "SKILL.md")))
    (rename-file file new-name t)
    (message "Renamed %s to %s" file new-name)))

(message "Build complete!")

(provide '.build-skills)
;;; .build-skills.el ends here
