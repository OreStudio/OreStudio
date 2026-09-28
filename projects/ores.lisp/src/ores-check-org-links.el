;;; ores-check-org-links.el --- Fail on a link org cannot resolve. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; The site build aborts on the first link it cannot resolve, which is the wrong
;; place to learn about one: it names a single file, it runs after the whole
;; publish has started, and on main it runs after the merge. Two defects have
;; reached main that way -- a file: link with a line-number search into a Python
;; file, and a POSIX character class that read as a link.
;;
;; This checks the one shape that is always a mistake: a link whose raw target
;; begins with a colon. No link type does. The shape appears when an inline
;; verbatim span is closed early by a character inside it -- =grep '[[:space:]]*='
;; closes at the = after the class, and the [[ that follows reads as a link to
;; :space: -- so the check is a property of the parsed document, not of the text.
;;
;; Only files that contain a [[: are parsed, which is what keeps this cheap: the
;; text scan reads every org file, and the parser sees the handful with a
;; candidate. A file whose parse fails is reported and does not fail the check;
;; the site build is the authority on whether a document exports.
;;
;; Usage:
;;
;;   emacs -Q --script projects/ores.lisp/src/ores-check-org-links.el [root]
;;
;;; Code:
(defvar ores/link-check-root
  (expand-file-name (or (pop command-line-args-left) "./"))
  "Directory to scan for org documents.")

(defvar ores/link-check-exclude
  "\\(^\\|/\\)\\(\\.packages\\|vcpkg\\|build\\|tmp\\|\\.claude/worktrees\\)/\\|projects/ores.org-js"
  "Directories the site build does not publish.")

(defun ores/link-check-files ()
  "Every org file under the root that the site build publishes."
  (let ((files nil))
    (dolist (file (directory-files-recursively ores/link-check-root "\\.org\\'"))
      (let ((rel (file-relative-name file ores/link-check-root)))
        (unless (string-match-p ores/link-check-exclude rel)
          (push file files))))
    (nreverse files)))

(defun ores/link-check-has-candidate-p (file)
  "Non-nil when FILE contains the only text that can precede the defect."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward "\\[\\[:" nil t)))

(defun ores/link-check-report (file)
  "Report every unresolved-looking link in FILE.  Return the count."
  (let ((found 0))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (let ((tree (condition-case err
                      (org-element-parse-buffer)
                    (error
                     (message "ores-check-org-links: cannot parse %s: %s"
                              (file-relative-name file ores/link-check-root)
                              (error-message-string err))
                     nil))))
        (when tree
          (org-element-map tree 'link
            (lambda (link)
              (let ((raw (org-element-property :raw-link link)))
                (when (and raw (string-prefix-p ":" raw))
                  (setq found (1+ found))
                  (message "%s:%d: link org cannot resolve: %S"
                           (file-relative-name file ores/link-check-root)
                           (line-number-at-pos (org-element-property :begin link))
                           raw))))))))
    found))

(let ((scanned 0)
      (parsed 0)
      (bad 0))
  (dolist (file (ores/link-check-files))
    (setq scanned (1+ scanned))
    (when (ores/link-check-has-candidate-p file)
      (setq parsed (1+ parsed))
      (setq bad (+ bad (ores/link-check-report file)))))
  (message "ores-check-org-links: %d file(s) scanned, %d parsed, %d unresolved link(s)."
           scanned parsed bad)
  (kill-emacs (if (> bad 0) 1 0)))

(provide 'ores-check-org-links)
;;; ores-check-org-links.el ends here
