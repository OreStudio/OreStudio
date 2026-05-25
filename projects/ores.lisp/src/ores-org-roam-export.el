;;; ores-org-roam-export.el --- org-roam static export utilities for the ORE Studio site. -*- lexical-binding: t -*-
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; Keywords: org-roam, export, static-site

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utilities for exporting org-roam graph data to JSON for static site
;; integration (e.g. org-roam-ui static mode).  Requires Emacs 29+ for
;; built-in SQLite support; works in both interactive and batch mode
;; without loading the full org-roam package.

;; Originally lifted from cunene's emacs config so the site build no
;; longer depends on a file under ~/.emacs.d/ on the CI runner.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'json)

(defun ores/org-roam-export-graph-json (db-path output-file &optional repo-root site-prefix)
  "Export org-roam graph from DB-PATH to OUTPUT-FILE as org-roam-ui JSON.
Optional REPO-ROOT strips the local filesystem prefix from file paths.
Optional SITE-PREFIX (e.g. \"/OreStudio/\") is prepended so paths become
site-relative HTML URLs.  Works in batch mode without loading org-roam.
Requires Emacs 29+ built-in SQLite support."
  (cond
   ((not (fboundp 'sqlite-open))
    (error "ores/org-roam-export-graph-json: Emacs built-in SQLite unavailable (needs Emacs 29+)"))
   ((not (file-exists-p db-path))
    (message "ores/org-roam-export-graph-json: %s not found, skipping" db-path))
   (t
    (make-directory (file-name-directory (expand-file-name output-file)) t)
    (let* ((db       (sqlite-open db-path))
           (nodes-q  "SELECT id, file, level, pos, title, properties, olp FROM nodes")
           (tags-q   "SELECT \"node-id\", tag FROM tags")
           (links-q  "SELECT source, dest, type FROM links")
           (node-rows (sqlite-select db nodes-q))
           (tag-rows  (sqlite-select db tags-q))
           (link-rows (sqlite-select db links-q))
           ;; node-id -> list of tag strings
           (tag-map   (make-hash-table :test 'equal))
           ;; unique tag set
           (all-tags-set (make-hash-table :test 'equal))
           ;; file path -> org source (read once per file)
           (file-cache  (make-hash-table :test 'equal)))
      ;; emacsql stores all scalars as prin1 strings: "foo" -> the string foo.
      (cl-flet ((unpack (s) (if s (condition-case nil (read s) (error s)) ""))
                (to-url (f)
                  (let* ((rel (if repo-root (file-relative-name f repo-root) f))
                         (html (replace-regexp-in-string "\\.org$" ".html" rel)))
                    (if site-prefix (concat site-prefix html) html)))
                (file-content (path)
                  (or (gethash path file-cache)
                      (let ((c (when (and path (file-exists-p path))
                                 (condition-case nil
                                     (with-temp-buffer
                                       (insert-file-contents path)
                                       (buffer-string))
                                   (error nil)))))
                        (puthash path c file-cache)
                        c))))
        ;; build tag index
        (dolist (row tag-rows)
          (let ((nid (unpack (nth 0 row))) (tag (unpack (nth 1 row))))
            (puthash nid (cons tag (gethash nid tag-map '())) tag-map)
            (puthash tag t all-tags-set)))
        ;; build output structures
        (let* ((nodes
                (mapcar
                 (lambda (row)
                   (let* ((id       (unpack (nth 0 row)))
                          (raw-path (unpack (nth 1 row)))
                          (file     (to-url raw-path))
                          (level    (or (nth 2 row) 0))
                          (pos      (or (nth 3 row) 0))
                          (title    (unpack (nth 4 row)))
                          (props    (let ((raw (nth 5 row)))
                                      (when raw
                                        (condition-case nil (read raw) (error nil)))))
                          (olp      (let ((raw (nth 6 row)))
                                      (when raw
                                        (condition-case nil (read raw) (error nil)))))
                          (tags     (apply #'vector (gethash id tag-map '())))
                          (content  (or (file-content raw-path) :json-null)))
                     (list :id id :file file :level level :pos pos :title title
                           :tags tags :content content
                           :properties (or props :json-null)
                           :olp (if (listp olp) (apply #'vector olp) []))))
                 node-rows))
               (links
                (mapcar (lambda (row)
                          (list :source (unpack (nth 0 row))
                                :target (unpack (nth 1 row))
                                :type   (unpack (nth 2 row))))
                        link-rows))
             (all-tags
              (let (ts)
                (maphash (lambda (k _) (push k ts)) all-tags-set)
                (apply #'vector ts)))
             (graph (list :nodes (apply #'vector nodes)
                          :links (apply #'vector links)
                          :tags  all-tags)))
        (sqlite-close db)
        (with-temp-file output-file
          (insert (json-encode graph)))
        (message "ores/org-roam-export-graph-json: wrote %s (%d nodes, %d links)"
                 output-file (length nodes) (length links))))))))

(provide 'ores-org-roam-export)
;;; ores-org-roam-export.el ends here
