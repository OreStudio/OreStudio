;;; ores-build-manual.el --- Build user manual PDF. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Marco Craveiro
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
;; Exports doc/manual/user_guide/user_manual.org to PDF via LaTeX.
;; Output: doc/manual/user_guide/user_manual.pdf
;;
;;; Code:
(require 'org)
(require 'org-id)
(require 'ox-latex)

(setq debug-on-error nil)
(setq debug-on-quit nil)

;; Render #+begin_src blocks via the listings package (background
;; colour and breaklines configured in user_manual.org's latex_header)
;; rather than plain verbatim. ores-shell is a custom babel language
;; name with no listings definition of its own; map it to bash, same
;; as org's own default mapping for sh.
(setq org-latex-listings t)
(setq org-latex-listings-langs
      (append '((ores-shell "bash")) org-latex-listings-langs))

;; Cap figures at 60% of the text width but never upscale: \maxwidth
;; (defined in user_manual.org) yields the image's natural width when it
;; is smaller than 0.6\linewidth, and 0.6\linewidth otherwise. Large
;; dialog screenshots shrink to 60%; small crops keep their natural size.
(setq org-latex-image-default-width "\\maxwidth")

;; Small figures go in tufte's wide margin. Org has no `marginfigure'
;; float, so after LaTeX export we promote any figure whose image is small
;; — width <= `ores/margin-max-w' AND height <= `ores/margin-max-h' pixels
;; — to a `marginfigure'. Larger screenshots stay in the text column. This
;; is a LaTeX-export-only transform; the HTML site is unaffected.
;;
;; The repo root is computed from this script's location (3 levels up from
;; projects/ores.lisp/src/). During org export `load-file-name' is nil and
;; `buffer-file-name' may be too, so it is captured at load time.
;; Float-placement options (`[htbp]' etc.) and `\includegraphics' options
;; are optional in the regexps, so begin/end can never mismatch and
;; option-less images are still matched.
(defvar ores/repo-root
  (if (or load-file-name buffer-file-name)
      (expand-file-name "../../../"
                        (file-name-directory (or load-file-name buffer-file-name)))
    default-directory)
  "Repository root, captured when this script is loaded.")

;; Load shared link-type definitions.  ores-link-types.el registers
;; proj:, adds it to org-latex-inline-image-rules, and advises
;; org-latex--inline-image to resolve proj: paths to absolute paths so
;; that org-latex--inline-image handles figures/captions/marginfigures
;; exactly as it would for file: links.
(load-file (expand-file-name
            "ores-link-types.el"
            (file-name-directory (or load-file-name buffer-file-name))))

(defvar ores/margin-max-w 510)
(defvar ores/margin-max-h 320)
(defun ores/png-dimensions (file)
  "Return (WIDTH . HEIGHT) in pixels from PNG FILE's IHDR, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil 0 24)
      (when (>= (buffer-size) 24)
        (let ((b (buffer-string)))
          (cons (logior (ash (aref b 16) 24) (ash (aref b 17) 16)
                        (ash (aref b 18) 8) (aref b 19))
                (logior (ash (aref b 20) 24) (ash (aref b 21) 16)
                        (ash (aref b 22) 8) (aref b 23))))))))
(defun ores/figures-to-margin (text backend _info)
  "Promote small figures to marginfigure in LaTeX output."
  (if (not (org-export-derived-backend-p backend 'latex))
      text
    (replace-regexp-in-string
     "\\\\begin{figure}\\(?:\\[[^]]*\\]\\)?\\(?:.\\|\n\\)*?\\\\end{figure}"
     (lambda (blk)
       ;; Inner regex ops clobber the outer matcher's state; isolate.
       (save-match-data
         ;; An explicit width (anything other than the default \maxwidth)
         ;; is author intent to keep the figure in the text column; only
         ;; default-sized figures are promoted. Checked first so the
         ;; includegraphics match data survives for the let* below.
         (if (and (string-match "width=\\\\maxwidth" blk)
                  (string-match "\\\\includegraphics\\(?:\\[[^]]*\\]\\)?{\\([^}]*\\.png\\)}" blk)
                  (let* ((name (file-name-nondirectory (match-string 1 blk)))
                         (dim (ores/png-dimensions
                               (expand-file-name
                                name (expand-file-name "assets/images" ores/repo-root)))))
                    (and dim (<= (car dim) ores/margin-max-w)
                         (<= (cdr dim) ores/margin-max-h))))
             (replace-regexp-in-string
              "\\\\end{figure}" "\\end{marginfigure}"
              (replace-regexp-in-string
               "\\\\begin{figure}\\(?:\\[[^]]*\\]\\)?" "\\begin{marginfigure}"
               blk nil t)
              nil t)
           blk)))
     text nil t)))
(add-to-list 'org-export-filter-final-output-functions 'ores/figures-to-margin)

;; Custom LaTeX class: * → \chapter, ** → \section (no \part level).
;; This gives a clean chapter-based book without Part I/II dividers.
(add-to-list 'org-latex-classes
             '("ores-manual"
               "\\documentclass[a4paper,11pt,oneside]{book}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

;; Tufte book: elegant wide-margin layout. tufte-book manages its own
;; geometry, headers, fonts and hyperref, so user_manual.org keeps only a
;; minimal preamble when this class is selected. Supports chapter/section/
;; subsection (the manual nests no deeper).
(add-to-list 'org-latex-classes
             '("ores-tufte"
               ;; Pass `section' to placeins before tufte-book loads it
               ;; without options; a later \usepackage[section]{placeins}
               ;; would otherwise trigger an option-clash LaTeX error.
               "\\PassOptionsToPackage{section}{placeins}\n\\documentclass[a4paper,justified]{tufte-book}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")))

;; Read version and git build info to produce a full version string matching
;; the format used by generate_version.cmake / version.cpp.in.
(defun ores/cmake-version ()
  "Return the project VERSION string from CMakeLists.txt, or \"0.0.0\"."
  (let ((cmake (expand-file-name "CMakeLists.txt" ores/repo-root)))
    (with-temp-buffer
      (insert-file-contents cmake)
      (if (re-search-forward
           "project(OreStudio VERSION \\([0-9]+\\.[0-9]+\\.[0-9]+\\)" nil t)
          (match-string 1)
        "0.0.0"))))

(defun ores/git-build-info ()
  "Return a build-info string like \"local abc1234\" or \"local abc1234-dirty\"."
  (let* ((default-directory ores/repo-root)
         (hash (string-trim
                (shell-command-to-string "git rev-parse --short HEAD 2>/dev/null")))
         (status (string-trim
                  (shell-command-to-string "git status --porcelain 2>/dev/null"))))
    (if (string-empty-p hash)
        "local unknown"
      (if (string-empty-p status)
          (format "local %s" hash)
        (format "local %s-dirty" hash)))))

(defun ores/full-version ()
  "Return version string with git build info, e.g. \"0.0.17 (local abc1234-dirty)\"."
  (format "%s (%s)" (ores/cmake-version) (ores/git-build-info)))

;; Custom title page with logo.  Image path is relative to the .tex
;; file location (doc/manual/user_guide/), so three levels up to reach
;; assets/images/.
(setq org-latex-title-command
      (format "\\begin{titlepage}
\\centering
\\vspace*{3cm}
{\\Huge\\bfseries ORE Studio User Manual\\par}
\\vspace{0.5cm}
{\\large Marco Craveiro\\par}
\\vspace{0.3cm}
{\\normalsize Version %s\\par}
\\vspace{2cm}
\\includegraphics[width=0.6\\textwidth]{../../../assets/images/login_screen.png}\\par
\\vfill
\\end{titlepage}" (ores/full-version)))

;; Run LaTeX twice so cross-references and TOC are resolved.
;; Keep logs: org-mode would normally delete them after a successful build.
;; After export we move the log to build/output/manual/ for inspection.
(defvar ores/manual-build-dir
  (expand-file-name "build/output/manual" ores/repo-root)
  "Directory for preserved LaTeX log/artifacts from the manual build.")
(make-directory ores/manual-build-dir t)
(setq org-latex-remove-logfiles nil)
;; Three passes: tufte-book's sidenotes and marginfigures shift label
;; positions enough that two passes can still leave "rerun needed" warnings.
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))

;; Refresh org-id locations over the whole doc tree before exporting, as
;; the site build does. Without this, id links resolve against a stale
;; known-file list and links to newly added chapters abort the export.
(setq org-id-locations-file
      (expand-file-name ".org-id-locations-file" ores/repo-root))
(org-id-update-id-locations
 (directory-files-recursively
  (expand-file-name "doc" ores/repo-root) "\\.org$"))

;; In the PDF, id links to documents OUTSIDE the manual resolve to the
;; published website rather than to local file paths (which would be
;; broken on any reader's machine). Links between manual chapters are
;; left alone — the chapters are #+include'd into one document, so org
;; exports them as internal cross-references. Returning nil from the
;; :export function falls through to org's default handling.
(defvar ores/site-base-url "https://orestudio.github.io/OreStudio/"
  "Base URL of the published site, mirroring the repository layout.")
(defvar ores/manual-dir
  (expand-file-name "doc/manual/user_guide" ores/repo-root)
  "Directory whose documents form the manual itself.")
(defun ores/manual-id-export (id desc backend)
  "Export ID links in the LaTeX backend.

Targets outside the manual become site URLs. File-level ids of manual
chapters become internal references to the chapter heading, whose
label user_manual.org sets via CUSTOM_ID to the chapter file's stem
(requires `org-latex-prefer-user-labels').  Heading-level ids inside
chapters return nil and fall through to org's native internal
resolution."
  (when (org-export-derived-backend-p backend 'latex)
    (let* ((found (org-id-find id))
           (file (car found))
           (pos (cdr found)))
      (when file
        (if (string-prefix-p ores/manual-dir (expand-file-name file))
            ;; File-level property drawers sit within the first few
            ;; hundred characters; heading ids appear much later.
            ;; CUSTOM_ID labels are emitted verbatim by ox-latex (no
            ;; sec: prefix) under org-latex-prefer-user-labels.
            (when (and pos (< pos 200))
              (format "\\hyperref[%s]{%s}"
                      (file-name-base file)
                      (or desc (file-name-base file))))
          (let ((url (concat ores/site-base-url
                             (replace-regexp-in-string
                              "\\.org\\'" ".html"
                              (file-relative-name (expand-file-name file)
                                                  ores/repo-root)))))
            (format "\\href{%s}{%s}" url (or desc url))))))))
(org-link-set-parameters "id" :export #'ores/manual-id-export)
(setq org-latex-prefer-user-labels t)

(let* ((manual-file (expand-file-name
                     "doc/manual/user_guide/user_manual.org"
                     ores/repo-root))
       (manual-dir  (file-name-directory manual-file))
       (log-exts    '("log" "aux" "out" "toc" "lof" "lot")))
  (condition-case err
      (progn
        (find-file manual-file)
        (org-latex-export-to-pdf)
        ;; Move LaTeX intermediate files to build/output/manual/ so they are
        ;; accessible for error inspection without polluting the source tree.
        (dolist (ext log-exts)
          (let ((src (expand-file-name (concat "user_manual." ext) manual-dir))
                (dst (expand-file-name (concat "user_manual." ext)
                                       ores/manual-build-dir)))
            (when (file-exists-p src)
              (rename-file src dst t))))
        (message "Manual PDF build succeeded.")
        (kill-emacs 0))
    (error
     (message "Manual PDF build failed: %s" (error-message-string err))
     (kill-emacs 1))))

(provide 'ores-build-manual)
;;; ores-build-manual.el ends here
