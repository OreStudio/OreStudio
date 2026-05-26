;;; .build-manual.el --- Build user manual PDF. -*- lexical-binding: t; -*-
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
(require 'ox-latex)

(setq debug-on-error nil)
(setq debug-on-quit nil)

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
(defvar ores/margin-max-w 420)
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
    (let ((root (file-name-directory (or load-file-name buffer-file-name))))
      (replace-regexp-in-string
       "\\\\begin{figure}\\[[^]]*\\]\\(?:.\\|\n\\)*?\\\\end{figure}"
       (lambda (blk)
         ;; Inner regex ops would clobber the outer matcher's state; isolate.
         (save-match-data
           (if (and (string-match "\\\\includegraphics\\[[^]]*\\]{\\([^}]*\\.png\\)}" blk)
                    (let* ((name (file-name-nondirectory (match-string 1 blk)))
                           (dim (ores/png-dimensions
                                 (expand-file-name name (expand-file-name "assets/images" root)))))
                      (and dim (<= (car dim) ores/margin-max-w)
                           (<= (cdr dim) ores/margin-max-h))))
               (thread-last blk
                 (string-replace "\\begin{figure}[htbp]" "\\begin{marginfigure}")
                 (string-replace "\\end{figure}" "\\end{marginfigure}"))
             blk)))
       text nil t))))
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
               "\\documentclass[a4paper,justified]{tufte-book}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")))

;; Read version from CMakeLists.txt so the cover page stays in sync
;; with the single source of truth.
(defun ores/cmake-version ()
  "Return the project VERSION string from CMakeLists.txt, or \"0.0.0\"."
  (let ((cmake (expand-file-name "CMakeLists.txt"
                (file-name-directory (or load-file-name buffer-file-name)))))
    (with-temp-buffer
      (insert-file-contents cmake)
      (if (re-search-forward
           "project(OreStudio VERSION \\([0-9]+\\.[0-9]+\\.[0-9]+\\)" nil t)
          (match-string 1)
        "0.0.0"))))

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
\\end{titlepage}" (ores/cmake-version)))

;; Run LaTeX twice so cross-references and TOC are resolved.
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))

(let ((manual-file (expand-file-name
                    "doc/manual/user_guide/user_manual.org"
                    (file-name-directory (or load-file-name buffer-file-name)))))
  (condition-case err
      (progn
        (find-file manual-file)
        (org-latex-export-to-pdf)
        (message "Manual PDF build succeeded.")
        (kill-emacs 0))
    (error
     (message "Manual PDF build failed: %s" (error-message-string err))
     (kill-emacs 1))))

(provide '.build-manual)
;;; .build-manual.el ends here
