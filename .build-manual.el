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

;; Custom title page with logo.  Image path is relative to the .tex
;; file location (doc/manual/user_guide/), so three levels up to reach
;; assets/images/.
(setq org-latex-title-command
      "\\begin{titlepage}
\\centering
\\vspace*{3cm}
\\includegraphics[width=0.25\\textwidth]{../../../assets/images/modern-icon.png}\\par
\\vspace{2cm}
{\\Huge\\bfseries ORE Studio User Manual\\par}
\\vspace{0.5cm}
{\\large Marco Craveiro\\par}
\\vfill
\\end{titlepage}")

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
