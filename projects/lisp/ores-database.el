;;; ores-database.el --- Database connectivity  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Marco Craveiro

;; Author: Marco Craveiro <marco.craveiro@gmail.com>
;; Keywords: local

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

;;
;;; Code:
(require 'sql)

;; Remove all default login parameters
(setq sql-postgres-login-params nil)

;; define your connections
(add-to-list 'sql-connection-alist
      '("ores-primary-db" (sql-product 'postgres)
        (sql-database (concat "postgresql://"
                              "ores"
                              ":" (auth-source-pick-first-password :host "localhost")
                              "@localhost"
                              ":5434"
                              "/oresdb"
                              ))))
(provide 'ores-database)
;;; ores-database.el ends here
