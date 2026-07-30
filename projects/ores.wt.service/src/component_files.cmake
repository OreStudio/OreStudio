# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
set(files
    "app/account_dialog.cpp"
    "app/account_list_widget.cpp"
    "app/country_dialog.cpp"
    "app/country_list_widget.cpp"
    "app/currency_dialog.cpp"
    "app/currency_list_widget.cpp"
    "app/login_widget.cpp"
    "app/ore_application.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "main.cpp"
    "messaging/registrar.cpp"
    "service/application_context.cpp"
    "service/session_manager.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/app/account_dialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/app/account_list_widget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/app/country_dialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/app/country_list_widget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/app/currency_dialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/app/currency_list_widget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/app/login_widget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/app/ore_application.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/config/options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/config/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/config/parser_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/service/application_context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.wt.service/service/session_manager.hpp"
)
