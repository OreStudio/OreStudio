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
    "app/application.cpp"
    "app/host.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "main.cpp"
    "messaging/ore_import_execute_handler.cpp"
    "messaging/ore_import_handler.cpp"
    "messaging/registrar.cpp"
    "messaging/report_package_handler.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/app/application.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/app/application_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/app/host.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/config/options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/config/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/config/parser_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/messaging/ore_import_execute_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/messaging/ore_import_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/messaging/report_package_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.service/ores.ore.service.hpp"
)
