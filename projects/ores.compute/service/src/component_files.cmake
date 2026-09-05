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
    "app/batch_workflow_bridge.cpp"
    "app/compute_grid_poller.cpp"
    "app/host.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "main.cpp"
    "messaging/app_event_registrar.cpp"
    "messaging/app_version_event_registrar.cpp"
    "messaging/batch_event_registrar.cpp"
    "messaging/host_event_registrar.cpp"
    "messaging/result_event_registrar.cpp"
    "messaging/workunit_event_registrar.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/app/application.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/app/application_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/app/batch_workflow_bridge.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/app/compute_grid_poller.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/app/host.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/config/options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/config/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/config/parser_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/messaging/app_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/messaging/app_version_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/messaging/batch_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/messaging/host_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/messaging/result_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/messaging/workunit_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.service/ores.compute.service.hpp"
)
