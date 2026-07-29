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
    "domain/service_definition_json_io.cpp"
    "domain/service_event_json_io.cpp"
    "domain/service_instance_json_io.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/domain/service_definition.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/domain/service_definition_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/domain/service_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/domain/service_event_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/domain/service_instance.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/domain/service_instance_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/messaging/service_definition_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/messaging/service_event_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/messaging/service_instance_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.api/ores.controller.api.hpp"
)
