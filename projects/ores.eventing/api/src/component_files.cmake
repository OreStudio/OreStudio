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
    "domain/entity_change_event_json_io.cpp"
    "domain/entity_change_event_table_io.cpp"
    "domain/event_channel_info_json_io.cpp"
    "generators/entity_change_event_generator.cpp"
    "service/event_bus.cpp"
    "service/event_channel_registry.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/domain/entity_change_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/domain/entity_change_event_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/domain/entity_change_event_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/domain/event_channel_info.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/domain/event_channel_info_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/domain/event_traits.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/domain/ores.eventing.domain.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/generators/entity_change_event_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/generators/ores.eventing.generators.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/ores.eventing.api.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/service/event_bus.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/service/event_channel_registry.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.eventing.api/service/ores.eventing.service.hpp"
)
