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
# AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
# Template: cmake_component_files_src.mustache
# To modify, update the template and regenerate.
set(files
    "domain/series_key_shape_json_io.cpp"
    "domain/series_key_shape_table.cpp"
    "domain/series_key_shape_table_io.cpp"
    "generators/series_key_shape_generator.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/series_key_shape.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/series_key_shape_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/series_key_shape_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/series_key_shape_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/eventing/series_key_shape_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/eventing/series_key_shape_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/generators/series_key_shape_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/ore_import_engine_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/ore_import_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/series_key_shape_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/net/ore_storage.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/ores.ore.api.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/workflow/ore_import_workflow.hpp"
)
