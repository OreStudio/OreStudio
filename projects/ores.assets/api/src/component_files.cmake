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
    "domain/image_json_io.cpp"
    "domain/image_table.cpp"
    "domain/image_table_io.cpp"
    "domain/image_tag_json_io.cpp"
    "domain/image_tag_table.cpp"
    "domain/image_tag_table_io.cpp"
    "domain/tag_json_io.cpp"
    "domain/tag_table.cpp"
    "domain/tag_table_io.cpp"
    "generators/image_generator.cpp"
    "generators/image_tag_generator.cpp"
    "generators/tag_generator.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/image.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/image_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/image_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/image_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/image_tag.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/image_tag_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/image_tag_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/image_tag_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/ores.assets.api.domain.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/tag.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/tag_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/tag_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/domain/tag_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/eventing/image_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/eventing/ores.assets.api.eventing.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/eventing/tag_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/generators/image_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/generators/image_tag_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/generators/tag_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/messaging/image_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/messaging/image_tag_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.api/messaging/tag_protocol.hpp"
)
