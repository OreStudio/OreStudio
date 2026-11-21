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
    "domain/knowledge_page_codec.cpp"
    "domain/knowledge_page_json_io.cpp"
    "domain/knowledge_page_table.cpp"
    "domain/knowledge_page_table_io.cpp"
    "generators/knowledge_page_generator.cpp"
    "messaging/knowledge_page_protocol_codec.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.api/domain/knowledge_page.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.api/domain/knowledge_page_codec.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.api/domain/knowledge_page_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.api/domain/knowledge_page_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.api/domain/knowledge_page_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.api/eventing/knowledge_page_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.api/generators/knowledge_page_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.api/messaging/knowledge_page_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.api/ores.orgmode.api.hpp"
)
