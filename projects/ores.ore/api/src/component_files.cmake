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
    "domain/ore_file_category_codec.cpp"
    "domain/ore_file_category_json_io.cpp"
    "domain/ore_file_category_table.cpp"
    "domain/ore_file_category_table_io.cpp"
    "domain/ore_file_codec.cpp"
    "domain/ore_file_json_io.cpp"
    "domain/ore_file_table.cpp"
    "domain/ore_file_table_io.cpp"
    "domain/ore_file_version_codec.cpp"
    "domain/ore_file_version_json_io.cpp"
    "domain/ore_file_version_table.cpp"
    "domain/ore_file_version_table_io.cpp"
    "generators/ore_file_category_generator.cpp"
    "generators/ore_file_generator.cpp"
    "generators/ore_file_version_generator.cpp"
    "messaging/currency_config_protocol_codec.cpp"
    "messaging/ore_document_protocol_codec.cpp"
    "messaging/ore_file_category_protocol_codec.cpp"
    "messaging/ore_file_protocol_codec.cpp"
    "messaging/ore_file_version_protocol_codec.cpp"
    "messaging/ore_import_protocol_codec.cpp"
    "messaging/pricing_engines_protocol_codec.cpp"
    "messaging/run_configuration_protocol_codec.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_category.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_category_codec.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_category_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_category_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_category_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_codec.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_version.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_version_codec.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_version_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_version_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/domain/ore_file_version_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/eventing/ore_file_category_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/eventing/ore_file_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/eventing/ore_file_version_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/generators/ore_file_category_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/generators/ore_file_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/generators/ore_file_version_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/currency_config_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/ore_document_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/ore_file_category_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/ore_file_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/ore_file_version_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/ore_import_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/pricing_engines_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/messaging/run_configuration_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/net/ore_storage.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/ores.ore.api.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/workflow/ore_import_workflow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.ore.api/workflow/run_configuration_import_workflow.hpp"
)
