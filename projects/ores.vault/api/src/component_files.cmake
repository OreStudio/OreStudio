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
    "domain/consumer_codec.cpp"
    "domain/consumer_json_io.cpp"
    "domain/consumer_table.cpp"
    "domain/consumer_table_io.cpp"
    "domain/secret_codec.cpp"
    "domain/secret_json_io.cpp"
    "domain/secret_table.cpp"
    "domain/secret_table_io.cpp"
    "generators/consumer_generator.cpp"
    "generators/secret_generator.cpp"
    "messaging/consumer_protocol_codec.cpp"
    "messaging/secret_protocol_codec.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/domain/consumer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/domain/consumer_codec.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/domain/consumer_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/domain/consumer_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/domain/consumer_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/domain/secret.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/domain/secret_codec.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/domain/secret_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/domain/secret_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/domain/secret_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/eventing/consumer_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/eventing/secret_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/generators/consumer_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/generators/secret_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/messaging/consumer_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/messaging/secret_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.api/ores.vault.api.hpp"
)
