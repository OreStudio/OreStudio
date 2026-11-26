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
    "messaging/consumer_history_provider_registrar.cpp"
    "messaging/consumer_registrar.cpp"
    "messaging/secret_history_provider_registrar.cpp"
    "messaging/secret_registrar.cpp"
    "messaging/secret_use_registrar.cpp"
    "presentation/consumer_history_field_mapper.cpp"
    "presentation/secret_history_field_mapper.cpp"
    "repository/consumer_entity.cpp"
    "repository/consumer_mapper.cpp"
    "repository/consumer_repository.cpp"
    "repository/secret_entity.cpp"
    "repository/secret_mapper.cpp"
    "repository/secret_repository.cpp"
    "repository/secret_use_entity.cpp"
    "repository/secret_use_mapper.cpp"
    "repository/secret_use_repository.cpp"
    "security/credential_source.cpp"
    "service/consumer_service.cpp"
    "service/secret_service.cpp"
    "service/secret_use_service.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/messaging/consumer_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/messaging/consumer_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/messaging/consumer_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/messaging/secret_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/messaging/secret_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/messaging/secret_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/messaging/secret_use_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/messaging/secret_use_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/ores.vault.core.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/presentation/consumer_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/presentation/secret_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/repository/consumer_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/repository/consumer_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/repository/consumer_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/repository/secret_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/repository/secret_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/repository/secret_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/repository/secret_use_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/repository/secret_use_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/repository/secret_use_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/security/credential_source.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/service/consumer_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/service/secret_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.vault.core/service/secret_use_service.hpp"
)
