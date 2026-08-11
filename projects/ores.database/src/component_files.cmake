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
    "config/database_configuration.cpp"
    "domain/database_info_json_io.cpp"
    "domain/database_options.cpp"
    "repository/bitemporal_operations.cpp"
    "repository/database_info_entity.cpp"
    "repository/database_info_mapper.cpp"
    "repository/database_info_repository.cpp"
    "service/context_factory.cpp"
    "service/health_monitor.cpp"
    "service/party_context.cpp"
    "service/postgres_listener_service.cpp"
    "service/tenant_context.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/config/database_configuration.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/config/ores.database.config.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/domain/context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/domain/database_info.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/domain/database_info_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/domain/database_options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/domain/exceptions.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/domain/ores.database.domain.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/domain/session_utilities.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/domain/tenant_aware_pool.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/ores.database.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/repository/bitemporal_operations.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/repository/database_info_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/repository/database_info_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/repository/database_info_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/repository/db_types.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/repository/helpers.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/repository/mapper_helpers.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/repository/ores.database.repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/repository/repository_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/repository/version_conflict_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/service/context_factory.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/service/health_monitor.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/service/ores.database.service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/service/party_context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/service/postgres_listener_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.database/service/tenant_context.hpp"
)
