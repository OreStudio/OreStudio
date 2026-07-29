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
    "domain/environment_tag_json_io.cpp"
    "domain/environment_tag_table_io.cpp"
    "domain/folder_json_io.cpp"
    "domain/folder_table_io.cpp"
    "domain/tag_json_io.cpp"
    "domain/tag_table_io.cpp"
    "generators/connection_generator.cpp"
    "generators/environment_generator.cpp"
    "generators/environment_tag_generator.cpp"
    "generators/folder_generator.cpp"
    "generators/tag_generator.cpp"
    "repository/connection_mapper.cpp"
    "repository/connection_repository.cpp"
    "repository/connection_tag_mapper.cpp"
    "repository/connection_tag_repository.cpp"
    "repository/environment_mapper.cpp"
    "repository/environment_repository.cpp"
    "repository/environment_tag_mapper.cpp"
    "repository/environment_tag_repository.cpp"
    "repository/folder_mapper.cpp"
    "repository/folder_repository.cpp"
    "repository/recent_party_mapper.cpp"
    "repository/recent_party_repository.cpp"
    "repository/sqlite_context.cpp"
    "repository/tag_mapper.cpp"
    "repository/tag_repository.cpp"
    "service/connection_manager.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/connection.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/connection_tag.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/environment.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/environment_tag.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/environment_tag_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/environment_tag_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/folder.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/folder_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/folder_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/recent_party.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/tag.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/tag_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/domain/tag_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/generators/connection_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/generators/environment_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/generators/environment_tag_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/generators/folder_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/generators/tag_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/ores.connections.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/connection_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/connection_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/connection_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/connection_tag_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/connection_tag_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/connection_tag_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/environment_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/environment_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/environment_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/environment_tag_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/environment_tag_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/environment_tag_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/folder_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/folder_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/folder_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/recent_party_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/recent_party_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/recent_party_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/sqlite_context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/tag_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/tag_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/repository/tag_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.connections/service/connection_manager.hpp"
)
