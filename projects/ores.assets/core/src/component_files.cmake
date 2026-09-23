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
    "messaging/image_history_provider_registrar.cpp"
    "messaging/image_registrar.cpp"
    "messaging/publish_from_dq_handler.cpp"
    "messaging/registrar.cpp"
    "messaging/tag_history_provider_registrar.cpp"
    "messaging/tag_registrar.cpp"
    "presentation/image_history_field_mapper.cpp"
    "presentation/tag_history_field_mapper.cpp"
    "repository/image_entity.cpp"
    "repository/image_mapper.cpp"
    "repository/image_repository.cpp"
    "repository/image_tag_entity.cpp"
    "repository/image_tag_mapper.cpp"
    "repository/image_tag_repository.cpp"
    "repository/tag_entity.cpp"
    "repository/tag_mapper.cpp"
    "repository/tag_repository.cpp"
    "service/image_service.cpp"
    "service/tag_service.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/messaging/image_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/messaging/image_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/messaging/image_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/messaging/publish_from_dq_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/messaging/tag_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/messaging/tag_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/messaging/tag_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/ores.assets.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/presentation/image_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/presentation/tag_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/repository/image_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/repository/image_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/repository/image_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/repository/image_tag_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/repository/image_tag_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/repository/image_tag_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/repository/ores.assets.repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/repository/tag_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/repository/tag_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/repository/tag_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/service/image_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.assets.core/service/tag_service.hpp"
)
