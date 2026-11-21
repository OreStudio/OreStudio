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
    "messaging/knowledge_page_history_provider_registrar.cpp"
    "messaging/knowledge_page_registrar.cpp"
    "messaging/publish_from_dq_handler.cpp"
    "messaging/registrar.cpp"
    "presentation/knowledge_page_history_field_mapper.cpp"
    "repository/knowledge_page_entity.cpp"
    "repository/knowledge_page_mapper.cpp"
    "repository/knowledge_page_repository.cpp"
    "service/knowledge_page_service.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/messaging/knowledge_page_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/messaging/knowledge_page_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/messaging/knowledge_page_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/messaging/publish_from_dq_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/ores.orgmode.core.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/presentation/knowledge_page_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/repository/knowledge_page_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/repository/knowledge_page_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/repository/knowledge_page_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.core/service/knowledge_page_service.hpp"
)
