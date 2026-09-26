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
    "messaging/registrar.cpp"
    "messaging/workflow_handler.cpp"
    "messaging/workflow_instance_history_provider_registrar.cpp"
    "messaging/workflow_instance_registrar.cpp"
    "messaging/workflow_query_handler.cpp"
    "messaging/workflow_step_history_provider_registrar.cpp"
    "messaging/workflow_step_registrar.cpp"
    "presentation/workflow_instance_history_field_mapper.cpp"
    "presentation/workflow_step_history_field_mapper.cpp"
    "repository/workflow_instance_entity.cpp"
    "repository/workflow_instance_mapper.cpp"
    "repository/workflow_instance_repository.cpp"
    "repository/workflow_step_entity.cpp"
    "repository/workflow_step_mapper.cpp"
    "repository/workflow_step_repository.cpp"
    "service/fsm_state_map.cpp"
    "service/workflow_engine.cpp"
    "service/workflow_instance_service.cpp"
    "service/workflow_step_service.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/workflow_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/workflow_instance_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/workflow_instance_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/workflow_instance_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/workflow_query_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/workflow_step_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/workflow_step_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/workflow_step_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/ores.workflow.core.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/presentation/workflow_instance_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/presentation/workflow_step_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_instance_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_instance_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_instance_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_step_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_step_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_step_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/service/fsm_state_map.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/service/workflow_engine.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/service/workflow_instance_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/service/workflow_step_service.hpp"
)
