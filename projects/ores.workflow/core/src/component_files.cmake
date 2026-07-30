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
    "domain/workflow_instance_json_io.cpp"
    "domain/workflow_step_json_io.cpp"
    "messaging/registrar.cpp"
    "messaging/workflow_handler.cpp"
    "messaging/workflow_query_handler.cpp"
    "repository/workflow_instance_entity.cpp"
    "repository/workflow_instance_mapper.cpp"
    "repository/workflow_instance_repository.cpp"
    "repository/workflow_step_entity.cpp"
    "repository/workflow_step_mapper.cpp"
    "repository/workflow_step_repository.cpp"
    "service/fsm_state_map.cpp"
    "service/workflow_engine.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/domain/workflow_instance.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/domain/workflow_instance_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/domain/workflow_step.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/domain/workflow_step_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/workflow_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/messaging/workflow_query_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/ores.workflow.core.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_instance_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_instance_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_instance_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_step_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_step_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/repository/workflow_step_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/service/fsm_state_map.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.workflow.core/service/workflow_engine.hpp"
)
