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
    "messaging/registrar.cpp"
    "repository/service_definition_entity.cpp"
    "repository/service_definition_mapper.cpp"
    "repository/service_definition_repository.cpp"
    "repository/service_dependency_repository.cpp"
    "repository/service_event_entity.cpp"
    "repository/service_event_mapper.cpp"
    "repository/service_event_repository.cpp"
    "repository/service_instance_entity.cpp"
    "repository/service_instance_mapper.cpp"
    "repository/service_instance_repository.cpp"
    "service/process_supervisor.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/messaging/service_definition_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/messaging/service_event_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/messaging/service_instance_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/ores.controller.core.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_definition_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_definition_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_definition_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_dependency_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_dependency_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_event_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_event_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_event_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_instance_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_instance_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/repository/service_instance_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.controller.core/service/process_supervisor.hpp"
)
