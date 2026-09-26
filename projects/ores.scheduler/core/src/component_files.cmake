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
    "builder/job_definition_builder.cpp"
    "messaging/registrar.cpp"
    "repository/job_definition_entity.cpp"
    "repository/job_definition_mapper.cpp"
    "repository/job_definition_repository.cpp"
    "repository/job_instance_mapper.cpp"
    "repository/job_instance_repository.cpp"
    "service/cron_scheduler.cpp"
    "service/job_definition_service.cpp"
    "service/mq_action_handler.cpp"
    "service/nats_publish_action_handler.cpp"
    "service/scheduler_loop.cpp"
    "service/sql_action_handler.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/builder/job_definition_builder.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/job_definition_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/job_instance_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/scheduler_status_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/ores.scheduler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_definition_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_definition_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_definition_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_instance_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_instance_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_instance_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/action_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/cron_scheduler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/job_definition_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/mq_action_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/nats_publish_action_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/scheduler_loop.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/sql_action_handler.hpp"
)
