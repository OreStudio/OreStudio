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
    "messaging/job_definition_history_provider_registrar.cpp"
    "messaging/job_definition_registrar.cpp"
    "messaging/publish_from_dq_handler.cpp"
    "messaging/registrar.cpp"
    "messaging/retained_store_history_provider_registrar.cpp"
    "messaging/retained_store_registrar.cpp"
    "messaging/retained_store_sample_registrar.cpp"
    "messaging/retention_policy_history_provider_registrar.cpp"
    "messaging/retention_policy_registrar.cpp"
    "presentation/job_definition_history_field_mapper.cpp"
    "presentation/retained_store_history_field_mapper.cpp"
    "presentation/retention_policy_history_field_mapper.cpp"
    "repository/job_definition_entity.cpp"
    "repository/job_definition_mapper.cpp"
    "repository/job_definition_repository.cpp"
    "repository/job_instance_mapper.cpp"
    "repository/job_instance_repository.cpp"
    "repository/retained_store_entity.cpp"
    "repository/retained_store_mapper.cpp"
    "repository/retained_store_repository.cpp"
    "repository/retained_store_sample_entity.cpp"
    "repository/retained_store_sample_mapper.cpp"
    "repository/retained_store_sample_repository.cpp"
    "repository/retention_policy_entity.cpp"
    "repository/retention_policy_mapper.cpp"
    "repository/retention_policy_repository.cpp"
    "service/job_definition_service.cpp"
    "service/mq_action_handler.cpp"
    "service/nats_publish_action_handler.cpp"
    "service/nats_request_action_handler.cpp"
    "service/publish_subject_plan.cpp"
    "service/retained_store_sample_service.cpp"
    "service/retained_store_service.cpp"
    "service/retention_policy_service.cpp"
    "service/schedule_decision.cpp"
    "service/scheduler_loop.cpp"
    "service/sql_action_handler.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/job_definition_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/job_definition_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/job_definition_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/job_instance_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/publish_from_dq_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/retained_store_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/retained_store_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/retained_store_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/retained_store_sample_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/retained_store_sample_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/retention_policy_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/retention_policy_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/retention_policy_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/messaging/scheduler_status_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/ores.scheduler.core.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/ores.scheduler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/presentation/job_definition_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/presentation/retained_store_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/presentation/retention_policy_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_definition_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_definition_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_definition_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_instance_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_instance_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/job_instance_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/retained_store_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/retained_store_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/retained_store_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/retained_store_sample_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/retained_store_sample_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/retained_store_sample_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/retention_policy_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/retention_policy_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/repository/retention_policy_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/action_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/action_payloads.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/job_definition_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/mq_action_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/nats_publish_action_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/nats_request_action_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/publish_subject_plan.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/retained_store_sample_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/retained_store_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/retention_policy_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/schedule_decision.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/scheduler_loop.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.core/service/sql_action_handler.hpp"
)
