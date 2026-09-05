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
    "messaging/concurrency_policy_history_provider_registrar.cpp"
    "messaging/concurrency_policy_registrar.cpp"
    "messaging/publish_from_dq_handler.cpp"
    "messaging/registrar.cpp"
    "messaging/report_definition_history_provider_registrar.cpp"
    "messaging/report_definition_registrar.cpp"
    "messaging/report_execution_handler.cpp"
    "messaging/report_instance_history_provider_registrar.cpp"
    "messaging/report_instance_registrar.cpp"
    "messaging/report_type_history_provider_registrar.cpp"
    "messaging/report_type_registrar.cpp"
    "presentation/concurrency_policy_history_field_mapper.cpp"
    "presentation/report_definition_history_field_mapper.cpp"
    "presentation/report_instance_history_field_mapper.cpp"
    "presentation/report_type_history_field_mapper.cpp"
    "repository/concurrency_policy_entity.cpp"
    "repository/concurrency_policy_mapper.cpp"
    "repository/concurrency_policy_repository.cpp"
    "repository/report_definition_entity.cpp"
    "repository/report_definition_mapper.cpp"
    "repository/report_definition_repository.cpp"
    "repository/report_input_bundle_repository.cpp"
    "repository/report_instance_entity.cpp"
    "repository/report_instance_mapper.cpp"
    "repository/report_instance_repository.cpp"
    "repository/report_type_entity.cpp"
    "repository/report_type_mapper.cpp"
    "repository/report_type_repository.cpp"
    "repository/risk_report_config_mapper.cpp"
    "repository/risk_report_config_repository.cpp"
    "service/concurrency_policy_service.cpp"
    "service/report_definition_service.cpp"
    "service/report_instance_service.cpp"
    "service/report_scheduling_service.cpp"
    "service/report_type_service.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/concurrency_policy_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/concurrency_policy_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/concurrency_policy_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/publish_from_dq_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_definition_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_definition_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_definition_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_definition_template_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_execution_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_instance_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_instance_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_instance_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_instance_trigger_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_scheduling_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_type_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_type_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/messaging/report_type_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/ores.reporting.core.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/presentation/concurrency_policy_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/presentation/report_definition_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/presentation/report_instance_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/presentation/report_type_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/concurrency_policy_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/concurrency_policy_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/concurrency_policy_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_definition_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_definition_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_definition_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_input_bundle_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_input_bundle_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_instance_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_instance_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_instance_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_type_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_type_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/report_type_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/risk_report_config_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/risk_report_config_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/repository/risk_report_config_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/service/concurrency_policy_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/service/report_definition_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/service/report_instance_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/service/report_scheduling_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.core/service/report_type_service.hpp"
)
