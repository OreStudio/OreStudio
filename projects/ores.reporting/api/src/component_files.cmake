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
    "domain/concurrency_policy_json_io.cpp"
    "domain/concurrency_policy_table.cpp"
    "domain/concurrency_policy_table_io.cpp"
    "domain/report_definition_json_io.cpp"
    "domain/report_definition_table.cpp"
    "domain/report_definition_table_io.cpp"
    "domain/report_definition_template_json_io.cpp"
    "domain/report_instance_json_io.cpp"
    "domain/report_instance_table.cpp"
    "domain/report_instance_table_io.cpp"
    "domain/report_type_json_io.cpp"
    "domain/report_type_table.cpp"
    "domain/report_type_table_io.cpp"
    "generators/concurrency_policy_generator.cpp"
    "generators/report_definition_generator.cpp"
    "generators/report_instance_generator.cpp"
    "generators/report_type_generator.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/concurrency_policy.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/concurrency_policy_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/concurrency_policy_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/concurrency_policy_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_definition.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_definition_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_definition_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_definition_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_definition_template.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_definition_template_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_instance.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_instance_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_instance_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_instance_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_type.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_type_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_type_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/report_type_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/domain/risk_report_config.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/eventing/concurrency_policy_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/eventing/report_definition_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/eventing/report_instance_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/eventing/report_type_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/generators/concurrency_policy_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/generators/report_definition_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/generators/report_instance_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/generators/report_type_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/messaging/concurrency_policy_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/messaging/report_definition_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/messaging/report_execution_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/messaging/report_instance_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/messaging/report_scheduling_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/messaging/report_type_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.reporting.api/workflow/report_execution_workflow.hpp"
)
