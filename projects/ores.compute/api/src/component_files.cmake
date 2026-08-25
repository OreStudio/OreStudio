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
    "domain/app_json_io.cpp"
    "domain/app_table.cpp"
    "domain/app_table_io.cpp"
    "domain/app_version_json_io.cpp"
    "domain/app_version_platform_json_io.cpp"
    "domain/app_version_platform_table.cpp"
    "domain/app_version_platform_table_io.cpp"
    "domain/app_version_table.cpp"
    "domain/app_version_table_io.cpp"
    "domain/batch_json_io.cpp"
    "domain/batch_table.cpp"
    "domain/batch_table_io.cpp"
    "domain/host_json_io.cpp"
    "domain/host_table.cpp"
    "domain/host_table_io.cpp"
    "domain/result_json_io.cpp"
    "domain/result_table.cpp"
    "domain/result_table_io.cpp"
    "domain/workunit_json_io.cpp"
    "domain/workunit_table.cpp"
    "domain/workunit_table_io.cpp"
    "generators/app_generator.cpp"
    "generators/app_version_generator.cpp"
    "generators/app_version_platform_generator.cpp"
    "generators/batch_generator.cpp"
    "generators/host_generator.cpp"
    "generators/result_generator.cpp"
    "generators/workunit_generator.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_version.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_version_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_version_platform.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_version_platform_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_version_platform_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_version_platform_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_version_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/app_version_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/batch.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/batch_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/batch_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/batch_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/compute_platform.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/grid_sample.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/host.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/host_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/host_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/host_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/node_sample.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/result.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/result_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/result_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/result_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/workunit.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/workunit_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/workunit_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/domain/workunit_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/eventing/app_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/eventing/app_version_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/eventing/batch_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/eventing/host_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/eventing/result_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/eventing/workunit_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/generators/app_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/generators/app_version_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/generators/app_version_platform_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/generators/batch_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/generators/host_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/generators/result_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/generators/workunit_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/messaging/app_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/messaging/app_version_platform_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/messaging/app_version_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/messaging/batch_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/messaging/host_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/messaging/platform_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/messaging/result_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/messaging/telemetry_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/messaging/work_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/messaging/workunit_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.compute.api/net/compute_storage.hpp"
)
