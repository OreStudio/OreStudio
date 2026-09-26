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
    "domain/cron_expression.cpp"
    "domain/job_definition_json_io.cpp"
    "domain/job_definition_table.cpp"
    "domain/job_definition_table_io.cpp"
    "generators/job_definition_generator.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/domain/cron_expression.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/domain/job_definition.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/domain/job_definition_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/domain/job_definition_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/domain/job_definition_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/domain/job_instance.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/domain/job_status.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/generators/job_definition_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/messaging/scheduler_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.scheduler.api/rfl/reflectors.hpp"
)
