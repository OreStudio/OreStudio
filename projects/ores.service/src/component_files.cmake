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
    "service/request_context.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/error_code.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/messaging/handler_helpers.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/messaging/workflow_helpers.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/ores.service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/service/domain_service_runner.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/service/domain_service_runner_impl.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/service/exit_codes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/service/heartbeat_publisher.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/service/request_context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/service/signing_service_runner.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/service/signing_service_runner_impl.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.service/service/wt_service_runner.hpp"
)
