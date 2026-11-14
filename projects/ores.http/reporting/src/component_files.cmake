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
    "routes/reporting/choice_routes.cpp"
    "routes/reporting/configuration_routes.cpp"
    "routes/reporting/cut_kind_routes.cpp"
    "routes/reporting/cut_kind_rows.cpp"
    "routes/reporting/cut_routes.cpp"
    "routes/reporting/report_routes.cpp"
    "routes/reporting/reporting_routes.cpp"
    "routes/reporting/result_rollup.cpp"
    "routes/reporting/result_routes.cpp"
    "routes/reporting/run_file_routes.cpp"
    "routes/reporting/run_lookup.cpp"
    "routes/reporting/run_routes.cpp"
    "routes/reporting/schedule_routes.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/ores.http.reporting.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/choice_routes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/configuration_routes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/cut_kind_routes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/cut_kind_rows.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/cut_routes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/report_health.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/report_routes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/reporting_routes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/result_rollup.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/result_routes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/run_file_routes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/run_lookup.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/run_routes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http/routes/reporting/schedule_routes.hpp"
)
