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
    "environment/environment.cpp"
    "environment/real_environment_provider.cpp"
    "filesystem/file.cpp"
    "net/network_info.cpp"
    "process/executable.cpp"
    "process/pid.cpp"
    "time/datetime.cpp"
    "time/relative_time_formatter.cpp"
    "time/time_utils.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/attributes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/environment/environment.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/environment/environment_provider.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/environment/fake_environment_provider.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/environment/ores.platform.environment.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/environment/real_environment_provider.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/filesystem/file.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/filesystem/file_not_found.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/filesystem/filesystem.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/filesystem/io_error.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/filesystem/ores.platform.filesystem.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/net/network_info.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/net/ores.platform.net.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/ores.platform.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/process/executable.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/process/pid.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/process/signals.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/time/datetime.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/time/ores.platform.time.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/time/relative_time_formatter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/time/time_utils.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.platform/unreachable.hpp"
)
