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
    "app/commands/compute/app_commands.cpp"
    "app/commands/compute/app_version_commands.cpp"
    "app/commands/compute/app_version_platform_commands.cpp"
    "app/commands/compute/batch_commands.cpp"
    "app/commands/compute/host_commands.cpp"
    "app/commands/compute/result_commands.cpp"
    "app/commands/compute/workunit_commands.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/compute/app_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/compute/app_version_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/compute/app_version_platform_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/compute/batch_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/compute/host_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/compute/result_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/compute/workunit_commands.hpp"
)
