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
    "app/command_args.cpp"
    "app/command_feedback.cpp"
    "app/commands/history_diff_renderer.cpp"
    "app/pagination_context.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/command_args.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/command_feedback.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/command_token.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/history_diff_renderer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/ores.shell.app.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/pagination_context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/request_helpers.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/ores.shell.hpp"
)
