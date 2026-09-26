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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#

set(name "ores.shell.compute")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)
set(ORES_SHELL_COMPUTE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)

include(${CMAKE_CURRENT_SOURCE_DIR}/component_files.cmake)

add_library(${lib_target_name} STATIC ${files} ${HEADERS})
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})

target_include_directories(${lib_target_name} PUBLIC
    ${ORES_SHELL_COMPUTE_DIR}/include)

find_package(cli CONFIG REQUIRED)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.shell.api.lib
        ores.nats.lib
        ores.logging.lib
    PRIVATE
        ores.compute.api.lib
        ores.platform.lib
        ores.utility.lib
        cli::cli)

# ores.shell is an application-internal component: its parts are linked into
# ores.shell.exe and are not useful as redistributable artifacts. Keep static, so the two parts share no import/export macro.
if(NOT WIN32)
    install(TARGETS ${lib_target_name} ARCHIVE DESTINATION lib)
endif()
