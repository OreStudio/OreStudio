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
set(name "ores.qt.scheduler")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)
set(ORES_QT_SCHEDULER_DIR ${CMAKE_SOURCE_DIR}/projects/ores.qt.scheduler)

# Qt code generation (MOC for Q_OBJECT)
set(CMAKE_AUTOMOC ON)

file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
file(GLOB_RECURSE HEADERS "${ORES_QT_SCHEDULER_DIR}/include/*.hpp")

add_library(${lib_target_name} SHARED ${files} ${HEADERS})

set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name}
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    LIBRARY_OUTPUT_DIRECTORY "${ORES_PLUGIN_OUTPUT_DIRECTORY}"
    INSTALL_RPATH "$ORIGIN/../lib:$ORIGIN"
    BUILD_RPATH "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")

target_include_directories(${lib_target_name} PUBLIC
    ${ORES_QT_SCHEDULER_DIR}/include)
target_include_directories(${lib_target_name} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}/${lib_target_name}_autogen/include"
    "${CMAKE_SOURCE_DIR}/projects/ores.qt.compute/include"
    "${CMAKE_SOURCE_DIR}/projects/ores.qt/include")

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.qt.api.lib
        ores.qt.compute.lib
    PRIVATE
        ores.qt.lib)

install(TARGETS ${lib_target_name}
    LIBRARY DESTINATION bin
    RUNTIME DESTINATION bin)
