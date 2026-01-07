# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
set(name "ores.telemetry")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files "")
file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

# Exclude messaging handler, registrar, and repository files - they require
# ores.comms.lib or ores.database.lib which would create circular dependencies.
# These files are built in ores.comms.service instead.
list(FILTER files EXCLUDE REGEX "messaging/telemetry_message_handler.cpp")
list(FILTER files EXCLUDE REGEX "messaging/registrar.cpp")
list(FILTER files EXCLUDE REGEX "repository/telemetry_repository.cpp")
list(FILTER files EXCLUDE REGEX "repository/telemetry_mapper.cpp")
list(FILTER files EXCLUDE REGEX "repository/telemetry_entity.cpp")

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.telemetry/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.logging.lib
        ores.utility.lib
        # NOTE: ores.database.lib dependency removed to break circular dependency.
        # Repository files are compiled in ores.comms.service instead.
    INTERFACE
        # reflectcpp is provided by ores.platform.lib at build time; expose to consumers here
        reflectcpp::reflectcpp
    PRIVATE
        ores.platform.lib
        Boost::boost)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
