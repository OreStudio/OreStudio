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
set(name "ores.comms")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files "")
file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.comms/include
    # Header-only dependency on ores.iam for session domain type.
    # We cannot link against ores.iam due to circular dependency chain
    # (ores.comms -> ores.iam -> ores.variability -> ores.comms).
    ${CMAKE_SOURCE_DIR}/projects/ores.iam/include
    # Required for session_recorder.hpp which uses ores.telemetry/log
    ${CMAKE_SOURCE_DIR}/projects/ores.telemetry/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.utility.lib
        ores.eventing.lib
        OpenSSL::SSL
    PRIVATE
        Boost::iostreams)
# Note: OpenSSL::SSL is PUBLIC because public headers (net/client.hpp,
# net/server.hpp, net/connection.hpp) include <boost/asio/ssl.hpp> which
# contains inline/template code that calls OpenSSL functions.
# Note: reflectcpp comes transitively through ores.eventing.lib
# -> ores.database.lib (reflectcpp is linked)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
