# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set(CMAKE_COMPILER_IS_CLANGXX "TRUE")
endif()

if (CMAKE_COMPILER_IS_GNUCXX OR CMAKE_COMPILER_IS_CLANGXX)
    if(WITH_PROFILING)
        if(CMAKE_COMPILER_IS_GNUCXX)
            set(flags "${flags} -fprofile-abs-path")
        endif()
        set(flags "${flags} -fprofile-arcs")
        set(flags "${flags} -ftest-coverage")
    endif()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${flags}")
    message(STATUS "C++ flags: ${CMAKE_CXX_FLAGS}")
endif()

add_compile_options(
    $<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>:-Werror>
    $<$<CXX_COMPILER_ID:MSVC>:/WX>)

#
# Catch2 args: common args, reporters specified per test target.
#
set(ORES_CATCH2_ARGS --durations yes --filenames-as-tags -r compact)

#
# Test logging configuration.
# ORES_TEST_LOG_LEVEL: Set to a log level (trace, debug, info, warn, error) to
#                      enable test logging. Default is OFF (logging disabled).
# ORES_TEST_LOG_CONSOLE: Set to ON to also output logs to console.
#
# Example usage:
#   cmake --preset linux-clang-debug -DORES_TEST_LOG_LEVEL=debug
#   cmake --preset linux-clang-debug -DORES_TEST_LOG_LEVEL=trace -DORES_TEST_LOG_CONSOLE=ON
#
set(ORES_TEST_LOG_LEVEL "OFF" CACHE STRING "Test log level (OFF, trace, debug, info, warn, error)")
set_property(CACHE ORES_TEST_LOG_LEVEL PROPERTY STRINGS OFF trace debug info warn error)
option(ORES_TEST_LOG_CONSOLE "Enable console output for test logs" OFF)

#
# Test environment variables: applied to all test targets.
# ORES_TEST_PASSWORD_FAST: Use fast scrypt parameters (16x faster) for tests.
#
set(ORES_TEST_ENV "ORES_TEST_PASSWORD_FAST=1")

# Add logging environment variables if logging is enabled
if(NOT ORES_TEST_LOG_LEVEL STREQUAL "OFF")
    string(APPEND ORES_TEST_ENV ";ORES_TEST_LOG_ENABLED=true")
    string(APPEND ORES_TEST_ENV ";ORES_TEST_LOG_LEVEL=${ORES_TEST_LOG_LEVEL}")
    if(ORES_TEST_LOG_CONSOLE)
        string(APPEND ORES_TEST_ENV ";ORES_TEST_LOG_CONSOLE=true")
    endif()
    message(STATUS "Test logging enabled: level=${ORES_TEST_LOG_LEVEL}, console=${ORES_TEST_LOG_CONSOLE}")
endif()

#
# Projects
#
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.utility)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.platform)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.logging)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.database)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.testing)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.telemetry)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.variability)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.security)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.geo)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.iam)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.cli)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.comms.shell)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.comms)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.comms.analyser)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.comms.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.refdata)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.assets)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.qt)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.eventing)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.connections)
if(WITH_WT)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.wt)
endif()
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.http)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.http.server)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/modeling)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.sql/modeling)
