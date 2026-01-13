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
# Test environment variables: applied to all test targets.
# ORES_TEST_PASSWORD_FAST: Use fast scrypt parameters (16x faster) for tests.
#
set(ORES_TEST_ENV "ORES_TEST_PASSWORD_FAST=1")

#
# Java args for PlantUML
#
set(ORES_PLANTUML_JAR "/usr/share/plantuml/plantuml.jar")
set(ORES_JAVA_ARGS "-Djava.awt.headless=true")
list(APPEND ORES_JAVA_ARGS "-DPLANTUML_SECURITY_PROFILE=UNSECURE")
list(APPEND ORES_JAVA_ARGS "-DPLANTUML_LIMIT_SIZE=131070")

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
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.geo)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.iam)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.cli)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.comms.shell)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.comms)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.comms.analyser)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.comms.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.risk)
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
