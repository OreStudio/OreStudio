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
# Projects
#
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.utility)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.platform)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.logging)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.database)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.testing)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.telemetry)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.telemetry.database)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.scheduler.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.scheduler.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.nats)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.variability.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.variability.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.eventing)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.dq)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.dq.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.dq.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.synthetic.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.synthetic.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.security)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.geo)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.iam.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.iam.client)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.iam.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.cli)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.shell)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.refdata.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.refdata.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.refdata.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.trading.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.trading.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.trading.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.assets.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.assets.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.assets.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.iam.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.dq.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.variability.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.telemetry.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.scheduler.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.synthetic.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.qt)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.connections)
if(WITH_WT)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.wt.service)
endif()
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.http.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.http.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.http.server)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.fpml)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.ore)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.reporting.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.reporting.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.reporting.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.compute.api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.compute.core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.compute.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.compute.wrapper)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/modeling)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.sql/modeling)
