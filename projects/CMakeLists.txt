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
    set(flags "")
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

# -Werror without -Wall only escalates the handful of diagnostics on by
# default, which is how a non-exhaustive switch over an enum class
# compiled clean here for years: -Wswitch arrives with -Wall.
#
# Two of -Wextra's warnings are switched back off. Both fire on
# aggregate initialisation that leaves a trailing field to its default
# value — reply(nats_, msg, get_images_response{false, e.what()}) and
# the like — which is deliberate here and not a defect. They accounted
# for 2040 of the 2153 warnings the first full sweep found, against 113
# that named something real, so leaving them on would have buried the
# signal rather than surfaced it.
#
# What this gives up, stated plainly because it is what a future
# missing-field bug will turn on: adding a field to an aggregate no
# longer errors at the call sites that do not set it. They keep
# compiling and the new field is value-initialised. If a field added to
# a struct is silently default somewhere it should not be, this is the
# warning that would have caught it.
#
# The designated-initializer variant is Clang's alone, hence the
# separate guard.
add_compile_options(
    $<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>:-Wall>
    $<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>:-Wextra>
    $<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>:-Wno-missing-field-initializers>
    $<$<CXX_COMPILER_ID:Clang>:-Wno-missing-designated-field-initializers>
    $<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>:-Werror>
    $<$<CXX_COMPILER_ID:MSVC>:/W4>
    $<$<CXX_COMPILER_ID:MSVC>:/WX>
    # C4251 fires on every exported class that carries a std::string or a
    # container, which is most of them. Every consumer is an in-tree target
    # built by this same toolchain against these same headers, so the ABI
    # mismatch the warning exists to catch cannot arise here.
    $<$<CXX_COMPILER_ID:MSVC>:/wd4251>
    # /FS forces synchronous PDB writes so sccache can cache /Zi compilations.
    # Without it, parallel cl.exe invocations race on the shared .pdb file.
    $<$<CXX_COMPILER_ID:MSVC>:/FS>)

#
# Catch2 args: common args, reporters specified per test target.
#
set(ORES_CATCH2_ARGS --allow-running-no-tests --durations yes --filenames-as-tags -r compact)

#
# Projects
#
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.utility)

# Interface library for RFL precompiled headers. Link PRIVATE to any target
# that includes <rfl.hpp> or <rfl/json.hpp>. PCH is omitted when
# WITH_PROFILING is set: sccache expects a .gcno alongside every compiled
# output, but the PCH step does not produce one.
add_library(ores.rfl.pch INTERFACE)
if(NOT WITH_PROFILING)
    target_precompile_headers(ores.rfl.pch INTERFACE
        <rfl.hpp>
        <rfl/json.hpp>
        <ores.utility/rfl/reflectors.hpp>)
endif()

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.platform)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.logging)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.diff)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.history)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.orgmode)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.database)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.testing)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.telemetry)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.scheduler)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.nats)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.variability)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.eventing)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.dq)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.synthetic)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.security)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.service)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.geo)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.iam)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.cli)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.shell)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.refdata)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.workspace)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.trading)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.assets)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.qt)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.connections)
if(WITH_WT)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.wt.service)
endif()
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.storage)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.http)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.fpml)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.ore)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.reporting)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.compute)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.workflow)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.marketdata)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.analytics)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/modeling)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.sql)
