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
# Called by add_custom_target on every build to stamp version.cpp with the
# current git hash and wall-clock timestamp. Uses copy_if_different so that
# the file's mtime (and therefore recompilation) is stable when nothing has
# changed. Only version.cpp itself is recompiled per build; all other TUs
# that include the stable version.hpp are unaffected.
#
# Required input variables (passed with -D):
#   SOURCE_DIR                - project root (for git commands)
#   OreStudio_VERSION_MAJOR   - major version number
#   OreStudio_VERSION_MINOR   - minor version number
#   OreStudio_VERSION_PATCH   - patch version number
#   INPUT_FILE                - path to version.cpp.in
#   OUTPUT_FILE               - path to generated version.cpp

cmake_minimum_required(VERSION 3.21)

# CI builds set all four env vars; use them verbatim so the build record is
# tied to the CI job, not the local clock.
if(DEFINED ENV{ORES_BUILD_PROVIDER} AND
   DEFINED ENV{ORES_BUILD_COMMIT}   AND
   DEFINED ENV{ORES_BUILD_NUMBER}   AND
   DEFINED ENV{ORES_BUILD_TIMESTAMP})

    set(ORES_BUILD_INFO "Build: Provider = '$ENV{ORES_BUILD_PROVIDER}'")
    set(ORES_BUILD_INFO "${ORES_BUILD_INFO} Number = '$ENV{ORES_BUILD_NUMBER}'")
    set(ORES_BUILD_INFO "${ORES_BUILD_INFO} Commit = '$ENV{ORES_BUILD_COMMIT}'")
    set(ORES_BUILD_INFO "${ORES_BUILD_INFO} Timestamp = '$ENV{ORES_BUILD_TIMESTAMP}'")

else()
    # Local build: embed git commit hash and dirty flag only. The wall-clock
    # build time is read at runtime from the executable's own mtime so there
    # is no reason to stamp it here. This keeps the generated content stable
    # between commits, meaning copy_if_different below will skip the write
    # (and therefore skip recompilation) on every build where git state has
    # not changed.
    find_program(GIT_EXECUTABLE git)
    if(GIT_EXECUTABLE)
        execute_process(
            COMMAND "${GIT_EXECUTABLE}" rev-parse --short HEAD
            WORKING_DIRECTORY "${SOURCE_DIR}"
            OUTPUT_VARIABLE GIT_COMMIT_SHORT
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
            RESULT_VARIABLE GIT_RESULT)

        execute_process(
            COMMAND "${GIT_EXECUTABLE}" status --porcelain
            WORKING_DIRECTORY "${SOURCE_DIR}"
            OUTPUT_VARIABLE GIT_STATUS
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET)

        if(GIT_RESULT EQUAL 0 AND NOT "${GIT_COMMIT_SHORT}" STREQUAL "")
            if(NOT "${GIT_STATUS}" STREQUAL "")
                set(GIT_COMMIT_SHORT "${GIT_COMMIT_SHORT}-dirty")
            endif()
            set(ORES_BUILD_INFO "local ${GIT_COMMIT_SHORT}")
        else()
            set(ORES_BUILD_INFO "local unknown")
        endif()
    else()
        set(ORES_BUILD_INFO "local unknown")
    endif()
endif()

# Write to a temp file then copy only if the content has changed.  This keeps
# the mtime of the real version.hpp stable when nothing has changed, avoiding
# unnecessary recompilation of all consumers.
set(tmp_file "${OUTPUT_FILE}.tmp")
configure_file("${INPUT_FILE}" "${tmp_file}" @ONLY)
execute_process(
    COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${tmp_file}" "${OUTPUT_FILE}")
file(REMOVE "${tmp_file}")
