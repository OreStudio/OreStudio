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
# Called by add_custom_target on every build. Writes two files, and the
# distinction between them is the whole point of this script:
#
#   version.cpp  - the template, with nothing build-specific substituted into
#                  it. Its content is therefore identical on every build, so
#                  copy_if_different leaves its mtime alone and no target
#                  recompiles or relinks when a commit lands.
#
#   the manifest - the fast-changing facts: the commit, whether the tree was
#                  dirty, the branch. It is a plain key=value file written
#                  beside the binaries, and version.cpp reads it at runtime.
#
# Nothing lists the manifest as a build input, so rewriting it relinks
# nothing. Do not make it a BYPRODUCT or a dependency: that reintroduces the
# relink cascade this file exists to remove.
#
# Required input variables (passed with -D):
#   SOURCE_DIR                - project root (for git commands)
#   OreStudio_VERSION_MAJOR   - major version number
#   OreStudio_VERSION_MINOR   - minor version number
#   OreStudio_VERSION_PATCH   - patch version number
#   ORES_PLATFORM_TRIPLET     - the triplet this build targets
#   INPUT_FILE                - path to version.cpp.in
#   OUTPUT_FILE               - path to the generated version.cpp
#   MANIFEST_FILE             - path to the manifest to write

cmake_minimum_required(VERSION 3.21)

set(ORES_VERSION
    "${OreStudio_VERSION_MAJOR}.${OreStudio_VERSION_MINOR}.${OreStudio_VERSION_PATCH}")

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
    set(ORES_COMMIT "$ENV{ORES_BUILD_COMMIT}")
    set(ORES_COMMIT_FULL "$ENV{ORES_BUILD_COMMIT}")
    set(ORES_DIRTY "false")
    set(ORES_BRANCH "")
else()
    # Local build: the git state and nothing else. The wall-clock build time
    # is read at runtime from the executable's own mtime, so there is no
    # reason to write it here and every reason not to: a timestamp would
    # change the manifest on every build for no gain.
    find_program(GIT_EXECUTABLE git)
    if(GIT_EXECUTABLE)
        execute_process(
            COMMAND "${GIT_EXECUTABLE}" rev-parse --short HEAD
            WORKING_DIRECTORY "${SOURCE_DIR}"
            OUTPUT_VARIABLE ORES_COMMIT
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
            RESULT_VARIABLE GIT_RESULT)

        execute_process(
            COMMAND "${GIT_EXECUTABLE}" rev-parse HEAD
            WORKING_DIRECTORY "${SOURCE_DIR}"
            OUTPUT_VARIABLE ORES_COMMIT_FULL
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET)

        execute_process(
            COMMAND "${GIT_EXECUTABLE}" rev-parse --abbrev-ref HEAD
            WORKING_DIRECTORY "${SOURCE_DIR}"
            OUTPUT_VARIABLE ORES_BRANCH
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET)

        execute_process(
            COMMAND "${GIT_EXECUTABLE}" status --porcelain
            WORKING_DIRECTORY "${SOURCE_DIR}"
            OUTPUT_VARIABLE GIT_STATUS
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET)

        if(GIT_RESULT EQUAL 0 AND NOT "${ORES_COMMIT}" STREQUAL "")
            if(NOT "${GIT_STATUS}" STREQUAL "")
                set(ORES_DIRTY "true")
                set(ORES_BUILD_INFO "local ${ORES_COMMIT}-dirty")
            else()
                set(ORES_DIRTY "false")
                set(ORES_BUILD_INFO "local ${ORES_COMMIT}")
            endif()
        else()
            set(ORES_BUILD_INFO "local unknown")
            set(ORES_COMMIT "unknown")
            set(ORES_COMMIT_FULL "unknown")
            set(ORES_DIRTY "false")
            set(ORES_BRANCH "")
        endif()
    else()
        set(ORES_BUILD_INFO "local unknown")
        set(ORES_COMMIT "unknown")
        set(ORES_COMMIT_FULL "unknown")
        set(ORES_DIRTY "false")
        set(ORES_BRANCH "")
    endif()
endif()

if(NOT DEFINED ENV{ORES_CHECKOUT_LABEL})
    set(ORES_CHECKOUT_LABEL "")
else()
    set(ORES_CHECKOUT_LABEL "$ENV{ORES_CHECKOUT_LABEL}")
endif()

# version.cpp carries no build-specific data, so its content is stable. Write
# to a temp file and copy only on change, which keeps its mtime stable and
# therefore keeps every consumer of ores.utility from relinking.
set(tmp_file "${OUTPUT_FILE}.tmp")
configure_file("${INPUT_FILE}" "${tmp_file}" @ONLY)
execute_process(
    COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${tmp_file}" "${OUTPUT_FILE}")
file(REMOVE "${tmp_file}")

# The manifest. Same copy-on-change treatment, though nothing depends on it.
get_filename_component(MANIFEST_DIR "${MANIFEST_FILE}" DIRECTORY)
file(MAKE_DIRECTORY "${MANIFEST_DIR}")
set(MANIFEST_CONTENT
"format=1
version=${ORES_VERSION}
build_info=${ORES_BUILD_INFO}
commit=${ORES_COMMIT}
commit_full=${ORES_COMMIT_FULL}
dirty=${ORES_DIRTY}
branch=${ORES_BRANCH}
triplet=${ORES_PLATFORM_TRIPLET}
checkout_label=${ORES_CHECKOUT_LABEL}
")
file(WRITE "${MANIFEST_FILE}.tmp" "${MANIFEST_CONTENT}")
execute_process(
    COMMAND "${CMAKE_COMMAND}" -E copy_if_different
            "${MANIFEST_FILE}.tmp" "${MANIFEST_FILE}")
file(REMOVE "${MANIFEST_FILE}.tmp")
