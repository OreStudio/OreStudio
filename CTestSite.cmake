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
# Submit the documentation site build (org-publish + Doxygen via the
# deploy_site target) to CDash as its own build group, so build engineers can
# see at a glance whether the documentation built. A broken org id-link fails
# ores-build-site.el, which fails the build step here and is reported to CDash.
#
# Usage:
#   ctest -VV --script "CTestSite.cmake,preset=<preset>,build_group=Site"
# Set ORES_CDASH_NOSUBMIT=1 in the environment to skip the CDash submission
# (used when testing the script locally).
#
cmake_minimum_required(VERSION 3.29 FATAL_ERROR)

# Transform "script,var1=value1,var2=value2" args into CMake variables.
if(DEFINED CTEST_SCRIPT_ARG)
    string(REPLACE "," ";" script_args "${CTEST_SCRIPT_ARG}")
    foreach(current_var ${script_args})
        if ("${current_var}" MATCHES "^([^=]+)=(.+)$")
            set("${CMAKE_MATCH_1}" "${CMAKE_MATCH_2}")
        endif()
    endforeach()
endif()

if(NOT DEFINED preset)
    message(FATAL_ERROR "Parameter preset not defined.")
endif()
if(NOT DEFINED build_group)
    set(build_group "Site")
endif()
message(STATUS "CDash build group: ${build_group}")

# with_doxygen controls which targets the build runs:
#   OFF (default) — org-publish site only; the fast link-checking build for PRs.
#   ON            — site + Doxygen; the full build, reserved for merges to main.
if(NOT DEFINED with_doxygen)
    set(with_doxygen OFF)
endif()
message(STATUS "Build Doxygen: ${with_doxygen}")

# Build name: distinguish the documentation build from the compile builds.
set(CTEST_BUILD_NAME "${preset}-site")
message(STATUS "CDash build name: ${CTEST_BUILD_NAME}")

# Derive the generator from the preset's build-tool segment (…-<tool>).
# Presets with fewer than 4 dash-separated segments (e.g. linux-doc-only)
# default to Unix Makefiles — the platform default and the make mixin.
string(TOLOWER "${preset}" preset_lower)
string(REPLACE "-" ";" preset_list ${preset_lower})
list(LENGTH preset_list preset_len)
if(preset_len GREATER 3)
    list(GET preset_list 3 build_tool)
else()
    set(build_tool "make")
endif()
if(build_tool STREQUAL "ninja")
    set(CTEST_CMAKE_GENERATOR "Ninja")
elseif(build_tool STREQUAL "make")
    set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
else()
    message(FATAL_ERROR "Unsupported build tool: ${build_tool}")
endif()

if(DEFINED ENV{ORES_BUILD_PROVIDER})
    set(CTEST_SITE $ENV{ORES_BUILD_PROVIDER})
else()
    site_name(APP_SITE)
    set(CTEST_SITE "${APP_SITE}")
endif()

set(CTEST_SOURCE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
set(CTEST_BINARY_DIRECTORY "${CTEST_SOURCE_DIRECTORY}/build/output/${preset}")
message(STATUS "Source directory: ${CTEST_SOURCE_DIRECTORY}")
message(STATUS "Binary directory: ${CTEST_BINARY_DIRECTORY}")

set(retry_delay 300)
set(retry_count 10)
set(had_failures OFF)

# Keep Emacs package bootstrap noise out of the CDash dashboard. The cold
# install (see ores-site-packages.el) is pre-warmed out of band below, but
# belt-and-suspenders: never let anything under ./.packages — third-party
# byte-compile warnings or the benign seq activation message — be counted as a
# build error/warning against our build.
set(CTEST_CUSTOM_WARNING_EXCEPTION ${CTEST_CUSTOM_WARNING_EXCEPTION}
    "\\.packages/" "package--load-files-for-activation")
set(CTEST_CUSTOM_ERROR_EXCEPTION ${CTEST_CUSTOM_ERROR_EXCEPTION}
    "\\.packages/" "package--load-files-for-activation")

ctest_start(${build_group})

# Version-only update (detached-HEAD PR checkouts return non-zero harmlessly).
find_package(Git)
set(CTEST_UPDATE_COMMAND "${GIT_EXECUTABLE}")
set(CTEST_UPDATE_VERSION_ONLY ON)
ctest_update(RETURN_VALUE update_result)

# Configure (reuses the preset; cheap on an already-configured tree).
ctest_configure(OPTIONS "--preset ${preset}" RETURN_VALUE configure_result)
if(NOT configure_result EQUAL 0)
    message(WARNING "Failed to configure")
    set(had_failures ON)
endif()

# Pre-warm the Emacs package cache OUT OF BAND, before the monitored build.
# On a cold CI checkout package-install byte-compiles dozens of third-party
# files; running it here via execute_process keeps that output in the plain CI
# step log instead of the CTest-scraped deploy_site build. When the cache is
# already warm (developer machines) this is a quick no-op.
if(configure_result EQUAL 0)
    message(STATUS "Pre-warming Emacs package cache (out of band)")
    execute_process(
        COMMAND emacs -Q --batch
            -l projects/ores.lisp/src/ores-site-packages.el
            --eval "(ores-site-ensure-packages)"
        WORKING_DIRECTORY "${CTEST_SOURCE_DIRECTORY}"
        RESULT_VARIABLE prewarm_result)
    if(NOT prewarm_result EQUAL 0)
        message(WARNING "Package pre-warm failed with error code: ${prewarm_result}")
        set(had_failures ON)
    endif()
endif()

# Build the org-publish site target. ores-build-site.el fails on a broken
# org id-link, so a bad link fails this step and is reported to CDash.
set(build_result 0)
if(configure_result EQUAL 0)
    set(CTEST_BUILD_TARGET "deploy_site")
    ctest_build(RETURN_VALUE build_result)
    if(NOT build_result EQUAL 0)
        message(WARNING "Site build failed with error code: ${build_result}")
        set(had_failures ON)
    endif()
endif()

# Doxygen only for the full build (with_doxygen=ON, merges to main). It is not
# a CMake target, so run it directly; a failure fails the Site build on CDash.
if(with_doxygen AND configure_result EQUAL 0 AND build_result EQUAL 0)
    message(STATUS "Running Doxygen")
    execute_process(
        COMMAND doxygen build/doxygen/Doxyfile
        WORKING_DIRECTORY "${CTEST_SOURCE_DIRECTORY}"
        RESULT_VARIABLE doxygen_result)
    if(NOT doxygen_result EQUAL 0)
        message(WARNING "Doxygen failed with error code: ${doxygen_result}")
        set(had_failures ON)
    endif()
endif()

# Attach the PR URL as Notes on pull-request builds (build name unchanged).
if(NOT "$ENV{ORES_PR_URL}" STREQUAL "")
    set(notes_file "${CTEST_BINARY_DIRECTORY}/cdash_site_notes.txt")
    file(WRITE "${notes_file}"
        "Pull request: $ENV{ORES_PR_URL}\n"
        "Commit: $ENV{ORES_BUILD_COMMIT}\n")
    set(CTEST_NOTES_FILES "${notes_file}")
endif()

# Submit to CDash (skipped for local testing via ORES_CDASH_NOSUBMIT=1).
if("$ENV{ORES_CDASH_NOSUBMIT}" STREQUAL "1")
    message(STATUS "ORES_CDASH_NOSUBMIT=1 — skipping CDash submission.")
else()
    ctest_submit(RETRY_COUNT ${retry_count} RETRY_DELAY ${retry_delay}
        RETURN_VALUE submit_result)
    if(submit_result)
        message(WARNING "CDash submission failed (exit code: ${submit_result}).")
    endif()
endif()

if(had_failures)
    message(FATAL_ERROR "Site build completed with failures. Results submitted to CDash.")
endif()
