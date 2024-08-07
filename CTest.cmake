# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be  useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.
#
cmake_minimum_required(VERSION 3.29 FATAL_ERROR)

#
# Handle input parameters to script. Define them as internal CMake
# variables.
#
if(DEFINED CTEST_SCRIPT_ARG)
    # transform ctest script arguments of the form
    # script.ctest,var1=value1,var2=value2
    # to variables with the respective names set to the respective values
    string(REPLACE "," ";" script_args "${CTEST_SCRIPT_ARG}")
    foreach(current_var ${script_args})
        if ("${current_var}" MATCHES "^([^=]+)=(.+)$")
            set("${CMAKE_MATCH_1}" "${CMAKE_MATCH_2}")
        endif()
    endforeach()
endif()

#
# Detect if we are in GitHub or not.
#
if(NOT DEFINED "ENV{GITHUB_ACTIONS}")
    message(STATUS "This script is being run outside of GitHub-Actions.")
else ()
    message(STATUS "This script is being run inside of GitHub-Actions")
endif()

#
# Ensure all mandatory parameters have been set. Handle all optional parameters
# as well.
#
if(NOT DEFINED preset)
    message(FATAL_ERROR "Parameter preset not defined.")
endif()
message(STATUS "CDash preset: ${preset}")

set(build_name ${preset})
if(DEFINED build_postfix)
    set(build_name ${build_name}-${build_postfix})
endif()
message(STATUS "CDash build name: ${build_name}")

if(NOT DEFINED build_group)
    message(FATAL_ERROR "Parameter build_group not defined.")
endif()

if(NOT DEFINED with_memcheck)
    set(with_memcheck OFF)
endif()
message(STATUS "Memory Check: ${with_memcheck}")

#
# Parse the preset to extract input parameters, and validate them.
#
string(TOLOWER "${preset}" preset_lower)
string(REPLACE "-" ";" preset_list ${preset_lower})
list(LENGTH preset_list pl_length)
if (NOT pl_length EQUAL 3)
    message(FATAL_ERROR "Invalid preset: ${preset}")
endif()

# Setup the operative system.
list(GET preset_list 0 operative_system)
if(NOT DEFINED operative_system)
    message(FATAL_ERROR "Operative system not supplied.")
endif()

 if(${operative_system} STREQUAL "linux")
     message(STATUS "OS: Linux")
 elseif(${operative_system} STREQUAL "windows")
     message(STATUS "OS: Windows")
 elseif(${operative_system} STREQUAL "macos")
     message(STATUS "OS: Mac OS")
 else()
    message(FATAL_ERROR "Unsupported operative system: ${operative_system}")
endif()

# Setup the compiler.
list(GET preset_list 1 compiler)
if(NOT DEFINED compiler)
    message(FATAL_ERROR "Compiler not supplied.")
endif()

if(${compiler} STREQUAL "gcc")
    message(STATUS "Compiler: GCC")
elseif(${compiler} STREQUAL "clang")
    message(STATUS "Compiler: Clang")
elseif(${compiler} STREQUAL "msvc")
    message(STATUS "Compiler: MSVC")
elseif(${compiler} STREQUAL "msvc-clang-cl")
    message(STATUS "Compiler: Clang-CL (MSVC)")
else()
    message(FATAL_ERROR "Unsupported compiler: ${compiler}")
endif()

# Setup the configuration
list(GET preset_list 2 configuration)
if(NOT DEFINED configuration)
     message(FATAL_ERROR "Configuration not supplied.")
 endif()

if(configuration STREQUAL "debug")
    message(STATUS "CMake configuration: Debug")
    set(CTEST_CONFIGURATION_TYPE "Debug")
elseif(configuration STREQUAL "release")
    message(STATUS "CMake configuration: Release")
    set(CTEST_CONFIGURATION_TYPE "Release")
else()
    message(FATAL_ERROR "Configuration not supported: ${configuration}")
endif()

# Setup the build group.
if(build_group STREQUAL "Nightly")
    message(STATUS "CDash build_group: Nightly")
elseif(build_group STREQUAL "Continuous")
    message(STATUS "CDash build_group: Continuous")
elseif(build_group STREQUAL "Experimental")
    message(STATUS "CDash build_group: Experimental")
else()
    message(FATAL_ERROR "Build group not supported: ${build_group}")
endif()

#
# Setup CTest variables
#
if (DEFINED ENV{VORE_BUILD_PROVIDER})
    set(CTEST_SITE $ENV{VORE_BUILD_PROVIDER})
else()
    site_name(APP_SITE)
    set(CTEST_SITE "${APP_SITE}")
endif()

#
# Setup the build name
#
set(CTEST_BUILD_NAME "${build_name}")

# Set the generator. This will override the presets, but we have no option as
# CTest refuses to configure unless there is a generator.
set(CTEST_CMAKE_GENERATOR "Ninja")

# Note that we expect CTest to be executed at the top-level directory.
set(CTEST_SOURCE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "Source directory: ${CTEST_SOURCE_DIRECTORY}")

# Note that we have hard-coded the paths to match the presets. This is not very
# nice but since it should not change often, it suffices.
set(CTEST_BINARY_DIRECTORY "${CTEST_SOURCE_DIRECTORY}/build/output/${preset}")
message(STATUS "Binary directory: ${CTEST_BINARY_DIRECTORY}")

# Read custom files
ctest_read_custom_files(${CTEST_SOURCE_DIRECTORY})

# Determine the number of jobs to run in parallel.
include(ProcessorCount)
ProcessorCount(nproc)
message(STATUS "Available processors: ${nproc}")
if(NOT "$ENV{CTEST_MAX_PARALLELISM}" STREQUAL "")
    if(nproc GREATER "$ENV{CTEST_MAX_PARALLELISM}")
        set(nproc "$ENV{CTEST_MAX_PARALLELISM}")
    endif()
endif()

# We assume Ninja, but this also works for make.
message(STATUS "Number of parallel jobs: ${nproc}")
set(CTEST_BUILD_FLAGS "-l${nproc}")

# How long to wait between timed-out CTest submissions
set(retry_delay 300)

# How many times to retry timed-out CTest submissions
set(retry_count 10)

set(WITH_COVERAGE false)
if(DEFINED code_coverage)
    if(${code_coverage} EQUAL 1)
        if(${compiler} STREQUAL "gcc")
            message(STATUS "Looking for gcov.")
            find_program(CTEST_COVERAGE_COMMAND NAMES gcov)
        elseif(${compiler} STREQUAL "clang")
            message(STATUS "Looking for llvm-cov.")
            find_program(CTEST_COVERAGE_COMMAND NAMES llvm-cov)
            set(CTEST_COVERAGE_EXTRA_FLAGS "gcov")
        else()
            message(STATUS "Compiler does not support code coverage.")
        endif()

        if(NOT CTEST_COVERAGE_COMMAND)
            message(STATUS "Coverage command not found, disabling coverage.")
            set(WITH_COVERAGE false)
        else()
            message(STATUS "Found coverage command: (${CTEST_COVERAGE_COMMAND}).")
            set(cmake_args ${cmake_args} "-DWITH_PROFILING=On")
            set(CTEST_COVERAGE_EXTRA_FLAGS
                "${CTEST_COVERAGE_EXTRA_FLAGS} --preserve-paths")
            set(WITH_COVERAGE true)
        endif()
    else()
        message(STATUS "Coverage not enabled.")
    endif()
else()
    message(STATUS "Coverage not enabled.")
endif()

#
# Step: start the build
#
ctest_start(${build_group})

#
# Memory Checks
#
set(USE_MEMCHECK OFF)
if(with_memcheck)
    # setup valgrind
    find_program(CTEST_MEMORYCHECK_COMMAND NAMES valgrind)
    if(NOT CTEST_MEMORYCHECK_COMMAND)
        message("valgrind not found, disabling it.")
    else()
        message("Found valgrind (${CTEST_MEMORYCHECK_COMMAND})...")
        set(USE_MEMCHECK ON)

        set(valgrind_options "--trace-children=yes")
        set(valgrind_options "${valgrind_options} --quiet")
        set(valgrind_options "${valgrind_options} --tool=memcheck")
        set(valgrind_options "${valgrind_options} --leak-check=full")
        set(valgrind_options "${valgrind_options} --show-reachable=yes")
        set(valgrind_options "${valgrind_options} --num-callers=50")
        set(valgrind_options "${valgrind_options} --demangle=yes")
        set(valgrind_options "${valgrind_options} --gen-suppressions=all")
        set(CTEST_MEMORYCHECK_COMMAND_OPTIONS ${valgrind_options})
        set(CTEST_MEMORYCHECK_SUPPRESSIONS_FILE
            "${CTEST_SOURCE_DIRECTORY}/build/valgrind/custom.supp")
    endif()
endif()

#
# Step: Version control.
#
find_package(Git)
set(CTEST_UPDATE_COMMAND "${GIT_EXECUTABLE}")

# In CI, we do not actually want to run an update, just retrieve the current
# version.
set(CTEST_UPDATE_VERSION_ONLY ON)
ctest_update(RETURN_VALUE update_result)
if(git_result)
    message(FATAL_ERROR "Failed to update source code from git.")
endif()

# Setup the preset for configuration.
set(cmake_args ${cmake_args} "--preset ${preset}")

message(STATUS "CMake args: ${cmake_args}")
ctest_configure(OPTIONS "${cmake_args}" RETURN_VALUE configure_result)
if(configure_result)
    message(FATAL_ERROR "Failed to configure")
endif()

#
# Step: build.
#
set(CTEST_BUILD_TARGET "package")
ctest_build(PARALLEL_LEVEL ${nproc})

#
# Step: test.
#
# If memcheck is enabled, run tests with valgrind, otherwise, run them normally.
#
# Note: because we are doing nothing with the return value, the build will be
# green even when tests fail. This is OK because we rely on CDash to see the
# testing status. Travis/AppVeyor just tells us weather the build and packaging
# steps have worked or failed.
if(USE_MEMCHECK)
    ctest_memcheck(PARALLEL_LEVEL ${nproc})
else()
    ctest_test(PARALLEL_LEVEL ${nproc} QUIET)
endif()

#
# Step: code coverage
#
if(WITH_COVERAGE)
    set(cov_result "")
    set(cov_capture_result "")
    ctest_coverage(RETURN_VALUE cov_result
        CAPTURE_CMAKE_ERROR cov_capture_result
        QUIET)
    message(STATUS "Result: ${cov_result}")
    message(STATUS "Cov capture result: ${cov_capture_result}")
endif()

#
# Step: submit build results
#
ctest_submit(RETRY_COUNT ${retry_count} RETRY_DELAY ${retry_delay})
