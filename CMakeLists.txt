# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-`
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
cmake_minimum_required(VERSION 3.28 FATAL_ERROR)

#
# vcpkg
#
# Always include common overlays
set(ORES_OVERLAYS_DIR "${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/overlays")
set(VCPKG_OVERLAY_PORTS "${ORES_OVERLAYS_DIR}/common")

# Add Linux-specific overlays
if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
    list(APPEND VCPKG_OVERLAY_PORTS "${ORES_OVERLAYS_DIR}/linux")
endif()

set(CMAKE_TOOLCHAIN_FILE
    ${CMAKE_CURRENT_SOURCE_DIR}/vcpkg/scripts/buildsystems/vcpkg.cmake
    CACHE STRING "Vcpkg toolchain file")

# Log CMake version. Useful for debugging CMake problems.
message(STATUS "CMake Version: ${CMAKE_VERSION}")

# ctest support
enable_testing()
configure_file(CTestCustom.cmake CTestCustom.cmake COPYONLY)

project(OreStudio VERSION 0.0.16 LANGUAGES CXX
    DESCRIPTION "UI and other utilities for the Open Source Risk Engine (ORE).")

# Copy all binaries into the publish directory.
set(ORES_PUBLISH_DIRECTORY ${CMAKE_BINARY_DIR}/publish)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${ORES_PUBLISH_DIRECTORY}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ORES_PUBLISH_DIRECTORY}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORES_PUBLISH_DIRECTORY}/bin)

# Qt domain plugins are placed alongside the application binary so that
# QPluginLoader finds them with a simple applicationDirPath() lookup on both
# Linux and Windows (no platform-specific subdirectory logic required).
set(ORES_PLUGIN_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

# use, i.e. don't skip the full RPATH for the build tree
set(CMAKE_SKIP_BUILD_RPATH FALSE)

# when building, don't use the install RPATH already, but later on when
# installing.
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

if(WIN32 AND (MSVC OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang"))
    # Export all symbols on windows for now. Bit of a hack.
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
endif()

if(UNIX)
    #
    # Note sure why, but this cannot be within the packaging configuration.
    #
    set(CMAKE_INSTALL_PREFIX "/opt/OreStudio/${CMAKE_PROJECT_VERSION}")
    set(CPACK_PACKAGING_INSTALL_PREFIX "/opt/OreStudio/${CMAKE_PROJECT_VERSION}")
    set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

    # add the automatically determined parts of the RPATH
    # which point to directories outside the build tree to the install RPATH
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

    # the RPATH to be used when installing, but only if it's not a system directory
    list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
    if("${isSystemDir}" STREQUAL "-1")
        set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
    endif()
endif()
#
# add our own modules
#
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/build/cmake/modules)

#
# options
#
option(WITH_PROFILING "Generate profiling information for code coverage." OFF)
option(BUILD_SHARED_LIBS "Build using shared libraries" ON)
option(WITH_WT "Build the Wt web interface (requires Wt library)" ON)

#
# Test environment variables: applied to all test targets.
# All variables from .env are forwarded verbatim to test processes (except the
# JWT private key which is multi-line encoded and not needed by tests). In CI
# the process environment already contains the required variables so .env is
# not read. ORES_TEST_PASSWORD_FAST enables fast scrypt parameters for tests.
#
set(ORES_TEST_ENV "ORES_TEST_PASSWORD_FAST=1")

# Load all variables from .env for local development. Skipped in CI
# environments where variables are already exported into the process env.
# .env is registered as a configure-time dependency so any change triggers
# an automatic re-configure on the next make/cmake --build invocation.
if(NOT DEFINED ENV{CI})
    set(_ores_env_file "${CMAKE_SOURCE_DIR}/.env")
    if(EXISTS "${_ores_env_file}")
        set_property(DIRECTORY APPEND PROPERTY
            CMAKE_CONFIGURE_DEPENDS "${_ores_env_file}")
        file(STRINGS "${_ores_env_file}" _ores_env_lines)
        foreach(_line ${_ores_env_lines})
            # Only process KEY=value lines; skip comments and blank lines
            if(NOT _line MATCHES "^[A-Za-z_][A-Za-z0-9_]*=")
                continue()
            endif()
            # Skip the JWT private key — too long and tests don't need it
            if(_line MATCHES "^ORES_IAM_SERVICE_JWT_PRIVATE_KEY")
                continue()
            endif()
            string(APPEND ORES_TEST_ENV ";${_line}")
        endforeach()
        message(STATUS "Loaded .env into test environment")
    endif()
endif()

# Command prefix for add_custom_target test runners: injects the test
# environment variables via "cmake -E env VAR=val ... --". This ensures
# tests launched by "make rat" see the same credentials as CTest-based runs.
set(ORES_TEST_ENV_CMD ${CMAKE_COMMAND} -E env ${ORES_TEST_ENV} --)

#
# c++ standard.
#
message(STATUS "Targeting C++ standard: ${CMAKE_CXX_STANDARD}")

#
# Setup CCache
#
find_program(CCACHE_PROGRAM sccache)
if (CCACHE_PROGRAM)
    message(STATUS "Found SCCache (${CCACHE_PROGRAM})...")
    set(CMAKE_C_COMPILER_LAUNCHER "${CCACHE_PROGRAM}" CACHE STRING "")
    set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}" CACHE STRING "")
else()
    message(STATUS "SCCache NOT found.")
endif()

# Threads
find_package(Threads REQUIRED)

# Boost
set(Boost_USE_MULTITHREADED ON)
macro(get_win_hex outvar)
    string(REGEX MATCH "^([0-9]+)\\.([0-9]+)" ${outvar} ${CMAKE_SYSTEM_VERSION})
    math(EXPR ${outvar} "(${CMAKE_MATCH_1} << 8) + ${CMAKE_MATCH_2}" OUTPUT_FORMAT HEXADECIMAL)
endmacro()

if(WIN32)
    get_win_hex(winver)
    add_compile_definitions(_WIN32_WINNT=${winver})
    add_compile_definitions(BOOST_USE_WINAPI_VERSION=${winver})
    # Prevent Windows.h from including the legacy WinSock.h header, which
    # conflicts with WinSock2.h pulled in by Boost.ASIO (socket_types.hpp).
    add_compile_definitions(WIN32_LEAN_AND_MEAN)
    if(MSVC)
        # Allow object files with more than 65535 addressable sections.
        # Required by large translation units (e.g. registrar.cpp with many
        # template instantiations from rfl and Boost headers).
        add_compile_options(/bigobj)
        # Serialise PDB writes when Ninja runs multiple cl.exe in parallel.
        # Without /FS, concurrent writes to the same vc140.pdb cause C1041.
        add_compile_options(/FS)
    endif()
endif()

find_package(Boost 1.88 REQUIRED COMPONENTS
    log
    json
    crc
    uuid
    exception
    scope_exit
    program_options
    iostreams
    process
    graph)

# OpenSSL
find_package(OpenSSL REQUIRED)

# Interface library for targets that use OPENSSL_cleanup() for proper resource
# cleanup. This centralizes the OpenSSL::Crypto dependency for main() functions
# and test harnesses that need to call OPENSSL_cleanup() on exit.
add_library(ores.openssl_cleanup INTERFACE)
target_link_libraries(ores.openssl_cleanup INTERFACE OpenSSL::Crypto)

# Qt
set(QT_NO_PRIVATE_MODULE_WARNING ON)
find_package(Qt6 REQUIRED COMPONENTS Core Gui Widgets Concurrent Network Svg Charts)

# Reflect CPP
find_package(pugixml CONFIG REQUIRED)
find_package(reflectcpp CONFIG REQUIRED)

# SQLGen
find_package(sqlgen REQUIRED)

# Magic enum
find_package(magic_enum CONFIG REQUIRED)

# Faker C++
find_package(faker-cxx REQUIRED)

# Catch2
find_package(Catch2 3 REQUIRED)

# LibFort
find_package(libfort)

# croncpp: header-only cron expression parser used by ores.scheduler
find_package(croncpp CONFIG REQUIRED)

# site (for ctest)
site_name(VORE_SITE)

#
# Handle build information
#
# version.hpp is now stamped on every build by cmake/generate_version.cmake
# (via add_custom_target in ores.utility/src/CMakeLists.txt). The top-level
# CMakeLists.txt no longer needs to compute ORES_BUILD_INFO at configure time.
#
message(STATUS "Build info: stamped at build time via cmake/generate_version.cmake")

#
# Build type
#
if(CMAKE_BUILD_TYPE MATCHES Debug)
    message(STATUS "Configuring in DEBUG mode")
elseif(CMAKE_BUILD_TYPE MATCHES Release)
    if (UNIX)
        set(CMAKE_INSTALL_DO_STRIP TRUE)
    endif()
    message(STATUS "Configuring in RELEASE mode")
elseif(CMAKE_BUILD_TYPE MATCHES RelWithDebInfo)
    message(STATUS "Configuring in RELEASE mode WITH symbols")
elseif(CMAKE_BUILD_TYPE MATCHES MinRelSize)
    if (UNIX)
        set(CMAKE_INSTALL_DO_STRIP TRUE)
    endif()
    message(STATUS "Configuring in RELEASE mode with minimised size")
else()
    message(STATUS "Unrecognized build type - will use cmake defaults")
endif()
message(STATUS "Product version: ${DOGEN_VERSION}")

#
# Analysis and formatting tools
#
find_package(ClangTools)
if (CLANG_FORMAT_FOUND)
  message(STATUS "Found clang-format (${CLANG_FORMAT_BIN}).")
else()
  message(STATUS "Could not find clang-format.")
endif()

if (CLANG_TIDY_FOUND)
  message(STATUS "Found clang-tidy (${CLANG_FORMAT_BIN}).")
else()
  message(STATUS "Could not find clang-tidy.")
endif()

#
# PlantUML
#
find_program(PLANTUML_PROGRAM plantuml)
if (PLANTUML_PROGRAM)
    message(STATUS "Found PlantUML: ${PLANTUML_PROGRAM}")
    set(WITH_PLANTUML "on")
    set(PLANTUML_ENVIRONMENT PLANTUML_LIMIT_SIZE=131072)
else()
    message(STATUS "PlantUML not found.")
endif()

#
# Emacs related targets
#
set(ORES_SITE_DIRECTORY "build/output/site")
add_custom_target(deploy_site
    COMMENT "Deploying ORE Studio Website to ${ORES_SITE_DIRECTORY}" VERBATIM
    COMMAND echo "Removing site directory" && rm -rf ${ORES_SITE_DIRECTORY}
    COMMAND emacs -Q --script .build-site.el
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

set(ORES_SKILLS_DIRECTORY ".claude/skills")
add_custom_target(deploy_skills
    COMMENT "Deploying ORE Studio Claude Code Skills to ${ORES_SIRE_DIRECTORY}" VERBATIM
    COMMAND echo "Removing skills directory" && rm -rf ${ORES_SKILLS_DIRECTORY}
    COMMAND emacs -Q --script .build-skills.el 2>&1
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

set(ORES_TASKS_DIRECTORY "build/output/plan")
add_custom_target(deploy_plan
    COMMENT "Deploying ORE Studio Plan to ${ORES_TASKS_DIRECTORY}" VERBATIM
    COMMAND echo "Removing site directory" && rm -rf ${ORES_TASKS_DIRECTORY}
    COMMAND emacs -Q --script .build-plan.el
    COMMAND mkdir -p ${ORES_TASKS_DIRECTORY}
    COMMAND mv doc/agile/v0/version_zero.tjp ${ORES_TASKS_DIRECTORY}
    COMMAND cd ${ORES_TASKS_DIRECTORY}
    COMMAND tj3 version_zero.tjp
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

add_custom_target(generate_protocol_docs
    COMMENT "Generating protocol documentation" VERBATIM
    COMMAND python3 ${CMAKE_SOURCE_DIR}/scripts/generate_protocol_docs.py
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

#
# Java args for PlantUML
#
set(ORES_PLANTUML_JAR "/usr/share/plantuml/plantuml.jar")
set(ORES_JAVA_ARGS "-Djava.awt.headless=true")
list(APPEND ORES_JAVA_ARGS "-DPLANTUML_LIMIT_SIZE=131072")

#
# aggregate targets and their aliases
#
add_custom_target(run_all_tests)
add_custom_target(rat)
add_dependencies(rat run_all_tests)

add_custom_target(make_all_diagrams)
add_custom_target(mad)
add_dependencies(mad make_all_diagrams)

add_subdirectory(${CMAKE_SOURCE_DIR}/build)
add_subdirectory(${CMAKE_SOURCE_DIR}/assets)
add_subdirectory(${CMAKE_SOURCE_DIR}/projects)
add_subdirectory(${CMAKE_SOURCE_DIR}/doc/skills/modeling)
