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
cmake_minimum_required(VERSION 3.29 FATAL_ERROR)

#
# vcpkg
#
set(CMAKE_TOOLCHAIN_FILE
    ${CMAKE_CURRENT_SOURCE_DIR}/vcpkg/scripts/buildsystems/vcpkg.cmake
    CACHE STRING "Vcpkg toolchain file")

# Log CMake version. Useful for debugging CMake problems.
message(STATUS "CMake Version: ${CMAKE_VERSION}")

# ctest support
enable_testing()

project(OreStudio VERSION 0.0.3 LANGUAGES CXX
    DESCRIPTION "UI and other utilities for the Open Source Risk Engine (ORE).")

# Copy all binaries into the publish directory.
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/publish/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/publish/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/publish/bin)

# use, i.e. don't skip the full RPATH for the build tree
set(CMAKE_SKIP_BUILD_RPATH FALSE)

# when building, don't use the install RPATH already, but later on when
# installing.
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

if(WIN32 AND (MSVC OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang"))
    # Export all symbols on windows for now. Bit of a hack.
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

    # Put all DLLs and executables in the same directory on Windows so tests can
    # find the DLLs they depend on.
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
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
include(AddBoostTests)

#
# options
#
option(WITH_PROFILING "Generate profiling information for code coverage." OFF)
option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

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
endif()

find_package(Boost 1.88 REQUIRED COMPONENTS
    log
    crc
    exception
    program_options
    unit_test_framework)

# OpenSSL
find_package(OpenSSL REQUIRED)

# Qt
set(QT_NO_PRIVATE_MODULE_WARNING ON)
find_package(Qt6 REQUIRED COMPONENTS Core Gui Widgets Concurrent)

# Reflect CPP
find_package(pugixml CONFIG REQUIRED)
find_package(reflectcpp CONFIG REQUIRED)

# SQLGen
find_package(sqlgen REQUIRED)

# Magic enum
find_package(magic_enum CONFIG REQUIRED)

# site (for ctest)
site_name(VORE_SITE)

#
# Handle build information
#
set(WITH_BUILD_INFO ON)
set(ORES_BUILD_INFO "")

if (NOT DEFINED ENV{ORES_BUILD_PROVIDER})
    set(WITH_BUILD_INFO OFF)
endif()

if (NOT DEFINED ENV{ORES_BUILD_COMMIT})
    set(WITH_BUILD_INFO OFF)
endif()

if (NOT DEFINED ENV{ORES_BUILD_NUMBER})
    set(WITH_BUILD_INFO OFF)
endif()

if (NOT DEFINED ENV{ORES_BUILD_TIMESTAMP})
    set(WITH_BUILD_INFO OFF)
endif()

if (WITH_BUILD_INFO)
    set(ORES_BUILD_INFO "Build: Provider = '$ENV{ORES_BUILD_PROVIDER}'")
    set(ORES_BUILD_INFO "${ORES_BUILD_INFO} Number = '$ENV{ORES_BUILD_NUMBER}'")
    set(ORES_BUILD_INFO "${ORES_BUILD_INFO} Commit = '$ENV{ORES_BUILD_COMMIT}'")
    set(ORES_BUILD_INFO "${ORES_BUILD_INFO} Timestamp = '$ENV{ORES_BUILD_TIMESTAMP}'")
else()
    set(ORES_BUILD_INFO "Build: Provider = LOCAL")
    message(STATUS "Not adding build info.")
endif()
message(STATUS "${ORES_BUILD_INFO}")

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
    set(PLANTUML_ENVIRONMENT PLANTUML_LIMIT_SIZE=65536 PLANTUML_SECURITY_PROFILE=UNSECURE)
else()
    message(STATUS "PlantUML not found.")
endif()

#
# aggregate targets and their aliases
#
add_custom_target(run_all_tests)
add_custom_target(rat)
add_dependencies(rat run_all_tests)

add_subdirectory(${CMAKE_SOURCE_DIR}/build)
add_subdirectory(${CMAKE_SOURCE_DIR}/assets)
add_subdirectory(${CMAKE_SOURCE_DIR}/projects)
