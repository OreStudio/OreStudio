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
set(ORES_VCPKG_ENABLED ON CACHE BOOL
    "Enable vcpkg package manager. Set OFF for doc-only builds (deploy_site, \
deploy_skills, etc.) to skip the dependency install step entirely. \
Note: project(LANGUAGES CXX) is unconditional so a C++ compiler is still \
required at configure time even when vcpkg is disabled.")

if(ORES_VCPKG_ENABLED)
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
endif()

# Log CMake version. Useful for debugging CMake problems.
message(STATUS "CMake Version: ${CMAKE_VERSION}")

# ctest support
enable_testing()
configure_file(CTestCustom.cmake CTestCustom.cmake COPYONLY)

# CMake >= 4.4 enforces our warnings.uninitialized preset (see
# CMakePresets.json) during project()'s own compiler/toolchain detection,
# which now flags vcpkg's bundled toolchain file (vcpkg/scripts/buildsystems/
# vcpkg.cmake) for its standard set(VAR "${VAR}" CACHE ... FORCE) idiom on
# VCPKG_MANIFEST_DIR/VCPKG_BOOTSTRAP_OPTIONS/VCPKG_OVERLAY_TRIPLETS/
# Z_VCPKG_FEATURE_FLAGS. That file is vendored and gets overwritten on the
# next vcpkg update, so silence uninitialized warnings only around the
# project() call that triggers it; the rest of the build still catches
# genuine uninitialized-variable bugs in our own CMake code.
if(CMAKE_VERSION VERSION_GREATER_EQUAL "4.4")
    cmake_diagnostic(PUSH)
    cmake_diagnostic(SET CMD_UNINITIALIZED IGNORE)
endif()

project(OreStudio VERSION 0.0.24 LANGUAGES CXX
    DESCRIPTION "UI and other utilities for the Open Source Risk Engine (ORE).")

if(CMAKE_VERSION VERSION_GREATER_EQUAL "4.4")
    cmake_diagnostic(POP)
endif()

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

# Compiler cache selection (sccache/ccache), read from .env below. Empty
# means "no compiler cache" — CI overrides this to sccache unconditionally.
set(ORES_COMPILER_CACHE "")

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
            if(_line MATCHES "^ORES_COMPILER_CACHE=(.*)$")
                set(ORES_COMPILER_CACHE "${CMAKE_MATCH_1}")
            endif()
            string(APPEND ORES_TEST_ENV ";${_line}")
        endforeach()
        message(STATUS "Loaded .env into test environment")
    endif()
endif()

if(DEFINED ENV{CI})
    set(ORES_COMPILER_CACHE "sccache")
endif()

# Command prefix for add_custom_target test runners: injects the test
# environment variables via "cmake -E env VAR=val ... --". This ensures
# tests launched by "make rat" see the same credentials as CTest-based runs.
set(ORES_TEST_ENV_CMD ${CMAKE_COMMAND} -E env ${ORES_TEST_ENV} --)

#
# c++ standard.
#
message(STATUS "Targeting C++ standard: ${CMAKE_CXX_STANDARD}")

# The project uses no C++20 modules, so the P1689 dependency-scanning
# pre-pass (clang-scan-deps) buys nothing. On Windows it is invoked via
# cmd.exe, whose ~8191-char command-line limit truncates our long
# transitive include/define argument lists, silently dropping trailing
# -I flags and causing spurious "file not found" errors; it also adds
# a slow per-TU pre-pass that contributes to Windows CI timeouts.
set(CMAKE_CXX_SCAN_FOR_MODULES OFF)

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
# Defined here (before C++ dependency discovery) so that doc-only builds
# (ORES_VCPKG_ENABLED=OFF) can reach deploy_site, deploy_skills, etc. without
# configuring the full C++ toolchain.
#
set(ORES_SITE_DIRECTORY "build/output/site")
add_custom_target(deploy_site
    COMMENT "Deploying ORE Studio Website to ${ORES_SITE_DIRECTORY}" VERBATIM
    COMMAND emacs -Q --script projects/ores.lisp/src/ores-build-site.el
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

add_custom_target(org_roam_db_sync
    COMMENT "Syncing org-roam database (.org-roam.db)" VERBATIM
    COMMAND emacs -Q --script projects/ores.lisp/src/ores-sync-org-roam.el
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

set(ORES_MANUAL_PDF "doc/manual/user_guide/user_manual.pdf")
add_custom_target(deploy_manual
    COMMENT "Building ORE Studio User Manual PDF to ${ORES_MANUAL_PDF}" VERBATIM
    COMMAND emacs -Q --script projects/ores.lisp/src/ores-build-manual.el
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

set(ORES_HELP_DIRECTORY "build/output/help")
add_custom_target(deploy_help
    COMMENT "Exporting self-contained manual HTML for Qt help to ${ORES_HELP_DIRECTORY}" VERBATIM
    COMMAND emacs -Q --script projects/ores.lisp/src/ores-build-help.el
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

set(ORES_HELP_QCH_SOURCE
    "${CMAKE_SOURCE_DIR}/projects/ores.qt/application/resources/help/user_manual.qch")
add_custom_target(deploy_help_qch
    COMMENT "Recompiling user_manual.qch and updating source tree" VERBATIM
    COMMAND python3 build/scripts/generate_help_qch.py
        --output-dir ${CMAKE_SOURCE_DIR}/projects/ores.qt/application/resources/help
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
add_dependencies(deploy_help_qch deploy_help)

add_custom_target(tangle_clang_format
    COMMENT "Tangling .clang-format from doc/knowledge/architecture/clang_format_config.org" VERBATIM
    COMMAND emacs -Q --script projects/ores.lisp/src/ores-build-clang-format.el 2>&1
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

add_custom_target(tangle_codegen_templates
    COMMENT "Tangling codegen mustache templates from literate facet docs in projects/ores.codegen/library/templates/" VERBATIM
    COMMAND emacs -Q --script projects/ores.lisp/src/ores-build-codegen-templates.el 2>&1
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

add_custom_target(tangle_shell_scripts
    COMMENT "Generating the ores-shell script library from recipes in doc/recipes/shell/ into projects/ores.shell/scripts/library/" VERBATIM
    COMMAND emacs -Q --script projects/ores.lisp/src/ores-build-recipe-scripts.el 2>&1
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

set(ORES_SKILLS_DIRECTORY ".claude/skills")
add_custom_target(deploy_skills
    COMMENT "Deploying ORE Studio Claude Code Skills to ${ORES_SKILLS_DIRECTORY}" VERBATIM
    COMMAND echo "Removing skills directory" && rm -rf ${ORES_SKILLS_DIRECTORY}
    COMMAND emacs -Q --script projects/ores.lisp/src/ores-build-skills.el 2>&1
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

set(ORES_CLAUDE_SETTINGS_FILE ".claude/settings.json")
add_custom_target(deploy_settings
    COMMENT "Deploying ORE Studio Claude Code Settings to ${ORES_CLAUDE_SETTINGS_FILE}" VERBATIM
    COMMAND echo "Removing settings file" && rm -f ${ORES_CLAUDE_SETTINGS_FILE}
    COMMAND emacs -Q --script projects/ores.lisp/src/ores-build-settings.el 2>&1
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

set(ORES_TASKS_DIRECTORY "build/output/plan")
add_custom_target(deploy_plan
    COMMENT "Deploying ORE Studio Plan to ${ORES_TASKS_DIRECTORY}" VERBATIM
    COMMAND echo "Removing site directory" && rm -rf ${ORES_TASKS_DIRECTORY}
    COMMAND emacs -Q --script projects/ores.lisp/src/ores-build-plan.el
    COMMAND mkdir -p ${ORES_TASKS_DIRECTORY}
    COMMAND mv doc/agile/v0/version_zero.tjp ${ORES_TASKS_DIRECTORY}
    COMMAND cd ${ORES_TASKS_DIRECTORY}
    COMMAND tj3 version_zero.tjp
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

add_custom_target(sprint_charts
    COMMENT "Generating sprint health charts (requires gnuplot)" VERBATIM
    COMMAND ${CMAKE_SOURCE_DIR}/projects/ores.compass/compass.sh sprint charts
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

add_custom_target(make_all_diagrams)
add_custom_target(mad)
add_dependencies(mad make_all_diagrams)

add_custom_target(deploy_org
    COMMENT "Deploying all ORE Studio org-mode artefacts" VERBATIM)
add_dependencies(deploy_org deploy_site deploy_skills deploy_settings make_all_diagrams)
add_custom_target(dorg)
add_dependencies(dorg deploy_org)

# Doc-only mode: all documentation targets are now defined; skip the C++
# build configuration entirely (no find_package, no add_subdirectory).
if(NOT ORES_VCPKG_ENABLED)
    message(STATUS "ORES_VCPKG_ENABLED=OFF: skipping C++ build configuration.")
    return()
endif()

#
# Setup compiler cache (sccache or ccache)
#
# ORES_COMPILER_CACHE is set above: "sccache" unconditionally in CI, or
# whatever `compass env configure --compiler-cache` wrote to .env for local
# dev, or empty (no compiler cache) when neither applies — a checkout that
# has never been configured must not silently pick up a compiler cache some
# other tool installed on the machine for unrelated reasons.
#
if(ORES_COMPILER_CACHE)
    find_program(CCACHE_PROGRAM "${ORES_COMPILER_CACHE}")
    if (CCACHE_PROGRAM)
        message(STATUS "Found ${ORES_COMPILER_CACHE} (${CCACHE_PROGRAM})...")
        if(WIN32)
            # .sh scripts are not valid Win32 executables; use the tool directly.
            # SCCACHE_BASEDIR cross-worktree sharing is not available on Windows.
            set(CMAKE_C_COMPILER_LAUNCHER "${CCACHE_PROGRAM}" CACHE STRING "" FORCE)
            set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}" CACHE STRING "" FORCE)
        else()
            # Use a wrapper script rather than the tool directly so that, for
            # sccache, SCCACHE_BASEDIR is always set to this worktree's root at
            # build time. sccache normalises any absolute path under BASEDIR to
            # a relative path before computing the cache key, which means every
            # git worktree of the same commit shares a single cache entry per
            # translation unit. Without this, each worktree compiles every TU
            # from scratch (100% cache misses). The wrapper resolves the
            # project root from its own location at runtime, so it works
            # correctly across worktrees and machines without hardcoding.
            set(_compiler_cache_wrapper "${CMAKE_SOURCE_DIR}/build/scripts/compiler_cache_wrapper.sh")
            if(NOT EXISTS "${_compiler_cache_wrapper}")
                message(FATAL_ERROR "compiler cache wrapper not found: ${_compiler_cache_wrapper}")
            endif()
            set(CMAKE_C_COMPILER_LAUNCHER "${_compiler_cache_wrapper}" CACHE STRING "" FORCE)
            set(CMAKE_CXX_COMPILER_LAUNCHER "${_compiler_cache_wrapper}" CACHE STRING "" FORCE)
        endif()
    else()
        message(STATUS "${ORES_COMPILER_CACHE} NOT found.")
    endif()
else()
    message(STATUS "No compiler cache configured.")
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
    graph
    circular_buffer)

# OpenSSL
find_package(OpenSSL REQUIRED)

# Interface library for targets that use OPENSSL_cleanup() for proper resource
# cleanup. This centralizes the OpenSSL::Crypto dependency for main() functions
# and test harnesses that need to call OPENSSL_cleanup() on exit.
add_library(ores.openssl_cleanup INTERFACE)
target_link_libraries(ores.openssl_cleanup INTERFACE OpenSSL::Crypto)

# Qt
set(QT_NO_PRIVATE_MODULE_WARNING ON)
find_package(Qt6 REQUIRED COMPONENTS Core Gui Widgets Concurrent Network Svg Charts Help)

# Reflect CPP
find_package(pugixml CONFIG REQUIRED)
find_package(reflectcpp CONFIG REQUIRED)
find_package(unofficial-sqlite3 CONFIG REQUIRED)

# SQLGen
find_package(sqlgen REQUIRED)

# libpq on Windows is distributed as a static archive whose symbols depend on
# the PostgreSQL portability libraries (libpgport, libpgcommon), OpenSSL for
# the SSL connection paths, and a few Windows system libraries. vcpkg's
# libpq find-module wrapper only attaches these when the triplet linkage is
# "static"; the x64-windows dynamic triplet skips them, leaving undefined
# references to pg_tolower / pg_vsnprintf / pg_strong_random / RAND_bytes /
# BIO_new / OPENSSL_sk_num / ... in every DLL that pulls libpq in
# transitively (via sqlgen). Attach them directly onto the
# PostgreSQL::PostgreSQL imported target so that every consumer — not just
# ores.database — inherits the fix through normal CMake transitivity.
if(WIN32)
    find_package(PostgreSQL REQUIRED)
    find_library(ORES_PGPORT_LIBRARY
        NAMES pgport libpgport
        PATHS "${VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib"
        NO_DEFAULT_PATH)
    find_library(ORES_PGCOMMON_LIBRARY
        NAMES pgcommon libpgcommon
        PATHS "${VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib"
        NO_DEFAULT_PATH)
    if(TARGET PostgreSQL::PostgreSQL
       AND ORES_PGPORT_LIBRARY
       AND ORES_PGCOMMON_LIBRARY)
        set_property(TARGET PostgreSQL::PostgreSQL APPEND PROPERTY
            INTERFACE_LINK_LIBRARIES
                ${ORES_PGPORT_LIBRARY}
                ${ORES_PGCOMMON_LIBRARY}
                OpenSSL::SSL
                OpenSSL::Crypto
                Secur32
                Wldap32)
    endif()
endif()

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
message(STATUS "Product version: ${CMAKE_PROJECT_VERSION}")

#
# Analysis and formatting tools
#
find_package(ClangTools)
if (CLANG_FORMAT_FOUND)
    message(STATUS "Found clang-format (${CLANG_FORMAT_BIN}).")
    #
    # format: run clang-format in-place on all C++ sources under projects/.
    # Usage: cmake --build --preset <preset> --target format
    #
    add_custom_target(format
        COMMENT "Formatting C++ sources with clang-format (in-place)"
        COMMAND find "${CMAKE_SOURCE_DIR}/projects" -type f
                    \( -name "*.cpp" -o -name "*.hpp" -o -name "*.h" \)
                    -exec ${CLANG_FORMAT_BIN} -i {} +
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        VERBATIM)

    #
    # check-format: dry-run clang-format; exits non-zero if any file would change.
    # Usage: cmake --build --preset <preset> --target check-format
    #        Used by the nightly format workflow and for local verification.
    #
    add_custom_target(check-format
        COMMENT "Checking C++ formatting with clang-format (dry run)"
        COMMAND find "${CMAKE_SOURCE_DIR}/projects" -type f
                    \( -name "*.cpp" -o -name "*.hpp" -o -name "*.h" \)
                    -exec ${CLANG_FORMAT_BIN} --dry-run --Werror {} +
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        VERBATIM)
else()
    message(STATUS "Could not find clang-format; format and check-format targets unavailable.")
endif()

if (CLANG_TIDY_FOUND)
    message(STATUS "Found clang-tidy (${CLANG_TIDY_BIN}).")
else()
    message(STATUS "Could not find clang-tidy.")
endif()

#
# Java args for PlantUML
#
set(ORES_PLANTUML_JAR "/usr/share/plantuml/plantuml.jar")
set(ORES_JAVA_ARGS "-Djava.awt.headless=true")
list(APPEND ORES_JAVA_ARGS "-DPLANTUML_LIMIT_SIZE=131072")

#
# C++ test runner aggregates
#
add_custom_target(run_all_tests)
add_custom_target(rat)
add_dependencies(rat run_all_tests)

add_subdirectory(${CMAKE_SOURCE_DIR}/build)
add_subdirectory(${CMAKE_SOURCE_DIR}/assets)
add_subdirectory(${CMAKE_SOURCE_DIR}/projects)
add_subdirectory(${CMAKE_SOURCE_DIR}/doc/llm/skills/modeling)

# On Windows we disable vcpkg's per-target X_VCPKG_APPLOCAL_DEPS_INSTALL hook
# (set in CMakePresets.json) because it registers one install(CODE ...) block
# per installed executable. With ~80 installed targets each pulling the
# transitive Qt+OpenSSL+libpq+Boost closure, WiX accumulates more than 65,535
# files per cabinet (LGHT0306). The build-time VCPKG_APPLOCAL_DEPS (still ON
# by default) already populates publish/bin/ with every DLL and Qt plugin
# exactly once, so we install that directory in one shot — excluding our own
# binaries (installed individually via install(TARGETS)), test executables
# (not meant for the installer), and build byproducts.
if(WIN32)
    install(DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/
        DESTINATION bin
        USE_SOURCE_PERMISSIONS
        REGEX "/ores\\." EXCLUDE
        REGEX "\\.tests\\." EXCLUDE
        REGEX "\\.pdb$" EXCLUDE
        REGEX "\\.ilk$" EXCLUDE
        REGEX "\\.exp$" EXCLUDE
        REGEX "\\.lib$" EXCLUDE)
endif()
