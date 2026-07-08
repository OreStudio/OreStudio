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
set(name "ores.qt")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)
set(ORES_QT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)

# Qt code generation (MOC for Q_OBJECT, UIC for .ui forms, RCC for resources)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTORCC ON)

set(files "")
file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
file(GLOB_RECURSE HEADERS "${ORES_QT_DIR}/include/*.hpp")
# UI forms for AUTOUIC.
file(GLOB FORMS "${ORES_QT_DIR}/ui/*.ui")

set(lib_files ${files})
list(FILTER lib_files EXCLUDE REGEX "main.cpp")

add_library(${lib_target_name} STATIC ${lib_files} ${HEADERS} ${FORMS})
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name}
    AUTOUIC_SEARCH_PATHS "${ORES_QT_DIR}/ui;${ORES_QT_API_UI_DIR}")
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${ORES_QT_DIR}/include)
# AUTOUIC headers are only consumed by .cpp files in ores.qt itself —
# MainWindow and AboutDialog hold their Ui::* pimpl-style (forward-declared
# in the .hpp, included in the .cpp). Keeping this dir PRIVATE stops stale
# ui_*.h copies from leaking to plugin libs (and previously shadowing the
# fresh ones at their own autogen dirs).
target_include_directories(${lib_target_name} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}/${lib_target_name}_autogen/include")
target_link_libraries(${lib_target_name}
    PUBLIC
        ores.qt.api.lib          # shared framework: ClientManager, EntityController,
                                 # IPlugin, PluginRegistry, utilities, caches — pulls Qt6 core modules
        # Domain plugins (ores.qt.admin, .compute, .refdata, .party, .mktdata, .trading)
        # are loaded at runtime via QPluginLoader — no compile-time link needed.
        ores.ore.core.lib
        ores.ore.api.lib
        ores.storage.lib
        ores.marketdata.api.lib
        ores.http.api.lib
        ores.refdata.core.lib
        ores.analytics.api.lib
        ores.trading.api.lib
        ores.scheduler.api.lib
        ores.reporting.api.lib
        ores.shell.lib
        ores.dq.api.lib
        ores.synthetic.api.lib
        ores.assets.api.lib
        ores.workflow.core.lib
        ores.connections.lib
        ores.variability.api.lib
        ores.telemetry.core.lib
        ores.controller.api.lib
        ores.security.lib
        ores.qt.workflow.lib
        Qt6::Charts
        Qt6::Help)
# Note: ores.nats, ores.eventing.api, ores.iam.api, ores.compute.api, ores.utility,
# ores.platform, and Qt6 core modules come transitively from ores.qt.api.lib (PUBLIC)
# Note: Boost::process comes transitively through ores.telemetry.core.lib (PUBLIC)

# ores.qt is an application-internal static library. It is linked statically
# into ores.qt.exe and is not useful as a redistributable artifact. On Windows
# debug builds the .lib exceeds WiX 3.x's 2GB per-file limit (LGHT0263).
if(NOT WIN32)
    install(TARGETS ${lib_target_name} ARCHIVE DESTINATION lib)
endif()

set(exe_binary_name ${name})
set(exe_target_name ${name}.exe)

set(exe_files ${files})
list(FILTER exe_files INCLUDE REGEX "main.cpp")
# Resources (.qrc) are embedded in the executable only.
file(GLOB RESOURCES "${ORES_QT_DIR}/resources/*.qrc")

qt_add_executable(${exe_target_name} ${exe_files} ${RESOURCES})
set_target_properties(${exe_target_name} PROPERTIES
    OUTPUT_NAME ${exe_binary_name}
    AUTORCC_RESOURCE_DIRS "${ORES_QT_DIR}/resources;${ORES_QT_DIR}/resources/icons")

target_link_libraries(${exe_target_name}
    PRIVATE
        ${lib_target_name}
        ores.openssl_cleanup
        ${CMAKE_THREAD_LIBS_INIT})

# Export all symbols from the exe so QPluginLoader-loaded plugins can resolve
# symbols from ores.qt.lib (e.g. SessionAuditDialog, host-only widgets).
if(NOT WIN32)
    target_link_options(${exe_target_name} PRIVATE -rdynamic)
endif()

# Domain plugins are loaded at runtime via QPluginLoader, not linked — so
# CMake's own dependency graph has no reason to know ores.qt.exe needs them
# rebuilt. Without this, a change to ores.qt.api.lib's ABI can leave a plugin
# .so compiled against the old layout sitting next to a freshly-linked exe:
# QPluginLoader loads it anyway (same .so name, no version check), and it
# segfaults on first virtual call into the mismatched vtable — reproduced
# twice while touching ores.qt.api during vintage-guard/price_source work.
# Force every plugin to always be rebuilt alongside the exe so this can't
# happen from an ordinary `cmake --build --target ores.qt.exe`.
add_dependencies(${exe_target_name}
    ores.qt.admin.lib
    ores.qt.analytics.lib
    ores.qt.compute.lib
    ores.qt.data_transfer.lib
    ores.qt.marketdata.lib
    ores.qt.mktdata.lib
    ores.qt.party.lib
    ores.qt.refdata.lib
    ores.qt.scheduler.lib
    ores.qt.synthetic.lib
    ores.qt.trading.lib
    ores.qt.workspace.lib)

# Copy main assets from assets/images directory
file(MAKE_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
file(GLOB ORES_MAIN_ASSETS "${CMAKE_SOURCE_DIR}/assets/images/*.png")
foreach(in_file IN LISTS ORES_MAIN_ASSETS)
    get_filename_component(out_file ${in_file} NAME)
    file(COPY_FILE ${in_file} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${out_file})
endforeach()

install(TARGETS ${exe_target_name} RUNTIME DESTINATION bin)

# QHelpEngine requires the Qt SQLite driver to create its .qhc collection.
# Deploy it next to the binary so Qt's plugin loader finds it in sqldrivers/.
find_file(ORES_QT_SQLITE_PLUGIN
    NAMES libqsqlite.so qsqlite.dll
    HINTS "${Qt6_DIR}/../../../plugins/sqldrivers"
    PATHS
        "/usr/lib/x86_64-linux-gnu/qt6/plugins/sqldrivers"
        "/usr/lib/aarch64-linux-gnu/qt6/plugins/sqldrivers"
        "/usr/lib/qt6/plugins/sqldrivers"
        "/opt/homebrew/opt/qt/plugins/sqldrivers"
    NO_DEFAULT_PATH)
if(ORES_QT_SQLITE_PLUGIN)
    add_custom_command(TARGET ${exe_target_name} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory
            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/sqldrivers
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${ORES_QT_SQLITE_PLUGIN}
            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/sqldrivers/
        COMMENT "Deploying Qt SQLite plugin for QHelpEngine"
        VERBATIM)
else()
    message(WARNING "Qt SQLite plugin not found; QHelpEngine will not work")
endif()

# Deploy the pre-built user manual help collection alongside the executable.
# The .qch is source-controlled in resources/help/ and copied here at build
# time so every developer gets it without running deploy_help_qch.
add_custom_command(TARGET ${exe_target_name} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory
        ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/help
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${CMAKE_CURRENT_SOURCE_DIR}/../resources/help/user_manual.qch
        ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/help/
    COMMENT "Deploying user_manual.qch"
    VERBATIM)
