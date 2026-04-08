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
set(name "ores.qt.trading")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)
set(ORES_QT_TRADING_DIR ${CMAKE_SOURCE_DIR}/projects/ores.qt.trading)

# Qt code generation (MOC for Q_OBJECT, UIC for .ui forms)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)

file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
file(GLOB_RECURSE HEADERS "${ORES_QT_TRADING_DIR}/include/*.hpp")
# UI forms for AUTOUIC.
file(GLOB FORMS "${ORES_QT_TRADING_DIR}/ui/*.ui")

add_library(${lib_target_name} SHARED ${files} ${HEADERS} ${FORMS})

set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name}
    AUTOUIC_SEARCH_PATHS "${ORES_QT_TRADING_DIR}/ui;${ORES_QT_API_UI_DIR}"
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/plugins"
    INSTALL_RPATH "$ORIGIN/..:$ORIGIN"
    BUILD_RPATH "${CMAKE_LIBRARY_OUTPUT_DIRECTORY};${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/plugins")

target_include_directories(${lib_target_name} PUBLIC
    ${ORES_QT_TRADING_DIR}/include)
target_include_directories(${lib_target_name} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}/${lib_target_name}_autogen/include"
    # Transient: trading controllers include headers still in ores.qt or other plugins.
    # Remove as files migrate to their final projects.
    "${CMAKE_SOURCE_DIR}/projects/ores.qt/include"
    "${CMAKE_SOURCE_DIR}/projects/ores.qt.party/include"
    "${CMAKE_SOURCE_DIR}/projects/ores.qt.mktdata/include"
    "${CMAKE_SOURCE_DIR}/projects/ores.qt.compute/include")

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.qt.api.lib
        ores.qt.party.lib
        ores.qt.compute.lib
        ores.refdata.core.lib
        ores.trading.api.lib
        ores.ore.lib
        ores.marketdata.api.lib
        ores.storage.lib)

install(TARGETS ${lib_target_name}
    LIBRARY DESTINATION lib/plugins
    RUNTIME DESTINATION bin/plugins)
