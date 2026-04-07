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
set(name "ores.qt.api")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)
set(ORES_QT_API_DIR ${CMAKE_SOURCE_DIR}/projects/ores.qt.api)

# Qt code generation (MOC for Q_OBJECT, UIC for .ui forms)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)

file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
file(GLOB_RECURSE HEADERS "${ORES_QT_API_DIR}/include/*.hpp")
# UI forms for AUTOUIC.
file(GLOB FORMS "${ORES_QT_API_DIR}/ui/*.ui")

add_library(${lib_target_name} SHARED ${files} ${HEADERS} ${FORMS})

# Define ORES_QT_API_LIBRARY when building this target so export.hpp
# expands to BOOST_SYMBOL_EXPORT instead of BOOST_SYMBOL_IMPORT.
target_compile_definitions(${lib_target_name} PRIVATE ORES_QT_API_LIBRARY)

set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name}
    AUTOUIC_SEARCH_PATHS ${ORES_QT_API_DIR}/ui
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${ORES_QT_API_DIR}/include
    # Expose AUTOUIC-generated ui_*.h headers to consumers.
    "${CMAKE_CURRENT_BINARY_DIR}/${lib_target_name}_autogen/include")

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.nats.lib
        ores.eventing.lib
        ores.iam.api.lib
        ores.refdata.api.lib
        ores.compute.api.lib
        ores.dq.api.lib
        ores.http.api.lib
        ores.assets.api.lib
        ores.logging.lib
        ores.utility.lib
        ores.platform.lib
        Qt6::Core
        Qt6::Gui
        Qt6::Widgets
        Qt6::Concurrent
        Qt6::Network
        Qt6::Svg)

install(TARGETS ${lib_target_name}
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin)  # .dll goes to bin/ on Windows
