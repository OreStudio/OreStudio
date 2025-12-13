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
set(exe_binary_name ${name})
set(ORES_QT_DIR ${CMAKE_SOURCE_DIR}/projects/ores.qt)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTORCC ON)

file(GLOB_RECURSE HEADERS "${ORES_QT_DIR}/*.hpp")
file(GLOB SOURCES "*.cpp")
file(GLOB FORMS "${ORES_QT_DIR}/ui/*.ui")
file(GLOB RESOURCES "${ORES_QT_DIR}/resources/*.qrc")

qt_add_executable(${exe_binary_name} ${SOURCES} ${HEADERS} ${FORMS} ${RESOURCES})

find_package(Boost REQUIRED COMPONENTS process)

target_link_libraries(${exe_binary_name} PRIVATE
    ores.risk.lib
    ores.comms.lib
    ores.accounts.lib
    ores.eventing.lib
    ores.utility.lib
    Boost::process
    Qt6::Core
    Qt6::Gui
    Qt6::Widgets
    Qt6::Concurrent)

set_target_properties(${exe_binary_name} PROPERTIES
    AUTORCC_RESOURCE_DIRS "${ORES_QT_DIR}/resources;${ORES_QT_DIR}/resources/icons"
    AUTOUIC_SEARCH_PATHS ${ORES_QT_DIR}/ui)

target_include_directories(${name} PRIVATE ${ORES_QT_DIR}/include)

# Copy main assets from assets/images directory
file(GLOB ORES_MAIN_ASSETS "${CMAKE_SOURCE_DIR}/assets/images/*.png")
foreach(in_file IN LISTS ORES_MAIN_ASSETS)
    get_filename_component(out_file ${in_file} NAME)
    file(COPY_FILE ${in_file} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${out_file})
endforeach()

install(TARGETS ores.qt RUNTIME DESTINATION bin)
