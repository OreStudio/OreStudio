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

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTORCC ON)

add_executable(ores.qt
    main.cpp
    MainWindow.cpp
    MainWindow.ui
    SplashScreen.cpp
    ClientCurrencyModel.cpp
    MainTabWidget.cpp
    CurrencyTabPage.cpp
    LoginDialog.cpp
)

target_link_libraries(ores.qt PRIVATE
    ores.risk.lib
    ores.comms.lib
    ores.accounts.lib
    ores.utility.lib
    Qt6::Core
    Qt6::Gui
    Qt6::Widgets
    Qt6::Concurrent
)

file(GLOB ORES_QT_ASSETS "*.png")
foreach(in_file IN LISTS ORES_QT_ASSETS)
    get_filename_component(out_file ${in_file} NAME)
    file(COPY_FILE ${in_file} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${out_file})
endforeach()

file(GLOB ORES_QT_ASSETS "*.svg")
foreach(in_file IN LISTS ORES_QT_ASSETS)
    get_filename_component(out_file ${in_file} NAME)
    file(COPY_FILE ${in_file} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${out_file})
endforeach()


target_include_directories(ores.qt
    PRIVATE ${CMAKE_SOURCE_DIR}/projects)

add_custom_target(run_ores_qt
    COMMENT "Running ores.qt" VERBATIM
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/projects/ores.qt
    COMMAND ./ores.qt)

set(X_VCPKG_APPLOCAL_DEPS_INSTALL 1)
install(TARGETS ores.qt RUNTIME DESTINATION bin)
