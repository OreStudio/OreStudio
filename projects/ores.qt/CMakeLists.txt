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
    resources.qrc
    main.cpp
    MainWindow.cpp
    MainWindow.ui
    SplashScreen.cpp
    MdiAreaWithBackground.cpp
    DetachableMdiSubWindow.hpp
    DetachableMdiSubWindow.cpp
    CurrencyMdiWindow.cpp
    CurrencyEditDialog.cpp
    CurrencyEditDialog.ui
    ClientCurrencyModel.cpp
    MainTabWidget.cpp
    CurrencyTabPage.cpp
    LoginDialog.cpp
    CurrencyItemDelegate.cpp
    CurrencyDetailPanel.ui
    CurrencyDetailPanel.cpp
    CurrencyHistoryDialog.ui
    CurrencyHistoryDialog.cpp
    CurrencyHistoryMdiWindow.hpp
    CurrencyHistoryMdiWindow.cpp
    MessageBoxHelper.cpp
    AboutDialog.cpp
    AboutDialog.ui
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

# Copy main assets from assets/images directory
file(GLOB ORES_MAIN_ASSETS "${CMAKE_SOURCE_DIR}/assets/images/*.png")
foreach(in_file IN LISTS ORES_MAIN_ASSETS)
    get_filename_component(out_file ${in_file} NAME)
    file(COPY_FILE ${in_file} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${out_file})
endforeach()

target_include_directories(ores.qt
    PRIVATE ${CMAKE_SOURCE_DIR}/projects)

set(name ores.qt)
set(diagram_target generate_${name}_diagram)
add_custom_target(${diagram_target}
    COMMENT "Generating PlantUML diagram for ${name}" VERBATIM
    COMMAND java -Djava.awt.headless=true -DPLANTUML_SECURITY_PROFILE=UNSECURE -DPLANTUML_LIMIT_SIZE=65535 -jar /usr/share/plantuml/plantuml.jar ${name}.puml
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/modeling)
add_dependencies(make_all_diagrams ${diagram_target})

add_custom_target(run_ores_qt
    COMMENT "Running ores.qt" VERBATIM
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    COMMAND ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ores.qt)

install(TARGETS ores.qt RUNTIME DESTINATION bin)
