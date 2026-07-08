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
set(tests_binary_name ${name}.tests)
set(tests_target_name ${name}.tests)

set(ORES_QT_API_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)

set(files "")
file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

# parse_trade_instrument.cpp is compiled directly into the test target so that
# the tests can exercise the dispatch logic without linking against ores.qt.api.lib
# (which would pull in Qt and require a display environment). The swap-type
# parsing lives in a dedicated TU (parse_swap_instruments.cpp) that
# parse_trade_instrument.cpp dispatches into, so it must be compiled in too.
#
# NOTE: enumerating these TUs by hand is a known stop-gap — it goes stale
# whenever the parse implementation is split into another TU (which is how
# the swap TU came to be missing). Tracked for a proper fix (move the
# Qt-free parse logic into its own component) by story
# move_trade_parse_out_of_qt.
set(parse_src
    "${ORES_QT_API_DIR}/src/parse_trade_instrument.cpp"
    "${ORES_QT_API_DIR}/src/parse_swap_instruments.cpp")

# HierarchyModelBuilder.cpp is compiled directly into the test target for the
# same reason as parse_trade_instrument.cpp above: it only needs QStandardItemModel
# (Qt6::Gui), not the full ores.qt.api.lib (which pulls in Widgets and requires a
# display environment). Qt6::Gui-level classes such as QStandardItemModel do not
# need a QApplication/display to construct and populate, so this can run headless.
set(hierarchy_src
    "${ORES_QT_API_DIR}/src/HierarchyModelBuilder.cpp")

# OrgDocRenderer/TestScenarioResultsWriter/EnvironmentMetadata only need
# Qt6::Core (QString, QRegularExpression, QFile, QProcess, QDateTime) — same
# headless-friendly reasoning as parse_src/hierarchy_src above, so they're
# compiled in directly rather than pulling the full ores.qt.api.lib (Widgets,
# display environment).
set(orgmode_src
    "${ORES_QT_API_DIR}/src/OrgDocRenderer.cpp"
    "${ORES_QT_API_DIR}/src/TestScenarioResultsWriter.cpp"
    "${ORES_QT_API_DIR}/src/EnvironmentMetadata.cpp")

add_executable(${tests_target_name} ${files} ${parse_src} ${hierarchy_src} ${orgmode_src})
set_target_properties(${tests_target_name}
    PROPERTIES OUTPUT_NAME ${tests_binary_name})

target_include_directories(${tests_target_name}
    PRIVATE ${ORES_QT_API_DIR}/include)

target_link_libraries(${tests_target_name}
    PRIVATE
        ores.trading.api.lib
        ores.orgmode.lib
        ores.logging.lib
        ores.utility.lib
        ores.testing.lib
        Catch2::Catch2
        Qt6::Gui
        Qt6::Core
        ${CMAKE_THREAD_LIBS_INIT})

add_custom_target(test_${tests_target_name}
    COMMENT "Testing ${tests_target_name}" VERBATIM
    COMMAND ${ORES_TEST_ENV_CMD} $<TARGET_FILE:${tests_binary_name}> ${ORES_CATCH2_ARGS}
        -r xml::out=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${tests_target_name}.xml
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    DEPENDS ${tests_target_name})

add_dependencies(run_all_tests test_${tests_target_name})

add_test(NAME ${tests_target_name}
    COMMAND ${tests_binary_name} ${ORES_CATCH2_ARGS} -r xml::out=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${tests_target_name}.xml
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

set_tests_properties(${tests_target_name} PROPERTIES
    PROCESSORS 1
    ENVIRONMENT "${ORES_TEST_ENV}"
    ATTACHED_FILES_ON_FAIL "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${tests_target_name}.xml"
)

set_property(TEST ${tests_target_name} PROPERTY USES_XML_OUTPUT ON)
