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
file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/test_data"
    DESTINATION "${ORES_PUBLISH_DIRECTORY}/")

set(name "ores.risk")
set(lib_target_name ${name}.lib)
set(tests_binary_name ${name}.tests)
set(tests_target_name ${name}.tests)

# Catch2 test files
set(files main.cpp orexml_currency_config_tests.cpp)

add_executable(${tests_target_name} ${files})
set_target_properties(${tests_target_name}
    PROPERTIES OUTPUT_NAME ${tests_binary_name})

target_include_directories(${tests_binary_name}
    PRIVATE ${CMAKE_SOURCE_DIR}/projects/${name}/include
    PRIVATE ${CMAKE_SOURCE_DIR}/projects/)

target_link_libraries(${tests_target_name}
    PRIVATE
        ${lib_target_name}
        ores.utility.lib
        Catch2::Catch2
        Boost::unit_test_framework
        ${CMAKE_THREAD_LIBS_INIT})

add_custom_target(run_${tests_target_name}
    COMMENT "Running ${tests_target_name}" VERBATIM
    COMMAND $<TARGET_FILE:${tests_binary_name}>
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    DEPENDS ${tests_target_name})

add_dependencies(run_all_tests run_${tests_target_name})

add_catch2_tests(${tests_target_name})
