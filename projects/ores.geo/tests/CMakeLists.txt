# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
set(name "ores.geo")
set(lib_target_name ${name}.lib)
set(tests_binary_name ${name}.tests)
set(tests_target_name ${name}.tests)

set(files "")
file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

add_executable(${tests_target_name} ${files})
set_target_properties(${tests_target_name}
    PROPERTIES OUTPUT_NAME ${tests_binary_name})

target_link_libraries(${tests_target_name}
    PRIVATE
        ${lib_target_name}
        ores.telemetry.lib
        ores.testing.lib
        Catch2::Catch2
        faker-cxx::faker-cxx
        ${CMAKE_THREAD_LIBS_INIT})

add_custom_target(test_${tests_target_name}
    COMMENT "Testing ${tests_target_name}" VERBATIM
    COMMAND $<TARGET_FILE:${tests_binary_name}>
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    DEPENDS ${tests_target_name})

add_dependencies(run_all_tests test_${tests_target_name})

add_test(NAME ${tests_target_name}
    COMMAND ${tests_binary_name} ${ORES_CATCH2_ARGS} -o ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${tests_target_name}.xml
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

set_tests_properties(${tests_target_name} PROPERTIES
    PROCESSORS 1
    ENVIRONMENT "${ORES_TEST_ENV}"
    ATTACHED_FILES_ON_FAIL "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${tests_target_name}.xml"
)

set_property(TEST ${tests_target_name} PROPERTY USES_XML_OUTPUT ON)
