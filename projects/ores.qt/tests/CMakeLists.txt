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
set(name "ores.qt")
set(tests_binary_name ${name}.tests)
set(tests_target_name ${name}.tests)

set(CMAKE_AUTOMOC ON)

set(files "")
file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

# Compile BadgeCache directly from the main source tree — ores.qt is an
# executable so there is no library target to link against.
# The header must also be listed so AUTOMOC finds Q_OBJECT and generates
# moc_BadgeCache.cpp (without it the signal implementations are undefined).
list(APPEND files "${CMAKE_SOURCE_DIR}/projects/ores.qt/src/BadgeCache.cpp")
list(APPEND files "${CMAKE_SOURCE_DIR}/projects/ores.qt/include/ores.qt/BadgeCache.hpp")

add_executable(${tests_target_name} ${files})
set_target_properties(${tests_target_name}
    PROPERTIES OUTPUT_NAME ${tests_binary_name})

target_include_directories(${tests_target_name} PRIVATE
    ${CMAKE_SOURCE_DIR}/projects/ores.qt/include)

target_link_libraries(${tests_target_name}
    PRIVATE
        ores.dq.api.lib
        ores.dq.lib
        ores.nats.lib
        ores.eventing.lib
        ores.iam.api.lib
        ores.logging.lib
        ores.utility.lib
        ores.testing.lib
        ores.openssl_cleanup
        Catch2::Catch2
        faker-cxx::faker-cxx
        Qt6::Core
        Qt6::Concurrent
        ${CMAKE_THREAD_LIBS_INIT})

add_custom_target(test_${tests_target_name}
    COMMENT "Testing ${tests_target_name}" VERBATIM
    COMMAND ${ORES_TEST_ENV_CMD} $<TARGET_FILE:${tests_binary_name}>
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
