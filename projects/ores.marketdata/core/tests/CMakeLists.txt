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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
set(name "ores.marketdata.core")
set(lib_target_name ${name}.lib)
set(tests_binary_name ${name}.tests)
set(tests_target_name ${name}.tests)

include(${CMAKE_CURRENT_SOURCE_DIR}/component_files.cmake)

add_executable(${tests_target_name} ${files})
set_target_properties(${tests_target_name}
    PROPERTIES OUTPUT_NAME ${tests_binary_name})

target_link_libraries(${tests_target_name}
    PRIVATE
        ${lib_target_name}
        ores.utility.lib
        ores.testing.lib
        ores.logging.lib
        ores.eventing.api.lib
        ores.eventing.core.lib
        Catch2::Catch2
        faker-cxx::faker-cxx
        ores.openssl_cleanup
        Threads::Threads)

add_custom_target(test_${tests_target_name}
    COMMENT "Testing ${tests_target_name}" VERBATIM
    COMMAND ${ORES_TEST_ENV_CMD} $<TARGET_FILE:${tests_binary_name}> ${ORES_CATCH2_ARGS}
        -r xml::out=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${tests_target_name}.xml
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    DEPENDS ${tests_target_name})

add_dependencies(run_all_tests test_${tests_target_name})

# The whole-corpus import walk is its own ctest entry, so it and the rest of the
# suite each have the full per-test time limit CI applies (600 s on Linux):
# together they ran 592 s. Both run by default.
set(corpus_tests_name ${name}.corpus.tests)
add_test(NAME ${tests_target_name}
    COMMAND ${tests_binary_name} ${ORES_CATCH2_ARGS} "~[corpus-full]"
        -r xml::out=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${tests_target_name}.xml
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
add_test(NAME ${corpus_tests_name}
    COMMAND ${tests_binary_name} ${ORES_CATCH2_ARGS} "[corpus-full]"
        -r xml::out=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${corpus_tests_name}.xml
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

# The whole-corpus walk writes every market payload under external/ore/examples
# and keeps the database busy throughout. Running it beside another suite slows
# that suite's database reads enough to expose a race of its own -- a reporting
# test asserts its version history is unchanged immediately after a delete,
# while the service's notification re-drive can still be writing -- so both
# entries run alone rather than beside anything.
foreach(entry ${tests_target_name} ${corpus_tests_name})
    set_tests_properties(${entry} PROPERTIES
        PROCESSORS 1
        RUN_SERIAL TRUE
        ENVIRONMENT "${ORES_TEST_ENV}"
        ATTACHED_FILES_ON_FAIL "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${entry}.xml"
    )
    set_property(TEST ${entry} PROPERTY USES_XML_OUTPUT ON)
endforeach()
