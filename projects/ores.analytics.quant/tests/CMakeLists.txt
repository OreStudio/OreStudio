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
set(name "ores.analytics.quant")
set(lib_target_name ${name}.lib)
set(tests_binary_name ${name}.tests)
set(tests_target_name ${name}.tests)

set(files
    "arithmetic_gaussian_mixture_model_process_tests.cpp"
    "business_day_calendar_set_tests.cpp"
    "calendar_rule_engine_tests.cpp"
    "cox_ingersoll_ross_process_tests.cpp"
    "curve_instrument_pricer_tests.cpp"
    "gaussian_mixture_model_process_tests.cpp"
    "hull_white_process_tests.cpp"
    "main.cpp"
    "ornstein_uhlenbeck_process_tests.cpp"
    "process_factory_tests.cpp"
    "process_parameter_validation_tests.cpp"
    "quantlib_calendar_rulesets_tests.cpp"
    "rate_delta_tracker_tests.cpp"
    "rate_engine_tests.cpp"
    "rate_reciprocator_tests.cpp"
    "risk_recentering_tests.cpp"
    "topology_builder_tests.cpp"
    "vasicek_process_tests.cpp"
)

add_executable(${tests_target_name} ${files})
set_target_properties(${tests_target_name}
    PROPERTIES OUTPUT_NAME ${tests_binary_name})

# Deliberately no ores.testing/database_lifecycle_listener -- this component
# has no database, so its tests need no DB-backed test fixture.
target_link_libraries(${tests_target_name}
    PRIVATE
        ${lib_target_name}
        Catch2::Catch2
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
