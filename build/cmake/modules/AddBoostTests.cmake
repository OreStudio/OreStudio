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

# parameters for boost tests
set(boost_test_parameters "--log_level=error")

# if you want to ignore any tests, set this variable at the top-level.
set(boost_tests_ignore "")

#
# macro to create CTest tests for each boost unit tests
#
macro(add_boost_tests test_module)
    foreach(cpp_file ${ARGN})
        file(READ "${cpp_file}" contents)

        string(REGEX MATCHALL "BOOST_AUTO_TEST_SUITE(\\([A-Za-z_0-9]+\\))"
            found_suites ${contents})

        list(LENGTH found_suites total_suites)
        if (total_suites GREATER 1)
            message(FATAL_ERROR "Only one test suite per file is supported. File: ${cpp_file}")
        elseif (total_suites EQUAL 1)
            list (GET found_suites 0 test_suite)
            string(REGEX REPLACE "BOOST_AUTO_TEST_SUITE(\\([A-Za-z_0-9]+\\))"
                "\\1" test_suite ${test_suite})
            string(REPLACE "(" "" test_suite ${test_suite})
            string(REPLACE ")" "" test_suite ${test_suite})

            string(REGEX MATCHALL "BOOST_AUTO_TEST_CASE\\(([A-Za-z_0-9]+)\\)"
                found_tests ${contents})
            foreach(hit ${found_tests})
                string(REGEX REPLACE "BOOST_AUTO_TEST_CASE(\\([A-Za-z_0-9]+\\))"
                    "\\1" test_name ${hit})
                string(REPLACE "(" "" test_name ${test_name})
                string(REPLACE ")" "" test_name ${test_name})

                if (${test_name} IN_LIST boost_tests_ignore)
                    message(STATUS "Ignoring test: ${test_name}")
                else()
                    set(output_dir ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/)
                    add_test(${test_module}/${test_suite}/${test_name}
                        ${CMAKE_COMMAND} -E chdir ${output_dir}
                        ${output_dir}/${test_module} --run_test=${test_suite}/${test_name}
                        ${boost_test_parameters})
                endif()
            endforeach()
        endif()
    endforeach()
endmacro()
