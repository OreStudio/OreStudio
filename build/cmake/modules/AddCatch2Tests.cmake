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

# Include Catch2's test discovery module
include(${Catch2_SOURCE_DIR}/extras/Catch.cmake OPTIONAL)

# If the above doesn't work, try the CMake modules path
if(NOT COMMAND catch_discover_tests)
    list(APPEND CMAKE_MODULE_PATH ${Catch2_SOURCE_DIR}/extras)
    include(Catch OPTIONAL)
endif()

# parameters for catch2 tests
set(catch2_test_extra_args "")

# if you want to ignore any tests, set this variable at the top-level.
set(catch2_tests_ignore "")

#
# macro to create CTest tests for Catch2 tests using automatic discovery
#
macro(add_catch2_tests test_target)
    if(COMMAND catch_discover_tests)
        # Use Catch2's built-in test discovery
        catch_discover_tests(${test_target}
            WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            EXTRA_ARGS ${catch2_test_extra_args}
        )
    else()
        message(WARNING "catch_discover_tests not available, skipping test discovery for ${test_target}")
    endif()
endmacro()
