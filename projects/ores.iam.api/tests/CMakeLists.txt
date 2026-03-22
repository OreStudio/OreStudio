# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
set(name "ores.iam.api")
set(tests_binary_name ${name}.tests)
set(tests_target_name ${name}.tests)

file(GLOB_RECURSE test_files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

add_executable(${tests_target_name} ${test_files})
set_target_properties(${tests_target_name} PROPERTIES OUTPUT_NAME ${tests_binary_name})

target_link_libraries(${tests_target_name}
    PRIVATE
        ores.iam.api.lib
        ores.utility.lib
        ores.testing.lib
        Catch2::Catch2)

set(command $<TARGET_FILE:${tests_target_name}> ${ORES_CATCH2_ARGS})
add_test(NAME ${tests_target_name} COMMAND ${command})
set_tests_properties(${tests_target_name} PROPERTIES LABELS "unit_tests")
