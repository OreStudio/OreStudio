# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
set(name "ores.shell.workflow")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)
set(ORES_SHELL_WORKFLOW_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)
include(${CMAKE_CURRENT_SOURCE_DIR}/component_files.cmake)
add_library(${lib_target_name} STATIC ${files} ${HEADERS})
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
target_include_directories(${lib_target_name} PUBLIC
    ${ORES_SHELL_WORKFLOW_DIR}/include)
find_package(cli CONFIG REQUIRED)
target_link_libraries(${lib_target_name}
    PUBLIC
        ores.shell.api.lib
        ores.nats.lib
        ores.logging.lib
    PRIVATE
        ores.workflow.api.lib
        ores.platform.lib
        ores.utility.lib
        cli::cli)
if(NOT WIN32)
    install(TARGETS ${lib_target_name} ARCHIVE DESTINATION lib)
endif()
