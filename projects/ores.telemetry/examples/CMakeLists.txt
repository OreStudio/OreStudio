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

# Example for database logging functionality
set(example_name database_sink_example)
set(example_target ${example_name}.exe)

add_executable(${example_target}
    ${CMAKE_CURRENT_SOURCE_DIR}/${example_name}.cpp)

set_target_properties(${example_target} PROPERTIES
    OUTPUT_NAME ${example_name})

target_link_libraries(${example_target}
    PRIVATE
        ores.telemetry.lib
        ores.logging.lib
        ores.utility.lib
        ores.platform.lib
        Boost::log
        Boost::program_options
        ${CMAKE_THREAD_LIBS_INIT})

install(TARGETS ${example_target} RUNTIME DESTINATION examples)