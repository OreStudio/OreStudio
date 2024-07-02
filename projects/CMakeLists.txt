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

if (CMAKE_COMPILER_IS_GNUCXX OR CMAKE_COMPILER_IS_CLANGXX)
    set(flags "-gdwarf-4 -gstrict-dwarf")
    if(WITH_PROFILING)
        set(flags "${flags} -fprofile-abs-path")
        set(flags "${flags} -fprofile-arcs")
        set(flags "${flags} -ftest-coverage")
    endif()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${flags}")
    message(STATUS "C++ flags: ${CMAKE_CXX_FLAGS}")
endif()

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.console)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.ore)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.utility)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.qt)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/ores.ore.test)
