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
set(name "ores.mq")
set(lib_name "${name}.lib")
set(ORES_MQ_DIR ${CMAKE_SOURCE_DIR}/projects/ores.mq)

file(GLOB_RECURSE HEADERS "${ORES_MQ_DIR}/include/*.hpp")

add_library(${lib_name} INTERFACE ${HEADERS})
target_include_directories(${lib_name} INTERFACE ${ORES_MQ_DIR}/include)
add_library(${name}::lib ALIAS ${lib_name})
