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
set(name "ores.history.api")
set(lib_target_name ${name}.lib)

# Header-only: history_protocol.hpp is plain structs, version_builder.hpp is
# a fully-inline template. No .cpp to compile.
add_library(${lib_target_name} INTERFACE)

target_include_directories(${lib_target_name} INTERFACE
    ${CMAKE_SOURCE_DIR}/projects/ores.history/api/include)

target_link_libraries(${lib_target_name}
    INTERFACE
        ores.diff.lib)

install(TARGETS ${lib_target_name})
