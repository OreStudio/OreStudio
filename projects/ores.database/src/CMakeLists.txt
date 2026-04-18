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
set(name "ores.database")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files "")
file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/${name}/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        sqlgen::sqlgen
        ores.logging.lib
        ores.platform.lib
    PRIVATE
        ores.utility.lib)

# libpq on Windows is distributed as a static archive that depends on the
# PostgreSQL portability libraries libpgport and libpgcommon. vcpkg's
# libpq find-module wrapper only attaches them when the triplet linkage is
# "static"; the x64-windows dynamic triplet skips them and we get undefined
# references to pg_tolower/pg_vsnprintf/gettimeofday/... when linking any
# DLL that pulls libpq in transitively (e.g. via sqlgen). Attach them
# explicitly on Windows so the symbols resolve.
if(WIN32)
    find_library(ORES_PGPORT_LIBRARY
        NAMES pgport libpgport
        PATHS "${VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib"
        NO_DEFAULT_PATH)
    find_library(ORES_PGCOMMON_LIBRARY
        NAMES pgcommon libpgcommon
        PATHS "${VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib"
        NO_DEFAULT_PATH)
    if(ORES_PGPORT_LIBRARY AND ORES_PGCOMMON_LIBRARY)
        target_link_libraries(${lib_target_name}
            PUBLIC
                ${ORES_PGPORT_LIBRARY}
                ${ORES_PGCOMMON_LIBRARY}
                Secur32.lib
                Wldap32.lib)
    endif()
endif()

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
