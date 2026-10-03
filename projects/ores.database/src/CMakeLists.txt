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
set(name "ores.database")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

include(${CMAKE_CURRENT_SOURCE_DIR}/component_files.cmake)

set(lib_files ${files})
# The fingerprint of the SQL the database is built from, compiled in so that
# context_factory can refuse a database built from other SQL. It is recomputed
# whenever an SQL file changes; a new SQL file triggers a reconfigure.
set(sql_dir "${CMAKE_SOURCE_DIR}/projects/ores.sql")
set(fingerprint_script "${sql_dir}/schema_fingerprint.cmake")
file(GLOB_RECURSE fingerprint_globbed CONFIGURE_DEPENDS
    "${sql_dir}/create/*.sql" "${sql_dir}/populate/*.sql" "${sql_dir}/instance/*.sql")
file(GLOB fingerprint_top CONFIGURE_DEPENDS "${sql_dir}/*.sql")
set(fingerprint_dir "${CMAKE_CURRENT_BINARY_DIR}/generated")
set(fingerprint_header "${fingerprint_dir}/ores.database/schema_fingerprint.hpp")
add_custom_command(
    OUTPUT "${fingerprint_header}"
    COMMAND ${CMAKE_COMMAND} -DSQL_DIR=${sql_dir} -DOUTPUT=${fingerprint_header}
        -P ${fingerprint_script}
    DEPENDS ${fingerprint_globbed} ${fingerprint_top} ${fingerprint_script}
    COMMENT "Fingerprinting the SQL schema"
    VERBATIM)

add_library(${lib_target_name} ${lib_files} ${fingerprint_header})
target_include_directories(${lib_target_name} PRIVATE ${fingerprint_dir})
target_compile_definitions(${lib_target_name} PRIVATE ORES_DATABASE_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../include)

target_link_libraries(${lib_target_name}
    PUBLIC
        sqlgen::sqlgen
        ores.logging.lib
        ores.platform.lib
    PRIVATE
        ores.utility.lib)


target_link_libraries(${lib_target_name} PRIVATE ores.rfl.pch)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
