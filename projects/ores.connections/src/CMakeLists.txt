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
set(name "ores.connections")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "domain/environment_tag_json_io.cpp"
    "domain/environment_tag_table_io.cpp"
    "domain/folder_json_io.cpp"
    "domain/folder_table_io.cpp"
    "domain/tag_json_io.cpp"
    "domain/tag_table_io.cpp"
    "generators/connection_generator.cpp"
    "generators/environment_generator.cpp"
    "generators/environment_tag_generator.cpp"
    "generators/folder_generator.cpp"
    "generators/tag_generator.cpp"
    "repository/connection_mapper.cpp"
    "repository/connection_repository.cpp"
    "repository/connection_tag_mapper.cpp"
    "repository/connection_tag_repository.cpp"
    "repository/environment_mapper.cpp"
    "repository/environment_repository.cpp"
    "repository/environment_tag_mapper.cpp"
    "repository/environment_tag_repository.cpp"
    "repository/folder_mapper.cpp"
    "repository/folder_repository.cpp"
    "repository/recent_party_mapper.cpp"
    "repository/recent_party_repository.cpp"
    "repository/sqlite_context.cpp"
    "repository/tag_mapper.cpp"
    "repository/tag_repository.cpp"
    "service/connection_manager.cpp"
)

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_CONNECTIONS_LIBRARY)
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
        ores.utility.lib
        ores.security.lib
    PRIVATE
        sqlgen::sqlgen
        faker-cxx::faker-cxx
        libfort::fort
        Boost::boost)


target_link_libraries(${lib_target_name} PRIVATE ores.rfl.pch)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
