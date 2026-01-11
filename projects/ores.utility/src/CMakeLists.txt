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
set(name "ores.utility")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files "")
file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

# Generate version.hpp to build directory to avoid polluting source tree
set(generated_include_dir "${CMAKE_CURRENT_BINARY_DIR}/include")
set(generated_version_dir "${generated_include_dir}/ores.utility/version")
configure_file(
    "${CMAKE_SOURCE_DIR}/projects/ores.utility/include/ores.utility/version/version.hpp.in"
    "${generated_version_dir}/version.hpp")

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})

set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

# Generated include dir first so version.hpp from build dir takes precedence
target_include_directories(${lib_target_name} PUBLIC
    ${generated_include_dir}
    ${CMAKE_SOURCE_DIR}/projects/ores.utility/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.platform.lib
        Boost::exception
        Boost::program_options
    PRIVATE
        OpenSSL::Crypto
        faker-cxx::faker-cxx)
# Note: reflectcpp comes transitively through ores.platform.lib (PUBLIC)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
