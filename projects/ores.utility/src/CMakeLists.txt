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

# version.hpp: stable between builds — only contains version number macros
# and function declarations. Generated once at configure time via
# configure_file(); never touched again unless the version changes.
set(generated_include_dir "${CMAKE_CURRENT_BINARY_DIR}/include")
set(generated_version_dir "${generated_include_dir}/ores.utility/version")
set(version_hpp_in  "${CMAKE_SOURCE_DIR}/projects/ores.utility/include/ores.utility/version/version.hpp.in")
set(version_hpp_out "${generated_version_dir}/version.hpp")
file(MAKE_DIRECTORY "${generated_version_dir}")
configure_file("${version_hpp_in}" "${version_hpp_out}" @ONLY)

# version.cpp: stamped on every build with the current git hash + timestamp.
# copy_if_different in the script keeps its mtime stable when nothing has
# changed. Only this one TU is recompiled per build; all other consumers of
# version.hpp are unaffected.
set(version_cpp_in  "${CMAKE_SOURCE_DIR}/projects/ores.utility/src/version/version.cpp.in")
set(version_cpp_out "${CMAKE_CURRENT_BINARY_DIR}/version/version.cpp")
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/version")

add_custom_target(ores.utility.version ALL
    COMMAND "${CMAKE_COMMAND}"
        "-DSOURCE_DIR=${CMAKE_SOURCE_DIR}"
        "-DOreStudio_VERSION_MAJOR=${OreStudio_VERSION_MAJOR}"
        "-DOreStudio_VERSION_MINOR=${OreStudio_VERSION_MINOR}"
        "-DOreStudio_VERSION_PATCH=${OreStudio_VERSION_PATCH}"
        "-DINPUT_FILE=${version_cpp_in}"
        "-DOUTPUT_FILE=${version_cpp_out}"
        -P "${CMAKE_SOURCE_DIR}/cmake/generate_version.cmake"
    COMMENT "Stamping version.cpp"
    VERBATIM)

# Bootstrap: ensure version.cpp exists before the first build so the compiler
# can resolve the library sources list.
if(NOT EXISTS "${version_cpp_out}")
    execute_process(
        COMMAND "${CMAKE_COMMAND}"
            "-DSOURCE_DIR=${CMAKE_SOURCE_DIR}"
            "-DOreStudio_VERSION_MAJOR=${OreStudio_VERSION_MAJOR}"
            "-DOreStudio_VERSION_MINOR=${OreStudio_VERSION_MINOR}"
            "-DOreStudio_VERSION_PATCH=${OreStudio_VERSION_PATCH}"
            "-DINPUT_FILE=${version_cpp_in}"
            "-DOUTPUT_FILE=${version_cpp_out}"
            -P "${CMAKE_SOURCE_DIR}/cmake/generate_version.cmake"
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
endif()

set_source_files_properties("${version_cpp_out}" PROPERTIES GENERATED TRUE)
set(lib_files ${files} "${version_cpp_out}")
add_library(${lib_target_name} ${lib_files})
add_dependencies(${lib_target_name} ores.utility.version)

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
        Boost::iostreams
        Boost::program_options
    PRIVATE
        OpenSSL::Crypto
        faker-cxx::faker-cxx)
# Note: reflectcpp comes transitively through ores.platform.lib (PUBLIC)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
