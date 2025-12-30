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
set(name "ores.wt")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files "")
file(GLOB_RECURSE files RELATIVE
    "${CMAKE_CURRENT_SOURCE_DIR}/"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")

set(lib_files ${files})
list(FILTER lib_files EXCLUDE REGEX "main.cpp")

# Use STATIC library to avoid symbol visibility issues with vcpkg static dependencies
add_library(${lib_target_name} STATIC ${lib_files})
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/${name}/include)

# Find libharu before wt, as wt's PDF support depends on it.
# vcpkg's libharu provides unofficial::libharu::haru but wt expects HPDF::HPDF,
# so we create an alias to bridge the gap.
find_package(unofficial-libharu CONFIG REQUIRED)
if(NOT TARGET HPDF::HPDF)
    add_library(HPDF::HPDF ALIAS unofficial::libharu::haru)
endif()
find_package(wt CONFIG REQUIRED)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.database.lib
        ores.iam.lib
        ores.risk.lib
        ores.variability.lib
        ores.telemetry.lib
        ores.utility.lib
        Wt::Wt
        Wt::HTTP
    PRIVATE
        Boost::boost
        Boost::program_options)

install(TARGETS ${lib_target_name} ARCHIVE DESTINATION lib)

set(exe_binary_name ${name})
set(exe_target_name ${name}.exe)

set(exe_files ${files})
list(FILTER exe_files INCLUDE REGEX "main.cpp")

# Find Wt's transitive dependencies for explicit linking
find_package(Freetype REQUIRED)
find_package(harfbuzz CONFIG REQUIRED)
find_package(EXPAT REQUIRED)
find_package(PkgConfig REQUIRED)
pkg_check_modules(PANGO REQUIRED IMPORTED_TARGET pango)
pkg_check_modules(PANGOFT2 REQUIRED IMPORTED_TARGET pangoft2)
pkg_check_modules(GLIB2 REQUIRED IMPORTED_TARGET glib-2.0)
pkg_check_modules(GOBJECT REQUIRED IMPORTED_TARGET gobject-2.0)
pkg_check_modules(FRIBIDI REQUIRED IMPORTED_TARGET fribidi)
pkg_check_modules(FONTCONFIG REQUIRED IMPORTED_TARGET fontconfig)
pkg_check_modules(LIBFFI REQUIRED IMPORTED_TARGET libffi)

add_executable(${exe_target_name} ${exe_files})
set_target_properties(${exe_target_name} PROPERTIES
    OUTPUT_NAME ${exe_binary_name})

target_include_directories(${exe_target_name}
    PRIVATE ${CMAKE_SOURCE_DIR}/projects)

target_link_libraries(${exe_target_name}
    PRIVATE
        ${lib_target_name}
        Wt::Wt
        Wt::HTTP
        ${CMAKE_THREAD_LIBS_INIT}
        # Explicit Wt transitive dependencies (vcpkg linking issue)
        PkgConfig::PANGOFT2
        PkgConfig::PANGO
        PkgConfig::FONTCONFIG
        Freetype::Freetype
        harfbuzz::harfbuzz
        EXPAT::EXPAT
        PkgConfig::GOBJECT
        PkgConfig::GLIB2
        PkgConfig::FRIBIDI
        PkgConfig::LIBFFI)

install(TARGETS ${exe_target_name} RUNTIME DESTINATION bin)
