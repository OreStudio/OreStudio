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
# AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
# Template: cmake_component_core_src.mustache
# To modify, update the template and regenerate.
set(name "ores.ore.core")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

include(${CMAKE_CURRENT_SOURCE_DIR}/component_files.cmake)

set(lib_files ${files})
set(domain_cpp_warnings "")
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    list(APPEND domain_cpp_warnings "-Wno-unused-const-variable")
    if(WIN32)
        list(APPEND domain_cpp_warnings "-Wno-unused-function")
    endif()
    if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
        list(APPEND domain_cpp_warnings
            "-Wno-implicit-fallthrough"
            "-Wno-unused-but-set-variable")
    endif()
elseif(MSVC)
    list(APPEND domain_cpp_warnings "/wd4505")
endif()
set_source_files_properties(
    ${CMAKE_CURRENT_SOURCE_DIR}/domain/domain.cpp
    PROPERTIES COMPILE_OPTIONS "${domain_cpp_warnings}")
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_ORE_CORE_LIBRARY)
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
        ores.ore.api.lib
        ores.nats.lib
    PRIVATE
        ores.service.lib
        ores.platform.lib
        ores.utility.lib
        ores.security.lib
        faker-cxx::faker-cxx
        libfort::fort)

# A component declares here what the profile cannot know: the libraries it
# needs beyond the baseline above, and the find_package calls they bring.
# Appending to the target rather than replacing the profile keeps the
# baseline honest for every other component.

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.analytics.api.lib
        ores.ore.api.lib
        ores.database.lib
        ores.refdata.api.lib
        ores.reporting.api.lib
        ores.trading.api.lib
    PRIVATE
        ores.analytics.core.lib
        ores.refdata.core.lib
        ores.reporting.core.lib
        ores.dq.api.lib
        ores.history.core.lib
        ores.logging.lib
        ores.platform.lib
        ores.utility.lib
        Boost::boost)

if(MSVC)
    target_compile_options(${lib_target_name} PUBLIC /bigobj)
endif()
install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
