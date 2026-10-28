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
set(name "ores.trading.core")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

include(${CMAKE_CURRENT_SOURCE_DIR}/component_files.cmake)

set(lib_files ${files})
if(WIN32)
    add_library(${lib_target_name} STATIC ${lib_files})
else()
    add_library(${lib_target_name} ${lib_files})
endif()
target_compile_definitions(${lib_target_name} PRIVATE ORES_TRADING_CORE_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
if(NOT WIN32)
    set_target_properties(${lib_target_name}
        PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})
endif()

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.trading.api.lib
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
        ores.trading.api.lib
        ores.nats.lib
        ores.diff.lib
        ores.refdata.api.lib
    PRIVATE
        ores.service.lib
        ores.storage.core.lib
        ores.database.lib
        ores.dq.api.lib
        ores.inbox.api.lib
        ores.inbox.core.lib
        ores.history.core.lib
        ores.diff.lib
        ores.utility.lib
        ores.security.lib
        faker-cxx::faker-cxx
        libfort::fort
        Boost::boost)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
