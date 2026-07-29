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
set(name "ores.marketdata.core")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "messaging/feed_binding_registrar.cpp"
    "messaging/market_observation_registrar.cpp"
    "messaging/publish_from_dq_handler.cpp"
    "messaging/registrar.cpp"
    "oresmd/oresmd_parser.cpp"
    "oresmd/oresmd_projections.cpp"
    "oresmd/oresmd_resolver.cpp"
    "presentation/feed_binding_history_field_mapper.cpp"
    "repository/feed_binding_entity.cpp"
    "repository/feed_binding_mapper.cpp"
    "repository/feed_binding_repository.cpp"
    "repository/market_fixing_entity.cpp"
    "repository/market_fixing_mapper.cpp"
    "repository/market_fixing_repository.cpp"
    "repository/market_fixings_repository.cpp"
    "repository/market_observation_entity.cpp"
    "repository/market_observation_mapper.cpp"
    "repository/market_observation_repository.cpp"
    "repository/market_observations_repository.cpp"
    "repository/market_series_entity.cpp"
    "repository/market_series_mapper.cpp"
    "repository/market_series_repository.cpp"
    "service/feed_binding_service.cpp"
    "service/import_service.cpp"
    "service/market_fixing_service.cpp"
    "service/market_observation_service.cpp"
    "service/market_series_service.cpp"
)

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_MARKETDATA_CORE_LIBRARY)
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

find_package(Boost REQUIRED COMPONENTS url)
find_package(magic_enum CONFIG REQUIRED)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.marketdata.api.lib
    PRIVATE
        ores.dq.api.lib
        ores.ore.core.lib
        ores.refdata.api.lib
        ores.service.lib
        ores.storage.lib
        ores.nats.lib
        ores.database.lib
        ores.utility.lib
        ores.security.lib
        sqlgen::sqlgen
        faker-cxx::faker-cxx
        magic_enum::magic_enum
        Boost::boost
        Boost::url)
# Note: reflectcpp comes transitively through ores.utility.lib -> ores.platform.lib (PUBLIC)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
