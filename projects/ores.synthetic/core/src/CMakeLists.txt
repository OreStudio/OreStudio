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
set(name "ores.synthetic.core")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "generators/account_generator.cpp"
    "generators/catalog_generator.cpp"
    "generators/dataset_generator.cpp"
    "messaging/folder_registrar.cpp"
    "messaging/ir_curve_generation_config_registrar.cpp"
    "messaging/ir_curve_template_entry_registrar.cpp"
    "messaging/publish_from_dq_handler.cpp"
    "messaging/registrar.cpp"
    "messaging/yield_curve_process_type_registrar.cpp"
    "presentation/ir_curve_generation_config_history_field_mapper.cpp"
    "presentation/ir_curve_template_entry_history_field_mapper.cpp"
    "presentation/yield_curve_process_type_history_field_mapper.cpp"
    "repository/folder_entity.cpp"
    "repository/folder_mapper.cpp"
    "repository/folder_repository.cpp"
    "repository/fx_spot_generation_config_entity.cpp"
    "repository/fx_spot_generation_config_mapper.cpp"
    "repository/fx_spot_generation_config_repository.cpp"
    "repository/gmm_component_entity.cpp"
    "repository/gmm_component_mapper.cpp"
    "repository/gmm_component_repository.cpp"
    "repository/ir_curve_generation_config_entity.cpp"
    "repository/ir_curve_generation_config_mapper.cpp"
    "repository/ir_curve_generation_config_repository.cpp"
    "repository/ir_curve_template_entry_entity.cpp"
    "repository/ir_curve_template_entry_mapper.cpp"
    "repository/ir_curve_template_entry_repository.cpp"
    "repository/market_data_generation_config_entity.cpp"
    "repository/market_data_generation_config_mapper.cpp"
    "repository/market_data_generation_config_repository.cpp"
    "repository/yield_curve_process_type_entity.cpp"
    "repository/yield_curve_process_type_mapper.cpp"
    "repository/yield_curve_process_type_repository.cpp"
    "service/catalog_generator_service.cpp"
    "service/folder_service.cpp"
    "service/fx_spot_generation_config_service.cpp"
    "service/gmm_component_service.cpp"
    "service/ir_curve_generation_config_service.cpp"
    "service/ir_curve_template_entry_service.cpp"
    "service/market_data_generation_config_service.cpp"
    "service/organisation_generator_service.cpp"
    "service/organisation_publisher_service.cpp"
    "service/yield_curve_process_type_service.cpp"
)

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_SYNTHETIC_CORE_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name}
    PUBLIC
        ${CMAKE_SOURCE_DIR}/projects/ores.synthetic/core/include
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.synthetic.api.lib
        ores.nats.lib
        ores.dq.api.lib
        ores.iam.api.lib
        ores.refdata.core.lib
        ores.database.lib
    PRIVATE
        ores.service.lib
        ores.utility.lib
        ores.security.lib
        faker-cxx::faker-cxx
        Boost::boost)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
