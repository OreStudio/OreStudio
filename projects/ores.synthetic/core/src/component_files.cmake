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

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/generators/account_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/generators/catalog_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/generators/dataset_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/folder_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/folder_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/fx_spot_generation_config_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/gmm_component_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/ir_curve_generation_config_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/ir_curve_generation_config_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/ir_curve_template_entry_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/ir_curve_template_entry_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/market_data_generation_config_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/organisation_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/publish_from_dq_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/yield_curve_process_type_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/messaging/yield_curve_process_type_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/ores.synthetic.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/presentation/ir_curve_generation_config_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/presentation/ir_curve_template_entry_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/presentation/yield_curve_process_type_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/folder_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/folder_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/folder_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/fx_spot_generation_config_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/fx_spot_generation_config_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/gmm_component_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/gmm_component_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/gmm_component_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/ir_curve_generation_config_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/ir_curve_generation_config_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/ir_curve_generation_config_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/ir_curve_template_entry_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/ir_curve_template_entry_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/ir_curve_template_entry_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/market_data_generation_config_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/market_data_generation_config_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/yield_curve_process_type_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/yield_curve_process_type_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/repository/yield_curve_process_type_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/service/catalog_generator_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/service/folder_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/service/fx_spot_generation_config_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/service/gmm_component_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/service/ir_curve_generation_config_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/service/ir_curve_template_entry_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/service/market_data_generation_config_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/service/organisation_generator_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/service/organisation_publisher_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.synthetic.core/service/yield_curve_process_type_service.hpp"
)
