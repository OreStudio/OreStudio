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
    "messaging/registrar.cpp"
    "repository/pricing_engine_type_entity.cpp"
    "repository/pricing_engine_type_mapper.cpp"
    "repository/pricing_engine_type_repository.cpp"
    "repository/pricing_model_config_entity.cpp"
    "repository/pricing_model_config_mapper.cpp"
    "repository/pricing_model_config_repository.cpp"
    "repository/pricing_model_product_entity.cpp"
    "repository/pricing_model_product_mapper.cpp"
    "repository/pricing_model_product_parameter_entity.cpp"
    "repository/pricing_model_product_parameter_mapper.cpp"
    "repository/pricing_model_product_parameter_repository.cpp"
    "repository/pricing_model_product_repository.cpp"
    "service/pricing_engine_type_service.cpp"
    "service/pricing_model_config_service.cpp"
    "service/pricing_model_product_parameter_service.cpp"
    "service/pricing_model_product_service.cpp"
    "presentation/pricing_engine_type_history_field_mapper.cpp"
    "presentation/pricing_model_config_history_field_mapper.cpp"
    "presentation/pricing_model_product_history_field_mapper.cpp"
    "presentation/pricing_model_product_parameter_history_field_mapper.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/messaging/pricing_engine_type_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/messaging/pricing_model_config_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/messaging/pricing_model_product_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/messaging/pricing_model_product_parameter_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/ores.analytics.core.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_engine_type_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_engine_type_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_engine_type_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_model_config_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_model_config_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_model_config_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_model_product_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_model_product_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_model_product_parameter_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_model_product_parameter_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_model_product_parameter_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/repository/pricing_model_product_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/service/pricing_engine_type_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/service/pricing_model_config_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/service/pricing_model_product_parameter_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/service/pricing_model_product_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/presentation/pricing_engine_type_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/presentation/pricing_model_config_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/presentation/pricing_model_product_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.core/presentation/pricing_model_product_parameter_history_field_mapper.hpp"
)
