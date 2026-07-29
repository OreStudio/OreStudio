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
    "domain/pricing_engine_type_json_io.cpp"
    "domain/pricing_engine_type_table.cpp"
    "domain/pricing_model_config_json_io.cpp"
    "domain/pricing_model_config_table.cpp"
    "domain/pricing_model_product_json_io.cpp"
    "domain/pricing_model_product_parameter_json_io.cpp"
    "domain/pricing_model_product_parameter_table.cpp"
    "domain/pricing_model_product_table.cpp"
    "generators/pricing_engine_type_generator.cpp"
    "generators/pricing_model_config_generator.cpp"
    "generators/pricing_model_product_generator.cpp"
    "generators/pricing_model_product_parameter_generator.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_engine_type.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_engine_type_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_engine_type_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_model_config.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_model_config_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_model_config_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_model_product.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_model_product_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_model_product_parameter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_model_product_parameter_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_model_product_parameter_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/domain/pricing_model_product_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/eventing/pricing_engine_type_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/eventing/pricing_model_config_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/eventing/pricing_model_product_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/eventing/pricing_model_product_parameter_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/generators/pricing_engine_type_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/generators/pricing_model_config_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/generators/pricing_model_product_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/generators/pricing_model_product_parameter_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/messaging/pricing_engine_type_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/messaging/pricing_model_config_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/messaging/pricing_model_product_parameter_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/messaging/pricing_model_product_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.api/ores.analytics.api.hpp"
)
