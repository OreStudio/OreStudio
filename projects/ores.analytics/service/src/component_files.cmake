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
# Template: cmake_component_files_src.mustache
# To modify, update the template and regenerate.
set(files
    "app/application.cpp"
    "app/host.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "main.cpp"
    "messaging/credit_simulation_config_event_registrar.cpp"
    "messaging/credit_simulation_entity_config_event_registrar.cpp"
    "messaging/credit_simulation_matrix_config_event_registrar.cpp"
    "messaging/credit_simulation_matrix_row_config_event_registrar.cpp"
    "messaging/credit_simulation_netting_set_config_event_registrar.cpp"
    "messaging/event_registrar.cpp"
    "messaging/pricing_engine_type_event_registrar.cpp"
    "messaging/pricing_model_config_event_registrar.cpp"
    "messaging/pricing_model_product_event_registrar.cpp"
    "messaging/pricing_model_product_parameter_event_registrar.cpp"
    "messaging/shift_type_event_registrar.cpp"
    "messaging/stress_shift_family_event_registrar.cpp"
    "messaging/stress_test_library_event_registrar.cpp"
    "messaging/stress_test_scenario_event_registrar.cpp"
    "messaging/stress_test_shift_event_registrar.cpp"
    "messaging/todays_market_collection_event_registrar.cpp"
    "messaging/todays_market_config_event_registrar.cpp"
    "messaging/todays_market_configuration_binding_event_registrar.cpp"
    "messaging/todays_market_configuration_event_registrar.cpp"
    "messaging/todays_market_entry_event_registrar.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/app/application.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/app/application_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/app/host.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/config/options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/config/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/config/parser_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/credit_simulation_config_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/credit_simulation_entity_config_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/credit_simulation_matrix_config_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/credit_simulation_matrix_row_config_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/credit_simulation_netting_set_config_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/pricing_engine_type_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/pricing_model_config_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/pricing_model_product_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/pricing_model_product_parameter_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/shift_type_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/stress_shift_family_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/stress_test_library_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/stress_test_scenario_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/stress_test_shift_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/todays_market_collection_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/todays_market_config_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/todays_market_configuration_binding_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/todays_market_configuration_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/messaging/todays_market_entry_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.service/ores.analytics.service.hpp"
)
