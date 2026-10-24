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
    "app/adapter_registry.cpp"
    "app/adapter_supervisor.cpp"
    "app/adapter_tick_translator.cpp"
    "app/application.cpp"
    "app/billing.cpp"
    "app/composite_engine.cpp"
    "app/crm_ingest_bridge.cpp"
    "app/curve_republish_service.cpp"
    "app/delivery_meter.cpp"
    "app/demand_calculator.cpp"
    "app/feed_ingest_loop.cpp"
    "app/host.cpp"
    "app/tick_plan.cpp"
    "app/tick_writer.cpp"
    "app/usage_planner.cpp"
    "app/usage_recorder.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "curve_pillar_reader.cpp"
    "curve_republish_resolver.cpp"
    "main.cpp"
    "messaging/composite_event_registrar.cpp"
    "messaging/composite_input_event_registrar.cpp"
    "messaging/delivery_count_event_registrar.cpp"
    "messaging/demand_interval_event_registrar.cpp"
    "messaging/feed_binding_event_registrar.cpp"
    "messaging/market_data_mapping_event_registrar.cpp"
    "messaging/market_data_source_event_registrar.cpp"
    "messaging/market_data_vendor_instrument_event_registrar.cpp"
    "messaging/market_fixing_event_registrar.cpp"
    "messaging/market_series_event_registrar.cpp"
    "messaging/observation_lineage_event_registrar.cpp"
    "messaging/provider_event_registrar.cpp"
    "messaging/rate_card_component_event_registrar.cpp"
    "messaging/route_event_registrar.cpp"
    "messaging/series_classification_rule_event_registrar.cpp"
    "messaging/subscription_event_registrar.cpp"
    "messaging/subscription_interval_event_registrar.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/adapter_registry.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/adapter_supervisor.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/adapter_tick_translator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/application.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/application_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/batch_handoff.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/batch_size.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/billing.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/composite_engine.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/crm_ingest_bridge.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/curve_republish_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/delivery_meter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/demand_calculator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/drop_oldest_queue.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/feed_ingest_loop.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/host.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/store_backoff.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/tick_age.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/tick_plan.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/tick_writer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/usage_planner.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/usage_recorder.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/config/options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/config/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/config/parser_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/adapter_status_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/bill_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/composite_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/composite_input_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/crm_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/curve_republish_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/delivery_count_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/demand_interval_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/feed_binding_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/market_data_mapping_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/market_data_source_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/market_data_vendor_instrument_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/market_fixing_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/market_series_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/observation_lineage_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/provider_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/rate_card_component_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/route_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/series_classification_rule_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/subscription_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/subscription_interval_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/ores.marketdata.service.hpp"
)
