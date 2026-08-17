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
    "domain/feed_binding_json_io.cpp"
    "domain/feed_binding_table.cpp"
    "domain/feed_binding_table_io.cpp"
    "domain/fx_spot_tick_json_io.cpp"
    "domain/i_feed.cpp"
    "domain/ir_curve_tick_json_io.cpp"
    "domain/market_fixing_json_io.cpp"
    "domain/market_fixing_table.cpp"
    "domain/market_fixing_table_io.cpp"
    "domain/market_observation_json_io.cpp"
    "domain/market_observation_table.cpp"
    "domain/market_observation_table_io.cpp"
    "domain/market_series_json_io.cpp"
    "domain/market_series_table.cpp"
    "domain/market_series_table_io.cpp"
    "domain/observation_lineage_json_io.cpp"
    "domain/observation_lineage_table.cpp"
    "domain/observation_lineage_table_io.cpp"
    "generators/feed_binding_generator.cpp"
    "generators/market_fixing_generator.cpp"
    "generators/market_observation_generator.cpp"
    "generators/market_series_generator.cpp"
    "generators/observation_lineage_generator.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/asset_class.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/feed_binding.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/feed_binding_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/feed_binding_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/feed_binding_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/fx_spot_tick.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/fx_spot_tick_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/i_feed.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/ir_curve_tick.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/ir_curve_tick_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_data_identifier.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_data_requirement.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_fixing.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_fixing_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_fixing_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_fixing_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_observation.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_observation_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_observation_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_observation_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_series.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_series_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_series_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/market_series_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/observation_lineage.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/observation_lineage_json_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/observation_lineage_table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/observation_lineage_table_io.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/oresmd_enums.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/oresmd_uri.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/series_subclass.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/domain/tick_subjects.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/eventing/feed_binding_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/eventing/market_fixing_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/eventing/market_observation_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/eventing/market_series_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/eventing/observation_lineage_changed_event.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/generators/feed_binding_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/generators/market_fixing_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/generators/market_observation_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/generators/market_series_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/generators/observation_lineage_generator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/crm_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/curve_republish_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/curve_snapshot_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/feed_binding_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/import_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/market_fixing_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/market_observation_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/market_series_export_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/market_series_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.api/messaging/observation_lineage_protocol.hpp"
)
