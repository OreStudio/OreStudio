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
    "app/application.cpp"
    "app/crm_ingest_bridge.cpp"
    "app/curve_republish_service.cpp"
    "app/feed_ingest_loop.cpp"
    "app/host.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "curve_republish_resolver.cpp"
    "main.cpp"
    "messaging/feed_binding_event_registrar.cpp"
    "messaging/market_fixing_event_registrar.cpp"
    "messaging/market_observation_event_registrar.cpp"
    "messaging/market_series_event_registrar.cpp"
    "messaging/observation_lineage_event_registrar.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/application.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/application_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/crm_ingest_bridge.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/curve_republish_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/feed_ingest_loop.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/app/host.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/config/options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/config/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/config/parser_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/crm_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/curve_republish_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/feed_binding_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/market_fixing_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/market_observation_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/market_series_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/messaging/observation_lineage_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.service/ores.marketdata.service.hpp"
)
