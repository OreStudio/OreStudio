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
    "messaging/feed_binding_history_provider_registrar.cpp"
    "messaging/feed_binding_registrar.cpp"
    "messaging/market_fixing_registrar.cpp"
    "messaging/market_observation_registrar.cpp"
    "messaging/market_series_history_provider_registrar.cpp"
    "messaging/market_series_registrar.cpp"
    "messaging/observation_lineage_history_provider_registrar.cpp"
    "messaging/observation_lineage_registrar.cpp"
    "messaging/publish_from_dq_handler.cpp"
    "messaging/registrar.cpp"
    "oresmd/oresmd_parser.cpp"
    "oresmd/oresmd_projections.cpp"
    "oresmd/oresmd_resolver.cpp"
    "presentation/feed_binding_history_field_mapper.cpp"
    "presentation/market_fixing_history_field_mapper.cpp"
    "presentation/market_observation_history_field_mapper.cpp"
    "presentation/market_series_history_field_mapper.cpp"
    "presentation/observation_lineage_history_field_mapper.cpp"
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
    "repository/observation_lineage_entity.cpp"
    "repository/observation_lineage_mapper.cpp"
    "repository/observation_lineage_repository.cpp"
    "service/feed_binding_service.cpp"
    "service/import_service.cpp"
    "service/market_fixing_service.cpp"
    "service/market_observation_service.cpp"
    "service/market_series_service.cpp"
    "service/observation_lineage_service.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/curve_snapshot_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/feed_binding_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/feed_binding_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/feed_binding_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/import_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/market_fixing_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/market_fixing_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/market_observation_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/market_observation_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/market_series_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/market_series_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/market_series_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/observation_lineage_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/observation_lineage_history_provider_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/observation_lineage_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/publish_from_dq_handler.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/messaging/registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/oresmd/detail/oresmd_index_family_utils.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/oresmd/detail/oresmd_string_utils.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/oresmd/oresmd_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/oresmd/oresmd_parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/oresmd/oresmd_projections.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/oresmd/oresmd_resolver.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/presentation/feed_binding_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/presentation/market_fixing_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/presentation/market_observation_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/presentation/market_series_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/presentation/observation_lineage_history_field_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/feed_binding_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/feed_binding_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/feed_binding_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_fixing_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_fixing_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_fixing_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_fixings_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_observation_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_observation_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_observation_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_observations_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_series_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_series_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/market_series_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/observation_lineage_entity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/observation_lineage_mapper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/repository/observation_lineage_repository.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/service/feed_binding_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/service/import_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/service/market_fixing_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/service/market_observation_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/service/market_series_service.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.marketdata.core/service/observation_lineage_service.hpp"
)
