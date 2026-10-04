/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_SERVICE_CURVE_PILLAR_READER_HPP
#define ORES_MARKETDATA_SERVICE_CURVE_PILLAR_READER_HPP

#include "curve_republish_resolver.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include <chrono>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Each pillar's observed rate, keyed by the pillar's own end tenor code --
 * the same key resolve_bootstrap_pillars() matches on -- and the market_series each
 * rate was read from, in the order the pillars were read.
 *
 * The series ids are what the caller writes into observation_lineage's
 * source_series_ids, so a curve's provenance names the quotes it was built from.
 */
struct ORES_MARKETDATA_SERVICE_EXPORT pillar_read final {
    std::unordered_map<std::string, double> rates_by_point_id;
    std::vector<std::string> series_ids;
};

/**
 * @brief Reads each pillar's quote from the series the pillar's own ORE key names.
 *
 * The feed publishes one instrument per pillar, so the reader derives the same key
 * the feed built -- the config's currency and the pillar's resolved start and end
 * dates -- and asks for the series that key projects to, scoped to the config's own
 * party as the ingest loop scopes the series it writes.
 *
 * A pillar whose series is absent, or whose series carries no observation at the
 * point this read derived, gets no rate at all. resolve_bootstrap_pillars() then
 * fails loudly on it, so a bootstrap never quietly runs on a grid it did not read;
 * the config names no source series to fall back to.
 *
 * @throws std::invalid_argument if a pillar's tenor code does not resolve under
 * @p refctx, or if the key it builds is one the oresmd grammar has no series for.
 * Both are configuration errors and are deliberately not caught here.
 */
/**
 * @brief The observation's value as a number.
 *
 * @throws std::runtime_error naming the observation when its value is not a
 *         number.
 */
ORES_MARKETDATA_SERVICE_EXPORT double observation_value(const domain::market_observation& obs);

ORES_MARKETDATA_SERVICE_EXPORT pillar_read
read_pillar_rates(ores::database::context ctx,
                  const ores::refdata::domain::ir_curve_bootstrap_config& config,
                  const std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar>& pillars,
                  const curve_republish_refdata_context& refctx,
                  std::chrono::system_clock::time_point as_of);

}

#endif
