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
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include <chrono>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Each pillar's observed rate, keyed by the pillar's own end tenor code --
 * the same key resolve_bootstrap_pillars() matches on -- and the market series the
 * rates were read from, each named once, in the order the pillars were read.
 *
 * One series answers every rung of a ladder, so the list holds each series once
 * however many pillars read it. The ids are what the caller writes into
 * observation_lineage's source_series_ids, so a curve's provenance names the
 * quotes it was built from.
 */
struct ORES_MARKETDATA_SERVICE_EXPORT pillar_read final {
    std::unordered_map<std::string, double> rates_by_point_id;
    std::vector<std::string> series_ids;
};

/**
 * @brief The observation's value as a number.
 *
 * @throws std::runtime_error naming the observation when its value is not a
 *         number.
 */
ORES_MARKETDATA_SERVICE_EXPORT double observation_value(const domain::market_observation& obs);

/**
 * @brief Reads each pillar's quote from the market series the pillar names.
 *
 * A pillar carries the soft reference to the series it reads, so this asks the
 * store for that series' observations at the as-of and takes the one whose datum
 * term is the pillar's own. The term is the pillar's end tenor resolved under
 * @p refctx, so the series says which ladder and the term says which rung of it:
 * every pillar of one spot-starting ladder reads one series at its own term.
 *
 * A pillar whose series is absent, or whose series carries no observation at the
 * term this read derived, gets no rate at all. resolve_bootstrap_pillars() then
 * fails loudly on it, so a bootstrap never quietly runs on a grid it did not read.
 *
 * @throws std::invalid_argument if a pillar's tenor code does not resolve under
 * @p refctx.
 */
ORES_MARKETDATA_SERVICE_EXPORT pillar_read
read_pillar_rates(ores::database::context ctx,
                  const std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar>& pillars,
                  const curve_republish_refdata_context& refctx,
                  std::chrono::system_clock::time_point as_of);

}

#endif
