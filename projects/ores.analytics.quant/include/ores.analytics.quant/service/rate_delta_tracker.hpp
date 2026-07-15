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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_RATE_DELTA_TRACKER_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_RATE_DELTA_TRACKER_HPP

#include "ores.analytics.quant/domain/crm_rate_view.hpp"
#include "ores.analytics.quant/export.hpp"
#include <map>
#include <mutex>
#include <string>
#include <utility>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief Stateful decorator that fills in @c crm_rate_view::delta_pct --
 * the %-change vs. the last value served for the same (base_code,
 * quote_code) pair, i.e. the pair as displayed, after any inversion has
 * already been applied by @c rate_inverter.
 *
 * Deliberately layered on top of, not inside, @c rate_engine: "delta vs.
 * last served value" is a serving-time/session concept (what a given
 * caller last saw), not a market/topology one, and it must apply to the
 * post-inversion displayed value, not the raw driver rate. Keeping this
 * as a separate wrapper leaves @c rate_engine's own concurrency model
 * (one immer::atom snapshot per batch) untouched.
 *
 * One instance is meant to be owned per (tenant, party, CRM name) --
 * e.g. by ores.marketdata's crm_ingest_bridge -- so "previous" means
 * "the last batch served to this specific consumer scope", matching the
 * Qt client's previous per-window previousRates_ map it replaces.
 *
 * Thread-safe: @c apply serializes internally via a mutex. Batches are
 * infrequent (one per client poll), so this is not a hot-path contention
 * point the way rate_engine::update()/rates() is.
 */
class ORES_ANALYTICS_QUANT_EXPORT rate_delta_tracker {
public:
    /// Fills in delta_pct on every element of @p views in place, then
    /// records their rates as "previous" for the next call. A view with
    /// status rate_status::unavailable neither receives a delta nor
    /// updates what's remembered as "previous" for its pair -- an
    /// unavailable reading is not a valid data point to diff against.
    void apply(std::vector<domain::crm_rate_view>& views);

private:
    using pair_key = std::pair<std::string, std::string>;

    std::mutex mutex_;
    std::map<pair_key, double> previous_;
};

} // namespace ores::analytics::quant::service

#endif
