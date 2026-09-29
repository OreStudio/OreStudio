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
#include "curve_pillar_reader.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote_key.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>

namespace ores::marketdata::service {

namespace {

// The one series the segment used to be, read whole and matched on point id. A
// database written before the feed keyed its pillars individually holds it, so it
// stays readable until the seed stops writing it.
std::unordered_map<std::string, double>
read_grid_rates(ores::database::context ctx,
                const boost::uuids::uuid& source_series_id,
                std::chrono::system_clock::time_point as_of) {
    repository::market_observations_repository obs_repo;
    std::unordered_map<std::string, double> out;
    for (const auto& obs : obs_repo.read_as_of(ctx, source_series_id, as_of))
        out.emplace(obs.point_id, std::stod(obs.value));
    return out;
}

}

pillar_read
read_pillar_rates(ores::database::context ctx,
                  const ores::refdata::domain::ir_curve_bootstrap_config& config,
                  const std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar>& pillars,
                  const curve_republish_refdata_context& refctx,
                  std::chrono::system_clock::time_point as_of) {
    namespace core = ores::marketdata::core;

    repository::market_series_repository series_repo;
    repository::market_observations_repository obs_repo;

    pillar_read out;
    std::vector<const ores::refdata::domain::ir_curve_bootstrap_pillar*> unresolved;
    for (const auto& p : pillars) {
        const auto key = core::make_pillar_quote_key(config.currency_code,
                                                     p.start_tenor_code,
                                                     resolve_tenor_date(refctx, p.start_tenor_code),
                                                     resolve_tenor_date(refctx, p.end_tenor_code));
        // The config's own party, as the ingest loop scopes the series it writes:
        // the curve is bootstrapped for that party, so its quotes come from it.
        const auto series = series_repo.read_latest_by_uri(
            ctx, core::pillar_series_uri(key), boost::uuids::to_string(config.party_id));
        if (series.empty()) {
            unresolved.push_back(&p);
            continue;
        }

        bool found = false;
        for (const auto& obs : obs_repo.read_as_of(ctx, series.front().id, as_of))
            if (obs.point_id == key.point) {
                out.rates_by_point_id.emplace(p.end_tenor_code, std::stod(obs.value));
                found = true;
            }
        if (!found) {
            // The series exists but not at the point this read derived, which a
            // horizon the feed did not publish under produces. The grid is then the
            // pillar's other source, so this pillar is resolved like a missing one.
            unresolved.push_back(&p);
            continue;
        }
        out.series_ids.push_back(boost::uuids::to_string(series.front().id));
    }

    if (!unresolved.empty()) {
        const auto grid = read_grid_rates(ctx, config.source_series_id, as_of);
        const auto grid_id = boost::uuids::to_string(config.source_series_id);
        for (const auto* p : unresolved) {
            if (auto it = grid.find(p->end_tenor_code); it != grid.end())
                out.rates_by_point_id.emplace(p->end_tenor_code, it->second);
            out.series_ids.push_back(grid_id);
        }
    }

    return out;
}

}
