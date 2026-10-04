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
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>

namespace ores::marketdata::service {

double observation_value(const domain::market_observation& obs) {
    const auto value = ores::platform::numeric::parse_double(obs.value);
    if (!value)
        throw std::runtime_error("observation " + boost::uuids::to_string(obs.id) +
                                 " holds a value that is not a number: '" + obs.value + "'");
    return *value;
}

pillar_read
read_pillar_rates(ores::database::context ctx,
                  const ores::refdata::domain::ir_curve_bootstrap_config& config,
                  const std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar>& pillars,
                  const curve_republish_refdata_context& refctx,
                  std::chrono::system_clock::time_point as_of) {
    namespace core = ores::marketdata::core;

    repository::market_series_repository series_repo;
    repository::market_observation_repository obs_repo;

    pillar_read out;
    for (const auto& p : pillars) {
        const auto key = core::make_pillar_quote_key(config.currency_code,
                                                     p.start_tenor_code,
                                                     resolve_tenor_date(refctx, p.start_tenor_code),
                                                     resolve_tenor_date(refctx, p.end_tenor_code));
        // The config's own party, as the ingest loop scopes the series it writes:
        // the curve is bootstrapped for that party, so its quotes come from it.
        const auto series = series_repo.read_latest_by_uri(
            ctx, core::pillar_series_uri(key), boost::uuids::to_string(config.party_id));
        if (series.empty())
            continue;

        // The row stores the datum's URI, so the pillar is found by the same
        // string the writer wrote.
        const auto datum_uri = core::pillar_datum_uri(key);
        bool found = false;
        for (const auto& obs : obs_repo.read_as_of(ctx, series.front().id, as_of))
            if (obs.oresmd_uri == datum_uri) {
                out.rates_by_point_id.emplace(p.end_tenor_code, observation_value(obs));
                found = true;
            }
        if (found)
            out.series_ids.push_back(boost::uuids::to_string(series.front().id));
    }

    return out;
}

}
