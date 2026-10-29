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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <stdexcept>
#include <string>

namespace ores::marketdata::service {

namespace {

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.marketdata.service.curve_pillar_reader");
    return instance;
}

}

double observation_value(const domain::market_observation& obs) {
    const auto value = ores::platform::numeric::parse_double(obs.value);
    if (!value)
        throw std::runtime_error("observation " + boost::uuids::to_string(obs.id) +
                                 " holds a value that is not a number: '" + obs.value + "'");
    return *value;
}

pillar_read
read_pillar_rates(ores::database::context ctx,
                  const std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar>& pillars,
                  const curve_republish_refdata_context& refctx,
                  std::chrono::system_clock::time_point as_of) {
    namespace datum = ores::marketdata::datum;

    repository::market_series_repository series_repo;
    repository::market_observation_repository obs_repo;

    pillar_read out;
    for (const auto& p : pillars) {
        const auto series =
            series_repo.read_latest(ctx, boost::uuids::to_string(p.market_series_id));
        if (series.empty()) {
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << "No market series " << boost::uuids::to_string(p.market_series_id)
                << " holds the pillar " << p.end_tenor_code << " of curve config "
                << boost::uuids::to_string(p.bootstrap_config_id);
            continue;
        }

        // The series says which ladder the pillar reads; its end tenor, resolved
        // through the config's convention, says which rung of it.
        const auto term = core::pillar_term_text(p.start_tenor_code,
                                                 p.end_tenor_code,
                                                 resolve_tenor_date(refctx, p.start_tenor_code),
                                                 resolve_tenor_date(refctx, p.end_tenor_code));
        bool found = false;
        for (const auto& obs : obs_repo.read_as_of(ctx, series.front().id, as_of)) {
            const auto named = datum::oresmd_uri_codec::read(obs.oresmd_uri);
            if (!named || named->is_series() || !named->holds(datum::field::term))
                continue;
            if (datum::text_of(named->at(datum::field::term)) != term)
                continue;
            out.rates_by_point_id.insert_or_assign(p.end_tenor_code, observation_value(obs));
            found = true;
        }
        // One series answers every rung of a ladder, so a list that repeated it
        // would say less than one that names it once: this is the set of series the
        // curve read, and it is what the lineage records.
        const auto series_id = boost::uuids::to_string(series.front().id);
        if (found && std::ranges::find(out.series_ids, series_id) == out.series_ids.end())
            out.series_ids.push_back(series_id);
    }

    return out;
}

}
