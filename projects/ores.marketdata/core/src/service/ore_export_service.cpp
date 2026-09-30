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
#include "ores.marketdata.core/service/ore_export_service.hpp"
#include "ores.marketdata.api/domain/oresmd_uri.hpp"
#include "ores.marketdata.core/oresmd/oresmd_parser.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.marketdata.core/repository/market_fixings_repository.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.ore.core/market/market_data_serializer.hpp"
#include <chrono>
#include <sstream>
#include <stdexcept>
#include <vector>

namespace ores::marketdata::service {

using namespace ores::logging;

namespace {

/**
 * @brief One stored observation as the serializer's input.
 *
 * Every writer stores the key its row is written under -- the import keeps the
 * file's, the ingest loop keeps the tick's, and the republish service and the DQ
 * publish project or carry their own -- so the export no longer rebuilds one. A
 * row with none is a row no writer produced, and the export says so rather than
 * writing a key it cannot know.
 */
ores::ore::market::market_datum to_datum(const domain::market_observation& o) {
    if (o.key.empty())
        throw std::runtime_error("market data export: an observation carries no key to export");
    ores::ore::market::market_datum d;
    d.date =
        std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(o.observation_datetime)};
    d.value = o.value;
    d.key = o.key;
    return d;
}

/**
 * @brief One stored fixing as the serializer's input.
 *
 * A fixing's index name is a projection of the series' identity, the same way a
 * quote's key is: the identity is what the row is named by, so the name is read
 * from it rather than from a decomposition column.
 */
ores::ore::market::fixing to_fixing(const std::string& index_name,
                                    const domain::market_fixing& f) {
    ores::ore::market::fixing r;
    r.date = f.fixing_date;
    r.index_name = index_name;
    r.value = f.value;
    return r;
}

}

ore_export_service::ore_export_service(context ctx)
    : ctx_(std::move(ctx)) {}

ore_export_result ore_export_service::write_all() const {
    repository::market_series_repository series_repo;
    repository::market_observations_repository obs_repo;
    repository::market_fixings_repository fixings_repo;

    const auto series = series_repo.read_latest(ctx_);
    std::vector<ores::ore::market::market_datum> data;
    std::vector<ores::ore::market::fixing> fixings;
    for (const auto& s : series) {
        // The identity says which kind of series this is: an index name projects
        // from a fixing's and nothing projects from a quote's, so the export reads
        // the fixings of the first and the observations of the second without a
        // classification column to tell them apart.
        const auto identifier = core::oresmd_parser::parse(domain::oresmd_uri{s.oresmd_uri});
        if (const auto index_name = core::oresmd_projections::to_index_name(identifier)) {
            for (const auto& f : fixings_repo.read_latest(ctx_, s.id))
                fixings.push_back(to_fixing(*index_name, f));
            continue;
        }
        for (const auto& o : obs_repo.read_latest(ctx_, s.id))
            data.push_back(to_datum(o));
    }

    ore_export_result result;
    result.series_count = static_cast<int>(series.size());
    result.observation_count = static_cast<int>(data.size());
    result.fixing_count = static_cast<int>(fixings.size());

    std::ostringstream market_data;
    ores::ore::market::serialize_market_data(market_data, data);
    result.market_data = market_data.str();

    std::ostringstream fixing_text;
    ores::ore::market::serialize_fixings(fixing_text, fixings);
    result.fixings = fixing_text.str();

    BOOST_LOG_SEV(lg(), info) << "Exported " << result.series_count << " series, "
                              << result.observation_count << " observation(s), "
                              << result.fixing_count << " fixing(s)";
    return result;
}

}
