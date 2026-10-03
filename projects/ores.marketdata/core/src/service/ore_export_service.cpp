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
#include "ores.marketdata.core/datum/ore_index_codec.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_fixings_repository.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.ore.core/market/market_data_serializer.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <sstream>
#include <stdexcept>
#include <vector>

namespace ores::marketdata::service {

using namespace ores::logging;

namespace {

/**
 * @brief One stored observation as the serializer's input, its key written from
 * the datum URI the row stores.
 *
 * The row holds one identity, so the key is that identity's canonical ORE
 * spelling; a row whose URI does not read is a data-integrity error and names
 * itself.
 */
ores::ore::market::market_datum to_datum(const domain::market_observation& o) {
    const auto fail = [&](const std::string& why) {
        return std::runtime_error("market data export: observation " +
                                  boost::uuids::to_string(o.id) + " has no ORE key: " + why);
    };
    const auto datum = datum::oresmd_uri_codec::read(o.oresmd_uri);
    if (!datum)
        throw fail(datum.error());
    const auto key = datum::ore_key_codec::write(*datum);
    if (!key)
        throw fail(key.error());
    ores::ore::market::market_datum d;
    d.date =
        std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(o.observation_datetime)};
    d.value = o.value;
    d.key = *key;
    return d;
}

/**
 * @brief One stored fixing as the serializer's input, under the index name its
 * series' URI writes.
 */
ores::ore::market::fixing to_fixing(const std::string& index_name, const domain::market_fixing& f) {
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
        // A quote series has a URI the datum codec reads and a fixing series one
        // the index codec reads. A series neither reads is a data-integrity error
        // and names both reasons.
        if (const auto quote_series = datum::oresmd_uri_codec::read(s.oresmd_uri); !quote_series) {
            const auto index = datum::oresmd_uri_codec::read_index(s.oresmd_uri);
            if (!index)
                throw std::runtime_error(
                    "market data export: series " + boost::uuids::to_string(s.id) + " carries '" +
                    s.oresmd_uri + "', which is no quote series (" + quote_series.error() +
                    ") and no fixing series (" + index.error() + ")");
            const auto index_name = datum::ore_index_codec::write(*index);
            for (const auto& f : fixings_repo.read_latest(ctx_, s.id))
                fixings.push_back(to_fixing(index_name, f));
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
