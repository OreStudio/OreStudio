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
#include "ores.marketdata.core/repository/market_fixings_repository.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.ore.core/market/market_data_serializer.hpp"
#include "ores.ore.core/market/series_key_registry.hpp"
#include "ores.ore.core/repository/series_key_shape_repository.hpp"
#include <chrono>
#include <sstream>
#include <vector>

namespace ores::marketdata::service {

using namespace ores::logging;

namespace {

/**
 * @brief One stored observation as the serializer's input.
 *
 * The producer's key wins when the row has one, because the import rewrote it
 * on the way in and only that text reproduces the file. The serializer emits
 * =key= as given whenever the decomposition is left empty, which is the
 * fallback its own contract describes.
 *
 * Without one the key is rebuilt from the series and the observation's point,
 * and the point is dropped for a series type that has no point dimension. Every
 * point-free row stores a point regardless -- =SPOT= for an FX rate, the empty
 * string for a recovery rate -- because a row has to name where it was
 * recorded. Emitting it would turn =FX/RATE/EUR/USD= into
 * =FX/RATE/EUR/USD/SPOT=, a key no producer writes, so the shape table decides
 * rather than the stored value.
 */
ores::ore::market::market_datum to_datum(const domain::market_series& s,
                                         const domain::market_observation& o,
                                         const ores::ore::market::series_key_registry& registry) {
    ores::ore::market::market_datum d;
    d.date =
        std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(o.observation_datetime)};
    d.value = o.value;
    if (!o.key.empty()) {
        d.key = o.key;
        return d;
    }
    d.series_type = s.series_type;
    d.metric = s.metric;
    d.qualifier = s.qualifier;
    if (registry.has_point_dimension(s.series_type))
        d.point_id = o.point_id;
    return d;
}

/**
 * @brief One stored fixing as the serializer's input.
 *
 * A fixing's index name is the series' qualifier. The parser stores the name
 * verbatim and fixings do not follow the key grammar, so nothing rewrote it and
 * there is no second spelling to prefer.
 */
ores::ore::market::fixing to_fixing(const domain::market_series& s,
                                    const domain::market_fixing& f) {
    ores::ore::market::fixing r;
    r.date = f.fixing_date;
    r.index_name = s.qualifier;
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
    // Read the key grammar once for the whole export: the point-elision rule
    // consults it per row.
    const ores::ore::market::series_key_registry registry{
        ores::ore::repository::series_key_shape_repository{}.read_latest(ctx_)};

    std::vector<ores::ore::market::market_datum> data;
    std::vector<ores::ore::market::fixing> fixings;
    for (const auto& s : series) {
        if (s.series_type == import_service::fixing_series_type) {
            for (const auto& f : fixings_repo.read_latest(ctx_, s.id))
                fixings.push_back(to_fixing(s, f));
            continue;
        }
        for (const auto& o : obs_repo.read_latest(ctx_, s.id))
            data.push_back(to_datum(s, o, registry));
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
