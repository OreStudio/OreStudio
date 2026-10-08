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
#include "ores.marketdata.core/repository/series_shape_check.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.api/datum/value.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/series_axis_repository.hpp"
#include "ores.marketdata.core/repository/series_axis_value_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;

namespace {

auto& shape_check_lg() {
    static auto instance = make_logger("ores.marketdata.repository.series_shape_check");
    return instance;
}

/// The values each axis field of each series declares, keyed by field name.
using declared_shapes = std::map<std::string, std::map<std::string, std::set<std::string>>>;

/**
 * The declared shapes of the series @p series_ids names, read once.
 *
 * Both reads filter by the series, so a batch of points reads the shapes of the
 * series those points belong to and nothing else. A series with an axis row is
 * present, and so declares a shape; a series with none is absent, and so
 * declares nothing.
 */
declared_shapes read_shapes(ores::database::context ctx,
                            const std::vector<std::string>& series_ids) {
    declared_shapes shapes;
    for (const auto& axis : series_axis_repository{}.read_latest_for_series(ctx, series_ids))
        shapes[boost::uuids::to_string(axis.series_id)][axis.axis_field];
    for (const auto& value :
         series_axis_value_repository{}.read_latest_for_series(ctx, series_ids)) {
        const auto it = shapes.find(boost::uuids::to_string(value.series_id));
        if (it != shapes.end())
            it->second[value.axis_field].insert(value.value);
    }
    return shapes;
}

}

void series_shape_check::check(ores::database::context ctx,
                               const std::vector<domain::market_observation>& observations) {
    if (observations.empty())
        return;

    const std::set<std::string> distinct{[&] {
        std::set<std::string> ids;
        for (const auto& obs : observations)
            ids.insert(boost::uuids::to_string(obs.series_id));
        return ids;
    }()};
    const std::vector<std::string> series_ids(distinct.begin(), distinct.end());

    const auto shapes = read_shapes(ctx, series_ids);
    for (const auto& obs : observations) {
        const auto id = boost::uuids::to_string(obs.series_id);
        const auto shape = shapes.find(id);
        if (shape == shapes.end())
            continue;

        const auto point = datum::oresmd_uri_codec::read(obs.oresmd_uri);
        if (!point) {
            BOOST_LOG_SEV(shape_check_lg(), warn)
                << "Series " << id
                << " has an oresmd URI the codec cannot read, so its point is accepted: "
                << obs.oresmd_uri;
            continue;
        }

        for (const auto& [axis_field, values] : shape->second) {
            const auto field = datum::field_named(axis_field);
            if (!field || !point->holds(*field))
                continue;
            const auto text = datum::text_of(point->at(*field));
            if (!values.contains(text))
                throw std::invalid_argument("the shape of series " + id + " declares no value '" +
                                            text + "' for axis '" + axis_field + "'");
        }
    }
}

}
