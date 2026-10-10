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
#include "ores.marketdata.core/service/series_slice_reader.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_identity_reader.hpp"
#include "ores.marketdata.core/service/series_shape.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::marketdata::service {

using namespace ores::logging;

namespace {

inline std::string_view logger_name = "ores.marketdata.core.series_slice_reader";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/// A term structure has one ladder, so a slice serves an object of one or two
/// axes and refuses a surface of surfaces.
constexpr std::size_t max_axes = 2;

using datum::field;

/// A refusal the caller reads, rather than an outcome it must infer.
messaging::get_series_slice_response refuse(std::string why) {
    messaging::get_series_slice_response response;
    response.success = false;
    response.message = std::move(why);
    return response;
}

}

messaging::get_series_slice_response
series_slice_reader::read(ores::database::context ctx,
                          const messaging::get_series_slice_request& request) {
    using ores::marketdata::datum::name_of;

    if (request.from_instant > request.to_instant)
        return refuse("the range ends before it starts");

    // The object, named the way the resolver names one, so no caller builds an
    // oresmd URI and none matches on one.
    repository::market_series_identity_reader resolver;
    const auto series = resolver.read(ctx, request.identity);
    if (series.empty()) {
        // No series carries the identity. That is not an error: a scope may hold
        // no series of that kind yet, and the answer is an empty slice.
        messaging::get_series_slice_response response;
        response.success = true;
        return response;
    }
    if (series.size() > 1)
        return refuse("the identity names " + std::to_string(series.size()) +
                      " series; state the party so that it names one");

    const auto series_id = boost::uuids::to_string(series.front().id);
    const auto declared = read_series_shape(ctx, series_id);
    if (declared.axes.empty())
        return refuse("the object declares no axis, so it is not a composite object");
    if (declared.axes.size() > max_axes)
        return refuse("the object declares " + std::to_string(declared.axes.size()) +
                      " axes, so its slice is not a term structure");

    // Which axis the component fixes and which one its nodes are read along.
    field component_axis{};
    field coordinate_axis{};
    if (declared.axes.size() == 1) {
        if (!request.component_field.empty())
            return refuse("the object declares one axis, '" +
                          std::string(name_of(declared.axes.front())) +
                          "', so its one component takes no component field");
        coordinate_axis = declared.axes.front();
    } else {
        if (request.component_field.empty())
            return refuse("the object declares '" + std::string(name_of(declared.axes[0])) +
                          "' and '" + std::string(name_of(declared.axes[1])) +
                          "', so the component must name the axis it fixes");
        const auto named = ores::marketdata::datum::field_named(request.component_field);
        if (!named || std::ranges::find(declared.axes, *named) == declared.axes.end())
            return refuse("'" + request.component_field +
                          "' is not an axis of the object; it "
                          "declares '" +
                          std::string(name_of(declared.axes[0])) + "' and '" +
                          std::string(name_of(declared.axes[1])) + "'");
        component_axis = *named;
        coordinate_axis = component_axis == declared.axes[0] ? declared.axes[1] : declared.axes[0];

        const auto& ladder = declared.values.at(component_axis);
        if (std::ranges::find(ladder, request.component_value) == ladder.end())
            return refuse("'" + request.component_value + "' is not a value the axis '" +
                          request.component_field + "' declares");
    }

    messaging::get_series_slice_response response;
    response.coordinate_field = std::string(name_of(coordinate_axis));
    response.coordinates = declared.values.at(coordinate_axis);

    // The instants the object states in the range, each with its own points: a
    // node the object does not state at an instant is a hole there, not a value
    // carried forward.
    const auto instants = repository::market_observation_repository{}.read_instants(
        ctx, series.front().id, request.from_instant, request.to_instant, {}, max_instants);

    for (const auto& instant : instants) {
        messaging::series_slice_instant row;
        row.as_of = instant.as_of;
        row.values.assign(response.coordinates.size(), std::string());
        for (const auto& point : instant.points) {
            const auto parsed = datum::oresmd_uri_codec::read(point.oresmd_uri);
            if (!parsed)
                throw std::runtime_error("a stored oresmd URI does not read: " + point.oresmd_uri);

            // A point belongs to the component only when it holds the value the
            // component fixes on that axis.
            if (declared.axes.size() == max_axes) {
                const auto held = coordinate_of(*parsed, component_axis);
                if (!held || *held != request.component_value)
                    continue;
            }

            const auto at = coordinate_of(*parsed, coordinate_axis);
            if (!at)
                continue;
            const auto node = std::ranges::find(response.coordinates, *at);
            if (node == response.coordinates.end())
                continue;
            row.values[static_cast<std::size_t>(node - response.coordinates.begin())] = point.value;
        }
        response.instants.push_back(std::move(row));
    }

    BOOST_LOG_SEV(lg(), debug) << "Read component " << request.component_field << "='"
                               << request.component_value << "' of series " << series_id << ": "
                               << response.instants.size() << " instants over "
                               << response.coordinates.size() << " coordinates";
    response.success = true;
    return response;
}

}
