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
#include "ores.marketdata.core/service/series_evolution_reader.hpp"
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
#include <variant>
#include <vector>

namespace ores::marketdata::service {

using namespace ores::logging;

namespace {

inline std::string_view logger_name = "ores.marketdata.core.series_evolution_reader";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

using datum::field;

/// The words a node's status takes, in the order the rule checks them.
constexpr std::string_view status_never_quoted = "never_quoted";
constexpr std::string_view status_persistent = "persistent";
constexpr std::string_view status_added = "added";
constexpr std::string_view status_dropped = "dropped";
constexpr std::string_view status_intermittent = "intermittent";

/// The value @p point holds on @p axis, or nothing when it holds none.
std::optional<std::string> coordinate_of(const datum::market_datum& point, field axis) {
    if (!point.holds(axis))
        return std::nullopt;
    const auto& held = point.at(axis);
    if (std::holds_alternative<datum::none_t>(held))
        return std::nullopt;
    return datum::text_of(held);
}

/// A refusal the caller reads, rather than an outcome it must infer.
messaging::get_series_evolution_response refuse(std::string why) {
    messaging::get_series_evolution_response response;
    response.success = false;
    response.message = std::move(why);
    return response;
}

}

messaging::get_series_evolution_response
series_evolution_reader::read(ores::database::context ctx,
                              const messaging::get_series_evolution_request& request) {
    using ores::marketdata::datum::name_of;

    // A stated set replaces the range, so the range is only checked when it is
    // the one that decides the instants.
    if (request.instants.empty() && request.from_instant > request.to_instant)
        return refuse("the range ends before it starts");

    repository::market_series_identity_reader resolver;
    const auto series = resolver.read(ctx, request.identity);
    if (series.empty()) {
        // No series carries the identity, which is not an error: a scope may
        // hold no series of that kind yet, and the answer is an empty matrix.
        messaging::get_series_evolution_response response;
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

    // The declared grid: every combination of the declared values, with the
    // last axis varying fastest, so a node's index is the sum of each value's
    // position times that axis's stride.
    std::vector<std::size_t> strides(declared.axes.size(), 1);
    std::size_t node_count = 1;
    for (std::size_t i = declared.axes.size(); i-- > 0;) {
        strides[i] = node_count;
        node_count *= declared.values.at(declared.axes[i]).size();
    }

    messaging::get_series_evolution_response response;
    response.coordinate_fields.reserve(declared.axes.size());
    for (const auto axis : declared.axes)
        response.coordinate_fields.emplace_back(name_of(axis));

    response.nodes.reserve(node_count);
    for (std::size_t n = 0; n < node_count; ++n) {
        messaging::evolution_node node;
        node.coordinates.reserve(declared.axes.size());
        for (std::size_t i = 0; i < declared.axes.size(); ++i) {
            const auto& values = declared.values.at(declared.axes[i]);
            node.coordinates.push_back(values[(n / strides[i]) % values.size()]);
        }
        response.nodes.push_back(std::move(node));
    }

    const auto instants =
        repository::market_observation_repository{}.read_instants(ctx,
                                                                  series.front().id,
                                                                  request.from_instant,
                                                                  request.to_instant,
                                                                  request.instants,
                                                                  max_instants);

    // The node a point belongs to, or nothing when it leaves an axis out or
    // holds a value the shape does not declare. Neither can be written today,
    // and a point that is neither is not a node of this object.
    const auto node_of = [&](const datum::market_datum& point) -> std::optional<std::size_t> {
        std::size_t index = 0;
        for (std::size_t i = 0; i < declared.axes.size(); ++i) {
            const auto held = coordinate_of(point, declared.axes[i]);
            if (!held)
                return std::nullopt;
            const auto& values = declared.values.at(declared.axes[i]);
            const auto at = std::ranges::find(values, *held);
            if (at == values.end())
                return std::nullopt;
            index += static_cast<std::size_t>(at - values.begin()) * strides[i];
        }
        return index;
    };

    for (const auto& instant : instants) {
        messaging::evolution_instant row;
        row.as_of = instant.as_of;
        row.values.assign(node_count, std::string());
        for (const auto& point : instant.points) {
            const auto parsed = datum::oresmd_uri_codec::read(point.oresmd_uri);
            if (!parsed)
                throw std::runtime_error("a stored oresmd URI does not read: " + point.oresmd_uri);
            const auto node = node_of(*parsed);
            if (!node)
                continue;
            row.values[*node] = point.value;
        }
        response.instants.push_back(std::move(row));
    }

    // One word per node for the change it shows over the range, in the order
    // never_quoted, persistent, added, dropped, intermittent.
    for (std::size_t n = 0; n < node_count; ++n) {
        bool carried_ever = false;
        bool carried_all = true;
        bool carried_first = false;
        bool carried_last = false;
        for (std::size_t i = 0; i < response.instants.size(); ++i) {
            const bool carried = !response.instants[i].values[n].empty();
            carried_ever = carried_ever || carried;
            carried_all = carried_all && carried;
            if (i == 0)
                carried_first = carried;
            if (i + 1 == response.instants.size())
                carried_last = carried;
        }
        auto& status = response.nodes[n].status;
        if (!carried_ever)
            status = status_never_quoted;
        else if (carried_all)
            status = status_persistent;
        else if (carried_last && !carried_first)
            status = status_added;
        else if (carried_first && !carried_last)
            status = status_dropped;
        else
            status = status_intermittent;
    }

    BOOST_LOG_SEV(lg(), debug) << "Read the evolution of series " << series_id << ": "
                               << response.instants.size() << " instants by " << node_count
                               << " nodes over " << declared.axes.size() << " axes";
    response.success = true;
    return response;
}

}
