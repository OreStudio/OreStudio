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
#include "ores.marketdata.core/service/manual_point_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_identity_reader.hpp"
#include "ores.marketdata.core/service/series_shape.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <charconv>
#include <cmath>
#include <cstddef>
#include <expected>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

namespace ores::marketdata::service {

using namespace ores::logging;

namespace {

inline std::string_view logger_name = "ores.marketdata.core.manual_point_service";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/// The series and the observation URI a request names.
struct node {
    boost::uuids::uuid series_id;
    std::string oresmd_uri;
};

/// The node the identity and the coordinates name, or the reason they name none.
std::expected<node, std::string> locate(ores::database::context ctx,
                                        const messaging::resolve_series_identity_request& identity,
                                        const std::vector<std::string>& coordinates) {
    using namespace ores::marketdata::datum;

    const auto found = repository::market_series_identity_reader::read(ctx, identity);
    if (found.empty())
        return std::unexpected("No series carries that identity.");
    if (found.size() > 1)
        return std::unexpected("The identity names more than one series; state the party.");
    const auto& series = found.front();

    // The observation takes the context's party, so a context for another party
    // would write a point the series' own party never sees.
    if (ctx.party_id() != series.party_id)
        return std::unexpected("The series belongs to another party.");

    const auto parsed = oresmd_uri_codec::read(series.oresmd_uri);
    if (!parsed || !parsed->is_series())
        return std::unexpected("The series is not one a point can be keyed on.");

    const auto shape = read_series_shape(ctx, boost::uuids::to_string(series.id));
    if (shape.axes.empty())
        return std::unexpected("The series declares no shape.");
    if (coordinates.size() != shape.axes.size())
        return std::unexpected("The node needs " + std::to_string(shape.axes.size()) +
                               " coordinates and the request states " +
                               std::to_string(coordinates.size()) + ".");

    std::vector<field_value> at;
    for (std::size_t i = 0; i < shape.axes.size(); ++i) {
        const auto axis = shape.axes[i];
        const auto& declared = shape.values.at(axis);
        if (std::ranges::find(declared, coordinates[i]) == declared.end())
            return std::unexpected("'" + coordinates[i] + "' is not a declared " +
                                   std::string(name_of(axis)) + " of the series.");
        auto held = parse_value(axis, coordinates[i]);
        if (!held)
            return std::unexpected(held.error());
        at.push_back({axis, *held});
    }

    const auto point = datum_at(*parsed, std::move(at));
    if (!point)
        return std::unexpected(point.error());
    auto uri = oresmd_uri_codec::write(*point);
    if (!uri)
        return std::unexpected(uri.error());
    return node{series.id, std::move(*uri)};
}

/// Whether @p text is one finite number, written whole in decimal. A leading space,
/// a plus sign and a hex float are not numbers here, because the text is stored as
/// it is stated.
bool is_number(const std::string& text) {
    double number = 0;
    const auto end = text.data() + text.size();
    const auto [stopped, error] = std::from_chars(text.data(), end, number);
    return error == std::errc{} && stopped == end && std::isfinite(number);
}

}

messaging::key_manual_point_response
manual_point_service::key(ores::database::context ctx,
                          const messaging::key_manual_point_request& request,
                          std::chrono::system_clock::time_point now) {
    messaging::key_manual_point_response response;
    const auto refuse = [&](std::string message) {
        response.success = false;
        response.message = std::move(message);
        return response;
    };

    if (!is_number(request.value))
        return refuse("'" + request.value + "' is not a number.");
    if (request.change_reason_code.empty())
        return refuse("A manual point needs a change reason code.");
    if (request.change_commentary.empty())
        return refuse("A manual point needs a commentary that says why.");

    const auto at = locate(ctx, request.identity, request.coordinates);
    if (!at)
        return refuse(at.error());

    repository::market_observation_repository{}.write_manual_point(
        ctx,
        at->series_id,
        at->oresmd_uri,
        std::chrono::floor<std::chrono::microseconds>(now),
        request.value,
        request.change_reason_code,
        request.change_commentary);
    BOOST_LOG_SEV(lg(), info) << "Keyed a manual point on series " << at->series_id;
    response.success = true;
    return response;
}

messaging::clear_manual_point_response
manual_point_service::clear(ores::database::context ctx,
                            const messaging::clear_manual_point_request& request) {
    messaging::clear_manual_point_response response;
    const auto at = locate(ctx, request.identity, request.coordinates);
    if (!at) {
        response.message = at.error();
        return response;
    }

    // A node can hold a manual point at more than one instant, because each key is
    // stamped when it is made. Clearing releases the node, so it releases all of
    // them in one transaction.
    const auto cleared = repository::market_observation_repository{}.clear_manual_points(
        ctx, at->series_id, at->oresmd_uri);
    if (cleared == 0) {
        response.message = "The node holds no manual point.";
        return response;
    }
    BOOST_LOG_SEV(lg(), info) << "Cleared " << cleared << " manual point(s) on series "
                              << at->series_id;
    response.success = true;
    return response;
}

}
