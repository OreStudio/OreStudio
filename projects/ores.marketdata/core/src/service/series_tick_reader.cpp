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
#include "ores.marketdata.core/service/series_tick_reader.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/datum/series_stream.hpp"
#include "ores.marketdata.core/repository/market_series_identity_reader.hpp"
#include "ores.marketdata.core/repository/tick_entitlement.hpp"
#include "ores.marketdata.core/repository/tick_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace ores::marketdata::service {

using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

namespace {

using reply = messaging::get_series_ticks_response;

/**
 * @brief The reply a span gets before any tick is read: a refusal of too many coordinates or of
 * a span that ends before it starts, an empty reply for a span with no end that starts after
 * now, or nothing when the span may be read.
 */
std::optional<reply> span_reply(const std::vector<std::string>& coordinates,
                                std::chrono::system_clock::time_point from,
                                std::optional<std::chrono::system_clock::time_point> to,
                                std::chrono::system_clock::time_point now) {
    reply response;
    if (coordinates.size() > series_tick_reader::max_coordinates) {
        response.result = refuse(
            outcome_code::filter_too_large,
            {.field = "coordinates", .limit = std::to_string(series_tick_reader::max_coordinates)});
        return response;
    }
    if (to.value_or(now) < from) {
        // A span with no end runs to now. A client clock a little ahead of the service puts its
        // start after now, and nothing has ticked after now, so that span is empty and not wrong.
        if (to)
            response.result = refuse(outcome_code::range_inverted, {.entity = "ticks"});
        return response;
    }
    return std::nullopt;
}

/// Every tick of @p stream in the span, or a refusal of a span over the ceiling.
reply read_stream(const ores::database::context& ctx,
                  const boost::uuids::uuid& stream,
                  std::chrono::system_clock::time_point from,
                  std::chrono::system_clock::time_point to,
                  const std::vector<std::string>& coordinates) {
    reply response;
    // One tick more than the ceiling tells a span over it from a span at it.
    const auto ticks = repository::tick_repository{}.read_range(
        ctx, stream, from, to, coordinates, series_tick_reader::max_ticks + 1);
    if (ticks.size() > series_tick_reader::max_ticks) {
        response.result =
            refuse(outcome_code::range_too_large,
                   {.entity = "ticks", .limit = std::to_string(series_tick_reader::max_ticks)});
        return response;
    }

    std::map<std::string, int> index_of;
    response.gaps_us.reserve(ticks.size());
    response.coordinate_indexes.reserve(ticks.size());
    response.values.reserve(ticks.size());
    auto previous = ticks.empty() ? from : ticks.front().at;
    if (!ticks.empty())
        response.first_instant = previous;
    for (const auto& t : ticks) {
        const auto found = index_of.try_emplace(t.oresmd_uri, static_cast<int>(index_of.size()));
        if (found.second)
            response.coordinates.push_back(t.oresmd_uri);
        response.gaps_us.push_back(
            std::chrono::duration_cast<std::chrono::microseconds>(t.at - previous).count());
        response.coordinate_indexes.push_back(found.first->second);
        response.values.push_back(t.value);
        previous = t.at;
    }
    return response;
}

}

reply series_tick_reader::read(const ores::database::context& ctx,
                               const messaging::get_series_ticks_request& request,
                               std::chrono::system_clock::time_point now) {
    reply response;

    // The store filters no tick by party, so a session with no party has no door to it and an
    // empty reply would look like a series with no ticks.
    if (ctx.visible_party_ids().empty()) {
        response.result = refuse(outcome_code::party_required, {.entity = "ticks"});
        return response;
    }
    if (auto early = span_reply(request.coordinates, request.from_instant, request.to_instant, now))
        return *early;

    // The series, named the way the resolver names one, so no caller builds an oresmd URI. A
    // series that no row carries has no ticks, and the answer is an empty reply.
    const auto series = repository::market_series_identity_reader::read(ctx, request.identity);
    if (series.empty())
        return response;
    if (series.size() > 1) {
        std::set<std::string> parties;
        std::set<std::string> providers;
        for (const auto& s : series) {
            parties.insert(boost::uuids::to_string(s.party_id));
            providers.insert(s.provider_code);
        }
        std::string provider_list;
        for (const auto& p : providers)
            provider_list += (provider_list.empty() ? "" : ", ") + p;
        const auto reason = std::to_string(parties.size()) + " parties; providers " + provider_list;
        response.result =
            refuse(outcome_code::series_ambiguous,
                   {.entity = "ticks", .value = std::to_string(series.size()), .reason = reason});
        return response;
    }
    const auto& resolved = series.front();

    // The tick store is shared by every party and no policy filters it by party, so the
    // subscription is the door to it. The history returns raw values, so the party must hold
    // display or non-display; may_read_ticks states the rule once for every read of ticks.
    if (!ctx.party_id()) {
        response.result = refuse(outcome_code::party_required, {.entity = "ticks"});
        return response;
    }
    if (!repository::may_read_ticks(ctx, resolved.provider_code)) {
        response.result =
            refuse(outcome_code::not_subscribed,
                   {.entity = "ticks", .field = "provider", .value = resolved.provider_code});
        return response;
    }

    // The stream is named by the provider and the series URI, as the ingest names it, and no
    // table maps a series to a stream.
    return read_stream(ctx,
                       datum::stream_id(resolved.provider_code, resolved.oresmd_uri),
                       request.from_instant,
                       request.to_instant.value_or(now),
                       request.coordinates);
}

reply series_tick_reader::read(const ores::database::context& ctx,
                               const messaging::get_provider_ticks_request& request,
                               std::chrono::system_clock::time_point now) {
    reply response;

    // The system tenant reads every provider; any other session reads through its party's
    // subscription, so a session with no party has no door.
    if (!ctx.tenant_id().is_system() && !ctx.party_id()) {
        response.result = refuse(outcome_code::party_required, {.entity = "ticks"});
        return response;
    }
    if (!repository::may_read_ticks(ctx, request.provider_code, request.for_display)) {
        response.result =
            refuse(outcome_code::not_subscribed,
                   {.entity = "ticks", .field = "provider", .value = request.provider_code});
        return response;
    }
    if (auto early = span_reply(request.coordinates, request.from_instant, request.to_instant, now))
        return *early;

    // The stream is named by the series' one spelling, so a point URI or another spelling of
    // the series reads the same stream.
    const auto series = datum::oresmd_uri_codec::read(request.series_uri)
                            .and_then([](const datum::market_datum& named) {
                                return datum::oresmd_uri_codec::write(datum::series_of(named));
                            });
    if (!series) {
        response.result =
            refuse(outcome_code::uri_invalid, {.field = "series_uri", .reason = series.error()});
        return response;
    }
    return read_stream(ctx,
                       datum::stream_id(request.provider_code, *series),
                       request.from_instant,
                       request.to_instant.value_or(now),
                       request.coordinates);
}

}
