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
#include "ores.database/domain/outcome_code.hpp"
#include "ores.marketdata.api/datum/series_stream.hpp"
#include "ores.marketdata.core/repository/market_series_identity_reader.hpp"
#include "ores.marketdata.core/repository/tick_entitlement.hpp"
#include "ores.marketdata.core/repository/tick_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <set>
#include <string>

namespace ores::marketdata::service {

using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

messaging::get_series_ticks_response
series_tick_reader::read(ores::database::context ctx,
                         const messaging::get_series_ticks_request& request,
                         std::chrono::system_clock::time_point now) {
    messaging::get_series_ticks_response response;

    // The store filters no tick by party, so a session with no party has no door to it and an
    // empty reply would look like a series with no ticks.
    if (ctx.visible_party_ids().empty()) {
        response.result = refuse(outcome_code::party_required, {.entity = "ticks"});
        return response;
    }

    if (request.coordinates.size() > max_coordinates) {
        response.result =
            refuse(outcome_code::filter_too_large,
                   {.field = "coordinates", .limit = std::to_string(max_coordinates)});
        return response;
    }

    const auto to = request.to_instant.value_or(now);
    if (to < request.from_instant) {
        // A span with no end runs to now. A client clock a little ahead of the service puts its
        // start after now, and nothing has ticked after now, so that span is empty and not wrong.
        if (!request.to_instant)
            return response;
        response.result = refuse(outcome_code::range_inverted, {.entity = "ticks"});
        return response;
    }

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

    // One tick more than the ceiling tells a span over it from a span at it.
    const auto& resolved = series.front();

    // The tick store is shared by every party and no policy filters it by party, so the
    // subscription is the door to it. The history returns raw values, so the party must hold
    // display or non-display; may_read_ticks states the rule once for every read of ticks.
    if (!ctx.party_id()) {
        response.result = refuse(outcome_code::party_required, {.entity = "ticks"});
        return response;
    }
    if (!repository::may_read_ticks(ctx, resolved.provider_code)) {
        response.result = refuse(outcome_code::not_subscribed,
                                 {.entity = "ticks",
                                  .field = "provider",
                                  .value = resolved.provider_code});
        return response;
    }

    // The stream is named by the provider and the series URI, as the ingest names it, and no
    // table maps a series to a stream.
    const auto stream = datum::stream_id(resolved.provider_code, resolved.oresmd_uri);
    const auto ticks = repository::tick_repository{}.read_range(
        ctx, stream, request.from_instant, to, request.coordinates, max_ticks + 1);
    if (ticks.size() > max_ticks) {
        response.result = refuse(outcome_code::range_too_large,
                                 {.entity = "ticks", .limit = std::to_string(max_ticks)});
        return response;
    }

    std::map<std::string, int> index_of;
    response.gaps_us.reserve(ticks.size());
    response.coordinate_indexes.reserve(ticks.size());
    response.values.reserve(ticks.size());
    auto previous = ticks.empty() ? request.from_instant : ticks.front().at;
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
