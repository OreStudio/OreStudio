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
#include "ores.marketdata.core/service/fixing_reader.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.marketdata.core/repository/fixing_read_repository.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <optional>
#include <set>
#include <string>
#include <variant>

namespace ores::marketdata::service {

using ores::database::domain::outcome_code;
using ores::database::domain::refuse;
using ores::utility::domain::result;

namespace {

/// The context a read of @p party_id sees, or the refusal of a party it may not read.
std::variant<ores::database::context, result> scope_to_party(ores::database::context ctx,
                                                             const std::string& party_id) {
    if (party_id.empty())
        return ctx;
    boost::uuids::uuid named;
    try {
        named = boost::uuids::string_generator{}(party_id);
    } catch (const std::exception&) {
        return refuse(outcome_code::filter_range_invalid, {.field = "party_id", .value = party_id});
    }
    // A service holds no party of its own, so it names the party whose data it reads. A
    // session that sees parties narrows to the one it names, and only to one it can see.
    if (!ctx.visible_party_ids().empty() &&
        std::ranges::find(ctx.visible_party_ids(), named) == ctx.visible_party_ids().end())
        return refuse(outcome_code::party_required, {.entity = "fixings"});
    return ctx.with_party(ctx.tenant_id(), named, {named}, ctx.service_account());
}

/// The one series an item names, or the refusal of a read that cannot name one.
std::variant<repository::fixing_series, result> resolve(ores::database::context ctx,
                                                        const std::string& item,
                                                        const std::string& provider_code,
                                                        const std::string& party_id) {
    const auto scoped = scope_to_party(std::move(ctx), party_id);
    if (const auto* refused = std::get_if<result>(&scoped))
        return *refused;
    ctx = std::get<ores::database::context>(scoped);
    // The party policy shows a session with no party no fixing, so an empty reply would look
    // like an item with no fixings.
    if (ctx.visible_party_ids().empty())
        return refuse(outcome_code::party_required, {.entity = "fixings"});
    const auto series = repository::fixing_read_repository{}.series_of(ctx, item, provider_code);
    if (series.empty())
        return refuse(outcome_code::not_found, {.entity = "fixing item " + item});
    if (series.size() > 1) {
        // One provider can reach a session through two parties, so the refusal names both
        // counts; naming the provider settles only the second.
        std::set<std::string> parties;
        std::set<std::string> providers;
        for (const auto& s : series) {
            parties.insert(boost::uuids::to_string(s.party_id));
            providers.insert(s.provider_code);
        }
        std::string list;
        for (const auto& p : providers)
            list += (list.empty() ? "" : ", ") + p;
        return refuse(outcome_code::series_ambiguous,
                      {.entity = "fixings",
                       .value = std::to_string(series.size()),
                       .reason = std::to_string(parties.size()) + " parties; providers " + list});
    }
    return series.front();
}

template <typename Response>
void name_the_source(ores::database::context ctx,
                     const std::string& item,
                     const repository::fixing_series& series,
                     Response& response) {
    response.provider_code = series.provider_code;
    if (const auto vendor =
            repository::fixing_read_repository{}.vendor_of(ctx, item, series.provider_code)) {
        response.vendor_scheme = vendor->scheme;
        response.vendor_id = vendor->id;
    }
}

}

messaging::get_fixing_response fixing_reader::latest(ores::database::context ctx,
                                                     const messaging::get_fixing_request& request) {
    messaging::get_fixing_response response;
    if (!request.on_or_before.ok()) {
        response.result =
            refuse(outcome_code::filter_range_invalid, {.field = "on_or_before", .value = "date"});
        return response;
    }
    const auto resolved = resolve(ctx, request.item, request.provider_code, request.party_id);
    if (const auto* refused = std::get_if<result>(&resolved)) {
        response.result = *refused;
        return response;
    }
    const auto& series = std::get<repository::fixing_series>(resolved);
    name_the_source(ctx, request.item, series, response);

    const auto found =
        repository::fixing_read_repository{}.on_or_before(ctx, series.id, request.on_or_before, 1);
    if (found.empty())
        return response;
    const auto& f = found.front();
    response.fixing_date = f.date;
    response.value = f.value;
    response.source = f.source;
    response.received_at = f.received_at;
    return response;
}

messaging::get_fixing_window_response
fixing_reader::window(ores::database::context ctx,
                      const messaging::get_fixing_window_request& request) {
    messaging::get_fixing_window_response response;
    response.days = request.days;
    if (!request.to.ok()) {
        response.result =
            refuse(outcome_code::filter_range_invalid, {.field = "to", .value = "date"});
        return response;
    }
    if (request.days < 1) {
        response.result = refuse(outcome_code::range_inverted, {.entity = "fixings"});
        return response;
    }
    if (request.days > max_days) {
        response.result =
            refuse(outcome_code::range_too_large,
                   {.entity = "fixings", .limit = std::to_string(max_days) + " days"});
        return response;
    }
    const auto resolved = resolve(ctx, request.item, request.provider_code, request.party_id);
    if (const auto* refused = std::get_if<result>(&resolved)) {
        response.result = *refused;
        return response;
    }
    const auto& series = std::get<repository::fixing_series>(resolved);
    name_the_source(ctx, request.item, series, response);

    // The window is the days that end on the last date, both ends included; a day with no
    // fixing, a weekend or a holiday, is skipped. The days are between 1 and max_days here,
    // so the first date is always a real date.
    const auto from = std::chrono::year_month_day(std::chrono::sys_days(request.to) -
                                                  std::chrono::days(request.days - 1));
    const auto found = repository::fixing_read_repository{}.between(
        ctx, series.id, from, request.to, static_cast<std::size_t>(request.days));
    for (const auto& f : found) {
        response.dates.push_back(f.date);
        response.values.push_back(f.value);
        response.sources.push_back(f.source);
        response.received_at.push_back(f.received_at);
    }
    response.used = static_cast<int>(found.size());
    return response;
}

}
