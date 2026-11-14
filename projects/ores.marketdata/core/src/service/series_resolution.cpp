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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.core/service/series_resolution.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <set>
#include <string>
#include <variant>

namespace ores::marketdata::service {

using ores::database::domain::outcome_code;
using ores::database::domain::refuse;
using ores::utility::domain::result;

namespace {

/// The context a read of @p party_id sees, or the refusal of a party it may not read.
std::variant<ores::database::context, result> scope_to_party(const ores::database::context& ctx,
                                                             const std::string& party_id,
                                                             const std::string& entity) {
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
        return refuse(outcome_code::party_required, {.entity = entity});
    return ctx.with_party(ctx.tenant_id(), named, {named}, ctx.service_account());
}

}

std::variant<repository::fixing_series, result> resolve_series(const ores::database::context& base,
                                                               const std::string& item,
                                                               const std::string& provider_code,
                                                               const std::string& party_id,
                                                               const std::string& entity,
                                                               const std::string& item_name) {
    const auto scoped = scope_to_party(base, party_id, entity);
    if (const auto* refused = std::get_if<result>(&scoped))
        return *refused;
    // The scoped context is the one this read runs as. It is bound once here, so
    // the parameter the caller owns is never written to.
    const auto& ctx = std::get<ores::database::context>(scoped);
    // The party policy shows a session with no party no row, so an empty reply would look
    // like an item with no values.
    if (ctx.visible_party_ids().empty())
        return refuse(outcome_code::party_required, {.entity = entity});
    const auto series = repository::fixing_read_repository{}.series_of(ctx, item, provider_code);
    if (series.empty())
        return refuse(outcome_code::not_found, {.entity = item_name + " " + item});
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
                      {.entity = entity,
                       .value = std::to_string(series.size()),
                       .reason = std::to_string(parties.size()) + " parties; providers " + list});
    }
    return series.front();
}

}
