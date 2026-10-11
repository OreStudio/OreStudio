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
#include "ores.marketdata.core/service/series_identity_lister.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_series_identity_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/series_axis_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

namespace ores::marketdata::service {

using namespace ores::logging;

namespace {

inline std::string_view logger_name = "ores.marketdata.core.series_identity_lister";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/// The typed identity a series URI states, or nothing when the URI is not one a
/// curve read can name.
std::optional<messaging::resolve_series_identity_request>
identity_of(const domain::market_series& series) {
    using namespace ores::marketdata::datum;

    const auto parsed = oresmd_uri_codec::read(series.oresmd_uri);
    if (!parsed || !parsed->is_series())
        return std::nullopt;

    const auto& row = schema_of(parsed->type());
    messaging::resolve_series_identity_request identity;
    identity.asset = std::string(name_of(row.asset));
    identity.instrument_type = oresmd_uri_codec::instrument_spelling(parsed->type());
    identity.quote_type = oresmd_uri_codec::quote_spelling(parsed->quote());
    identity.party_id = boost::uuids::to_string(series.party_id);

    // The subject is the scope and every other identity field the series holds is
    // a field of the identity. A field that holds none is not stated.
    for (const auto& held : parsed->fields()) {
        if (std::holds_alternative<none_t>(held.held))
            continue;
        if (held.name == row.subject)
            identity.scope = text_of(held.held);
        else
            identity.fields.push_back({std::string(name_of(held.name)), text_of(held.held)});
    }
    return identity;
}

/// The declared axes of each series, in order, from one read.
std::map<std::string, std::vector<std::string>>
axes_of(ores::database::context ctx, const std::vector<domain::market_series>& series) {
    std::vector<std::string> ids;
    ids.reserve(series.size());
    for (const auto& s : series)
        ids.push_back(boost::uuids::to_string(s.id));

    std::map<std::string, std::vector<std::pair<int, std::string>>> ordered;
    for (const auto& axis : repository::series_axis_repository{}.read_latest_for_series(ctx, ids))
        ordered[boost::uuids::to_string(axis.series_id)].emplace_back(axis.sequence,
                                                                      axis.axis_field);

    std::map<std::string, std::vector<std::string>> result;
    for (auto& [id, axes] : ordered) {
        std::ranges::sort(axes);
        for (auto& axis : axes)
            result[id].push_back(std::move(axis.second));
    }
    return result;
}

}

messaging::list_series_identities_response
series_identity_lister::list(ores::database::context ctx,
                             const messaging::list_series_identities_request& request) {
    messaging::list_series_identities_response response;

    // What a party may see is the identity projection's rule: the projection is
    // isolated by party, while the series table is isolated by tenant alone. So the
    // projection names the visible series and the table supplies their rows.
    std::vector<std::string> visible_ids;
    for (const auto& identity : repository::market_series_identity_repository{}.read_latest(ctx)) {
        if (identity.identity_kind == "series")
            visible_ids.push_back(boost::uuids::to_string(identity.series_id));
    }
    auto visible = visible_ids.empty() ?
                       std::vector<domain::market_series>{} :
                       repository::market_series_repository{}.read_latest(ctx, visible_ids);
    if (!request.party_id.empty()) {
        std::erase_if(visible, [&](const domain::market_series& s) {
            return boost::uuids::to_string(s.party_id) != request.party_id;
        });
    }

    const auto axes = axes_of(ctx, visible);
    for (const auto& series : visible) {
        auto identity = identity_of(series);
        if (!identity)
            continue;
        messaging::listed_series row;
        row.series_id = boost::uuids::to_string(series.id);
        row.identity = std::move(*identity);
        if (const auto found = axes.find(row.series_id); found != axes.end())
            row.axes = found->second;
        response.series.push_back(std::move(row));
    }

    // A stable order, so the same party sees the same list: by what the series is,
    // then by whose it is.
    std::ranges::sort(response.series, [](const auto& a, const auto& b) {
        const auto key = [](const messaging::listed_series& s) {
            std::string fields;
            for (const auto& f : s.identity.fields)
                fields += f.name + "=" + f.text + ";";
            return std::tuple(s.identity.asset,
                              s.identity.scope,
                              s.identity.instrument_type,
                              s.identity.quote_type,
                              fields,
                              s.identity.party_id);
        };
        return key(a) < key(b);
    });

    if (response.series.size() > max_series) {
        response.series.resize(max_series);
        response.truncated = true;
        BOOST_LOG_SEV(lg(), warn) << "The series list was cut at " << max_series;
    }
    response.success = true;
    BOOST_LOG_SEV(lg(), debug) << "Listed " << response.series.size() << " series identities";
    return response;
}

}
