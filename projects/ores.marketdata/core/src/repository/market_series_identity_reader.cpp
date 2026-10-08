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
#include "ores.marketdata.core/repository/market_series_identity_reader.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_series_entity.hpp"
#include "ores.marketdata.core/repository/market_series_mapper.hpp"
#include <algorithm>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::repository {

namespace {

auto& reader_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.repository.market_series_identity_reader");
    return instance;
}

/// ORE writes its instrument and quote types in upper case, in the key and in
/// the URI both, so a request may state either.
std::string upper_text(const std::string& text) {
    std::string out(text);
    std::ranges::transform(
        out, out.begin(), [](char c) { return c >= 'a' && c <= 'z' ? char(c - 'a' + 'A') : c; });
    return out;
}

/// A row matches when its column begins with the identity's text. The prefix is
/// the codec's own spelling of a series identity, so this narrows by the
/// grammar rather than by a pattern this read invents.
sqlgen::dynamic::Condition begins_with(const std::string& column, const std::string& prefix) {
    return {.val = sqlgen::dynamic::Condition::Like{
                .op = {.val = sqlgen::dynamic::Column{.name = column}},
                .pattern = {.val = sqlgen::dynamic::String{.val = prefix}}}};
}

}

std::vector<domain::market_series>
market_series_identity_reader::read(
    ores::database::context ctx,
    const messaging::resolve_series_identity_request& identity) {
    using namespace ores::marketdata::datum;
    using namespace ores::database::repository;
    using namespace sqlgen;
    using namespace sqlgen::literals;

    const auto type = instrument_type_named(upper_text(identity.instrument_type));
    if (!type)
        throw std::invalid_argument("'" + identity.instrument_type +
                                    "' is not an ORE instrument type");
    const auto quote = quote_type_named(upper_text(identity.quote_type));
    if (!quote)
        throw std::invalid_argument("'" + identity.quote_type + "' is not an ORE quote type");

    std::vector<field_value> stated;
    stated.reserve(identity.fields.size());
    for (const auto& f : identity.fields) {
        const auto name = field_named(f.name);
        if (!name)
            throw std::invalid_argument("'" + f.name + "' is not an oresmd field name");
        stated.push_back({*name, value{std::string(f.text)}});
    }

    const auto prefix = oresmd_uri_codec::series_prefix(*type, *quote, stated);
    if (!prefix)
        throw std::invalid_argument(prefix.error());

    static const auto max(make_timestamp(MAX_TIMESTAMP, reader_lg()));
    const auto tid = ctx.tenant_id().to_string();

    std::vector<sqlgen::dynamic::Condition> conditions{
        equals("tenant_id", filter_value(tid)),
        equals("valid_to", filter_value(max.value())),
        begins_with("oresmd_uri", *prefix + "%")};
    if (!identity.party_id.empty())
        conditions.push_back(equals("party_id", filter_value(identity.party_id)));

    const auto query = sqlgen::read<std::vector<market_series_entity>> |
                       where(all_of(std::move(conditions)).value()) | order_by("id"_c);
    auto candidates = execute_read_query<market_series_entity, domain::market_series>(
        ctx,
        query,
        [](const auto& entities) { return market_series_mapper::map(entities); },
        reader_lg(),
        "Reading market series by typed identity");

    /*
     * The prefix reached the fields the URI grammar fixes in place. Every other
     * field sits in the row's order, which differs between instrument types, so
     * the codec reads the candidate and the codec decides whether it carries
     * what the request stated. A row whose URI does not read, or reads as
     * another type, is not a series this identity names.
     */
    const auto holds = [&](const domain::market_series& s) {
        const auto datum = oresmd_uri_codec::read(s.oresmd_uri);
        if (!datum || datum->type() != *type || datum->quote() != *quote)
            return false;
        return std::ranges::all_of(stated, [&](const field_value& want) {
            const auto* held = datum->get(want.name);
            return held != nullptr && held->held == want.held;
        });
    };

    const auto kept = std::ranges::remove_if(candidates, std::not_fn(holds));
    candidates.erase(kept.begin(), kept.end());
    return candidates;
}

}
