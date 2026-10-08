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
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/ore_types.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_series_identity_entity.hpp"
#include "ores.marketdata.core/repository/market_series_identity_mapper.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <stdexcept>
#include <string>
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

/**
 * @brief The projection column that holds @p field.
 *
 * A column is named after its field, with one exception: a field whose name is
 * a word the store refuses has a column name of its own, and =offset= is that
 * field. See =ores.marketdata.market_series_identity= for why, and
 * =build/scripts/check_marketdata_identity_columns.py= for the same mapping.
 */
std::string column_of(datum::field field) {
    auto name = std::string(datum::name_of(field));
    return name == "offset" ? "offset_value" : name;
}

/**
 * @brief Refuses @p name unless @p row declares it as identity, and it is not
 * the subject.
 *
 * A name the type does not declare would reach the store as a column the table
 * has no such field in, so it is refused here rather than left to the database
 * to reject. The subject is refused because the request states it as the scope,
 * and a second spelling of the same field could contradict it.
 */
void check_field(const std::string& name, const datum::schema_row& row) {
    const auto field = datum::field_named(name);
    if (!field)
        throw std::invalid_argument("'" + name + "' is not an oresmd field name");
    const auto declared = std::ranges::any_of(row.fields, [&](const datum::field_spec& spec) {
        return spec.name == *field && spec.role == datum::field_role::identity;
    });
    if (!declared)
        throw std::invalid_argument("the instrument type does not declare '" + name +
                                    "' as one of its identity fields");
    if (*field == row.subject)
        throw std::invalid_argument("'" + name +
                                    "' is the scope; state it in scope rather than in fields");
}

}

std::vector<domain::market_series>
market_series_identity_reader::read(ores::database::context ctx,
                                    const messaging::resolve_series_identity_request& identity) {
    using namespace ores::marketdata::datum;
    using namespace ores::database::repository;
    using namespace sqlgen;
    using namespace sqlgen::literals;

    const auto type = instrument_type_named(upper_text(identity.instrument_type));
    if (!type)
        throw std::invalid_argument("'" + identity.instrument_type +
                                    "' is not an ORE instrument type");
    if (oresmd_uri_codec::instrument_spelling(*type) != identity.instrument_type)
        throw std::invalid_argument("'" + identity.instrument_type +
                                    "' is not how a URI writes that instrument type");

    const auto quote = quote_type_named(upper_text(identity.quote_type));
    if (!quote)
        throw std::invalid_argument("'" + identity.quote_type + "' is not an ORE quote type");
    if (oresmd_uri_codec::quote_spelling(*quote) != identity.quote_type)
        throw std::invalid_argument("'" + identity.quote_type +
                                    "' is not how a URI writes that quote type");

    const auto& row = schema_of(*type);
    if (std::string(name_of(row.asset)) != identity.asset)
        throw std::invalid_argument("a " + identity.instrument_type +
                                    " belongs to the asset class " +
                                    std::string(name_of(row.asset)) + ", not " + identity.asset);

    /*
     * The projection holds one row per series with a column per identity field,
     * so the identity is a filter rather than a parse. The scope is the value
     * the URI writes in its path, which is the subject field's column.
     */
    std::vector<sqlgen::dynamic::Condition> conditions{
        equals("tenant_id", filter_value(ctx.tenant_id().to_string())),
        equals("asset_class", filter_value(identity.asset)),
        equals("instrument_type", filter_value(identity.instrument_type)),
        equals("quote_type", filter_value(identity.quote_type)),
        equals(column_of(row.subject), filter_value(identity.scope))};

    for (const auto& f : identity.fields) {
        check_field(f.name, row);
        conditions.push_back(equals(column_of(*field_named(f.name)), filter_value(f.text)));
    }
    if (!identity.party_id.empty())
        conditions.push_back(equals("party_id", filter_value(identity.party_id)));

    /*
     * A condition built at run time reaches a read as its filter, not through
     * the query's own where clause, so the query states the one condition that
     * holds of every series identity and the rest travel beside it.
     */
    const auto query = sqlgen::read<std::vector<market_series_identity_entity>> |
                       where("identity_kind"_c == std::string("series"));
    const auto identities =
        execute_ordered_read_query<market_series_identity_entity, domain::market_series_identity>(
            ctx,
            query,
            make_order({}, false, {"series_id"}),
            all_of(std::move(conditions)),
            [](const auto& entities) { return market_series_identity_mapper::map(entities); },
            reader_lg(),
            "Reading the series identity projection by typed identity");

    if (identities.empty())
        return {};

    /*
     * The projection names the series; the series table holds its row. The
     * second read is by key, so it is one indexed lookup however many identities
     * the first read matched.
     */
    std::vector<std::string> ids;
    ids.reserve(identities.size());
    for (const auto& i : identities)
        ids.push_back(boost::uuids::to_string(i.series_id));
    return market_series_repository{}.read_latest(ctx, ids);
}

}
