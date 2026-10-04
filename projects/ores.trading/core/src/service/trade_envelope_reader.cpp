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
#include "ores.trading.core/service/trade_envelope_reader.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <unordered_set>
#include <utility>

namespace ores::trading::service {

using namespace ores::logging;

namespace {

std::string to_uuid_array(const std::vector<std::string>& ids) {
    std::string r = "{";
    for (std::size_t i = 0; i < ids.size(); ++i) {
        if (i != 0)
            r += ',';
        r += ids[i];
    }
    return r + "}";
}

}

trade_envelope_reader::trade_envelope_reader(context ctx)
    : ctx_(std::move(ctx)) {}

std::unordered_map<std::string, domain::trade_envelope_data>
trade_envelope_reader::read_envelopes(const std::vector<std::string>& trade_ids) const {
    std::unordered_map<std::string, domain::trade_envelope_data> result;
    if (trade_ids.empty())
        return result;

    for (const auto& row : repository::read_envelopes_by_trade_ids(ctx_, trade_ids)) {
        domain::trade_envelope_data data;
        data.counter_party = row.counter_party;
        data.netting_set_id = row.netting_set_id;
        if (row.has_portfolio_ids)
            data.portfolio_ids = std::vector<std::string>{};
        if (row.has_additional_fields)
            data.additional_fields = std::vector<domain::trade_envelope_field>{};
        result.emplace(boost::uuids::to_string(row.trade_id), std::move(data));
    }
    for (auto& child : repository::read_portfolio_ids_by_trade_ids(ctx_, trade_ids)) {
        auto it = result.find(boost::uuids::to_string(child.trade_id));
        if (it == result.end() || !it->second.portfolio_ids)
            continue;
        it->second.portfolio_ids->push_back(std::move(child.portfolio_id));
    }

    for (auto& child : repository::read_additional_fields_by_trade_ids(ctx_, trade_ids)) {
        auto it = result.find(boost::uuids::to_string(child.trade_id));
        if (it == result.end() || !it->second.additional_fields)
            continue;
        it->second.additional_fields->push_back(
            {.name = std::move(child.name), .value = std::move(child.value)});
    }

    read_booked_envelopes(trade_ids, result);

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " trade envelopes.";
    return result;
}

void trade_envelope_reader::read_booked_envelopes(
    const std::vector<std::string>& trade_ids,
    std::unordered_map<std::string, domain::trade_envelope_data>& envelopes) const {
    using database::repository::execute_parameterized_multi_column_query;

    const auto ids = to_uuid_array(trade_ids);
    std::unordered_set<std::string> booked;
    for (const auto& row : execute_parameterized_multi_column_query(
             ctx_,
             "SELECT trade_id::text, counter_party, netting_set_id "
             "FROM ores_trading_trade_envelope_names_fn($1::uuid[])",
             {ids},
             lg(),
             "Reading the envelope names of booked trades.")) {
        auto& data = envelopes[*row[0]];
        data.counter_party = row[1];
        data.netting_set_id = row[2];
        data.additional_fields.reset();
        booked.insert(*row[0]);
    }
    if (booked.empty())
        return;

    for (const auto& row : execute_parameterized_multi_column_query(
             ctx_,
             "SELECT trade_id::text, name, value FROM ores_trading_trade_additional_fields_tbl "
             "WHERE tenant_id = ores_iam_current_tenant_id_fn() "
             "AND valid_to = ores_utility_infinity_timestamp_fn() "
             "AND trade_id = ANY($1::uuid[]) ORDER BY trade_id, sequence_number",
             {ids},
             lg(),
             "Reading the additional fields of booked trades.")) {
        if (!booked.contains(*row[0]))
            continue;
        auto& fields = envelopes[*row[0]].additional_fields;
        if (!fields)
            fields.emplace();
        fields->push_back({.name = row[1].value_or(""), .value = row[2].value_or("")});
    }
}

}
