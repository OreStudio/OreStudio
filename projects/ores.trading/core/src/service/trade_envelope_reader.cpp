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
#include "shared_helpers.hpp"
#include <algorithm>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::trading::service {

using namespace ores::logging;


trade_envelope_reader::trade_envelope_reader(context ctx)
    : ctx_(std::move(ctx)) {}

std::unordered_map<std::string, domain::trade_envelope_data>
trade_envelope_reader::read_envelopes(const std::vector<std::string>& trade_ids) const {
    using database::repository::execute_parameterized_multi_column_query;

    std::unordered_map<std::string, domain::trade_envelope_data> result;
    if (trade_ids.empty())
        return result;

    // The names a reader sees beside the ids, kept apart from the fields the
    // trade's own source recorded so that a recorded field is never replaced.
    std::unordered_map<std::string, std::vector<std::pair<std::string, std::optional<std::string>>>>
        context;
    std::unordered_map<std::string, std::vector<std::string>> portfolio_names;

    const auto ids = to_uuid_array(trade_ids);
    for (const auto& row : execute_parameterized_multi_column_query(
             ctx_,
             "SELECT trade_id::text, counter_party, netting_set_id, counterparty_name, "
             "counterparty_lei, counterparty_alias, netting_set_code, netting_set_alias, "
             "book_id, book_name FROM ores_trading_trade_envelope_names_fn($1::uuid[])",
             {ids},
             lg(),
             "Reading the envelope names of booked trades.")) {
        auto& data = result[*row[0]];
        data.counter_party = row[1];
        data.netting_set_id = row[2];
        context[*row[0]] = {{"CounterpartyName", row[3]},
                            {"CounterpartyLei", row[4]},
                            {"CounterpartyAlias", row[5]},
                            {"NettingSetCode", row[6]},
                            {"NettingSetAlias", row[7]},
                            {"BookId", row[8]},
                            {"BookName", row[9]}};
    }
    if (result.empty())
        return result;

    for (const auto& row : execute_parameterized_multi_column_query(
             ctx_,
             "SELECT trade_id::text, portfolio_id::text, name "
             "FROM ores_trading_trade_portfolio_names_fn($1::uuid[])",
             {ids},
             lg(),
             "Reading the portfolio names of booked trades.")) {
        const auto it = result.find(*row[0]);
        if (it == result.end())
            continue;
        auto& portfolios = it->second.portfolio_ids;
        if (!portfolios)
            portfolios.emplace();
        portfolios->push_back(row[1].value_or(""));
        portfolio_names[*row[0]].push_back(row[2].value_or(""));
    }

    for (const auto& row : execute_parameterized_multi_column_query(
             ctx_,
             "SELECT trade_id::text, name, value FROM ores_trading_trade_additional_fields_tbl "
             "WHERE tenant_id = ores_iam_current_tenant_id_fn() "
             "AND valid_to = ores_utility_infinity_timestamp_fn() "
             "AND trade_id = ANY($1::uuid[]) ORDER BY trade_id, sequence_number",
             {ids},
             lg(),
             "Reading the additional fields of booked trades.")) {
        const auto it = result.find(*row[0]);
        if (it == result.end())
            continue;
        auto& fields = it->second.additional_fields;
        if (!fields)
            fields.emplace();
        fields->push_back({.name = row[1].value_or(""), .value = row[2].value_or("")});
    }

    for (auto& [trade_id, data] : result) {
        auto& fields = data.additional_fields;
        const auto recorded = [&](const std::string& name) {
            return fields &&
                   std::ranges::any_of(*fields, [&](const auto& f) { return f.name == name; });
        };
        const auto add = [&](const std::string& name, const std::string& value) {
            if (value.empty() || recorded(name))
                return;
            if (!fields)
                fields.emplace();
            fields->push_back({.name = name, .value = value});
        };
        for (const auto& [name, value] : context[trade_id])
            if (value)
                add(name, *value);
        std::string names;
        for (const auto& name : portfolio_names[trade_id])
            names += (names.empty() ? "" : ", ") + name;
        add("PortfolioNames", names);
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " trade envelopes.";
    return result;
}

}
