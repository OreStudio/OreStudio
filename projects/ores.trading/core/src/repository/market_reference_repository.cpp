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
#include "ores.trading.core/repository/market_reference_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <string>

namespace ores::trading::repository {

namespace {

/*
 * The address each instrument row holds, read for the set and read at the
 * caller's transaction time. The function owns which columns count, so this
 * read says nothing about the instrument model.
 */
constexpr auto references_query = "select distinct r.oresmd_uri"
                                  " from ores_trading_market_references_fn("
                                  "     $1::uuid,"
                                  "     array(select jsonb_array_elements_text($2::jsonb)::uuid),"
                                  "     $3::timestamptz) r"
                                  " order by r.oresmd_uri";

}

std::vector<std::string>
market_reference_repository::read(const context& ctx,
                                  const std::vector<std::string>& trade_ids,
                                  const std::optional<std::string>& as_of) {
    std::vector<std::string> out;
    if (trade_ids.empty())
        return out;

    // The run states a business date, and a trade booked during that day is one the run
    // prices. Reading at the day's first instant would leave out every trade booked since, so
    // a bare date is read at the close of its day.
    std::optional<std::string> read_at = as_of;
    if (as_of && as_of->size() == 10 && as_of->find('T') == std::string::npos)
        read_at = *as_of + "T23:59:59.999999Z";

    const std::vector<std::optional<std::string>> params{
        ctx.tenant_id().to_string(), ores::utility::codec::json::write(trade_ids), read_at};

    for (const auto& row : ores::database::query_builder::execute_parameterized_multi_column_query(
             ctx, references_query, params, lg(), "Reading the market data a trade set needs"))
        if (!row.empty() && row[0])
            out.push_back(*row[0]);
    return out;
}

}
