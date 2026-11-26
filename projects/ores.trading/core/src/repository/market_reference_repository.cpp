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
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <map>
#include <string>

namespace ores::trading::repository {

namespace {

/*
 * The address each instrument row holds, read per trade and read at the caller's transaction
 * time. The function owns which columns count, so this read says nothing about the instrument
 * model.
 */
constexpr auto references_by_trade_query =
    "select r.trade_id::text, r.oresmd_uri"
    " from ores_trading_market_references_fn("
    "     $1::uuid,"
    "     array(select jsonb_array_elements_text($2::jsonb)::uuid),"
    "     $3::timestamptz) r";

/*
 * The run states a business date, and a trade booked during that day is one the run prices.
 * Reading at the day's first instant would leave out every trade booked since, so a bare date
 * is read at the close of its day.
 */
std::optional<std::string> read_at(const std::optional<std::string>& as_of) {
    if (as_of && as_of->size() == 10 && as_of->find('T') == std::string::npos)
        return *as_of + "T23:59:59.999999Z";
    return as_of;
}

}

std::vector<trade_requirement>
market_reference_repository::read_by_trade(const context& ctx,
                                           const std::vector<std::string>& trade_ids,
                                           const std::optional<std::string>& as_of) {
    if (trade_ids.empty())
        return {};

    const std::vector<std::optional<std::string>> params{
        ctx.tenant_id().to_string(), ores::utility::codec::json::write(trade_ids), read_at(as_of)};

    std::vector<market_reference_row> rows;
    for (const auto& row : ores::database::query_builder::execute_parameterized_multi_column_query(
             ctx,
             references_by_trade_query,
             params,
             lg(),
             "Reading the market data each trade needs"))
        if (row.size() >= 2 && row[0] && row[1])
            rows.push_back({.trade_id = *row[0], .oresmd_uri = *row[1]});
    return trade_requirements_from(rows, trade_ids);
}

std::vector<trade_requirement>
trade_requirements_from(const std::vector<market_reference_row>& rows,
                        const std::vector<std::string>& trade_ids) {
    /*
     * The rows carry the identifier the store writes; the request carries the identifier a
     * caller stated, which is the same identifier in any spelling the format allows. They are
     * matched by value so a caller's spelling cannot lose its own answer.
     */
    const auto as_key = [](const std::string& id) -> std::string {
        std::string text;
        text.reserve(id.size());
        for (const auto character : id)
            if (character != '{' && character != '}')
                text += static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
        if (constexpr std::string_view urn = "urn:uuid:"; text.starts_with(urn))
            text.erase(0, urn.size());
        try {
            return boost::uuids::to_string(boost::uuids::string_generator{}(text));
        } catch (const std::exception&) {
            return id;
        }
    };

    std::vector<trade_requirement> answer;
    answer.reserve(trade_ids.size());
    std::map<std::string, std::size_t> index_of;
    for (const auto& trade_id : trade_ids) {
        // A trade the request names twice answers once, on its first place.
        if (!index_of.emplace(as_key(trade_id), answer.size()).second)
            continue;
        answer.push_back({.trade_id = trade_id, .oresmd_uris = {}});
    }
    for (const auto& row : rows) {
        const auto found = index_of.find(as_key(row.trade_id));
        if (found == index_of.end())
            continue;
        auto& uris = answer[found->second].oresmd_uris;
        if (std::ranges::find(uris, row.oresmd_uri) == uris.end())
            uris.push_back(row.oresmd_uri);
    }
    for (auto& trade : answer)
        std::ranges::sort(trade.oresmd_uris);
    return answer;
}

}
