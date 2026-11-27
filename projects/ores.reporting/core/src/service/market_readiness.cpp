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
#include "ores.reporting.core/service/market_readiness.hpp"
#include <algorithm>
#include <map>
#include <set>

namespace ores::reporting::service {

namespace {

/*
 * The trades in the order they were named, each once. A caller that names a
 * trade twice is a caller with a duplicate somewhere upstream, and counting it
 * twice would break the partition the record exists to state.
 */
std::vector<std::string> distinct_in_order(const std::vector<std::string>& trade_ids) {
    std::vector<std::string> out;
    std::set<std::string> seen;
    out.reserve(trade_ids.size());
    for (const auto& id : trade_ids)
        if (seen.insert(id).second)
            out.push_back(id);
    return out;
}

}

market_readiness decide_market_readiness(const std::vector<std::string>& trade_ids,
                                         const std::vector<trade_requirement>& requirements,
                                         const std::vector<address_answer>& answers) {
    std::map<std::string, address_answer> answer_of;
    for (const auto& answer : answers)
        answer_of[answer.oresmd_uri] = answer;

    std::map<std::string, const trade_requirement*> requirement_of;
    for (const auto& one : requirements)
        requirement_of[one.trade_id] = &one;

    market_readiness out;
    const auto booked = distinct_in_order(trade_ids);
    out.booked = booked.size();

    std::set<std::string> addresses;
    for (const auto& trade_id : booked) {
        const auto found = requirement_of.find(trade_id);
        if (found == requirement_of.end())
            throw std::invalid_argument("market readiness: the trade '" + trade_id +
                                        "' is in the scope and has no requirement entry");

        const auto& needed = found->second->oresmd_uris;
        trade_readiness entry{.trade_id = trade_id};
        for (const auto& uri : needed) {
            const auto answer = answer_of.find(uri);
            if (answer == answer_of.end())
                throw std::invalid_argument("market readiness: the trade '" + trade_id +
                                            "' names the address '" + uri +
                                            "', which the market read did not answer");
            addresses.insert(uri);
            if (answer->second.cause.empty())
                continue;
            ++entry.gap_count;
        }
        entry.complete = entry.gap_count == 0;
        if (entry.complete) {
            ++out.complete;
            out.survivors.push_back(trade_id);
        }
        out.trades.push_back(std::move(entry));
    }
    out.excluded = out.booked - out.complete;

    out.addresses.reserve(addresses.size());
    for (const auto& uri : addresses) {
        const auto& answer = answer_of.at(uri);
        out.addresses.push_back(address_readiness{
            .oresmd_uri = uri, .series_id = answer.series_id, .gap_cause = answer.cause});
    }
    return out;
}

}
