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
#include "ores.refdata.core/service/currency_pair_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

currency_pair_service::currency_pair_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::currency_pair> currency_pair_service::list_pairs(std::uint32_t offset,
                                                                     std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currency pairs";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t currency_pair_service::count_pairs() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total currency pairs count";
    return repo_.get_total_pair_count(ctx_);
}


std::optional<domain::currency_pair> currency_pair_service::get_pair(const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency pair: " << pair_code;
    auto results = repo_.read_latest(ctx_, pair_code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void currency_pair_service::save_pair(const domain::currency_pair& v) {
    if (v.pair_code.empty())
        throw std::invalid_argument("Currency Pair pair_code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving currency pair: " << v.pair_code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved currency pair: " << v.pair_code;
}

void currency_pair_service::save_pairs(const std::vector<domain::currency_pair>& pairs) {
    for (const auto& e : pairs)
        if (e.pair_code.empty())
            throw std::invalid_argument("Currency Pair pair_code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << pairs.size() << " currency pairs";
    auto ts = pairs;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void currency_pair_service::delete_pair(const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency pair: " << pair_code;
    repo_.remove(ctx_, pair_code);
    BOOST_LOG_SEV(lg(), info) << "Removed currency pair: " << pair_code;
}

void currency_pair_service::delete_pairs(const std::vector<std::string>& pair_codes) {
    repo_.remove(ctx_, pair_codes);
}

std::vector<domain::currency_pair>
currency_pair_service::get_pair_history(const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for currency pair: " << pair_code;
    return repo_.read_all(ctx_, pair_code);
}

}
