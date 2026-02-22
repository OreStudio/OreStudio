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
#include "ores.refdata/service/currency_market_tier_service.hpp"

#include <stdexcept>

namespace ores::refdata::service {

using namespace ores::logging;

currency_market_tier_service::currency_market_tier_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::currency_market_tier> currency_market_tier_service::list_types() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currency market tiers";
    return repo_.read_latest(ctx_);
}

std::optional<domain::currency_market_tier>
currency_market_tier_service::find_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding currency market tier: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void currency_market_tier_service::save_type(const domain::currency_market_tier& v) {
    if (v.code.empty())
        throw std::invalid_argument("Currency Market Tier code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving currency market tier: " << v.code;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved currency market tier: " << v.code;
}

void currency_market_tier_service::remove_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency market tier: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed currency market tier: " << code;
}

std::vector<domain::currency_market_tier>
currency_market_tier_service::get_type_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for currency market tier: " << code;
    return repo_.read_all(ctx_, code);
}

}
