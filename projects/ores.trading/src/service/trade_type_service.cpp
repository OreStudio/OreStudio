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
#include "ores.trading/service/trade_type_service.hpp"

#include <stdexcept>

namespace ores::trading::service {

using namespace ores::logging;

trade_type_service::trade_type_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::trade_type> trade_type_service::list_types() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all trade types";
    return repo_.read_latest(ctx_);
}

std::optional<domain::trade_type>
trade_type_service::find_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding trade type: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void trade_type_service::save_type(const domain::trade_type& v) {
    if (v.code.empty())
        throw std::invalid_argument("Trade Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving trade type: " << v.code;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved trade type: " << v.code;
}

void trade_type_service::save_types(const std::vector<domain::trade_type>& types) {
    for (const auto& t : types) {
        if (t.code.empty())
            throw std::invalid_argument("Trade Type code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << types.size() << " trade types";
    repo_.write(ctx_, types);
}

void trade_type_service::remove_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade type: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed trade type: " << code;
}

void trade_type_service::remove_types(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::trade_type>
trade_type_service::get_type_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for trade type: " << code;
    return repo_.read_all(ctx_, code);
}

}
