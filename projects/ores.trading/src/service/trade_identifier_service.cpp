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
#include "ores.trading/service/trade_identifier_service.hpp"

#include <stdexcept>

namespace ores::trading::service {

using namespace ores::logging;

trade_identifier_service::trade_identifier_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::trade_identifier> trade_identifier_service::list_identifiers() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all trade identifiers";
    return repo_.read_latest(ctx_);
}

std::optional<domain::trade_identifier>
trade_identifier_service::find_identifier(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding trade identifier: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void trade_identifier_service::save_identifier(const domain::trade_identifier& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Trade Identifier id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving trade identifier: " << v.id;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved trade identifier: " << v.id;
}

void trade_identifier_service::remove_identifier(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade identifier: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed trade identifier: " << id;
}

std::vector<domain::trade_identifier>
trade_identifier_service::get_identifier_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for trade identifier: " << id;
    return repo_.read_all(ctx_, id);
}

}
