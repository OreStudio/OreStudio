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
#include "ores.trading/service/trade_party_role_service.hpp"

#include <stdexcept>

namespace ores::trading::service {

using namespace ores::logging;

trade_party_role_service::trade_party_role_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::trade_party_role> trade_party_role_service::list_roles() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all trade party roles";
    return repo_.read_latest(ctx_);
}

std::optional<domain::trade_party_role>
trade_party_role_service::find_role(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding trade party role: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void trade_party_role_service::save_role(const domain::trade_party_role& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Trade Party Role id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving trade party role: " << v.id;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved trade party role: " << v.id;
}

void trade_party_role_service::save_roles(
    const std::vector<domain::trade_party_role>& roles) {
    for (const auto& r : roles) {
        if (r.id.is_nil())
            throw std::invalid_argument("Trade Party Role id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << roles.size() << " trade party roles";
    repo_.write(ctx_, roles);
}

void trade_party_role_service::remove_role(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade party role: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed trade party role: " << id;
}

std::vector<domain::trade_party_role>
trade_party_role_service::get_role_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for trade party role: " << id;
    return repo_.read_all(ctx_, id);
}

}
