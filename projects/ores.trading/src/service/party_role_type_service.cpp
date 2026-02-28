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
#include "ores.trading/service/party_role_type_service.hpp"

#include <stdexcept>

namespace ores::trading::service {

using namespace ores::logging;

party_role_type_service::party_role_type_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::party_role_type> party_role_type_service::list_role_types() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all party role types";
    return repo_.read_latest(ctx_);
}

std::optional<domain::party_role_type>
party_role_type_service::find_role_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding party role type: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void party_role_type_service::save_role_type(const domain::party_role_type& v) {
    if (v.code.empty())
        throw std::invalid_argument("Party Role Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving party role type: " << v.code;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved party role type: " << v.code;
}

void party_role_type_service::save_role_types(
    const std::vector<domain::party_role_type>& role_types) {
    for (const auto& rt : role_types) {
        if (rt.code.empty())
            throw std::invalid_argument("Party Role Type code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << role_types.size() << " party role types";
    repo_.write(ctx_, role_types);
}

void party_role_type_service::remove_role_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party role type: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed party role type: " << code;
}

std::vector<domain::party_role_type>
party_role_type_service::get_role_type_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for party role type: " << code;
    return repo_.read_all(ctx_, code);
}

}
