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
#include "ores.trading.core/service/party_role_type_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

party_role_type_service::party_role_type_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::party_role_type> party_role_type_service::list_role_types(std::uint32_t offset,
                                                                              std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all party role types";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t party_role_type_service::count_role_types() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total party role types count";
    return repo_.get_total_role_type_count(ctx_);
}


std::optional<domain::party_role_type>
party_role_type_service::get_role_type_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting party role type at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::party_role_type>
party_role_type_service::get_role_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting party role type. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void party_role_type_service::save_role_type(const domain::party_role_type& v) {
    if (v.code.empty())
        throw std::invalid_argument("Party Role Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving party role type. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved party role type. " << "code: " << v.code;
}

void party_role_type_service::save_role_types(
    const std::vector<domain::party_role_type>& role_types) {
    for (const auto& e : role_types) {
        if (e.code.empty())
            throw std::invalid_argument("Party Role Type code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << role_types.size() << " party role types";
    auto ts = role_types;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void party_role_type_service::delete_role_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party role type. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed party role type. " << "code: " << code;
}

void party_role_type_service::delete_role_types(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::party_role_type>
party_role_type_service::get_role_type_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for party role type. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
