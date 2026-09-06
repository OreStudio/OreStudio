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
#include "ores.trading.core/service/trade_party_role_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

trade_party_role_service::trade_party_role_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::trade_party_role> trade_party_role_service::list_roles(std::uint32_t offset,
                                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all trade party roles";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t trade_party_role_service::count_roles() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total trade party roles count";
    return repo_.get_total_role_count(ctx_);
}


std::optional<domain::trade_party_role>
trade_party_role_service::get_role_at_version(const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting trade party role at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::trade_party_role> trade_party_role_service::get_role(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting trade party role. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void trade_party_role_service::save_role(const domain::trade_party_role& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Trade Party Role id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving trade party role. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved trade party role. " << "id: " << v.id;
}

void trade_party_role_service::save_roles(const std::vector<domain::trade_party_role>& roles) {
    for (const auto& e : roles) {
        if (e.id.is_nil())
            throw std::invalid_argument("Trade Party Role id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << roles.size() << " trade party roles";
    auto ts = roles;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void trade_party_role_service::delete_role(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade party role. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed trade party role. " << "id: " << id;
}

void trade_party_role_service::delete_roles(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::trade_party_role>
trade_party_role_service::get_role_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for trade party role. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
