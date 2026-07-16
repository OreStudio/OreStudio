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
#include "ores.refdata.core/service/party_id_scheme_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

party_id_scheme_service::party_id_scheme_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::party_id_scheme> party_id_scheme_service::list_schemes(std::uint32_t offset,
                                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all party ID schemes";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t party_id_scheme_service::count_schemes() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total party ID schemes count";
    return repo_.get_total_scheme_count(ctx_);
}

std::optional<domain::party_id_scheme>
party_id_scheme_service::get_scheme_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting party ID scheme at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::party_id_scheme>
party_id_scheme_service::get_scheme(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting party ID scheme: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void party_id_scheme_service::save_scheme(const domain::party_id_scheme& v) {
    if (v.code.empty())
        throw std::invalid_argument("Party ID Scheme code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving party ID scheme: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved party ID scheme: " << v.code;
}

void party_id_scheme_service::save_schemes(const std::vector<domain::party_id_scheme>& schemes) {
    for (const auto& e : schemes)
        if (e.code.empty())
            throw std::invalid_argument("Party ID Scheme code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << schemes.size() << " party ID schemes";
    auto ts = schemes;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void party_id_scheme_service::delete_scheme(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party ID scheme: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed party ID scheme: " << code;
}

void party_id_scheme_service::delete_schemes(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::party_id_scheme>
party_id_scheme_service::get_scheme_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for party ID scheme: " << code;
    return repo_.read_all(ctx_, code);
}

}
