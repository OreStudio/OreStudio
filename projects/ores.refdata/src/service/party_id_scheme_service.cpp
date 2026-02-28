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
#include "ores.refdata/service/party_id_scheme_service.hpp"

#include <stdexcept>

namespace ores::refdata::service {

using namespace ores::logging;

party_id_scheme_service::party_id_scheme_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::party_id_scheme>
party_id_scheme_service::list_schemes() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all party ID schemes";
    return repo_.read_latest(ctx_);
}

std::optional<domain::party_id_scheme>
party_id_scheme_service::find_scheme(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding party ID scheme: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void party_id_scheme_service::save_scheme(
    const domain::party_id_scheme& scheme) {
    if (scheme.code.empty()) {
        throw std::invalid_argument("Party ID scheme code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving party ID scheme: " << scheme.code;
    repo_.write(ctx_, scheme);
    BOOST_LOG_SEV(lg(), info) << "Saved party ID scheme: " << scheme.code;
}

void party_id_scheme_service::save_schemes(
    const std::vector<domain::party_id_scheme>& schemes) {
    for (const auto& s : schemes) {
        if (s.code.empty())
            throw std::invalid_argument("Party ID scheme code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << schemes.size() << " party ID schemes";
    repo_.write(ctx_, schemes);
}

void party_id_scheme_service::remove_scheme(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party ID scheme: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed party ID scheme: " << code;
}

void party_id_scheme_service::remove_schemes(
    const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::party_id_scheme>
party_id_scheme_service::get_scheme_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for party ID scheme: "
                               << code;
    return repo_.read_all(ctx_, code);
}

}
