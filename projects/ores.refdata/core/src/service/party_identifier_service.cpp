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
#include "ores.refdata.core/service/party_identifier_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

party_identifier_service::party_identifier_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::party_identifier>
party_identifier_service::list_party_identifiers(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all party identifiers";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t party_identifier_service::count_party_identifiers() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total party identifiers count";
    return repo_.get_total_party_identifier_count(ctx_);
}

std::vector<domain::party_identifier> party_identifier_service::list_party_identifiers_by_party_id(
    const std::string& party_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing party identifiers by party_id: " << party_id;
    return repo_.read_latest_by_party_id(ctx_, party_id, offset, limit);
}

std::uint32_t
party_identifier_service::count_party_identifiers_by_party_id(const std::string& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total party identifiers count by party_id: " << party_id;
    return repo_.get_total_party_identifier_count_by_party_id(ctx_, party_id);
}

std::optional<domain::party_identifier>
party_identifier_service::get_party_identifier(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting party identifier: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void party_identifier_service::save_party_identifier(const domain::party_identifier& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Party Identifier id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving party identifier: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved party identifier: " << v.id;
}

void party_identifier_service::save_party_identifiers(
    const std::vector<domain::party_identifier>& party_identifiers) {
    for (const auto& e : party_identifiers)
        if (e.id.is_nil())
            throw std::invalid_argument("Party Identifier id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << party_identifiers.size() << " party identifiers";
    auto ts = party_identifiers;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void party_identifier_service::delete_party_identifier(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party identifier: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed party identifier: " << id;
}

void party_identifier_service::delete_party_identifiers(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::party_identifier>
party_identifier_service::get_party_identifier_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for party identifier: " << id;
    return repo_.read_all(ctx_, id);
}

std::optional<domain::party_identifier>
party_identifier_service::get_party_identifier_at_version(const std::string& id,
                                                          std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting party identifier at version: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::vector<domain::party_identifier> party_identifier_service::list_party_identifiers_by_party_as_of(
    const std::string& party_id,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug) << "Listing party identifiers by party as of window: " << party_id;
    return repo_.read_by_party_id_as_of(ctx_, party_id, valid_from_bound, valid_to_bound);
}

}
