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
#include "ores.refdata.core/service/party_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

party_service::party_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::party> party_service::list_parties(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all parties";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t party_service::count_parties() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total parties count";
    return repo_.get_total_party_count(ctx_);
}

std::optional<domain::party> party_service::get_party_at_version(const std::string& id,
                                                                 std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting party at version: " << id << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::party> party_service::get_party(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting party: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void party_service::save_party(const domain::party& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Party id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving party: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved party: " << v.id;
}

void party_service::save_parties(const std::vector<domain::party>& parties) {
    for (const auto& e : parties)
        if (e.id.is_nil())
            throw std::invalid_argument("Party id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << parties.size() << " parties";
    auto ts = parties;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void party_service::delete_party(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed party: " << id;
}

void party_service::delete_parties(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::party> party_service::get_party_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for party: " << id;
    return repo_.read_all(ctx_, id);
}

std::vector<ores::utility::domain::hierarchy_node>
party_service::get_hierarchy(const boost::uuids::uuid& root_id, bool from_root) {
    BOOST_LOG_SEV(lg(), debug) << "Getting hierarchy for party root: " << root_id;
    auto rows = repo_.get_hierarchy(ctx_, root_id, from_root);
    return ores::utility::domain::build_tree(rows);
}

}
