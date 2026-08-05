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
#include "ores.dq.core/service/lei_relationship_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

lei_relationship_service::lei_relationship_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::lei_relationship>
lei_relationship_service::list_relationships(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all LEI relationships";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t lei_relationship_service::count_relationships() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total LEI relationships count";
    return repo_.get_total_relationship_count(ctx_);
}


std::optional<domain::lei_relationship> lei_relationship_service::get_relationship_at_version(
    const std::string& relationship_start_node_node_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting LEI relationship at version. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id << " version: " << version;
    return repo_.read_at_version(ctx_, relationship_start_node_node_id, version);
}

std::optional<domain::lei_relationship>
lei_relationship_service::get_relationship(const std::string& relationship_start_node_node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    auto results = repo_.read_latest(ctx_, relationship_start_node_node_id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void lei_relationship_service::save_relationship(const domain::lei_relationship& v) {
    if (v.relationship_start_node_node_id.empty())
        throw std::invalid_argument(
            "LEI Relationship relationship_start_node_node_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving LEI relationship. " << "relationship_start_node_node_id: "
                               << v.relationship_start_node_node_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved LEI relationship. " << "relationship_start_node_node_id: "
                              << v.relationship_start_node_node_id;
}

void lei_relationship_service::save_relationships(
    const std::vector<domain::lei_relationship>& relationships) {
    for (const auto& e : relationships) {
        if (e.relationship_start_node_node_id.empty())
            throw std::invalid_argument(
                "LEI Relationship relationship_start_node_node_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << relationships.size() << " LEI relationships";
    auto ts = relationships;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void lei_relationship_service::delete_relationship(
    const std::string& relationship_start_node_node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    repo_.remove(ctx_, relationship_start_node_node_id);
    BOOST_LOG_SEV(lg(), info) << "Removed LEI relationship. " << "relationship_start_node_node_id: "
                              << relationship_start_node_node_id;
}

void lei_relationship_service::delete_relationships(
    const std::vector<std::string>& relationship_start_node_node_ids) {
    repo_.remove(ctx_, relationship_start_node_node_ids);
}

std::vector<domain::lei_relationship> lei_relationship_service::get_relationship_history(
    const std::string& relationship_start_node_node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    return repo_.read_all(ctx_, relationship_start_node_node_id);
}

}
