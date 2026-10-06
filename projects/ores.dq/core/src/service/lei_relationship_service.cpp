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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.dq.core/service/lei_relationship_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/lei_relationship.hpp"
#include "ores.dq.api/messaging/lei_relationship_protocol.hpp"
#include "ores.dq.core/repository/lei_relationship_repository.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

lei_relationship_service::lei_relationship_service(context ctx)
    : ctx_(std::move(ctx)) {}
namespace {

/**
 * @brief The current row a key names, or an empty vector when there is none.
 *
 * A key record carries each column with the column's own type, and the
 * repository takes the text form every one of its key parameters shares, so
 * the conversion lives here rather than at every call site.
 *
 * The key record carries the key the model declares, which is the one a caller
 * holds. When that is not the storage key the row is found by it and the
 * repository's storage-key read is not used at all.
 */
std::vector<domain::lei_relationship> read_one(repository::lei_relationship_repository& repo,
                                               const ores::database::context& ctx,
                                               const messaging::lei_relationship_key& key) {
    return repo.read_latest(ctx, key.relationship_start_node_node_id);
}

}

messaging::list_lei_relationships_response lei_relationship_service::list_lei_relationships(
    const messaging::list_lei_relationships_request& request) {
    messaging::list_lei_relationships_response response;
    if (!request.order.field.empty() &&
        !repository::lei_relationship_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of LEI relationships cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->relationship_start_node_node_id_one_of &&
        request.filter->relationship_start_node_node_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message =
            "The filter lists more than 1000 values in relationship_start_node_node_id_one_of.";
        return response;
    }
    response.relationships =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_relationship_count(ctx_, request.filter);
    return response;
}

messaging::get_lei_relationship_response lei_relationship_service::get_lei_relationship(
    const messaging::get_lei_relationship_request& request) {
    messaging::get_lei_relationship_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.lei_relationship = std::move(found.front());
    return response;
}

messaging::get_many_lei_relationships_response lei_relationship_service::get_many_lei_relationships(
    const messaging::get_many_lei_relationships_request& request) {
    messaging::get_many_lei_relationships_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::lei_relationship_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.lei_relationship = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::lei_relationship>
lei_relationship_service::list_relationships(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all LEI relationships";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t lei_relationship_service::count_relationships() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total LEI relationships count";
    return repo_.get_total_relationship_count(ctx_);
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

std::vector<domain::lei_relationship> lei_relationship_service::get_relationships(
    const std::vector<std::string>& relationship_start_node_node_ids) {
    return repo_.read_latest(ctx_, relationship_start_node_node_ids);
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
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
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


}
