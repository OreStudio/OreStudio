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
#include "ores.iam.core/service/run_grant_service.hpp"
#include "ores.iam.api/domain/run_grant.hpp"
#include "ores.iam.api/messaging/run_grant_protocol.hpp"
#include "ores.iam.core/repository/run_grant_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>
// Log lines stream uuids with uuid_io's operator<<, which the include check
// does not count as a use.
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

using ores::service::messaging::stamp;

namespace ores::iam::service {

using namespace ores::logging;

run_grant_service::run_grant_service(context ctx)
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
std::vector<domain::run_grant> read_one(repository::run_grant_repository& repo,
                                        const ores::database::context& ctx,
                                        const messaging::run_grant_key& key) {
    return repo.read_latest_by_resource(ctx, key.resource);
}

}

messaging::list_run_grants_response
run_grant_service::list_run_grants(const messaging::list_run_grants_request& request) {
    messaging::list_run_grants_response response;
    if (!request.order.field.empty() &&
        !repository::run_grant_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of run grants cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
        return response;
    }
    if (request.filter && request.filter->grantor_account_id_one_of &&
        request.filter->grantor_account_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message =
            "The filter lists more than 1000 values in grantor_account_id_one_of.";
        return response;
    }
    // A stated instant is checked here, so a malformed one is the caller's
    // mistake rather than a database error. The caller's text is what the
    // store reads, so a fraction of a second is kept.
    std::optional<std::string> as_of;
    if (request.as_of) {
        as_of = ores::database::repository::parse_as_of(*request.as_of);
        if (!as_of) {
            response.result.outcome = ores::utility::domain::outcome::invalid;
            response.result.code = "as_of_invalid";
            response.result.message = "as_of is not a UTC timestamp: " + *request.as_of;
            return response;
        }
    }
    response.grants = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_grant_count(ctx_, request.filter, as_of);
    return response;
}

messaging::list_by_grantor_account_id_run_grants_response
run_grant_service::list_by_grantor_account_id_run_grants(
    const messaging::list_by_grantor_account_id_run_grants_request& request) {
    messaging::list_by_grantor_account_id_run_grants_response response;
    if (!request.order.field.empty() &&
        !repository::run_grant_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of run grants cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
        return response;
    }
    if (request.filter && request.filter->grantor_account_id_one_of &&
        request.filter->grantor_account_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message =
            "The filter lists more than 1000 values in grantor_account_id_one_of.";
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "scope_not_supported";
        response.result.message = "This resource reads its direct members; it has no subtree.";
        return response;
    }
    const auto relation = boost::uuids::to_string(request.grantor_account_id);
    response.grants = repo_.read_latest_by_grantor_account_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total =
        repo_.get_total_grant_count_by_grantor_account_id(ctx_, relation, request.filter);
    return response;
}

messaging::get_run_grant_response
run_grant_service::get_run_grant(const messaging::get_run_grant_request& request) {
    messaging::get_run_grant_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.run_grant = std::move(found.front());
    return response;
}

messaging::get_many_run_grants_response
run_grant_service::get_many_run_grants(const messaging::get_many_run_grants_request& request) {
    messaging::get_many_run_grants_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::run_grant_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.run_grant = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::list_run_grant_versions_response run_grant_service::list_run_grant_versions(
    const messaging::list_run_grant_versions_request& request) {
    messaging::list_run_grant_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    if (request.filter) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_not_supported";
        response.result.message = "Filtering is not served for this resource yet.";
        return response;
    }
    // The versions of the row the caller's key names. The repository reads by
    // the storage key, so the declared key is resolved once here.
    const auto named = read_one(repo_, ctx_, request.key);
    if (named.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    const auto& row = named.front();
    auto all = repo_.read_all(ctx_, boost::uuids::to_string(row.id));
    // The store reads versions newest first, and the order a caller gets when
    // it states none is key order, which for a version key is oldest first.
    std::reverse(all.begin(), all.end());
    response.total = all.size();
    const auto begin = std::min<std::size_t>(request.offset, all.size());
    const auto end = std::min<std::size_t>(begin + request.limit, all.size());
    response.versions.assign(std::make_move_iterator(all.begin() + begin),
                             std::make_move_iterator(all.begin() + end));
    return response;
}

messaging::get_run_grant_version_response
run_grant_service::get_run_grant_version(const messaging::get_run_grant_version_request& request) {
    messaging::get_run_grant_version_response response;
    // The version key nests the entity's own key, which is the declared one.
    // The repository reads by the storage key, so it is resolved once here.
    const auto named = read_one(repo_, ctx_, request.key.run_grant);
    if (named.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    const auto& row = named.front();
    auto found = repo_.read_at_version(ctx_, boost::uuids::to_string(row.id), request.key.version);
    if (!found) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.version = std::move(*found);
    return response;
}


std::vector<domain::run_grant> run_grant_service::list_grants(std::uint32_t offset,
                                                              std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all run grants";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t run_grant_service::count_grants() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total run grants count";
    return repo_.get_total_grant_count(ctx_);
}


std::vector<domain::run_grant> run_grant_service::list_grants_by_grantor_account_id(
    const std::string& grantor_account_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing run grants by grantor_account_id: "
                               << grantor_account_id;
    return repo_.read_latest_by_grantor_account_id(ctx_, grantor_account_id, offset, limit);
}

std::uint32_t
run_grant_service::count_grants_by_grantor_account_id(const std::string& grantor_account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total run grants count by grantor_account_id: "
                               << grantor_account_id;
    return repo_.get_total_grant_count_by_grantor_account_id(ctx_, grantor_account_id);
}


std::optional<domain::run_grant>
run_grant_service::get_grant_at_version(const boost::uuids::uuid& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting run grant at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::run_grant> run_grant_service::get_grant(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting run grant. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::optional<domain::run_grant>
run_grant_service::get_grant_by_resource(const std::string& resource) {
    BOOST_LOG_SEV(lg(), debug) << "Getting run grant by resource: " << resource;
    messaging::run_grant_key k;
    k.resource = resource;
    auto found = read_one(repo_, ctx_, k);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

std::optional<domain::run_grant> run_grant_service::find_grant(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding run grant. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::run_grant> run_grant_service::get_grants(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void run_grant_service::save_grant(const domain::run_grant& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Run Grant id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving run grant. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved run grant. " << "id: " << v.id;
}

void run_grant_service::save_grants(const std::vector<domain::run_grant>& grants) {
    for (const auto& e : grants) {
        if (e.id.is_nil())
            throw std::invalid_argument("Run Grant id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << grants.size() << " run grants";
    auto ts = grants;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void run_grant_service::delete_grant(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing run grant. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed run grant. " << "id: " << id;
}

void run_grant_service::remove_grant(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing run grant. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed run grant. " << "id: " << id;
}

void run_grant_service::delete_grants(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::run_grant> run_grant_service::get_grant_history(const std::string& key) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for run grant. key: " << key;
    // The caller holds the key the model declares and this reads by the
    // storage key, so the two are joined here exactly as they are for any
    // other read. Without this step a provider looks the versions up under a
    // value the storage key never holds, and reports an entity that has a
    // history as having none.
    messaging::run_grant_key k;
    k.resource = key;
    // A delete here closes the transaction-time window and leaves every version
    // in place, so resolving through a latest read would lose the history at
    // exactly the moment it is wanted. This takes the newest row carrying the
    // declared key whether or not it is still current, which for a record that
    // still exists is the same row the latest read would have returned.
    const auto found = repo_.read_any_by_resource(ctx_, k.resource);
    if (found.empty())
        return {};
    const auto& row = found.front();
    return repo_.read_all(ctx_, boost::uuids::to_string(row.id));
}

std::vector<domain::run_grant> run_grant_service::get_grant_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for run grant. " << "id: " << id;
    return repo_.read_all(ctx_, boost::uuids::to_string(id));
}

}
