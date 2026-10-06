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
#include "ores.analytics.core/service/todays_market_configuration_service.hpp"
#include "ores.analytics.api/domain/todays_market_configuration.hpp"
#include "ores.analytics.api/messaging/todays_market_configuration_protocol.hpp"
#include "ores.analytics.core/repository/todays_market_configuration_repository.hpp"
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

namespace ores::analytics::service {

using namespace ores::logging;

todays_market_configuration_service::todays_market_configuration_service(context ctx)
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
std::vector<domain::todays_market_configuration>
read_one(repository::todays_market_configuration_repository& repo,
         const ores::database::context& ctx,
         const messaging::todays_market_configuration_key& key) {
    return repo.read_latest_by_configuration_id(ctx, key.configuration_id);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::todays_market_configuration_key key_from(const domain::todays_market_configuration& v) {
    messaging::todays_market_configuration_key key;
    key.configuration_id = v.configuration_id;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::todays_market_configuration
to_domain(const messaging::todays_market_configuration_write& write) {
    domain::todays_market_configuration v;
    v.id = write.id;
    v.todays_market_config_id = write.todays_market_config_id;
    v.configuration_id = write.configuration_id;
    v.position = write.position;
    return v;
}

}

messaging::list_todays_market_configurations_response
todays_market_configuration_service::list_todays_market_configurations(
    const messaging::list_todays_market_configurations_request& request) {
    messaging::list_todays_market_configurations_response response;
    if (!request.order.field.empty() &&
        !repository::todays_market_configuration_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of today's market configuration blocks cannot be ordered by " +
            request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
        return response;
    }
    if (request.filter && request.filter->todays_market_config_id_one_of &&
        request.filter->todays_market_config_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message =
            "The filter lists more than 1000 values in todays_market_config_id_one_of.";
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
    response.configurations = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_configuration_count(ctx_, request.filter, as_of);
    return response;
}

messaging::list_by_todays_market_config_id_todays_market_configurations_response
todays_market_configuration_service::list_by_todays_market_config_id_todays_market_configurations(
    const messaging::list_by_todays_market_config_id_todays_market_configurations_request&
        request) {
    messaging::list_by_todays_market_config_id_todays_market_configurations_response response;
    if (!request.order.field.empty() &&
        !repository::todays_market_configuration_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of today's market configuration blocks cannot be ordered by " +
            request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_one_of.";
        return response;
    }
    if (request.filter && request.filter->todays_market_config_id_one_of &&
        request.filter->todays_market_config_id_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message =
            "The filter lists more than 1000 values in todays_market_config_id_one_of.";
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "scope_not_supported";
        response.result.message = "This resource reads its direct members; it has no subtree.";
        return response;
    }
    const auto relation = boost::uuids::to_string(request.todays_market_config_id);
    response.configurations = repo_.read_latest_by_todays_market_config_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_configuration_count_by_todays_market_config_id(
        ctx_, relation, request.filter);
    return response;
}

messaging::get_todays_market_configuration_response
todays_market_configuration_service::get_todays_market_configuration(
    const messaging::get_todays_market_configuration_request& request) {
    messaging::get_todays_market_configuration_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.todays_market_configuration = std::move(found.front());
    return response;
}

messaging::get_many_todays_market_configurations_response
todays_market_configuration_service::get_many_todays_market_configurations(
    const messaging::get_many_todays_market_configurations_request& request) {
    messaging::get_many_todays_market_configurations_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::todays_market_configuration_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.todays_market_configuration = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_todays_market_configuration_response
todays_market_configuration_service::put_todays_market_configuration(
    const messaging::put_todays_market_configuration_request& request) {
    messaging::put_todays_market_configuration_response response;
    domain::todays_market_configuration value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.todays_market_configuration = std::move(written.front());
    return response;
}

messaging::put_many_todays_market_configurations_response
todays_market_configuration_service::put_many_todays_market_configurations(
    const messaging::put_many_todays_market_configurations_request& request) {
    messaging::put_many_todays_market_configurations_response response;
    std::vector<domain::todays_market_configuration> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::todays_market_configuration value;
        const auto result = prepare_change(change, request.intent, value);
        if (result.outcome != ores::utility::domain::outcome::ok) {
            // Nothing has been written: the whole set is checked before any
            // of it lands, so a refused element refuses the batch.
            response.result = result;
            return response;
        }
        batch.push_back(std::move(value));
    }
    // One statement, so the set lands together. The store checks each row's
    // claim inside that statement, which is what makes the check above and the
    // write one decision rather than two.
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(request.changes.size());
    for (const auto& change : request.changes)
        claims.push_back(change.precondition);
    repo_.write(ctx_, batch, claims);
    response.configurations.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.configurations.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_todays_market_configuration_response
todays_market_configuration_service::delete_todays_market_configuration(
    const messaging::delete_todays_market_configuration_request& request) {
    messaging::delete_todays_market_configuration_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    if (request.removal.precondition.kind == precondition_kind::must_not_exist) {
        response.result.outcome = outcome::invalid;
        response.result.code = "precondition_not_supported";
        response.result.message = "A removal cannot require that a row is absent.";
        return response;
    }
    std::optional<std::uint32_t> expected;
    if (request.removal.precondition.kind == precondition_kind::must_match_version) {
        if (!request.removal.precondition.version) {
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_incomplete";
            response.result.message = "A versioned removal must state the version it expects.";
            return response;
        }
        expected = request.removal.precondition.version;
    }
    const auto named = read_one(repo_, ctx_, request.removal.key);
    if (named.empty()) {
        response.result.outcome = outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    const auto& row = named.front();
    switch (repo_.remove(ctx_, boost::uuids::to_string(row.id), expected)) {
        case repository::todays_market_configuration_repository::remove_status::removed:
            break;
        case repository::todays_market_configuration_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::todays_market_configuration_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::todays_market_configuration_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_todays_market_configurations_response
todays_market_configuration_service::delete_many_todays_market_configurations(
    const messaging::delete_many_todays_market_configurations_request& request) {
    messaging::delete_many_todays_market_configurations_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    for (const auto& removal : request.removals) {
        if (removal.precondition.kind != precondition_kind::any) {
            // The store removes a set in one statement, which carries no
            // per-row version. Refusing is the only answer that keeps the
            // batch atomic: serving it as a sequence of single removals would
            // leave a partial batch behind as soon as one row had moved on.
            response.result.outcome = outcome::invalid;
            response.result.code = "batch_removal_is_unconditional";
            response.result.message =
                "A batch removal is unconditional; remove the rows one at a time "
                "to state a version.";
            return response;
        }
    }
    if (request.removals.empty())
        return response;
    // A removal names its row by the key a caller holds, and the repository
    // takes the storage key, so the two are joined once here rather than at
    // each column's conversion. A name that matches no row is skipped: the
    // batch reports what it removed, and a row that is already gone is not a
    // failure.
    std::vector<domain::todays_market_configuration> resolved;
    resolved.reserve(request.removals.size());
    for (const auto& removal : request.removals) {
        auto named = read_one(repo_, ctx_, removal.key);
        if (!named.empty())
            resolved.push_back(std::move(named.front()));
    }
    std::vector<std::string> id_keys;
    id_keys.reserve(resolved.size());
    for (const auto& row : resolved)
        id_keys.push_back(boost::uuids::to_string(row.id));
    repo_.remove(ctx_, id_keys);
    return response;
}

messaging::list_todays_market_configuration_versions_response
todays_market_configuration_service::list_todays_market_configuration_versions(
    const messaging::list_todays_market_configuration_versions_request& request) {
    messaging::list_todays_market_configuration_versions_response response;
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

messaging::get_todays_market_configuration_version_response
todays_market_configuration_service::get_todays_market_configuration_version(
    const messaging::get_todays_market_configuration_version_request& request) {
    messaging::get_todays_market_configuration_version_response response;
    // The version key nests the entity's own key, which is the declared one.
    // The repository reads by the storage key, so it is resolved once here.
    const auto named = read_one(repo_, ctx_, request.key.todays_market_configuration);
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

ores::utility::domain::result todays_market_configuration_service::prepare_change(
    const messaging::todays_market_configuration_change& change,
    const ores::utility::domain::change_intent& intent,
    domain::todays_market_configuration& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, ctx_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty()) {
                result.outcome = outcome::conflict;
                result.code = "already_exists";
                return result;
            }
            break;
        case precondition_kind::must_match_version:
            if (current.empty()) {
                result.outcome = outcome::missing;
                result.code = "not_found";
                return result;
            }
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                result.outcome = outcome::conflict;
                result.code = "version_conflict";
                return result;
            }
            break;
        case precondition_kind::any:
            break;
    }
    // The version is the repository's to state, from the claim: it is the one
    // thing the store's arbiter reads, and stating it in two places is how the
    // two come to disagree.
    stamp(out,
          ctx_,
          intent.reason_code.empty() ?
              std::string(ores::service::messaging::change_reasons::new_record) :
              intent.reason_code);
    out.change_commentary = intent.commentary;
    return result;
}


std::vector<domain::todays_market_configuration>
todays_market_configuration_service::list_configurations(std::uint32_t offset,
                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all today's market configuration blocks";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t todays_market_configuration_service::count_configurations() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total today's market configuration blocks count";
    return repo_.get_total_configuration_count(ctx_);
}


std::vector<domain::todays_market_configuration>
todays_market_configuration_service::list_configurations_by_todays_market_config_id(
    const std::string& todays_market_config_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug)
        << "Listing today's market configuration blocks by todays_market_config_id: "
        << todays_market_config_id;
    return repo_.read_latest_by_todays_market_config_id(
        ctx_, todays_market_config_id, offset, limit);
}

std::uint32_t todays_market_configuration_service::count_configurations_by_todays_market_config_id(
    const std::string& todays_market_config_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting total today's market configuration blocks count by todays_market_config_id: "
        << todays_market_config_id;
    return repo_.get_total_configuration_count_by_todays_market_config_id(ctx_,
                                                                          todays_market_config_id);
}


std::optional<domain::todays_market_configuration>
todays_market_configuration_service::get_configuration_at_version(const boost::uuids::uuid& id,
                                                                  std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting today's market configuration block at version. "
                               << "id: " << id << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::todays_market_configuration>
todays_market_configuration_service::get_configuration(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting today's market configuration block. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::optional<domain::todays_market_configuration>
todays_market_configuration_service::get_configuration_by_configuration_id(
    const std::string& configuration_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting today's market configuration block by configuration_id: "
                               << configuration_id;
    messaging::todays_market_configuration_key k;
    k.configuration_id = configuration_id;
    auto found = read_one(repo_, ctx_, k);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

std::optional<domain::todays_market_configuration>
todays_market_configuration_service::find_configuration(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding today's market configuration block. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::todays_market_configuration>
todays_market_configuration_service::get_configurations(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void todays_market_configuration_service::save_configuration(
    const domain::todays_market_configuration& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Todays Market Configuration id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving today's market configuration block. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved today's market configuration block. " << "id: " << v.id;
}

void todays_market_configuration_service::save_configurations(
    const std::vector<domain::todays_market_configuration>& configurations) {
    for (const auto& e : configurations) {
        if (e.id.is_nil())
            throw std::invalid_argument("Todays Market Configuration id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << configurations.size()
                               << " today's market configuration blocks";
    auto ts = configurations;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void todays_market_configuration_service::delete_configuration(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing today's market configuration block. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed today's market configuration block. " << "id: " << id;
}

void todays_market_configuration_service::remove_configuration(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing today's market configuration block. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed today's market configuration block. " << "id: " << id;
}

void todays_market_configuration_service::delete_configurations(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::todays_market_configuration>
todays_market_configuration_service::get_configuration_history(const std::string& key) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for today's market configuration block. key: "
                               << key;
    // The caller holds the key the model declares and this reads by the
    // storage key, so the two are joined here exactly as they are for any
    // other read. Without this step a provider looks the versions up under a
    // value the storage key never holds, and reports an entity that has a
    // history as having none.
    messaging::todays_market_configuration_key k;
    k.configuration_id = key;
    // A delete here closes the transaction-time window and leaves every version
    // in place, so resolving through a latest read would lose the history at
    // exactly the moment it is wanted. This takes the newest row carrying the
    // declared key whether or not it is still current, which for a record that
    // still exists is the same row the latest read would have returned.
    const auto found = repo_.read_any_by_configuration_id(ctx_, k.configuration_id);
    if (found.empty())
        return {};
    const auto& row = found.front();
    return repo_.read_all(ctx_, boost::uuids::to_string(row.id));
}

std::vector<domain::todays_market_configuration>
todays_market_configuration_service::get_configuration_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for today's market configuration block. "
                               << "id: " << id;
    return repo_.read_all(ctx_, boost::uuids::to_string(id));
}

}
