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
#include "ores.iam.core/service/permission_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <algorithm>
#include <cstdint>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using ores::service::messaging::stamp;

namespace ores::iam::service {

using namespace ores::logging;

permission_service::permission_service(context ctx)
    : ctx_(std::move(ctx)) {}
namespace {

/**
 * @brief The current row a key names, or an empty vector when there is none.
 *
 * A key record carries each column with the column's own type, and the
 * repository takes the text form every one of its key parameters shares, so
 * the conversion lives here rather than at every call site.
 */
std::vector<domain::permission> read_one(repository::permission_repository& repo,
                                         const ores::database::context& ctx,
                                         const messaging::permission_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::permission_key key_from(const domain::permission& v) {
    messaging::permission_key key;
    key.id = v.id;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::permission to_domain(const messaging::permission_write& write) {
    domain::permission v;
    v.id = write.id;
    v.code = write.code;
    v.description = write.description;
    return v;
}

} // namespace

messaging::list_permissions_response
permission_service::list_permissions(const messaging::list_permissions_request& request) {
    messaging::list_permissions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    response.permissions = repo_.read_latest(ctx_, request.offset, request.limit);
    response.total = repo_.get_total_permission_count(ctx_);
    return response;
}

messaging::get_permission_response
permission_service::get_permission(const messaging::get_permission_request& request) {
    messaging::get_permission_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.permission = std::move(found.front());
    return response;
}

messaging::get_many_permissions_response
permission_service::get_many_permissions(const messaging::get_many_permissions_request& request) {
    messaging::get_many_permissions_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::permission_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.permission = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_permission_response
permission_service::put_permission(const messaging::put_permission_request& request) {
    messaging::put_permission_response response;
    domain::permission value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.permission = std::move(written.front());
    return response;
}

messaging::put_many_permissions_response
permission_service::put_many_permissions(const messaging::put_many_permissions_request& request) {
    messaging::put_many_permissions_response response;
    std::vector<domain::permission> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::permission value;
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
    response.permissions.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.permissions.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_permission_response
permission_service::delete_permission(const messaging::delete_permission_request& request) {
    messaging::delete_permission_response response;
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
    switch (repo_.remove(ctx_, boost::uuids::to_string(request.removal.key.id), expected)) {
        case repository::permission_repository::remove_status::removed:
            break;
        case repository::permission_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::permission_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::permission_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_permissions_response permission_service::delete_many_permissions(
    const messaging::delete_many_permissions_request& request) {
    messaging::delete_many_permissions_response response;
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
    std::vector<std::string> id_keys;
    id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        id_keys.push_back(boost::uuids::to_string(removal.key.id));
    repo_.remove(ctx_, id_keys);
    return response;
}

ores::utility::domain::result
permission_service::prepare_change(const messaging::permission_change& change,
                                   const ores::utility::domain::change_intent& intent,
                                   domain::permission& out) {
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
            result.outcome = outcome::invalid;
            result.code = "precondition_not_supported";
            result.message = "This resource keeps no version to match.";
            return result;
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
    return result;
}


std::vector<domain::permission> permission_service::list_permissions(std::uint32_t offset,
                                                                     std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all permissions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t permission_service::count_permissions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total permissions count";
    return repo_.get_total_permission_count(ctx_);
}


std::optional<domain::permission> permission_service::get_permission(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting permission. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::optional<domain::permission>
permission_service::find_permission_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding permission by code: " << code;
    auto results = repo_.read_latest_by_code(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::permission>
permission_service::get_permissions(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void permission_service::save_permission(const domain::permission& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Permission id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving permission. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved permission. " << "id: " << v.id;
}

void permission_service::save_permissions(const std::vector<domain::permission>& permissions) {
    for (const auto& e : permissions) {
        if (e.id.is_nil())
            throw std::invalid_argument("Permission id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << permissions.size() << " permissions";
    auto ts = permissions;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void permission_service::delete_permission(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing permission. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed permission. " << "id: " << id;
}

void permission_service::delete_permissions(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::permission> permission_service::get_permission_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for permission. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
