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
#include "ores.iam.core/service/role_grant_request_role_service.hpp"
#include "ores.iam.api/domain/role_grant_request_role.hpp"
#include "ores.iam.api/messaging/role_grant_request_role_protocol.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <vector>
// Log lines stream uuids with uuid_io's operator<<, which the include check
// does not count as a use.
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

namespace ores::iam::service {

using namespace ores::logging;
using ores::service::messaging::stamp;


role_grant_request_role_service::role_grant_request_role_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_(ctx_) {}

namespace {

/**
 * @brief The current row a key names, or an empty vector when there is none.
 *
 * A junction key is the pair of columns that names one link, and the
 * repository takes each in its own column's type, so the pair passes
 * straight through.
 */
std::vector<domain::role_grant_request_role>
read_one(repository::role_grant_request_role_repository& repo,
         const messaging::role_grant_request_role_key& key) {
    return repo.read_latest(key.request_id, key.role_id);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::role_grant_request_role_key key_from(const domain::role_grant_request_role& v) {
    messaging::role_grant_request_role_key key;
    key.request_id = v.request_id;
    key.role_id = v.role_id;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the two halves of the key and the junction's own
 * columns and nothing else: tenancy, provenance, the version and the
 * validity window are the service's and the database's to state, and are
 * set after this conversion.
 */
domain::role_grant_request_role to_domain(const messaging::role_grant_request_role_write& write) {
    domain::role_grant_request_role v;
    v.request_id = write.request_id;
    v.role_id = write.role_id;
    v.applied_at = write.applied_at;
    return v;
}

}

messaging::list_role_grant_request_roles_response
role_grant_request_role_service::list_role_grant_request_roles(
    const messaging::list_role_grant_request_roles_request& request) {
    messaging::list_role_grant_request_roles_response response;
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
    response.role_grant_request_roles = repo_.read_latest(request.offset, request.limit);
    response.total = repo_.get_total_role_grant_request_role_count();
    return response;
}

messaging::list_by_request_id_role_grant_request_roles_response
role_grant_request_role_service::list_by_request_id_role_grant_request_roles(
    const messaging::list_by_request_id_role_grant_request_roles_request& request) {
    messaging::list_by_request_id_role_grant_request_roles_response response;
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
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "scope_not_supported";
        response.result.message = "This resource reads its direct members; it has no subtree.";
        return response;
    }
    response.role_grant_request_roles =
        repo_.read_latest_by_request(request.request_id, request.offset, request.limit);
    response.total = repo_.get_total_role_grant_request_role_count_by_request(request.request_id);
    return response;
}

messaging::get_role_grant_request_role_response
role_grant_request_role_service::get_role_grant_request_role(
    const messaging::get_role_grant_request_role_request& request) {
    messaging::get_role_grant_request_role_response response;
    auto found = read_one(repo_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.role_grant_request_role = std::move(found.front());
    return response;
}

messaging::get_many_role_grant_request_roles_response
role_grant_request_role_service::get_many_role_grant_request_roles(
    const messaging::get_many_role_grant_request_roles_request& request) {
    messaging::get_many_role_grant_request_roles_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::role_grant_request_role_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, k);
        if (!found.empty())
            entry.role_grant_request_role = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_role_grant_request_role_response
role_grant_request_role_service::put_role_grant_request_role(
    const messaging::put_role_grant_request_role_request& request) {
    messaging::put_role_grant_request_role_response response;
    domain::role_grant_request_role value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(value, request.change.precondition);
    auto written = read_one(repo_, key_from(value));
    if (!written.empty())
        response.role_grant_request_role = std::move(written.front());
    return response;
}

messaging::put_many_role_grant_request_roles_response
role_grant_request_role_service::put_many_role_grant_request_roles(
    const messaging::put_many_role_grant_request_roles_request& request) {
    messaging::put_many_role_grant_request_roles_response response;
    std::vector<domain::role_grant_request_role> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::role_grant_request_role value;
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
    repo_.write(batch, claims);
    response.role_grant_request_roles.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, key_from(value));
        response.role_grant_request_roles.push_back(written.empty() ? value :
                                                                      std::move(written.front()));
    }
    return response;
}

messaging::delete_role_grant_request_role_response
role_grant_request_role_service::delete_role_grant_request_role(
    const messaging::delete_role_grant_request_role_request& request) {
    messaging::delete_role_grant_request_role_response response;
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
    switch (repo_.remove(request.removal.key.request_id, request.removal.key.role_id, expected)) {
        case repository::role_grant_request_role_repository::remove_status::removed:
            break;
        case repository::role_grant_request_role_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::role_grant_request_role_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::role_grant_request_role_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_role_grant_request_roles_response
role_grant_request_role_service::delete_many_role_grant_request_roles(
    const messaging::delete_many_role_grant_request_roles_request& request) {
    messaging::delete_many_role_grant_request_roles_response response;
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
    std::vector<boost::uuids::uuid> request_id_keys;
    request_id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        request_id_keys.push_back(removal.key.request_id);
    std::vector<boost::uuids::uuid> role_id_keys;
    role_id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        role_id_keys.push_back(removal.key.role_id);
    repo_.remove(request_id_keys, role_id_keys);
    return response;
}

ores::utility::domain::result role_grant_request_role_service::prepare_change(
    const messaging::role_grant_request_role_change& change,
    const ores::utility::domain::change_intent& intent,
    domain::role_grant_request_role& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, key_from(out));
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
    return result;
}

}
