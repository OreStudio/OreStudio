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
#include "ores.dq.core/service/dataset_bundle_member_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::dq::service {

using namespace ores::logging;
using ores::service::messaging::stamp;


dataset_bundle_member_service::dataset_bundle_member_service(context ctx)
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
std::vector<domain::dataset_bundle_member>
read_one(repository::dataset_bundle_member_repository& repo,
         const messaging::dataset_bundle_member_key& key) {
    return repo.read_latest(key.bundle_code, key.dataset_code);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::dataset_bundle_member_key key_from(const domain::dataset_bundle_member& v) {
    messaging::dataset_bundle_member_key key;
    key.bundle_code = v.bundle_code;
    key.dataset_code = v.dataset_code;
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
domain::dataset_bundle_member to_domain(const messaging::dataset_bundle_member_write& write) {
    domain::dataset_bundle_member v;
    v.bundle_code = write.bundle_code;
    v.dataset_code = write.dataset_code;
    v.display_order = write.display_order;
    v.optional = write.optional;
    return v;
}

} // namespace

messaging::list_dataset_bundle_members_response
dataset_bundle_member_service::list_dataset_bundle_members(
    const messaging::list_dataset_bundle_members_request& request) {
    messaging::list_dataset_bundle_members_response response;
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
    response.dataset_bundle_members = repo_.read_latest(request.offset, request.limit);
    response.total = repo_.get_total_member_count();
    return response;
}

messaging::list_by_bundle_code_dataset_bundle_members_response
dataset_bundle_member_service::list_by_bundle_code_dataset_bundle_members(
    const messaging::list_by_bundle_code_dataset_bundle_members_request& request) {
    messaging::list_by_bundle_code_dataset_bundle_members_response response;
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
    response.dataset_bundle_members =
        repo_.read_latest_by_bundle(request.bundle_code, request.offset, request.limit);
    response.total = repo_.get_total_member_count_by_bundle(request.bundle_code);
    return response;
}

messaging::get_dataset_bundle_member_response
dataset_bundle_member_service::get_dataset_bundle_member(
    const messaging::get_dataset_bundle_member_request& request) {
    messaging::get_dataset_bundle_member_response response;
    auto found = read_one(repo_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.dataset_bundle_member = std::move(found.front());
    return response;
}

messaging::get_many_dataset_bundle_members_response
dataset_bundle_member_service::get_many_dataset_bundle_members(
    const messaging::get_many_dataset_bundle_members_request& request) {
    messaging::get_many_dataset_bundle_members_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::dataset_bundle_member_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, k);
        if (!found.empty())
            entry.dataset_bundle_member = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_dataset_bundle_member_response
dataset_bundle_member_service::put_dataset_bundle_member(
    const messaging::put_dataset_bundle_member_request& request) {
    messaging::put_dataset_bundle_member_response response;
    domain::dataset_bundle_member value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(value, request.change.precondition);
    auto written = read_one(repo_, key_from(value));
    if (!written.empty())
        response.dataset_bundle_member = std::move(written.front());
    return response;
}

messaging::put_many_dataset_bundle_members_response
dataset_bundle_member_service::put_many_dataset_bundle_members(
    const messaging::put_many_dataset_bundle_members_request& request) {
    messaging::put_many_dataset_bundle_members_response response;
    std::vector<domain::dataset_bundle_member> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::dataset_bundle_member value;
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
    response.dataset_bundle_members.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, key_from(value));
        response.dataset_bundle_members.push_back(written.empty() ? value :
                                                                    std::move(written.front()));
    }
    return response;
}

messaging::delete_dataset_bundle_member_response
dataset_bundle_member_service::delete_dataset_bundle_member(
    const messaging::delete_dataset_bundle_member_request& request) {
    messaging::delete_dataset_bundle_member_response response;
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
    switch (
        repo_.remove(request.removal.key.bundle_code, request.removal.key.dataset_code, expected)) {
        case repository::dataset_bundle_member_repository::remove_status::removed:
            break;
        case repository::dataset_bundle_member_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::dataset_bundle_member_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::dataset_bundle_member_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_dataset_bundle_members_response
dataset_bundle_member_service::delete_many_dataset_bundle_members(
    const messaging::delete_many_dataset_bundle_members_request& request) {
    messaging::delete_many_dataset_bundle_members_response response;
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
    std::vector<std::string> bundle_code_keys;
    bundle_code_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        bundle_code_keys.push_back(removal.key.bundle_code);
    std::vector<std::string> dataset_code_keys;
    dataset_code_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        dataset_code_keys.push_back(removal.key.dataset_code);
    repo_.remove(bundle_code_keys, dataset_code_keys);
    return response;
}

ores::utility::domain::result
dataset_bundle_member_service::prepare_change(const messaging::dataset_bundle_member_change& change,
                                              const ores::utility::domain::change_intent& intent,
                                              domain::dataset_bundle_member& out) {
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
