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
#include "ores.refdata.core/service/currency_currency_group_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::refdata::service {

using namespace ores::logging;
using ores::service::messaging::stamp;


currency_currency_group_service::currency_currency_group_service(context ctx)
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
std::vector<domain::currency_currency_group>
read_one(repository::currency_currency_group_repository& repo,
         const messaging::currency_currency_group_key& key) {
    return repo.read_latest(key.currency_iso_code, key.currency_group_code);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::currency_currency_group_key key_from(const domain::currency_currency_group& v) {
    messaging::currency_currency_group_key key;
    key.currency_iso_code = v.currency_iso_code;
    key.currency_group_code = v.currency_group_code;
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
domain::currency_currency_group to_domain(const messaging::currency_currency_group_write& write) {
    domain::currency_currency_group v;
    v.currency_iso_code = write.currency_iso_code;
    v.currency_group_code = write.currency_group_code;
    return v;
}

} // namespace

messaging::list_currency_currency_groups_response
currency_currency_group_service::list_currency_currency_groups(
    const messaging::list_currency_currency_groups_request& request) {
    messaging::list_currency_currency_groups_response response;
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
    response.currency_groups = repo_.read_latest(request.offset, request.limit);
    response.total = repo_.get_total_currency_group_count();
    return response;
}

messaging::list_by_currency_iso_code_currency_currency_groups_response
currency_currency_group_service::list_by_currency_iso_code_currency_currency_groups(
    const messaging::list_by_currency_iso_code_currency_currency_groups_request& request) {
    messaging::list_by_currency_iso_code_currency_currency_groups_response response;
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
    response.currency_groups =
        repo_.read_latest_by_currency(request.currency_iso_code, request.offset, request.limit);
    response.total = repo_.get_total_currency_group_count_by_currency(request.currency_iso_code);
    return response;
}

messaging::get_currency_currency_group_response
currency_currency_group_service::get_currency_currency_group(
    const messaging::get_currency_currency_group_request& request) {
    messaging::get_currency_currency_group_response response;
    auto found = read_one(repo_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.currency_currency_group = std::move(found.front());
    return response;
}

messaging::get_many_currency_currency_groups_response
currency_currency_group_service::get_many_currency_currency_groups(
    const messaging::get_many_currency_currency_groups_request& request) {
    messaging::get_many_currency_currency_groups_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::currency_currency_group_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, k);
        if (!found.empty())
            entry.currency_currency_group = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_currency_currency_group_response
currency_currency_group_service::put_currency_currency_group(
    const messaging::put_currency_currency_group_request& request) {
    messaging::put_currency_currency_group_response response;
    domain::currency_currency_group value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(value, request.change.precondition);
    auto written = read_one(repo_, key_from(value));
    if (!written.empty())
        response.currency_currency_group = std::move(written.front());
    return response;
}

messaging::put_many_currency_currency_groups_response
currency_currency_group_service::put_many_currency_currency_groups(
    const messaging::put_many_currency_currency_groups_request& request) {
    messaging::put_many_currency_currency_groups_response response;
    std::vector<domain::currency_currency_group> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::currency_currency_group value;
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
    response.currency_groups.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, key_from(value));
        response.currency_groups.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_currency_currency_group_response
currency_currency_group_service::delete_currency_currency_group(
    const messaging::delete_currency_currency_group_request& request) {
    messaging::delete_currency_currency_group_response response;
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
    switch (repo_.remove(
        request.removal.key.currency_iso_code, request.removal.key.currency_group_code, expected)) {
        case repository::currency_currency_group_repository::remove_status::removed:
            break;
        case repository::currency_currency_group_repository::remove_status::missing:
            response.result.outcome = outcome::missing;
            response.result.code = "not_found";
            break;
        case repository::currency_currency_group_repository::remove_status::conflicting:
            response.result.outcome = outcome::conflict;
            response.result.code = "version_conflict";
            break;
        case repository::currency_currency_group_repository::remove_status::unsupported:
            response.result.outcome = outcome::invalid;
            response.result.code = "precondition_not_supported";
            response.result.message = "This resource keeps no version to match.";
            break;
    }
    return response;
}

messaging::delete_many_currency_currency_groups_response
currency_currency_group_service::delete_many_currency_currency_groups(
    const messaging::delete_many_currency_currency_groups_request& request) {
    messaging::delete_many_currency_currency_groups_response response;
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
    std::vector<std::string> currency_iso_code_keys;
    currency_iso_code_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        currency_iso_code_keys.push_back(removal.key.currency_iso_code);
    std::vector<std::string> currency_group_code_keys;
    currency_group_code_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        currency_group_code_keys.push_back(removal.key.currency_group_code);
    repo_.remove(currency_iso_code_keys, currency_group_code_keys);
    return response;
}

ores::utility::domain::result currency_currency_group_service::prepare_change(
    const messaging::currency_currency_group_change& change,
    const ores::utility::domain::change_intent& intent,
    domain::currency_currency_group& out) {
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
