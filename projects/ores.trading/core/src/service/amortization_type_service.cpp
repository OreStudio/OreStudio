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
#include "ores.trading.core/service/amortization_type_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.trading.api/domain/amortization_type.hpp"
#include "ores.trading.api/messaging/amortization_type_protocol.hpp"
#include "ores.trading.core/repository/amortization_type_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

namespace ores::trading::service {

using namespace ores::logging;

amortization_type_service::amortization_type_service(context ctx)
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
std::vector<domain::amortization_type> read_one(repository::amortization_type_repository& repo,
                                                const ores::database::context& ctx,
                                                const messaging::amortization_type_key& key) {
    return repo.read_latest(ctx, key.code);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::amortization_type_key key_from(const domain::amortization_type& v) {
    messaging::amortization_type_key key;
    key.code = v.code;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::amortization_type to_domain(const messaging::amortization_type_write& write) {
    domain::amortization_type v;
    v.code = write.code;
    v.description = write.description;
    return v;
}

}

messaging::list_amortization_types_response amortization_type_service::list_amortization_types(
    const messaging::list_amortization_types_request& request) {
    messaging::list_amortization_types_response response;
    if (!request.order.field.empty() &&
        !repository::amortization_type_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "amortization types", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->code_one_of &&
        request.filter->code_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "code_one_of", .limit = "1000"});
        return response;
    }
    // A stated instant is checked here, so a malformed one is the caller's
    // mistake rather than a database error. The caller's text is what the
    // store reads, so a fraction of a second is kept.
    std::optional<std::string> as_of;
    if (request.as_of) {
        as_of = ores::database::repository::parse_as_of(*request.as_of);
        if (!as_of) {
            response.result = refuse(outcome_code::as_of_invalid, {.value = *request.as_of});
            return response;
        }
    }
    response.amortization_types = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_amortization_type_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_amortization_type_response amortization_type_service::get_amortization_type(
    const messaging::get_amortization_type_request& request) {
    messaging::get_amortization_type_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "amortization_type"});
        return response;
    }
    response.amortization_type = std::move(found.front());
    return response;
}

messaging::get_many_amortization_types_response
amortization_type_service::get_many_amortization_types(
    const messaging::get_many_amortization_types_request& request) {
    messaging::get_many_amortization_types_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::amortization_type_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.amortization_type = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_amortization_type_response amortization_type_service::put_amortization_type(
    const messaging::put_amortization_type_request& request) {
    messaging::put_amortization_type_response response;
    domain::amortization_type value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.amortization_type = std::move(written.front());
    return response;
}

messaging::put_many_amortization_types_response
amortization_type_service::put_many_amortization_types(
    const messaging::put_many_amortization_types_request& request) {
    messaging::put_many_amortization_types_response response;
    std::vector<domain::amortization_type> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::amortization_type value;
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
    response.amortization_types.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.amortization_types.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_amortization_type_response amortization_type_service::delete_amortization_type(
    const messaging::delete_amortization_type_request& request) {
    messaging::delete_amortization_type_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    if (request.removal.precondition.kind == precondition_kind::must_not_exist) {
        response.result = refuse(outcome_code::precondition_not_supported);
        return response;
    }
    std::optional<std::uint32_t> expected;
    if (request.removal.precondition.kind == precondition_kind::must_match_version) {
        if (!request.removal.precondition.version) {
            response.result = refuse(outcome_code::precondition_incomplete);
            return response;
        }
        expected = request.removal.precondition.version;
    }
    switch (repo_.remove(ctx_, request.removal.key.code, expected)) {
        case repository::amortization_type_repository::remove_status::removed:
            break;
        case repository::amortization_type_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "amortization_type"});
            break;
        case repository::amortization_type_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "amortization_type",
                 .field = "code",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::amortization_type_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_amortization_types_response
amortization_type_service::delete_many_amortization_types(
    const messaging::delete_many_amortization_types_request& request) {
    messaging::delete_many_amortization_types_response response;
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    for (const auto& removal : request.removals) {
        if (removal.precondition.kind != precondition_kind::any) {
            // The store removes a set in one statement, which carries no
            // per-row version. Refusing is the only answer that keeps the
            // batch atomic: serving it as a sequence of single removals would
            // leave a partial batch behind as soon as one row had moved on.
            response.result = refuse(outcome_code::batch_removal_is_unconditional);
            return response;
        }
    }
    if (request.removals.empty())
        return response;
    std::vector<std::string> code_keys;
    code_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        code_keys.push_back(removal.key.code);
    repo_.remove(ctx_, code_keys);
    return response;
}

messaging::list_amortization_type_versions_response
amortization_type_service::list_amortization_type_versions(
    const messaging::list_amortization_type_versions_request& request) {
    messaging::list_amortization_type_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "amortization types", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "amortization types"});
        return response;
    }
    auto all = repo_.read_all(ctx_, request.key.code);
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

messaging::get_amortization_type_version_response
amortization_type_service::get_amortization_type_version(
    const messaging::get_amortization_type_version_request& request) {
    messaging::get_amortization_type_version_response response;
    auto found =
        repo_.read_at_version(ctx_, request.key.amortization_type.code, request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "amortization_type"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
amortization_type_service::prepare_change(const messaging::amortization_type_change& change,
                                          const ores::utility::domain::change_intent& intent,
                                          domain::amortization_type& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, ctx_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "amortization_type", .field = "code"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "amortization_type"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "amortization_type",
                               .field = "code",
                               .expected = change.precondition.version ?
                                               std::to_string(*change.precondition.version) :
                                               std::string{},
                               .current = std::to_string(current.front().version)});
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


std::vector<domain::amortization_type>
amortization_type_service::list_amortization_types(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all amortization types";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t amortization_type_service::count_amortization_types() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total amortization types count";
    return repo_.get_total_amortization_type_count(ctx_);
}


std::optional<domain::amortization_type>
amortization_type_service::get_amortization_type_at_version(const std::string& code,
                                                            std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting amortization type at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::amortization_type>
amortization_type_service::get_amortization_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting amortization type. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::amortization_type>
amortization_type_service::get_amortization_types(const std::vector<std::string>& codes) {
    return repo_.read_latest(ctx_, codes);
}

void amortization_type_service::save_amortization_type(const domain::amortization_type& v) {
    if (v.code.empty())
        throw std::invalid_argument("Amortization Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving amortization type. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved amortization type. " << "code: " << v.code;
}

void amortization_type_service::save_amortization_types(
    const std::vector<domain::amortization_type>& amortization_types) {
    for (const auto& e : amortization_types) {
        if (e.code.empty())
            throw std::invalid_argument("Amortization Type code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << amortization_types.size() << " amortization types";
    auto ts = amortization_types;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void amortization_type_service::delete_amortization_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing amortization type. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed amortization type. " << "code: " << code;
}

void amortization_type_service::delete_amortization_types(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::amortization_type>
amortization_type_service::get_amortization_type_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for amortization type. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
