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
#include "ores.refdata.core/service/credit_limit_service.hpp"
#include "ores.refdata.api/domain/credit_limit.hpp"
#include "ores.refdata.api/messaging/credit_limit_protocol.hpp"
#include "ores.refdata.core/repository/credit_limit_repository.hpp"
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
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

namespace ores::refdata::service {

using namespace ores::logging;

credit_limit_service::credit_limit_service(context ctx)
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
std::vector<domain::credit_limit> read_one(repository::credit_limit_repository& repo,
                                           const ores::database::context& ctx,
                                           const messaging::credit_limit_key& key) {
    return repo.read_latest_by_code(ctx, key.code);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::credit_limit_key key_from(const domain::credit_limit& v) {
    messaging::credit_limit_key key;
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
domain::credit_limit to_domain(const messaging::credit_limit_write& write) {
    domain::credit_limit v;
    v.id = write.id;
    v.code = write.code;
    v.party_id = write.party_id;
    v.counterparty_id = write.counterparty_id;
    v.limit_amount = write.limit_amount;
    v.description = write.description;
    return v;
}


}

messaging::list_credit_limits_response
credit_limit_service::list_credit_limits(const messaging::list_credit_limits_request& request) {
    messaging::list_credit_limits_response response;
    if (!request.order.field.empty() &&
        !repository::credit_limit_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "credit limits", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->counterparty_id_one_of &&
        request.filter->counterparty_id_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "counterparty_id_one_of", .limit = "1000"});
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
    response.credit_limits = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_credit_limit_count(ctx_, request.filter, as_of);
    return response;
}

messaging::list_by_counterparty_id_credit_limits_response
credit_limit_service::list_by_counterparty_id_credit_limits(
    const messaging::list_by_counterparty_id_credit_limits_request& request) {
    messaging::list_by_counterparty_id_credit_limits_response response;
    if (!request.order.field.empty() &&
        !repository::credit_limit_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "credit limits", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->counterparty_id_one_of &&
        request.filter->counterparty_id_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "counterparty_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result = refuse(outcome_code::scope_not_supported, {.entity = "credit limits"});
        return response;
    }
    const auto relation = boost::uuids::to_string(request.counterparty_id);
    response.credit_limits = repo_.read_latest_by_counterparty_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total =
        repo_.get_total_credit_limit_count_by_counterparty_id(ctx_, relation, request.filter);
    return response;
}

messaging::get_credit_limit_response
credit_limit_service::get_credit_limit(const messaging::get_credit_limit_request& request) {
    messaging::get_credit_limit_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "credit_limit"});
        return response;
    }
    response.credit_limit = std::move(found.front());
    return response;
}

messaging::get_many_credit_limits_response credit_limit_service::get_many_credit_limits(
    const messaging::get_many_credit_limits_request& request) {
    messaging::get_many_credit_limits_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::credit_limit_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.credit_limit = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_credit_limit_response
credit_limit_service::put_credit_limit(const messaging::put_credit_limit_request& request) {
    messaging::put_credit_limit_response response;
    domain::credit_limit value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.credit_limit = std::move(written.front());
    return response;
}

messaging::put_many_credit_limits_response credit_limit_service::put_many_credit_limits(
    const messaging::put_many_credit_limits_request& request) {
    messaging::put_many_credit_limits_response response;
    std::vector<domain::credit_limit> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::credit_limit value;
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
    response.credit_limits.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.credit_limits.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_credit_limit_response
credit_limit_service::delete_credit_limit(const messaging::delete_credit_limit_request& request) {
    messaging::delete_credit_limit_response response;
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
    const auto named = read_one(repo_, ctx_, request.removal.key);
    if (named.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "credit_limit"});
        return response;
    }
    const auto& row = named.front();
    switch (repo_.remove(ctx_, boost::uuids::to_string(row.id), expected)) {
        case repository::credit_limit_repository::remove_status::removed:
            break;
        case repository::credit_limit_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "credit_limit"});
            break;
        case repository::credit_limit_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "credit_limit",
                 .field = "id",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::credit_limit_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_credit_limits_response credit_limit_service::delete_many_credit_limits(
    const messaging::delete_many_credit_limits_request& request) {
    messaging::delete_many_credit_limits_response response;
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
    // A removal names its row by the key a caller holds, and the repository
    // takes the storage key, so the two are joined once here rather than at
    // each column's conversion. A name that matches no row is skipped: the
    // batch reports what it removed, and a row that is already gone is not a
    // failure.
    std::vector<domain::credit_limit> resolved;
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

messaging::list_credit_limit_versions_response credit_limit_service::list_credit_limit_versions(
    const messaging::list_credit_limit_versions_request& request) {
    messaging::list_credit_limit_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "credit limits", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result = refuse(outcome_code::filter_not_supported, {.entity = "credit limits"});
        return response;
    }
    // The versions of the row the caller's key names. The repository reads by
    // the storage key, so the declared key is resolved once here.
    const auto named = read_one(repo_, ctx_, request.key);
    if (named.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "credit_limit"});
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

messaging::get_credit_limit_version_response credit_limit_service::get_credit_limit_version(
    const messaging::get_credit_limit_version_request& request) {
    messaging::get_credit_limit_version_response response;
    // The version key nests the entity's own key, which is the declared one.
    // The repository reads by the storage key, so it is resolved once here.
    const auto named = read_one(repo_, ctx_, request.key.credit_limit);
    if (named.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "credit_limit"});
        return response;
    }
    const auto& row = named.front();
    auto found = repo_.read_at_version(ctx_, boost::uuids::to_string(row.id), request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "credit_limit"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
credit_limit_service::prepare_change(const messaging::credit_limit_change& change,
                                     const ores::utility::domain::change_intent& intent,
                                     domain::credit_limit& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, ctx_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "credit_limit", .field = "id"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "credit_limit"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "credit_limit",
                               .field = "id",
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


std::vector<domain::credit_limit> credit_limit_service::list_credit_limits(std::uint32_t offset,
                                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all credit limits";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t credit_limit_service::count_credit_limits() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total credit limits count";
    return repo_.get_total_credit_limit_count(ctx_);
}


std::vector<domain::credit_limit> credit_limit_service::list_credit_limits_by_counterparty_id(
    const std::string& counterparty_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing credit limits by counterparty_id: " << counterparty_id;
    return repo_.read_latest_by_counterparty_id(ctx_, counterparty_id, offset, limit);
}

std::uint32_t
credit_limit_service::count_credit_limits_by_counterparty_id(const std::string& counterparty_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total credit limits count by counterparty_id: "
                               << counterparty_id;
    return repo_.get_total_credit_limit_count_by_counterparty_id(ctx_, counterparty_id);
}

std::vector<domain::credit_limit> credit_limit_service::list_credit_limits_by_counterparty_id(
    const boost::uuids::uuid& counterparty_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing credit limits by counterparty_id: " << counterparty_id;
    return repo_.read_latest_by_counterparty_id(
        ctx_, boost::uuids::to_string(counterparty_id), offset, limit);
}

std::uint32_t credit_limit_service::count_credit_limits_by_counterparty_id(
    const boost::uuids::uuid& counterparty_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total credit limits count by counterparty_id: "
                               << counterparty_id;
    return repo_.get_total_credit_limit_count_by_counterparty_id(
        ctx_, boost::uuids::to_string(counterparty_id));
}


std::optional<domain::credit_limit>
credit_limit_service::get_credit_limit_at_version(const boost::uuids::uuid& id,
                                                  std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting credit limit at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, boost::uuids::to_string(id), version);
}

std::optional<domain::credit_limit>
credit_limit_service::get_credit_limit(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting credit limit. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::optional<domain::credit_limit>
credit_limit_service::get_credit_limit_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting credit limit by code: " << code;
    messaging::credit_limit_key k;
    k.code = code;
    auto found = read_one(repo_, ctx_, k);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

std::optional<domain::credit_limit>
credit_limit_service::find_credit_limit(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding credit limit. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::credit_limit>
credit_limit_service::get_credit_limits(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void credit_limit_service::save_credit_limit(const domain::credit_limit& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Credit Limit id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving credit limit. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved credit limit. " << "id: " << v.id;
}

void credit_limit_service::save_credit_limits(
    const std::vector<domain::credit_limit>& credit_limits) {
    for (const auto& e : credit_limits) {
        if (e.id.is_nil())
            throw std::invalid_argument("Credit Limit id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << credit_limits.size() << " credit limits";
    auto ts = credit_limits;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void credit_limit_service::delete_credit_limit(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing credit limit. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed credit limit. " << "id: " << id;
}

void credit_limit_service::remove_credit_limit(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing credit limit. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed credit limit. " << "id: " << id;
}

void credit_limit_service::delete_credit_limits(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::credit_limit>
credit_limit_service::get_credit_limit_history(const std::string& key) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for credit limit. key: " << key;
    // The caller holds the key the model declares and this reads by the
    // storage key, so the two are joined here exactly as they are for any
    // other read. Without this step a provider looks the versions up under a
    // value the storage key never holds, and reports an entity that has a
    // history as having none.
    messaging::credit_limit_key k;
    k.code = key;
    // A delete here closes the transaction-time window and leaves every version
    // in place, so resolving through a latest read would lose the history at
    // exactly the moment it is wanted. This takes the newest row carrying the
    // declared key whether or not it is still current, which for a record that
    // still exists is the same row the latest read would have returned.
    const auto found = repo_.read_any_by_code(ctx_, k.code);
    if (found.empty())
        return {};
    const auto& row = found.front();
    return repo_.read_all(ctx_, boost::uuids::to_string(row.id));
}

std::vector<domain::credit_limit>
credit_limit_service::get_credit_limit_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for credit limit. " << "id: " << id;
    return repo_.read_all(ctx_, boost::uuids::to_string(id));
}

}
