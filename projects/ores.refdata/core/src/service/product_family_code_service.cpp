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
#include "ores.refdata.core/service/product_family_code_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/product_family_code.hpp"
#include "ores.refdata.api/messaging/product_family_code_protocol.hpp"
#include "ores.refdata.core/repository/product_family_code_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;
using ores::utility::domain::max_batch_size;

namespace ores::refdata::service {

using namespace ores::logging;

product_family_code_service::product_family_code_service(const context& ctx)
    : ctx_(ctx) {}
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
std::vector<domain::product_family_code>
product_family_code_read_one(repository::product_family_code_repository& repo,
                             const ores::database::context& ctx,
                             const messaging::product_family_code_key& key) {
    return repo.read_latest(ctx, key.code);
}

/**
 * @brief The current rows a key set names, one entry per key in the keys' order.
 *
 * Each entry holds what the one-key read holds for its key, so a batch checks
 * each row against the same answer a single request gets. The repository
 * reads the key set in one batch read.
 *
 * A row is joined back to its key by the text form of each key column. Both
 * sides convert the typed value with the same function, and the store compares
 * key text exactly, so every row the read returns finds its key.
 *
 * When the declared key is not the storage key the batch is read by the
 * declared key instead. A model that states its own read by that key has no
 * batch form of it, so its keys are read one at a time.
 */
std::vector<std::vector<domain::product_family_code>>
product_family_code_read_many(repository::product_family_code_repository& repo,
                              const ores::database::context& ctx,
                              const std::vector<messaging::product_family_code_key>& keys) {
    std::vector<std::vector<domain::product_family_code>> found(keys.size());
    using key_tuple = std::tuple<std::string>;
    std::vector<std::string> code_keys;
    code_keys.reserve(keys.size());
    for (const auto& key : keys) {
        code_keys.push_back(key.code);
    }
    std::map<key_tuple, std::vector<domain::product_family_code>> by_key;
    for (auto& row : repo.read_latest(ctx, code_keys)) {
        key_tuple named(row.code);
        by_key[std::move(named)].push_back(std::move(row));
    }
    for (std::size_t i = 0; i < keys.size(); ++i) {
        const auto& key = keys[i];
        const auto rows = by_key.find(key_tuple(key.code));
        if (rows != by_key.end())
            found[i] = rows->second;
    }
    return found;
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::product_family_code_key
product_family_code_key_from(const domain::product_family_code& v) {
    messaging::product_family_code_key key;
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
domain::product_family_code
product_family_code_to_domain(const messaging::product_family_code_write& write) {
    domain::product_family_code v;
    v.code = write.code;
    v.name = write.name;
    v.description = write.description;
    v.display_order = write.display_order;
    return v;
}


}

messaging::list_product_family_codes_response
product_family_code_service::list_product_family_codes(
    const messaging::list_product_family_codes_request& request) {
    messaging::list_product_family_codes_response response;
    if (!request.order.field.empty() &&
        !repository::product_family_code_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "product families", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->code_one_of &&
        request.filter->code_one_of->size() > max_batch_size) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "code_one_of", .limit = std::to_string(max_batch_size)});
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
    response.families = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_family_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_product_family_code_response product_family_code_service::get_product_family_code(
    const messaging::get_product_family_code_request& request) {
    messaging::get_product_family_code_response response;
    auto found = product_family_code_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "product_family_code"});
        return response;
    }
    response.product_family_code = std::move(found.front());
    return response;
}

messaging::get_many_product_family_codes_response
product_family_code_service::get_many_product_family_codes(
    const messaging::get_many_product_family_codes_request& request) {
    messaging::get_many_product_family_codes_response response;
    // A key set carries at most as many keys as the protocol lets a batch
    // carry, so one request cannot ask for an unbounded reply.
    if (request.keys.size() > max_batch_size) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "keys", .limit = std::to_string(max_batch_size)});
        return response;
    }
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    auto found = product_family_code_read_many(repo_, ctx_, request.keys);
    for (std::size_t i = 0; i < request.keys.size(); ++i) {
        messaging::product_family_code_lookup entry;
        entry.key = request.keys[i];
        if (!found[i].empty())
            entry.product_family_code = std::move(found[i].front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_product_family_code_response product_family_code_service::put_product_family_code(
    const messaging::put_product_family_code_request& request) {
    messaging::put_product_family_code_response response;
    domain::product_family_code value;
    const auto current = product_family_code_read_one(
        repo_,
        ctx_,
        product_family_code_key_from(product_family_code_to_domain(request.change.write)));
    response.result = prepare_change(request.change, request.intent, current, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = product_family_code_read_one(repo_, ctx_, product_family_code_key_from(value));
    if (!written.empty())
        response.product_family_code = std::move(written.front());
    return response;
}

messaging::put_many_product_family_codes_response
product_family_code_service::put_many_product_family_codes(
    const messaging::put_many_product_family_codes_request& request) {
    messaging::put_many_product_family_codes_response response;
    if (request.changes.size() > max_batch_size) {
        response.result =
            refuse(outcome_code::batch_too_large,
                   {.entity = "product_family_code", .limit = std::to_string(max_batch_size)});
        return response;
    }
    // The rows the set names are read once, before any change is checked, so
    // a set of n rows costs one read rather than n.
    std::vector<messaging::product_family_code_key> keys;
    keys.reserve(request.changes.size());
    for (const auto& change : request.changes)
        keys.push_back(product_family_code_key_from(product_family_code_to_domain(change.write)));
    const auto current = product_family_code_read_many(repo_, ctx_, keys);
    std::vector<domain::product_family_code> batch;
    batch.reserve(request.changes.size());
    for (std::size_t i = 0; i < request.changes.size(); ++i) {
        domain::product_family_code value;
        const auto result = prepare_change(request.changes[i], request.intent, current[i], value);
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
    response.families.reserve(batch.size());
    // The keys of the written rows are the keys read above: a write keeps
    // the key its change states.
    auto written = product_family_code_read_many(repo_, ctx_, keys);
    for (std::size_t i = 0; i < batch.size(); ++i) {
        response.families.push_back(written[i].empty() ? batch[i] : std::move(written[i].front()));
    }
    return response;
}

messaging::delete_product_family_code_response
product_family_code_service::delete_product_family_code(
    const messaging::delete_product_family_code_request& request) {
    messaging::delete_product_family_code_response response;
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
        case repository::product_family_code_repository::remove_status::removed:
            break;
        case repository::product_family_code_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "product_family_code"});
            break;
        case repository::product_family_code_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = product_family_code_read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "product_family_code",
                 .field = "code",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::product_family_code_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_product_family_codes_response
product_family_code_service::delete_many_product_family_codes(
    const messaging::delete_many_product_family_codes_request& request) {
    messaging::delete_many_product_family_codes_response response;
    if (request.removals.size() > max_batch_size) {
        response.result =
            refuse(outcome_code::batch_too_large,
                   {.entity = "product_family_code", .limit = std::to_string(max_batch_size)});
        return response;
    }
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

messaging::list_product_family_code_versions_response
product_family_code_service::list_product_family_code_versions(
    const messaging::list_product_family_code_versions_request& request) {
    messaging::list_product_family_code_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "product families", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "product families"});
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

messaging::get_product_family_code_version_response
product_family_code_service::get_product_family_code_version(
    const messaging::get_product_family_code_version_request& request) {
    messaging::get_product_family_code_version_response response;
    auto found =
        repo_.read_at_version(ctx_, request.key.product_family_code.code, request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "product_family_code"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
product_family_code_service::prepare_change(const messaging::product_family_code_change& change,
                                            const ores::utility::domain::change_intent& intent,
                                            const std::vector<domain::product_family_code>& current,
                                            domain::product_family_code& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = product_family_code_to_domain(change.write);
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "product_family_code", .field = "code"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "product_family_code"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "product_family_code",
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


std::vector<domain::product_family_code>
product_family_code_service::list_families(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all product families";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t product_family_code_service::count_families() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total product families count";
    return repo_.get_total_family_count(ctx_);
}


std::optional<domain::product_family_code>
product_family_code_service::get_family_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting product family at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::product_family_code>
product_family_code_service::get_family(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting product family. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::product_family_code>
product_family_code_service::get_families(const std::vector<std::string>& codes) {
    return repo_.read_latest(ctx_, codes);
}

void product_family_code_service::save_family(const domain::product_family_code& v) {
    if (v.code.empty())
        throw std::invalid_argument("Product Family Code code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving product family. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved product family. " << "code: " << v.code;
}

void product_family_code_service::save_families(
    const std::vector<domain::product_family_code>& families) {
    for (const auto& e : families) {
        if (e.code.empty())
            throw std::invalid_argument("Product Family Code code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << families.size() << " product families";
    auto ts = families;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void product_family_code_service::delete_family(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing product family. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed product family. " << "code: " << code;
}

void product_family_code_service::delete_families(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::product_family_code>
product_family_code_service::get_family_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for product family. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
