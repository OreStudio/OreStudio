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
#include "ores.scheduler.core/service/retained_store_sample_service.hpp"
#include "ores.scheduler.api/domain/retained_store_sample.hpp"
#include "ores.scheduler.api/messaging/retained_store_sample_protocol.hpp"
#include "ores.scheduler.core/repository/retained_store_sample_repository.hpp"
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>
#include <vector>
// Log lines stream uuids with uuid_io's operator<<, which the include check
// does not count as a use.
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;
using ores::utility::domain::max_batch_size;

namespace ores::scheduler::service {

using namespace ores::logging;

retained_store_sample_service::retained_store_sample_service(const context& ctx)
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
std::vector<domain::retained_store_sample>
retained_store_sample_read_one(repository::retained_store_sample_repository& repo,
                               const ores::database::context& ctx,
                               const messaging::retained_store_sample_key& key) {
    return repo.read_latest(ctx,
                            boost::uuids::to_string(key.id),
                            ores::platform::time::datetime::to_db_string(key.sampled_at));
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
std::vector<std::vector<domain::retained_store_sample>>
retained_store_sample_read_many(repository::retained_store_sample_repository& repo,
                                const ores::database::context& ctx,
                                const std::vector<messaging::retained_store_sample_key>& keys) {
    std::vector<std::vector<domain::retained_store_sample>> found(keys.size());
    using key_tuple = std::tuple<std::string, std::string>;
    std::vector<std::string> id_keys;
    id_keys.reserve(keys.size());
    std::vector<std::string> sampled_at_keys;
    sampled_at_keys.reserve(keys.size());
    for (const auto& key : keys) {
        id_keys.push_back(boost::uuids::to_string(key.id));
        sampled_at_keys.push_back(ores::platform::time::datetime::to_db_string(key.sampled_at));
    }
    std::map<key_tuple, std::vector<domain::retained_store_sample>> by_key;
    for (auto& row : repo.read_latest(ctx, id_keys, sampled_at_keys)) {
        key_tuple named(boost::uuids::to_string(row.id),
                        ores::platform::time::datetime::to_db_string(row.sampled_at));
        by_key[std::move(named)].push_back(std::move(row));
    }
    for (std::size_t i = 0; i < keys.size(); ++i) {
        const auto& key = keys[i];
        const auto rows =
            by_key.find(key_tuple(boost::uuids::to_string(key.id),
                                  ores::platform::time::datetime::to_db_string(key.sampled_at)));
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
messaging::retained_store_sample_key
retained_store_sample_key_from(const domain::retained_store_sample& v) {
    messaging::retained_store_sample_key key;
    key.id = v.id;
    key.sampled_at = v.sampled_at;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::retained_store_sample
retained_store_sample_to_domain(const messaging::retained_store_sample_write& write) {
    domain::retained_store_sample v;
    v.id = write.id;
    v.sampled_at = write.sampled_at;
    v.store_code = write.store_code;
    v.size_bytes = write.size_bytes;
    v.measure_count = write.measure_count;
    return v;
}


}

messaging::list_retained_store_samples_response
retained_store_sample_service::list_retained_store_samples(
    const messaging::list_retained_store_samples_request& request) {
    messaging::list_retained_store_samples_response response;
    if (!request.order.field.empty() &&
        !repository::retained_store_sample_repository::is_sortable(request.order.field)) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "retained store samples", .field = request.order.field});
        return response;
    }
    response.samples = repo_.read_latest(ctx_, request.offset, request.limit, request.order);
    response.total = repo_.get_total_sample_count(ctx_);
    return response;
}

messaging::get_retained_store_sample_response
retained_store_sample_service::get_retained_store_sample(
    const messaging::get_retained_store_sample_request& request) {
    messaging::get_retained_store_sample_response response;
    auto found = retained_store_sample_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "retained_store_sample"});
        return response;
    }
    response.retained_store_sample = std::move(found.front());
    return response;
}

messaging::get_many_retained_store_samples_response
retained_store_sample_service::get_many_retained_store_samples(
    const messaging::get_many_retained_store_samples_request& request) {
    messaging::get_many_retained_store_samples_response response;
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
    auto found = retained_store_sample_read_many(repo_, ctx_, request.keys);
    for (std::size_t i = 0; i < request.keys.size(); ++i) {
        messaging::retained_store_sample_lookup entry;
        entry.key = request.keys[i];
        if (!found[i].empty())
            entry.retained_store_sample = std::move(found[i].front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_retained_store_sample_response
retained_store_sample_service::put_retained_store_sample(
    const messaging::put_retained_store_sample_request& request) {
    messaging::put_retained_store_sample_response response;
    domain::retained_store_sample value;
    const auto current = retained_store_sample_read_one(
        repo_,
        ctx_,
        retained_store_sample_key_from(retained_store_sample_to_domain(request.change.write)));
    response.result = prepare_change(request.change, request.intent, current, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written =
        retained_store_sample_read_one(repo_, ctx_, retained_store_sample_key_from(value));
    if (!written.empty())
        response.retained_store_sample = std::move(written.front());
    return response;
}

messaging::put_many_retained_store_samples_response
retained_store_sample_service::put_many_retained_store_samples(
    const messaging::put_many_retained_store_samples_request& request) {
    messaging::put_many_retained_store_samples_response response;
    if (request.changes.size() > max_batch_size) {
        response.result =
            refuse(outcome_code::batch_too_large,
                   {.entity = "retained_store_sample", .limit = std::to_string(max_batch_size)});
        return response;
    }
    // The rows the set names are read once, before any change is checked, so
    // a set of n rows costs one read rather than n.
    std::vector<messaging::retained_store_sample_key> keys;
    keys.reserve(request.changes.size());
    for (const auto& change : request.changes)
        keys.push_back(
            retained_store_sample_key_from(retained_store_sample_to_domain(change.write)));
    const auto current = retained_store_sample_read_many(repo_, ctx_, keys);
    std::vector<domain::retained_store_sample> batch;
    batch.reserve(request.changes.size());
    for (std::size_t i = 0; i < request.changes.size(); ++i) {
        domain::retained_store_sample value;
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
    response.samples.reserve(batch.size());
    // The keys of the written rows are the keys read above: a write keeps
    // the key its change states.
    auto written = retained_store_sample_read_many(repo_, ctx_, keys);
    for (std::size_t i = 0; i < batch.size(); ++i) {
        response.samples.push_back(written[i].empty() ? batch[i] : std::move(written[i].front()));
    }
    return response;
}

messaging::delete_retained_store_sample_response
retained_store_sample_service::delete_retained_store_sample(
    const messaging::delete_retained_store_sample_request& request) {
    messaging::delete_retained_store_sample_response response;
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
    switch (
        repo_.remove(ctx_,
                     boost::uuids::to_string(request.removal.key.id),
                     ores::platform::time::datetime::to_db_string(request.removal.key.sampled_at),
                     expected)) {
        case repository::retained_store_sample_repository::remove_status::removed:
            break;
        case repository::retained_store_sample_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "retained_store_sample"});
            break;
        case repository::retained_store_sample_repository::remove_status::conflicting: {
            // A resource that keeps no version cannot reach this status, so there
            // is no current version for its sentence to state.
            response.result =
                refuse(outcome_code::version_conflict,
                       {.entity = "retained_store_sample",
                        .field = "id",
                        .expected = expected ? std::to_string(*expected) : std::string{}});
            break;
        }
        case repository::retained_store_sample_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_retained_store_samples_response
retained_store_sample_service::delete_many_retained_store_samples(
    const messaging::delete_many_retained_store_samples_request& request) {
    messaging::delete_many_retained_store_samples_response response;
    if (request.removals.size() > max_batch_size) {
        response.result =
            refuse(outcome_code::batch_too_large,
                   {.entity = "retained_store_sample", .limit = std::to_string(max_batch_size)});
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
    std::vector<std::string> id_keys;
    id_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        id_keys.push_back(boost::uuids::to_string(removal.key.id));
    std::vector<std::string> sampled_at_keys;
    sampled_at_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        sampled_at_keys.push_back(
            ores::platform::time::datetime::to_db_string(removal.key.sampled_at));
    repo_.remove(ctx_, id_keys, sampled_at_keys);
    return response;
}

ores::utility::domain::result retained_store_sample_service::prepare_change(
    const messaging::retained_store_sample_change& change,
    const ores::utility::domain::change_intent& intent,
    const std::vector<domain::retained_store_sample>& current,
    domain::retained_store_sample& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = retained_store_sample_to_domain(change.write);
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "retained_store_sample", .field = "id"});
            break;
        case precondition_kind::must_match_version:
            return refuse(outcome_code::precondition_not_supported);
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


std::vector<domain::retained_store_sample>
retained_store_sample_service::list_samples(std::uint32_t offset, std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Listing all retained store samples";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t retained_store_sample_service::count_samples() {
    ORES_LOG_SEV(lg(), debug) << "Getting total retained store samples count";
    return repo_.get_total_sample_count(ctx_);
}


std::optional<domain::retained_store_sample>
retained_store_sample_service::get_sample(const std::string& id, const std::string& sampled_at) {
    ORES_LOG_SEV(lg(), debug) << "Getting retained store sample. " << "id: " << id
                              << " sampled_at: " << sampled_at;
    auto results = repo_.read_latest(ctx_, id, sampled_at);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::retained_store_sample>
retained_store_sample_service::get_samples(const std::vector<std::string>& ids,
                                           const std::vector<std::string>& sampled_ats) {
    return repo_.read_latest(ctx_, ids, sampled_ats);
}

void retained_store_sample_service::save_sample(const domain::retained_store_sample& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Retained Store Sample id cannot be empty.");
    ORES_LOG_SEV(lg(), debug) << "Saving retained store sample. " << "id: " << v.id
                              << " sampled_at: " << v.sampled_at;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    ORES_LOG_SEV(lg(), info) << "Saved retained store sample. " << "id: " << v.id
                             << " sampled_at: " << v.sampled_at;
}

void retained_store_sample_service::save_samples(
    const std::vector<domain::retained_store_sample>& samples) {
    for (const auto& e : samples) {
        if (e.id.is_nil())
            throw std::invalid_argument("Retained Store Sample id cannot be empty.");
    }
    ORES_LOG_SEV(lg(), debug) << "Saving " << samples.size() << " retained store samples";
    auto ts = samples;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void retained_store_sample_service::delete_sample(const std::string& id,
                                                  const std::string& sampled_at) {
    ORES_LOG_SEV(lg(), debug) << "Removing retained store sample. " << "id: " << id
                              << " sampled_at: " << sampled_at;
    repo_.remove(ctx_, id, sampled_at);
    ORES_LOG_SEV(lg(), info) << "Removed retained store sample. " << "id: " << id
                             << " sampled_at: " << sampled_at;
}

void retained_store_sample_service::delete_samples(const std::vector<std::string>& ids,
                                                   const std::vector<std::string>& sampled_ats) {
    repo_.remove(ctx_, ids, sampled_ats);
}


}
