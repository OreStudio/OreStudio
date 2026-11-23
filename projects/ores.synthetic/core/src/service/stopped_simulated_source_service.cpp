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
#include "ores.synthetic.core/service/stopped_simulated_source_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.synthetic.api/domain/stopped_simulated_source.hpp"
#include "ores.synthetic.api/messaging/stopped_simulated_source_protocol.hpp"
#include "ores.synthetic.core/repository/stopped_simulated_source_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
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

namespace ores::synthetic::service {

using namespace ores::logging;

stopped_simulated_source_service::stopped_simulated_source_service(const context& ctx)
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
std::vector<domain::stopped_simulated_source>
stopped_simulated_source_read_one(repository::stopped_simulated_source_repository& repo,
                                  const ores::database::context& ctx,
                                  const messaging::stopped_simulated_source_key& key) {
    return repo.read_latest(ctx, key.source_name);
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
std::vector<std::vector<domain::stopped_simulated_source>> stopped_simulated_source_read_many(
    repository::stopped_simulated_source_repository& repo,
    const ores::database::context& ctx,
    const std::vector<messaging::stopped_simulated_source_key>& keys) {
    std::vector<std::vector<domain::stopped_simulated_source>> found(keys.size());
    using key_tuple = std::tuple<std::string>;
    std::vector<std::string> source_name_keys;
    source_name_keys.reserve(keys.size());
    for (const auto& key : keys) {
        source_name_keys.push_back(key.source_name);
    }
    std::map<key_tuple, std::vector<domain::stopped_simulated_source>> by_key;
    for (auto& row : repo.read_latest(ctx, source_name_keys)) {
        key_tuple named(row.source_name);
        by_key[std::move(named)].push_back(std::move(row));
    }
    for (std::size_t i = 0; i < keys.size(); ++i) {
        const auto& key = keys[i];
        const auto rows = by_key.find(key_tuple(key.source_name));
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
messaging::stopped_simulated_source_key
stopped_simulated_source_key_from(const domain::stopped_simulated_source& v) {
    messaging::stopped_simulated_source_key key;
    key.source_name = v.source_name;
    return key;
}

/**
 * @brief Builds the domain object a write record states.
 *
 * The record carries the user-owned fields and nothing else: tenancy,
 * provenance, the version and the validity window are the service's and the
 * database's to state, and are set after this conversion.
 */
domain::stopped_simulated_source
stopped_simulated_source_to_domain(const messaging::stopped_simulated_source_write& write) {
    domain::stopped_simulated_source v;
    v.source_name = write.source_name;
    v.provider_code = write.provider_code;
    return v;
}


}

messaging::list_stopped_simulated_sources_response
stopped_simulated_source_service::list_stopped_simulated_sources(
    const messaging::list_stopped_simulated_sources_request& request) {
    messaging::list_stopped_simulated_sources_response response;
    if (!request.order.field.empty() &&
        !repository::stopped_simulated_source_repository::is_sortable(request.order.field)) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "stopped simulated sources", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->source_name_one_of &&
        request.filter->source_name_one_of->size() > max_batch_size) {
        response.result =
            refuse(outcome_code::filter_too_large,
                   {.field = "source_name_one_of", .limit = std::to_string(max_batch_size)});
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
    response.stopped_sources = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_stopped_source_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_stopped_simulated_source_response
stopped_simulated_source_service::get_stopped_simulated_source(
    const messaging::get_stopped_simulated_source_request& request) {
    messaging::get_stopped_simulated_source_response response;
    auto found = stopped_simulated_source_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "stopped_simulated_source"});
        return response;
    }
    response.stopped_simulated_source = std::move(found.front());
    return response;
}

messaging::get_many_stopped_simulated_sources_response
stopped_simulated_source_service::get_many_stopped_simulated_sources(
    const messaging::get_many_stopped_simulated_sources_request& request) {
    messaging::get_many_stopped_simulated_sources_response response;
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
    auto found = stopped_simulated_source_read_many(repo_, ctx_, request.keys);
    for (std::size_t i = 0; i < request.keys.size(); ++i) {
        messaging::stopped_simulated_source_lookup entry;
        entry.key = request.keys[i];
        if (!found[i].empty())
            entry.stopped_simulated_source = std::move(found[i].front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_stopped_simulated_source_response
stopped_simulated_source_service::put_stopped_simulated_source(
    const messaging::put_stopped_simulated_source_request& request) {
    messaging::put_stopped_simulated_source_response response;
    domain::stopped_simulated_source value;
    const auto current = stopped_simulated_source_read_one(
        repo_,
        ctx_,
        stopped_simulated_source_key_from(
            stopped_simulated_source_to_domain(request.change.write)));
    response.result = prepare_change(request.change, request.intent, current, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written =
        stopped_simulated_source_read_one(repo_, ctx_, stopped_simulated_source_key_from(value));
    if (!written.empty())
        response.stopped_simulated_source = std::move(written.front());
    return response;
}

messaging::put_many_stopped_simulated_sources_response
stopped_simulated_source_service::put_many_stopped_simulated_sources(
    const messaging::put_many_stopped_simulated_sources_request& request) {
    messaging::put_many_stopped_simulated_sources_response response;
    if (request.changes.size() > max_batch_size) {
        response.result =
            refuse(outcome_code::batch_too_large,
                   {.entity = "stopped_simulated_source", .limit = std::to_string(max_batch_size)});
        return response;
    }
    // The rows the set names are read once, before any change is checked, so
    // a set of n rows costs one read rather than n.
    std::vector<messaging::stopped_simulated_source_key> keys;
    keys.reserve(request.changes.size());
    for (const auto& change : request.changes)
        keys.push_back(
            stopped_simulated_source_key_from(stopped_simulated_source_to_domain(change.write)));
    const auto current = stopped_simulated_source_read_many(repo_, ctx_, keys);
    std::vector<domain::stopped_simulated_source> batch;
    batch.reserve(request.changes.size());
    for (std::size_t i = 0; i < request.changes.size(); ++i) {
        domain::stopped_simulated_source value;
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
    response.stopped_sources.reserve(batch.size());
    // The keys of the written rows are the keys read above: a write keeps
    // the key its change states.
    auto written = stopped_simulated_source_read_many(repo_, ctx_, keys);
    for (std::size_t i = 0; i < batch.size(); ++i) {
        response.stopped_sources.push_back(written[i].empty() ? batch[i] :
                                                                std::move(written[i].front()));
    }
    return response;
}

messaging::delete_stopped_simulated_source_response
stopped_simulated_source_service::delete_stopped_simulated_source(
    const messaging::delete_stopped_simulated_source_request& request) {
    messaging::delete_stopped_simulated_source_response response;
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
    switch (repo_.remove(ctx_, request.removal.key.source_name, expected)) {
        case repository::stopped_simulated_source_repository::remove_status::removed:
            break;
        case repository::stopped_simulated_source_repository::remove_status::missing:
            response.result =
                refuse(outcome_code::not_found, {.entity = "stopped_simulated_source"});
            break;
        case repository::stopped_simulated_source_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = stopped_simulated_source_read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "stopped_simulated_source",
                 .field = "source_name",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::stopped_simulated_source_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_stopped_simulated_sources_response
stopped_simulated_source_service::delete_many_stopped_simulated_sources(
    const messaging::delete_many_stopped_simulated_sources_request& request) {
    messaging::delete_many_stopped_simulated_sources_response response;
    if (request.removals.size() > max_batch_size) {
        response.result =
            refuse(outcome_code::batch_too_large,
                   {.entity = "stopped_simulated_source", .limit = std::to_string(max_batch_size)});
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
    std::vector<std::string> source_name_keys;
    source_name_keys.reserve(request.removals.size());
    for (const auto& removal : request.removals)
        source_name_keys.push_back(removal.key.source_name);
    repo_.remove(ctx_, source_name_keys);
    return response;
}

messaging::list_stopped_simulated_source_versions_response
stopped_simulated_source_service::list_stopped_simulated_source_versions(
    const messaging::list_stopped_simulated_source_versions_request& request) {
    messaging::list_stopped_simulated_source_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "stopped simulated sources", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "stopped simulated sources"});
        return response;
    }
    auto all = repo_.read_all(ctx_, request.key.source_name);
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

messaging::get_stopped_simulated_source_version_response
stopped_simulated_source_service::get_stopped_simulated_source_version(
    const messaging::get_stopped_simulated_source_version_request& request) {
    messaging::get_stopped_simulated_source_version_response response;
    auto found = repo_.read_at_version(
        ctx_, request.key.stopped_simulated_source.source_name, request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "stopped_simulated_source"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result stopped_simulated_source_service::prepare_change(
    const messaging::stopped_simulated_source_change& change,
    const ores::utility::domain::change_intent& intent,
    const std::vector<domain::stopped_simulated_source>& current,
    domain::stopped_simulated_source& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = stopped_simulated_source_to_domain(change.write);
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "stopped_simulated_source", .field = "source_name"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "stopped_simulated_source"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "stopped_simulated_source",
                               .field = "source_name",
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


std::vector<domain::stopped_simulated_source>
stopped_simulated_source_service::list_stopped_sources(std::uint32_t offset, std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Listing all stopped simulated sources";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t stopped_simulated_source_service::count_stopped_sources() {
    ORES_LOG_SEV(lg(), debug) << "Getting total stopped simulated sources count";
    return repo_.get_total_stopped_source_count(ctx_);
}


std::optional<domain::stopped_simulated_source>
stopped_simulated_source_service::get_stopped_source_at_version(const std::string& source_name,
                                                                std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Getting stopped simulated source at version. "
                              << "source_name: " << source_name << " version: " << version;
    return repo_.read_at_version(ctx_, source_name, version);
}

std::optional<domain::stopped_simulated_source>
stopped_simulated_source_service::get_stopped_source(const std::string& source_name) {
    ORES_LOG_SEV(lg(), debug) << "Getting stopped simulated source. "
                              << "source_name: " << source_name;
    auto results = repo_.read_latest(ctx_, source_name);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::stopped_simulated_source> stopped_simulated_source_service::get_stopped_sources(
    const std::vector<std::string>& source_names) {
    return repo_.read_latest(ctx_, source_names);
}

void stopped_simulated_source_service::save_stopped_source(
    const domain::stopped_simulated_source& v) {
    if (v.source_name.empty())
        throw std::invalid_argument("Stopped Simulated Source source_name cannot be empty.");
    ORES_LOG_SEV(lg(), debug) << "Saving stopped simulated source. "
                              << "source_name: " << v.source_name;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    ORES_LOG_SEV(lg(), info) << "Saved stopped simulated source. "
                             << "source_name: " << v.source_name;
}

void stopped_simulated_source_service::save_stopped_sources(
    const std::vector<domain::stopped_simulated_source>& stopped_sources) {
    for (const auto& e : stopped_sources) {
        if (e.source_name.empty())
            throw std::invalid_argument("Stopped Simulated Source source_name cannot be empty.");
    }
    ORES_LOG_SEV(lg(), debug) << "Saving " << stopped_sources.size()
                              << " stopped simulated sources";
    auto ts = stopped_sources;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void stopped_simulated_source_service::delete_stopped_source(const std::string& source_name) {
    ORES_LOG_SEV(lg(), debug) << "Removing stopped simulated source. "
                              << "source_name: " << source_name;
    repo_.remove(ctx_, source_name);
    ORES_LOG_SEV(lg(), info) << "Removed stopped simulated source. "
                             << "source_name: " << source_name;
}

void stopped_simulated_source_service::delete_stopped_sources(
    const std::vector<std::string>& source_names) {
    repo_.remove(ctx_, source_names);
}

std::vector<domain::stopped_simulated_source>
stopped_simulated_source_service::get_stopped_source_history(const std::string& source_name) {
    ORES_LOG_SEV(lg(), debug) << "Getting history for stopped simulated source. "
                              << "source_name: " << source_name;
    return repo_.read_all(ctx_, source_name);
}

}
