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
#include "ores.trading.core/service/lifecycle_event_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.trading.api/domain/lifecycle_event.hpp"
#include "ores.trading.api/messaging/lifecycle_event_protocol.hpp"
#include "ores.trading.core/repository/lifecycle_event_repository.hpp"
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

lifecycle_event_service::lifecycle_event_service(context ctx)
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
std::vector<domain::lifecycle_event> read_one(repository::lifecycle_event_repository& repo,
                                              const ores::database::context& ctx,
                                              const messaging::lifecycle_event_key& key) {
    return repo.read_latest(ctx, key.code);
}

/**
 * @brief The key a domain object states, so a written row can be read back.
 *
 * A create states its own key in the write record, so the key of the row a
 * write produced is the one the object carries.
 */
messaging::lifecycle_event_key key_from(const domain::lifecycle_event& v) {
    messaging::lifecycle_event_key key;
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
domain::lifecycle_event to_domain(const messaging::lifecycle_event_write& write) {
    domain::lifecycle_event v;
    v.code = write.code;
    v.description = write.description;
    v.fsm_state_id = write.fsm_state_id;
    return v;
}

}

messaging::list_lifecycle_events_response lifecycle_event_service::list_lifecycle_events(
    const messaging::list_lifecycle_events_request& request) {
    messaging::list_lifecycle_events_response response;
    if (!request.order.field.empty() &&
        !repository::lifecycle_event_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "lifecycle events", .field = request.order.field});
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
    response.events = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_event_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_lifecycle_event_response lifecycle_event_service::get_lifecycle_event(
    const messaging::get_lifecycle_event_request& request) {
    messaging::get_lifecycle_event_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "lifecycle_event"});
        return response;
    }
    response.lifecycle_event = std::move(found.front());
    return response;
}

messaging::get_many_lifecycle_events_response lifecycle_event_service::get_many_lifecycle_events(
    const messaging::get_many_lifecycle_events_request& request) {
    messaging::get_many_lifecycle_events_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::lifecycle_event_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.lifecycle_event = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::put_lifecycle_event_response lifecycle_event_service::put_lifecycle_event(
    const messaging::put_lifecycle_event_request& request) {
    messaging::put_lifecycle_event_response response;
    domain::lifecycle_event value;
    response.result = prepare_change(request.change, request.intent, value);
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    repo_.write(ctx_, value, request.change.precondition);
    auto written = read_one(repo_, ctx_, key_from(value));
    if (!written.empty())
        response.lifecycle_event = std::move(written.front());
    return response;
}

messaging::put_many_lifecycle_events_response lifecycle_event_service::put_many_lifecycle_events(
    const messaging::put_many_lifecycle_events_request& request) {
    messaging::put_many_lifecycle_events_response response;
    std::vector<domain::lifecycle_event> batch;
    batch.reserve(request.changes.size());
    for (const auto& change : request.changes) {
        domain::lifecycle_event value;
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
    response.events.reserve(batch.size());
    for (const auto& value : batch) {
        auto written = read_one(repo_, ctx_, key_from(value));
        response.events.push_back(written.empty() ? value : std::move(written.front()));
    }
    return response;
}

messaging::delete_lifecycle_event_response lifecycle_event_service::delete_lifecycle_event(
    const messaging::delete_lifecycle_event_request& request) {
    messaging::delete_lifecycle_event_response response;
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
        case repository::lifecycle_event_repository::remove_status::removed:
            break;
        case repository::lifecycle_event_repository::remove_status::missing:
            response.result = refuse(outcome_code::not_found, {.entity = "lifecycle_event"});
            break;
        case repository::lifecycle_event_repository::remove_status::conflicting: {
            // A conflicting removal states the version it expected but not the one
            // the row now holds, and the sentence wants both. The row is read only
            // on the refusal path.
            const auto live = read_one(repo_, ctx_, request.removal.key);
            response.result = refuse(
                outcome_code::version_conflict,
                {.entity = "lifecycle_event",
                 .field = "code",
                 .expected = expected ? std::to_string(*expected) : std::string{},
                 .current = live.empty() ? std::string{} : std::to_string(live.front().version)});
            break;
        }
        case repository::lifecycle_event_repository::remove_status::unsupported:
            response.result = refuse(outcome_code::precondition_not_supported);
            break;
    }
    return response;
}

messaging::delete_many_lifecycle_events_response
lifecycle_event_service::delete_many_lifecycle_events(
    const messaging::delete_many_lifecycle_events_request& request) {
    messaging::delete_many_lifecycle_events_response response;
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

messaging::list_lifecycle_event_versions_response
lifecycle_event_service::list_lifecycle_event_versions(
    const messaging::list_lifecycle_event_versions_request& request) {
    messaging::list_lifecycle_event_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "lifecycle events", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result =
            refuse(outcome_code::filter_not_supported, {.entity = "lifecycle events"});
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

messaging::get_lifecycle_event_version_response
lifecycle_event_service::get_lifecycle_event_version(
    const messaging::get_lifecycle_event_version_request& request) {
    messaging::get_lifecycle_event_version_response response;
    auto found = repo_.read_at_version(ctx_, request.key.lifecycle_event.code, request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "lifecycle_event"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}

ores::utility::domain::result
lifecycle_event_service::prepare_change(const messaging::lifecycle_event_change& change,
                                        const ores::utility::domain::change_intent& intent,
                                        domain::lifecycle_event& out) {
    using ores::utility::domain::outcome;
    using ores::utility::domain::precondition_kind;
    ores::utility::domain::result result;
    out = to_domain(change.write);
    const auto current = read_one(repo_, ctx_, key_from(out));
    switch (change.precondition.kind) {
        case precondition_kind::must_not_exist:
            if (!current.empty())
                return refuse(outcome_code::already_exists,
                              {.entity = "lifecycle_event", .field = "code"});
            break;
        case precondition_kind::must_match_version:
            if (current.empty())
                return refuse(outcome_code::not_found, {.entity = "lifecycle_event"});
            // The protocol states the version as a uint32 and the row carries it
            // as an int, so the comparison states the conversion.
            if (!change.precondition.version ||
                static_cast<std::uint32_t>(current.front().version) !=
                    *change.precondition.version) {
                return refuse(outcome_code::version_conflict,
                              {.entity = "lifecycle_event",
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


std::vector<domain::lifecycle_event> lifecycle_event_service::list_events(std::uint32_t offset,
                                                                          std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all lifecycle events";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t lifecycle_event_service::count_events() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total lifecycle events count";
    return repo_.get_total_event_count(ctx_);
}


std::optional<domain::lifecycle_event>
lifecycle_event_service::get_event_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting lifecycle event at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::lifecycle_event> lifecycle_event_service::get_event(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting lifecycle event. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::lifecycle_event>
lifecycle_event_service::get_events(const std::vector<std::string>& codes) {
    return repo_.read_latest(ctx_, codes);
}

void lifecycle_event_service::save_event(const domain::lifecycle_event& v) {
    if (v.code.empty())
        throw std::invalid_argument("Lifecycle Event code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving lifecycle event. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved lifecycle event. " << "code: " << v.code;
}

void lifecycle_event_service::save_events(const std::vector<domain::lifecycle_event>& events) {
    for (const auto& e : events) {
        if (e.code.empty())
            throw std::invalid_argument("Lifecycle Event code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << events.size() << " lifecycle events";
    auto ts = events;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void lifecycle_event_service::delete_event(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing lifecycle event. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed lifecycle event. " << "code: " << code;
}

void lifecycle_event_service::delete_events(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::lifecycle_event>
lifecycle_event_service::get_event_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for lifecycle event. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
