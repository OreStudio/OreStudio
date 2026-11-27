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
#include "ores.reporting.core/service/report_error_event_service.hpp"
#include "ores.reporting.api/domain/report_error_event.hpp"
#include "ores.reporting.api/messaging/report_error_event_protocol.hpp"
#include "ores.reporting.core/repository/report_error_event_repository.hpp"
#include <boost/uuid/uuid.hpp>
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

namespace ores::reporting::service {

using namespace ores::logging;

report_error_event_service::report_error_event_service(const context& ctx)
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
std::vector<domain::report_error_event>
report_error_event_read_one(repository::report_error_event_repository& repo,
                            const ores::database::context& ctx,
                            const messaging::report_error_event_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
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
std::vector<std::vector<domain::report_error_event>>
report_error_event_read_many(repository::report_error_event_repository& repo,
                             const ores::database::context& ctx,
                             const std::vector<messaging::report_error_event_key>& keys) {
    std::vector<std::vector<domain::report_error_event>> found(keys.size());
    using key_tuple = std::tuple<std::string>;
    std::vector<std::string> id_keys;
    id_keys.reserve(keys.size());
    for (const auto& key : keys) {
        id_keys.push_back(boost::uuids::to_string(key.id));
    }
    std::map<key_tuple, std::vector<domain::report_error_event>> by_key;
    for (auto& row : repo.read_latest(ctx, id_keys)) {
        key_tuple named(boost::uuids::to_string(row.id));
        by_key[std::move(named)].push_back(std::move(row));
    }
    for (std::size_t i = 0; i < keys.size(); ++i) {
        const auto& key = keys[i];
        const auto rows = by_key.find(key_tuple(boost::uuids::to_string(key.id)));
        if (rows != by_key.end())
            found[i] = rows->second;
    }
    return found;
}

}

messaging::list_report_error_events_response report_error_event_service::list_report_error_events(
    const messaging::list_report_error_events_request& request) {
    messaging::list_report_error_events_response response;
    if (!request.order.field.empty() &&
        !repository::report_error_event_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "report error events", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of &&
        request.filter->id_one_of->size() > max_batch_size) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "id_one_of", .limit = std::to_string(max_batch_size)});
        return response;
    }
    if (request.filter && request.filter->report_instance_id_one_of &&
        request.filter->report_instance_id_one_of->size() > max_batch_size) {
        response.result =
            refuse(outcome_code::filter_too_large,
                   {.field = "report_instance_id_one_of", .limit = std::to_string(max_batch_size)});
        return response;
    }
    if (request.filter && request.filter->severity_one_of &&
        request.filter->severity_one_of->size() > max_batch_size) {
        response.result =
            refuse(outcome_code::filter_too_large,
                   {.field = "severity_one_of", .limit = std::to_string(max_batch_size)});
        return response;
    }
    response.errors =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_error_count(ctx_, request.filter);
    return response;
}

messaging::list_by_report_instance_id_report_error_events_response
report_error_event_service::list_by_report_instance_id_report_error_events(
    const messaging::list_by_report_instance_id_report_error_events_request& request) {
    messaging::list_by_report_instance_id_report_error_events_response response;
    if (!request.order.field.empty() &&
        !repository::report_error_event_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "report error events", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of &&
        request.filter->id_one_of->size() > max_batch_size) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "id_one_of", .limit = std::to_string(max_batch_size)});
        return response;
    }
    if (request.filter && request.filter->report_instance_id_one_of &&
        request.filter->report_instance_id_one_of->size() > max_batch_size) {
        response.result =
            refuse(outcome_code::filter_too_large,
                   {.field = "report_instance_id_one_of", .limit = std::to_string(max_batch_size)});
        return response;
    }
    if (request.filter && request.filter->severity_one_of &&
        request.filter->severity_one_of->size() > max_batch_size) {
        response.result =
            refuse(outcome_code::filter_too_large,
                   {.field = "severity_one_of", .limit = std::to_string(max_batch_size)});
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result =
            refuse(outcome_code::scope_not_supported, {.entity = "report error events"});
        return response;
    }
    const auto relation = boost::uuids::to_string(request.report_instance_id);
    response.errors = repo_.read_latest_by_report_instance_id(
        ctx_, relation, request.offset, request.limit, request.order, request.filter);
    response.total =
        repo_.get_total_error_count_by_report_instance_id(ctx_, relation, request.filter);
    return response;
}

messaging::get_report_error_event_response report_error_event_service::get_report_error_event(
    const messaging::get_report_error_event_request& request) {
    messaging::get_report_error_event_response response;
    auto found = report_error_event_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "report_error_event"});
        return response;
    }
    response.report_error_event = std::move(found.front());
    return response;
}

messaging::get_many_report_error_events_response
report_error_event_service::get_many_report_error_events(
    const messaging::get_many_report_error_events_request& request) {
    messaging::get_many_report_error_events_response response;
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
    auto found = report_error_event_read_many(repo_, ctx_, request.keys);
    for (std::size_t i = 0; i < request.keys.size(); ++i) {
        messaging::report_error_event_lookup entry;
        entry.key = request.keys[i];
        if (!found[i].empty())
            entry.report_error_event = std::move(found[i].front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::report_error_event>
report_error_event_service::list_errors(std::uint32_t offset, std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Listing all report error events";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t report_error_event_service::count_errors() {
    ORES_LOG_SEV(lg(), debug) << "Getting total report error events count";
    return repo_.get_total_error_count(ctx_);
}


std::vector<domain::report_error_event>
report_error_event_service::list_errors_by_report_instance_id(const std::string& report_instance_id,
                                                              std::uint32_t offset,
                                                              std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Listing report error events by report_instance_id: "
                              << report_instance_id;
    return repo_.read_latest_by_report_instance_id(ctx_, report_instance_id, offset, limit);
}

std::uint32_t report_error_event_service::count_errors_by_report_instance_id(
    const std::string& report_instance_id) {
    ORES_LOG_SEV(lg(), debug) << "Getting total report error events count by report_instance_id: "
                              << report_instance_id;
    return repo_.get_total_error_count_by_report_instance_id(ctx_, report_instance_id);
}


std::optional<domain::report_error_event>
report_error_event_service::get_error(const boost::uuids::uuid& id) {
    ORES_LOG_SEV(lg(), debug) << "Getting report error event. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::report_error_event>
report_error_event_service::get_errors(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void report_error_event_service::save_error(const domain::report_error_event& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Report Error Event id cannot be empty.");
    ORES_LOG_SEV(lg(), debug) << "Saving report error event. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    ORES_LOG_SEV(lg(), info) << "Saved report error event. " << "id: " << v.id;
}

void report_error_event_service::save_errors(
    const std::vector<domain::report_error_event>& errors) {
    for (const auto& e : errors) {
        if (e.id.is_nil())
            throw std::invalid_argument("Report Error Event id cannot be empty.");
    }
    ORES_LOG_SEV(lg(), debug) << "Saving " << errors.size() << " report error events";
    auto ts = errors;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void report_error_event_service::delete_error(const boost::uuids::uuid& id) {
    ORES_LOG_SEV(lg(), debug) << "Removing report error event. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    ORES_LOG_SEV(lg(), info) << "Removed report error event. " << "id: " << id;
}

void report_error_event_service::delete_errors(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}


messaging::get_report_error_counts_response report_error_event_service::get_report_error_counts(
    const messaging::get_report_error_counts_request& request) {
    messaging::get_report_error_counts_response response;
    // The cap is the same one the generated filters state, so one caller cannot
    // ask the store for more runs than the list reads allow.
    if (request.report_instance_ids.size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "report_instance_ids", .limit = "1000"});
        return response;
    }
    response.counts = repo_.read_counts_by_report_instance_ids(ctx_, request.report_instance_ids);
    return response;
}
}
