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
#include "ores.inbox.core/service/delivery_outcome_type_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.inbox.api/domain/delivery_outcome_type.hpp"
#include "ores.inbox.api/messaging/delivery_outcome_type_protocol.hpp"
#include "ores.inbox.core/repository/delivery_outcome_type_repository.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <cstddef>
#include <cstdint>
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

namespace ores::inbox::service {

using namespace ores::logging;

delivery_outcome_type_service::delivery_outcome_type_service(const context& ctx)
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
std::vector<domain::delivery_outcome_type>
delivery_outcome_type_read_one(repository::delivery_outcome_type_repository& repo,
                               const ores::database::context& ctx,
                               const messaging::delivery_outcome_type_key& key) {
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
std::vector<std::vector<domain::delivery_outcome_type>>
delivery_outcome_type_read_many(repository::delivery_outcome_type_repository& repo,
                                const ores::database::context& ctx,
                                const std::vector<messaging::delivery_outcome_type_key>& keys) {
    std::vector<std::vector<domain::delivery_outcome_type>> found(keys.size());
    using key_tuple = std::tuple<std::string>;
    std::vector<std::string> code_keys;
    code_keys.reserve(keys.size());
    for (const auto& key : keys) {
        code_keys.push_back(key.code);
    }
    std::map<key_tuple, std::vector<domain::delivery_outcome_type>> by_key;
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

}

messaging::list_delivery_outcome_types_response
delivery_outcome_type_service::list_delivery_outcome_types(
    const messaging::list_delivery_outcome_types_request& request) {
    messaging::list_delivery_outcome_types_response response;
    if (!request.order.field.empty() &&
        !repository::delivery_outcome_type_repository::is_sortable(request.order.field)) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "delivery outcome types", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->code_one_of &&
        request.filter->code_one_of->size() > max_batch_size) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "code_one_of", .limit = std::to_string(max_batch_size)});
        return response;
    }
    response.delivery_outcome_types =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_delivery_outcome_type_count(ctx_, request.filter);
    return response;
}

messaging::get_delivery_outcome_type_response
delivery_outcome_type_service::get_delivery_outcome_type(
    const messaging::get_delivery_outcome_type_request& request) {
    messaging::get_delivery_outcome_type_response response;
    auto found = delivery_outcome_type_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "delivery_outcome_type"});
        return response;
    }
    response.delivery_outcome_type = std::move(found.front());
    return response;
}

messaging::get_many_delivery_outcome_types_response
delivery_outcome_type_service::get_many_delivery_outcome_types(
    const messaging::get_many_delivery_outcome_types_request& request) {
    messaging::get_many_delivery_outcome_types_response response;
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
    auto found = delivery_outcome_type_read_many(repo_, ctx_, request.keys);
    for (std::size_t i = 0; i < request.keys.size(); ++i) {
        messaging::delivery_outcome_type_lookup entry;
        entry.key = request.keys[i];
        if (!found[i].empty())
            entry.delivery_outcome_type = std::move(found[i].front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::delivery_outcome_type>
delivery_outcome_type_service::list_delivery_outcome_types(std::uint32_t offset,
                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all delivery outcome types";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t delivery_outcome_type_service::count_delivery_outcome_types() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total delivery outcome types count";
    return repo_.get_total_delivery_outcome_type_count(ctx_);
}


std::optional<domain::delivery_outcome_type>
delivery_outcome_type_service::get_delivery_outcome_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting delivery outcome type. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::delivery_outcome_type>
delivery_outcome_type_service::get_delivery_outcome_types(const std::vector<std::string>& codes) {
    return repo_.read_latest(ctx_, codes);
}

void delivery_outcome_type_service::save_delivery_outcome_type(
    const domain::delivery_outcome_type& v) {
    if (v.code.empty())
        throw std::invalid_argument("Delivery Outcome Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving delivery outcome type. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved delivery outcome type. " << "code: " << v.code;
}

void delivery_outcome_type_service::save_delivery_outcome_types(
    const std::vector<domain::delivery_outcome_type>& delivery_outcome_types) {
    for (const auto& e : delivery_outcome_types) {
        if (e.code.empty())
            throw std::invalid_argument("Delivery Outcome Type code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << delivery_outcome_types.size()
                               << " delivery outcome types";
    auto ts = delivery_outcome_types;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void delivery_outcome_type_service::delete_delivery_outcome_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing delivery outcome type. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed delivery outcome type. " << "code: " << code;
}

void delivery_outcome_type_service::delete_delivery_outcome_types(
    const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}


}
