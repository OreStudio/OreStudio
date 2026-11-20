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
#include "ores.marketdata.core/service/supplier_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.marketdata.api/domain/supplier.hpp"
#include "ores.marketdata.api/messaging/supplier_protocol.hpp"
#include "ores.marketdata.core/repository/supplier_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
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

namespace ores::marketdata::service {

using namespace ores::logging;

supplier_service::supplier_service(const context& ctx)
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
std::vector<domain::supplier> supplier_read_one(repository::supplier_repository& repo,
                                                const ores::database::context& ctx,
                                                const messaging::supplier_key& key) {
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
std::vector<std::vector<domain::supplier>>
supplier_read_many(repository::supplier_repository& repo,
                   const ores::database::context& ctx,
                   const std::vector<messaging::supplier_key>& keys) {
    std::vector<std::vector<domain::supplier>> found(keys.size());
    using key_tuple = std::tuple<std::string>;
    std::vector<std::string> code_keys;
    code_keys.reserve(keys.size());
    for (const auto& key : keys) {
        code_keys.push_back(key.code);
    }
    std::map<key_tuple, std::vector<domain::supplier>> by_key;
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

messaging::list_suppliers_response
supplier_service::list_suppliers(const messaging::list_suppliers_request& request) {
    messaging::list_suppliers_response response;
    if (!request.order.field.empty() &&
        !repository::supplier_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "suppliers", .field = request.order.field});
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
    response.suppliers = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_supplier_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_supplier_response
supplier_service::get_supplier(const messaging::get_supplier_request& request) {
    messaging::get_supplier_response response;
    auto found = supplier_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "supplier"});
        return response;
    }
    response.supplier = std::move(found.front());
    return response;
}

messaging::get_many_suppliers_response
supplier_service::get_many_suppliers(const messaging::get_many_suppliers_request& request) {
    messaging::get_many_suppliers_response response;
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
    auto found = supplier_read_many(repo_, ctx_, request.keys);
    for (std::size_t i = 0; i < request.keys.size(); ++i) {
        messaging::supplier_lookup entry;
        entry.key = request.keys[i];
        if (!found[i].empty())
            entry.supplier = std::move(found[i].front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::list_supplier_versions_response
supplier_service::list_supplier_versions(const messaging::list_supplier_versions_request& request) {
    messaging::list_supplier_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "suppliers", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result = refuse(outcome_code::filter_not_supported, {.entity = "suppliers"});
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

messaging::get_supplier_version_response
supplier_service::get_supplier_version(const messaging::get_supplier_version_request& request) {
    messaging::get_supplier_version_response response;
    auto found = repo_.read_at_version(ctx_, request.key.supplier.code, request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "supplier"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}


std::vector<domain::supplier> supplier_service::list_suppliers(std::uint32_t offset,
                                                               std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Listing all suppliers";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t supplier_service::count_suppliers() {
    ORES_LOG_SEV(lg(), debug) << "Getting total suppliers count";
    return repo_.get_total_supplier_count(ctx_);
}


std::optional<domain::supplier> supplier_service::get_supplier_at_version(const std::string& code,
                                                                          std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Getting supplier at version. " << "code: " << code
                              << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::supplier> supplier_service::get_supplier(const std::string& code) {
    ORES_LOG_SEV(lg(), debug) << "Getting supplier. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::supplier>
supplier_service::get_suppliers(const std::vector<std::string>& codes) {
    return repo_.read_latest(ctx_, codes);
}

void supplier_service::save_supplier(const domain::supplier& v) {
    if (v.code.empty())
        throw std::invalid_argument("Supplier code cannot be empty.");
    ORES_LOG_SEV(lg(), debug) << "Saving supplier. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    ORES_LOG_SEV(lg(), info) << "Saved supplier. " << "code: " << v.code;
}

void supplier_service::save_suppliers(const std::vector<domain::supplier>& suppliers) {
    for (const auto& e : suppliers) {
        if (e.code.empty())
            throw std::invalid_argument("Supplier code cannot be empty.");
    }
    ORES_LOG_SEV(lg(), debug) << "Saving " << suppliers.size() << " suppliers";
    auto ts = suppliers;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void supplier_service::delete_supplier(const std::string& code) {
    ORES_LOG_SEV(lg(), debug) << "Removing supplier. " << "code: " << code;
    repo_.remove(ctx_, code);
    ORES_LOG_SEV(lg(), info) << "Removed supplier. " << "code: " << code;
}

void supplier_service::delete_suppliers(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::supplier> supplier_service::get_supplier_history(const std::string& code) {
    ORES_LOG_SEV(lg(), debug) << "Getting history for supplier. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
