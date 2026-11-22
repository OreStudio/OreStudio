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
#include "ores.orgmode.core/service/knowledge_page_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.orgmode.api/domain/knowledge_page.hpp"
#include "ores.orgmode.api/messaging/knowledge_page_protocol.hpp"
#include "ores.orgmode.core/repository/knowledge_page_repository.hpp"
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

namespace ores::orgmode::service {

using namespace ores::logging;

knowledge_page_service::knowledge_page_service(const context& ctx)
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
std::vector<domain::knowledge_page>
knowledge_page_read_one(repository::knowledge_page_repository& repo,
                        const ores::database::context& ctx,
                        const messaging::knowledge_page_key& key) {
    return repo.read_latest(ctx, key.id);
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
std::vector<std::vector<domain::knowledge_page>>
knowledge_page_read_many(repository::knowledge_page_repository& repo,
                         const ores::database::context& ctx,
                         const std::vector<messaging::knowledge_page_key>& keys) {
    std::vector<std::vector<domain::knowledge_page>> found(keys.size());
    using key_tuple = std::tuple<std::string>;
    std::vector<std::string> id_keys;
    id_keys.reserve(keys.size());
    for (const auto& key : keys) {
        id_keys.push_back(key.id);
    }
    std::map<key_tuple, std::vector<domain::knowledge_page>> by_key;
    for (auto& row : repo.read_latest(ctx, id_keys)) {
        key_tuple named(row.id);
        by_key[std::move(named)].push_back(std::move(row));
    }
    for (std::size_t i = 0; i < keys.size(); ++i) {
        const auto& key = keys[i];
        const auto rows = by_key.find(key_tuple(key.id));
        if (rows != by_key.end())
            found[i] = rows->second;
    }
    return found;
}

}

messaging::list_knowledge_pages_response knowledge_page_service::list_knowledge_pages(
    const messaging::list_knowledge_pages_request& request) {
    messaging::list_knowledge_pages_response response;
    if (!request.order.field.empty() &&
        !repository::knowledge_page_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "knowledge pages", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of &&
        request.filter->id_one_of->size() > max_batch_size) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "id_one_of", .limit = std::to_string(max_batch_size)});
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
    response.pages = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_page_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_knowledge_page_response
knowledge_page_service::get_knowledge_page(const messaging::get_knowledge_page_request& request) {
    messaging::get_knowledge_page_response response;
    auto found = knowledge_page_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "knowledge_page"});
        return response;
    }
    response.knowledge_page = std::move(found.front());
    return response;
}

messaging::get_many_knowledge_pages_response knowledge_page_service::get_many_knowledge_pages(
    const messaging::get_many_knowledge_pages_request& request) {
    messaging::get_many_knowledge_pages_response response;
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
    auto found = knowledge_page_read_many(repo_, ctx_, request.keys);
    for (std::size_t i = 0; i < request.keys.size(); ++i) {
        messaging::knowledge_page_lookup entry;
        entry.key = request.keys[i];
        if (!found[i].empty())
            entry.knowledge_page = std::move(found[i].front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::list_knowledge_page_versions_response
knowledge_page_service::list_knowledge_page_versions(
    const messaging::list_knowledge_page_versions_request& request) {
    messaging::list_knowledge_page_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "knowledge pages", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result = refuse(outcome_code::filter_not_supported, {.entity = "knowledge pages"});
        return response;
    }
    auto all = repo_.read_all(ctx_, request.key.id);
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

messaging::get_knowledge_page_version_response knowledge_page_service::get_knowledge_page_version(
    const messaging::get_knowledge_page_version_request& request) {
    messaging::get_knowledge_page_version_response response;
    auto found = repo_.read_at_version(ctx_, request.key.knowledge_page.id, request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "knowledge_page"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}


std::vector<domain::knowledge_page> knowledge_page_service::list_pages(std::uint32_t offset,
                                                                       std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Listing all knowledge pages";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t knowledge_page_service::count_pages() {
    ORES_LOG_SEV(lg(), debug) << "Getting total knowledge pages count";
    return repo_.get_total_page_count(ctx_);
}


std::optional<domain::knowledge_page>
knowledge_page_service::get_page_at_version(const std::string& id, std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Getting knowledge page at version. " << "id: " << id
                              << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::knowledge_page> knowledge_page_service::get_page(const std::string& id) {
    ORES_LOG_SEV(lg(), debug) << "Getting knowledge page. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::knowledge_page>
knowledge_page_service::get_pages(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void knowledge_page_service::save_page(const domain::knowledge_page& v) {
    if (v.id.empty())
        throw std::invalid_argument("Knowledge Page id cannot be empty.");
    ORES_LOG_SEV(lg(), debug) << "Saving knowledge page. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    ORES_LOG_SEV(lg(), info) << "Saved knowledge page. " << "id: " << v.id;
}

void knowledge_page_service::save_pages(const std::vector<domain::knowledge_page>& pages) {
    for (const auto& e : pages) {
        if (e.id.empty())
            throw std::invalid_argument("Knowledge Page id cannot be empty.");
    }
    ORES_LOG_SEV(lg(), debug) << "Saving " << pages.size() << " knowledge pages";
    auto ts = pages;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void knowledge_page_service::delete_page(const std::string& id) {
    ORES_LOG_SEV(lg(), debug) << "Removing knowledge page. " << "id: " << id;
    repo_.remove(ctx_, id);
    ORES_LOG_SEV(lg(), info) << "Removed knowledge page. " << "id: " << id;
}

void knowledge_page_service::delete_pages(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::knowledge_page>
knowledge_page_service::get_page_history(const std::string& id) {
    ORES_LOG_SEV(lg(), debug) << "Getting history for knowledge page. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
