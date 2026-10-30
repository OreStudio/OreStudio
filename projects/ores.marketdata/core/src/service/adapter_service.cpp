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
#include "ores.marketdata.core/service/adapter_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/adapter.hpp"
#include "ores.marketdata.api/messaging/adapter_protocol.hpp"
#include "ores.marketdata.core/repository/adapter_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
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

namespace ores::marketdata::service {

using namespace ores::logging;

adapter_service::adapter_service(context ctx)
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
std::vector<domain::adapter> adapter_read_one(repository::adapter_repository& repo,
                                              const ores::database::context& ctx,
                                              const messaging::adapter_key& key) {
    return repo.read_latest(ctx, key.code);
}

}

messaging::list_adapters_response
adapter_service::list_adapters(const messaging::list_adapters_request& request) {
    messaging::list_adapters_response response;
    if (!request.order.field.empty() &&
        !repository::adapter_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "adapters", .field = request.order.field});
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
    response.adapters = repo_.read_latest(
        ctx_, request.offset, request.limit, request.order, request.filter, as_of);
    response.total = repo_.get_total_adapter_count(ctx_, request.filter, as_of);
    return response;
}

messaging::get_adapter_response
adapter_service::get_adapter(const messaging::get_adapter_request& request) {
    messaging::get_adapter_response response;
    auto found = adapter_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "adapter"});
        return response;
    }
    response.adapter = std::move(found.front());
    return response;
}

messaging::get_many_adapters_response
adapter_service::get_many_adapters(const messaging::get_many_adapters_request& request) {
    messaging::get_many_adapters_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::adapter_lookup entry;
        entry.key = k;
        auto found = adapter_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.adapter = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

messaging::list_adapter_versions_response
adapter_service::list_adapter_versions(const messaging::list_adapter_versions_request& request) {
    messaging::list_adapter_versions_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "adapters", .field = request.order.field});
        return response;
    }
    if (request.filter) {
        response.result = refuse(outcome_code::filter_not_supported, {.entity = "adapters"});
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

messaging::get_adapter_version_response
adapter_service::get_adapter_version(const messaging::get_adapter_version_request& request) {
    messaging::get_adapter_version_response response;
    auto found = repo_.read_at_version(ctx_, request.key.adapter.code, request.key.version);
    if (!found) {
        response.result = refuse(outcome_code::not_found, {.entity = "adapter"});
        return response;
    }
    response.version = std::move(*found);
    return response;
}


std::vector<domain::adapter> adapter_service::list_adapters(std::uint32_t offset,
                                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all adapters";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t adapter_service::count_adapters() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total adapters count";
    return repo_.get_total_adapter_count(ctx_);
}


std::optional<domain::adapter> adapter_service::get_adapter_at_version(const std::string& code,
                                                                       std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting adapter at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::adapter> adapter_service::get_adapter(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting adapter. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::adapter> adapter_service::get_adapters(const std::vector<std::string>& codes) {
    return repo_.read_latest(ctx_, codes);
}

void adapter_service::save_adapter(const domain::adapter& v) {
    if (v.code.empty())
        throw std::invalid_argument("Adapter code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving adapter. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved adapter. " << "code: " << v.code;
}

void adapter_service::save_adapters(const std::vector<domain::adapter>& adapters) {
    for (const auto& e : adapters) {
        if (e.code.empty())
            throw std::invalid_argument("Adapter code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << adapters.size() << " adapters";
    auto ts = adapters;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void adapter_service::delete_adapter(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing adapter. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed adapter. " << "code: " << code;
}

void adapter_service::delete_adapters(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::adapter> adapter_service::get_adapter_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for adapter. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
