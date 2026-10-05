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
#include "ores.trading.core/service/entry_channel_type_service.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <algorithm>
#include <cstdint>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

entry_channel_type_service::entry_channel_type_service(context ctx)
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
std::vector<domain::entry_channel_type> read_one(repository::entry_channel_type_repository& repo,
                                                 const ores::database::context& ctx,
                                                 const messaging::entry_channel_type_key& key) {
    return repo.read_latest(ctx, key.code);
}

}

messaging::list_entry_channel_types_response entry_channel_type_service::list_entry_channel_types(
    const messaging::list_entry_channel_types_request& request) {
    messaging::list_entry_channel_types_response response;
    if (!request.order.field.empty() &&
        !repository::entry_channel_type_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of entry channel types cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->code_one_of &&
        request.filter->code_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in code_one_of.";
        return response;
    }
    response.entry_channel_types =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_entry_channel_type_count(ctx_, request.filter);
    return response;
}

messaging::get_entry_channel_type_response entry_channel_type_service::get_entry_channel_type(
    const messaging::get_entry_channel_type_request& request) {
    messaging::get_entry_channel_type_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.entry_channel_type = std::move(found.front());
    return response;
}

messaging::get_many_entry_channel_types_response
entry_channel_type_service::get_many_entry_channel_types(
    const messaging::get_many_entry_channel_types_request& request) {
    messaging::get_many_entry_channel_types_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::entry_channel_type_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.entry_channel_type = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::entry_channel_type>
entry_channel_type_service::list_entry_channel_types(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all entry channel types";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t entry_channel_type_service::count_entry_channel_types() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total entry channel types count";
    return repo_.get_total_entry_channel_type_count(ctx_);
}


std::optional<domain::entry_channel_type>
entry_channel_type_service::get_entry_channel_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting entry channel type. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::entry_channel_type>
entry_channel_type_service::get_entry_channel_types(const std::vector<std::string>& codes) {
    return repo_.read_latest(ctx_, codes);
}

void entry_channel_type_service::save_entry_channel_type(const domain::entry_channel_type& v) {
    if (v.code.empty())
        throw std::invalid_argument("Entry Channel Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving entry channel type. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved entry channel type. " << "code: " << v.code;
}

void entry_channel_type_service::save_entry_channel_types(
    const std::vector<domain::entry_channel_type>& entry_channel_types) {
    for (const auto& e : entry_channel_types) {
        if (e.code.empty())
            throw std::invalid_argument("Entry Channel Type code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << entry_channel_types.size() << " entry channel types";
    auto ts = entry_channel_types;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void entry_channel_type_service::delete_entry_channel_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing entry channel type. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed entry channel type. " << "code: " << code;
}

void entry_channel_type_service::delete_entry_channel_types(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}


}
