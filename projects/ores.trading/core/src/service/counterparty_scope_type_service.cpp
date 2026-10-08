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
#include "ores.trading.core/service/counterparty_scope_type_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.trading.api/domain/counterparty_scope_type.hpp"
#include "ores.trading.api/messaging/counterparty_scope_type_protocol.hpp"
#include "ores.trading.core/repository/counterparty_scope_type_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <cstdint>
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

counterparty_scope_type_service::counterparty_scope_type_service(context ctx)
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
std::vector<domain::counterparty_scope_type>
read_one(repository::counterparty_scope_type_repository& repo,
         const ores::database::context& ctx,
         const messaging::counterparty_scope_type_key& key) {
    return repo.read_latest(ctx, key.code);
}

}

messaging::list_counterparty_scope_types_response
counterparty_scope_type_service::list_counterparty_scope_types(
    const messaging::list_counterparty_scope_types_request& request) {
    messaging::list_counterparty_scope_types_response response;
    if (!request.order.field.empty() &&
        !repository::counterparty_scope_type_repository::is_sortable(request.order.field)) {
        response.result =
            refuse(outcome_code::order_not_supported,
                   {.entity = "counterparty scope types", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->code_one_of &&
        request.filter->code_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "code_one_of", .limit = "1000"});
        return response;
    }
    response.counterparty_scope_types =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_counterparty_scope_type_count(ctx_, request.filter);
    return response;
}

messaging::get_counterparty_scope_type_response
counterparty_scope_type_service::get_counterparty_scope_type(
    const messaging::get_counterparty_scope_type_request& request) {
    messaging::get_counterparty_scope_type_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "counterparty_scope_type"});
        return response;
    }
    response.counterparty_scope_type = std::move(found.front());
    return response;
}

messaging::get_many_counterparty_scope_types_response
counterparty_scope_type_service::get_many_counterparty_scope_types(
    const messaging::get_many_counterparty_scope_types_request& request) {
    messaging::get_many_counterparty_scope_types_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::counterparty_scope_type_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.counterparty_scope_type = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::counterparty_scope_type>
counterparty_scope_type_service::list_counterparty_scope_types(std::uint32_t offset,
                                                               std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all counterparty scope types";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t counterparty_scope_type_service::count_counterparty_scope_types() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total counterparty scope types count";
    return repo_.get_total_counterparty_scope_type_count(ctx_);
}


std::optional<domain::counterparty_scope_type>
counterparty_scope_type_service::get_counterparty_scope_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting counterparty scope type. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::counterparty_scope_type>
counterparty_scope_type_service::get_counterparty_scope_types(
    const std::vector<std::string>& codes) {
    return repo_.read_latest(ctx_, codes);
}

void counterparty_scope_type_service::save_counterparty_scope_type(
    const domain::counterparty_scope_type& v) {
    if (v.code.empty())
        throw std::invalid_argument("Counterparty Scope Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving counterparty scope type. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved counterparty scope type. " << "code: " << v.code;
}

void counterparty_scope_type_service::save_counterparty_scope_types(
    const std::vector<domain::counterparty_scope_type>& counterparty_scope_types) {
    for (const auto& e : counterparty_scope_types) {
        if (e.code.empty())
            throw std::invalid_argument("Counterparty Scope Type code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << counterparty_scope_types.size()
                               << " counterparty scope types";
    auto ts = counterparty_scope_types;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void counterparty_scope_type_service::delete_counterparty_scope_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty scope type. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed counterparty scope type. " << "code: " << code;
}

void counterparty_scope_type_service::delete_counterparty_scope_types(
    const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}


}
