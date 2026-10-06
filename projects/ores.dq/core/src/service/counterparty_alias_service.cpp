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
#include "ores.dq.core/service/counterparty_alias_service.hpp"
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

namespace ores::dq::service {

using namespace ores::logging;

counterparty_alias_service::counterparty_alias_service(context ctx)
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
std::vector<domain::counterparty_alias> read_one(repository::counterparty_alias_repository& repo,
                                                 const ores::database::context& ctx,
                                                 const messaging::counterparty_alias_key& key) {
    return repo.read_latest(ctx, key.id_value);
}

}

messaging::list_counterparty_aliases_response counterparty_alias_service::list_counterparty_aliases(
    const messaging::list_counterparty_aliases_request& request) {
    messaging::list_counterparty_aliases_response response;
    if (!request.order.field.empty() &&
        !repository::counterparty_alias_repository::is_sortable(request.order.field)) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "A list of counterparty aliases cannot be ordered by " + request.order.field + ".";
        return response;
    }
    if (request.filter && request.filter->id_value_one_of &&
        request.filter->id_value_one_of->size() > 1000) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_too_large";
        response.result.message = "The filter lists more than 1000 values in id_value_one_of.";
        return response;
    }
    response.counterparty_aliases =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_counterparty_alias_count(ctx_, request.filter);
    return response;
}

messaging::get_counterparty_alias_response counterparty_alias_service::get_counterparty_alias(
    const messaging::get_counterparty_alias_request& request) {
    messaging::get_counterparty_alias_response response;
    auto found = read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.counterparty_alias = std::move(found.front());
    return response;
}

messaging::get_many_counterparty_aliases_response
counterparty_alias_service::get_many_counterparty_aliases(
    const messaging::get_many_counterparty_aliases_request& request) {
    messaging::get_many_counterparty_aliases_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::counterparty_alias_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.counterparty_alias = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::counterparty_alias>
counterparty_alias_service::list_counterparty_aliases(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all counterparty aliases";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t counterparty_alias_service::count_counterparty_aliases() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total counterparty aliases count";
    return repo_.get_total_counterparty_alias_count(ctx_);
}


std::optional<domain::counterparty_alias>
counterparty_alias_service::get_counterparty_alias(const std::string& id_value) {
    BOOST_LOG_SEV(lg(), debug) << "Getting counterparty alias. " << "id_value: " << id_value;
    auto results = repo_.read_latest(ctx_, id_value);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::counterparty_alias>
counterparty_alias_service::get_counterparty_aliases(const std::vector<std::string>& id_values) {
    return repo_.read_latest(ctx_, id_values);
}

void counterparty_alias_service::save_counterparty_alias(const domain::counterparty_alias& v) {
    if (v.id_value.empty())
        throw std::invalid_argument("Counterparty Alias id_value cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving counterparty alias. " << "id_value: " << v.id_value;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved counterparty alias. " << "id_value: " << v.id_value;
}

void counterparty_alias_service::save_counterparty_aliases(
    const std::vector<domain::counterparty_alias>& counterparty_aliases) {
    for (const auto& e : counterparty_aliases) {
        if (e.id_value.empty())
            throw std::invalid_argument("Counterparty Alias id_value cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << counterparty_aliases.size()
                               << " counterparty aliases";
    auto ts = counterparty_aliases;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void counterparty_alias_service::delete_counterparty_alias(const std::string& id_value) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty alias. " << "id_value: " << id_value;
    repo_.remove(ctx_, id_value);
    BOOST_LOG_SEV(lg(), info) << "Removed counterparty alias. " << "id_value: " << id_value;
}

void counterparty_alias_service::delete_counterparty_aliases(
    const std::vector<std::string>& id_values) {
    repo_.remove(ctx_, id_values);
}


}
