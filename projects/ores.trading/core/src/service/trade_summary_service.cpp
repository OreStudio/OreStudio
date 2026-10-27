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
#include "ores.trading.core/service/trade_summary_service.hpp"
#include "ores.trading.api/domain/trade_summary.hpp"
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.trading.core/repository/trade_summary_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>
// Log lines stream uuids with uuid_io's operator<<, which the include check
// does not count as a use.
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.database/repository/range_bound.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp> // IWYU pragma: keep.

using ores::service::messaging::stamp;
// Every refusal names its outcome; the catalogue supplies the code and the
// sentence, so a service states what happened and nothing about how to say it.
using ores::database::domain::outcome_args;
using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

namespace ores::trading::service {

using namespace ores::logging;

trade_summary_service::trade_summary_service(context ctx)
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
std::vector<domain::trade_summary>
trade_summary_read_one(repository::trade_summary_repository& repo,
                       const ores::database::context& ctx,
                       const messaging::trade_summary_key& key) {
    return repo.read_latest(ctx, boost::uuids::to_string(key.id));
}

}

messaging::list_trade_summaries_response trade_summary_service::list_trade_summaries(
    const messaging::list_trade_summaries_request& request) {
    messaging::list_trade_summaries_response response;
    if (!request.order.field.empty() &&
        !repository::trade_summary_repository::is_sortable(request.order.field)) {
        response.result = refuse(outcome_code::order_not_supported,
                                 {.entity = "trade summaries", .field = request.order.field});
        return response;
    }
    if (request.filter && request.filter->id_one_of && request.filter->id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->row_kind_one_of &&
        request.filter->row_kind_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "row_kind_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->party_id_one_of &&
        request.filter->party_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "party_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->counterparty_id_one_of &&
        request.filter->counterparty_id_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "counterparty_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->book_id_one_of &&
        request.filter->book_id_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "book_id_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->instrument_type_one_of &&
        request.filter->instrument_type_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "instrument_type_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->status_one_of &&
        request.filter->status_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "status_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->authorisation_status_one_of &&
        request.filter->authorisation_status_one_of->size() > 1000) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "authorisation_status_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->trade_date_one_of &&
        request.filter->trade_date_one_of->size() > 1000) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "trade_date_one_of", .limit = "1000"});
        return response;
    }
    if (request.filter && request.filter->trade_date_from &&
        !ores::database::repository::range_bound(
            *request.filter->trade_date_from, ores::database::repository::range_end::from, false)) {
        response.result =
            refuse(outcome_code::filter_range_invalid,
                   {.field = "trade_date_from", .value = *request.filter->trade_date_from});
        return response;
    }
    if (request.filter && request.filter->trade_date_to &&
        !ores::database::repository::range_bound(
            *request.filter->trade_date_to, ores::database::repository::range_end::to, false)) {
        response.result =
            refuse(outcome_code::filter_range_invalid,
                   {.field = "trade_date_to", .value = *request.filter->trade_date_to});
        return response;
    }
    if (request.filter && request.filter->search && request.filter->search->size() > 256) {
        response.result =
            refuse(outcome_code::filter_too_large, {.field = "search", .limit = "256"});
        return response;
    }
    response.trade_summaries =
        repo_.read_latest(ctx_, request.offset, request.limit, request.order, request.filter);
    response.total = repo_.get_total_trade_summary_count(ctx_, request.filter);
    return response;
}

messaging::get_trade_summary_response
trade_summary_service::get_trade_summary(const messaging::get_trade_summary_request& request) {
    messaging::get_trade_summary_response response;
    auto found = trade_summary_read_one(repo_, ctx_, request.key);
    if (found.empty()) {
        response.result = refuse(outcome_code::not_found, {.entity = "trade_summary"});
        return response;
    }
    response.trade_summary = std::move(found.front());
    return response;
}

messaging::get_many_trade_summaries_response trade_summary_service::get_many_trade_summaries(
    const messaging::get_many_trade_summaries_request& request) {
    messaging::get_many_trade_summaries_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::trade_summary_lookup entry;
        entry.key = k;
        auto found = trade_summary_read_one(repo_, ctx_, k);
        if (!found.empty())
            entry.trade_summary = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}


std::vector<domain::trade_summary>
trade_summary_service::list_trade_summaries(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all trade summaries";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t trade_summary_service::count_trade_summaries() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total trade summaries count";
    return repo_.get_total_trade_summary_count(ctx_);
}


std::optional<domain::trade_summary>
trade_summary_service::get_trade_summary(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting trade summary. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::trade_summary>
trade_summary_service::get_trade_summaries(const std::vector<std::string>& ids) {
    return repo_.read_latest(ctx_, ids);
}

void trade_summary_service::save_trade_summary(const domain::trade_summary& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Trade Summary id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving trade summary. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved trade summary. " << "id: " << v.id;
}

void trade_summary_service::save_trade_summaries(
    const std::vector<domain::trade_summary>& trade_summaries) {
    for (const auto& e : trade_summaries) {
        if (e.id.is_nil())
            throw std::invalid_argument("Trade Summary id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << trade_summaries.size() << " trade summaries";
    auto ts = trade_summaries;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void trade_summary_service::delete_trade_summary(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade summary. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed trade summary. " << "id: " << id;
}

void trade_summary_service::delete_trade_summaries(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}


}
