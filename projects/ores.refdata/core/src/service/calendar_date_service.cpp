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
#include "ores.refdata.core/service/calendar_date_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::refdata::service {

using namespace ores::logging;
using ores::service::messaging::stamp;


calendar_date_service::calendar_date_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_(ctx_) {}

namespace {

/**
 * @brief The current row a key names, or an empty vector when there is none.
 *
 * A junction key is the pair of columns that names one link, and the
 * repository takes each in its own column's type, so the pair passes
 * straight through.
 */
std::vector<domain::calendar_date> read_one(repository::calendar_date_repository& repo,
                                            const messaging::calendar_date_key& key) {
    return repo.read_latest(key.calendar_code, key.date);
}

} // namespace

messaging::list_calendar_dates_response
calendar_date_service::list_calendar_dates(const messaging::list_calendar_dates_request& request) {
    messaging::list_calendar_dates_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    if (request.filter) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_not_supported";
        response.result.message = "Filtering is not served for this resource yet.";
        return response;
    }
    response.calendar_dates = repo_.read_latest(request.offset, request.limit);
    response.total = repo_.get_total_calendar_date_count();
    return response;
}

messaging::list_by_calendar_code_calendar_dates_response
calendar_date_service::list_by_calendar_code_calendar_dates(
    const messaging::list_by_calendar_code_calendar_dates_request& request) {
    messaging::list_by_calendar_code_calendar_dates_response response;
    if (!request.order.field.empty() || request.order.descending) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "order_not_supported";
        response.result.message =
            "This store pages in key order and cannot order by a stated field.";
        return response;
    }
    if (request.filter) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "filter_not_supported";
        response.result.message = "Filtering is not served for this resource yet.";
        return response;
    }
    if (request.scope == ores::utility::domain::scope::subtree) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "scope_not_supported";
        response.result.message = "This resource reads its direct members; it has no subtree.";
        return response;
    }
    response.calendar_dates =
        repo_.read_latest_by_calendar(request.calendar_code, request.offset, request.limit);
    response.total = repo_.get_total_calendar_date_count_by_calendar(request.calendar_code);
    return response;
}

messaging::get_calendar_date_response
calendar_date_service::get_calendar_date(const messaging::get_calendar_date_request& request) {
    messaging::get_calendar_date_response response;
    auto found = read_one(repo_, request.key);
    if (found.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        return response;
    }
    response.calendar_date = std::move(found.front());
    return response;
}

messaging::get_many_calendar_dates_response calendar_date_service::get_many_calendar_dates(
    const messaging::get_many_calendar_dates_request& request) {
    messaging::get_many_calendar_dates_response response;
    // One entry per requested key, in the order asked for, so the reply is
    // positional and a caller reads absence from an empty entry rather than
    // from a missing one.
    response.entries.reserve(request.keys.size());
    for (const auto& k : request.keys) {
        messaging::calendar_date_lookup entry;
        entry.key = k;
        auto found = read_one(repo_, k);
        if (!found.empty())
            entry.calendar_date = std::move(found.front());
        response.entries.push_back(std::move(entry));
    }
    return response;
}

}
