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
#include "ores.refdata.core/service/currency_pair_convention_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

currency_pair_convention_service::currency_pair_convention_service(context ctx)
    : ctx_(std::move(ctx))
    , calendar_repo_(ctx_) {}

std::vector<domain::currency_pair_convention>
currency_pair_convention_service::list_conventions(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currency pair conventions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t currency_pair_convention_service::count_conventions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total currency pair conventions count";
    return repo_.get_total_convention_count(ctx_);
}

std::optional<domain::currency_pair_convention>
currency_pair_convention_service::get_convention_at_version(const std::string& pair_code,
                                                            std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency pair convention at version: " << pair_code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, pair_code, version);
}

std::optional<domain::currency_pair_convention>
currency_pair_convention_service::get_convention(const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency pair convention: " << pair_code;
    auto results = repo_.read_latest(ctx_, pair_code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void currency_pair_convention_service::save_convention(const domain::currency_pair_convention& v) {
    if (v.pair_code.empty())
        throw std::invalid_argument("Currency Pair Convention pair_code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving currency pair convention: " << v.pair_code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved currency pair convention: " << v.pair_code;
}

void currency_pair_convention_service::save_conventions(
    const std::vector<domain::currency_pair_convention>& conventions) {
    for (const auto& e : conventions)
        if (e.pair_code.empty())
            throw std::invalid_argument("Currency Pair Convention pair_code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << conventions.size() << " currency pair conventions";
    auto ts = conventions;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void currency_pair_convention_service::delete_convention(const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency pair convention: " << pair_code;
    repo_.remove(ctx_, pair_code);
    BOOST_LOG_SEV(lg(), info) << "Removed currency pair convention: " << pair_code;
}

void currency_pair_convention_service::delete_conventions(
    const std::vector<std::string>& pair_codes) {
    repo_.remove(ctx_, pair_codes);
}

std::vector<domain::currency_pair_convention>
currency_pair_convention_service::get_convention_history(const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for currency pair convention: " << pair_code;
    return repo_.read_all(ctx_, pair_code);
}

std::vector<ores::refdata::domain::currency_pair_convention_calendar>
currency_pair_convention_service::list_calendars_for_pair_convention(const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing calendars for pair convention: " << pair_code;
    return calendar_repo_.read_latest_by_pair(pair_code);
}

void currency_pair_convention_service::assign_calendar_to_pair_convention(
    const ores::refdata::domain::currency_pair_convention_calendar& row) {
    BOOST_LOG_SEV(lg(), debug) << "Assigning calendar to pair convention: " << row.pair_code << "/"
                               << row.calendar_code;
    calendar_repo_.write(row);
    BOOST_LOG_SEV(lg(), info) << "Assigned calendar to pair convention: " << row.pair_code << "/"
                              << row.calendar_code;
}

void currency_pair_convention_service::revoke_calendar_from_pair_convention(
    const std::string& pair_code, const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Revoking calendar from pair convention: " << pair_code << "/"
                               << calendar_code;
    calendar_repo_.remove(pair_code, calendar_code);
    BOOST_LOG_SEV(lg(), info) << "Revoked calendar from pair convention: " << pair_code << "/"
                              << calendar_code;
}
}
