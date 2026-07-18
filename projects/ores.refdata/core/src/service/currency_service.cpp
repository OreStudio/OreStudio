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
#include "ores.refdata.core/service/currency_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <stdexcept>
#include <unordered_set>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

currency_service::currency_service(context ctx)
    : ctx_(std::move(ctx))
    , junction_repo_(ctx_)

    , calendar_repo_(ctx_) {}

std::vector<domain::currency> currency_service::list_currencies(std::uint32_t offset,
                                                                std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currencies";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t currency_service::count_currencies() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total currencies count";
    return repo_.get_total_currency_count(ctx_);
}

std::optional<domain::currency>
currency_service::get_currency_at_version(const std::string& iso_code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency at version: " << iso_code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, iso_code, version);
}

std::optional<domain::currency> currency_service::get_currency(const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency: " << iso_code;
    auto results = repo_.read_latest(ctx_, iso_code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void currency_service::save_currency(const domain::currency& v) {
    if (v.iso_code.empty())
        throw std::invalid_argument("Currency iso_code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving currency: " << v.iso_code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved currency: " << v.iso_code;
}

void currency_service::save_currencies(const std::vector<domain::currency>& currencies) {
    for (const auto& e : currencies)
        if (e.iso_code.empty())
            throw std::invalid_argument("Currency iso_code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << currencies.size() << " currencies";
    auto ts = currencies;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void currency_service::delete_currency(const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency: " << iso_code;
    repo_.remove(ctx_, iso_code);
    BOOST_LOG_SEV(lg(), info) << "Removed currency: " << iso_code;
}

void currency_service::delete_currencies(const std::vector<std::string>& iso_codes) {
    repo_.remove(ctx_, iso_codes);
}

std::vector<domain::currency> currency_service::get_currency_history(const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for currency: " << iso_code;
    return repo_.read_all(ctx_, iso_code);
}

std::vector<domain::currency> currency_service::list_currencies_for_party(
    const boost::uuids::uuid& party_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing currencies for party: " << party_id
                               << " offset=" << offset << " limit=" << limit;

    const auto junctions = junction_repo_.read_latest_by_party(party_id);
    std::unordered_set<std::string> visible;
    visible.reserve(junctions.size());
    for (const auto& j : junctions)
        visible.insert(j.currency_iso_code);

    auto all = repo_.read_latest(ctx_);
    std::vector<domain::currency> filtered;
    filtered.reserve(visible.size());
    for (auto& c : all) {
        if (visible.count(c.iso_code))
            filtered.push_back(std::move(c));
    }

    if (offset >= filtered.size())
        return {};
    const auto end = std::min<std::size_t>(offset + limit, filtered.size());
    return std::vector<domain::currency>(filtered.begin() + offset, filtered.begin() + end);
}

std::uint32_t currency_service::count_currencies_for_party(const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Counting currencies for party: " << party_id;
    const auto junctions = junction_repo_.read_latest_by_party(party_id);
    return static_cast<std::uint32_t>(junctions.size());
}

std::vector<ores::refdata::domain::currency_calendar>
currency_service::list_calendars_for_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing calendars for currency: " << currency_iso_code;
    return calendar_repo_.read_latest_by_currency(currency_iso_code);
}

void currency_service::assign_calendar_to_currency(
    const ores::refdata::domain::currency_calendar& row) {
    BOOST_LOG_SEV(lg(), debug) << "Assigning calendar to currency: " << row.currency_iso_code << "/"
                               << row.calendar_code;
    calendar_repo_.write(row);
    BOOST_LOG_SEV(lg(), info) << "Assigned calendar to currency: " << row.currency_iso_code << "/"
                              << row.calendar_code;
}

void currency_service::revoke_calendar_from_currency(const std::string& currency_iso_code,
                                                     const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Revoking calendar from currency: " << currency_iso_code << "/"
                               << calendar_code;
    calendar_repo_.remove(currency_iso_code, calendar_code);
    BOOST_LOG_SEV(lg(), info) << "Revoked calendar from currency: " << currency_iso_code << "/"
                              << calendar_code;
}
}
