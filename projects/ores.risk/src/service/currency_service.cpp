/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.risk/service/currency_service.hpp"

namespace ores::risk::service {

using namespace ores::telemetry::log;

currency_service::currency_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_{} {
}

std::vector<domain::currency> currency_service::list_currencies(
    std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing currencies with offset=" << offset
                               << " limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t currency_service::count_currencies() {
    BOOST_LOG_SEV(lg(), debug) << "Counting currencies";
    return repo_.get_total_currency_count(ctx_);
}

void currency_service::save_currency(const domain::currency& currency) {
    BOOST_LOG_SEV(lg(), debug) << "Saving currency: " << currency.iso_code;
    repo_.write(ctx_, currency);
}

void currency_service::delete_currency(const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Deleting currency: " << iso_code;
    repo_.remove(ctx_, iso_code);
}

std::optional<domain::currency> currency_service::get_currency(
    const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency: " << iso_code;
    auto results = repo_.read_latest(ctx_, iso_code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::vector<domain::currency> currency_service::get_currency_history(
    const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency history for: " << iso_code;
    return repo_.read_all(ctx_, iso_code);
}

std::optional<domain::currency_version_history>
currency_service::get_currency_version_history(const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency version history for: " << iso_code;

    auto currencies = repo_.read_all(ctx_, iso_code);
    if (currencies.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No history found for currency: " << iso_code;
        return std::nullopt;
    }

    domain::currency_version_history history;
    history.iso_code = iso_code;

    // Sort by version descending (newest first)
    std::sort(currencies.begin(), currencies.end(),
        [](const auto& a, const auto& b) {
            return a.version > b.version;
        });

    for (const auto& currency : currencies) {
        domain::currency_version version;
        version.data = currency;
        version.version_number = currency.version;
        version.recorded_by = currency.recorded_by;
        version.recorded_at = currency.recorded_at;
        version.change_summary = "Version " + std::to_string(version.version_number);

        BOOST_LOG_SEV(lg(), trace) << "Adding version: iso_code=" << currency.iso_code
                                   << ", version=" << version.version_number;

        history.versions.push_back(std::move(version));
    }

    BOOST_LOG_SEV(lg(), info) << "Retrieved " << history.versions.size()
                              << " versions for currency: " << iso_code;

    return history;
}

}
