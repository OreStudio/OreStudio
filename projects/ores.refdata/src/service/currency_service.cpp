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
#include "ores.refdata/service/currency_service.hpp"

#include <algorithm>
#include <unordered_set>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::service {

using namespace ores::logging;

currency_service::currency_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_{}
    , junction_repo_(ctx_) {
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
    if (currency.iso_code.empty()) {
        throw std::invalid_argument("Currency ISO code cannot be empty.");
    }
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
        version.modified_by = currency.modified_by;
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

std::vector<domain::currency> currency_service::list_currencies_for_party(
    const boost::uuids::uuid& party_id,
    std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing currencies for party: " << party_id
                               << " offset=" << offset << " limit=" << limit;

    // Get visible ISO codes from the junction table
    const auto junctions = junction_repo_.read_latest_by_party(party_id);
    std::unordered_set<std::string> visible;
    visible.reserve(junctions.size());
    for (const auto& j : junctions)
        visible.insert(j.currency_iso_code);

    // Fetch all currencies and filter to visible set
    auto all = repo_.read_latest(ctx_);
    std::vector<domain::currency> filtered;
    filtered.reserve(visible.size());
    for (auto& c : all) {
        if (visible.count(c.iso_code))
            filtered.push_back(std::move(c));
    }

    // Apply pagination
    if (offset >= filtered.size())
        return {};
    const auto end = std::min<std::size_t>(offset + limit, filtered.size());
    return std::vector<domain::currency>(
        filtered.begin() + offset, filtered.begin() + end);
}

std::uint32_t currency_service::count_currencies_for_party(
    const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Counting currencies for party: " << party_id;
    const auto junctions = junction_repo_.read_latest_by_party(party_id);
    return static_cast<std::uint32_t>(junctions.size());
}

}
