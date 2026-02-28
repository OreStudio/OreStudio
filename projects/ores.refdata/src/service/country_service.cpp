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
#include "ores.refdata/service/country_service.hpp"

#include <algorithm>
#include <unordered_set>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::service {

using namespace ores::logging;

country_service::country_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_{}
    , junction_repo_(ctx_) {
}

std::vector<domain::country> country_service::list_countries(
    std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing countries with offset=" << offset
                               << " limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t country_service::count_countries() {
    BOOST_LOG_SEV(lg(), debug) << "Counting countries";
    return repo_.get_total_country_count(ctx_);
}

void country_service::save_country(const domain::country& country) {
    if (country.alpha2_code.empty()) {
        throw std::invalid_argument("Country alpha2 code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving country: " << country.alpha2_code;
    repo_.write(ctx_, country);
}

void country_service::save_countries(const std::vector<domain::country>& countries) {
    for (const auto& c : countries) {
        if (c.alpha2_code.empty())
            throw std::invalid_argument("Country alpha2 code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << countries.size() << " countries";
    repo_.write(ctx_, countries);
}

void country_service::delete_country(const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Deleting country: " << alpha2_code;
    repo_.remove(ctx_, alpha2_code);
}

std::optional<domain::country> country_service::get_country(
    const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting country: " << alpha2_code;
    auto results = repo_.read_latest(ctx_, alpha2_code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::vector<domain::country> country_service::get_country_history(
    const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting country history for: " << alpha2_code;
    return repo_.read_all(ctx_, alpha2_code);
}

std::vector<domain::country> country_service::list_countries_for_party(
    const boost::uuids::uuid& party_id,
    std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing countries for party: " << party_id
                               << " offset=" << offset << " limit=" << limit;

    // Get visible alpha-2 codes from the junction table
    const auto junctions = junction_repo_.read_latest_by_party(party_id);
    std::unordered_set<std::string> visible;
    visible.reserve(junctions.size());
    for (const auto& j : junctions)
        visible.insert(j.country_alpha2_code);

    // Fetch all countries and filter to visible set
    auto all = repo_.read_latest(ctx_);
    std::vector<domain::country> filtered;
    filtered.reserve(visible.size());
    for (auto& c : all) {
        if (visible.count(c.alpha2_code))
            filtered.push_back(std::move(c));
    }

    // Apply pagination
    if (offset >= filtered.size())
        return {};
    const auto end = std::min<std::size_t>(offset + limit, filtered.size());
    return std::vector<domain::country>(
        filtered.begin() + offset, filtered.begin() + end);
}

std::uint32_t country_service::count_countries_for_party(
    const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Counting countries for party: " << party_id;
    const auto junctions = junction_repo_.read_latest_by_party(party_id);
    return static_cast<std::uint32_t>(junctions.size());
}

}
