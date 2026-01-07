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
#include "ores.risk/service/country_service.hpp"

namespace ores::risk::service {

using namespace ores::telemetry::log;

country_service::country_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_{} {
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
    BOOST_LOG_SEV(lg(), debug) << "Saving country: " << country.alpha2_code;
    repo_.write(ctx_, country);
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

}
