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
#include "ores.refdata.core/service/country_service.hpp"

#include <stdexcept>

namespace ores::refdata::service {

using namespace ores::logging;

country_service::country_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::country> country_service::list_countries() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all countries";
    return repo_.read_latest(ctx_);
}

std::optional<domain::country>
country_service::get_country(const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting country: " << alpha2_code;
    auto results = repo_.read_latest(ctx_, alpha2_code);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void country_service::save_country(const domain::country& v) {
    if (v.alpha2_code.empty())
        throw std::invalid_argument("Country alpha2_code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving country: " << v.alpha2_code;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved country: " << v.alpha2_code;
}

void country_service::save_countries(
    const std::vector<domain::country>& countries) {
    for (const auto& e : countries)
        if (e.alpha2_code.empty())
            throw std::invalid_argument("Country alpha2_code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << countries.size()
        << " countries";
    repo_.write(ctx_, countries);
}

void country_service::remove_country(const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing country: " << alpha2_code;
    repo_.remove(ctx_, alpha2_code);
    BOOST_LOG_SEV(lg(), info) << "Removed country: " << alpha2_code;
}

void country_service::remove_countries(
    const std::vector<std::string>& alpha2_codes) {
    repo_.remove(ctx_, alpha2_codes);
}

std::vector<domain::country>
country_service::get_country_history(const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for country: " << alpha2_code;
    return repo_.read_all(ctx_, alpha2_code);
}

}
