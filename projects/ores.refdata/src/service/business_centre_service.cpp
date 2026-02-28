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
#include "ores.refdata/service/business_centre_service.hpp"

namespace ores::refdata::service {

using namespace ores::logging;

business_centre_service::business_centre_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_{} {
}

std::vector<domain::business_centre> business_centre_service::list_business_centres(
    std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing business centres with offset=" << offset
                               << " limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t business_centre_service::count_business_centres() {
    BOOST_LOG_SEV(lg(), debug) << "Counting business centres";
    return repo_.get_total_business_centre_count(ctx_);
}

void business_centre_service::save_business_centre(
    const domain::business_centre& bc) {
    if (bc.code.empty()) {
        throw std::invalid_argument("Business centre code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving business centre: " << bc.code;
    repo_.write(ctx_, bc);
}

void business_centre_service::save_business_centres(
    const std::vector<domain::business_centre>& business_centres) {
    for (const auto& bc : business_centres) {
        if (bc.code.empty())
            throw std::invalid_argument("Business centre code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << business_centres.size() << " business centres";
    repo_.write(ctx_, business_centres);
}

void business_centre_service::delete_business_centre(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Deleting business centre: " << code;
    repo_.remove(ctx_, code);
}

std::optional<domain::business_centre> business_centre_service::get_business_centre(
    const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting business centre: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::vector<domain::business_centre> business_centre_service::get_business_centre_history(
    const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting business centre history for: " << code;
    return repo_.read_all(ctx_, code);
}

}
