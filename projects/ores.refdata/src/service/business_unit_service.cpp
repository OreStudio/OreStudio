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
#include "ores.refdata/service/business_unit_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::service {

using namespace ores::logging;

business_unit_service::business_unit_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::business_unit> business_unit_service::list_business_units() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all business units";
    return repo_.read_latest();
}

std::optional<domain::business_unit>
business_unit_service::find_business_unit(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding business unit: " << id;
    auto results = repo_.read_latest(id);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::optional<domain::business_unit>
business_unit_service::find_business_unit_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding business unit by code: " << code;
    auto results = repo_.read_latest_by_code(code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void business_unit_service::save_business_unit(const domain::business_unit& business_unit) {
    if (business_unit.id.is_nil()) {
        throw std::invalid_argument("Business Unit ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving business unit: " << business_unit.id;
    repo_.write(business_unit);
    BOOST_LOG_SEV(lg(), info) << "Saved business unit: " << business_unit.id;
}

void business_unit_service::save_business_units(
    const std::vector<domain::business_unit>& business_units) {
    for (const auto& bu : business_units) {
        if (bu.id.is_nil())
            throw std::invalid_argument("Business Unit ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << business_units.size() << " business units";
    repo_.write(business_units);
}

void business_unit_service::remove_business_unit(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing business unit: " << id;
    repo_.remove(id);
    BOOST_LOG_SEV(lg(), info) << "Removed business unit: " << id;
}

std::vector<domain::business_unit>
business_unit_service::get_business_unit_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for business unit: " << id;
    return repo_.read_all(id);
}

}
