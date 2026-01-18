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
#include "ores.dq/service/dimension_service.hpp"

#include <stdexcept>

namespace ores::dq::service {

using namespace ores::logging;

dimension_service::dimension_service(context ctx)
    : nature_repo_(ctx), origin_repo_(ctx), treatment_repo_(ctx) {}

// ============================================================================
// Nature Dimension Management
// ============================================================================

std::vector<domain::nature_dimension> dimension_service::list_nature_dimensions() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all nature dimensions";
    return nature_repo_.read_latest();
}

std::optional<domain::nature_dimension>
dimension_service::find_nature_dimension(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding nature dimension: " << code;
    auto dimensions = nature_repo_.read_latest(code);
    if (dimensions.empty()) {
        return std::nullopt;
    }
    return dimensions.front();
}

domain::nature_dimension
dimension_service::create_nature_dimension(const domain::nature_dimension& dimension) {
    BOOST_LOG_SEV(lg(), debug) << "Creating nature dimension: " << dimension.code;

    auto existing = find_nature_dimension(dimension.code);
    if (existing) {
        throw std::runtime_error("Nature dimension already exists: " + dimension.code);
    }

    nature_repo_.write(dimension);
    BOOST_LOG_SEV(lg(), info) << "Created nature dimension: " << dimension.code;

    auto created = find_nature_dimension(dimension.code);
    if (!created) {
        throw std::runtime_error("Failed to retrieve created nature dimension");
    }
    return *created;
}

void dimension_service::update_nature_dimension(const domain::nature_dimension& dimension) {
    BOOST_LOG_SEV(lg(), debug) << "Updating nature dimension: " << dimension.code;
    nature_repo_.write(dimension);
    BOOST_LOG_SEV(lg(), info) << "Updated nature dimension: " << dimension.code;
}

void dimension_service::remove_nature_dimension(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing nature dimension: " << code;
    nature_repo_.remove(code);
    BOOST_LOG_SEV(lg(), info) << "Removed nature dimension: " << code;
}

std::vector<domain::nature_dimension>
dimension_service::get_nature_dimension_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for nature dimension: " << code;
    return nature_repo_.read_all(code);
}

// ============================================================================
// Origin Dimension Management
// ============================================================================

std::vector<domain::origin_dimension> dimension_service::list_origin_dimensions() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all origin dimensions";
    return origin_repo_.read_latest();
}

std::optional<domain::origin_dimension>
dimension_service::find_origin_dimension(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding origin dimension: " << code;
    auto dimensions = origin_repo_.read_latest(code);
    if (dimensions.empty()) {
        return std::nullopt;
    }
    return dimensions.front();
}

domain::origin_dimension
dimension_service::create_origin_dimension(const domain::origin_dimension& dimension) {
    BOOST_LOG_SEV(lg(), debug) << "Creating origin dimension: " << dimension.code;

    auto existing = find_origin_dimension(dimension.code);
    if (existing) {
        throw std::runtime_error("Origin dimension already exists: " + dimension.code);
    }

    origin_repo_.write(dimension);
    BOOST_LOG_SEV(lg(), info) << "Created origin dimension: " << dimension.code;

    auto created = find_origin_dimension(dimension.code);
    if (!created) {
        throw std::runtime_error("Failed to retrieve created origin dimension");
    }
    return *created;
}

void dimension_service::update_origin_dimension(const domain::origin_dimension& dimension) {
    BOOST_LOG_SEV(lg(), debug) << "Updating origin dimension: " << dimension.code;
    origin_repo_.write(dimension);
    BOOST_LOG_SEV(lg(), info) << "Updated origin dimension: " << dimension.code;
}

void dimension_service::remove_origin_dimension(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing origin dimension: " << code;
    origin_repo_.remove(code);
    BOOST_LOG_SEV(lg(), info) << "Removed origin dimension: " << code;
}

std::vector<domain::origin_dimension>
dimension_service::get_origin_dimension_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for origin dimension: " << code;
    return origin_repo_.read_all(code);
}

// ============================================================================
// Treatment Dimension Management
// ============================================================================

std::vector<domain::treatment_dimension> dimension_service::list_treatment_dimensions() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all treatment dimensions";
    return treatment_repo_.read_latest();
}

std::optional<domain::treatment_dimension>
dimension_service::find_treatment_dimension(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding treatment dimension: " << code;
    auto dimensions = treatment_repo_.read_latest(code);
    if (dimensions.empty()) {
        return std::nullopt;
    }
    return dimensions.front();
}

domain::treatment_dimension
dimension_service::create_treatment_dimension(const domain::treatment_dimension& dimension) {
    BOOST_LOG_SEV(lg(), debug) << "Creating treatment dimension: " << dimension.code;

    auto existing = find_treatment_dimension(dimension.code);
    if (existing) {
        throw std::runtime_error("Treatment dimension already exists: " + dimension.code);
    }

    treatment_repo_.write(dimension);
    BOOST_LOG_SEV(lg(), info) << "Created treatment dimension: " << dimension.code;

    auto created = find_treatment_dimension(dimension.code);
    if (!created) {
        throw std::runtime_error("Failed to retrieve created treatment dimension");
    }
    return *created;
}

void dimension_service::update_treatment_dimension(const domain::treatment_dimension& dimension) {
    BOOST_LOG_SEV(lg(), debug) << "Updating treatment dimension: " << dimension.code;
    treatment_repo_.write(dimension);
    BOOST_LOG_SEV(lg(), info) << "Updated treatment dimension: " << dimension.code;
}

void dimension_service::remove_treatment_dimension(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing treatment dimension: " << code;
    treatment_repo_.remove(code);
    BOOST_LOG_SEV(lg(), info) << "Removed treatment dimension: " << code;
}

std::vector<domain::treatment_dimension>
dimension_service::get_treatment_dimension_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for treatment dimension: " << code;
    return treatment_repo_.read_all(code);
}

}
