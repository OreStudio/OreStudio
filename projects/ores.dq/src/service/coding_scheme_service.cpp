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
#include "ores.dq/service/coding_scheme_service.hpp"

#include <stdexcept>

namespace ores::dq::service {

using namespace ores::logging;

coding_scheme_service::coding_scheme_service(context ctx)
    : coding_scheme_repo_(ctx) {}

// ============================================================================
// Coding Scheme Management
// ============================================================================

std::vector<domain::coding_scheme> coding_scheme_service::list_coding_schemes() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all coding schemes";
    return coding_scheme_repo_.read_latest();
}

std::vector<domain::coding_scheme>
coding_scheme_service::list_coding_schemes(std::uint32_t offset,
                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing coding schemes with pagination: offset="
                               << offset << ", limit=" << limit;
    return coding_scheme_repo_.read_latest(offset, limit);
}

std::vector<domain::coding_scheme>
coding_scheme_service::list_coding_schemes_by_authority_type(
    const std::string& authority_type) {
    BOOST_LOG_SEV(lg(), debug) << "Listing coding schemes for authority type: "
                               << authority_type;
    return coding_scheme_repo_.read_latest_by_authority_type(authority_type);
}

std::uint32_t coding_scheme_service::get_coding_scheme_count() {
    return coding_scheme_repo_.get_total_count();
}

std::optional<domain::coding_scheme>
coding_scheme_service::find_coding_scheme(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding coding scheme: " << code;
    auto schemes = coding_scheme_repo_.read_latest(code);
    if (schemes.empty()) {
        return std::nullopt;
    }
    return schemes.front();
}

domain::coding_scheme
coding_scheme_service::create_coding_scheme(const domain::coding_scheme& scheme) {
    BOOST_LOG_SEV(lg(), debug) << "Creating coding scheme: " << scheme.code;

    if (scheme.code.empty()) {
        throw std::invalid_argument("Coding scheme code cannot be empty");
    }

    auto existing = find_coding_scheme(scheme.code);
    if (existing) {
        throw std::runtime_error("Coding scheme already exists: " + scheme.code);
    }

    coding_scheme_repo_.write(scheme);
    BOOST_LOG_SEV(lg(), info) << "Created coding scheme: " << scheme.code;

    auto created = find_coding_scheme(scheme.code);
    if (!created) {
        throw std::runtime_error("Failed to retrieve created coding scheme");
    }
    return *created;
}

void coding_scheme_service::update_coding_scheme(
    const domain::coding_scheme& scheme) {
    BOOST_LOG_SEV(lg(), debug) << "Updating coding scheme: " << scheme.code;

    if (scheme.code.empty()) {
        throw std::invalid_argument("Coding scheme code cannot be empty");
    }

    coding_scheme_repo_.write(scheme);
    BOOST_LOG_SEV(lg(), info) << "Updated coding scheme: " << scheme.code;
}

void coding_scheme_service::remove_coding_scheme(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing coding scheme: " << code;
    coding_scheme_repo_.remove(code);
    BOOST_LOG_SEV(lg(), info) << "Removed coding scheme: " << code;
}

std::vector<domain::coding_scheme>
coding_scheme_service::get_coding_scheme_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for coding scheme: " << code;
    return coding_scheme_repo_.read_all(code);
}

}
