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
#include "ores.dq.core/service/badge_mapping_service.hpp"
#include <stdexcept>

namespace ores::dq::service {

using namespace ores::logging;

badge_mapping_service::badge_mapping_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::badge_mapping> badge_mapping_service::list_mappings() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all badge mappings";
    return repo_.read_latest();
}

std::vector<domain::badge_mapping>
badge_mapping_service::list_mappings_by_code_domain(const std::string& code_domain_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing badge mappings for code domain: " << code_domain_code;
    return repo_.read_latest_by_code_domain(code_domain_code);
}

void badge_mapping_service::save_mapping(const domain::badge_mapping& mapping) {
    if (mapping.code_domain_code.empty()) {
        throw std::invalid_argument("Code Domain cannot be empty.");
    }
    if (mapping.entity_code.empty()) {
        throw std::invalid_argument("Entity Code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving badge mapping: " << mapping.code_domain_code << "/"
                               << mapping.entity_code;
    repo_.write(mapping);
    BOOST_LOG_SEV(lg(), info) << "Saved badge mapping: " << mapping.code_domain_code << "/"
                              << mapping.entity_code;
}

void badge_mapping_service::remove_mapping(const std::string& code_domain_code,
                                           const std::string& entity_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing badge mapping: " << code_domain_code << "/"
                               << entity_code;
    repo_.remove(code_domain_code, entity_code);
    BOOST_LOG_SEV(lg(), info) << "Removed badge mapping: " << code_domain_code << "/"
                              << entity_code;
}

}
