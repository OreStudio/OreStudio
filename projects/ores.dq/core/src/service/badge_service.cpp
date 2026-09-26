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
#include "ores.dq.core/service/badge_service.hpp"
#include "ores.dq.api/domain/badge_mapping_json_io.hpp" // IWYU pragma: keep.

namespace ores::dq::service {

using namespace ores::logging;

badge_service::badge_service(context ctx)
    : ctx_(ctx)
    , map_repo_(ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Badge service initialised.";
}

// =============================================================================
// Badge Mapping (read-only)
// =============================================================================

std::vector<messaging::badge_mapping> badge_service::list_mappings() {
    BOOST_LOG_SEV(lg(), debug) << "Listing badge mappings.";
    const auto domain_mappings = map_repo_.read_latest();
    std::vector<messaging::badge_mapping> result;
    result.reserve(domain_mappings.size());
    for (const auto& m : domain_mappings) {
        result.push_back(messaging::badge_mapping{
            .code_domain_code = m.code_domain_code,
            .entity_code = m.entity_code,
            .badge_code = m.badge_code,
        });
    }
    return result;
}

}
