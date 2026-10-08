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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.trading.core/repository/booking_nature_type_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/booking_nature_type.hpp"
#include "ores.trading.api/domain/booking_nature_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/booking_nature_type_entity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::booking_nature_type booking_nature_type_mapper::map(const booking_nature_type_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::booking_nature_type r;
    r.version = v.version;
    r.code = v.code.value();
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

booking_nature_type_entity booking_nature_type_mapper::map(const domain::booking_nature_type& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    booking_nature_type_entity r;
    r.code = v.code;
    r.version = v.version;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::booking_nature_type>
booking_nature_type_mapper::map(const std::vector<booking_nature_type_entity>& v) {
    return map_vector<booking_nature_type_entity, domain::booking_nature_type>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<booking_nature_type_entity>
booking_nature_type_mapper::map(const std::vector<domain::booking_nature_type>& v) {
    return map_vector<domain::booking_nature_type, booking_nature_type_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
