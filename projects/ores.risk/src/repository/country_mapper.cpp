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
#include "ores.risk/repository/country_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.risk/domain/country_json_io.hpp" // IWYU pragma: keep.

namespace ores::risk::repository {

using namespace ores::telemetry::log;
using namespace ores::database::repository;

domain::country country_mapper::map(const country_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::country r;
    r.version = v.version;
    BOOST_LOG_SEV(lg(), trace) << "Mapped version: entity.version=" << v.version
                               << " -> domain.version=" << r.version;
    r.alpha2_code = v.alpha2_code.value();
    r.alpha3_code = v.alpha3_code;
    r.numeric_code = v.numeric_code;
    r.name = v.name;
    r.official_name = v.official_name;
    r.recorded_by = v.modified_by;
    r.recorded_at = timestamp_to_timepoint(v.valid_from.value());

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

country_entity country_mapper::map(const domain::country& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    country_entity r;
    r.alpha2_code = v.alpha2_code;
    r.version = v.version;
    r.alpha3_code = v.alpha3_code;
    r.numeric_code = v.numeric_code;
    r.name = v.name;
    r.official_name = v.official_name;
    r.modified_by = v.recorded_by;
    // Note: recorded_at is read-only; valid_from/valid_to are managed by database triggers

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::country>
country_mapper::map(const std::vector<country_entity>& v) {
    return map_vector<country_entity, domain::country>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<country_entity>
country_mapper::map(const std::vector<domain::country>& v) {
    return map_vector<domain::country, country_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
