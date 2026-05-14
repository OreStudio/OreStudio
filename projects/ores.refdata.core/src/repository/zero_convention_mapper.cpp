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
#include "ores.refdata.core/repository/zero_convention_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/zero_convention_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::zero_convention
zero_convention_mapper::map(const zero_convention_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::zero_convention r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = v.id.value();
    r.tenor_based = v.tenor_based;
    r.day_count_fraction = v.day_count_fraction;
    r.compounding = v.compounding;
    r.compounding_frequency = v.compounding_frequency;
    r.tenor_calendar = v.tenor_calendar;
    r.spot_lag = v.spot_lag;
    r.spot_calendar = v.spot_calendar;
    r.roll_convention = v.roll_convention;
    r.end_of_month = v.end_of_month;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

zero_convention_entity
zero_convention_mapper::map(const domain::zero_convention& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    zero_convention_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.tenor_based = v.tenor_based;
    r.day_count_fraction = v.day_count_fraction;
    r.compounding = v.compounding;
    r.compounding_frequency = v.compounding_frequency;
    r.tenor_calendar = v.tenor_calendar;
    r.spot_lag = v.spot_lag;
    r.spot_calendar = v.spot_calendar;
    r.roll_convention = v.roll_convention;
    r.end_of_month = v.end_of_month;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::zero_convention>
zero_convention_mapper::map(const std::vector<zero_convention_entity>& v) {
    return map_vector<zero_convention_entity, domain::zero_convention>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<zero_convention_entity>
zero_convention_mapper::map(const std::vector<domain::zero_convention>& v) {
    return map_vector<domain::zero_convention, zero_convention_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
