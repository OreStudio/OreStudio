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
#include "ores.refdata.core/repository/ois_convention_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/ois_convention_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::ois_convention
ois_convention_mapper::map(const ois_convention_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::ois_convention r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = v.id.value();
    r.spot_lag = v.spot_lag;
    r.index = v.index;
    r.fixed_day_count_fraction = v.fixed_day_count_fraction;
    r.fixed_calendar = v.fixed_calendar;
    r.payment_lag = v.payment_lag;
    r.end_of_month = v.end_of_month;
    r.fixed_frequency = v.fixed_frequency;
    r.fixed_convention = v.fixed_convention;
    r.fixed_payment_convention = v.fixed_payment_convention;
    r.rule = v.rule;
    r.payment_calendar = v.payment_calendar;
    r.rate_cutoff = v.rate_cutoff;
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

ois_convention_entity
ois_convention_mapper::map(const domain::ois_convention& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    ois_convention_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.spot_lag = v.spot_lag;
    r.index = v.index;
    r.fixed_day_count_fraction = v.fixed_day_count_fraction;
    r.fixed_calendar = v.fixed_calendar;
    r.payment_lag = v.payment_lag;
    r.end_of_month = v.end_of_month;
    r.fixed_frequency = v.fixed_frequency;
    r.fixed_convention = v.fixed_convention;
    r.fixed_payment_convention = v.fixed_payment_convention;
    r.rule = v.rule;
    r.payment_calendar = v.payment_calendar;
    r.rate_cutoff = v.rate_cutoff;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::ois_convention>
ois_convention_mapper::map(const std::vector<ois_convention_entity>& v) {
    return map_vector<ois_convention_entity, domain::ois_convention>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<ois_convention_entity>
ois_convention_mapper::map(const std::vector<domain::ois_convention>& v) {
    return map_vector<domain::ois_convention, ois_convention_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
