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
#include "ores.refdata.core/repository/cds_convention_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/cds_convention_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::cds_convention
cds_convention_mapper::map(const cds_convention_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::cds_convention r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = v.id.value();
    r.settlement_days = v.settlement_days;
    r.calendar = v.calendar;
    r.frequency = v.frequency;
    r.payment_convention = v.payment_convention;
    r.rule = v.rule;
    r.day_count_fraction = v.day_count_fraction;
    r.settles_accrual = v.settles_accrual;
    r.pays_at_default_time = v.pays_at_default_time;
    r.upfront_settlement_days = v.upfront_settlement_days;
    r.last_period_day_count_fraction = v.last_period_day_count_fraction;
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

cds_convention_entity
cds_convention_mapper::map(const domain::cds_convention& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    cds_convention_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.settlement_days = v.settlement_days;
    r.calendar = v.calendar;
    r.frequency = v.frequency;
    r.payment_convention = v.payment_convention;
    r.rule = v.rule;
    r.day_count_fraction = v.day_count_fraction;
    r.settles_accrual = v.settles_accrual;
    r.pays_at_default_time = v.pays_at_default_time;
    r.upfront_settlement_days = v.upfront_settlement_days;
    r.last_period_day_count_fraction = v.last_period_day_count_fraction;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::cds_convention>
cds_convention_mapper::map(const std::vector<cds_convention_entity>& v) {
    return map_vector<cds_convention_entity, domain::cds_convention>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<cds_convention_entity>
cds_convention_mapper::map(const std::vector<domain::cds_convention>& v) {
    return map_vector<domain::cds_convention, cds_convention_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
