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
#include "ores.refdata.core/repository/fx_convention_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/fx_convention_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::fx_convention
fx_convention_mapper::map(const fx_convention_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::fx_convention r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.workspace_id = boost::lexical_cast<boost::uuids::uuid>(v.workspace_id);
    r.id = v.id.value();
    r.spot_days = v.spot_days;
    r.source_currency = v.source_currency;
    r.target_currency = v.target_currency;
    r.points_factor = v.points_factor;
    r.advance_calendar = v.advance_calendar;
    r.spot_relative = v.spot_relative;
    r.end_of_month = v.end_of_month;
    r.convention = v.convention;
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

fx_convention_entity
fx_convention_mapper::map(const domain::fx_convention& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    fx_convention_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id.to_string();
    r.workspace_id = boost::uuids::to_string(v.workspace_id);
    r.version = v.version;
    r.spot_days = v.spot_days;
    r.source_currency = v.source_currency;
    r.target_currency = v.target_currency;
    r.points_factor = v.points_factor;
    r.advance_calendar = v.advance_calendar;
    r.spot_relative = v.spot_relative;
    r.end_of_month = v.end_of_month;
    r.convention = v.convention;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::fx_convention>
fx_convention_mapper::map(const std::vector<fx_convention_entity>& v) {
    return map_vector<fx_convention_entity, domain::fx_convention>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<fx_convention_entity>
fx_convention_mapper::map(const std::vector<domain::fx_convention>& v) {
    return map_vector<domain::fx_convention, fx_convention_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
