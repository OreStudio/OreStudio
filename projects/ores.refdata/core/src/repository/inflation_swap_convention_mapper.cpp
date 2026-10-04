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
#include "ores.refdata.core/repository/inflation_swap_convention_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/inflation_swap_convention_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::inflation_swap_convention
inflation_swap_convention_mapper::map(const inflation_swap_convention_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::inflation_swap_convention r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.workspace_id = boost::lexical_cast<boost::uuids::uuid>(v.workspace_id);
    r.id = v.id.value();
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.fix_calendar = v.fix_calendar;
    r.fix_convention = v.fix_convention;
    r.day_count_fraction = v.day_count_fraction;
    r.index = v.index;
    r.interpolated = v.interpolated;
    r.observation_lag = v.observation_lag;
    r.adjust_inflation_observation_dates = v.adjust_inflation_observation_dates;
    r.inflation_calendar = v.inflation_calendar;
    r.inflation_convention = v.inflation_convention;
    r.publication_roll = v.publication_roll;
    r.start_delay = v.start_delay;
    r.start_delay_convention = v.start_delay_convention;
    r.publication_schedule_name = v.publication_schedule_name;
    r.publication_schedule_rules = v.publication_schedule_rules;
    r.publication_schedule_dates = v.publication_schedule_dates;
    r.publication_schedule_derived_groups = v.publication_schedule_derived_groups;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

inflation_swap_convention_entity
inflation_swap_convention_mapper::map(const domain::inflation_swap_convention& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    inflation_swap_convention_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id.to_string();
    r.workspace_id = boost::uuids::to_string(v.workspace_id);
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.fix_calendar = v.fix_calendar;
    r.fix_convention = v.fix_convention;
    r.day_count_fraction = v.day_count_fraction;
    r.index = v.index;
    r.interpolated = v.interpolated;
    r.observation_lag = v.observation_lag;
    r.adjust_inflation_observation_dates = v.adjust_inflation_observation_dates;
    r.inflation_calendar = v.inflation_calendar;
    r.inflation_convention = v.inflation_convention;
    r.publication_roll = v.publication_roll;
    r.start_delay = v.start_delay;
    r.start_delay_convention = v.start_delay_convention;
    r.publication_schedule_name = v.publication_schedule_name;
    r.publication_schedule_rules = v.publication_schedule_rules;
    r.publication_schedule_dates = v.publication_schedule_dates;
    r.publication_schedule_derived_groups = v.publication_schedule_derived_groups;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::inflation_swap_convention>
inflation_swap_convention_mapper::map(const std::vector<inflation_swap_convention_entity>& v) {
    return map_vector<inflation_swap_convention_entity, domain::inflation_swap_convention>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<inflation_swap_convention_entity>
inflation_swap_convention_mapper::map(const std::vector<domain::inflation_swap_convention>& v) {
    return map_vector<domain::inflation_swap_convention, inflation_swap_convention_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
