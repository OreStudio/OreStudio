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
#include "ores.trading.core/repository/bond_leg_rate_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/bond_leg_rate_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::bond_leg_rate bond_leg_rate_mapper::map(const bond_leg_rate_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::bond_leg_rate r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.instrument_id.value());
    r.leg_role = v.leg_role.value();
    r.leg_number = boost::lexical_cast<int>(v.leg_number.value());
    r.rate_kind = v.rate_kind;
    r.index = v.index;
    r.is_in_arrears = v.is_in_arrears;
    r.fixing_days = v.fixing_days;
    r.fixing_calendar = v.fixing_calendar;
    r.last_recent_period = v.last_recent_period;
    r.last_recent_period_calendar = v.last_recent_period_calendar;
    r.lookback = v.lookback;
    r.rate_cutoff = v.rate_cutoff;
    r.is_averaged = v.is_averaged;
    r.has_sub_periods = v.has_sub_periods;
    r.include_spread = v.include_spread;
    r.is_not_resetting_xccy = v.is_not_resetting_xccy;
    r.naked_option = v.naked_option;
    r.local_cap_floor = v.local_cap_floor;
    r.stub_use_original_curve = v.stub_use_original_curve;
    r.observation_shift = v.observation_shift;
    r.front_stub_short_index = v.front_stub_short_index;
    r.front_stub_long_index = v.front_stub_long_index;
    r.front_stub_rounding_type = v.front_stub_rounding_type;
    r.front_stub_rounding_precision = v.front_stub_rounding_precision;
    r.back_stub_short_index = v.back_stub_short_index;
    r.back_stub_long_index = v.back_stub_long_index;
    r.back_stub_rounding_type = v.back_stub_rounding_type;
    r.back_stub_rounding_precision = v.back_stub_rounding_precision;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

bond_leg_rate_entity bond_leg_rate_mapper::map(const domain::bond_leg_rate& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    bond_leg_rate_entity r;
    r.instrument_id = boost::uuids::to_string(v.instrument_id);
    r.leg_role = v.leg_role;
    r.leg_number = std::to_string(v.leg_number);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.rate_kind = v.rate_kind;
    r.index = v.index;
    r.is_in_arrears = v.is_in_arrears;
    r.fixing_days = v.fixing_days;
    r.fixing_calendar = v.fixing_calendar;
    r.last_recent_period = v.last_recent_period;
    r.last_recent_period_calendar = v.last_recent_period_calendar;
    r.lookback = v.lookback;
    r.rate_cutoff = v.rate_cutoff;
    r.is_averaged = v.is_averaged;
    r.has_sub_periods = v.has_sub_periods;
    r.include_spread = v.include_spread;
    r.is_not_resetting_xccy = v.is_not_resetting_xccy;
    r.naked_option = v.naked_option;
    r.local_cap_floor = v.local_cap_floor;
    r.stub_use_original_curve = v.stub_use_original_curve;
    r.observation_shift = v.observation_shift;
    r.front_stub_short_index = v.front_stub_short_index;
    r.front_stub_long_index = v.front_stub_long_index;
    r.front_stub_rounding_type = v.front_stub_rounding_type;
    r.front_stub_rounding_precision = v.front_stub_rounding_precision;
    r.back_stub_short_index = v.back_stub_short_index;
    r.back_stub_long_index = v.back_stub_long_index;
    r.back_stub_rounding_type = v.back_stub_rounding_type;
    r.back_stub_rounding_precision = v.back_stub_rounding_precision;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::bond_leg_rate>
bond_leg_rate_mapper::map(const std::vector<bond_leg_rate_entity>& v) {
    return map_vector<bond_leg_rate_entity, domain::bond_leg_rate>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<bond_leg_rate_entity>
bond_leg_rate_mapper::map(const std::vector<domain::bond_leg_rate>& v) {
    return map_vector<domain::bond_leg_rate, bond_leg_rate_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
