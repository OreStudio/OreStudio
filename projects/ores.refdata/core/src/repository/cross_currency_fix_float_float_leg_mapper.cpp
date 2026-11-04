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
#include "ores.refdata.core/repository/cross_currency_fix_float_float_leg_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_float_leg.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_float_leg_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/cross_currency_fix_float_float_leg_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::cross_currency_fix_float_float_leg
cross_currency_fix_float_float_leg_mapper::map(const cross_currency_fix_float_float_leg_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::cross_currency_fix_float_float_leg r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.convention_id = v.convention_id;
    r.party_id = v.party_id;
    r.index = v.index;
    r.float_index_is_resettable = v.float_index_is_resettable;
    r.include_spread = v.include_spread;
    r.lookback = v.lookback;
    r.fixing_days = v.fixing_days;
    r.rate_cutoff = v.rate_cutoff;
    r.is_averaged = v.is_averaged;
    r.observation_shift = v.observation_shift;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

cross_currency_fix_float_float_leg_entity cross_currency_fix_float_float_leg_mapper::map(
    const domain::cross_currency_fix_float_float_leg& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    cross_currency_fix_float_float_leg_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.convention_id = v.convention_id;
    r.party_id = v.party_id;
    r.index = v.index;
    r.float_index_is_resettable = v.float_index_is_resettable;
    r.include_spread = v.include_spread;
    r.lookback = v.lookback;
    r.fixing_days = v.fixing_days;
    r.rate_cutoff = v.rate_cutoff;
    r.is_averaged = v.is_averaged;
    r.observation_shift = v.observation_shift;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::cross_currency_fix_float_float_leg>
cross_currency_fix_float_float_leg_mapper::map(
    const std::vector<cross_currency_fix_float_float_leg_entity>& v) {
    return map_vector<cross_currency_fix_float_float_leg_entity,
                      domain::cross_currency_fix_float_float_leg>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<cross_currency_fix_float_float_leg_entity>
cross_currency_fix_float_float_leg_mapper::map(
    const std::vector<domain::cross_currency_fix_float_float_leg>& v) {
    return map_vector<domain::cross_currency_fix_float_float_leg,
                      cross_currency_fix_float_float_leg_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
