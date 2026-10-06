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
#include "ores.refdata.core/repository/cross_currency_basis_convention_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/cross_currency_basis_convention.hpp"
#include "ores.refdata.api/domain/cross_currency_basis_convention_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/cross_currency_basis_convention_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::cross_currency_basis_convention
cross_currency_basis_convention_mapper::map(const cross_currency_basis_convention_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::cross_currency_basis_convention r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = v.id.value();
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.settlement_days = v.settlement_days;
    r.settlement_calendar = v.settlement_calendar;
    r.roll_convention = v.roll_convention;
    r.flat_index = v.flat_index;
    r.spread_index = v.spread_index;
    r.eom = v.eom;
    r.is_resettable = v.is_resettable;
    r.flat_index_is_resettable = v.flat_index_is_resettable;
    r.flat_tenor = v.flat_tenor;
    r.spread_tenor = v.spread_tenor;
    r.spread_payment_lag = v.spread_payment_lag;
    r.flat_payment_lag = v.flat_payment_lag;
    r.spread_include_spread = v.spread_include_spread;
    r.spread_lookback = v.spread_lookback;
    r.spread_fixing_days = v.spread_fixing_days;
    r.spread_rate_cutoff = v.spread_rate_cutoff;
    r.spread_is_averaged = v.spread_is_averaged;
    r.spread_observation_shift = v.spread_observation_shift;
    r.flat_include_spread = v.flat_include_spread;
    r.flat_lookback = v.flat_lookback;
    r.flat_fixing_days = v.flat_fixing_days;
    r.flat_rate_cutoff = v.flat_rate_cutoff;
    r.flat_is_averaged = v.flat_is_averaged;
    r.flat_observation_shift = v.flat_observation_shift;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

cross_currency_basis_convention_entity
cross_currency_basis_convention_mapper::map(const domain::cross_currency_basis_convention& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    cross_currency_basis_convention_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.settlement_days = v.settlement_days;
    r.settlement_calendar = v.settlement_calendar;
    r.roll_convention = v.roll_convention;
    r.flat_index = v.flat_index;
    r.spread_index = v.spread_index;
    r.eom = v.eom;
    r.is_resettable = v.is_resettable;
    r.flat_index_is_resettable = v.flat_index_is_resettable;
    r.flat_tenor = v.flat_tenor;
    r.spread_tenor = v.spread_tenor;
    r.spread_payment_lag = v.spread_payment_lag;
    r.flat_payment_lag = v.flat_payment_lag;
    r.spread_include_spread = v.spread_include_spread;
    r.spread_lookback = v.spread_lookback;
    r.spread_fixing_days = v.spread_fixing_days;
    r.spread_rate_cutoff = v.spread_rate_cutoff;
    r.spread_is_averaged = v.spread_is_averaged;
    r.spread_observation_shift = v.spread_observation_shift;
    r.flat_include_spread = v.flat_include_spread;
    r.flat_lookback = v.flat_lookback;
    r.flat_fixing_days = v.flat_fixing_days;
    r.flat_rate_cutoff = v.flat_rate_cutoff;
    r.flat_is_averaged = v.flat_is_averaged;
    r.flat_observation_shift = v.flat_observation_shift;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::cross_currency_basis_convention> cross_currency_basis_convention_mapper::map(
    const std::vector<cross_currency_basis_convention_entity>& v) {
    return map_vector<cross_currency_basis_convention_entity,
                      domain::cross_currency_basis_convention>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<cross_currency_basis_convention_entity> cross_currency_basis_convention_mapper::map(
    const std::vector<domain::cross_currency_basis_convention>& v) {
    return map_vector<domain::cross_currency_basis_convention,
                      cross_currency_basis_convention_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
