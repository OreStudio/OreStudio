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
#include "ores.trading.core/repository/bond_leg_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/bond_leg_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::bond_leg bond_leg_mapper::map(const bond_leg_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::bond_leg r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.instrument_id.value());
    r.leg_role = v.leg_role.value();
    r.leg_number = boost::lexical_cast<int>(v.leg_number.value());
    r.payer = v.payer;
    r.leg_type = v.leg_type;
    r.currency = v.currency;
    r.payment_convention = v.payment_convention;
    r.payment_lag = v.payment_lag;
    r.payment_calendar = v.payment_calendar;
    r.day_counter = v.day_counter;
    r.last_period_day_counter = v.last_period_day_counter;
    r.notional_payment_lag = v.notional_payment_lag;
    r.strict_notional_dates = v.strict_notional_dates;
    r.indexings_from_asset_leg = v.indexings_from_asset_leg;
    r.settlement_fx_index = v.settlement_fx_index;
    r.settlement_fixing_date = v.settlement_fixing_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

bond_leg_entity bond_leg_mapper::map(const domain::bond_leg& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    bond_leg_entity r;
    r.instrument_id = boost::uuids::to_string(v.instrument_id);
    r.leg_role = v.leg_role;
    r.leg_number = std::to_string(v.leg_number);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.payer = v.payer;
    r.leg_type = v.leg_type;
    r.currency = v.currency;
    r.payment_convention = v.payment_convention;
    r.payment_lag = v.payment_lag;
    r.payment_calendar = v.payment_calendar;
    r.day_counter = v.day_counter;
    r.last_period_day_counter = v.last_period_day_counter;
    r.notional_payment_lag = v.notional_payment_lag;
    r.strict_notional_dates = v.strict_notional_dates;
    r.indexings_from_asset_leg = v.indexings_from_asset_leg;
    r.settlement_fx_index = v.settlement_fx_index;
    r.settlement_fixing_date = v.settlement_fixing_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::bond_leg> bond_leg_mapper::map(const std::vector<bond_leg_entity>& v) {
    return map_vector<bond_leg_entity, domain::bond_leg>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<bond_leg_entity> bond_leg_mapper::map(const std::vector<domain::bond_leg>& v) {
    return map_vector<domain::bond_leg, bond_leg_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
