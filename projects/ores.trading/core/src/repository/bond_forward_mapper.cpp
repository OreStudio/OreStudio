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
#include "ores.trading.core/repository/bond_forward_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/bond_forward_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::bond_forward bond_forward_mapper::map(const bond_forward_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::bond_forward r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.instrument_id.value());
    r.long_in_forward = v.long_in_forward;
    r.forward_maturity_date = v.forward_maturity_date;
    r.forward_settlement_date = v.forward_settlement_date;
    r.settlement = v.settlement;
    r.amount = v.amount;
    r.lock_rate = v.lock_rate;
    r.dv01 = v.dv01;
    r.lock_rate_day_counter = v.lock_rate_day_counter;
    r.settlement_dirty = v.settlement_dirty;
    r.premium_amount = v.premium_amount;
    r.premium_date = v.premium_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

bond_forward_entity bond_forward_mapper::map(const domain::bond_forward& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    bond_forward_entity r;
    r.instrument_id = boost::uuids::to_string(v.instrument_id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.long_in_forward = v.long_in_forward;
    r.forward_maturity_date = v.forward_maturity_date;
    r.forward_settlement_date = v.forward_settlement_date;
    r.settlement = v.settlement;
    r.amount = v.amount;
    r.lock_rate = v.lock_rate;
    r.dv01 = v.dv01;
    r.lock_rate_day_counter = v.lock_rate_day_counter;
    r.settlement_dirty = v.settlement_dirty;
    r.premium_amount = v.premium_amount;
    r.premium_date = v.premium_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::bond_forward>
bond_forward_mapper::map(const std::vector<bond_forward_entity>& v) {
    return map_vector<bond_forward_entity, domain::bond_forward>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<bond_forward_entity>
bond_forward_mapper::map(const std::vector<domain::bond_forward>& v) {
    return map_vector<domain::bond_forward, bond_forward_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
