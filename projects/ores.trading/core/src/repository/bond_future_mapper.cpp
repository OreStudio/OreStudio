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
#include "ores.trading.core/repository/bond_future_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/bond_future_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::bond_future bond_future_mapper::map(const bond_future_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::bond_future r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.instrument_id.value());
    r.contract_name = v.contract_name;
    r.contract_notional = v.contract_notional;
    r.long_short = v.long_short;
    r.currency = v.currency;
    r.contract_month = v.contract_month;
    r.deliverable_grade = v.deliverable_grade.value_or("");
    r.fair_price = v.fair_price;
    r.settlement = v.settlement;
    r.settlement_dirty = v.settlement_dirty;
    r.root_date = v.root_date.value_or("");
    r.expiry_basis = v.expiry_basis.value_or("");
    r.settlement_basis = v.settlement_basis.value_or("");
    r.expiry_lag = v.expiry_lag;
    r.settlement_lag = v.settlement_lag;
    r.last_trading_date = v.last_trading_date;
    r.last_delivery_date = v.last_delivery_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

bond_future_entity bond_future_mapper::map(const domain::bond_future& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    bond_future_entity r;
    r.instrument_id = boost::uuids::to_string(v.instrument_id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.contract_name = v.contract_name;
    r.contract_notional = v.contract_notional;
    r.long_short = v.long_short;
    r.currency = v.currency;
    r.contract_month = v.contract_month;
    r.deliverable_grade =
        v.deliverable_grade.empty() ? std::nullopt : std::optional(v.deliverable_grade);
    r.fair_price = v.fair_price;
    r.settlement = v.settlement;
    r.settlement_dirty = v.settlement_dirty;
    r.root_date = v.root_date.empty() ? std::nullopt : std::optional(v.root_date);
    r.expiry_basis = v.expiry_basis.empty() ? std::nullopt : std::optional(v.expiry_basis);
    r.settlement_basis =
        v.settlement_basis.empty() ? std::nullopt : std::optional(v.settlement_basis);
    r.expiry_lag = v.expiry_lag;
    r.settlement_lag = v.settlement_lag;
    r.last_trading_date = v.last_trading_date;
    r.last_delivery_date = v.last_delivery_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::bond_future> bond_future_mapper::map(const std::vector<bond_future_entity>& v) {
    return map_vector<bond_future_entity, domain::bond_future>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<bond_future_entity> bond_future_mapper::map(const std::vector<domain::bond_future>& v) {
    return map_vector<domain::bond_future, bond_future_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
