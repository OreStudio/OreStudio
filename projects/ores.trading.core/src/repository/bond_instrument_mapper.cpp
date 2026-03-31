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
#include "ores.trading.core/repository/bond_instrument_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/bond_instrument_json_io.hpp" // IWYU pragma: keep.

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::bond_instrument
bond_instrument_mapper::map(const bond_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::bond_instrument r;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.version = v.version;
    r.trade_id = v.trade_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.trade_id)) : std::nullopt;
    r.trade_type_code = v.trade_type_code;
    r.issuer = v.issuer;
    r.currency = v.currency;
    r.face_value = v.face_value;
    r.coupon_rate = v.coupon_rate;
    r.coupon_frequency_code = v.coupon_frequency_code;
    r.day_count_code = v.day_count_code;
    r.issue_date = v.issue_date;
    r.maturity_date = v.maturity_date;
    r.settlement_days = v.settlement_days.value_or(0);
    r.call_date = v.call_date.value_or("");
    r.conversion_ratio = v.conversion_ratio.value_or(0.0);
    r.description = v.description.value_or("");
    r.future_expiry_date = v.future_expiry_date.value_or("");
    r.option_type = v.option_type.value_or("");
    r.option_expiry_date = v.option_expiry_date.value_or("");
    r.option_strike = v.option_strike;
    r.trs_return_type = v.trs_return_type.value_or("");
    r.trs_funding_leg_code = v.trs_funding_leg_code.value_or("");
    r.ascot_option_type = v.ascot_option_type.value_or("");
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

bond_instrument_entity
bond_instrument_mapper::map(const domain::bond_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    bond_instrument_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.trade_id = v.trade_id.has_value() ? std::optional(boost::uuids::to_string(*v.trade_id)) : std::nullopt;
    r.trade_type_code = v.trade_type_code;
    r.issuer = v.issuer;
    r.currency = v.currency;
    r.face_value = v.face_value;
    r.coupon_rate = v.coupon_rate;
    r.coupon_frequency_code = v.coupon_frequency_code;
    r.day_count_code = v.day_count_code;
    r.issue_date = v.issue_date;
    r.maturity_date = v.maturity_date;
    r.settlement_days = v.settlement_days == 0
        ? std::nullopt : std::optional(v.settlement_days);
    r.call_date = v.call_date.empty()
        ? std::nullopt : std::optional(v.call_date);
    r.conversion_ratio = v.conversion_ratio == 0.0
        ? std::nullopt : std::optional(v.conversion_ratio);
    r.description = v.description.empty()
        ? std::nullopt : std::optional(v.description);
    r.future_expiry_date = v.future_expiry_date.empty()
        ? std::nullopt : std::optional(v.future_expiry_date);
    r.option_type = v.option_type.empty()
        ? std::nullopt : std::optional(v.option_type);
    r.option_expiry_date = v.option_expiry_date.empty()
        ? std::nullopt : std::optional(v.option_expiry_date);
    r.option_strike = v.option_strike;
    r.trs_return_type = v.trs_return_type.empty()
        ? std::nullopt : std::optional(v.trs_return_type);
    r.trs_funding_leg_code = v.trs_funding_leg_code.empty()
        ? std::nullopt : std::optional(v.trs_funding_leg_code);
    r.ascot_option_type = v.ascot_option_type.empty()
        ? std::nullopt : std::optional(v.ascot_option_type);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::bond_instrument>
bond_instrument_mapper::map(const std::vector<bond_instrument_entity>& v) {
    return map_vector<bond_instrument_entity, domain::bond_instrument>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<bond_instrument_entity>
bond_instrument_mapper::map(const std::vector<domain::bond_instrument>& v) {
    return map_vector<domain::bond_instrument, bond_instrument_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
