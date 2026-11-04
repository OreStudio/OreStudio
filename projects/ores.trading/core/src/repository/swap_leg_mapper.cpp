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
#include "ores.trading.core/repository/swap_leg_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/swap_leg_entity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::swap_leg swap_leg_mapper::map(const swap_leg_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::swap_leg r;
    r.identity.version = v.version;
    r.identity.tenant_id = v.tenant_id;
    r.identity.trade_id = v.trade_id;
    r.identity.leg_number = v.leg_number;
    r.identity.party_id = v.party_id;
    r.identity.trade_activity_id = v.trade_activity_id;
    r.payer = v.payer;
    r.leg_type_code = v.leg_type_code;
    r.day_count_fraction_code = v.day_count_fraction_code;
    r.business_day_convention_code = v.business_day_convention_code;
    r.payment_frequency_code = v.payment_frequency_code;
    r.floating_index_code = v.floating_index_code.value_or("");
    r.currency = v.currency;
    r.leg_group = v.leg_group;
    r.fixing_days = v.fixing_days;
    r.is_in_arrears = v.is_in_arrears;
    r.include_spread = v.include_spread;
    r.is_averaged = v.is_averaged;
    r.has_sub_periods = v.has_sub_periods;
    r.notional_initial_exchange = v.notional_initial_exchange;
    r.notional_final_exchange = v.notional_final_exchange;
    r.payment_lag = v.payment_lag;
    r.payment_calendar = v.payment_calendar;
    r.commodity_price_type = v.commodity_price_type;
    r.future_month_offset = v.future_month_offset;
    r.daily_expiry_offset = v.daily_expiry_offset;
    r.pricing_calendar = v.pricing_calendar;
    r.equity_return_type = v.equity_return_type;
    r.equity_name = v.equity_name;
    r.equity_underlying_type = v.equity_underlying_type;
    r.equity_underlying_identifier_type = v.equity_underlying_identifier_type;
    r.equity_quantity = v.equity_quantity;
    r.equity_initial_price = v.equity_initial_price;
    r.equity_initial_price_currency = v.equity_initial_price_currency;
    r.equity_dividend_factor = v.equity_dividend_factor;
    r.equity_notional_reset = v.equity_notional_reset;
    r.equity_currency = v.equity_currency;
    r.fx_index = v.fx_index;
    r.indexings_from_asset_leg = v.indexings_from_asset_leg;
    r.audit.modified_by = v.modified_by;
    r.audit.performed_by = v.performed_by;
    r.audit.change_reason_code = v.change_reason_code;
    r.audit.change_commentary = v.change_commentary;
    r.audit.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

swap_leg_entity swap_leg_mapper::map(const domain::swap_leg& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    swap_leg_entity r;
    r.trade_id = v.identity.trade_id;
    r.leg_number = v.identity.leg_number;
    r.tenant_id = v.identity.tenant_id;
    r.version = v.identity.version;
    r.party_id = v.identity.party_id;
    r.trade_activity_id = v.identity.trade_activity_id;
    r.payer = v.payer;
    r.leg_type_code = v.leg_type_code;
    r.day_count_fraction_code = v.day_count_fraction_code;
    r.business_day_convention_code = v.business_day_convention_code;
    r.payment_frequency_code = v.payment_frequency_code;
    r.floating_index_code =
        v.floating_index_code.empty() ? std::nullopt : std::optional(v.floating_index_code);
    r.currency = v.currency;
    r.leg_group = v.leg_group;
    r.fixing_days = v.fixing_days;
    r.is_in_arrears = v.is_in_arrears;
    r.include_spread = v.include_spread;
    r.is_averaged = v.is_averaged;
    r.has_sub_periods = v.has_sub_periods;
    r.notional_initial_exchange = v.notional_initial_exchange;
    r.notional_final_exchange = v.notional_final_exchange;
    r.payment_lag = v.payment_lag;
    r.payment_calendar = v.payment_calendar;
    r.commodity_price_type = v.commodity_price_type;
    r.future_month_offset = v.future_month_offset;
    r.daily_expiry_offset = v.daily_expiry_offset;
    r.pricing_calendar = v.pricing_calendar;
    r.equity_return_type = v.equity_return_type;
    r.equity_name = v.equity_name;
    r.equity_underlying_type = v.equity_underlying_type;
    r.equity_underlying_identifier_type = v.equity_underlying_identifier_type;
    r.equity_quantity = v.equity_quantity;
    r.equity_initial_price = v.equity_initial_price;
    r.equity_initial_price_currency = v.equity_initial_price_currency;
    r.equity_dividend_factor = v.equity_dividend_factor;
    r.equity_notional_reset = v.equity_notional_reset;
    r.equity_currency = v.equity_currency;
    r.fx_index = v.fx_index;
    r.indexings_from_asset_leg = v.indexings_from_asset_leg;
    r.modified_by = v.audit.modified_by;
    r.performed_by = v.audit.performed_by;
    r.change_reason_code = v.audit.change_reason_code;
    r.change_commentary = v.audit.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::swap_leg> swap_leg_mapper::map(const std::vector<swap_leg_entity>& v) {
    return map_vector<swap_leg_entity, domain::swap_leg>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<swap_leg_entity> swap_leg_mapper::map(const std::vector<domain::swap_leg>& v) {
    return map_vector<domain::swap_leg, swap_leg_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
