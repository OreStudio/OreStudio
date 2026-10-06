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
#include "ores.trading.core/repository/instrument_option_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/instrument_option.hpp"
#include "ores.trading.api/domain/instrument_option_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/instrument_option_entity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::instrument_option instrument_option_mapper::map(const instrument_option_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::instrument_option r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.trade_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_id.value());
    r.long_short = v.long_short;
    r.option_type = v.option_type;
    r.payoff_type = v.payoff_type;
    r.payoff_type_2 = v.payoff_type_2;
    r.style = v.style;
    r.notice_period = v.notice_period;
    r.notice_calendar = v.notice_calendar;
    r.notice_convention = v.notice_convention;
    r.mid_coupon_exercise = v.mid_coupon_exercise;
    r.settlement = v.settlement;
    r.settlement_method = v.settlement_method;
    r.pay_off_at_expiry = v.pay_off_at_expiry;
    r.premium_amount = v.premium_amount;
    r.premium_currency = v.premium_currency;
    r.premium_pay_date = v.premium_pay_date;
    r.exercise_prices = v.exercise_prices;
    r.exercise_fee_settlement_period = v.exercise_fee_settlement_period;
    r.exercise_fee_settlement_calendar = v.exercise_fee_settlement_calendar;
    r.exercise_fee_settlement_convention = v.exercise_fee_settlement_convention;
    r.automatic_exercise = v.automatic_exercise;
    r.has_exercise_data = v.has_exercise_data;
    r.exercise_date =
        v.exercise_date.has_value() ?
            std::optional(ores::platform::time::datetime::from_iso8601_date(*v.exercise_date)) :
            std::nullopt;
    r.exercise_price =
        v.exercise_price.has_value() ?
            std::optional(ores::utility::decimal::decimal::from_string(*v.exercise_price).value()) :
            std::nullopt;
    r.has_payment_data = v.has_payment_data;
    r.payment_lag = v.payment_lag;
    r.payment_calendar = v.payment_calendar;
    r.payment_convention = v.payment_convention;
    r.payment_relative_to = v.payment_relative_to;
    r.has_settlement_data = v.has_settlement_data;
    r.settlement_pay_currency = v.settlement_pay_currency;
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

instrument_option_entity instrument_option_mapper::map(const domain::instrument_option& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    instrument_option_entity r;
    r.trade_id = boost::uuids::to_string(v.trade_id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.long_short = v.long_short;
    r.option_type = v.option_type;
    r.payoff_type = v.payoff_type;
    r.payoff_type_2 = v.payoff_type_2;
    r.style = v.style;
    r.notice_period = v.notice_period;
    r.notice_calendar = v.notice_calendar;
    r.notice_convention = v.notice_convention;
    r.mid_coupon_exercise = v.mid_coupon_exercise;
    r.settlement = v.settlement;
    r.settlement_method = v.settlement_method;
    r.pay_off_at_expiry = v.pay_off_at_expiry;
    r.premium_amount = v.premium_amount;
    r.premium_currency = v.premium_currency;
    r.premium_pay_date = v.premium_pay_date;
    r.exercise_prices = v.exercise_prices;
    r.exercise_fee_settlement_period = v.exercise_fee_settlement_period;
    r.exercise_fee_settlement_calendar = v.exercise_fee_settlement_calendar;
    r.exercise_fee_settlement_convention = v.exercise_fee_settlement_convention;
    r.automatic_exercise = v.automatic_exercise;
    r.has_exercise_data = v.has_exercise_data;
    r.exercise_date =
        v.exercise_date.has_value() ?
            std::optional(ores::platform::time::datetime::to_iso8601_date(*v.exercise_date)) :
            std::nullopt;
    r.exercise_price =
        v.exercise_price.has_value() ? std::optional(v.exercise_price->to_string()) : std::nullopt;
    r.has_payment_data = v.has_payment_data;
    r.payment_lag = v.payment_lag;
    r.payment_calendar = v.payment_calendar;
    r.payment_convention = v.payment_convention;
    r.payment_relative_to = v.payment_relative_to;
    r.has_settlement_data = v.has_settlement_data;
    r.settlement_pay_currency = v.settlement_pay_currency;
    r.settlement_fx_index = v.settlement_fx_index;
    r.settlement_fixing_date = v.settlement_fixing_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::instrument_option>
instrument_option_mapper::map(const std::vector<instrument_option_entity>& v) {
    return map_vector<instrument_option_entity, domain::instrument_option>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<instrument_option_entity>
instrument_option_mapper::map(const std::vector<domain::instrument_option>& v) {
    return map_vector<domain::instrument_option, instrument_option_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
