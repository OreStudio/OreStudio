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
#include "ores.trading.core/repository/commodity_instrument_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/commodity_instrument_json_io.hpp" // IWYU pragma: keep.

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::commodity_instrument
commodity_instrument_mapper::map(const commodity_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::commodity_instrument r;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.version = v.version;
    r.trade_id = v.trade_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.trade_id)) : std::nullopt;
    r.trade_type_code = v.trade_type_code;
    r.commodity_code = v.commodity_code;
    r.currency = v.currency;
    r.quantity = v.quantity;
    r.unit = v.unit;
    r.start_date = v.start_date.value_or("");
    r.maturity_date = v.maturity_date.value_or("");
    r.fixed_price = v.fixed_price;
    r.option_type = v.option_type.value_or("");
    r.strike_price = v.strike_price;
    r.exercise_type = v.exercise_type.value_or("");
    r.average_type = v.average_type.value_or("");
    r.averaging_start_date = v.averaging_start_date.value_or("");
    r.averaging_end_date = v.averaging_end_date.value_or("");
    r.spread_commodity_code = v.spread_commodity_code.value_or("");
    r.spread_amount = v.spread_amount;
    r.strip_frequency_code = v.strip_frequency_code.value_or("");
    r.variance_strike = v.variance_strike;
    r.accumulation_amount = v.accumulation_amount;
    r.knock_out_barrier = v.knock_out_barrier;
    r.barrier_type = v.barrier_type.value_or("");
    r.lower_barrier = v.lower_barrier;
    r.upper_barrier = v.upper_barrier;
    r.basket_json = v.basket_json.value_or("");
    r.day_count_code = v.day_count_code.value_or("");
    r.payment_frequency_code = v.payment_frequency_code.value_or("");
    r.swaption_expiry_date = v.swaption_expiry_date.value_or("");
    r.description = v.description.value_or("");
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

commodity_instrument_entity
commodity_instrument_mapper::map(const domain::commodity_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    commodity_instrument_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.trade_id = v.trade_id.has_value() ? std::optional(boost::uuids::to_string(*v.trade_id)) : std::nullopt;
    r.trade_type_code = v.trade_type_code;
    r.commodity_code = v.commodity_code;
    r.currency = v.currency;
    r.quantity = v.quantity;
    r.unit = v.unit;
    r.start_date = v.start_date.empty() ? std::nullopt : std::optional(v.start_date);
    r.maturity_date = v.maturity_date.empty() ? std::nullopt : std::optional(v.maturity_date);
    r.fixed_price = v.fixed_price;
    r.option_type = v.option_type.empty() ? std::nullopt : std::optional(v.option_type);
    r.strike_price = v.strike_price;
    r.exercise_type = v.exercise_type.empty() ? std::nullopt : std::optional(v.exercise_type);
    r.average_type = v.average_type.empty() ? std::nullopt : std::optional(v.average_type);
    r.averaging_start_date = v.averaging_start_date.empty() ? std::nullopt : std::optional(v.averaging_start_date);
    r.averaging_end_date = v.averaging_end_date.empty() ? std::nullopt : std::optional(v.averaging_end_date);
    r.spread_commodity_code = v.spread_commodity_code.empty() ? std::nullopt : std::optional(v.spread_commodity_code);
    r.spread_amount = v.spread_amount;
    r.strip_frequency_code = v.strip_frequency_code.empty() ? std::nullopt : std::optional(v.strip_frequency_code);
    r.variance_strike = v.variance_strike;
    r.accumulation_amount = v.accumulation_amount;
    r.knock_out_barrier = v.knock_out_barrier;
    r.barrier_type = v.barrier_type.empty() ? std::nullopt : std::optional(v.barrier_type);
    r.lower_barrier = v.lower_barrier;
    r.upper_barrier = v.upper_barrier;
    r.basket_json = v.basket_json.empty() ? std::nullopt : std::optional(v.basket_json);
    r.day_count_code = v.day_count_code.empty() ? std::nullopt : std::optional(v.day_count_code);
    r.payment_frequency_code = v.payment_frequency_code.empty() ? std::nullopt : std::optional(v.payment_frequency_code);
    r.swaption_expiry_date = v.swaption_expiry_date.empty() ? std::nullopt : std::optional(v.swaption_expiry_date);
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::commodity_instrument>
commodity_instrument_mapper::map(const std::vector<commodity_instrument_entity>& v) {
    return map_vector<commodity_instrument_entity, domain::commodity_instrument>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<commodity_instrument_entity>
commodity_instrument_mapper::map(const std::vector<domain::commodity_instrument>& v) {
    return map_vector<domain::commodity_instrument, commodity_instrument_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
