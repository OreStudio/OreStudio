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
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/commodity_instrument_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::commodity_instrument
commodity_instrument_mapper::map(const commodity_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::commodity_instrument r;
    r.identity.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.identity.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.identity.workspace_id = boost::lexical_cast<boost::uuids::uuid>(v.workspace_id);
    r.identity.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.identity.version = v.version;
    r.identity.trade_id = v.trade_id.has_value() ?
                              std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.trade_id)) :
                              std::nullopt;
    r.identity.trade_type_code = v.trade_type_code;
    r.terms.commodity_code = v.commodity_code;
    r.terms.currency = v.currency;
    r.terms.quantity = v.quantity;
    r.terms.unit = v.unit;
    r.terms.start_date = v.start_date.value_or("");
    r.terms.maturity_date = v.maturity_date.value_or("");
    r.terms.fixed_price = v.fixed_price;
    r.terms.day_count_code = v.day_count_code.value_or("");
    r.terms.payment_frequency_code = v.payment_frequency_code.value_or("");
    r.option.option_type = v.option_type.value_or("");
    r.option.strike_price = v.strike_price;
    r.option.exercise_type = v.exercise_type.value_or("");
    r.option.swaption_expiry_date = v.swaption_expiry_date.value_or("");
    r.pricing.average_type = v.average_type.value_or("");
    r.pricing.averaging_start_date = v.averaging_start_date.value_or("");
    r.pricing.averaging_end_date = v.averaging_end_date.value_or("");
    r.pricing.spread_commodity_code = v.spread_commodity_code.value_or("");
    r.pricing.spread_amount = v.spread_amount;
    r.pricing.strip_frequency_code = v.strip_frequency_code.value_or("");
    r.exotic.variance_strike = v.variance_strike;
    r.exotic.accumulation_amount = v.accumulation_amount;
    r.exotic.knock_out_barrier = v.knock_out_barrier;
    r.exotic.barrier_type = v.barrier_type.value_or("");
    r.exotic.lower_barrier = v.lower_barrier;
    r.exotic.upper_barrier = v.upper_barrier;
    r.exotic.basket_json = v.basket_json.value_or("");
    r.description = v.description.value_or("");
    r.audit.modified_by = v.modified_by;
    r.audit.performed_by = v.performed_by;
    r.audit.change_reason_code = v.change_reason_code;
    r.audit.change_commentary = v.change_commentary;
    r.audit.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

commodity_instrument_entity
commodity_instrument_mapper::map(const domain::commodity_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    commodity_instrument_entity r;
    r.id = boost::uuids::to_string(v.identity.instrument_id);
    r.tenant_id = v.identity.tenant_id.to_string();
    r.workspace_id = boost::uuids::to_string(v.identity.workspace_id);
    r.party_id = boost::uuids::to_string(v.identity.party_id);
    r.version = v.identity.version;
    r.trade_id = v.identity.trade_id.has_value() ?
                     std::optional(boost::uuids::to_string(*v.identity.trade_id)) :
                     std::nullopt;
    r.trade_type_code = v.identity.trade_type_code;
    r.commodity_code = v.terms.commodity_code;
    r.currency = v.terms.currency;
    r.quantity = v.terms.quantity;
    r.unit = v.terms.unit;
    r.start_date = v.terms.start_date.empty() ? std::nullopt : std::optional(v.terms.start_date);
    r.maturity_date =
        v.terms.maturity_date.empty() ? std::nullopt : std::optional(v.terms.maturity_date);
    r.fixed_price = v.terms.fixed_price;
    r.day_count_code =
        v.terms.day_count_code.empty() ? std::nullopt : std::optional(v.terms.day_count_code);
    r.payment_frequency_code = v.terms.payment_frequency_code.empty() ?
                                   std::nullopt :
                                   std::optional(v.terms.payment_frequency_code);
    r.option_type =
        v.option.option_type.empty() ? std::nullopt : std::optional(v.option.option_type);
    r.strike_price = v.option.strike_price;
    r.exercise_type =
        v.option.exercise_type.empty() ? std::nullopt : std::optional(v.option.exercise_type);
    r.swaption_expiry_date = v.option.swaption_expiry_date.empty() ?
                                 std::nullopt :
                                 std::optional(v.option.swaption_expiry_date);
    r.average_type =
        v.pricing.average_type.empty() ? std::nullopt : std::optional(v.pricing.average_type);
    r.averaging_start_date = v.pricing.averaging_start_date.empty() ?
                                 std::nullopt :
                                 std::optional(v.pricing.averaging_start_date);
    r.averaging_end_date = v.pricing.averaging_end_date.empty() ?
                               std::nullopt :
                               std::optional(v.pricing.averaging_end_date);
    r.spread_commodity_code = v.pricing.spread_commodity_code.empty() ?
                                  std::nullopt :
                                  std::optional(v.pricing.spread_commodity_code);
    r.spread_amount = v.pricing.spread_amount;
    r.strip_frequency_code = v.pricing.strip_frequency_code.empty() ?
                                 std::nullopt :
                                 std::optional(v.pricing.strip_frequency_code);
    r.variance_strike = v.exotic.variance_strike;
    r.accumulation_amount = v.exotic.accumulation_amount;
    r.knock_out_barrier = v.exotic.knock_out_barrier;
    r.barrier_type =
        v.exotic.barrier_type.empty() ? std::nullopt : std::optional(v.exotic.barrier_type);
    r.lower_barrier = v.exotic.lower_barrier;
    r.upper_barrier = v.exotic.upper_barrier;
    r.basket_json =
        v.exotic.basket_json.empty() ? std::nullopt : std::optional(v.exotic.basket_json);
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.audit.modified_by;
    r.performed_by = v.audit.performed_by;
    r.change_reason_code = v.audit.change_reason_code;
    r.change_commentary = v.audit.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::commodity_instrument>
commodity_instrument_mapper::map(const std::vector<commodity_instrument_entity>& v) {
    return map_vector<commodity_instrument_entity, domain::commodity_instrument>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<commodity_instrument_entity>
commodity_instrument_mapper::map(const std::vector<domain::commodity_instrument>& v) {
    return map_vector<domain::commodity_instrument, commodity_instrument_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
