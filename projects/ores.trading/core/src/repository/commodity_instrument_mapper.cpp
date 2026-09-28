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
#include "ores.trading.core/repository/commodity_instrument_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/commodity_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.utility/decimal/decimal.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::commodity_instrument
commodity_instrument_mapper::map(const commodity_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::commodity_instrument r;
    r.identity.version = v.version;
    r.identity.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.identity.workspace_id = boost::lexical_cast<boost::uuids::uuid>(v.workspace_id);
    r.identity.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.instrument_id.value());
    r.identity.trade_type_code = v.trade_type_code;
    r.identity.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.identity.trade_id = v.trade_id.has_value() ?
                              std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.trade_id)) :
                              std::nullopt;
    r.commodity_code = v.commodity_code;
    r.currency = v.currency;
    r.quantity = v.quantity;
    r.unit = v.unit;
    r.start_date =
        v.start_date.has_value() ?
            std::optional(ores::platform::time::datetime::from_iso8601_date(*v.start_date)) :
            std::nullopt;
    r.maturity_date =
        v.maturity_date.has_value() ?
            std::optional(ores::platform::time::datetime::from_iso8601_date(*v.maturity_date)) :
            std::nullopt;
    r.fixed_price =
        v.fixed_price.has_value() ?
            std::optional(ores::utility::decimal::decimal::from_string(*v.fixed_price).value()) :
            std::nullopt;
    r.option_type = v.option_type.value_or("");
    r.strike_price =
        v.strike_price.has_value() ?
            std::optional(ores::utility::decimal::decimal::from_string(*v.strike_price).value()) :
            std::nullopt;
    r.exercise_type = v.exercise_type.value_or("");
    r.average_type = v.average_type.value_or("");
    r.averaging_start_date = v.averaging_start_date.has_value() ?
                                 std::optional(ores::platform::time::datetime::from_iso8601_date(
                                     *v.averaging_start_date)) :
                                 std::nullopt;
    r.averaging_end_date = v.averaging_end_date.has_value() ?
                               std::optional(ores::platform::time::datetime::from_iso8601_date(
                                   *v.averaging_end_date)) :
                               std::nullopt;
    r.spread_commodity_code = v.spread_commodity_code.value_or("");
    r.spread_amount =
        v.spread_amount.has_value() ?
            std::optional(ores::utility::decimal::decimal::from_string(*v.spread_amount).value()) :
            std::nullopt;
    r.strip_frequency_code = v.strip_frequency_code.value_or("");
    r.variance_strike = v.variance_strike;
    r.accumulation_amount =
        v.accumulation_amount.has_value() ?
            std::optional(
                ores::utility::decimal::decimal::from_string(*v.accumulation_amount).value()) :
            std::nullopt;
    r.knock_out_barrier =
        v.knock_out_barrier.has_value() ?
            std::optional(
                ores::utility::decimal::decimal::from_string(*v.knock_out_barrier).value()) :
            std::nullopt;
    r.barrier_type = v.barrier_type.value_or("");
    r.lower_barrier =
        v.lower_barrier.has_value() ?
            std::optional(ores::utility::decimal::decimal::from_string(*v.lower_barrier).value()) :
            std::nullopt;
    r.upper_barrier =
        v.upper_barrier.has_value() ?
            std::optional(ores::utility::decimal::decimal::from_string(*v.upper_barrier).value()) :
            std::nullopt;
    r.day_count_fraction_code = v.day_count_fraction_code.value_or("");
    r.payment_frequency_code = v.payment_frequency_code.value_or("");
    r.swaption_expiry_date = v.swaption_expiry_date.has_value() ?
                                 std::optional(ores::platform::time::datetime::from_iso8601_date(
                                     *v.swaption_expiry_date)) :
                                 std::nullopt;
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
    r.instrument_id = boost::uuids::to_string(v.identity.instrument_id);
    r.tenant_id = v.identity.tenant_id.to_string();
    r.workspace_id = boost::uuids::to_string(v.identity.workspace_id);
    r.version = v.identity.version;
    r.trade_type_code = v.identity.trade_type_code;
    r.party_id = boost::uuids::to_string(v.identity.party_id);
    r.trade_id = v.identity.trade_id.has_value() ?
                     std::optional(boost::uuids::to_string(*v.identity.trade_id)) :
                     std::nullopt;
    r.commodity_code = v.commodity_code;
    r.currency = v.currency;
    r.quantity = v.quantity;
    r.unit = v.unit;
    r.start_date =
        v.start_date.has_value() ?
            std::optional(ores::platform::time::datetime::to_iso8601_date(*v.start_date)) :
            std::nullopt;
    r.maturity_date =
        v.maturity_date.has_value() ?
            std::optional(ores::platform::time::datetime::to_iso8601_date(*v.maturity_date)) :
            std::nullopt;
    r.fixed_price =
        v.fixed_price.has_value() ? std::optional(v.fixed_price->to_string()) : std::nullopt;
    r.option_type = v.option_type.empty() ? std::nullopt : std::optional(v.option_type);
    r.strike_price =
        v.strike_price.has_value() ? std::optional(v.strike_price->to_string()) : std::nullopt;
    r.exercise_type = v.exercise_type.empty() ? std::nullopt : std::optional(v.exercise_type);
    r.average_type = v.average_type.empty() ? std::nullopt : std::optional(v.average_type);
    r.averaging_start_date = v.averaging_start_date.has_value() ?
                                 std::optional(ores::platform::time::datetime::to_iso8601_date(
                                     *v.averaging_start_date)) :
                                 std::nullopt;
    r.averaging_end_date =
        v.averaging_end_date.has_value() ?
            std::optional(ores::platform::time::datetime::to_iso8601_date(*v.averaging_end_date)) :
            std::nullopt;
    r.spread_commodity_code =
        v.spread_commodity_code.empty() ? std::nullopt : std::optional(v.spread_commodity_code);
    r.spread_amount =
        v.spread_amount.has_value() ? std::optional(v.spread_amount->to_string()) : std::nullopt;
    r.strip_frequency_code =
        v.strip_frequency_code.empty() ? std::nullopt : std::optional(v.strip_frequency_code);
    r.variance_strike = v.variance_strike;
    r.accumulation_amount = v.accumulation_amount.has_value() ?
                                std::optional(v.accumulation_amount->to_string()) :
                                std::nullopt;
    r.knock_out_barrier = v.knock_out_barrier.has_value() ?
                              std::optional(v.knock_out_barrier->to_string()) :
                              std::nullopt;
    r.barrier_type = v.barrier_type.empty() ? std::nullopt : std::optional(v.barrier_type);
    r.lower_barrier =
        v.lower_barrier.has_value() ? std::optional(v.lower_barrier->to_string()) : std::nullopt;
    r.upper_barrier =
        v.upper_barrier.has_value() ? std::optional(v.upper_barrier->to_string()) : std::nullopt;
    r.day_count_fraction_code =
        v.day_count_fraction_code.empty() ? std::nullopt : std::optional(v.day_count_fraction_code);
    r.payment_frequency_code =
        v.payment_frequency_code.empty() ? std::nullopt : std::optional(v.payment_frequency_code);
    r.swaption_expiry_date = v.swaption_expiry_date.has_value() ?
                                 std::optional(ores::platform::time::datetime::to_iso8601_date(
                                     *v.swaption_expiry_date)) :
                                 std::nullopt;
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
