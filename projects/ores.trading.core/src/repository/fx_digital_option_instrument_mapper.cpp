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
#include "ores.trading.core/repository/fx_digital_option_instrument_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/fx_digital_option_instrument_json_io.hpp" // IWYU pragma: keep.

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::fx_digital_option_instrument
fx_digital_option_instrument_mapper::map(const fx_digital_option_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::fx_digital_option_instrument r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.instrument_id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.trade_id = v.trade_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.trade_id)) : std::nullopt;
    r.trade_type_code = v.trade_type_code;
    r.foreign_currency = v.foreign_currency;
    r.domestic_currency = v.domestic_currency;
    r.payoff_currency = v.payoff_currency;
    r.payoff_amount = v.payoff_amount;
    r.option_type = v.option_type.value_or("");
    r.expiry_date = v.expiry_date;
    r.long_short = v.long_short;
    r.strike = v.strike;
    r.barrier_type = v.barrier_type.value_or("");
    r.lower_barrier = v.lower_barrier;
    r.upper_barrier = v.upper_barrier;
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

fx_digital_option_instrument_entity
fx_digital_option_instrument_mapper::map(const domain::fx_digital_option_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    fx_digital_option_instrument_entity r;
    r.instrument_id = boost::uuids::to_string(v.instrument_id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.trade_id = v.trade_id.has_value() ? std::optional(boost::uuids::to_string(*v.trade_id)) : std::nullopt;
    r.trade_type_code = v.trade_type_code;
    r.foreign_currency = v.foreign_currency;
    r.domestic_currency = v.domestic_currency;
    r.payoff_currency = v.payoff_currency;
    r.payoff_amount = v.payoff_amount;
    r.option_type = v.option_type.empty() ? std::nullopt : std::optional(v.option_type);
    r.expiry_date = v.expiry_date;
    r.long_short = v.long_short;
    r.strike = v.strike;
    r.barrier_type = v.barrier_type.empty() ? std::nullopt : std::optional(v.barrier_type);
    r.lower_barrier = v.lower_barrier;
    r.upper_barrier = v.upper_barrier;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::fx_digital_option_instrument>
fx_digital_option_instrument_mapper::map(const std::vector<fx_digital_option_instrument_entity>& v) {
    return map_vector<fx_digital_option_instrument_entity, domain::fx_digital_option_instrument>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<fx_digital_option_instrument_entity>
fx_digital_option_instrument_mapper::map(const std::vector<domain::fx_digital_option_instrument>& v) {
    return map_vector<domain::fx_digital_option_instrument, fx_digital_option_instrument_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
