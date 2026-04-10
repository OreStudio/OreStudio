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
#include "ores.trading.core/repository/equity_accumulator_instrument_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/equity_accumulator_instrument_json_io.hpp" // IWYU pragma: keep.

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::equity_accumulator_instrument
equity_accumulator_instrument_mapper::map(const equity_accumulator_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::equity_accumulator_instrument r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.instrument_id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.trade_id = v.trade_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.trade_id)) : std::nullopt;
    r.trade_type_code = v.trade_type_code;
    r.underlying_name = v.underlying_name;
    r.currency = v.currency;
    r.strike = v.strike;
    r.fixing_amount = v.fixing_amount;
    r.start_date = v.start_date;
    r.expiry_date = v.expiry_date;
    r.fixing_frequency = v.fixing_frequency;
    r.long_short = v.long_short;
    r.knock_out_level = v.knock_out_level;
    r.target_amount = v.target_amount;
    r.target_type = v.target_type.value_or("");
    r.payoff_type = v.payoff_type;
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

equity_accumulator_instrument_entity
equity_accumulator_instrument_mapper::map(const domain::equity_accumulator_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    equity_accumulator_instrument_entity r;
    r.instrument_id = boost::uuids::to_string(v.instrument_id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.trade_id = v.trade_id.has_value() ? std::optional(boost::uuids::to_string(*v.trade_id)) : std::nullopt;
    r.trade_type_code = v.trade_type_code;
    r.underlying_name = v.underlying_name;
    r.currency = v.currency;
    r.strike = v.strike;
    r.fixing_amount = v.fixing_amount;
    r.start_date = v.start_date;
    r.expiry_date = v.expiry_date;
    r.fixing_frequency = v.fixing_frequency;
    r.long_short = v.long_short;
    r.knock_out_level = v.knock_out_level;
    r.target_amount = v.target_amount;
    r.target_type = v.target_type.empty() ? std::nullopt : std::optional(v.target_type);
    r.payoff_type = v.payoff_type;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::equity_accumulator_instrument>
equity_accumulator_instrument_mapper::map(const std::vector<equity_accumulator_instrument_entity>& v) {
    return map_vector<equity_accumulator_instrument_entity, domain::equity_accumulator_instrument>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<equity_accumulator_instrument_entity>
equity_accumulator_instrument_mapper::map(const std::vector<domain::equity_accumulator_instrument>& v) {
    return map_vector<domain::equity_accumulator_instrument, equity_accumulator_instrument_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
