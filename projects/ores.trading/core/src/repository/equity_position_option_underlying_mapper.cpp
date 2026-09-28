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
#include "ores.trading.core/repository/equity_position_option_underlying_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/equity_position_option_underlying_json_io.hpp" // IWYU pragma: keep.
#include "ores.utility/decimal/decimal.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::equity_position_option_underlying
equity_position_option_underlying_mapper::map(const equity_position_option_underlying_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::equity_position_option_underlying r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.trade_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_id.value());
    r.sequence_number = boost::lexical_cast<int>(v.sequence_number.value());
    r.underlying_name = v.underlying_name;
    r.strike = ores::utility::decimal::decimal::from_string(v.strike).value();
    r.weight = v.weight.has_value() ?
                   std::optional(ores::utility::decimal::decimal::from_string(*v.weight).value()) :
                   std::nullopt;
    r.long_short = v.long_short;
    r.option_type = v.option_type.value_or("");
    r.exercise_type = v.exercise_type.value_or("");
    r.settlement_type = v.settlement_type.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

equity_position_option_underlying_entity
equity_position_option_underlying_mapper::map(const domain::equity_position_option_underlying& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    equity_position_option_underlying_entity r;
    r.trade_id = boost::uuids::to_string(v.trade_id);
    r.sequence_number = std::to_string(v.sequence_number);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.underlying_name = v.underlying_name;
    r.strike = v.strike.to_string();
    r.weight = v.weight.has_value() ? std::optional(v.weight->to_string()) : std::nullopt;
    r.long_short = v.long_short;
    r.option_type = v.option_type.empty() ? std::nullopt : std::optional(v.option_type);
    r.exercise_type = v.exercise_type.empty() ? std::nullopt : std::optional(v.exercise_type);
    r.settlement_type = v.settlement_type.empty() ? std::nullopt : std::optional(v.settlement_type);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::equity_position_option_underlying>
equity_position_option_underlying_mapper::map(
    const std::vector<equity_position_option_underlying_entity>& v) {
    return map_vector<equity_position_option_underlying_entity,
                      domain::equity_position_option_underlying>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<equity_position_option_underlying_entity> equity_position_option_underlying_mapper::map(
    const std::vector<domain::equity_position_option_underlying>& v) {
    return map_vector<domain::equity_position_option_underlying,
                      equity_position_option_underlying_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
