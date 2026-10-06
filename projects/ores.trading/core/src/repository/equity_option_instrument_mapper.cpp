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
#include "ores.trading.core/repository/equity_option_instrument_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/equity_option_instrument.hpp"
#include "ores.trading.api/domain/equity_option_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/equity_option_instrument_entity.hpp"
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

domain::equity_option_instrument
equity_option_instrument_mapper::map(const equity_option_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::equity_option_instrument r;
    r.identity.version = v.version;
    r.identity.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.identity.trade_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_id.value());
    r.identity.trade_type_code = v.trade_type_code;
    r.identity.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.underlying_name = v.underlying_name;
    r.currency = v.currency;
    r.notional = ores::utility::decimal::decimal::from_string(v.notional).value();
    r.option_type = v.option_type;
    r.strike = ores::utility::decimal::decimal::from_string(v.strike).value();
    r.expiry_date = ores::platform::time::datetime::from_iso8601_date(v.expiry_date);
    r.exercise_type = v.exercise_type;
    r.long_short = v.long_short;
    r.settlement_type = v.settlement_type.value_or("");
    r.cliquet_frequency = v.cliquet_frequency.value_or("");
    r.description = v.description.value_or("");
    r.audit.modified_by = v.modified_by;
    r.audit.performed_by = v.performed_by;
    r.audit.change_reason_code = v.change_reason_code;
    r.audit.change_commentary = v.change_commentary;
    r.audit.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

equity_option_instrument_entity
equity_option_instrument_mapper::map(const domain::equity_option_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    equity_option_instrument_entity r;
    r.trade_id = boost::uuids::to_string(v.identity.trade_id);
    r.tenant_id = v.identity.tenant_id.to_string();
    r.version = v.identity.version;
    r.trade_type_code = v.identity.trade_type_code;
    r.party_id = boost::uuids::to_string(v.identity.party_id);
    r.underlying_name = v.underlying_name;
    r.currency = v.currency;
    r.notional = v.notional.to_string();
    r.option_type = v.option_type;
    r.strike = v.strike.to_string();
    r.expiry_date = ores::platform::time::datetime::to_iso8601_date(v.expiry_date);
    r.exercise_type = v.exercise_type;
    r.long_short = v.long_short;
    r.settlement_type = v.settlement_type.empty() ? std::nullopt : std::optional(v.settlement_type);
    r.cliquet_frequency =
        v.cliquet_frequency.empty() ? std::nullopt : std::optional(v.cliquet_frequency);
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.audit.modified_by;
    r.performed_by = v.audit.performed_by;
    r.change_reason_code = v.audit.change_reason_code;
    r.change_commentary = v.audit.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::equity_option_instrument>
equity_option_instrument_mapper::map(const std::vector<equity_option_instrument_entity>& v) {
    return map_vector<equity_option_instrument_entity, domain::equity_option_instrument>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<equity_option_instrument_entity>
equity_option_instrument_mapper::map(const std::vector<domain::equity_option_instrument>& v) {
    return map_vector<domain::equity_option_instrument, equity_option_instrument_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
