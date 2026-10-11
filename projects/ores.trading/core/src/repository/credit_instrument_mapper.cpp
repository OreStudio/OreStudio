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
#include "ores.trading.core/repository/credit_instrument_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include "ores.trading.api/domain/credit_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/credit_instrument_entity.hpp"
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

domain::credit_instrument credit_instrument_mapper::map(const credit_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::credit_instrument r;
    r.identity.version = v.version;
    r.identity.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.identity.trade_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_id.value());
    r.identity.trade_type_code = v.trade_type_code;
    r.identity.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.identity.trade_activity_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_activity_id);
    r.reference_entity = v.reference_entity;
    r.currency = v.currency;
    r.notional = ores::utility::decimal::decimal::from_string(v.notional).value();
    r.spread = ores::utility::decimal::decimal::from_string(v.spread).value();
    r.recovery_rate = v.recovery_rate;
    r.tenor = v.tenor;
    r.start_date = ores::platform::time::datetime::from_iso8601_date(v.start_date);
    r.maturity_date = ores::platform::time::datetime::from_iso8601_date(v.maturity_date);
    r.day_count_fraction_code = v.day_count_fraction_code;
    r.payment_frequency_code = v.payment_frequency_code;
    r.index_name = v.index_name.value_or("");
    r.index_series = v.index_series;
    r.seniority = v.seniority.value_or("");
    r.restructuring = v.restructuring.value_or("");
    r.description = v.description.value_or("");
    r.option_type = v.option_type.value_or("");
    r.option_expiry_date = v.option_expiry_date.has_value() ?
                               std::optional(ores::platform::time::datetime::from_iso8601_date(
                                   *v.option_expiry_date)) :
                               std::nullopt;
    r.option_strike =
        v.option_strike.has_value() ?
            std::optional(ores::utility::decimal::decimal::from_string(*v.option_strike).value()) :
            std::nullopt;
    r.linked_asset_code = v.linked_asset_code.value_or("");
    r.tranche_attachment = v.tranche_attachment;
    r.tranche_detachment = v.tranche_detachment;
    r.audit.modified_by = v.modified_by;
    r.audit.performed_by = v.performed_by;
    r.audit.change_reason_code = v.change_reason_code;
    r.audit.change_commentary = v.change_commentary;
    r.audit.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

credit_instrument_entity credit_instrument_mapper::map(const domain::credit_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    credit_instrument_entity r;
    r.trade_id = boost::uuids::to_string(v.identity.trade_id);
    r.tenant_id = v.identity.tenant_id.to_string();
    r.version = v.identity.version;
    r.trade_type_code = v.identity.trade_type_code;
    r.party_id = boost::uuids::to_string(v.identity.party_id);
    r.trade_activity_id = boost::uuids::to_string(v.identity.trade_activity_id);
    r.reference_entity = v.reference_entity;
    r.currency = v.currency;
    r.notional = v.notional.to_string();
    r.spread = v.spread.to_string();
    r.recovery_rate = v.recovery_rate;
    r.tenor = v.tenor;
    r.start_date = ores::platform::time::datetime::to_iso8601_date(v.start_date);
    r.maturity_date = ores::platform::time::datetime::to_iso8601_date(v.maturity_date);
    r.day_count_fraction_code = v.day_count_fraction_code;
    r.payment_frequency_code = v.payment_frequency_code;
    r.index_name = v.index_name.empty() ? std::nullopt : std::optional(v.index_name);
    r.index_series = v.index_series;
    r.seniority = v.seniority.empty() ? std::nullopt : std::optional(v.seniority);
    r.restructuring = v.restructuring.empty() ? std::nullopt : std::optional(v.restructuring);
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.option_type = v.option_type.empty() ? std::nullopt : std::optional(v.option_type);
    r.option_expiry_date =
        v.option_expiry_date.has_value() ?
            std::optional(ores::platform::time::datetime::to_iso8601_date(*v.option_expiry_date)) :
            std::nullopt;
    r.option_strike =
        v.option_strike.has_value() ? std::optional(v.option_strike->to_string()) : std::nullopt;
    r.linked_asset_code =
        v.linked_asset_code.empty() ? std::nullopt : std::optional(v.linked_asset_code);
    r.tranche_attachment = v.tranche_attachment;
    r.tranche_detachment = v.tranche_detachment;
    r.modified_by = v.audit.modified_by;
    r.performed_by = v.audit.performed_by;
    r.change_reason_code = v.audit.change_reason_code;
    r.change_commentary = v.audit.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::credit_instrument>
credit_instrument_mapper::map(const std::vector<credit_instrument_entity>& v) {
    return map_vector<credit_instrument_entity, domain::credit_instrument>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<credit_instrument_entity>
credit_instrument_mapper::map(const std::vector<domain::credit_instrument>& v) {
    return map_vector<domain::credit_instrument, credit_instrument_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
