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
#include "ores.trading.core/repository/synthetic_cdo_instrument_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/synthetic_cdo_instrument.hpp"
#include "ores.trading.api/domain/synthetic_cdo_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/synthetic_cdo_instrument_entity.hpp"
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

domain::synthetic_cdo_instrument
synthetic_cdo_instrument_mapper::map(const synthetic_cdo_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::synthetic_cdo_instrument r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.trade_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_id);
    r.trade_activity_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_activity_id);
    r.qualifier = v.qualifier;
    r.protection_start_date =
        ores::platform::time::datetime::from_iso8601_date(v.protection_start_date);
    r.attachment_point = v.attachment_point;
    r.detachment_point = v.detachment_point;
    r.recovery_rate = v.recovery_rate;
    r.settles_accrual = v.settles_accrual;
    r.rebates_accrual = v.rebates_accrual;
    r.pays_at_default_time = v.pays_at_default_time;
    r.protection_payment_time = v.protection_payment_time;
    r.upfront_date =
        v.upfront_date.has_value() ?
            std::optional(ores::platform::time::datetime::from_iso8601_date(*v.upfront_date)) :
            std::nullopt;
    r.upfront_fee =
        v.upfront_fee.has_value() ?
            std::optional(ores::utility::decimal::decimal::from_string(*v.upfront_fee).value()) :
            std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

synthetic_cdo_instrument_entity
synthetic_cdo_instrument_mapper::map(const domain::synthetic_cdo_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    synthetic_cdo_instrument_entity r;
    r.trade_id = boost::uuids::to_string(v.trade_id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.trade_activity_id = boost::uuids::to_string(v.trade_activity_id);
    r.qualifier = v.qualifier;
    r.protection_start_date =
        ores::platform::time::datetime::to_iso8601_date(v.protection_start_date);
    r.attachment_point = v.attachment_point;
    r.detachment_point = v.detachment_point;
    r.recovery_rate = v.recovery_rate;
    r.settles_accrual = v.settles_accrual;
    r.rebates_accrual = v.rebates_accrual;
    r.pays_at_default_time = v.pays_at_default_time;
    r.protection_payment_time = v.protection_payment_time;
    r.upfront_date =
        v.upfront_date.has_value() ?
            std::optional(ores::platform::time::datetime::to_iso8601_date(*v.upfront_date)) :
            std::nullopt;
    r.upfront_fee =
        v.upfront_fee.has_value() ? std::optional(v.upfront_fee->to_string()) : std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::synthetic_cdo_instrument>
synthetic_cdo_instrument_mapper::map(const std::vector<synthetic_cdo_instrument_entity>& v) {
    return map_vector<synthetic_cdo_instrument_entity, domain::synthetic_cdo_instrument>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<synthetic_cdo_instrument_entity>
synthetic_cdo_instrument_mapper::map(const std::vector<domain::synthetic_cdo_instrument>& v) {
    return map_vector<domain::synthetic_cdo_instrument, synthetic_cdo_instrument_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
