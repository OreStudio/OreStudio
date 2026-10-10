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
#include "ores.trading.core/repository/swap_leg_rate_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/swap_leg_rate.hpp"
#include "ores.trading.api/domain/swap_leg_rate_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/swap_leg_rate_entity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::swap_leg_rate swap_leg_rate_mapper::map(const swap_leg_rate_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::swap_leg_rate r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.trade_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_id.value());
    r.leg_number = boost::lexical_cast<int>(v.leg_number.value());
    r.rate_role = v.rate_role.value();
    r.sequence_number = boost::lexical_cast<int>(v.sequence_number.value());
    r.trade_activity_id = boost::lexical_cast<boost::uuids::uuid>(v.trade_activity_id);
    r.start_date =
        v.start_date.has_value() ?
            std::optional(ores::platform::time::datetime::from_iso8601_date(*v.start_date)) :
            std::nullopt;
    r.value = ores::utility::decimal::decimal::from_string(v.value).value();
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

swap_leg_rate_entity swap_leg_rate_mapper::map(const domain::swap_leg_rate& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    swap_leg_rate_entity r;
    r.trade_id = boost::uuids::to_string(v.trade_id);
    r.leg_number = std::to_string(v.leg_number);
    r.rate_role = v.rate_role;
    r.sequence_number = std::to_string(v.sequence_number);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.trade_activity_id = boost::uuids::to_string(v.trade_activity_id);
    r.start_date =
        v.start_date.has_value() ?
            std::optional(ores::platform::time::datetime::to_iso8601_date(*v.start_date)) :
            std::nullopt;
    r.value = v.value.to_string();
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::swap_leg_rate>
swap_leg_rate_mapper::map(const std::vector<swap_leg_rate_entity>& v) {
    return map_vector<swap_leg_rate_entity, domain::swap_leg_rate>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<swap_leg_rate_entity>
swap_leg_rate_mapper::map(const std::vector<domain::swap_leg_rate>& v) {
    return map_vector<domain::swap_leg_rate, swap_leg_rate_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
