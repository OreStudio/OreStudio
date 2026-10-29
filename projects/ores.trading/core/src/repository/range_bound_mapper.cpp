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
#include "ores.trading.core/repository/range_bound_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/range_bound.hpp"
#include "ores.trading.api/domain/range_bound_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/range_bound_entity.hpp"
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

domain::range_bound range_bound_mapper::map(const range_bound_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::range_bound r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.trade_id = v.trade_id;
    r.sequence_number = v.sequence_number;
    r.trade_activity_id = v.trade_activity_id;
    r.start_date = v.start_date;
    r.range_from = v.range_from;
    r.range_to = v.range_to;
    r.leverage = v.leverage;
    r.strike = v.strike;
    r.strike_adjustment = v.strike_adjustment;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

range_bound_entity range_bound_mapper::map(const domain::range_bound& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    range_bound_entity r;
    r.trade_id = v.trade_id;
    r.sequence_number = v.sequence_number;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.trade_activity_id = v.trade_activity_id;
    r.start_date = v.start_date;
    r.range_from = v.range_from;
    r.range_to = v.range_to;
    r.leverage = v.leverage;
    r.strike = v.strike;
    r.strike_adjustment = v.strike_adjustment;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::range_bound> range_bound_mapper::map(const std::vector<range_bound_entity>& v) {
    return map_vector<range_bound_entity, domain::range_bound>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<range_bound_entity> range_bound_mapper::map(const std::vector<domain::range_bound>& v) {
    return map_vector<domain::range_bound, range_bound_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
