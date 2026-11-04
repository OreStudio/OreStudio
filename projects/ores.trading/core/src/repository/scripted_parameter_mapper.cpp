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
#include "ores.trading.core/repository/scripted_parameter_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/scripted_parameter.hpp"
#include "ores.trading.api/domain/scripted_parameter_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/scripted_parameter_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::scripted_parameter scripted_parameter_mapper::map(const scripted_parameter_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::scripted_parameter r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.trade_id = v.trade_id;
    r.sequence_number = v.sequence_number;
    r.trade_activity_id = v.trade_activity_id;
    r.name = v.name;
    r.declared_type = v.declared_type;
    r.shape = v.shape;
    r.value = v.value;
    r.apply_coarsening = v.apply_coarsening;
    r.base_schedule = v.base_schedule;
    r.shift = v.shift;
    r.calendar = v.calendar;
    r.convention = v.convention;
    r.remove_first_date = v.remove_first_date;
    r.remove_last_date = v.remove_last_date;
    r.shift_unit = v.shift_unit;
    r.shift_anchor = v.shift_anchor;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

scripted_parameter_entity scripted_parameter_mapper::map(const domain::scripted_parameter& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    scripted_parameter_entity r;
    r.trade_id = v.trade_id;
    r.sequence_number = v.sequence_number;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.trade_activity_id = v.trade_activity_id;
    r.name = v.name;
    r.declared_type = v.declared_type;
    r.shape = v.shape;
    r.value = v.value;
    r.apply_coarsening = v.apply_coarsening;
    r.base_schedule = v.base_schedule;
    r.shift = v.shift;
    r.calendar = v.calendar;
    r.convention = v.convention;
    r.remove_first_date = v.remove_first_date;
    r.remove_last_date = v.remove_last_date;
    r.shift_unit = v.shift_unit;
    r.shift_anchor = v.shift_anchor;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::scripted_parameter>
scripted_parameter_mapper::map(const std::vector<scripted_parameter_entity>& v) {
    return map_vector<scripted_parameter_entity, domain::scripted_parameter>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<scripted_parameter_entity>
scripted_parameter_mapper::map(const std::vector<domain::scripted_parameter>& v) {
    return map_vector<domain::scripted_parameter, scripted_parameter_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
