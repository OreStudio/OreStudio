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
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::synthetic::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::yield_curve_process_parameter_definition
yield_curve_process_parameter_definition_mapper::map(
    const yield_curve_process_parameter_definition_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::yield_curve_process_parameter_definition r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());

    r.process_type_code = v.process_type_code;


    r.parameter_name = v.parameter_name;

    r.display_name = v.display_name;
    r.symbol = v.symbol;
    r.short_label = v.short_label;
    r.description = v.description;
    r.data_type = v.data_type;
    r.default_value = v.default_value;
    r.min_value = v.min_value;
    r.max_value = v.max_value;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

yield_curve_process_parameter_definition_entity
yield_curve_process_parameter_definition_mapper::map(
    const domain::yield_curve_process_parameter_definition& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    yield_curve_process_parameter_definition_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.process_type_code = v.process_type_code;


    r.parameter_name = v.parameter_name;

    r.display_name = v.display_name;
    r.symbol = v.symbol;
    r.short_label = v.short_label;
    r.description = v.description;
    r.data_type = v.data_type;
    r.default_value = v.default_value;
    r.min_value = v.min_value;
    r.max_value = v.max_value;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::yield_curve_process_parameter_definition>
yield_curve_process_parameter_definition_mapper::map(
    const std::vector<yield_curve_process_parameter_definition_entity>& v) {
    return map_vector<yield_curve_process_parameter_definition_entity,
                      domain::yield_curve_process_parameter_definition>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<yield_curve_process_parameter_definition_entity>
yield_curve_process_parameter_definition_mapper::map(
    const std::vector<domain::yield_curve_process_parameter_definition>& v) {
    return map_vector<domain::yield_curve_process_parameter_definition,
                      yield_curve_process_parameter_definition_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
