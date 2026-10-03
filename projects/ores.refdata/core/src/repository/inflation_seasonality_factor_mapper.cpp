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
#include "ores.refdata.core/repository/inflation_seasonality_factor_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/inflation_seasonality_factor_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::inflation_seasonality_factor
inflation_seasonality_factor_mapper::map(const inflation_seasonality_factor_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::inflation_seasonality_factor r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.curve_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.curve_definition_id);
    r.factor = v.factor;
    r.position = v.position;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

inflation_seasonality_factor_entity
inflation_seasonality_factor_mapper::map(const domain::inflation_seasonality_factor& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    inflation_seasonality_factor_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.curve_definition_id = boost::uuids::to_string(v.curve_definition_id);
    r.factor = v.factor;
    r.position = v.position;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::inflation_seasonality_factor> inflation_seasonality_factor_mapper::map(
    const std::vector<inflation_seasonality_factor_entity>& v) {
    return map_vector<inflation_seasonality_factor_entity, domain::inflation_seasonality_factor>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<inflation_seasonality_factor_entity> inflation_seasonality_factor_mapper::map(
    const std::vector<domain::inflation_seasonality_factor>& v) {
    return map_vector<domain::inflation_seasonality_factor, inflation_seasonality_factor_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
