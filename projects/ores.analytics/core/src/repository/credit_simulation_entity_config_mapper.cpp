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
#include "ores.analytics.core/repository/credit_simulation_entity_config_mapper.hpp"
#include "ores.analytics.api/domain/credit_simulation_entity_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.database/repository/mapper_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::analytics::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::credit_simulation_entity_config
credit_simulation_entity_config_mapper::map(const credit_simulation_entity_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::credit_simulation_entity_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.credit_simulation_config_id =
        boost::lexical_cast<boost::uuids::uuid>(v.credit_simulation_config_id);
    r.name = v.name;
    r.transition_matrix_id = boost::lexical_cast<boost::uuids::uuid>(v.transition_matrix_id);
    r.initial_state = v.initial_state;
    r.factor_loadings = v.factor_loadings.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

credit_simulation_entity_config_entity
credit_simulation_entity_config_mapper::map(const domain::credit_simulation_entity_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    credit_simulation_entity_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.credit_simulation_config_id = boost::uuids::to_string(v.credit_simulation_config_id);
    r.name = v.name;
    r.transition_matrix_id = boost::uuids::to_string(v.transition_matrix_id);
    r.initial_state = v.initial_state;
    r.factor_loadings = v.factor_loadings.empty() ? std::nullopt : std::optional(v.factor_loadings);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::credit_simulation_entity_config> credit_simulation_entity_config_mapper::map(
    const std::vector<credit_simulation_entity_config_entity>& v) {
    return map_vector<credit_simulation_entity_config_entity,
                      domain::credit_simulation_entity_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<credit_simulation_entity_config_entity> credit_simulation_entity_config_mapper::map(
    const std::vector<domain::credit_simulation_entity_config>& v) {
    return map_vector<domain::credit_simulation_entity_config,
                      credit_simulation_entity_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
