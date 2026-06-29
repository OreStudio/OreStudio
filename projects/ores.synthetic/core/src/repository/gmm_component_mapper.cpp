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
#include "ores.synthetic.core/repository/gmm_component_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.synthetic.api/domain/gmm_component_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::synthetic::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::gmm_component gmm_component_mapper::map(const gmm_component_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::gmm_component r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.fx_spot_config_id = boost::lexical_cast<boost::uuids::uuid>(v.fx_spot_config_id);
    r.component_index = v.component_index;
    r.description = v.description;
    r.mean = v.mean;
    r.stdev = v.stdev;
    r.weight = v.weight;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

gmm_component_entity gmm_component_mapper::map(const domain::gmm_component& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    gmm_component_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.party_id = boost::uuids::to_string(v.party_id);
    r.fx_spot_config_id = boost::uuids::to_string(v.fx_spot_config_id);
    r.version = v.version;
    r.component_index = v.component_index;
    r.description = v.description;
    r.mean = v.mean;
    r.stdev = v.stdev;
    r.weight = v.weight;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    // Note: recorded_at is read-only; valid_from/valid_to are managed by database triggers

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::gmm_component>
gmm_component_mapper::map(const std::vector<gmm_component_entity>& v) {
    return map_vector<gmm_component_entity, domain::gmm_component>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<gmm_component_entity>
gmm_component_mapper::map(const std::vector<domain::gmm_component>& v) {
    return map_vector<domain::gmm_component, gmm_component_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
