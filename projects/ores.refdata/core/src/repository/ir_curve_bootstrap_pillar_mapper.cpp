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
#include "ores.refdata.core/repository/ir_curve_bootstrap_pillar_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::ir_curve_bootstrap_pillar
ir_curve_bootstrap_pillar_mapper::map(const ir_curve_bootstrap_pillar_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::ir_curve_bootstrap_pillar r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);

    r.bootstrap_config_id = boost::lexical_cast<boost::uuids::uuid>(v.bootstrap_config_id);


    r.sequence_index = v.sequence_index;

    r.start_tenor_code = v.start_tenor_code;
    r.end_tenor_code = v.end_tenor_code;
    r.curve_role_code = v.curve_role_code;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

ir_curve_bootstrap_pillar_entity
ir_curve_bootstrap_pillar_mapper::map(const domain::ir_curve_bootstrap_pillar& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    ir_curve_bootstrap_pillar_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);

    r.bootstrap_config_id = boost::uuids::to_string(v.bootstrap_config_id);


    r.sequence_index = v.sequence_index;

    r.start_tenor_code = v.start_tenor_code;
    r.end_tenor_code = v.end_tenor_code;
    r.curve_role_code = v.curve_role_code;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::ir_curve_bootstrap_pillar>
ir_curve_bootstrap_pillar_mapper::map(const std::vector<ir_curve_bootstrap_pillar_entity>& v) {
    return map_vector<ir_curve_bootstrap_pillar_entity, domain::ir_curve_bootstrap_pillar>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<ir_curve_bootstrap_pillar_entity>
ir_curve_bootstrap_pillar_mapper::map(const std::vector<domain::ir_curve_bootstrap_pillar>& v) {
    return map_vector<domain::ir_curve_bootstrap_pillar, ir_curve_bootstrap_pillar_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
