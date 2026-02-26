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
#include "ores.refdata/repository/business_unit_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata/domain/business_unit_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::business_unit
business_unit_mapper::map(const business_unit_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::business_unit r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.unit_name = v.unit_name;
    if (v.parent_business_unit_id.has_value() && !v.parent_business_unit_id->empty())
        r.parent_business_unit_id = boost::lexical_cast<boost::uuids::uuid>(*v.parent_business_unit_id);
    if (v.unit_type_id.has_value() && !v.unit_type_id->empty())
        r.unit_type_id = boost::lexical_cast<boost::uuids::uuid>(*v.unit_type_id);
    r.unit_code = v.unit_code;
    r.business_centre_code = v.business_centre_code;
    r.status = v.status;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

business_unit_entity
business_unit_mapper::map(const domain::business_unit& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    business_unit_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.unit_name = v.unit_name;
    if (v.parent_business_unit_id)
        r.parent_business_unit_id = boost::uuids::to_string(*v.parent_business_unit_id);
    if (v.unit_type_id)
        r.unit_type_id = boost::uuids::to_string(*v.unit_type_id);
    r.unit_code = v.unit_code;
    r.business_centre_code = v.business_centre_code;
    r.status = v.status;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::business_unit>
business_unit_mapper::map(const std::vector<business_unit_entity>& v) {
    return map_vector<business_unit_entity, domain::business_unit>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<business_unit_entity>
business_unit_mapper::map(const std::vector<domain::business_unit>& v) {
    return map_vector<domain::business_unit, business_unit_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
