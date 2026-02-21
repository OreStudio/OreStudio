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
#include "ores.refdata/repository/business_centre_mapper.hpp"

#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata/domain/business_centre_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::business_centre business_centre_mapper::map(const business_centre_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::business_centre r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    BOOST_LOG_SEV(lg(), trace) << "Mapped version: entity.version=" << v.version
                               << " -> domain.version=" << r.version;
    r.code = v.code.value();
    r.source = v.source.value_or("");
    r.description = v.description.value_or("");
    r.city_name = v.city_name.value_or("");
    r.coding_scheme_code = v.coding_scheme_code;
    r.country_alpha2_code = v.country_alpha2_code.value_or("");
    if (v.image_id) {
        r.image_id = boost::lexical_cast<boost::uuids::uuid>(*v.image_id);
    }
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

business_centre_entity business_centre_mapper::map(const domain::business_centre& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    business_centre_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    if (!v.source.empty()) {
        r.source = v.source;
    }
    if (!v.description.empty()) {
        r.description = v.description;
    }
    if (!v.city_name.empty()) {
        r.city_name = v.city_name;
    }
    r.coding_scheme_code = v.coding_scheme_code;
    if (!v.country_alpha2_code.empty()) {
        r.country_alpha2_code = v.country_alpha2_code;
    }
    if (v.image_id) {
        r.image_id = boost::uuids::to_string(*v.image_id);
    }
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    // Note: recorded_at is read-only; valid_from/valid_to are managed by database triggers

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::business_centre>
business_centre_mapper::map(const std::vector<business_centre_entity>& v) {
    return map_vector<business_centre_entity, domain::business_centre>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<business_centre_entity>
business_centre_mapper::map(const std::vector<domain::business_centre>& v) {
    return map_vector<domain::business_centre, business_centre_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
