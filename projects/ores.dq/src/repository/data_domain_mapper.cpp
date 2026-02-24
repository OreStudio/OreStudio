/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/repository/data_domain_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq/domain/data_domain_json_io.hpp" // IWYU pragma: keep.

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::data_domain
data_domain_mapper::map(const data_domain_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::data_domain r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.name = v.name.value();
    r.description = v.description;
r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

data_domain_entity
data_domain_mapper::map(const domain::data_domain& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    data_domain_entity r;
    r.name = v.name;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.description = v.description;
r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::data_domain>
data_domain_mapper::map(const std::vector<data_domain_entity>& v) {
    return map_vector<data_domain_entity, domain::data_domain>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<data_domain_entity>
data_domain_mapper::map(const std::vector<domain::data_domain>& v) {
    return map_vector<domain::data_domain, data_domain_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
