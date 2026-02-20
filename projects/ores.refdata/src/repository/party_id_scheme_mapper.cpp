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
#include "ores.refdata/repository/party_id_scheme_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata/domain/party_id_scheme_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::party_id_scheme
party_id_scheme_mapper::map(const party_id_scheme_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::party_id_scheme r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.code = v.code.value();
    r.name = v.name;
    r.description = v.description;
    r.coding_scheme_code = v.coding_scheme_code;
    r.display_order = v.display_order;
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

party_id_scheme_entity
party_id_scheme_mapper::map(const domain::party_id_scheme& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    party_id_scheme_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.name = v.name;
    r.description = v.description;
    r.coding_scheme_code = v.coding_scheme_code;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::party_id_scheme>
party_id_scheme_mapper::map(const std::vector<party_id_scheme_entity>& v) {
    return map_vector<party_id_scheme_entity, domain::party_id_scheme>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<party_id_scheme_entity>
party_id_scheme_mapper::map(const std::vector<domain::party_id_scheme>& v) {
    return map_vector<domain::party_id_scheme, party_id_scheme_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
