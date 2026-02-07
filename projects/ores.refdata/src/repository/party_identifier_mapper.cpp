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
#include "ores.refdata/repository/party_identifier_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata/domain/party_identifier_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::party_identifier
party_identifier_mapper::map(const party_identifier_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::party_identifier r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.id_scheme = v.id_scheme;
    r.id_value = v.id_value;
    r.description = v.description;
    r.recorded_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

party_identifier_entity
party_identifier_mapper::map(const domain::party_identifier& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    party_identifier_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.id_scheme = v.id_scheme;
    r.id_value = v.id_value;
    r.description = v.description;
    r.modified_by = v.recorded_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::party_identifier>
party_identifier_mapper::map(const std::vector<party_identifier_entity>& v) {
    return map_vector<party_identifier_entity, domain::party_identifier>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<party_identifier_entity>
party_identifier_mapper::map(const std::vector<domain::party_identifier>& v) {
    return map_vector<domain::party_identifier, party_identifier_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
