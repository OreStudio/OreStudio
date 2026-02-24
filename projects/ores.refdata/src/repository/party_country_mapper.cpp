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
#include "ores.refdata/repository/party_country_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata/domain/party_country_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::party_country
party_country_mapper::map(const party_country_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::party_country r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id.value());
    r.country_alpha2_code = v.country_alpha2_code;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

party_country_entity
party_country_mapper::map(const domain::party_country& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    party_country_entity r;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.tenant_id = v.tenant_id;
    r.country_alpha2_code = v.country_alpha2_code;
    r.version = v.version;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::party_country>
party_country_mapper::map(const std::vector<party_country_entity>& v) {
    return map_vector<party_country_entity, domain::party_country>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<party_country_entity>
party_country_mapper::map(const std::vector<domain::party_country>& v) {
    return map_vector<domain::party_country, party_country_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
