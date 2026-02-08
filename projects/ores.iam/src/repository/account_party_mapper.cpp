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
#include "ores.iam/repository/account_party_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam/domain/account_party_json_io.hpp" // IWYU pragma: keep.

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::account_party
account_party_mapper::map(const account_party_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::account_party r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.account_id = boost::lexical_cast<boost::uuids::uuid>(v.account_id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.recorded_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

account_party_entity
account_party_mapper::map(const domain::account_party& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    account_party_entity r;
    r.account_id = boost::uuids::to_string(v.account_id);
    r.tenant_id = v.tenant_id;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.version = v.version;
    r.modified_by = v.recorded_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::account_party>
account_party_mapper::map(const std::vector<account_party_entity>& v) {
    return map_vector<account_party_entity, domain::account_party>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<account_party_entity>
account_party_mapper::map(const std::vector<domain::account_party>& v) {
    return map_vector<domain::account_party, account_party_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
