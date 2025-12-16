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
#include "ores.accounts/repository/account_role_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::accounts::repository {

using namespace ores::utility::log;
using namespace ores::database::repository;

domain::account_role account_role_mapper::map(const account_role_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::account_role r;
    r.account_id = boost::lexical_cast<boost::uuids::uuid>(v.account_id.value());
    r.role_id = boost::lexical_cast<boost::uuids::uuid>(v.role_id);
    r.assigned_by = v.assigned_by;
    r.assigned_at = v.assigned_at.str();

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity.";
    return r;
}

account_role_entity account_role_mapper::map(const domain::account_role& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity.";

    account_role_entity r;
    r.account_id = boost::lexical_cast<std::string>(v.account_id);
    r.role_id = boost::lexical_cast<std::string>(v.role_id);
    r.assigned_by = v.assigned_by;
    // Note: assigned_at is managed by the database

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::account_role>
account_role_mapper::map(const std::vector<account_role_entity>& v) {
    return map_vector<account_role_entity, domain::account_role>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<account_role_entity>
account_role_mapper::map(const std::vector<domain::account_role>& v) {
    return map_vector<domain::account_role, account_role_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
