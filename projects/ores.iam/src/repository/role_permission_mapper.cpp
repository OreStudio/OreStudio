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
#include "ores.iam/repository/role_permission_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::role_permission role_permission_mapper::map(const role_permission_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::role_permission r;
    r.role_id = boost::lexical_cast<boost::uuids::uuid>(v.role_id);
    r.permission_id = boost::lexical_cast<boost::uuids::uuid>(v.permission_id);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity.";
    return r;
}

role_permission_entity role_permission_mapper::map(const domain::role_permission& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity.";

    role_permission_entity r;
    r.role_id = boost::lexical_cast<std::string>(v.role_id);
    r.permission_id = boost::lexical_cast<std::string>(v.permission_id);

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::role_permission>
role_permission_mapper::map(const std::vector<role_permission_entity>& v) {
    return map_vector<role_permission_entity, domain::role_permission>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<role_permission_entity>
role_permission_mapper::map(const std::vector<domain::role_permission>& v) {
    return map_vector<domain::role_permission, role_permission_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
