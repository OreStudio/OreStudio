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
#include "ores.database/repository/database_info_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.database/domain/database_info_json_io.hpp" // IWYU pragma: keep.

namespace ores::database::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::database_info
database_info_mapper::map(const database_info_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::database_info r;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.schema_version = v.schema_version;
    r.build_environment = v.build_environment;
    r.git_commit = v.git_commit;
    r.git_date = v.git_date;
    if (!v.created_at)
        throw std::logic_error("Cannot map entity with null created_at to domain object.");
    r.created_at = timestamp_to_timepoint(*v.created_at);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

std::vector<domain::database_info>
database_info_mapper::map(const std::vector<database_info_entity>& v) {
    return map_vector<database_info_entity, domain::database_info>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

}
