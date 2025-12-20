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
#include "ores.assets/repository/tag_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::assets::repository {

using namespace ores::utility::log;
using namespace ores::database::repository;

domain::tag tag_mapper::map(const tag_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::tag r;
    r.version = v.version;
    r.tag_id = v.tag_id.value();
    r.name = v.name;
    r.description = v.description;
    r.recorded_by = v.modified_by;
    r.recorded_at = v.valid_from.value().str();

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity.";
    return r;
}

tag_entity tag_mapper::map(const domain::tag& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity.";

    tag_entity r;
    r.tag_id = v.tag_id;
    r.version = v.version;
    r.name = v.name;
    r.description = v.description;
    r.modified_by = v.recorded_by;
    // Note: recorded_at is read-only; valid_from/valid_to are managed by database triggers

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::tag>
tag_mapper::map(const std::vector<tag_entity>& v) {
    return map_vector<tag_entity, domain::tag>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<tag_entity>
tag_mapper::map(const std::vector<domain::tag>& v) {
    return map_vector<domain::tag, tag_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
