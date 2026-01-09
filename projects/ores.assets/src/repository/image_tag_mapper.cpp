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
#include "ores.assets/repository/image_tag_mapper.hpp"

#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::assets::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::image_tag image_tag_mapper::map(const image_tag_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::image_tag r;
    r.image_id = boost::lexical_cast<boost::uuids::uuid>(v.image_id.value());
    r.tag_id = boost::lexical_cast<boost::uuids::uuid>(v.tag_id.value());
    r.assigned_by = v.assigned_by;
    r.assigned_at = timestamp_to_timepoint(v.assigned_at.value());

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity.";
    return r;
}

image_tag_entity image_tag_mapper::map(const domain::image_tag& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity.";

    image_tag_entity r;
    r.image_id = boost::uuids::to_string(v.image_id);
    r.tag_id = boost::uuids::to_string(v.tag_id);
    r.assigned_by = v.assigned_by;
    // Note: assigned_at is read-only; managed by database triggers

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::image_tag>
image_tag_mapper::map(const std::vector<image_tag_entity>& v) {
    return map_vector<image_tag_entity, domain::image_tag>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<image_tag_entity>
image_tag_mapper::map(const std::vector<domain::image_tag>& v) {
    return map_vector<domain::image_tag, image_tag_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
