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
#include "ores.assets/repository/image_mapper.hpp"

#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::assets::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::image image_mapper::map(const image_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::image r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.image_id = boost::lexical_cast<boost::uuids::uuid>(v.image_id.value());
    r.key = v.key;
    r.description = v.description;
    r.svg_data = v.svg_data;
    r.modified_by = v.modified_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from.value());

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity.";
    return r;
}

image_entity image_mapper::map(const domain::image& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity.";

    image_entity r;
    r.image_id = boost::uuids::to_string(v.image_id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.key = v.key;
    r.description = v.description;
    r.svg_data = v.svg_data;
    r.modified_by = v.modified_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    // Note: recorded_at is read-only; valid_from/valid_to are managed by database triggers

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::image>
image_mapper::map(const std::vector<image_entity>& v) {
    return map_vector<image_entity, domain::image>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<image_entity>
image_mapper::map(const std::vector<domain::image>& v) {
    return map_vector<domain::image, image_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
