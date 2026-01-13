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
#include "ores.connections/repository/environment_tag_mapper.hpp"

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::connections::repository {

environment_tag_entity environment_tag_mapper::to_entity(
    const domain::environment_tag& et) {
    environment_tag_entity e;
    e.environment_id = boost::uuids::to_string(et.environment_id);
    e.tag_id = boost::uuids::to_string(et.tag_id);
    return e;
}

domain::environment_tag environment_tag_mapper::to_domain(
    const environment_tag_entity& e) {
    boost::uuids::string_generator gen;
    domain::environment_tag et;
    et.environment_id = gen(e.environment_id);
    et.tag_id = gen(e.tag_id);
    return et;
}

std::vector<environment_tag_entity> environment_tag_mapper::to_entities(
    const std::vector<domain::environment_tag>& tags) {
    std::vector<environment_tag_entity> entities;
    entities.reserve(tags.size());
    for (const auto& et : tags) {
        entities.push_back(to_entity(et));
    }
    return entities;
}

std::vector<domain::environment_tag> environment_tag_mapper::to_domain(
    const std::vector<environment_tag_entity>& entities) {
    std::vector<domain::environment_tag> tags;
    tags.reserve(entities.size());
    for (const auto& e : entities) {
        tags.push_back(to_domain(e));
    }
    return tags;
}

}
