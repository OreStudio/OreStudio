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
#include "ores.connections/repository/tag_mapper.hpp"

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::connections::repository {

tag_entity tag_mapper::to_entity(const domain::tag& t) {
    tag_entity e;
    e.id = boost::uuids::to_string(t.id);
    e.name = t.name;
    return e;
}

domain::tag tag_mapper::to_domain(const tag_entity& e) {
    boost::uuids::string_generator gen;
    domain::tag t;
    t.id = gen(e.id.value());
    t.name = e.name;
    return t;
}

std::vector<tag_entity> tag_mapper::to_entities(
    const std::vector<domain::tag>& tags) {
    std::vector<tag_entity> entities;
    entities.reserve(tags.size());
    for (const auto& t : tags) {
        entities.push_back(to_entity(t));
    }
    return entities;
}

std::vector<domain::tag> tag_mapper::to_domain(
    const std::vector<tag_entity>& entities) {
    std::vector<domain::tag> tags;
    tags.reserve(entities.size());
    for (const auto& e : entities) {
        tags.push_back(to_domain(e));
    }
    return tags;
}

}
