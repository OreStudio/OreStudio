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
#include "ores.connections/repository/folder_mapper.hpp"

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::connections::repository {

folder_entity folder_mapper::to_entity(const domain::folder& f) {
    folder_entity e;
    e.id = boost::uuids::to_string(f.id);
    e.name = f.name;
    if (f.parent_id) {
        e.parent_id = boost::uuids::to_string(*f.parent_id);
    }
    return e;
}

domain::folder folder_mapper::to_domain(const folder_entity& e) {
    boost::uuids::string_generator gen;
    domain::folder f;
    f.id = gen(e.id.value());
    f.name = e.name;
    if (e.parent_id) {
        f.parent_id = gen(*e.parent_id);
    }
    return f;
}

std::vector<folder_entity> folder_mapper::to_entities(
    const std::vector<domain::folder>& folders) {
    std::vector<folder_entity> entities;
    entities.reserve(folders.size());
    for (const auto& f : folders) {
        entities.push_back(to_entity(f));
    }
    return entities;
}

std::vector<domain::folder> folder_mapper::to_domain(
    const std::vector<folder_entity>& entities) {
    std::vector<domain::folder> folders;
    folders.reserve(entities.size());
    for (const auto& e : entities) {
        folders.push_back(to_domain(e));
    }
    return folders;
}

}
