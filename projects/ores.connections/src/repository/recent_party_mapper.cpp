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
#include "ores.connections/repository/recent_party_mapper.hpp"

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::connections::repository {

recent_party_entity recent_party_mapper::to_entity(
    const domain::recent_party& rp) {
    recent_party_entity e;
    e.party_id = boost::uuids::to_string(rp.party_id);
    e.party_name = rp.party_name;
    e.last_selected_at = rp.last_selected_at;
    return e;
}

domain::recent_party recent_party_mapper::to_domain(
    const recent_party_entity& e) {
    boost::uuids::string_generator gen;
    domain::recent_party rp;
    rp.party_id = gen(e.party_id.value());
    rp.party_name = e.party_name;
    rp.last_selected_at = e.last_selected_at;
    return rp;
}

std::vector<domain::recent_party> recent_party_mapper::to_domain(
    const std::vector<recent_party_entity>& entities) {
    std::vector<domain::recent_party> result;
    result.reserve(entities.size());
    for (const auto& e : entities)
        result.push_back(to_domain(e));
    return result;
}

}
