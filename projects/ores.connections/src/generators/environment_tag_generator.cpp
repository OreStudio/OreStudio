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
#include "ores.connections/generators/environment_tag_generator.hpp"

#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::connections::generators {

using ores::utility::uuid::uuid_v7_generator;

domain::environment_tag generate_synthetic_environment_tag() {
    static uuid_v7_generator gen;

    domain::environment_tag r;
    r.environment_id = gen();
    r.tag_id = gen();

    return r;
}

domain::environment_tag generate_synthetic_environment_tag(
    const boost::uuids::uuid& environment_id,
    const boost::uuids::uuid& tag_id) {
    domain::environment_tag r;
    r.environment_id = environment_id;
    r.tag_id = tag_id;
    return r;
}

std::vector<domain::environment_tag> generate_synthetic_environment_tags(std::size_t n) {
    std::vector<domain::environment_tag> r;
    r.reserve(n);
    for (std::size_t i = 0; i < n; ++i) {
        r.push_back(generate_synthetic_environment_tag());
    }
    return r;
}

}
