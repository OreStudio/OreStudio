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
#include "ores.assets/generators/image_tag_generator.hpp"

#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <faker-cxx/internet.h>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/faker/datetime.hpp"

namespace ores::assets::generators {

domain::image_tag generate_synthetic_image_tag() {
    domain::image_tag r;

    static boost::uuids::random_generator gen;
    r.image_id = boost::uuids::to_string(gen());
    r.tag_id = boost::uuids::to_string(gen());
    r.assigned_by = std::string(faker::internet::username());
    r.assigned_at = utility::faker::datetime::past_string();

    return r;
}

domain::image_tag generate_synthetic_image_tag(const std::string& image_id,
                                                const std::string& tag_id) {
    domain::image_tag r;

    r.image_id = image_id;
    r.tag_id = tag_id;
    r.assigned_by = std::string(faker::internet::username());
    r.assigned_at = utility::faker::datetime::past_string();

    return r;
}

std::vector<domain::image_tag> generate_synthetic_image_tags(std::size_t n) {
    std::vector<domain::image_tag> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_image_tag());

    return r;
}

}
