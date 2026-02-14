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
#include "ores.assets/generators/image_generator.hpp"

#include <unordered_set>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <faker-cxx/lorem.h>
#include <faker-cxx/internet.h>
#include <faker-cxx/string.h>
#include <faker-cxx/number.h>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/faker/datetime.hpp"

namespace ores::assets::generators {

domain::image generate_synthetic_image() {
    domain::image r;

    static boost::uuids::random_generator gen;
    r.image_id = gen();
    r.key = faker::string::alphanumeric(8);
    r.description = std::string(faker::lorem::sentence());
    r.svg_data = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 100\">"
                 "<rect fill=\"#" + faker::number::hexadecimal(6) + "\" width=\"100\" height=\"100\"/>"
                 "</svg>";
    r.modified_by = std::string(faker::internet::username());
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_timepoint();

    return r;
}

std::vector<domain::image> generate_synthetic_images(std::size_t n) {
    std::vector<domain::image> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_image());

    return r;
}

std::vector<domain::image> generate_unique_synthetic_images(std::size_t n) {
    std::unordered_set<std::string> seen;
    seen.reserve(n);

    std::vector<domain::image> r;
    r.reserve(n);

    std::size_t suffix = 0;
    while (r.size() < n) {
        auto image = generate_synthetic_image();
        // Loop until we find a unique key
        if (!seen.insert(image.key).second) {
            auto base_key = image.key;
            do {
                image.key = base_key + "_" + std::to_string(++suffix);
            } while (!seen.insert(image.key).second);
        }
        r.push_back(std::move(image));
    }
    return r;
}

}
