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
#include "ores.assets/generators/currency_image_generator.hpp"

#include <unordered_set>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <faker-cxx/finance.h>
#include <faker-cxx/internet.h>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/faker/datetime.hpp"

namespace ores::assets::generators {

domain::currency_image generate_synthetic_currency_image() {
    domain::currency_image r;

    static boost::uuids::random_generator gen;
    auto fakerCurrency = faker::finance::currency();
    r.iso_code = fakerCurrency.code;
    r.image_id = boost::uuids::to_string(gen());
    r.assigned_by = std::string(faker::internet::username());
    r.assigned_at = utility::faker::datetime::past_string();

    return r;
}

domain::currency_image generate_synthetic_currency_image(const std::string& iso_code,
                                                          const std::string& image_id) {
    domain::currency_image r;

    r.iso_code = iso_code;
    r.image_id = image_id;
    r.assigned_by = std::string(faker::internet::username());
    r.assigned_at = utility::faker::datetime::past_string();

    return r;
}

std::vector<domain::currency_image> generate_synthetic_currency_images(std::size_t n) {
    std::vector<domain::currency_image> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_currency_image());

    return r;
}

std::vector<domain::currency_image> generate_unique_synthetic_currency_images(std::size_t n) {
    std::unordered_set<std::string> seen;
    seen.reserve(n);

    std::vector<domain::currency_image> r;
    r.reserve(n);

    std::size_t suffix = 0;
    while (r.size() < n) {
        auto currency_image = generate_synthetic_currency_image();
        // Loop until we find a unique key
        if (!seen.insert(currency_image.iso_code).second) {
            auto base_code = currency_image.iso_code;
            do {
                currency_image.iso_code = base_code + std::to_string(++suffix);
            } while (!seen.insert(currency_image.iso_code).second);
        }
        r.push_back(std::move(currency_image));
    }
    return r;
}

}
