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
#include "ores.connections/generators/tag_generator.hpp"

#include <unordered_set>
#include <faker-cxx/word.h>
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::connections::generators {

using ores::utility::uuid::uuid_v7_generator;

domain::tag generate_synthetic_tag() {
    static uuid_v7_generator gen;

    domain::tag r;
    r.id = gen();
    r.name = std::string(faker::word::adjective());

    return r;
}

std::vector<domain::tag> generate_synthetic_tags(std::size_t n) {
    std::vector<domain::tag> r;
    r.reserve(n);
    for (std::size_t i = 0; i < n; ++i) {
        r.push_back(generate_synthetic_tag());
    }
    return r;
}

std::vector<domain::tag> generate_unique_synthetic_tags(std::size_t n) {
    std::unordered_set<std::string> seen;
    seen.reserve(n);

    std::vector<domain::tag> r;
    r.reserve(n);

    std::size_t suffix = 0;
    while (r.size() < n) {
        auto tag = generate_synthetic_tag();
        if (!seen.insert(tag.name).second) {
            auto base_name = tag.name;
            do {
                tag.name = base_name + "_" + std::to_string(++suffix);
            } while (!seen.insert(tag.name).second);
        }
        r.push_back(std::move(tag));
    }
    return r;
}

}
