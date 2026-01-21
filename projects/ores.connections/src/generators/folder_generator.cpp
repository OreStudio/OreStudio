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
#include "ores.connections/generators/folder_generator.hpp"

#include <faker-cxx/word.h>
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::connections::generators {

using ores::utility::uuid::uuid_v7_generator;

domain::folder generate_synthetic_folder() {
    static uuid_v7_generator gen;

    domain::folder r;
    r.id = gen();
    r.name = std::string(faker::word::noun());
    r.parent_id = std::nullopt;

    return r;
}

domain::folder generate_synthetic_folder(const boost::uuids::uuid& parent_id) {
    auto r = generate_synthetic_folder();
    r.parent_id = parent_id;
    return r;
}

std::vector<domain::folder> generate_synthetic_folders(std::size_t n) {
    std::vector<domain::folder> r;
    r.reserve(n);
    for (std::size_t i = 0; i < n; ++i) {
        r.push_back(generate_synthetic_folder());
    }
    return r;
}

}
