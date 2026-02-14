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
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::connections::generators {

using ores::utility::generation::generation_keys;

domain::folder generate_synthetic_folder(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");
    const auto tenant_id = ctx.env().get_or(
        std::string(generation_keys::tenant_id), "system");

    domain::folder r;
    r.id = ctx.generate_uuid();
    r.name = std::string(faker::word::noun());
    r.parent_id = std::nullopt;

    return r;
}

domain::folder generate_synthetic_folder(
    utility::generation::generation_context& ctx,
    const boost::uuids::uuid& parent_id) {
    auto r = generate_synthetic_folder(ctx);
    r.parent_id = parent_id;
    return r;
}

std::vector<domain::folder> generate_synthetic_folders(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::folder> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_folder(ctx));

    return r;
}

}
