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

#include "ores.utility/generation/generation_keys.hpp"

namespace ores::connections::generators {

using ores::utility::generation::generation_keys;

domain::environment_tag generate_synthetic_environment_tag(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");
    const auto tenant_id = ctx.env().get_or(
        generation_keys::tenant_id, "system");

    domain::environment_tag r;
    r.environment_id = ctx.generate_uuid();
    r.tag_id = ctx.generate_uuid();

    return r;
}

domain::environment_tag generate_synthetic_environment_tag(
    utility::generation::generation_context& ctx,
    const boost::uuids::uuid& environment_id,
    const boost::uuids::uuid& tag_id) {
    domain::environment_tag r;
    r.environment_id = environment_id;
    r.tag_id = tag_id;
    return r;
}

std::vector<domain::environment_tag> generate_synthetic_environment_tags(
    std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::environment_tag> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_environment_tag(ctx));

    return r;
}

}
