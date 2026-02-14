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
#include "ores.assets/generators/tag_generator.hpp"

#include <unordered_set>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <faker-cxx/lorem.h>
#include <faker-cxx/word.h>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/faker/datetime.hpp"
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::assets::generators {

using ores::utility::generation::generation_keys;

domain::tag generate_synthetic_tag(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");
    const auto tenant_id = ctx.env().get_or(
        std::string(generation_keys::tenant_id), "system");

    domain::tag r;
    r.tenant_id = tenant_id;
    r.tag_id = boost::uuids::to_string(ctx.generate_uuid());
    // Use UUID suffix to ensure global uniqueness across tests.
    // Take the last 12 chars (random part of UUID v7, avoiding timestamp prefix).
    const auto uuid = boost::uuids::to_string(ctx.generate_uuid());
    r.name = "test_" + std::string(faker::word::noun()) + "_"
        + uuid.substr(uuid.size() - 12);
    r.description = std::string(faker::lorem::sentence());
    r.modified_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_string();

    return r;
}

std::vector<domain::tag> generate_synthetic_tags(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::tag> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_tag(ctx));

    return r;
}

std::vector<domain::tag> generate_unique_synthetic_tags(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::unordered_set<std::string> seen;
    seen.reserve(n);

    std::vector<domain::tag> r;
    r.reserve(n);

    std::size_t suffix = 0;
    while (r.size() < n) {
        auto tag = generate_synthetic_tag(ctx);
        // Loop until we find a unique key
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
