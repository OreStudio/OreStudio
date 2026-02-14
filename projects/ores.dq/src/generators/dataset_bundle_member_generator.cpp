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
#include "ores.dq/generators/dataset_bundle_member_generator.hpp"

#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::dq::generators {

using ores::utility::generation::generation_keys;

domain::dataset_bundle_member generate_synthetic_dataset_bundle_member(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto idx = ++counter;
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");

    domain::dataset_bundle_member r;
    r.version = 1;
    r.bundle_code = std::string(faker::word::noun()) + "_bundle_" + std::to_string(idx);
    r.dataset_code = std::string(faker::word::noun()) + "." + std::string(faker::word::noun())
        + "_" + std::to_string(idx);
    r.display_order = ctx.random_int(1, 100);
    r.modified_by = modified_by;
    r.change_reason_code = "system.new_record";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::dataset_bundle_member>
generate_synthetic_dataset_bundle_members(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::dataset_bundle_member> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_dataset_bundle_member(ctx));
    return r;
}

}
