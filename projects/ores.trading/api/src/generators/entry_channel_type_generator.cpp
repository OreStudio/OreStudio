/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_generator.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.trading.api/generators/entry_channel_type_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::trading::generators {

using ores::utility::generation::generation_keys;

domain::entry_channel_type generate_synthetic_entry_channel_type(
    [[maybe_unused]] utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};

    domain::entry_channel_type r;
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.code = std::string("manual") + "-" + std::to_string(idx);
    r.description = std::string(faker::lorem::sentence());
    return r;
}

std::vector<domain::entry_channel_type>
generate_synthetic_entry_channel_types(std::size_t n,
                                       utility::generation::generation_context& ctx) {
    std::vector<domain::entry_channel_type> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_entry_channel_type(ctx));
    return r;
}

}
