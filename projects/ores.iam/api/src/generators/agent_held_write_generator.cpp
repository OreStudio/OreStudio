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
#include "ores.iam.api/generators/agent_held_write_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::agent_held_write
generate_synthetic_agent_held_write([[maybe_unused]] utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};

    domain::agent_held_write r;
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.request_id = std::string(faker::word::noun()) + "-" + std::to_string(idx);
    r.tenant_id = std::string(faker::word::noun());
    r.party_id = std::string(faker::word::noun());
    r.grant_id = std::string(faker::word::noun());
    r.grantor_account_id = std::string(faker::word::noun());
    r.method = std::string(faker::word::noun());
    r.route = std::string(faker::word::noun());
    r.target = std::string(faker::word::noun());
    r.content_type = std::string(faker::word::noun());
    r.body = std::string(faker::word::noun());
    r.state = std::string(faker::word::noun());
    r.outcome = std::string(faker::word::noun());
    return r;
}

std::vector<domain::agent_held_write>
generate_synthetic_agent_held_writes(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::agent_held_write> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_agent_held_write(ctx));
    return r;
}

}
