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
#include "ores.iam.api/generators/auth_event_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::auth_event
generate_synthetic_auth_event([[maybe_unused]] utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};

    domain::auth_event r;
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.id = std::string(faker::word::noun()) + "-" + std::to_string(idx);
    r.event_time = ctx.past_timepoint();
    r.tenant_id = std::string(faker::word::noun());
    r.account_id = std::string(faker::word::noun());
    r.event_type = std::string(faker::word::noun());
    r.username = std::string(faker::word::noun());
    r.session_id = std::string(faker::word::noun());
    r.party_id = std::string(faker::word::noun());
    r.error_detail = std::string(faker::word::noun());
    return r;
}

std::vector<domain::auth_event>
generate_synthetic_auth_events(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::auth_event> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_auth_event(ctx));
    return r;
}

}
