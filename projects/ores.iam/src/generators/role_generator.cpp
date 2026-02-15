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
#include "ores.iam/generators/role_generator.hpp"

#include <atomic>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::role generate_synthetic_role(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto idx = ++counter;
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");
    const auto tid = ctx.env().get_or(
        generation_keys::tenant_id, "system");
    const auto parsed_tid = utility::uuid::tenant_id::from_string(tid);

    domain::role r;
    r.version = 1;
    r.tenant_id = parsed_tid.has_value() ? parsed_tid.value()
        : utility::uuid::tenant_id::system();
    r.id = ctx.generate_uuid();
    r.name = "Role_" + ctx.alphanumeric(6) + "_" + std::to_string(idx);
    r.description = "Synthetic test role";
    r.modified_by = modified_by;
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.performed_by = modified_by;
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::role>
generate_synthetic_roles(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::role> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_role(ctx));
    return r;
}

}
