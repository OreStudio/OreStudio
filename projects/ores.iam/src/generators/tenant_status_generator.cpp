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
#include "ores.iam/generators/tenant_status_generator.hpp"

#include <atomic>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::tenant_status generate_synthetic_tenant_status(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto idx = ++counter;
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");

    domain::tenant_status r;
    r.version = 1;
    r.status = "status_" + ctx.alphanumeric(8) + "_" + std::to_string(idx);
    r.name = "Status " + std::to_string(idx);
    r.description = "Synthetic test status";
    r.display_order = idx;
    r.modified_by = modified_by;
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.performed_by = modified_by;
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::tenant_status>
generate_synthetic_tenant_statuses(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::tenant_status> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_tenant_status(ctx));
    return r;
}

}
