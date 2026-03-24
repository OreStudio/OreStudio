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
#include "ores.compute.core/generators/host_generator.hpp"

#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::compute::generators {

using ores::utility::generation::generation_keys;

domain::host generate_synthetic_host(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto idx = ++counter;
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");
    const auto tenant_id = ctx.env().get_or(
        generation_keys::tenant_id, "system");

    domain::host r;
    r.version = 1;
    r.tenant_id = utility::uuid::tenant_id::from_string(tenant_id).value();
    r.id = ctx.generate_uuid();
    r.external_id = std::string("host-") + std::to_string(idx);
    r.location = std::string("us-east-1");
    r.cpu_count = 8;
    r.ram_mb = 32768;
    r.gpu_type = "";
    r.last_rpc_time = ctx.past_timepoint();
    r.credit_total = 0.0;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::host>
generate_synthetic_hosts(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::host> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_host(ctx));
    return r;
}

}
