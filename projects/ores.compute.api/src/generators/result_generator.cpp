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
#include "ores.compute.api/generators/result_generator.hpp"

#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::compute::generators {

using ores::utility::generation::generation_keys;

domain::result generate_synthetic_result(
    const boost::uuids::uuid& workunit_id,
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");
    const auto tenant_id = ctx.env().get_or(
        generation_keys::tenant_id, "system");

    domain::result r;
    r.version = 1;
    r.tenant_id = utility::uuid::tenant_id::from_string(tenant_id).value();
    r.id = ctx.generate_uuid();
    r.workunit_id = workunit_id;
    r.host_id = boost::uuids::uuid{};  // unassigned until dispatched
    r.pgmq_msg_id = 0;                 // unset until queued
    r.server_state = 1;                // Inactive
    r.outcome = 0;                     // pending
    r.output_uri = "";
    r.received_at = std::chrono::system_clock::time_point{};
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::result>
generate_synthetic_results(std::size_t n,
    const boost::uuids::uuid& workunit_id,
    utility::generation::generation_context& ctx) {
    std::vector<domain::result> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_result(workunit_id, ctx));
    return r;
}

}
