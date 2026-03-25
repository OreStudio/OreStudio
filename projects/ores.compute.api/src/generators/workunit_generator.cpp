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
#include "ores.compute.api/generators/workunit_generator.hpp"

#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::compute::generators {

using ores::utility::generation::generation_keys;

domain::workunit generate_synthetic_workunit(
    const boost::uuids::uuid& batch_id,
    const boost::uuids::uuid& app_version_id,
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto idx = ++counter;
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");
    const auto tenant_id = ctx.env().get_or(
        generation_keys::tenant_id, "system");

    domain::workunit r;
    r.version = 1;
    r.tenant_id = utility::uuid::tenant_id::from_string(tenant_id).value();
    r.id = ctx.generate_uuid();
    r.batch_id = batch_id;
    r.app_version_id = app_version_id;
    r.input_uri = std::string("s3://inputs/wu-") + std::to_string(idx) + ".zip";
    r.config_uri = std::string("s3://configs/wu-") + std::to_string(idx) + ".xml";
    r.priority = 0;
    r.target_redundancy = 1;
    r.canonical_result_id = boost::uuids::uuid{};
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::workunit>
generate_synthetic_workunits(std::size_t n,
    const boost::uuids::uuid& batch_id,
    const boost::uuids::uuid& app_version_id,
    utility::generation::generation_context& ctx) {
    std::vector<domain::workunit> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_workunit(batch_id, app_version_id, ctx));
    return r;
}

}
