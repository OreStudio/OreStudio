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
#include "ores.compute.api/generators/app_version_platform_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.

namespace ores::compute::generators {

using ores::utility::generation::generation_keys;

domain::app_version_platform
generate_synthetic_app_version_platform(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(generation_keys::modified_by, "system");
    const auto tenant_id = ctx.env().get_or(generation_keys::tenant_id, "system");

    domain::app_version_platform r;
    r.version = 0;
    r.tenant_id = tenant_id;
    r.app_version_id = ctx.generate_uuid();
    r.platform_id = ctx.generate_uuid();
    r.package_uri = std::string(faker::internet::url());
    r.sha256 = std::string(faker::crypto::sha256());
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::app_version_platform>
generate_synthetic_app_version_platforms(std::size_t n,
                                         utility::generation::generation_context& ctx) {
    std::vector<domain::app_version_platform> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_app_version_platform(ctx));
    return r;
}

}
