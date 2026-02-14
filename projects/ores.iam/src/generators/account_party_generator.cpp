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
#include "ores.iam/generators/account_party_generator.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::account_party generate_synthetic_account_party(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");
    const auto tenant_id = ctx.env().get_or(
        std::string(generation_keys::tenant_id), "system");

    domain::account_party r;
    r.version = 1;
    r.tenant_id = tenant_id;
    r.account_id = ctx.generate_uuid();
    r.party_id = ctx.generate_uuid();
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::account_party>
generate_synthetic_account_parties(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::account_party> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_account_party(ctx));
    return r;
}

}
