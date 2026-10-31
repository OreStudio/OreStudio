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
#include "ores.reporting.api/generators/cut_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::reporting::generators {

using ores::utility::generation::generation_keys;

domain::cut generate_synthetic_cut(utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::cut r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    r.id = ctx.generate_uuid();
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.kind_code = std::string(faker::word::noun()) + "-" + std::to_string(idx);
    r.party_id = ctx.generate_uuid();
    r.scope_code = std::string("ACME-LONDON") + "-" + std::to_string(idx);
    r.business_centre_code = std::string(faker::word::noun()) + "-" + std::to_string(idx);
    r.timepoint = ctx.past_timepoint();
    r.take_ordinal = 1;
    r.timepoint_timezone = std::string("Europe/London");
    r.cut_time_code = std::nullopt;
    r.market_data_snapshot_id = std::nullopt;
    r.deal_population_id = std::nullopt;
    r.reference_data_version = std::nullopt;
    r.engine_version = std::nullopt;
    r.code_version = std::nullopt;
    r.taken_by = std::string(faker::word::noun());
    r.taken_at = ctx.past_timepoint();
    r.requires_authorisation = false;
    r.approval_request_id = std::nullopt;
    r.approved_by = std::string("");
    r.approved_at = std::nullopt;
    r.refused_at = std::nullopt;
    r.refusal_comment = std::string("");
    r.retention_months = 25;
    r.retention_destination_code = std::string("cold");
    r.legal_hold = false;
    r.output_is_indicative = false;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::cut> generate_synthetic_cuts(std::size_t n,
                                                 utility::generation::generation_context& ctx) {
    std::vector<domain::cut> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_cut(ctx));
    return r;
}

}
