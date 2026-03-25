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
#include "ores.reporting.api/generators/report_definition_generator.hpp"

#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <boost/uuid/string_generator.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::reporting::generators {

using ores::utility::generation::generation_keys;

domain::report_definition generate_synthetic_report_definition(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");
    const auto party_id_str = ctx.env().get_or(
        std::string(generation_keys::party_id), "");

    domain::report_definition r;
    r.version = 1;
    r.id = ctx.generate_uuid();
    r.name = std::string(faker::word::noun()) + "_report";
    r.party_id = party_id_str.empty()
        ? ctx.generate_uuid()
        : boost::uuids::string_generator{}(party_id_str);
    r.description = std::string(faker::lorem::sentence());
    r.report_type = std::string("risk");
    r.fsm_state_id = std::nullopt;
    r.schedule_expression = std::string("0 6 * * 1");
    r.concurrency_policy = std::string("skip");
    r.scheduler_job_id = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::report_definition>
generate_synthetic_report_definitions(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::report_definition> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_report_definition(ctx));
    return r;
}

}
