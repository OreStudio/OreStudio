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
#include "ores.reporting/generators/report_instance_generator.hpp"

#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::reporting::generators {

using ores::utility::generation::generation_keys;

domain::report_instance generate_synthetic_report_instance(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");

    domain::report_instance r;
    r.version = 1;
    r.id = ctx.generate_uuid();
    r.party_id = boost::uuids::random_generator()();
    r.definition_id = boost::uuids::random_generator()();
    r.fsm_state_id = std::nullopt;
    r.trigger_run_id = faker::number::integer<std::int64_t>(1, 1000000);
    r.output_message = std::string();
    r.started_at = std::nullopt;
    r.completed_at = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.new";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::report_instance>
generate_synthetic_report_instances(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::report_instance> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_report_instance(ctx));
    return r;
}

}
