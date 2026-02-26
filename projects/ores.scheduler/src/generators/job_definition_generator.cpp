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
#include "ores.scheduler/generators/job_definition_generator.hpp"

#include <faker-cxx/word.h>
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::scheduler::generators {

domain::job_definition generate_synthetic_job_definition(
    const utility::uuid::tenant_id& tenant_id,
    const boost::uuids::uuid& party_id) {
    static int counter = 0;

    utility::uuid::uuid_v7_generator gen;

    // Build a unique job name using faker
    const auto noun = std::string(faker::word::noun());
    const std::string job_name = noun + "_job_" + std::to_string(++counter);

    // Use a simple daily schedule for synthetic jobs
    auto schedule = domain::cron_expression::from_string("0 0 * * *");

    return domain::job_definition{
        .id = gen(),
        .tenant_id = tenant_id,
        .party_id = party_id,
        .cron_job_id = std::nullopt,
        .job_name = job_name,
        .description = "Synthetic job: " + noun,
        .command = "SELECT 1; -- synthetic no-op",
        .schedule_expression = std::move(*schedule),
        .database_name = "ores_default",
        .is_active = true,
        .version = 0,
        .modified_by = "test_generator",
    };
}

} // namespace ores::scheduler::generators
