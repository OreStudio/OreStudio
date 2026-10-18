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
#include "ores.database/domain/context.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/service/next_fire_plan.hpp"
#include "ores.scheduler.api/messaging/scheduling_operations_protocol.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

// The decision these pin: the next fire time is the scheduler's, joined to a
// definition by the scheduler job the definition holds. A definition with no job
// is run on demand and states no time; a job the scheduler states no time for
// states none either. Neither is a time the reporting service made up.

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[service][database]");

/**
 * @brief A party of this test's own, with the session scoped to it.
 */
ores::database::context
write_test_party_and_scope_context(ores::testing::scoped_database_helper& h,
                                   ores::utility::generation::generation_context& gen) {
    using ores::refdata::repository::party_repository;
    party_repository parties;
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& existing : parties.read_latest(h.context())) {
        if (existing.tenant_id == party.tenant_id) {
            party.parent_party_id = existing.id;
            break;
        }
    }
    parties.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

/**
 * @brief Writes a definition, scheduled or not, and answers its id.
 */
boost::uuids::uuid write_definition(const ores::database::context& party_ctx,
                                    ores::utility::generation::generation_context& gen,
                                    const std::optional<boost::uuids::uuid>& scheduler_job_id) {
    auto definition = ores::reporting::generators::generate_synthetic_report_definition(gen);
    definition.change_reason_code = "system.test";
    definition.party_id = *party_ctx.party_id();
    definition.scheduler_job_id = scheduler_job_id;
    ores::reporting::repository::report_definition_repository().write(party_ctx, definition);
    return definition.id;
}

std::optional<std::string>
next_fire_of(const std::vector<ores::reporting::messaging::definition_next_fire>& schedules,
             const boost::uuids::uuid& definition_id) {
    for (const auto& schedule : schedules) {
        if (schedule.definition_id == definition_id)
            return schedule.next_fire_at;
    }
    return std::nullopt;
}

}

using ores::testing::scoped_database_helper;

TEST_CASE("a definition's schedule reads the next fire the scheduler states for its job", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);

    // One definition the party scheduled and one it runs on demand. The job id
    // is the definition's own, stated by the test so the join can be read back.
    const auto job_id = gen.generate_uuid();
    const auto scheduled = write_definition(ctx, gen, job_id);
    const auto on_demand = write_definition(ctx, gen, std::nullopt);

    // The definitions are read back the way the read reads them, so the join
    // column is proved to survive the store.
    ores::reporting::repository::report_definition_repository definitions;
    const auto read_back = definitions.read_latest(
        ctx, {boost::uuids::to_string(scheduled), boost::uuids::to_string(on_demand)});
    REQUIRE(read_back.size() == 2);

    const auto job_of =
        [&read_back](const boost::uuids::uuid& definition_id) -> std::optional<boost::uuids::uuid> {
        for (const auto& definition : read_back) {
            if (definition.id == definition_id)
                return definition.scheduler_job_id;
        }
        return std::nullopt;
    };
    REQUIRE(job_of(scheduled).has_value());
    CHECK(*job_of(scheduled) == job_id);
    CHECK_FALSE(job_of(on_demand).has_value());

    // The scheduler states a time for the job it runs, and states none for a
    // paused one. A job no definition holds is nobody's next fire.
    const std::string stated_next_fire("2026-10-18T09:00:00Z");
    const std::vector<ores::scheduler::messaging::job_schedule_status> running_job{
        {.job_definition_id = boost::uuids::to_string(job_id),
         .job_name = "report",
         .schedule_expression = "0 9 * * *",
         .is_active = true,
         .next_fire_at = stated_next_fire},
        {.job_definition_id = boost::uuids::to_string(gen.generate_uuid()),
         .job_name = "another component's job",
         .schedule_expression = "0 10 * * *",
         .is_active = true,
         .next_fire_at = std::string("2026-10-18T10:00:00Z")}};

    const auto schedules = ores::reporting::service::plan_next_fire(read_back, running_job);
    REQUIRE(schedules.size() == 2);
    CHECK(next_fire_of(schedules, scheduled) == stated_next_fire);
    CHECK_FALSE(next_fire_of(schedules, on_demand).has_value());

    // A paused job states no next fire, and the definition that holds it reads
    // none rather than a stale or invented time.
    const std::vector<ores::scheduler::messaging::job_schedule_status> paused_job{
        {.job_definition_id = boost::uuids::to_string(job_id),
         .job_name = "report",
         .schedule_expression = "0 9 * * *",
         .is_active = false,
         .next_fire_at = std::nullopt}};
    const auto paused = ores::reporting::service::plan_next_fire(read_back, paused_job);
    CHECK_FALSE(next_fire_of(paused, scheduled).has_value());
}
