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
#include "ores.iam.core/messaging/provision_step_arguments.hpp"
#include "ores.iam.core/messaging/report_schedule_selection.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstring>
#include <fstream>
#include <iterator>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <string>
#include <vector>

/**
 * @file provision_schedule_reports_tests.cpp
 * @brief What the schedule_reports step schedules, and what it leaves alone.
 *
 * A provisioned tenant ticks nothing on its own: the catalogue states each
 * report's schedule, but a run needs a scheduler job and a run grant, and only
 * the reporting schedule operation creates the pair. The step that follows the
 * parties calls that operation for every report a party publishes, and this
 * test holds the two ends of it: which definitions it asks for, and the row of
 * the starting point that orders it.
 */

namespace {

using ores::iam::messaging::on_demand_schedule_expression;
using ores::iam::messaging::report_schedule_candidate;
using ores::iam::messaging::select_report_definitions_to_schedule;

const std::string
    profile_seed_path("projects/ores.sql/populate/iam/iam_seed_profiles_populate.sql");

/// One step row of a seed profile, as the SQL states it.
struct profile_step_row {
    std::string kind;
    int display_order = 0;
    std::string arguments_json;
};

std::string read_seed_file(const std::string& relative_path) {
    std::ifstream in(ores::testing::project_root::resolve(relative_path));
    REQUIRE(in.good());
    return {std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
}

/// Every step row of a profile, read from the seed the way the database would
/// read it: the kind, the order, and the arguments that follow.
///
/// The scan is confined to the block that inserts the steps, because the
/// parameters block names the same profile and a parameter is not a step.
std::vector<profile_step_row> step_rows(const std::string& seed, const std::string& profile_code) {
    const auto block_begin = seed.find("insert into ores_iam_seed_profile_steps_tbl");
    REQUIRE(block_begin != std::string::npos);
    const auto block_end =
        seed.find("insert into ores_iam_seed_profile_parameters_tbl", block_begin);
    REQUIRE(block_end != std::string::npos);
    const auto steps_block = seed.substr(block_begin, block_end - block_begin);

    std::vector<profile_step_row> rows;
    const std::string prefix = "('" + profile_code + "', '";
    auto at = steps_block.find(prefix);
    while (at != std::string::npos) {
        const auto kind_begin = at + prefix.size();
        const auto kind_end = steps_block.find('\'', kind_begin);
        REQUIRE(kind_end != std::string::npos);

        const auto order_begin = kind_end + std::strlen("', ");
        const auto order_end = steps_block.find(',', order_begin);
        REQUIRE(order_end != std::string::npos);

        const auto arguments_begin = steps_block.find("'", order_end) + 1;
        const auto arguments_end = steps_block.find("'::jsonb)", arguments_begin);
        REQUIRE(arguments_end != std::string::npos);

        profile_step_row row;
        row.kind = steps_block.substr(kind_begin, kind_end - kind_begin);
        row.display_order = std::stoi(steps_block.substr(order_begin, order_end - order_begin));
        row.arguments_json = steps_block.substr(arguments_begin, arguments_end - arguments_begin);
        rows.push_back(std::move(row));
        at = steps_block.find(prefix, arguments_end);
    }
    return rows;
}

const profile_step_row* find_step(const std::vector<profile_step_row>& rows,
                                  const std::string& kind) {
    for (const auto& row : rows)
        if (row.kind == kind)
            return &row;
    return nullptr;
}

}

TEST_CASE("a definition with a real cron is selected for scheduling", "[schedule_reports]") {
    const auto selection = select_report_definitions_to_schedule(
        {{.id = "eod", .schedule_expression = "0 6 * * 1-5"},
         {.id = "headline", .schedule_expression = "*/10 * * * *"},
         {.id = "monthly", .schedule_expression = "0 6 1 * *"}});

    CHECK(selection.ids == std::vector<std::string>{"eod", "headline", "monthly"});
    CHECK(selection.on_demand == 0);
    CHECK(selection.already_scheduled == 0);
    CHECK(selection.not_a_cron == 0);
}

TEST_CASE("an on-demand report is left for a person to ask for", "[schedule_reports]") {
    const auto selection = select_report_definitions_to_schedule(
        {{.id = "eod", .schedule_expression = "0 6 * * 1-5"},
         {.id = "bump", .schedule_expression = std::string(on_demand_schedule_expression)},
         {.id = "strike_map", .schedule_expression = std::string(on_demand_schedule_expression)}});

    // The placeholder is not a fault: the catalogue states it deliberately, so
    // the step passes it over and keeps going rather than failing the run.
    CHECK(selection.ids == std::vector<std::string>{"eod"});
    CHECK(selection.on_demand == 2);
}

TEST_CASE("a schedule that is not a cron is passed over", "[schedule_reports]") {
    const auto selection = select_report_definitions_to_schedule(
        {{.id = "broken", .schedule_expression = "every day"},
         {.id = "eod", .schedule_expression = "0 6 * * 1-5"}});

    CHECK(selection.ids == std::vector<std::string>{"eod"});
    CHECK(selection.not_a_cron == 1);
}

TEST_CASE("a second provision asks for nothing the first already scheduled", "[schedule_reports]") {
    const auto selection = select_report_definitions_to_schedule(
        {{.id = "eod", .schedule_expression = "0 6 * * 1-5", .holds_scheduler_job = true},
         {.id = "headline", .schedule_expression = "*/10 * * * *", .holds_scheduler_job = true},
         {.id = "bump", .schedule_expression = std::string(on_demand_schedule_expression)}});

    // Scheduling is idempotent because the step asks for nothing: a definition
    // that already holds a job keeps it, so no second job and no second run
    // grant is created.
    CHECK(selection.ids.empty());
    CHECK(selection.already_scheduled == 2);
    CHECK(selection.on_demand == 1);
}

TEST_CASE("the acme profile orders the schedule step after the parties are provisioned",
          "[schedule_reports][seed]") {
    const auto rows = step_rows(read_seed_file(profile_seed_path), "acme_demo");
    REQUIRE(!rows.empty());

    const auto* schedule = find_step(rows, "schedule_reports");
    REQUIRE(schedule != nullptr);
    const auto* parties = find_step(rows, "provision_party");
    REQUIRE(parties != nullptr);
    const auto* staff = find_step(rows, "load_staff");
    REQUIRE(staff != nullptr);
    const auto* trades = find_step(rows, "generate_trades");
    REQUIRE(trades != nullptr);

    CHECK(schedule->display_order > parties->display_order);
    CHECK(schedule->display_order > staff->display_order);
    CHECK(schedule->display_order < trades->display_order);
}

TEST_CASE("the schedule row names the parties the staff step loads, and nothing else",
          "[schedule_reports][seed]") {
    const auto seed = read_seed_file(profile_seed_path);
    const auto rows = step_rows(seed, "acme_demo");

    const auto* schedule = find_step(rows, "schedule_reports");
    REQUIRE(schedule != nullptr);
    const auto* staff = find_step(rows, "load_staff");
    REQUIRE(staff != nullptr);

    const auto arguments =
        ores::iam::messaging::parse_report_schedule_arguments(schedule->arguments_json);
    const auto staff_assignments =
        ores::iam::messaging::parse_staff_assignments(staff->arguments_json);
    REQUIRE(staff_assignments.size() == 4);

    std::vector<std::string> staff_parties;
    for (const auto& assignment : staff_assignments)
        staff_parties.push_back(assignment.party_name);
    CHECK(arguments.parties == staff_parties);

    // The row states the parties and nothing else: which definitions a party
    // runs, and what each one's schedule is, are the catalogue's statements and
    // a copy of them here would go stale.
    const auto generic = rfl::json::read<rfl::Generic>(schedule->arguments_json);
    REQUIRE(generic);
    const auto* object = std::get_if<rfl::Object<rfl::Generic>>(&generic->get());
    REQUIRE(object != nullptr);
    CHECK(object->size() == 1);
    CHECK(object->count("parties") == 1);
}
