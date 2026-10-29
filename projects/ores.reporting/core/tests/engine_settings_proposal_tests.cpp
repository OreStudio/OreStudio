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
#include "engine_settings_proposal_fixtures.hpp"
#include "ores.reporting.api/domain/engine_settings_group_setting.hpp"
#include "ores.reporting.core/repository/engine_settings_group_setting_repository.hpp"
#include "ores.reporting.service/service/engine_settings_proposal.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>

// A numbers-changing group of engine run settings never changes in place. These
// tests drive the lifecycle the configuration screen standard states: a proposal
// while a version is in force, the person who asked kept out of the decision, and
// the apply writing the group once the request is approved.

namespace {

using namespace ores::reporting::tests;
using ores::reporting::service::engine_settings_proposal_service;
using ores::utility::domain::outcome;

/** One proposed value for a setting, as the screen sends it. */
ores::reporting::messaging::engine_settings_value value(const std::string& code,
                                                        const std::string& text) {
    return {.setting_code = code, .text_value = text};
}

/** Every value of the fixings group, as its settings in force state them. */
std::vector<ores::reporting::messaging::engine_settings_value>
fixings_values(bool imply_todays_fixings) {
    return {value("fixings:imply_todays_fixings", imply_todays_fixings ? "true" : "false"),
            value("fixings:include_todays_cash_flows", ""),
            value("fixings:include_reference_date_events", ""),
            value("fixings:enrich_index_fixings", ""),
            value("fixings:ignore_fixing_lag", ""),
            value("fixings:ignore_fixing_lead", "")};
}

ores::reporting::messaging::raise_engine_settings_change_request
raise_request(const world& w, bool imply_todays_fixings) {
    return {.engine_settings_id = w.bound_id,
            .group_code = "fixings",
            .base_version = 0,
            .values = fixings_values(imply_todays_fixings),
            .reason = "Turn on today's fixings"};
}

}

TEST_CASE("a_numbers_changing_group_of_a_configuration_a_report_binds_is_proposed",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);

    CHECK(svc.needs_approval(w.bound_id, "fixings"));
    CHECK(svc.needs_approval(w.bound_id, "building"));
    // The execution group is operational, and a draft is not governed at all.
    CHECK_FALSE(svc.needs_approval(w.bound_id, "execution"));
    CHECK_FALSE(svc.needs_approval(w.draft_id, "fixings"));
}

TEST_CASE("a_proposal_holds_its_values_and_the_version_in_force_does_not_move",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);

    const auto raised = svc.raise(raise_request(w, true));

    REQUIRE(raised.result.outcome == outcome::ok);
    REQUIRE_FALSE(raised.request_id.empty());
    // Nothing a run reads has changed: the group has no row in force and a
    // proposal does not write one.
    CHECK_FALSE(fixings_of(w.ctx, w.bound_id).has_value());

    const auto lines = svc.lines(raised.request_id);
    REQUIRE(lines.size() == 1);
    CHECK(lines.front().group_code == "fixings");
    CHECK(lines.front().base_version == 0);
    CHECK_FALSE(lines.front().applied);
    const auto changed =
        std::ranges::find_if(lines.front().fields, [](const auto& f) { return f.changed; });
    REQUIRE(changed != lines.front().fields.end());
    CHECK(changed->column_name == "imply_todays_fixings");
    CHECK(changed->after == "true");
}

TEST_CASE("a_draft_changes_in_place_and_is_refused_as_a_proposal",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);

    auto request = raise_request(w, true);
    request.engine_settings_id = w.draft_id;
    const auto raised = svc.raise(request);

    CHECK(raised.result.outcome == outcome::invalid);
    CHECK(raised.request_id.empty());
    CHECK(raised.result.message.find("draft") != std::string::npos);
}

TEST_CASE("the_execution_group_is_refused_as_a_proposal", "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);

    const auto raised = svc.raise({.engine_settings_id = w.bound_id,
                                   .group_code = "execution",
                                   .base_version = 0,
                                   .values = {value("execution:n_threads", "4")},
                                   .reason = "Use four threads"});

    CHECK(raised.result.outcome == outcome::invalid);
    CHECK(raised.result.message.find("operational") != std::string::npos);
}

TEST_CASE("the_kind_names_one_part_for_each_number_changing_group",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);

    CHECK(svc.part_codes("fixings") == std::vector<std::string>{"fixings"});
    CHECK(svc.part_codes("building") == std::vector<std::string>{"building"});
    CHECK(svc.part_codes("execution").empty());

    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto named = lifecycle.part_codes_of_kind("reporting.engine_settings_change");
    CHECK(named == std::vector<std::string>{"fixings", "building"});
}

TEST_CASE("the_proposer_cannot_sign_off_their_own_proposal",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);
    const auto raised = svc.raise(raise_request(w, true));
    REQUIRE(raised.result.outcome == outcome::ok);

    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto request = lifecycle.request(raised.request_id);
    REQUIRE(request.has_value());
    CHECK(request->requested_by == w.maker_id);

    // The rule is the schema's, so the refusal is the database's own refusal and
    // not a check this service could be talked out of.
    CHECK_THROWS_AS(lifecycle.decide(raised.request_id,
                                     request->version,
                                     "approve",
                                     w.maker_id,
                                     "I approve my own change",
                                     "fixings"),
                    std::exception);
}

TEST_CASE("an_approved_proposal_writes_the_group_and_marks_the_line_applied",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);
    const auto raised = svc.raise(raise_request(w, true));
    REQUIRE(raised.result.outcome == outcome::ok);

    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto request = lifecycle.request(raised.request_id);
    REQUIRE(request.has_value());
    // The decider is another person, holding the part the proposal names.
    const auto decided = lifecycle.decide(
        raised.request_id, request->version, "approve", w.decider_id, "", "fixings");
    REQUIRE(decided.outcome == "ok");
    CHECK(decided.state_code == "approved");

    const auto applied = svc.apply(raised.request_id);
    REQUIRE(applied.applied);
    CHECK(applied.groups_applied == 1);

    const auto in_force = fixings_of(w.ctx, w.bound_id);
    REQUIRE(in_force.has_value());
    CHECK(in_force->imply_todays_fixings == std::optional<bool>(true));

    const auto lines = svc.lines(raised.request_id);
    REQUIRE(lines.size() == 1);
    CHECK(lines.front().applied);

    // A second apply finds nothing to do, because a line applies once.
    const auto again = svc.apply(raised.request_id);
    CHECK(again.applied);
    CHECK(again.groups_applied == 0);
}

TEST_CASE("a_group_that_moved_since_the_proposal_is_refused_rather_than_overwritten",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);
    const auto raised = svc.raise(raise_request(w, true));
    REQUIRE(raised.result.outcome == outcome::ok);

    // Somebody writes the group directly while the proposal waits, which is
    // what a stale claim is for.
    write_fixings(w.ctx, w, false);

    const auto refusals = svc.refusals(raised.request_id);
    REQUIRE(refusals.size() == 1);
    CHECK(refusals.front().find("moved") != std::string::npos);

    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto request = lifecycle.request(raised.request_id);
    REQUIRE(request.has_value());
    const auto decided = lifecycle.decide(
        raised.request_id, request->version, "approve", w.decider_id, "", "fixings");
    REQUIRE(decided.state_code == "approved");

    const auto applied = svc.apply(raised.request_id);
    CHECK_FALSE(applied.applied);
    CHECK(applied.refusal.find("moved") != std::string::npos);
    // The refusal ends the request, and the row somebody else wrote stands.
    const auto after = lifecycle.request(raised.request_id);
    REQUIRE(after.has_value());
    CHECK(after->state_code == "apply_failed");
    const auto in_force = fixings_of(w.ctx, w.bound_id);
    REQUIRE(in_force.has_value());
    CHECK(in_force->imply_todays_fixings == std::optional<bool>(false));
}

TEST_CASE("a_proposal_against_a_version_that_moved_is_refused_as_stale",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);
    // The group is at version 1 before anybody opens the screen.
    write_fixings(w.ctx, w, false);

    // A client that read version 0 is stale, whatever it sends.
    const auto held_before = all_change_lines(w.ctx);
    auto request = raise_request(w, true);
    request.base_version = 0;
    const auto refused = svc.raise(request);

    CHECK(refused.result.outcome == outcome::conflict);
    // The words the screen shows, so a person reads one refusal.
    CHECK(refused.result.message ==
          "These settings changed since you opened them. Read them again before you change them.");
    // Nothing was held: no request, and no line added for one.
    CHECK(refused.request_id.empty());
    CHECK(all_change_lines(w.ctx) == held_before);

    // A client that read the version in force is answered, and the proposal
    // claims that version rather than the one it was told.
    auto current = raise_request(w, true);
    current.base_version = 1;
    const auto raised = svc.raise(current);
    REQUIRE(raised.result.outcome == outcome::ok);
    REQUIRE_FALSE(raised.request_id.empty());
    CHECK(change_lines(w.ctx, raised.request_id) == 6);

    const auto lines = svc.lines(raised.request_id);
    REQUIRE(lines.size() == 1);
    CHECK(lines.front().base_version == 1);
}

TEST_CASE("a_proposal_holds_no_line_when_the_version_it_claims_is_stale",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);
    write_fixings(w.ctx, w, false);
    write_fixings(w.ctx, w, true); // now at version 2

    // The claim is ahead of the group, which is just as stale as one behind it.
    const auto held_before = all_change_lines(w.ctx);
    auto ahead = raise_request(w, false);
    ahead.base_version = 5;
    const auto refused = svc.raise(ahead);
    CHECK(refused.result.outcome == outcome::conflict);
    CHECK(refused.result.message ==
          "These settings changed since you opened them. Read them again before you change them.");

    // The group in force is untouched, and no line was written for the claim.
    const auto in_force = fixings_of(w.ctx, w.bound_id);
    REQUIRE(in_force.has_value());
    CHECK(in_force->imply_todays_fixings == std::optional<bool>(true));
    CHECK(in_force->version == 2);
    // Nothing was held for the claim.
    CHECK(all_change_lines(w.ctx) == held_before);
}

TEST_CASE("a_whole_too_large_to_hold_is_refused_rather_than_retried",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);

    // A number that overflows an integer is content the setting cannot hold, so
    // the refusal is permanent. Thrown as out_of_range, the apply would read it
    // as an infrastructure failure and retry it forever.
    auto huge = raise_request(w, true);
    huge.values.push_back(value("fixings:ignore_fixing_lag", "99999999999999999999"));
    const auto refused = svc.raise(huge);
    CHECK(refused.result.outcome == outcome::invalid);
    CHECK(refused.result.message.find("number the setting can hold") != std::string::npos);
    CHECK(refused.request_id.empty());

    // Text that is not a whole is refused the same way.
    auto letters = raise_request(w, true);
    letters.values.push_back(value("fixings:ignore_fixing_lag", "12x"));
    const auto wrong = svc.raise(letters);
    CHECK(wrong.result.outcome == outcome::invalid);
    CHECK(wrong.result.message.find("digits") != std::string::npos);
    CHECK(wrong.request_id.empty());
}

TEST_CASE("a_held_value_too_large_to_hold_ends_the_apply_rather_than_retrying_it",
          "[engine_settings_proposal][database]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    engine_settings_proposal_service svc(w.ctx);
    const auto raised = svc.raise(raise_request(w, true));
    REQUIRE(raised.result.outcome == outcome::ok);

    // A line nobody validated this time: a value that overflows a whole is
    // content the setting cannot hold, so the apply must refuse it for good.
    set_held_value(w.ctx, raised.request_id, "fixings:ignore_fixing_lag", "99999999999999999999");

    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto request = lifecycle.request(raised.request_id);
    REQUIRE(request.has_value());
    const auto decided = lifecycle.decide(
        raised.request_id, request->version, "approve", w.decider_id, "", "fixings");
    REQUIRE(decided.state_code == "approved");

    const auto applied = svc.apply(raised.request_id);
    CHECK_FALSE(applied.applied);
    // A refusal, not a transient failure: a retry would meet the same value.
    CHECK_FALSE(applied.transient);
    CHECK(applied.refusal.find("number the setting can hold") != std::string::npos);

    const auto after = lifecycle.request(raised.request_id);
    REQUIRE(after.has_value());
    CHECK(after->state_code == "apply_failed");
    // The group in force did not move.
    CHECK_FALSE(fixings_of(w.ctx, w.bound_id).has_value());
}
