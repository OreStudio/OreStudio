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
#include "approval_fixture.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/service/approval_apply_retry.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>
#include <vector>

using ores::inbox::tests::seed_account;
using ores::inbox::tests::acting;
using ores::inbox::tests::sweeping;

namespace {

using ores::inbox::service::approval_apply_retry;
using ores::inbox::service::approval_lifecycle;

/**
 * @brief An engine that records what it was asked and answers as told.
 */
struct recording_engine final : ores::inbox::service::apply_engine {
    std::vector<ores::workflow::messaging::start_workflow_message> started;
    std::vector<std::string> retried;
    bool resumes = true;
    bool fails = false;
    std::string status = "failed";

    void start(const ores::workflow::messaging::start_workflow_message& start) override {
        if (fails)
            throw std::runtime_error("the engine is down");
        started.push_back(start);
    }

    ores::workflow::messaging::retry_workflow_instance_response
    retry(const std::string& workflow_instance_id) override {
        retried.push_back(workflow_instance_id);
        ores::workflow::messaging::retry_workflow_instance_response answer;
        answer.success = resumes;
        answer.message = resumes ? "" : "The run has not stopped, so there is nothing to resume.";
        answer.workflow_instance_id = workflow_instance_id;
        answer.step_name = "apply_book_changes";
        return answer;
    }

    std::string run_status(const std::string&) override {
        return status;
    }

    bool started_for(const std::string& request_id) const {
        return std::ranges::any_of(started,
                                   [&](const auto& s) { return s.target_id == request_id; });
    }
};

bool contains(const std::vector<std::string>& ids, const std::string& id) {
    return std::ranges::find(ids, id) != ids.end();
}

// An approved request of the book change kind, which names an apply workflow.
std::string approved_book_change(ores::testing::scoped_database_helper& h) {
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));
    const auto raised = lifecycle.raise(*lifecycle.kind("refdata.book_change"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(lifecycle.decide(id, raised.version, "approve", decider.id, "").state_code ==
            "approved");
    return id;
}

// The book change kind retries for a day after approval, so two days on its
// window has ended.
std::chrono::system_clock::time_point after_the_window() {
    return std::chrono::system_clock::now() + std::chrono::hours(48);
}

}

TEST_CASE("an_approved_request_inside_its_window_is_started_again_and_resumed",
          "[approval][apply_retry]") {
    ores::testing::scoped_database_helper h;
    const auto id = approved_book_change(h);
    recording_engine engine;

    const auto done = approval_apply_retry(sweeping(h), engine).run();

    CHECK(engine.started_for(id));
    CHECK(contains(done.resumed, id));
    CHECK_FALSE(contains(done.gave_up, id));
    // The same run is named every time, so the start repairs a lost one and
    // changes nothing when the run exists.
    const auto run =
        std::ranges::find_if(engine.started, [&](const auto& s) { return s.target_id == id; });
    REQUIRE(run != engine.started.end());
    CHECK(std::ranges::find(engine.retried, run->instance_id) != engine.retried.end());
    CHECK(approval_lifecycle(acting(h)).request(id)->state_code == "approved");
}

TEST_CASE("a_run_the_engine_will_not_resume_is_left_for_the_next_pass", "[approval][apply_retry]") {
    ores::testing::scoped_database_helper h;
    const auto id = approved_book_change(h);
    recording_engine engine;
    engine.resumes = false;

    const auto done = approval_apply_retry(sweeping(h), engine).run();

    CHECK(engine.started_for(id));
    CHECK_FALSE(contains(done.resumed, id));
    CHECK_FALSE(contains(done.gave_up, id));
    CHECK(approval_lifecycle(acting(h)).request(id)->state_code == "approved");
}

TEST_CASE("a_request_past_its_window_moves_to_apply_failed_and_is_not_retried",
          "[approval][apply_retry]") {
    ores::testing::scoped_database_helper h;
    const auto id = approved_book_change(h);
    recording_engine engine;

    const auto done = approval_apply_retry(sweeping(h), engine).run(after_the_window());

    CHECK(contains(done.gave_up, id));
    CHECK_FALSE(engine.started_for(id));
    const auto request = approval_lifecycle(acting(h)).request(id);
    CHECK(request->state_code == "apply_failed");
    CHECK(request->change_commentary == "could not apply");

    // Final: the next pass finds nothing to do for it.
    recording_engine again;
    const auto next = approval_apply_retry(sweeping(h), again).run(after_the_window());
    CHECK_FALSE(contains(next.gave_up, id));
    CHECK_FALSE(again.started_for(id));
}

TEST_CASE("a_request_in_another_state_is_never_retried", "[approval][apply_retry]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    approval_lifecycle lifecycle(acting(h));
    const auto raised = lifecycle.raise(*lifecycle.kind("refdata.book_change"), "Please", asker.id);
    const auto waiting = boost::uuids::to_string(raised.id);

    const auto failed_id = approved_book_change(h);
    REQUIRE(lifecycle.fail_apply(failed_id, "Line 1: refused").outcome == "ok");
    recording_engine engine;

    const auto done = approval_apply_retry(sweeping(h), engine).run();

    CHECK_FALSE(engine.started_for(waiting));
    CHECK_FALSE(engine.started_for(failed_id));
    CHECK_FALSE(contains(done.resumed, failed_id));
}

TEST_CASE("a_kind_that_names_no_workflow_is_never_retried", "[approval][apply_retry]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));
    const auto raised = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(lifecycle.decide(id, raised.version, "approve", decider.id, "").state_code ==
            "approved");
    recording_engine engine;

    const auto done = approval_apply_retry(sweeping(h), engine).run();

    CHECK_FALSE(engine.started_for(id));
    CHECK_FALSE(contains(done.gave_up, id));
    CHECK(lifecycle.request(id)->state_code == "approved");
}

TEST_CASE("an_engine_that_is_down_does_not_stop_the_pass", "[approval][apply_retry]") {
    ores::testing::scoped_database_helper h;
    const auto id = approved_book_change(h);
    recording_engine engine;
    engine.fails = true;

    const auto done = approval_apply_retry(sweeping(h), engine).run();

    CHECK_FALSE(contains(done.resumed, id));
    CHECK(approval_lifecycle(acting(h)).request(id)->state_code == "approved");
}

TEST_CASE("a_run_still_working_past_the_window_keeps_its_request_approved",
          "[approval][apply_retry]") {
    ores::testing::scoped_database_helper h;
    const auto id = approved_book_change(h);
    recording_engine engine;
    engine.status = "in_progress";

    const auto done = approval_apply_retry(sweeping(h), engine).run(after_the_window());

    CHECK_FALSE(contains(done.gave_up, id));
    CHECK_FALSE(engine.started_for(id));
    CHECK(approval_lifecycle(acting(h)).request(id)->state_code == "approved");
}

TEST_CASE("a_request_with_no_run_past_the_window_is_given_up", "[approval][apply_retry]") {
    ores::testing::scoped_database_helper h;
    const auto id = approved_book_change(h);
    recording_engine engine;
    engine.status = "";

    const auto done = approval_apply_retry(sweeping(h), engine).run(after_the_window());

    CHECK(contains(done.gave_up, id));
    CHECK(approval_lifecycle(acting(h)).request(id)->state_code == "apply_failed");
}
