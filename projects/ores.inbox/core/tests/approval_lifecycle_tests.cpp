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
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/repository/approval_request_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>

namespace {

const std::string tags("[approval]");

ores::iam::domain::account seed_account(ores::testing::scoped_database_helper& h) {
    auto ctx = ores::testing::make_generation_context(h);
    auto a = ores::iam::generators::generate_synthetic_account(ctx);
    a.change_reason_code = "system.test";
    ores::iam::repository::account_repository repo;
    repo.write(h.context(), a);
    return a;
}

// The database stamps every row with the account that wrote it, and refuses a
// write that names nobody, as production's signed-in context always names one.
ores::database::context acting(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(h.tenant_id(), h.db_user());
}

// The context the sweep runs under. It comes from the scheduler, so nobody is
// signed in and there is no actor; the service account is the only name the
// write has. A sweep that reached for the actor instead of the service account
// would name nobody and be refused by the store.
ores::database::context sweeping(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(h.tenant_id(), "");
}

}

using ores::inbox::service::approval_lifecycle;

TEST_CASE("a_raised_request_waits_and_a_second_person_approves_it", tags) {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto kind = lifecycle.kind("iam.role_grant");
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "I cover the desk next week", asker.id);
    CHECK(raised.state_code == "waiting");
    CHECK(raised.requested_by == asker.id);
    // The role request kind carries a review window, so a request states the
    // moment it stops waiting. The window is two weeks; the tolerance is the
    // second the column keeps.
    REQUIRE(raised.expires_at);
    const auto window = *raised.expires_at - raised.requested_at;
    CHECK(window > std::chrono::days(13));
    CHECK(window < std::chrono::days(15));

    const auto id = boost::uuids::to_string(raised.id);
    const auto r = lifecycle.decide(id, raised.version, "approve", decider.id, "");
    CHECK(r.outcome == "ok");
    CHECK(r.state_code == "approved");
    CHECK(lifecycle.request(id)->state_code == "approved");
}

TEST_CASE("the_person_who_asked_cannot_decide_their_own_request", tags) {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto raised = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);

    CHECK_THROWS(lifecycle.decide(id, raised.version, "approve", asker.id, ""));
    CHECK(lifecycle.request(id)->state_code == "waiting");
}

TEST_CASE("a_decision_on_a_version_someone_else_moved_is_a_conflict", tags) {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto raised = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);

    const auto stale = lifecycle.decide(id, raised.version + 1, "approve", decider.id, "");
    CHECK(stale.outcome == "conflict");
    CHECK(lifecycle.request(id)->state_code == "waiting");
}

TEST_CASE("a_closed_request_cannot_be_decided_again", tags) {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto raised = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);
    const auto refused =
        lifecycle.decide(id, raised.version, "refuse", decider.id, "Not this desk");
    REQUIRE(refused.outcome == "ok");
    CHECK(refused.state_code == "refused");

    const auto again = lifecycle.decide(id, refused.version, "approve", decider.id, "");
    CHECK(again.outcome == "conflict");
    CHECK(lifecycle.request(id)->state_code == "refused");
}

TEST_CASE("a_refusal_needs_a_comment", tags) {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto raised = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);

    const auto r = lifecycle.decide(id, raised.version, "refuse", decider.id, "  ");
    CHECK(r.outcome == "invalid");
    CHECK(lifecycle.request(id)->state_code == "waiting");
}

TEST_CASE("a_kind_that_does_not_hold_refuses_a_hold", tags) {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto raised = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);

    const auto r =
        lifecycle.decide(id, raised.version, "hold", decider.id, "Waiting on the desk head");
    CHECK(r.outcome == "invalid");
}

TEST_CASE("only_the_person_who_asked_withdraws_a_request", tags) {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto other = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto raised = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);

    CHECK_THROWS(lifecycle.decide(id, raised.version, "withdraw", other.id, ""));
    const auto r = lifecycle.decide(id, raised.version, "withdraw", asker.id, "");
    CHECK(r.outcome == "ok");
    CHECK(r.state_code == "withdrawn");
}

TEST_CASE("the_queue_shows_a_decider_others_requests_and_not_their_own", tags) {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto raised = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Please", asker.id);
    const auto in = [&](const auto& page) {
        return std::ranges::any_of(page.requests, [&](const auto& r) { return r.id == raised.id; });
    };

    CHECK(in(lifecycle.queue({"iam.role_grant"}, decider.id, 0, 500)));
    CHECK_FALSE(in(lifecycle.queue({"iam.role_grant"}, asker.id, 0, 500)));
    CHECK_FALSE(in(lifecycle.queue({}, decider.id, 0, 500)));
    CHECK(in(lifecycle.raised_by(asker.id, 0, 100)));
    CHECK_FALSE(in(lifecycle.raised_by(decider.id, 0, 100)));
}

TEST_CASE("the_sweep_closes_what_ran_out_and_leaves_what_did_not", tags) {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto overdue = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Nobody will ask", asker.id);
    const auto in_time = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Still fresh", asker.id);
    const auto overdue_id = boost::uuids::to_string(overdue.id);
    const auto in_time_id = boost::uuids::to_string(in_time.id);

    const auto closed = [&](const auto& swept, const std::string& id) {
        return std::ranges::any_of(swept, [&](const auto& e) { return e.request_id == id; });
    };

    // Neither is due: the fortnight has not passed, so the sweep closes
    // nothing and both requests stay in front of a decider.
    approval_lifecycle sweep(sweeping(h));
    const auto first = sweep.expire_overdue();
    CHECK_FALSE(closed(first, overdue_id));
    CHECK(lifecycle.request(overdue_id)->state_code == "waiting");

    // Move one deadline behind us, which is what waiting the fortnight comes
    // to. The write is the ordinary one, so the row gains a version.
    auto lapsed = *lifecycle.request(overdue_id);
    lapsed.expires_at = std::chrono::system_clock::now() - std::chrono::hours(1);
    lapsed.change_reason_code = "system.test";
    ores::inbox::repository::approval_request_repository repo;
    repo.write(acting(h), lapsed);

    const auto second = sweep.expire_overdue();
    CHECK(closed(second, overdue_id));
    CHECK_FALSE(closed(second, in_time_id));

    const auto after = lifecycle.request(overdue_id);
    REQUIRE(after);
    CHECK(after->state_code == "expired");
    CHECK(lifecycle.request(in_time_id)->state_code == "waiting");
    // The closed row names the service, which is who ran the sweep.
    CHECK(after->modified_by == sweeping(h).service_account());

    // A request it already closed is no longer open, so the next sweep is a
    // no-op rather than a second closure.
    CHECK_FALSE(closed(sweep.expire_overdue(), overdue_id));
    CHECK(lifecycle.request(overdue_id)->version == after->version);
}
