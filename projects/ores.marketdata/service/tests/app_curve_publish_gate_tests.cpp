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
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.marketdata.service/app/curve_publish_gate.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::inbox::service::approval_lifecycle;
using ores::marketdata::service::app::curve_publish_kind_code;
using ores::marketdata::service::app::raise_publish_request;
using ores::marketdata::service::app::verdict_on_publish;

/**
 * @brief An account of the test tenant, and the context that acts as it.
 *
 * The database stamps every row with the account that wrote it, so a context
 * that names no account cannot raise a request.
 */
struct person {
    ores::iam::domain::account account;
    ores::database::context ctx;
};

person seed_person(ores::testing::scoped_database_helper& h) {
    auto gctx = ores::testing::make_generation_context(h);
    auto a = ores::iam::generators::generate_synthetic_account(gctx);
    // A person's account written into the shared test tenant would end its
    // bootstrap, so the account is a service account, as the other approval
    // suites make theirs.
    a.account_type = "service";
    a.change_reason_code = "system.test";
    ores::iam::repository::account_repository{}.write(h.context(), a);
    return {.account = a, .ctx = h.context().with_tenant(h.tenant_id(), a.username)};
}

}

TEST_CASE("a_publish_that_names_no_request_is_refused_and_states_the_kind", "[curve_publish][approval]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_person(h);

    const auto verdict = verdict_on_publish(maker.ctx, "");

    CHECK_FALSE(verdict.approved);
    CHECK(verdict.state_code.empty());
    CHECK(verdict.message.find(std::string(curve_publish_kind_code)) != std::string::npos);
}

TEST_CASE("asking_for_a_publish_raises_the_market_data_kind_for_the_person_who_asked", "[curve_publish][approval]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_person(h);

    const auto raised = raise_publish_request(maker.ctx, "The desk prices off this curve");

    CHECK(raised.kind_code == curve_publish_kind_code);
    CHECK(raised.state_code == "waiting");
    CHECK(raised.requested_by == maker.account.id);
    // The kind carries a review window, so a request states when it stops
    // waiting rather than staying in a queue nobody reads.
    CHECK(raised.expires_at.has_value());

    // The publish waits: the request it names is not approved yet.
    const auto verdict = verdict_on_publish(maker.ctx, boost::uuids::to_string(raised.id));
    CHECK_FALSE(verdict.approved);
    CHECK(verdict.state_code == "waiting");
}

TEST_CASE("the_market_risk_decider_sees_the_request_and_approving_it_permits_the_publish", "[curve_publish][approval]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_person(h);
    const auto decider = seed_person(h);

    const auto raised = raise_publish_request(maker.ctx, "Today's bootstrapped curve");
    const auto id = boost::uuids::to_string(raised.id);

    approval_lifecycle deciders_view(decider.ctx);
    const auto queue = deciders_view.queue({.kind_codes = {std::string(curve_publish_kind_code)}},
                                           decider.account.id,
                                           0,
                                           500);
    const auto listed = std::ranges::any_of(
        queue.requests, [&](const auto& r) { return r.id == raised.id; });
    CHECK(listed);

    const auto decided = deciders_view.decide(id, raised.version, "approve", decider.account.id, "");
    REQUIRE(decided.outcome == "ok");
    CHECK(decided.state_code == "approved");

    const auto verdict = verdict_on_publish(maker.ctx, id);
    CHECK(verdict.approved);
    CHECK(verdict.state_code == "approved");
    CHECK(verdict.message.empty());
}

TEST_CASE("a_refused_publish_request_does_not_permit_the_publish", "[curve_publish][approval]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_person(h);
    const auto decider = seed_person(h);

    const auto raised = raise_publish_request(maker.ctx, "A curve nobody checked");
    const auto id = boost::uuids::to_string(raised.id);
    const auto refused = approval_lifecycle(maker.ctx).decide(
        id, raised.version, "refuse", decider.account.id, "The fit is wrong");
    REQUIRE(refused.outcome == "ok");
    CHECK(refused.state_code == "refused");

    const auto verdict = verdict_on_publish(maker.ctx, id);
    CHECK_FALSE(verdict.approved);
    CHECK(verdict.state_code == "refused");
}

TEST_CASE("an_approved_request_of_another_kind_does_not_permit_a_publish", "[curve_publish][approval]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_person(h);
    const auto decider = seed_person(h);

    approval_lifecycle lifecycle(maker.ctx);
    const auto kind = lifecycle.kind("iam.role_grant");
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "Please", maker.account.id);
    const auto id = boost::uuids::to_string(raised.id);
    const auto approved = lifecycle.decide(id, raised.version, "approve", decider.account.id, "");
    REQUIRE(approved.state_code == "approved");

    const auto verdict = verdict_on_publish(maker.ctx, id);
    CHECK_FALSE(verdict.approved);
    CHECK(verdict.state_code == "approved");
    CHECK(verdict.message.find("iam.role_grant") != std::string::npos);
}
