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
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

/**
 * @file approval_maker_checker_tests.cpp
 * @brief The maker and checker pair: one permission to raise, another to decide.
 *
 * The pair is the kind's, never a request's: a maker who could name a weaker
 * checker would choose their own control. Four eyes stays a rule of the record,
 * so an account that holds both halves still cannot decide its own request.
 *
 * The permissions are read from the context's own list, which is the list a
 * service builds from the caller's token. A context that carries none is the
 * service's own, and its checks pass, which is why the trusted cases below name
 * no roles.
 */

namespace {

using ores::inbox::service::approval_lifecycle;
using ores::inbox::tests::acting;
using ores::inbox::tests::seed_account;

/// The kind the market data journeys name for a publish, and its maker's code.
const std::string curve_publish_kind = "marketdata.curve_publish";
const std::string curve_publish_permission = "marketdata::curve_bootstrap:publish";
const std::string market_risk_decision = "inbox::approvals:decide_market_risk";

/**
 * @brief A context that acts as one person and holds exactly these codes.
 *
 * The list is the token's, so a code it leaves out is one the person does not
 * hold. Codes that grant another are not implied: grants() applies the wildcard
 * rule the services apply, and nothing else.
 */
ores::database::context holding(ores::testing::scoped_database_helper& h,
                                std::vector<std::string> codes) {
    auto ctx = h.context().with_tenant(h.tenant_id(), acting(h).actor());
    return ctx.with_roles(std::move(codes));
}

}

TEST_CASE("a_raise_without_the_kinds_raise_permission_is_refused", "[approval][maker_checker]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_account(h);
    const auto planner = seed_account(h);

    // The seeded curve publish kind names both halves of the pair.
    approval_lifecycle trusted(acting(h));
    const auto kind = trusted.kind(curve_publish_kind);
    REQUIRE(kind);
    REQUIRE(kind->raise_permission_code.has_value());
    CHECK(*kind->raise_permission_code == curve_publish_permission);

    // An account that holds the decider's code and not the maker's may not raise.
    approval_lifecycle as_planner(holding(h, {market_risk_decision}));
    CHECK_FALSE(as_planner.may_raise(*kind));

    // The maker's code is what raises it, and it is not the decider's: the two
    // halves are different people's rights.
    approval_lifecycle as_maker(holding(h, {curve_publish_permission}));
    CHECK(as_maker.may_raise(*kind));
    CHECK_FALSE(as_maker.may_decide(*kind));
}

TEST_CASE("a_decider_without_the_kinds_decide_permission_is_refused",
          "[approval][maker_checker]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_account(h);
    const auto planner = seed_account(h);

    approval_lifecycle trusted(acting(h));
    const auto kind = trusted.kind(curve_publish_kind);
    REQUIRE(kind);

    approval_lifecycle as_maker(holding(h, {curve_publish_permission}));
    CHECK_FALSE(as_maker.may_decide(*kind));

    approval_lifecycle as_decider(holding(h, {market_risk_decision}));
    CHECK(as_decider.may_decide(*kind));
}

TEST_CASE("the_pair_is_the_kinds_and_no_request_widens_it", "[approval][maker_checker]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_account(h);
    const auto decider = seed_account(h);

    approval_lifecycle lifecycle(acting(h));
    const auto kind = lifecycle.kind(curve_publish_kind);
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "Today's curve", maker.id);
    const auto id = boost::uuids::to_string(raised.id);

    // A request carries no permission of its own: what it may be decided under is
    // the kind's, so the decider holds the kind's code and nobody elses's helps.
    approval_lifecycle as_decider(holding(h, {market_risk_decision}));
    CHECK(as_decider.may_decide(*kind));
    approval_lifecycle as_stranger(holding(h, {"refdata::currencies:read"}));
    CHECK_FALSE(as_stranger.may_decide(*kind));

    // And the person who asked still cannot decide it, whichever half they hold.
    approval_lifecycle as_asker(holding(h, {curve_publish_permission, market_risk_decision}));
    CHECK(as_asker.may_decide(*kind));
    CHECK_THROWS(as_asker.decide(id, raised.version, "approve", maker.id, ""));
    CHECK(lifecycle.request(id)->state_code == "waiting");
}

TEST_CASE("the_queue_is_filtered_by_the_kinds_decide_permission", "[approval][maker_checker]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_account(h);
    const auto decider = seed_account(h);
    const auto stranger = seed_account(h);

    approval_lifecycle trusted(acting(h));
    const auto kind = trusted.kind(curve_publish_kind);
    REQUIRE(kind);
    const auto raised = trusted.raise(*kind, "Today's curve", maker.id);

    // A decider holding the kind's code finds the request.
    approval_lifecycle as_decider(holding(h, {market_risk_decision}));
    const auto scope = as_decider.decidable_by_role();
    CHECK(std::ranges::find(scope.kind_codes, curve_publish_kind) != scope.kind_codes.end());
    const auto page = as_decider.queue(scope, decider.id, 0, 100);
    CHECK(std::ranges::any_of(page.requests, [&](const auto& r) { return r.id == raised.id; }));

    // A person holding the maker's code and not the decider's finds nothing: the
    // queue is the checker's, and the maker's half does not open it.
    approval_lifecycle as_maker(holding(h, {curve_publish_permission}));
    const auto maker_scope = as_maker.decidable_by_role();
    CHECK(std::ranges::find(maker_scope.kind_codes, curve_publish_kind) ==
          maker_scope.kind_codes.end());
    CHECK(as_maker.queue(maker_scope, stranger.id, 0, 100).requests.empty());

    // The asker never sees their own request, whatever they hold. The queue is
    // read without a page of its own, because the tenant's other requests are
    // not this test's to know about.
    approval_lifecycle as_asker(holding(h, {market_risk_decision}));
    const auto asker_scope = as_asker.decidable_by_role();
    const auto asker_page = as_asker.queue(asker_scope, maker.id, 0, 500);
    CHECK_FALSE(std::ranges::any_of(asker_page.requests,
                                    [&](const auto& r) { return r.id == raised.id; }));
}

TEST_CASE("a_kind_that_names_no_raise_permission_keeps_the_rule_it_had",
          "[approval][maker_checker]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_account(h);

    approval_lifecycle lifecycle(acting(h));
    const auto kind = lifecycle.kind("iam.role_grant");
    REQUIRE(kind);
    REQUIRE_FALSE(kind->raise_permission_code.has_value());

    // The role request names no maker's code, so anybody the component lets
    // through may raise it, and the decide half is untouched.
    approval_lifecycle as_member(holding(h, {"refdata::currencies:read"}));
    CHECK(as_member.may_raise(*kind));
    CHECK_FALSE(as_member.may_decide(*kind));
}
