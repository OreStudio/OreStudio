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
#include "ores.inbox.core/repository/approval_decision_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

// The operational authorisation kind is the one seeded kind that allows a hold,
// so it is the kind these cases park.
constexpr auto holding_kind = "trading.operational_authorisation";

ores::iam::domain::account seed_account(ores::testing::scoped_database_helper& h) {
    auto ctx = ores::testing::make_generation_context(h);
    auto a = ores::iam::generators::generate_synthetic_account(ctx);
    a.change_reason_code = "system.test";
    ores::iam::repository::account_repository repo;
    repo.write(h.context(), a);
    return a;
}

ores::database::context acting(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(h.tenant_id(), h.db_user());
}

}

using ores::inbox::service::approval_lifecycle;

TEST_CASE("a_hold_takes_a_reason_from_the_lookup_and_parks_the_request", "[approval][hold]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto kind = lifecycle.kind(holding_kind);
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "Release the trade on the desk", asker.id);
    const auto id = boost::uuids::to_string(raised.id);

    const auto held = lifecycle.decide(id,
                                       raised.version,
                                       "hold",
                                       decider.id,
                                       "Waiting on the sales ticket",
                                       "",
                                       "awaiting.sales_ticket");
    REQUIRE(held.outcome == "ok");
    CHECK(held.state_code == "held");
    CHECK(lifecycle.request(id)->state_code == "held");

    // The reason is on the record, so the held-over report has something to
    // group by and the person who asked reads what the hold waits for.
    const auto decisions =
        ores::inbox::repository::approval_decision_repository{}.read_latest_by_request_id(
            acting(h),
            id,
            0,
            10);
    REQUIRE(decisions.size() == 1);
    CHECK(decisions.front().decision_code == "hold");
    CHECK(decisions.front().hold_reason_code == std::optional<std::string>("awaiting.sales_ticket"));
}

TEST_CASE("a_hold_naming_no_reason_is_refused_with_the_codes_the_lookup_holds", "[approval][hold]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto kind = lifecycle.kind(holding_kind);
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "Release the trade on the desk", asker.id);
    const auto id = boost::uuids::to_string(raised.id);

    const auto refused = lifecycle.decide(id, raised.version, "hold", decider.id, "Not yet", "", "");
    CHECK(refused.outcome == "invalid");
    // The refusal lists what the lookup holds, so a person reads the answer
    // rather than guessing a code.
    CHECK(refused.message.find("awaiting.sales_ticket") != std::string::npos);
    CHECK(lifecycle.request(id)->state_code == "waiting");
}

TEST_CASE("a_hold_naming_an_unknown_reason_is_refused_with_the_lookup_terms", "[approval][hold]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    const auto kind = lifecycle.kind(holding_kind);
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "Release the trade on the desk", asker.id);
    const auto id = boost::uuids::to_string(raised.id);

    const auto refused = lifecycle.decide(
        id, raised.version, "hold", decider.id, "Not yet", "", "made.up.reason");
    CHECK(refused.outcome == "invalid");
    CHECK(refused.message.find("made.up.reason") != std::string::npos);
    CHECK(refused.message.find("exception.unconfirmed") != std::string::npos);
    CHECK(lifecycle.request(id)->state_code == "waiting");
}
