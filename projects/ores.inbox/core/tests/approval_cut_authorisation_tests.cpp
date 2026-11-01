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
 * @file approval_cut_authorisation_tests.cpp
 * @brief A cut's authorisation is decided by the inbox's own rules.
 *
 * A cut adds a detail row and a descriptor and nothing else: the kind states
 * which function answers it, four eyes is the record's rule, and a refusal
 * carries the reason the asker reads. These cases pin the two cut kinds the
 * design seeds, on the terms every other kind is decided on.
 */

namespace {

/// The kind an end of day cut's authorisation is raised under.
constexpr std::string_view cut_eod_kind = "reporting.eod_cut_authorisation";

/// The kind a verification cut's authorisation is raised under.
constexpr std::string_view cut_ipv_kind = "reporting.ipv_cut_authorisation";

/// The permissions the two kinds name as their deciding function's.
constexpr std::string_view cut_finance_decision = "inbox::approvals:decide_finance";
constexpr std::string_view cut_market_risk_decision = "inbox::approvals:decide_market_risk";

/** A context carrying the permissions a caller's token states. */
ores::database::context cut_holding_permissions(ores::testing::scoped_database_helper& h,
                               std::vector<std::string> codes) {
    auto ctx = h.context().with_tenant(h.tenant_id(), acting(h).actor());
    return ctx.with_roles(std::move(codes));
}

}

TEST_CASE("the_two_cut_kinds_name_a_deciding_function_and_no_raise", "[approval][cut]") {
    ores::testing::scoped_database_helper h;
    approval_lifecycle lifecycle(acting(h));

    const auto eod = lifecycle.kind(std::string(cut_eod_kind));
    REQUIRE(eod);
    // Finance signs off data the firm produced.
    CHECK(eod->decide_permission_code == cut_finance_decision);
    // The cut kind that points here states the occasion, so the raise is the
    // reporting service's to gate and the kind names none.
    CHECK_FALSE(eod->raise_permission_code.has_value());
    // One person answers a cut's authorisation.
    CHECK(eod->approvals_required == 1);
    // A refusal always needs a comment, and an approval does not.
    CHECK_FALSE(eod->comment_on_approve);

    const auto ipv = lifecycle.kind(std::string(cut_ipv_kind));
    REQUIRE(ipv);
    // Testing the firm's own marks is Market Risk's decision.
    CHECK(ipv->decide_permission_code == cut_market_risk_decision);
}

TEST_CASE("the_decider_is_the_permission_the_cut_kind_names", "[approval][cut]") {
    ores::testing::scoped_database_helper h;

    ores::inbox::service::approval_lifecycle as_finance(
        cut_holding_permissions(h, {std::string(cut_finance_decision)}));
    const auto eod = as_finance.kind(std::string(cut_eod_kind));
    REQUIRE(eod);
    CHECK(as_finance.may_decide(*eod));

    // A person who answers the verification cut may not answer the close.
    ores::inbox::service::approval_lifecycle as_market_risk(
        cut_holding_permissions(h, {std::string(cut_market_risk_decision)}));
    CHECK_FALSE(as_market_risk.may_decide(*eod));
    const auto ipv = as_market_risk.kind(std::string(cut_ipv_kind));
    REQUIRE(ipv);
    CHECK(as_market_risk.may_decide(*ipv));
}

TEST_CASE("the_person_who_took_the_cut_cannot_authorise_it", "[approval][cut]") {
    ores::testing::scoped_database_helper h;
    const auto taker = seed_account(h);
    const auto finance = seed_account(h);

    approval_lifecycle lifecycle(acting(h));
    const auto kind = lifecycle.kind(std::string(cut_eod_kind));
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "The day is closed", taker.id);
    REQUIRE(raised.state_code == "waiting");
    const auto id = boost::uuids::to_string(raised.id);

    // Four eyes is the record's rule and the cut adds nothing to it.
    CHECK_THROWS(lifecycle.decide(id, raised.version, "approve", taker.id, ""));
    CHECK(lifecycle.request(id)->state_code == "waiting");

    // A second person may decide it, and a cut's approval asks for no comment.
    const auto approved = lifecycle.decide(id, raised.version, "approve", finance.id, "");
    CHECK(approved.outcome == "ok");
    CHECK(approved.state_code == "approved");
    CHECK(lifecycle.request(id)->state_code == "approved");
}

TEST_CASE("a_refused_cut_authorisation_keeps_the_reason_and_admits_no_second_answer",
          "[approval][cut]") {
    ores::testing::scoped_database_helper h;
    const auto taker = seed_account(h);
    const auto finance = seed_account(h);

    approval_lifecycle lifecycle(acting(h));
    const auto raised =
        lifecycle.raise(*lifecycle.kind(std::string(cut_eod_kind)), "The day is closed", taker.id);
    const auto id = boost::uuids::to_string(raised.id);

    // A refusal is the sentence the asker reads, so it is required.
    const auto silent = lifecycle.decide(id, raised.version, "refuse", finance.id, "");
    CHECK(silent.outcome == "invalid");
    CHECK(lifecycle.request(id)->state_code == "waiting");

    const auto refused = lifecycle.decide(
        id, raised.version, "refuse", finance.id, "A book is missing from the population");
    CHECK(refused.outcome == "ok");
    CHECK(refused.state_code == "refused");

    // A refusal is terminal for that body: the remedy is a new take, not a
    // second decision on the same request.
    const auto again = lifecycle.decide(id, refused.version, "approve", finance.id, "");
    CHECK(again.outcome == "conflict");
    CHECK(lifecycle.request(id)->state_code == "refused");
}
