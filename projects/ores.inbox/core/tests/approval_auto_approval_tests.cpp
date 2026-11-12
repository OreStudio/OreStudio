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
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.api/domain/approval_kind.hpp"
#include "ores.inbox.core/repository/approval_kind_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>

/**
 * @file approval_auto_approval_tests.cpp
 * @brief The two rules that close a request without a person.
 *
 * A maker and checker pair puts a second person in front of what a person
 * proposed. Where no person is the maker there is no judgement to check, and the
 * request closes approved in the same act, as the system rather than as the
 * asker. Two things may say so: the deployment, through =system.approvals.mode=,
 * and the kind, through =auto_approval_code=. Whichever closed the request is
 * written on it and named in its decision, so an auto-approved act never reads as
 * one a person signed.
 *
 * The kinds under test are written into the system tenant's catalogue, where a
 * kind lives, under codes of their own so the seeded rows are untouched, and are
 * removed again when each test ends.
 */

namespace {

auto& test_lg() {
    static auto instance =
        ores::logging::make_logger("ores.inbox.core.tests.approval_auto_approval");
    return instance;
}

using ores::inbox::service::approval_lifecycle;
using ores::inbox::service::auto_approval_rule;
using ores::inbox::tests::acting;
using ores::inbox::tests::seed_account;

/// A code no seeded kind holds, so a test never changes a row another suite reads.
std::string test_kind_code(std::string_view what) {
    static int instance = 0;
    return "test." + std::string(what) + "_" + std::to_string(++instance);
}

/**
 * @brief A kind written into the catalogue, with the policy the test states.
 *
 * A kind is the system tenant's row, and its write is checked against the tenant
 * the context acts for, so the fixture acts there. The row is deleted when the
 * fixture goes, so the shared catalogue keeps its history and loses the row.
 */
class test_kind {
public:
    test_kind(ores::testing::scoped_database_helper& h,
              std::string code,
              std::string auto_approval_code,
              int approvals_required)
        : ctx_(h.context().with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()))
        , code_(std::move(code)) {
        seed_permission();
        ores::inbox::domain::approval_kind kind;
        kind.code = code_;
        kind.name = "Test kind " + code_;
        kind.description = "A kind written by the auto-approval tests";
        kind.decide_permission_code = permission_;
        kind.approvals_required = approvals_required;
        kind.auto_approval_code = std::move(auto_approval_code);
        kind.display_order = 999;
        kind.modified_by = h.db_user();
        kind.performed_by = h.db_user();
        kind.change_reason_code = "system.test";
        kind.change_commentary = "A kind written by the auto-approval tests";
        ores::inbox::repository::approval_kind_repository repo;
        repo.write(ctx_, kind);
    }

    ~test_kind() {
        try {
            ores::database::query_builder::execute_parameterized_command(
                ctx_,
                "delete from ores_inbox_approval_kinds_tbl where code = $1",
                {code_},
                test_lg(),
                "Removing the auto-approval test kind");
        } catch (...) {
            // A row left behind carries a code of its own, so no other test reads
            // it. Cleaning up must not fail a suite that already passed.
        }
    }

    test_kind(const test_kind&) = delete;
    test_kind& operator=(const test_kind&) = delete;

    [[nodiscard]] const std::string& code() const {
        return code_;
    }

private:
    void seed_permission() {
        const auto rows = ores::database::query_builder::execute_parameterized_string_query(
            ctx_,
            "select code from ores_iam_permissions_tbl"
            " where tenant_id = ores_utility_system_tenant_id_fn()"
            "   and valid_to = ores_utility_infinity_timestamp_fn()"
            " order by code limit 1",
            {},
            test_lg(),
            "Reading a permission the test kind may name");
        if (rows.empty())
            throw std::runtime_error("The permission catalogue is empty.");
        permission_ = rows.front();
    }

    ores::database::context ctx_;
    std::string code_;
    std::string permission_;
};

/**
 * @brief The deployment states whether it enforces approvals, for one test.
 *
 * The mode is read in the caller's tenant first, so a test states it in its own
 * and leaves the installation's row alone. The row is removed when the fixture
 * goes, so a deployment that enforces keeps doing so.
 */
class approvals_mode {
public:
    approvals_mode(ores::testing::scoped_database_helper& h, std::string value)
        : ctx_(acting(h)) {
        write(std::move(value));
    }

    ~approvals_mode() {
        try {
            ores::database::query_builder::execute_parameterized_command(
                ctx_,
                "delete from ores_variability_system_settings_tbl"
                " where name = 'system.approvals.mode' and tenant_id = $1::uuid",
                {ctx_.tenant_id().to_string()},
                test_lg(),
                "Removing the approvals mode the test wrote");
        } catch (...) {
            // The tenant is a test's own, so a row left behind changes nothing
            // another suite reads.
        }
    }

    approvals_mode(const approvals_mode&) = delete;
    approvals_mode& operator=(const approvals_mode&) = delete;

private:
    void write(std::string value) {
        // A tenant provisioned by the harness already holds the installation's
        // copy of the setting, so the test states its own over it rather than
        // writing a second row.
        ores::database::query_builder::execute_parameterized_command(
            ctx_,
            "delete from ores_variability_system_settings_tbl"
            " where name = 'system.approvals.mode' and tenant_id = $1::uuid",
            {ctx_.tenant_id().to_string()},
            test_lg(),
            "Clearing the approvals mode for a test");
        ores::database::query_builder::execute_parameterized_command(
            ctx_,
            "insert into ores_variability_system_settings_tbl ("
            "  id, tenant_id, party_id, name, value, data_type, description,"
            "  modified_by, performed_by, change_reason_code, change_commentary,"
            "  valid_from, valid_to)"
            " values (gen_random_uuid(), $1::uuid,"
            "   (select id from ores_refdata_parties_tbl"
            "     where tenant_id = $1::uuid and party_category = 'System'"
            "       and valid_to = ores_utility_infinity_timestamp_fn()),"
            "   'system.approvals.mode', $2, 'string', 'Written by the auto-approval tests',"
            "   $3, $3, 'system.test', 'Written by the auto-approval tests',"
            "   current_timestamp, ores_utility_infinity_timestamp_fn())",
            {ctx_.tenant_id().to_string(), std::move(value), ctx_.service_account()},
            test_lg(),
            "Writing the approvals mode for a test");
    }

    ores::database::context ctx_;
};

/// The decisions on one request, as the record holds them.
std::vector<std::vector<std::optional<std::string>>>
decisions_on(ores::testing::scoped_database_helper& h, const std::string& request_id) {
    return ores::database::query_builder::execute_parameterized_multi_column_query(
        acting(h),
        "select decision_code, decided_by::text, comment"
        " from ores_inbox_approval_decisions_tbl"
        " where request_id = $1::uuid"
        "   and valid_to = ores_utility_infinity_timestamp_fn()",
        {request_id},
        test_lg(),
        "Reading the decisions on a request");
}

}

TEST_CASE("a_kind_that_approves_every_raise_closes_it_on_the_raise", "[approval][auto]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    test_kind fixture(h, test_kind_code("every"), "every_request", 1);
    approval_lifecycle lifecycle(acting(h));

    const auto kind = lifecycle.kind(fixture.code());
    REQUIRE(kind);
    REQUIRE(kind->auto_approval_code == "every_request");

    const auto raised =
        lifecycle.raise(*kind, "The desk needs this now", asker.id, /*part_codes=*/{});

    // The raise is the whole act, and the request says which rule closed it.
    CHECK(raised.state_code == "approved");
    CHECK(raised.auto_approval_code == "every_request");
    CHECK(raised.reason == "The desk needs this now");
    CHECK(raised.requested_by == asker.id);
}

TEST_CASE("the_record_of_an_auto_approved_request_names_the_system_and_the_rule",
          "[approval][auto]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    test_kind fixture(h, test_kind_code("record"), "every_request", 1);
    approval_lifecycle lifecycle(acting(h));

    const auto kind = lifecycle.kind(fixture.code());
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "Nobody has to agree", asker.id);
    const auto decisions = decisions_on(h, boost::uuids::to_string(raised.id));

    // One request, one approval, and it is the system's: the asker did not decide
    // their own request, so the record never reads as if they had.
    REQUIRE(decisions.size() == 1);
    CHECK(decisions.front()[0].value_or("") == "approve");
    const auto decider = lifecycle.system_actor_account_id();
    REQUIRE(decider.has_value());
    CHECK(decisions.front()[1].value_or("") == boost::uuids::to_string(*decider));
    CHECK(decisions.front()[1].value_or("") != boost::uuids::to_string(asker.id));

    // The comment names the policy in the words a person reads, and the kind it
    // closed.
    const auto comment = decisions.front()[2].value_or("");
    CHECK(comment.find("Approved by policy") != std::string::npos);
    CHECK(comment.find(fixture.code()) != std::string::npos);
}

TEST_CASE("a_kind_that_accepts_a_system_raise_closes_only_in_the_system_tenant",
          "[approval][auto]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    test_kind fixture(h, test_kind_code("system"), "system_tenant", 1);
    approval_lifecycle lifecycle(acting(h));

    // The test tenant is somebody's book, so the same kind's raise waits there.
    const auto kind = lifecycle.kind(fixture.code());
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "A curve of this tenant's own", asker.id);
    CHECK(raised.state_code == "waiting");
    CHECK(raised.auto_approval_code.empty());
}

TEST_CASE("a_deployment_that_does_not_enforce_approvals_closes_every_kind", "[approval][auto]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    approvals_mode mode(h, "auto_approved");
    approval_lifecycle lifecycle(acting(h));

    // A seeded kind names no policy of its own, and the deployment closes it.
    const auto seeded = lifecycle.kind("iam.role_grant");
    REQUIRE(seeded);
    REQUIRE(seeded->auto_approval_code == "none");
    const auto raised = lifecycle.raise(*seeded, "A test bed needs the role", asker.id);

    CHECK(raised.state_code == "approved");
    CHECK(raised.auto_approval_code == "tenant");
    const auto decisions = decisions_on(h, boost::uuids::to_string(raised.id));
    REQUIRE(decisions.size() == 1);
    CHECK(decisions.front()[2].value_or("").find("does not enforce approvals") !=
          std::string::npos);
}

TEST_CASE("a_person_in_another_party_is_bound_by_the_tenants_mode", "[approval][auto]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    approvals_mode mode(h, "auto_approved");
    const auto base = acting(h);
    const auto other_party = boost::uuids::random_generator{}();
    approval_lifecycle lifecycle(
        base.with_party(base.tenant_id(), other_party, {other_party}, base.actor()));

    const auto seeded = lifecycle.kind("iam.role_grant");
    REQUIRE(seeded);
    const auto raised = lifecycle.raise(*seeded, "The party's person asks", asker.id);

    CHECK(raised.state_code == "approved");
    CHECK(raised.auto_approval_code == "tenant");
}

TEST_CASE("a_raise_inside_a_unit_of_work_by_a_party_is_bound_by_the_tenants_mode",
          "[approval][auto]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    approvals_mode mode(h, "auto_approved");
    const auto base = acting(h);
    const auto party = boost::uuids::random_generator{}();
    const auto as_party = base.with_party(base.tenant_id(), party, {party}, base.actor());

    // The raise and its subject are one transaction. The policy is read before
    // it opens, under the party's view, and still finds the tenant's mode; the
    // raise inside it only writes.
    approval_lifecycle reader(as_party);
    const auto seeded = reader.kind("iam.role_grant");
    REQUIRE(seeded);
    const auto policy = reader.policy_for(*seeded, false);
    ores::database::repository::unit_of_work uow(as_party);
    const auto raised = approval_lifecycle(uow.ctx()).raise_under(
        policy, *seeded, "Raised with its subject", asker.id);
    uow.commit();

    CHECK(raised.state_code == "approved");
    CHECK(raised.auto_approval_code == "tenant");
}

TEST_CASE("a_raise_inside_a_unit_of_work_that_reads_its_own_policy_is_refused",
          "[approval][auto]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    approvals_mode mode(h, "auto_approved");
    approval_lifecycle reader(acting(h));
    const auto seeded = reader.kind("iam.role_grant");
    REQUIRE(seeded);

    // The policy is the tenant's and the system tenant's to read, which a
    // transaction fixed to one tenant and party cannot do, so the read fails
    // where it is made rather than answering for the wrong tenant.
    ores::database::repository::unit_of_work uow(acting(h));
    CHECK_THROWS_AS(approval_lifecycle(uow.ctx()).raise(*seeded, "Reads inside", asker.id),
                    std::logic_error);
}

TEST_CASE("turning_the_mode_off_makes_the_next_raise_wait", "[approval][auto]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    approvals_mode mode(h, "auto_approved");
    approval_lifecycle lifecycle(acting(h));

    REQUIRE_FALSE(lifecycle.approvals_mode().enforced);
    lifecycle.set_approvals_mode(false, "People decide from here on");

    const auto state = lifecycle.approvals_mode();
    CHECK(state.enforced);
    CHECK(state.stated_by_tenant);
    CHECK(state.reason == "People decide from here on");
    const auto seeded = lifecycle.kind("iam.role_grant");
    REQUIRE(seeded);
    CHECK(lifecycle.raise(*seeded, "Asked after the switch", asker.id).state_code == "waiting");
}

TEST_CASE("a_kind_that_asks_for_no_approvals_and_names_no_policy_refuses_the_raise",
          "[approval][auto]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    test_kind fixture(h, test_kind_code("none"), "none", 0);
    approval_lifecycle lifecycle(acting(h));

    // A request that waits on nobody can never be answered, so the raise says so
    // rather than writing a request no person can clear.
    const auto kind = lifecycle.kind(fixture.code());
    REQUIRE(kind);
    CHECK_THROWS_AS(lifecycle.raise(*kind, "Nobody can answer this", asker.id), std::logic_error);
}

TEST_CASE("a_kind_that_needs_one_approval_still_waits_for_a_person", "[approval][auto]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));

    // The path a person takes is untouched: the seeded role request needs one
    // approval, and it starts waiting.
    const auto seeded = lifecycle.kind("iam.role_grant");
    REQUIRE(seeded);
    REQUIRE(seeded->approvals_required == 1);
    const auto raised = lifecycle.raise(*seeded, "I cover the desk next week", asker.id);
    CHECK(raised.state_code == "waiting");
    CHECK(raised.auto_approval_code.empty());

    const auto id = boost::uuids::to_string(raised.id);
    const auto decided = lifecycle.decide(id, raised.version, "approve", decider.id, "");
    REQUIRE(decided.outcome == "ok");
    CHECK(decided.state_code == "approved");
}
