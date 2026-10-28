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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.inbox.api/domain/approval_kind.hpp"
#include "ores.inbox.core/repository/approval_kind_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>

/**
 * @file approval_zero_approval_tests.cpp
 * @brief The zero-approval path: a kind that needs nobody is approved by the
 * raise itself, and the record says who closed it.
 *
 * A kind whose =approvals_required= is zero has no decider, so nothing a person
 * does can answer its requests. The raise therefore closes the request approved
 * in the same act, as the system rather than as the asker, and the owning
 * component applies it afterwards.
 *
 * The kind under test is written into the system tenant's catalogue, which is
 * where a kind lives, under a code of its own so the seeded rows are untouched.
 * It is removed again when the test ends.
 */

namespace {

auto& test_lg() {
    static auto instance = ores::logging::make_logger("ores.inbox.core.tests.approval_zero_approval");
    return instance;
}

using ores::inbox::service::approval_lifecycle;
using ores::inbox::tests::acting;
using ores::inbox::tests::seed_account;

/// A code no seeded kind holds, so a test never changes a row another suite reads.
std::string zero_approval_kind_code() {
    static int instance = 0;
    return "test.zero_approval_" + std::to_string(++instance);
}

/**
 * @brief A kind that needs no approval, written where a kind lives.
 *
 * A kind is the system tenant's row, and its write is checked against the
 * tenant the context acts for, so the fixture acts there. The row is deleted
 * when the fixture goes, so the shared catalogue keeps its history and loses
 * the row.
 */
class zero_approval_kind {
public:
    explicit zero_approval_kind(ores::testing::scoped_database_helper& h)
        : ctx_(h.context().with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()))
        , code_(zero_approval_kind_code()) {
        seed_permission();
        ores::inbox::domain::approval_kind kind;
        kind.code = code_;
        kind.name = "Zero approval " + code_;
        kind.description = "A kind that needs no approval, for the zero-approval tests";
        kind.decide_permission_code = permission_;
        kind.approvals_required = 0;
        kind.display_order = 999;
        kind.modified_by = h.db_user();
        kind.performed_by = h.db_user();
        kind.change_reason_code = "system.test";
        kind.change_commentary = "A kind that needs no approval, for the tests";
        ores::inbox::repository::approval_kind_repository repo;
        repo.write(ctx_, kind);
    }

    ~zero_approval_kind() {
        try {
            ores::database::repository::execute_parameterized_string_query(
                ctx_,
                "delete from ores_inbox_approval_kinds_tbl where code = $1",
                {code_},
                test_lg(),
                "Removing the zero-approval test kind");
        } catch (...) {
            // A row left behind is a row no test reads again: it carries a code
            // of its own. Cleaning up must not fail a suite that already passed.
        }
    }

    zero_approval_kind(const zero_approval_kind&) = delete;
    zero_approval_kind& operator=(const zero_approval_kind&) = delete;

    /// @brief The code the kind was written under.
    [[nodiscard]] const std::string& code() const {
        return code_;
    }

private:
    /**
     * @brief Names a permission the kind may carry.
     *
     * The kind's decider permission is a foreign key to the catalogue, so it
     * references a seeded row rather than writing one: the shared catalogue is
     * the populate scripts'.
     */
    void seed_permission() {
        const auto rows = ores::database::repository::execute_parameterized_string_query(
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

}

TEST_CASE("a_kind_that_needs_no_approval_is_approved_by_its_own_raise", "[approval][zero]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    zero_approval_kind fixture(h);
    approval_lifecycle lifecycle(acting(h));

    const auto kind = lifecycle.kind(fixture.code());
    REQUIRE(kind);
    REQUIRE(kind->approvals_required == 0);

    const auto raised =
        lifecycle.raise(*kind, "The desk needs this now", asker.id, /*part_codes=*/{});

    // The raise is the whole act: the request exists, approved, with the reason
    // and the subject the asker gave it.
    CHECK(raised.state_code == "approved");
    CHECK(raised.kind_code == fixture.code());
    CHECK(raised.reason == "The desk needs this now");
    CHECK(raised.requested_by == asker.id);
}

TEST_CASE("the_record_of_a_zero_approval_request_names_the_system_and_not_the_asker",
          "[approval][zero]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    zero_approval_kind fixture(h);
    approval_lifecycle lifecycle(acting(h));

    const auto kind = lifecycle.kind(fixture.code());
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "Nobody has to agree", asker.id);
    const auto id = boost::uuids::to_string(raised.id);

    const auto decisions = ores::database::repository::execute_parameterized_multi_column_query(
        acting(h),
        "select decision_code, decided_by::text, comment, part_code"
        " from ores_inbox_approval_decisions_tbl"
        " where request_id = $1::uuid"
        "   and valid_to = ores_utility_infinity_timestamp_fn()",
        {id},
        test_lg(),
        "Reading the decisions on a zero-approval request");

    // One request, one approval, and it is the system's: the asker did not
    // decide their own request, so the record never reads as if they had.
    REQUIRE(decisions.size() == 1);
    CHECK(decisions.front()[0].value_or("") == "approve");
    const auto decider = std::make_optional(lifecycle.system_actor_account_id());
    REQUIRE(decider.has_value());
    CHECK(decisions.front()[1].value_or("") == boost::uuids::to_string(**decider));
    CHECK(decisions.front()[1].value_or("") != boost::uuids::to_string(asker.id));

    // The comment names the kind and its zero count, so a reader of the row
    // learns why nobody decided it.
    const auto comment = decisions.front()[2].value_or("");
    CHECK(comment.find(fixture.code()) != std::string::npos);
    CHECK(comment.find("0 approvals") != std::string::npos);
}

TEST_CASE("a_kind_that_needs_one_approval_still_waits_for_a_person", "[approval][zero]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    zero_approval_kind fixture(h);
    approval_lifecycle lifecycle(acting(h));

    // The path a person takes is untouched: the seeded role request needs one
    // approval, and it starts waiting.
    const auto seeded = lifecycle.kind("iam.role_grant");
    REQUIRE(seeded);
    REQUIRE(seeded->approvals_required == 1);
    const auto raised = lifecycle.raise(*seeded, "I cover the desk next week", asker.id);
    CHECK(raised.state_code == "waiting");

    const auto id = boost::uuids::to_string(raised.id);
    const auto decided = lifecycle.decide(id, raised.version, "approve", decider.id, "");
    REQUIRE(decided.outcome == "ok");
    CHECK(decided.state_code == "approved");

    // And the person who answered it is the one the record names.
    CHECK(decided.state_code == lifecycle.request(id)->state_code);
}
