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
#include "ores.testing/database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workflow.core/repository/step_claim_repository.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <future>
#include <optional>
#include <string>
#include <vector>

namespace {

const std::string tags("[repository][step_claim]");

using ores::testing::database_helper;
using ores::utility::uuid::tenant_id;
using ores::workflow::repository::step_claim_repository;
using std::chrono::seconds;

boost::uuids::uuid random_uuid() {
    return boost::uuids::random_generator()();
}

/**
 * @brief Removes the claims a test took when the test ends.
 *
 * The tests run against the development database, and a claim left behind would
 * keep a delivery out of a step that no longer exists.
 */
class claim_cleanup {
public:
    explicit claim_cleanup(database_helper& h)
        : h_(h) {}
    claim_cleanup(const claim_cleanup&) = delete;
    claim_cleanup& operator=(const claim_cleanup&) = delete;

    ~claim_cleanup() {
        for (const auto& id : ids_) {
            try {
                step_claim_repository().release(system(), id);
            } catch (const std::exception&) {
                // A failed delete leaves a row behind and must not fail the test.
            }
        }
    }

    ores::database::context system() const {
        return h_.context().with_tenant(tenant_id::system(), h_.db_user());
    }

    boost::uuids::uuid watch(const boost::uuids::uuid& step_id) {
        ids_.push_back(step_id);
        return step_id;
    }

private:
    database_helper& h_;
    std::vector<boost::uuids::uuid> ids_;
};

}

TEST_CASE("a_claim_is_taken_once_and_refused_while_its_lease_is_live", tags) {
    database_helper h;
    claim_cleanup cleanup(h);
    // A step id no other test and no run uses, so the claim is the only one on it.
    const auto step = cleanup.watch(random_uuid());
    const auto work_tenant = random_uuid();

    step_claim_repository claims;
    const auto first = claims.claim(cleanup.system(), step, work_tenant, "first/1", seconds(300));

    // The first caller holds the step, and the attempt count counts that claim.
    REQUIRE(first.has_value());
    CHECK(*first == 1);
    CHECK(claims.holder(cleanup.system(), step) == std::optional<std::string>("first/1"));

    // A second delivery arriving while the lease is live takes nothing. This is
    // the guarantee the claim exists for: an empty answer here is the pass.
    const auto second = claims.claim(cleanup.system(), step, work_tenant, "second/2", seconds(300));
    CHECK(!second.has_value());

    // The holder did not move, so the refused caller changed nothing.
    CHECK(claims.holder(cleanup.system(), step) == std::optional<std::string>("first/1"));
}

TEST_CASE("a_claim_is_taken_over_once_its_lease_has_ended", tags) {
    database_helper h;
    claim_cleanup cleanup(h);
    const auto step = cleanup.watch(random_uuid());
    const auto work_tenant = random_uuid();

    step_claim_repository claims;
    const auto first = claims.claim(cleanup.system(), step, work_tenant, "first/1", seconds(0));
    REQUIRE(first.has_value());

    // A lease of no seconds is already over, so the same statement takes the
    // claim over and the attempt count says this is the second attempt.
    const auto second = claims.claim(cleanup.system(), step, work_tenant, "second/2", seconds(300));
    REQUIRE(second.has_value());
    CHECK(*second == 2);
    CHECK(claims.holder(cleanup.system(), step) == std::optional<std::string>("second/2"));
}

TEST_CASE("two_claims_arriving_together_leave_exactly_one_owner", tags) {
    database_helper h;
    claim_cleanup cleanup(h);
    const auto step = cleanup.watch(random_uuid());
    const auto work_tenant = random_uuid();
    auto ctx = cleanup.system();

    // Both callers state a live lease and neither waits, so the store decides.
    // Exactly one may come back owning the step.
    auto one = std::async(std::launch::async, [&] {
        return step_claim_repository().claim(ctx, step, work_tenant, "one/1", seconds(300));
    });
    auto two = std::async(std::launch::async, [&] {
        return step_claim_repository().claim(ctx, step, work_tenant, "two/2", seconds(300));
    });

    const auto one_won = one.get().has_value();
    const auto two_won = two.get().has_value();

    CHECK(one_won != two_won);

    const auto holder = step_claim_repository().holder(ctx, step);
    REQUIRE(holder.has_value());
    CHECK(*holder == (one_won ? "one/1" : "two/2"));
}

TEST_CASE("release_removes_a_claim_whatever_the_holder_named_itself", tags) {
    database_helper h;
    claim_cleanup cleanup(h);
    const auto step = cleanup.watch(random_uuid());
    const auto work_tenant = random_uuid();

    step_claim_repository claims;
    REQUIRE(
        claims
            .claim(cleanup.system(), step, work_tenant, "ores.reporting.service/4242", seconds(300))
            .has_value());

    // The engine releases on the holder's behalf, so the holder's name is not
    // part of the predicate. A name the engine never computed must still go.
    CHECK(claims.release(cleanup.system(), step));
    CHECK(!claims.holder(cleanup.system(), step).has_value());

    // Nothing was there to remove the second time.
    CHECK(!claims.release(cleanup.system(), step));
}
