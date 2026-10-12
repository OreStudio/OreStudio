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
#include "ores.compute.core/repository/work_ledger_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <future>
#include <optional>
#include <string>
#include <vector>

namespace {

const std::string tags("[repository][work_ledger]");

using ores::compute::repository::ledger_entry;
using ores::compute::repository::work_ledger_repository;
using ores::testing::database_helper;
using ores::utility::uuid::tenant_id;
using std::chrono::seconds;

boost::uuids::uuid random_uuid() {
    return boost::uuids::random_generator()();
}

/// The platform a test queues for. The development database may hold real entries, so each
/// test uses a platform no node has and so claims only what it queued.
std::string unique_platform() {
    return "test-" + boost::uuids::to_string(random_uuid());
}

ledger_entry entry_for(const std::string& platform,
                       const boost::uuids::uuid& tenant,
                       int priority = 0) {
    return ledger_entry{.result_id = random_uuid(),
                        .work_tenant_id = tenant,
                        .workunit_id = random_uuid(),
                        .platforms = {platform},
                        .priority = priority};
}

/// Deletes the entries a test queued when the test ends. The tests run against the
/// development database, and an entry left behind would be claimed by a node.
class ledger_cleanup {
public:
    explicit ledger_cleanup(database_helper& h)
        : h_(h) {}
    ledger_cleanup(const ledger_cleanup&) = delete;
    ledger_cleanup& operator=(const ledger_cleanup&) = delete;

    ~ledger_cleanup() {
        for (const auto& id : ids_) {
            try {
                work_ledger_repository().remove(system(), id);
            } catch (const std::exception&) {
                // A failed delete leaves an entry behind and must not fail the test.
            }
        }
    }

    ores::database::context system() const {
        return h_.context().with_tenant(tenant_id::system(), h_.db_user());
    }

    ledger_entry add(const ledger_entry& entry) {
        ids_.push_back(entry.result_id);
        work_ledger_repository().enqueue(system(), entry);
        return entry;
    }

private:
    database_helper& h_;
    std::vector<boost::uuids::uuid> ids_;
};

}

TEST_CASE("claim_takes_a_queued_entry_for_the_platform_and_counts_the_attempt", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto queued = cleanup.add(entry_for(platform, random_uuid()));
    const auto host = random_uuid();

    work_ledger_repository ledger;
    const auto claimed = ledger.claim(cleanup.system(), host, platform, seconds(300));

    REQUIRE(claimed.has_value());
    CHECK(claimed->result_id == queued.result_id);
    CHECK(claimed->work_tenant_id == queued.work_tenant_id);
    CHECK(claimed->workunit_id == queued.workunit_id);
    CHECK(claimed->attempts == 1);
    CHECK(ledger.holder(cleanup.system(), queued.result_id) == host);
}

TEST_CASE("claim_takes_nothing_for_a_platform_with_no_waiting_entry", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    cleanup.add(entry_for(unique_platform(), random_uuid()));

    const auto claimed =
        work_ledger_repository().claim(cleanup.system(), random_uuid(), unique_platform(), seconds(300));

    CHECK(!claimed.has_value());
}

TEST_CASE("two_claims_never_take_one_entry", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto first = cleanup.add(entry_for(platform, random_uuid()));
    const auto second = cleanup.add(entry_for(platform, random_uuid()));

    work_ledger_repository ledger;
    const auto a = ledger.claim(cleanup.system(), random_uuid(), platform, seconds(300));
    const auto b = ledger.claim(cleanup.system(), random_uuid(), platform, seconds(300));
    const auto c = ledger.claim(cleanup.system(), random_uuid(), platform, seconds(300));

    REQUIRE(a.has_value());
    REQUIRE(b.has_value());
    CHECK(a->result_id != b->result_id);
    CHECK(!c.has_value());
    CHECK((a->result_id == first.result_id || a->result_id == second.result_id));
}

TEST_CASE("claim_prefers_the_tenant_with_the_fewest_entries_claimed", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto deep = random_uuid();
    const auto shallow = random_uuid();
    // The deep tenant queued first and has three waiting. The shallow tenant has one.
    cleanup.add(entry_for(platform, deep));
    cleanup.add(entry_for(platform, deep));
    cleanup.add(entry_for(platform, deep));
    cleanup.add(entry_for(platform, shallow));

    work_ledger_repository ledger;
    const auto first = ledger.claim(cleanup.system(), random_uuid(), platform, seconds(300));
    const auto second = ledger.claim(cleanup.system(), random_uuid(), platform, seconds(300));
    const auto third = ledger.claim(cleanup.system(), random_uuid(), platform, seconds(300));

    REQUIRE(first.has_value());
    REQUIRE(second.has_value());
    REQUIRE(third.has_value());
    // The oldest entry is the deep tenant's, so it goes first. After that the deep tenant holds
    // one claim and the shallow tenant none, so the shallow tenant's entry goes next, and
    // then the tenants are level again.
    CHECK(first->work_tenant_id == deep);
    CHECK(second->work_tenant_id == shallow);
    CHECK(third->work_tenant_id == deep);
}

TEST_CASE("claim_prefers_the_higher_priority_among_tenants_with_equal_load", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto tenant = random_uuid();
    cleanup.add(entry_for(platform, tenant, 0));
    const auto urgent = cleanup.add(entry_for(platform, tenant, 5));

    const auto claimed =
        work_ledger_repository().claim(cleanup.system(), random_uuid(), platform, seconds(300));

    REQUIRE(claimed.has_value());
    CHECK(claimed->result_id == urgent.result_id);
}

TEST_CASE("release_returns_a_claimed_entry_to_the_queue_and_gives_the_attempt_back", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto queued = cleanup.add(entry_for(platform, random_uuid()));
    work_ledger_repository ledger;
    const auto host = random_uuid();
    REQUIRE(ledger.claim(cleanup.system(), host, platform, seconds(300)).has_value());

    ledger.release(cleanup.system(), queued.result_id);

    CHECK(!ledger.holder(cleanup.system(), queued.result_id).has_value());
    const auto again = ledger.claim(cleanup.system(), host, platform, seconds(300));
    REQUIRE(again.has_value());
    CHECK(again->result_id == queued.result_id);
    // The claim that was given back never ran, so it does not count as an attempt.
    CHECK(again->attempts == 1);
}

TEST_CASE("concurrent_claims_never_take_the_same_entry", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    constexpr int entries = 6;
    for (int i = 0; i < entries; ++i)
        cleanup.add(entry_for(platform, random_uuid()));

    // Twelve claims race for six entries. Each entry goes to exactly one claim.
    std::vector<std::future<std::optional<ores::compute::repository::claimed_entry>>> claims;
    for (int i = 0; i < 2 * entries; ++i) {
        claims.push_back(std::async(std::launch::async, [&cleanup, &platform] {
            return work_ledger_repository().claim(
                cleanup.system(), random_uuid(), platform, seconds(300));
        }));
    }
    std::vector<boost::uuids::uuid> taken;
    for (auto& claim : claims) {
        if (const auto entry = claim.get())
            taken.push_back(entry->result_id);
    }

    CHECK(taken.size() == entries);
    std::ranges::sort(taken);
    CHECK(std::ranges::adjacent_find(taken) == taken.end());
}

TEST_CASE("requeue_expired_returns_an_expired_claim_that_has_attempts_left", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto queued = cleanup.add(entry_for(platform, random_uuid()));
    work_ledger_repository ledger;
    REQUIRE(ledger.claim(cleanup.system(), random_uuid(), platform, seconds(0)).has_value());

    CHECK(ledger.requeue_expired(cleanup.system()) >= 1);

    CHECK(!ledger.holder(cleanup.system(), queued.result_id).has_value());
    const auto again = ledger.claim(cleanup.system(), random_uuid(), platform, seconds(300));
    REQUIRE(again.has_value());
    CHECK(again->result_id == queued.result_id);
    CHECK(again->attempts == 2);
}

TEST_CASE("requeue_expired_leaves_a_live_lease_alone", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto queued = cleanup.add(entry_for(platform, random_uuid()));
    const auto host = random_uuid();
    work_ledger_repository ledger;
    REQUIRE(ledger.claim(cleanup.system(), host, platform, seconds(300)).has_value());

    ledger.requeue_expired(cleanup.system());

    CHECK(ledger.holder(cleanup.system(), queued.result_id) == host);
}

TEST_CASE("an_expired_claim_with_no_attempts_left_is_exhausted_and_not_requeued", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto queued = cleanup.add(entry_for(platform, random_uuid()));
    work_ledger_repository ledger;
    const auto host = random_uuid();
    // One attempt is allowed, and the claim uses it.
    REQUIRE(ledger.claim(cleanup.system(), host, platform, seconds(0)).has_value());

    ledger.requeue_expired(cleanup.system(), 1);

    const auto exhausted = ledger.exhausted(cleanup.system(), 1, 10000);
    const auto found = std::ranges::find(
        exhausted, queued.result_id, &ores::compute::repository::expired_entry::result_id);
    REQUIRE(found != exhausted.end());
    CHECK(found->host_id == host);
    CHECK(found->attempts == 1);
    CHECK(ledger.remove_expired(cleanup.system(), queued.result_id));
    CHECK(!ledger.remove_expired(cleanup.system(), queued.result_id));
}

TEST_CASE("remove_expired_keeps_an_entry_whose_lease_was_renewed", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto queued = cleanup.add(entry_for(platform, random_uuid()));
    const auto host = random_uuid();
    work_ledger_repository ledger;
    REQUIRE(ledger.claim(cleanup.system(), host, platform, seconds(0)).has_value());
    ledger.extend_leases(cleanup.system(), host, seconds(300));

    CHECK(!ledger.remove_expired(cleanup.system(), queued.result_id));
    CHECK(ledger.holder(cleanup.system(), queued.result_id) == host);
}

TEST_CASE("waiting_counts_the_queued_entries_of_one_tenant", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto tenant = random_uuid();
    cleanup.add(entry_for(platform, tenant));
    cleanup.add(entry_for(platform, tenant));
    cleanup.add(entry_for(platform, random_uuid()));
    work_ledger_repository ledger;
    REQUIRE(ledger.claim(cleanup.system(), random_uuid(), platform, seconds(300)).has_value());

    // Two were queued for the tenant and one claim took one of the three, whichever it was.
    const auto waiting = ledger.waiting(cleanup.system(), tenant);
    CHECK(waiting >= 1);
    CHECK(waiting <= 2);
}

TEST_CASE("remove_deletes_an_entry_so_it_is_not_claimed", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto queued = cleanup.add(entry_for(platform, random_uuid()));
    work_ledger_repository ledger;

    ledger.remove(cleanup.system(), queued.result_id);

    CHECK(!ledger.claim(cleanup.system(), random_uuid(), platform, seconds(300)).has_value());
}

TEST_CASE("holder_is_nothing_for_an_entry_that_waits_and_for_one_not_in_the_ledger", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto queued = cleanup.add(entry_for(unique_platform(), random_uuid()));
    work_ledger_repository ledger;

    CHECK(!ledger.holder(cleanup.system(), queued.result_id).has_value());
    CHECK(!ledger.holder(cleanup.system(), random_uuid()).has_value());
}

TEST_CASE("extend_leases_extends_only_the_entries_the_host_holds", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    cleanup.add(entry_for(platform, random_uuid()));
    cleanup.add(entry_for(platform, random_uuid()));
    work_ledger_repository ledger;
    const auto host = random_uuid();
    const auto stranger = random_uuid();
    REQUIRE(ledger.claim(cleanup.system(), host, platform, seconds(60)).has_value());

    CHECK(ledger.extend_leases(cleanup.system(), host, seconds(300)) == 1);
    CHECK(ledger.extend_leases(cleanup.system(), stranger, seconds(300)) == 0);
}

TEST_CASE("enqueue_leaves_an_entry_already_in_the_ledger_as_it_is", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto queued = cleanup.add(entry_for(platform, random_uuid(), 3));
    work_ledger_repository ledger;
    const auto host = random_uuid();
    REQUIRE(ledger.claim(cleanup.system(), host, platform, seconds(300)).has_value());

    // The dispatcher may queue a result again on a repeated event. A claimed entry is not reset.
    ledger.enqueue(cleanup.system(), queued);

    CHECK(ledger.holder(cleanup.system(), queued.result_id) == host);
}

TEST_CASE("a_tenant_session_reads_none_of_the_ledger_and_writes_none_of_it", tags) {
    database_helper h;
    ledger_cleanup cleanup(h);
    const auto platform = unique_platform();
    const auto queued = cleanup.add(entry_for(platform, random_uuid()));
    work_ledger_repository ledger;
    const auto host = random_uuid();
    REQUIRE(ledger.claim(cleanup.system(), host, platform, seconds(300)).has_value());

    // The test tenant is not the system tenant, so the policy admits it to no row.
    CHECK(ledger.count(h.context(), "claimed") == 0);
    CHECK(ledger.count(h.context(), "queued") == 0);
    CHECK(!ledger.holder(h.context(), queued.result_id).has_value());
    CHECK_THROWS(ledger.enqueue(h.context(), entry_for(platform, random_uuid())));
}
