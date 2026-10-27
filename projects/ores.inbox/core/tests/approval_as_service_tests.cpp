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
#include "ores.database/service/context_factory.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/repository/approval_request_repository.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

// The other inbox tests sign in under the read-write role, which may do
// anything, so they cannot say whether the inbox service's own database user
// holds every right the service uses. These tests sign in as that user, with the
// credentials the service reads from its environment, and do the work the
// service does: read a request, write the notice that tells its asker, and run
// the three sweeps.

namespace {

using ores::inbox::service::approval_announcer;
using ores::inbox::service::approval_lifecycle;
using ores::inbox::service::notification_center;
using ores::platform::environment::environment;

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

/**
 * @brief The context the inbox service runs a request in: its own database
 * user, scoped to the test tenant, acting as its service account.
 */
ores::database::context as_inbox_service(ores::testing::scoped_database_helper& h) {
    const ores::database::database_options options{
        .user = environment::get_value_or_throw("ORES_INBOX_SERVICE_DB_USER"),
        .password = environment::get_value_or_throw("ORES_INBOX_SERVICE_DB_PASSWORD"),
        .host = environment::get_value_or_default("ORES_INBOX_SERVICE_DB_HOST", "localhost"),
        .database = environment::get_value_or_throw("ORES_INBOX_SERVICE_DB_DATABASE"),
        .port = environment::get_int_value_or_default("ORES_INBOX_SERVICE_DB_PORT", 5432),
        .tenant = h.tenant_id().to_string()};
    const ores::database::context_factory::configuration config{.database_options = options,
                                                                .pool_size = 2,
                                                                .num_attempts = 3,
                                                                .wait_time_in_seconds = 1,
                                                                .service_account = options.user};
    const auto base = ores::database::context_factory::make_context(config);
    return base.with_tenant(base.tenant_id(), base.service_account());
}

}

TEST_CASE("the_inbox_service_user_reads_a_request_and_tells_the_person_who_asked", "[approval][as_service]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));
    const auto raised = lifecycle.raise(*lifecycle.kind("refdata.book_change"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(lifecycle.decide(id, raised.version, "approve", decider.id, "").state_code ==
            "approved");

    const auto service = as_inbox_service(h);
    approval_lifecycle as_service(service);
    const auto request = as_service.request(id);
    REQUIRE(request);
    const auto kind = as_service.kind(request->kind_code);
    REQUIRE(kind);

    // The announcer logs a failure to tell and never throws, so only the notice
    // itself shows that the service user may write it.
    approval_announcer(service).tell_asker(*kind, *request, "applied", "");

    notification_center center(acting(h));
    const auto page = center.mine(asker.id, false, 0, 200);
    CHECK(std::ranges::any_of(page.notifications, [&](const auto& n) { return n.link_id == id; }));
}

TEST_CASE("the_inbox_service_user_closes_an_overdue_request_in_the_expiry_sweep", "[approval][as_service]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    approval_lifecycle lifecycle(acting(h));
    const auto raised = lifecycle.raise(*lifecycle.kind("iam.role_grant"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);

    // Move the deadline behind us, which is what waiting out the window comes to.
    auto lapsed = *lifecycle.request(id);
    lapsed.expires_at = std::chrono::system_clock::now() - std::chrono::hours(1);
    lapsed.change_reason_code = "system.test";
    ores::inbox::repository::approval_request_repository repo;
    repo.write(acting(h), lapsed);

    const auto closed = approval_lifecycle(as_inbox_service(h)).expire_overdue();

    CHECK(std::ranges::any_of(closed, [&](const auto& e) { return e.request_id == id; }));
    CHECK(lifecycle.request(id)->state_code == "expired");
}

TEST_CASE("the_inbox_service_user_lists_an_approved_request_whose_apply_has_not_finished", "[approval][as_service]") {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    const auto decider = seed_account(h);
    approval_lifecycle lifecycle(acting(h));
    const auto raised = lifecycle.raise(*lifecycle.kind("refdata.book_change"), "Please", asker.id);
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(lifecycle.decide(id, raised.version, "approve", decider.id, "").state_code ==
            "approved");

    const auto stalled =
        approval_lifecycle(as_inbox_service(h)).stalled_applies(std::chrono::system_clock::now());

    CHECK(std::ranges::any_of(stalled, [&](const auto& s) { return s.request_id == id; }));
}

TEST_CASE("the_inbox_service_user_runs_the_reminder_sweep", "[approval][as_service]") {
    ores::testing::scoped_database_helper h;

    // The reminder reads across tenants and raises a notice only for a request
    // close to its deadline. An error from the database is thrown and not
    // swallowed, so a missing right fails the call.
    CHECK_NOTHROW(approval_lifecycle(as_inbox_service(h)).remind_expiring(std::chrono::hours(48)));
}

TEST_CASE("the_inbox_service_user_reads_who_holds_a_permission", "[approval][as_service]") {
    ores::testing::scoped_database_helper h;
    notification_center center(as_inbox_service(h));

    CHECK_NOTHROW(center.holders_of("inbox::approvals:decide_operations"));
}
