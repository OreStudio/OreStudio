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
#include "book_proposal_fixtures.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.platform/environment/environment.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>

// The other proposal tests sign in under the read-write role, which may do
// anything, so they cannot say whether the refdata service's own database user
// may raise and apply a request. These tests sign in as that user, with the
// credentials the service reads from its environment, and run the same work.

namespace {

using namespace ores::refdata::tests;
using ores::refdata::service::book_proposal_service;
using ores::platform::environment::environment;

/**
 * @brief The context the refdata service runs a request in, acting as an account
 * of the test tenant and scoped to one party.
 */
ores::database::context as_refdata_service(ores::testing::scoped_database_helper& h,
                                           const world& w) {
    const ores::database::database_options options{
        .user = environment::get_value_or_throw("ORES_REFDATA_SERVICE_DB_USER"),
        .password = environment::get_value_or_throw("ORES_REFDATA_SERVICE_DB_PASSWORD"),
        .host = environment::get_value_or_default("ORES_REFDATA_SERVICE_DB_HOST", "localhost"),
        .database = environment::get_value_or_throw("ORES_REFDATA_SERVICE_DB_DATABASE"),
        .port = environment::get_int_value_or_default("ORES_REFDATA_SERVICE_DB_PORT", 5432),
        .tenant = h.tenant_id().to_string()};
    const ores::database::context_factory::configuration config{.database_options = options,
                                                                .pool_size = 2,
                                                                .num_attempts = 3,
                                                                .wait_time_in_seconds = 1,
                                                                .service_account = options.user};
    const auto base = ores::database::context_factory::make_context(config);
    return base.with_party(base.tenant_id(), w.party_id, {w.party_id}, w.ctx.actor());
}

}

TEST_CASE("the_refdata_service_user_raises_a_request_and_tells_its_deciders",
          "[book_proposal][as_service]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(as_refdata_service(h, w));
    const auto line = new_book_line(h, w);

    const auto proposal = svc.raise({line}, "Open the desk");

    REQUIRE(proposal.request);
    CHECK(proposal.request->state_code == "waiting");
}

TEST_CASE("the_refdata_service_user_previews_without_writing", "[book_proposal][as_service]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(as_refdata_service(h, w));

    const auto preview = svc.preview({new_book_line(h, w)});

    REQUIRE(preview.lines.size() == 1);
    CHECK(preview.lines[0].refusal.empty());
    CHECK_FALSE(preview.part_codes.empty());
}

TEST_CASE("the_refdata_service_user_applies_an_approved_request", "[book_proposal][as_service]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(as_refdata_service(h, w));
    const auto line = new_book_line(h, w);
    const auto proposal = svc.raise({line}, "Open the desk");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);

    const auto result = svc.apply(boost::uuids::to_string(proposal.request->id));

    CHECK(result.applied);
    CHECK_FALSE(result.transient);
    ores::refdata::service::book_service books(w.ctx);
    CHECK(books.get_book(line.entity_id).has_value());
}

TEST_CASE("the_refdata_service_user_moves_a_refused_request_to_apply_failed",
          "[book_proposal][as_service]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(as_refdata_service(h, w));
    const auto existing = new_book_line(h, w);
    const auto live = make_live(w, existing);
    auto edit = edit_line(live, existing);
    edit.book_status = live.book_status == "Active" ? "Frozen" : "Active";
    const auto proposal = svc.raise({edit}, "Freeze the book");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);

    // The book moves between the approval and the apply, so the line's claim is stale.
    ores::refdata::service::book_service books(w.ctx);
    auto moved = *books.get_book(live.id);
    moved.description = "Changed by someone else";
    moved.change_reason_code = "system.test";
    write_live(w, moved);

    const auto id = boost::uuids::to_string(proposal.request->id);
    const auto result = svc.apply(id);

    CHECK_FALSE(result.applied);
    CHECK_FALSE(result.transient);
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    CHECK(lifecycle.request(id)->state_code == "apply_failed");

    // The step handler tells the person who asked, acting as the service. The
    // announcer logs a failure to tell and never throws, so only the notice
    // itself shows that the service user may write it.
    const auto request = lifecycle.request(id);
    const auto kind = lifecycle.kind(request->kind_code);
    REQUIRE(kind);
    const auto service_ctx = as_refdata_service(h, w);
    const auto sender =
        service_ctx.with_tenant(service_ctx.tenant_id(), service_ctx.service_account());
    ores::inbox::service::approval_announcer(sender).tell_asker(
        *kind, *request, request->state_code, "Line 1: the book changed");

    ores::inbox::service::notification_center center(w.ctx);
    const auto page = center.mine(request->requested_by, false, 0, 200);
    CHECK(std::ranges::any_of(page.notifications, [&](const auto& n) { return n.link_id == id; }));
}

TEST_CASE("the_refdata_service_user_reads_who_holds_a_permission", "[book_proposal][as_service]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto service_ctx = as_refdata_service(h, w);

    // Telling the deciders starts from who holds the permission. The tenant has
    // no person who does, so the answer is empty; the point is that the read
    // itself is allowed, including the function that resolves a person's roles.
    ores::inbox::service::notification_center center(service_ctx);
    CHECK_NOTHROW(center.holders_of("inbox::approvals:decide_operations"));

    auto lg = ores::logging::make_logger("ores.refdata.tests");
    CHECK_NOTHROW(ores::database::repository::execute_parameterized_multi_column_query(
        service_ctx,
        "select code from ores_iam_get_effective_permissions_fn($1::uuid)",
        {"00000000-0000-4000-8000-000000000001"},
        lg,
        "Resolving the permissions of one account"));
}
