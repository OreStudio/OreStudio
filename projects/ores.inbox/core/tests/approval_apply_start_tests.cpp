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
#include "ores.inbox.core/service/approval_apply_start.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::inbox::service::apply_start_of;

ores::inbox::domain::approval_kind kind_applied_by(const std::string& workflow) {
    ores::inbox::domain::approval_kind kind;
    kind.code = "refdata.book_change";
    kind.name = "Book change";
    if (!workflow.empty())
        kind.apply_workflow_type = workflow;
    return kind;
}

ores::inbox::domain::approval_request a_request() {
    static boost::uuids::random_generator gen;
    ores::inbox::domain::approval_request request;
    request.id = gen();
    return request;
}

}

TEST_CASE("a_kind_that_names_no_workflow_starts_nothing", "[approval][apply_start]") {
    ores::inbox::domain::approval_kind none = kind_applied_by("");
    CHECK_FALSE(apply_start_of(none, a_request(), "corr").has_value());

    ores::inbox::domain::approval_kind empty = kind_applied_by("x");
    empty.apply_workflow_type = std::string();
    CHECK_FALSE(apply_start_of(empty, a_request(), "corr").has_value());
}

TEST_CASE("the_start_names_the_workflow_the_request_and_its_tenant", "[approval][apply_start]") {
    const auto request = a_request();

    const auto start = apply_start_of(kind_applied_by("book_apply_workflow"), request, "corr-1");

    REQUIRE(start.has_value());
    const auto id = boost::uuids::to_string(request.id);
    CHECK(start->type == "book_apply_workflow");
    CHECK(start->tenant_id == request.tenant_id.to_string());
    CHECK(start->target_kind == "approval_request");
    CHECK(start->target_id == id);
    CHECK(start->correlation_id == "corr-1");
    CHECK(start->request_json == "{\"request_id\":\"" + id + "\"}");
}

TEST_CASE("a_request_always_names_the_same_run_and_two_requests_name_two", "[approval][apply_start]") {
    const auto first = a_request();
    const auto second = a_request();
    const auto kind = kind_applied_by("book_apply_workflow");

    const auto once = apply_start_of(kind, first, "a");
    const auto again = apply_start_of(kind, first, "b");
    const auto other = apply_start_of(kind, second, "a");

    REQUIRE(once.has_value());
    REQUIRE(again.has_value());
    REQUIRE(other.has_value());
    CHECK_FALSE(once->instance_id.empty());
    CHECK(once->instance_id == again->instance_id);
    CHECK(once->instance_id != other->instance_id);
}

TEST_CASE("the_seeded_kinds_name_their_apply_workflow", "[approval][apply_start]") {
    ores::testing::scoped_database_helper h;
    ores::inbox::service::approval_lifecycle lifecycle(
        h.context().with_tenant(h.tenant_id(), h.db_user()));

    const auto book = lifecycle.kind("refdata.book_change");
    const auto role = lifecycle.kind("iam.role_grant");

    REQUIRE(book.has_value());
    REQUIRE(role.has_value());
    CHECK(book->apply_workflow_type == std::optional<std::string>("book_apply_workflow"));
    CHECK_FALSE(role->apply_workflow_type.has_value());
}
