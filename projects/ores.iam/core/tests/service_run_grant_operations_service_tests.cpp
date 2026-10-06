/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.iam.api/domain/permission.hpp"
#include "ores.iam.api/domain/role_permission.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.api/generators/permission_generator.hpp"
#include "ores.iam.api/generators/role_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/permission_repository.hpp"
#include "ores.iam.core/repository/role_permission_repository.hpp"
#include "ores.iam.core/repository/role_repository.hpp"
#include "ores.iam.core/service/run_grant_operations_service.hpp"
#include "ores.iam.core/service/run_grant_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

namespace {

const std::string_view test_suite("ores.iam.tests.run_grant_operations");
const std::string tags("[service][run_grant]");

using ores::database::context;
using ores::iam::domain::account;
using ores::iam::domain::role;
using ores::iam::messaging::create_run_grant_request;
using ores::iam::messaging::revoke_run_grant_request;
using ores::iam::service::run_grant_operations_service;
using ores::iam::service::run_grant_service;
using ores::testing::database_helper;
using ores::utility::generation::generation_context;
using Catch::Matchers::ContainsSubstring;
using namespace ores::logging;

const std::string granted_code("iam::accounts:read");

account write_account(database_helper& h, generation_context& gen) {
    ores::iam::repository::account_repository repo;
    auto value = ores::iam::generators::generate_synthetic_account(gen);
    repo.write(h.context(), value);
    return value;
}

role write_role_bundling(database_helper& h, generation_context& gen, const std::string& code) {
    ores::iam::repository::role_repository roles;
    ores::iam::repository::permission_repository permissions_repo;
    ores::iam::repository::role_permission_repository links(h.context());

    auto r = ores::iam::generators::generate_synthetic_role(gen);
    roles.write(h.context(), r);

    auto existing = permissions_repo.read_latest_by_code(h.context(), code);
    auto p = existing.empty() ? ores::iam::generators::generate_synthetic_permission(gen) :
                                existing.front();
    if (existing.empty()) {
        p.code = code;
        permissions_repo.write(h.context(), p);
    }

    ores::iam::domain::role_permission link;
    link.tenant_id = h.tenant_id();
    link.role_id = r.id;
    link.permission_id = p.id;
    link.assigned_by = h.db_user();
    link.change_reason_code = "system.test";
    link.change_commentary = "Synthetic test data";
    links.write(link);
    return r;
}

context person(database_helper& h,
               const account& a,
               const boost::uuids::uuid& party,
               std::vector<std::string> held) {
    return h.context()
        .with_party(h.tenant_id(), party, {party}, a.username)
        .with_roles(std::move(held));
}

create_run_grant_request request_for(const role& r, const std::string& resource) {
    create_run_grant_request req;
    req.resource = resource;
    req.role = r.name;
    req.audience = "ores.reporting.service";
    return req;
}

std::string resource_name() {
    return "reporting.report_definition/" +
           boost::uuids::to_string(boost::uuids::random_generator()());
}

boost::uuids::uuid id_of(const std::string& s) {
    return boost::uuids::string_generator()(s);
}

}

TEST_CASE("create_run_grant_records_the_grant_for_a_role_the_person_holds", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto a = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, granted_code);
    const auto party = boost::uuids::random_generator()();
    const auto ctx = person(h, a, party, {granted_code});

    run_grant_operations_service sut(ctx);
    const auto resp = sut.create_run_grant(request_for(r, resource_name()));
    BOOST_LOG_SEV(lg, info) << "Response: " << resp.message;

    REQUIRE(resp.success);
    CHECK(resp.created);
    const auto stored = run_grant_service(ctx).find_grant(id_of(resp.grant_id));
    REQUIRE(stored);
    CHECK(stored->party_id == party);
    CHECK(stored->grantor_account_id == a.id);
    CHECK(stored->role_id == r.id);
    CHECK(stored->modified_by == a.username);
    CHECK(stored->revoked_at == std::chrono::system_clock::time_point{});
}

TEST_CASE("create_run_grant_refuses_a_role_the_person_does_not_hold_in_full", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto a = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, granted_code);
    const auto ctx = person(h, a, boost::uuids::random_generator()(), {"iam::accounts:update"});

    const auto resp =
        run_grant_operations_service(ctx).create_run_grant(request_for(r, resource_name()));
    CHECK_FALSE(resp.success);
    CHECK(resp.grant_id.empty());
    CHECK_THAT(resp.message, ContainsSubstring(granted_code));
}

TEST_CASE("create_run_grant_refuses_a_session_that_acts_for_no_party", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto a = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, granted_code);
    const auto ctx = h.context().with_tenant(h.tenant_id(), a.username).with_roles({granted_code});

    const auto resp =
        run_grant_operations_service(ctx).create_run_grant(request_for(r, resource_name()));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("party"));
}

TEST_CASE("create_run_grant_refuses_a_context_without_the_persons_permissions", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto a = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, granted_code);
    const auto party = boost::uuids::random_generator()();
    const auto ctx = h.context().with_party(h.tenant_id(), party, {party}, a.username);

    const auto resp =
        run_grant_operations_service(ctx).create_run_grant(request_for(r, resource_name()));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("permissions"));
}

TEST_CASE("create_run_grant_returns_the_active_grant_for_the_same_resource", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto a = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, granted_code);
    const auto ctx = person(h, a, boost::uuids::random_generator()(), {granted_code});
    run_grant_operations_service sut(ctx);
    const auto resource = resource_name();

    const auto first = sut.create_run_grant(request_for(r, resource));
    const auto second = sut.create_run_grant(request_for(r, resource));
    REQUIRE(first.success);
    REQUIRE(second.success);
    CHECK_FALSE(second.created);
    CHECK(second.grant_id == first.grant_id);
}

TEST_CASE("revoke_run_grant_by_the_grantor_ends_the_grant_and_repeats_safely", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto a = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, granted_code);
    const auto ctx = person(h, a, boost::uuids::random_generator()(), {granted_code});
    run_grant_operations_service sut(ctx);
    const auto created = sut.create_run_grant(request_for(r, resource_name()));
    REQUIRE(created.success);

    revoke_run_grant_request req;
    req.grant_id = created.grant_id;
    req.reason = "unscheduled";
    REQUIRE(sut.revoke_run_grant(req).success);

    const auto stored = run_grant_service(ctx).find_grant(id_of(created.grant_id));
    REQUIRE(stored);
    CHECK(stored->revoked_at != std::chrono::system_clock::time_point{});
    CHECK(stored->revoke_reason == "unscheduled");
    CHECK(stored->revoked_by == a.username);

    const auto again = sut.revoke_run_grant(req);
    CHECK(again.success);
    CHECK_THAT(again.message, ContainsSubstring("already"));
}

TEST_CASE("create_run_grant_reactivates_a_revoked_grant_under_the_same_id", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto a = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, granted_code);
    const auto ctx = person(h, a, boost::uuids::random_generator()(), {granted_code});
    run_grant_operations_service sut(ctx);
    const auto resource = resource_name();
    const auto first = sut.create_run_grant(request_for(r, resource));
    revoke_run_grant_request revoke;
    revoke.grant_id = first.grant_id;
    REQUIRE(sut.revoke_run_grant(revoke).success);

    const auto again = sut.create_run_grant(request_for(r, resource));
    REQUIRE(again.success);
    CHECK(again.created);
    CHECK(again.grant_id == first.grant_id);
    const auto stored = run_grant_service(ctx).find_grant(id_of(again.grant_id));
    REQUIRE(stored);
    CHECK(stored->revoked_at == std::chrono::system_clock::time_point{});
}

TEST_CASE("revoke_run_grant_needs_the_grantor_or_the_revoke_permission", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto grantor = write_account(h, gen);
    const auto other = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, granted_code);
    const auto party = boost::uuids::random_generator()();
    const auto created = run_grant_operations_service(person(h, grantor, party, {granted_code}))
                             .create_run_grant(request_for(r, resource_name()));
    REQUIRE(created.success);
    revoke_run_grant_request req;
    req.grant_id = created.grant_id;

    const auto refused =
        run_grant_operations_service(person(h, other, party, {granted_code})).revoke_run_grant(req);
    CHECK_FALSE(refused.success);

    const auto allowed =
        run_grant_operations_service(person(h, other, party, {"iam::run_grants:revoke"}))
            .revoke_run_grant(req);
    CHECK(allowed.success);
}

TEST_CASE("create_run_grant_renews_a_grant_past_its_not_after", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto a = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, granted_code);
    const auto ctx = person(h, a, boost::uuids::random_generator()(), {granted_code});
    run_grant_operations_service sut(ctx);
    const auto resource = resource_name();
    const auto first = sut.create_run_grant(request_for(r, resource));
    REQUIRE(first.success);

    run_grant_service grants(ctx);
    auto expired = grants.find_grant(id_of(first.grant_id));
    REQUIRE(expired);
    expired->not_after = std::chrono::system_clock::now() - std::chrono::hours(1);
    expired->change_reason_code = "system.test";
    grants.save_grant(*expired);

    const auto renewed = sut.create_run_grant(request_for(r, resource));
    REQUIRE(renewed.success);
    CHECK(renewed.created);
    CHECK(renewed.grant_id == first.grant_id);
    const auto stored = grants.find_grant(id_of(renewed.grant_id));
    REQUIRE(stored);
    CHECK(stored->not_after == std::chrono::system_clock::time_point{});
}

TEST_CASE("create_run_grant_replaces_an_active_grant_with_a_new_role", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto a = write_account(h, gen);
    const auto first_role = write_role_bundling(h, gen, granted_code);
    const auto second_role = write_role_bundling(h, gen, granted_code);
    const auto ctx = person(h, a, boost::uuids::random_generator()(), {granted_code});
    run_grant_operations_service sut(ctx);
    const auto resource = resource_name();
    const auto first = sut.create_run_grant(request_for(first_role, resource));
    REQUIRE(first.success);

    const auto replaced = sut.create_run_grant(request_for(second_role, resource));
    REQUIRE(replaced.success);
    CHECK(replaced.created);
    CHECK(replaced.grant_id == first.grant_id);
    CHECK_THAT(replaced.message, ContainsSubstring("Replaced"));
    const auto stored = run_grant_service(ctx).find_grant(id_of(replaced.grant_id));
    REQUIRE(stored);
    CHECK(stored->role_id == second_role.id);
}

TEST_CASE("revoke_run_grant_by_an_administrator_of_another_party_keeps_the_grants_party", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto grantor = write_account(h, gen);
    const auto admin = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, granted_code);
    const auto party = boost::uuids::random_generator()();
    const auto other_party = boost::uuids::random_generator()();
    const auto created = run_grant_operations_service(person(h, grantor, party, {granted_code}))
                             .create_run_grant(request_for(r, resource_name()));
    REQUIRE(created.success);

    const auto admin_ctx =
        h.context()
            .with_party(h.tenant_id(), other_party, {other_party, party}, admin.username)
            .with_roles({"iam::run_grants:revoke"});
    revoke_run_grant_request req;
    req.grant_id = created.grant_id;
    REQUIRE(run_grant_operations_service(admin_ctx).revoke_run_grant(req).success);

    const auto stored = run_grant_service(admin_ctx).find_grant(id_of(created.grant_id));
    REQUIRE(stored);
    CHECK(stored->party_id == party);
    CHECK(stored->revoked_by == admin.username);
}
