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
#include "ores.iam.api/domain/account.hpp"
#include "ores.iam.api/domain/permission.hpp"
#include "ores.iam.api/domain/permission_codes.hpp"
#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.api/domain/role_permission.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.api/generators/account_role_generator.hpp"
#include "ores.iam.api/generators/permission_generator.hpp"
#include "ores.iam.api/generators/role_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/account_role_repository.hpp"
#include "ores.iam.core/repository/permission_repository.hpp"
#include "ores.iam.core/repository/role_permission_repository.hpp"
#include "ores.iam.core/repository/role_repository.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.iam.tests.authorization_access");
const std::string tags("[authorization]");

using ores::iam::domain::account;
using ores::iam::domain::role;
using ores::iam::generators::generate_synthetic_account;
using ores::iam::generators::generate_synthetic_account_role;
using ores::iam::generators::generate_synthetic_permission;
using ores::iam::generators::generate_synthetic_role;
using ores::iam::repository::account_repository;
using ores::iam::repository::account_role_repository;
using ores::iam::repository::permission_repository;
using ores::iam::repository::role_permission_repository;
using ores::iam::repository::role_repository;
using ores::iam::service::authorization_service;
using ores::testing::database_helper;
using ores::utility::domain::outcome;
using ores::utility::generation::generation_context;
using namespace ores::logging;
namespace permissions = ores::iam::domain::permissions;

account write_account(database_helper& h, generation_context& gen) {
    account_repository repo;
    auto value = generate_synthetic_account(gen);
    repo.write(h.context(), value);
    return value;
}

void assign(database_helper& h, generation_context& gen, const account& a, const role& r) {
    account_role_repository repo(h.context());
    auto assignment = generate_synthetic_account_role(gen);
    assignment.account_id = a.id;
    assignment.role_id = r.id;
    repo.write(assignment);
}

/*
 * A role bundling one permission code, written once per run: the code is the
 * tenant's natural key, so a second test that needs it joins the row the first
 * one wrote rather than colliding with the unique index.
 */
role write_role_bundling(database_helper& h, generation_context& gen, const std::string& code) {
    role_repository roles;
    permission_repository permissions_repo;
    role_permission_repository links(h.context());

    auto r = generate_synthetic_role(gen);
    roles.write(h.context(), r);

    auto existing = permissions_repo.read_latest_by_code(h.context(), code);
    auto p = existing.empty() ? generate_synthetic_permission(gen) : existing.front();
    if (existing.empty()) {
        p.code = code;
        permissions_repo.write(h.context(), p);
    }

    ores::iam::domain::role_permission link;
    link.tenant_id = h.tenant_id();
    link.role_id = r.id;
    link.permission_id = p.id;
    links.write(link);

    return r;
}

}

TEST_CASE("read_own_access_returns_roles_with_permissions_and_assignment_tail", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    auto r = write_role_bundling(h, gen, std::string(permissions::accounts_read));
    assign(h, gen, caller, r);

    authorization_service svc(h.context());
    const auto access = svc.read_own_access(caller.id);

    BOOST_LOG_SEV(lg, debug) << "Own access rows: " << access.roles.size();

    REQUIRE(access.result.outcome == outcome::ok);
    REQUIRE(access.roles.size() == 1);

    const auto& entry = access.roles.front();
    CHECK(entry.role.id == r.id);
    CHECK(entry.role.name == r.name);
    REQUIRE(entry.permission_codes.size() == 1);
    CHECK(entry.permission_codes.front() == permissions::accounts_read);
    CHECK(entry.assigned_by == h.db_user());
    CHECK(entry.change_reason_code == "system.test");
    CHECK(entry.change_commentary == "Synthetic test data");
    CHECK(entry.assigned_at.time_since_epoch().count() != 0);
}

TEST_CASE("read_account_access_is_denied_without_the_read_permission", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    auto other = write_account(h, gen);
    auto r = write_role_bundling(h, gen, std::string(permissions::accounts_read));
    assign(h, gen, other, r);

    authorization_service svc(h.context());
    const auto access = svc.read_account_access(caller.id, other.id);

    BOOST_LOG_SEV(lg, debug) << "Denied access rows: " << access.roles.size();

    CHECK(access.result.outcome == outcome::denied);
    CHECK(access.result.code == permissions::roles_read);
    CHECK_FALSE(access.result.message.empty());
    CHECK(access.roles.empty());
}

TEST_CASE("read_account_access_returns_the_composed_shape_with_the_read_permission", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    auto caller_role = write_role_bundling(h, gen, std::string(permissions::roles_read));
    assign(h, gen, caller, caller_role);

    auto other = write_account(h, gen);
    auto other_role = write_role_bundling(h, gen, std::string(permissions::accounts_read));
    assign(h, gen, other, other_role);

    authorization_service svc(h.context());
    const auto access = svc.read_account_access(caller.id, other.id);

    BOOST_LOG_SEV(lg, debug) << "Privileged access rows: " << access.roles.size();

    REQUIRE(access.result.outcome == outcome::ok);
    REQUIRE(access.roles.size() == 1);

    const auto& entry = access.roles.front();
    CHECK(entry.role.id == other_role.id);
    REQUIRE(entry.permission_codes.size() == 1);
    CHECK(entry.permission_codes.front() == permissions::accounts_read);
    CHECK_FALSE(entry.assigned_by.empty());
    CHECK(entry.assigned_at.time_since_epoch().count() != 0);
}

TEST_CASE("read_own_access_for_an_account_holding_nothing_is_ok_and_empty", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);

    authorization_service svc(h.context());
    const auto access = svc.read_own_access(caller.id);

    BOOST_LOG_SEV(lg, debug) << "Empty own access rows: " << access.roles.size();

    CHECK(access.result.outcome == outcome::ok);
    CHECK(access.roles.empty());
}

/*
 * The request context carries the actor's username, and the permission check
 * is keyed by account id. A read that took the username for an account id
 * failed every live request, so the resolution is tested on its own rather
 * than through a read whose answer would look the same.
 */
TEST_CASE("caller_account_resolves_the_context_actor_to_their_account", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);

    authorization_service svc(h.context().with_tenant(h.tenant_id(), caller.username));
    const auto resolved = svc.caller_account();

    BOOST_LOG_SEV(lg, debug) << "Actor '" << caller.username << "' resolved: " << resolved.has_value();

    REQUIRE(resolved.has_value());
    CHECK(*resolved == caller.id);
}

TEST_CASE("caller_account_is_empty_when_no_account_carries_the_actor", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    authorization_service svc(
        h.context().with_tenant(h.tenant_id(), "no-such-actor@ores.invalid"));
    const auto resolved = svc.caller_account();

    CHECK_FALSE(resolved.has_value());
}
