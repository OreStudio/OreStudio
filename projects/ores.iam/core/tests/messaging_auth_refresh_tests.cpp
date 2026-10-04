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
#include "ores.iam.api/generators/tenant_generator.hpp"
#include "ores.iam.core/messaging/auth_handler.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[refresh]");

using ores::iam::messaging::auth_refresh_refusal;
using ores::iam::messaging::auth_refreshed_permissions;
using ores::iam::messaging::auth_tenant_refusal;
using ores::security::jwt::jwt_claims;

jwt_claims person() {
    jwt_claims claims;
    claims.subject = "3d18212d-fc5f-445c-abba-83fca88889d5";
    claims.tenant_id = "ffffffff-ffff-ffff-ffff-ffffffffffff";
    claims.party_id = "22222222-2222-2222-2222-222222222222";
    return claims;
}

}

/*
 * Which tokens refresh renews. Refresh reads the permissions again, so a token
 * it should not renew would come back carrying a full list.
 */

TEST_CASE("a_persons_token_is_refreshed", tags) {
    CHECK_FALSE(auth_refresh_refusal(person()).has_value());
}

TEST_CASE("a_services_token_names_no_party_and_is_refreshed", tags) {
    auto service = person();
    service.party_id.reset();
    CHECK_FALSE(auth_refresh_refusal(service).has_value());
}

TEST_CASE("a_token_that_only_chooses_a_party_is_not_refreshed", tags) {
    auto choosing = person();
    choosing.party_id.reset();
    choosing.audience = "select_party_only";
    CHECK(auth_refresh_refusal(choosing) == "A token that only chooses a party is not refreshed.");
}

TEST_CASE("a_session_inside_a_tenant_is_not_refreshed", tags) {
    auto inside = person();
    inside.acting_from_tenant_id = "ffffffff-ffff-ffff-ffff-ffffffffffff";
    CHECK(auth_refresh_refusal(inside) == "A session inside a tenant is not refreshed.");
}

TEST_CASE("refresh_refuses_to_read_permissions_for_a_token_it_cannot_place", tags) {
    ores::testing::scoped_database_helper h;

    auto no_tenant = person();
    no_tenant.tenant_id.reset();
    CHECK_THROWS(auth_refreshed_permissions(h.context(), no_tenant));

    auto no_account = person();
    no_account.subject = "not-an-account";
    CHECK_THROWS(auth_refreshed_permissions(h.context(), no_account));
}

/*
 * Refresh admits the same tenants sign-in does, so a person signed in to a
 * tenant that has since closed cannot renew their access.
 */

TEST_CASE("refresh_admits_a_token_of_an_active_tenant", tags) {
    ores::testing::scoped_database_helper h;
    CHECK_FALSE(auth_tenant_refusal(h.context(), person()).has_value());
}

TEST_CASE("refresh_refuses_a_token_of_a_tenant_that_is_not_active", tags) {
    ores::testing::scoped_database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto suspended = ores::iam::generators::generate_synthetic_tenant(gen_ctx);
    suspended.status = "suspended";
    ores::iam::repository::tenant_repository().write(
        h.context().with_tenant(ores::utility::uuid::tenant_id::system(), ""), suspended);

    auto claims = person();
    claims.tenant_id = boost::uuids::to_string(suspended.id);
    CHECK(auth_tenant_refusal(h.context(), claims) == "The tenant is not active.");
}

TEST_CASE("refresh_refuses_a_token_of_a_removed_tenant", tags) {
    ores::testing::scoped_database_helper h;
    const auto system_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto tenant = ores::iam::generators::generate_synthetic_tenant(gen_ctx);
    tenant.status = "active";
    ores::iam::repository::tenant_repository repo;
    repo.write(system_ctx, tenant);

    auto claims = person();
    claims.tenant_id = boost::uuids::to_string(tenant.id);
    REQUIRE_FALSE(auth_tenant_refusal(h.context(), claims).has_value());

    repo.remove(system_ctx, boost::uuids::to_string(tenant.id));
    CHECK(auth_tenant_refusal(h.context(), claims) == "The tenant is closed.");

    auto unknown = person();
    unknown.tenant_id = boost::uuids::to_string(boost::uuids::random_generator()());
    CHECK(auth_tenant_refusal(h.context(), unknown) == "The tenant is closed.");

    auto nameless = person();
    nameless.tenant_id.reset();
    CHECK(auth_tenant_refusal(h.context(), nameless) == "The token names no tenant.");
}
