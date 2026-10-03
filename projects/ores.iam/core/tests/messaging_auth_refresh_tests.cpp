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
#include "ores.iam.core/messaging/auth_handler.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[refresh]");

using ores::iam::messaging::auth_refresh_refusal;
using ores::iam::messaging::auth_refreshed_permissions;
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
