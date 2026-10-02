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
#include "ores.iam.core/service/tenant_session_service.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>

using ores::iam::messaging::enter_tenant_request;
using ores::iam::service::describe;
using ores::iam::service::tenant_session_caller;
using ores::iam::service::tenant_session_refusal;
using ores::iam::service::tenant_session_service;
using ores::testing::scoped_database_helper;
using ores::utility::uuid::tenant_id;

namespace {

const std::string tags("[service][tenant_session]");
const std::string test_secret("super-secret-key-for-testing-purposes-only-32bytes!");

auto signer() {
    return ores::security::jwt::jwt_authenticator::create_hs256(test_secret);
}

tenant_session_caller administrator(std::vector<std::string> permissions = {"*"}) {
    return {.account_id = boost::uuids::random_generator()(),
            .username = "admin",
            .session_id = "session-1",
            .tenant_id = tenant_id::system(),
            .party_id = std::nullopt,
            .acting_from_tenant_id = std::nullopt,
            .permissions = std::move(permissions)};
}

/*
 * The events the tenant's own trail holds for one account, read the way an
 * auditor would read them.
 */
std::vector<std::string> events_for(scoped_database_helper& h,
                                    const boost::uuids::uuid& account_id) {
    auto lg(ores::logging::make_logger("ores.iam.tests"));
    return ores::database::repository::execute_parameterized_string_query(
        h.context(),
        "SELECT event_type FROM ores_iam_auth_events_tbl "
        "WHERE tenant_id = $1 AND account_id = $2 ORDER BY event_time",
        {h.tenant_id().to_string(), boost::uuids::to_string(account_id)},
        lg,
        "Reading the tenant's events for one account");
}

}

TEST_CASE("an_administrator_enters_a_tenant_reading_only_as_its_system_party", tags) {
    scoped_database_helper h;
    auto sign = signer();
    tenant_session_service svc(h.context(), sign, std::chrono::seconds{600});
    const auto caller = administrator();

    const auto entered =
        svc.enter(caller, enter_tenant_request{.tenant_id = h.tenant_id().to_string()});

    REQUIRE(entered.success);
    const auto claims = sign.validate(entered.token);
    REQUIRE(claims.has_value());
    CHECK(claims->tenant_id == h.tenant_id().to_string());
    CHECK(claims->acting_from_tenant_id == tenant_id::system().to_string());
    CHECK(claims->subject == boost::uuids::to_string(caller.account_id));
    CHECK(claims->party_id == entered.party_id);
    CHECK_FALSE(entered.party_name.empty());
    CHECK(std::ranges::find(claims->visible_party_ids, entered.party_id) !=
          claims->visible_party_ids.end());
    REQUIRE_FALSE(claims->roles.empty());
    CHECK(std::ranges::all_of(claims->roles, [](const auto& r) { return r.ends_with(":read"); }));
    CHECK(entered.access_lifetime_s == 600);
    CHECK(events_for(h, caller.account_id) == std::vector<std::string>{"tenant_entered"});
}

TEST_CASE("the_tenant_session_carries_only_the_reads_the_administrator_holds", tags) {
    const std::vector<std::string> catalogue{
        "iam::accounts:read", "iam::accounts:write", "refdata::parties:delete",
        "refdata::parties:read"};

    CHECK(tenant_session_service::read_only({"*"}, catalogue) ==
          std::vector<std::string>{"iam::accounts:read", "refdata::parties:read"});
    CHECK(tenant_session_service::read_only({"refdata::parties:read", "refdata::parties:write"},
                                            catalogue) ==
          std::vector<std::string>{"refdata::parties:read"});
    CHECK(tenant_session_service::read_only({"iam::accounts:write"}, catalogue).empty());
}

TEST_CASE("an_entry_is_refused_for_each_rule_it_breaks", tags) {
    scoped_database_helper h;
    tenant_session_service svc(h.context(), signer(), std::chrono::seconds{600});
    const auto target = enter_tenant_request{.tenant_id = h.tenant_id().to_string()};
    const auto refused_with = [](const auto& response, tenant_session_refusal refusal) {
        return !response.success && response.token.empty() && response.message == describe(refusal);
    };

    auto outside = administrator();
    outside.tenant_id = h.tenant_id();
    CHECK(refused_with(svc.enter(outside, target),
                       tenant_session_refusal::outside_system_administration));

    auto inside = administrator();
    inside.acting_from_tenant_id = tenant_id::system().to_string();
    CHECK(refused_with(svc.enter(inside, target), tenant_session_refusal::already_inside_a_tenant));

    CHECK(refused_with(svc.enter(administrator({"iam::tenants:read"}), target),
                       tenant_session_refusal::not_permitted));

    CHECK(refused_with(
        svc.enter(administrator(), enter_tenant_request{.tenant_id = "not-a-tenant"}),
        tenant_session_refusal::unreadable_tenant_id));

    CHECK(refused_with(
        svc.enter(administrator(),
                  enter_tenant_request{.tenant_id = tenant_id::system().to_string()}),
        tenant_session_refusal::system_tenant));

    const auto nobody = boost::uuids::to_string(boost::uuids::random_generator()());
    CHECK(refused_with(svc.enter(administrator(), enter_tenant_request{.tenant_id = nobody}),
                       tenant_session_refusal::unknown_tenant));

    CHECK(refused_with(svc.enter(administrator({"iam::tenants:impersonate"}), target),
                       tenant_session_refusal::no_read_permissions));
}

TEST_CASE("leaving_records_the_exit_and_is_refused_outside_a_tenant", tags) {
    scoped_database_helper h;
    tenant_session_service svc(h.context(), signer(), std::chrono::seconds{600});

    CHECK_FALSE(svc.leave(administrator()).success);

    auto inside = administrator();
    inside.tenant_id = h.tenant_id();
    inside.acting_from_tenant_id = tenant_id::system().to_string();
    REQUIRE(svc.leave(inside).success);
    CHECK(events_for(h, inside.account_id) == std::vector<std::string>{"tenant_left"});
}
