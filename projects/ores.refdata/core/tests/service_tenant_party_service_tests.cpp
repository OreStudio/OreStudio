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
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/service/tenant_party_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>

using ores::refdata::messaging::list_tenant_parties_request;
using ores::refdata::repository::party_repository;
using ores::refdata::service::tenant_party_service;
using ores::testing::scoped_database_helper;
using ores::utility::uuid::tenant_id;

namespace {

const std::string tags("[service][tenant_party]");

/*
 * A system administrator's request context: the test's database context moved
 * to the system tenant, which is where every caller of this read lives.
 */
ores::database::context system_caller(scoped_database_helper& h) {
    return h.context().with_tenant(tenant_id::system(), h.db_user());
}

}

TEST_CASE("a_system_caller_reads_the_named_tenants_parties_and_no_others", tags) {
    scoped_database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    party_repository repo;

    auto party = ores::refdata::generators::generate_synthetic_party(gen_ctx);
    party.change_reason_code = "system.test";
    party.parent_party_id = repo.read_system_party(h.context(), h.tenant_id().to_string()).at(0).id;
    repo.write(h.context(), party);

    tenant_party_service svc(system_caller(h));
    const auto page = svc.list_tenant_parties(
        list_tenant_parties_request{.tenant_id = h.tenant_id().to_string(), .limit = 1000});

    REQUIRE(page.success);
    CHECK(std::ranges::any_of(page.parties, [&](const auto& p) { return p.id == party.id; }));
    CHECK(std::ranges::all_of(page.parties,
                              [&](const auto& p) { return p.tenant_id == h.tenant_id(); }));
    CHECK(page.total == page.parties.size());
}

TEST_CASE("a_caller_outside_the_system_tenant_is_refused", tags) {
    scoped_database_helper h;

    tenant_party_service svc(h.context());
    const auto page = svc.list_tenant_parties(
        list_tenant_parties_request{.tenant_id = h.tenant_id().to_string()});

    CHECK_FALSE(page.success);
    CHECK(page.parties.empty());
}

TEST_CASE("the_system_tenant_is_refused_as_a_target", tags) {
    scoped_database_helper h;

    tenant_party_service svc(system_caller(h));
    const auto page = svc.list_tenant_parties(
        list_tenant_parties_request{.tenant_id = tenant_id::system().to_string()});

    CHECK_FALSE(page.success);
    CHECK(page.parties.empty());
}

TEST_CASE("a_tenant_id_that_is_not_an_id_is_refused", tags) {
    scoped_database_helper h;

    tenant_party_service svc(system_caller(h));
    const auto page =
        svc.list_tenant_parties(list_tenant_parties_request{.tenant_id = "not-a-tenant"});

    CHECK_FALSE(page.success);
}
