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
#include "ores.database/domain/party_scope.hpp"
#include "ores.reporting.core/repository/configuration_repository.hpp"
#include "ores.testing/party_database_fixture.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>

/**
 * @file pooled_session_tests.cpp
 * @brief Session settings on connections the pool hands out again.
 *
 * A pooled connection keeps the settings of the context that used it last, so
 * these cases alternate contexts over many acquires, through a repository, and
 * read what each context is allowed to see.
 */

namespace {

ores::reporting::domain::configuration make_header(const boost::uuids::uuid& party) {
    ores::reporting::domain::configuration header;
    header.id = boost::uuids::random_generator()();
    header.name = "pooled session";
    header.configuration_type_code = "risk_report_config";
    header.owning_component = "ores.reporting";
    ores::database::domain::assign_party(header, party);
    header.modified_by = "test.actor";
    header.performed_by = "test.actor";
    header.change_reason_code = "system.test";
    header.change_commentary = "Written by the test suite.";
    return header;
}

}

TEST_CASE("a context without a party does not inherit a pooled connection's party",
          "[ore][session][database]") {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::testing::make_two_parties(h);
    ores::reporting::repository::configuration_repository repo;
    const auto header = make_header(parties.a);
    repo.write(parties.a_context, header);

    const auto tenant_only = h.context().with_tenant(h.tenant_id(), "");
    const auto holds_a = [&](const auto& rows) {
        return std::ranges::any_of(rows, [&](const auto& r) { return r.id == header.id; });
    };
    for (int i = 0; i < 50; ++i) {
        INFO("round " << i);
        CHECK_FALSE(holds_a(repo.read_latest(parties.b_context)));
        CHECK(holds_a(repo.read_latest(tenant_only)));
    }
}

TEST_CASE("an actor holding a quote reaches a pooled session", "[ore][session][database]") {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::testing::make_two_parties(h);
    const auto ctx = h.context().with_party(h.tenant_id(), parties.a, {parties.a}, "o'brien");
    ores::reporting::repository::configuration_repository repo;
    const auto header = make_header(parties.a);
    CHECK_NOTHROW(repo.write(ctx, header));
}
