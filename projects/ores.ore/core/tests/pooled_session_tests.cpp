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
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/run_document_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.reporting.core/repository/report_run_setup_repository.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "party_fixture.hpp"
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

const std::string tags("[ore][session][database]");

ores::reporting::domain::report_run_setup make_setup(const boost::uuids::uuid& party) {
    ores::ore::domain::ore run;
    ores::ore::domain::load_data(
        ores::platform::filesystem::file::read_content(ores::testing::project_root::resolve(
            "external/ore/examples/ORE-Python/Notebooks/Example_1/Input/ore.xml")),
        run);
    auto next_id = boost::uuids::random_generator();
    auto setup = ores::ore::domain::run_document_mapper::map_setup(run);
    setup.id = next_id();
    setup.report_definition_id = next_id();
    ores::database::domain::assign_party(setup, party);
    return setup;
}

}

TEST_CASE("a context without a party does not inherit a pooled connection's party", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    ores::reporting::repository::report_run_setup_repository repo;
    const auto setup = make_setup(parties.a);
    repo.write(parties.a_context, setup);

    const auto tenant_only = h.context().with_tenant(h.tenant_id(), "");
    const auto holds_a = [&](const auto& rows) {
        return std::ranges::any_of(rows, [&](const auto& r) { return r.id == setup.id; });
    };
    for (int i = 0; i < 50; ++i) {
        INFO("round " << i);
        CHECK_FALSE(holds_a(repo.read_latest(parties.b_context)));
        CHECK(holds_a(repo.read_latest(tenant_only)));
    }
}

TEST_CASE("an actor holding a quote reaches a pooled session", tags) {
    ores::testing::scoped_database_helper h;
    const auto parties = ores::ore::tests::make_two_parties(h);
    const auto ctx = h.context().with_party(h.tenant_id(), parties.a, {parties.a}, "o'brien");
    ores::reporting::repository::report_run_setup_repository repo;
    const auto setup = make_setup(parties.a);
    CHECK_NOTHROW(repo.write(ctx, setup));
}
