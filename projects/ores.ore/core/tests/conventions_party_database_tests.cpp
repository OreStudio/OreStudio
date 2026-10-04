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
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/party_scope.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.refdata.core/repository/deposit_convention_repository.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "party_fixture.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <string>

namespace {

const std::string tags("[ore][conventions][party]");

std::vector<ores::refdata::domain::deposit_convention> corpus_deposits() {
    ores::ore::domain::conventions c;
    ores::ore::domain::load_data(
        ores::platform::filesystem::file::read_content(ores::testing::project_root::resolve(
            "external/ore/examples/Legacy/Example_62/Input/conventions.xml")),
        c);
    return ores::ore::domain::conventions_mapper::map(c).deposit;
}

std::set<std::string> ids_of(const std::vector<ores::refdata::domain::deposit_convention>& rows,
                             const boost::uuids::uuid& party) {
    std::set<std::string> out;
    for (const auto& r : rows)
        if (r.party_id == party)
            out.insert(r.id);
    return out;
}

}

TEST_CASE("two parties each hold conventions with the same ids", tags) {
    ores::testing::scoped_database_helper h;
    auto parties = ores::ore::tests::make_two_parties(h);

    auto for_a = corpus_deposits();
    REQUIRE_FALSE(for_a.empty());
    auto for_b = for_a;
    ores::ore::domain::assign_party(for_a, parties.a);
    ores::ore::domain::assign_party(for_b, parties.b);

    ores::refdata::repository::deposit_convention_repository repo;
    repo.write(parties.a_context, for_a);
    repo.write(parties.b_context, for_b);

    std::set<std::string> expected;
    for (const auto& r : for_a)
        expected.insert(r.id);

    const auto seen_by_a = repo.read_latest(parties.a_context);
    CHECK(ids_of(seen_by_a, parties.a) == expected);
    CHECK(ids_of(seen_by_a, parties.b).empty());

    const auto seen_by_b = repo.read_latest(parties.b_context);
    CHECK(ids_of(seen_by_b, parties.b) == expected);
    CHECK(ids_of(seen_by_b, parties.a).empty());
}
