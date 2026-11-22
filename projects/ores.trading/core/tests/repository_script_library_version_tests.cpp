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
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.core/repository/script_library_version_repository.hpp"
#include "ores.trading.core/repository/script_repository.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>

using namespace ores::trading;

namespace {

const auto world = ores::utility::uuid::tenant_id::system().to_uuid();

}

TEST_CASE("a stated version of a world script reads back as its script and the rows beneath it",
          "[repository][script_library]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = h.context();

    // The read returns the tenant's own script of the name beside the world's,
    // and a test of the tenant's publish may have written one.
    auto current = repository::script_repository().read_latest(ctx, "Accumulator01");
    std::erase_if(current, [](const auto& s) { return !s.tenant_id.is_system(); });
    REQUIRE(current.size() == 1u);
    const auto version = current.front().version;

    messaging::export_script_library_response out;
    repository::script_library_version_repository().read(
        ctx,
        {{.tenant_id = world, .name = "Accumulator01", .version = version},
         {.tenant_id = world, .name = "Accumulator01", .version = version}},
        out);

    CHECK(out.missing.empty());
    REQUIRE(out.scripts.size() == 1u);
    CHECK(out.scripts.front().version == version);
    CHECK_FALSE(out.variants.empty());
    CHECK_FALSE(out.results.empty());
    CHECK(std::ranges::all_of(out.variants, [&](const auto& v) {
        return v.script_name == "Accumulator01" && v.script_version == version;
    }));
    CHECK(std::ranges::all_of(out.results, [&](const auto& r) {
        return r.script_name == "Accumulator01" && r.script_version == version;
    }));
}

TEST_CASE("a version the store does not hold is named as missing", "[repository][script_library]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = h.context();
    const auto elsewhere = boost::uuids::random_generator()();

    messaging::export_script_library_response out;
    repository::script_library_version_repository().read(
        ctx,
        {{.tenant_id = world, .name = "Accumulator01", .version = 999999},
         {.tenant_id = elsewhere, .name = "Accumulator01", .version = 1}},
        out);

    CHECK(out.scripts.empty());
    CHECK(out.variants.empty());
    REQUIRE(out.missing.size() == 2u);
    CHECK(out.missing[0].version == 999999);
    CHECK(out.missing[1].tenant_id == elsewhere);
}
