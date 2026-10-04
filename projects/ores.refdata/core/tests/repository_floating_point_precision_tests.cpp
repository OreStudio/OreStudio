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
#include "ores.refdata.api/generators/commodity_forward_convention_generator.hpp"
#include "ores.refdata.core/repository/commodity_forward_convention_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

/**
 * @file repository_floating_point_precision_tests.cpp
 * @brief A double written through a generated repository reads back unchanged.
 *
 * sqlgen once wrote every double with std::to_string, which keeps six decimal
 * places: 1e-12 was stored as 0 and 0.0123456789 as 0.012346. The value here
 * is written and read back through the same generated repository every entity
 * uses, so the check covers the path, not a helper.
 */

namespace {

const std::string tags("[repository][floating_point]");

using ores::refdata::repository::commodity_forward_convention_repository;

double round_trip(ores::testing::scoped_database_helper& h, double value) {
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto convention =
        ores::refdata::generators::generate_synthetic_commodity_forward_convention(gen_ctx);
    convention.points_factor = value;

    commodity_forward_convention_repository repo;
    repo.write(h.context(), convention);

    const auto read = repo.read_latest(h.context(), convention.id);
    REQUIRE(read.size() == 1);
    REQUIRE(read.front().points_factor.has_value());
    return *read.front().points_factor;
}

}

TEST_CASE("a value smaller than a millionth survives the database", tags) {
    ores::testing::scoped_database_helper h;
    CHECK(round_trip(h, 1e-12) == 1e-12);
}

TEST_CASE("a value with more than six decimal places survives the database", tags) {
    ores::testing::scoped_database_helper h;
    CHECK(round_trip(h, 0.0123456789) == 0.0123456789);
}

TEST_CASE("a large value survives the database", tags) {
    ores::testing::scoped_database_helper h;
    CHECK(round_trip(h, 123456789.123456789) == 123456789.123456789);
}
