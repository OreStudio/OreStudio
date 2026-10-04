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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>

namespace {

const std::string test_suite("ores.database.tests");
const std::string tags("[session_settings]");

}

using ores::database::repository::execute_raw_multi_column_query;

TEST_CASE("a raw query's setting holding a quote reaches the session unchanged", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto ctx = h.context().with_tenant(h.tenant_id(), "o'brien");
    const auto rows =
        execute_raw_multi_column_query(ctx, "select ores_iam_current_actor_fn()", lg, "actor");
    REQUIRE(rows.size() == 1);
    CHECK(rows[0][0] == std::optional<std::string>("o'brien"));
}
