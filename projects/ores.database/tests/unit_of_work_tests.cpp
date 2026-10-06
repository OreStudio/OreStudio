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
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string test_suite("ores.database.tests");
const std::string tags("[unit_of_work]");

}

using ores::database::repository::execute_raw_multi_column_query;
using ores::database::repository::unit_of_work;

TEST_CASE("a unit of work binds its transaction to the context it hands out", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;

    REQUIRE_FALSE(h.context().active_transaction().has_value());

    unit_of_work uow(h.context());
    REQUIRE(uow.ctx().active_transaction().has_value());
    uow.commit();
}

TEST_CASE("a unit of work that is not committed frees its connection", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;

    {
        unit_of_work uow(h.context());
        REQUIRE(uow.ctx().active_transaction().has_value());
    }

    // The destructor rolled the transaction back, so the next acquisition
    // finds a usable connection rather than one left in a transaction.
    const auto rows = execute_raw_multi_column_query(h.context(), "select 1", lg, "probe");
    REQUIRE(rows.size() == 1);
}
