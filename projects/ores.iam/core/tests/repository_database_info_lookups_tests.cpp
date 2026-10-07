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
#include "ores.iam.core/repository/database_info_lookups.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;

/**
 * The database row the login answer carries.
 *
 * The read is asserted against the database a test run was created against,
 * which compass db recreate writes the row to. A helper that stops returning
 * the row leaves every field empty and fails here, which is the whole point:
 * the login handler has no seam of its own, so this read is what is tested.
 */
TEST_CASE("read_database_info_carries_the_recorded_build", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::database_helper h;

    const auto info = ores::iam::repository::read_database_info(h.context());

    BOOST_LOG_SEV(lg, debug) << "Database info: fingerprint=" << info.fingerprint
                             << " environment=" << info.environment << " commit=" << info.commit
                             << " created=" << info.created;

    CHECK_FALSE(info.fingerprint.empty());
    CHECK_FALSE(info.environment.empty());
    CHECK_FALSE(info.commit.empty());
    CHECK_FALSE(info.created.empty());
}
