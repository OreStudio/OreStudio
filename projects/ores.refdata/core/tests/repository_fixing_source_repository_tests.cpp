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
#include "ores.logging/logger.hpp"
#include "ores.refdata.api/domain/fixing_source.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/fixing_source_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/fixing_source_generator.hpp"
#include "ores.refdata.core/repository/fixing_source_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>

using namespace ores::refdata::generators;
using ores::platform::time::time_of_day;
using ores::refdata::repository::fixing_source_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;
using namespace std::chrono_literals;

TEST_CASE("a_fixing_source_reads_back_its_publication_time", "[repository]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    fixing_source_repository repo;
    for (const auto at : {time_of_day(16h, 0min), time_of_day(23h, 59min, 59s)}) {
        auto fs = generate_synthetic_fixing_source(ctx);
        fs.change_reason_code = "system.test";
        fs.publication_time = at;
        ORES_LOG_SEV(lg, debug) << "Fixing source: " << fs;
        repo.write(h.context(), fs);

        const auto rows = repo.read_latest(h.context(), fs.code);
        REQUIRE(rows.size() == 1);
        CHECK(rows[0].publication_time == at);
        CHECK(rows[0].publication_time_zone == fs.publication_time_zone);
    }
}
