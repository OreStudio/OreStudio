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
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/monetary_nature.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/monetary_nature_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/monetary_nature_generator.hpp"
#include "ores.refdata.core/repository/monetary_nature_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <set>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::repository::monetary_nature_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_monetary_nature", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto mn = generate_synthetic_monetary_nature(ctx);
    mn.code = mn.code + "_" + std::string(faker::string::alphanumeric(8));
    mn.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Monetary nature: " << mn;

    monetary_nature_repository repo;
    repo.write(h.context(), mn);

    const auto read_monetary_natures = repo.read_latest(h.context(), mn.code);
    REQUIRE(read_monetary_natures.size() == 1);
    CHECK(read_monetary_natures[0].name == mn.name);
}

TEST_CASE("read_latest_monetary_natures_no_duplicate_codes", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    monetary_nature_repository repo;

    auto keeper = generate_synthetic_monetary_nature(ctx);
    keeper.code = keeper.code + "_" + std::string(faker::string::alphanumeric(8));
    keeper.change_reason_code = "system.test";
    repo.write(h.context(), keeper);

    // Regression: reads must be tenant-filtered. Without the filter the
    // system-tenant seed rows appear alongside the tenant's provisioned
    // copies, duplicating every code in the UI.
    auto read_monetary_natures = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read monetary natures: " << read_monetary_natures;

    // The read answers for the row that was written.
    const auto keeper_it = std::ranges::find_if(
        read_monetary_natures, [&](const auto& mn) { return mn.code == keeper.code; });
    REQUIRE(keeper_it != read_monetary_natures.end());
    CHECK(keeper_it->name == keeper.name);

    std::set<std::string> codes;
    for (const auto& mn : read_monetary_natures)
        codes.insert(mn.code);
    CHECK(codes.size() == read_monetary_natures.size());
}

TEST_CASE("read_latest_monetary_nature_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto mn = generate_synthetic_monetary_nature(ctx);
    mn.code = mn.code + "_" + std::string(faker::string::alphanumeric(8));
    mn.change_reason_code = "system.test";
    const auto original_name = mn.name;
    BOOST_LOG_SEV(lg, debug) << "Monetary nature: " << mn;

    monetary_nature_repository repo;
    repo.write(h.context(), mn);

    mn.name = original_name + " v2";
    repo.write(h.context(), mn);

    auto read_monetary_natures = repo.read_latest(h.context(), mn.code);
    BOOST_LOG_SEV(lg, debug) << "Read monetary natures: " << read_monetary_natures;

    REQUIRE(read_monetary_natures.size() == 1);
    CHECK(read_monetary_natures[0].code == mn.code);
    CHECK(read_monetary_natures[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_monetary_nature_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    monetary_nature_repository repo;

    // Write a row the read can answer with, so that an empty answer is the code
    // at work rather than a read that does nothing.
    auto keeper = generate_synthetic_monetary_nature(ctx);
    keeper.code = keeper.code + "_" + std::string(faker::string::alphanumeric(8));
    keeper.change_reason_code = "system.test";
    repo.write(h.context(), keeper);

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_monetary_natures = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read monetary natures: " << read_monetary_natures;

    const auto answered_with_another_key = std::ranges::any_of(
        read_monetary_natures, [&](const auto& mn) { return mn.code == nonexistent_code; });
    CHECK_FALSE(answered_with_another_key);

    // The same read does answer for the code that was written.
    const auto keeper_rows = repo.read_latest(h.context(), keeper.code);
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].name == keeper.name);
}
