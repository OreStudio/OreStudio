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
#include "ores.refdata.api/domain/currency_market_tier.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/currency_market_tier_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/currency_market_tier_generator.hpp"
#include "ores.refdata.core/repository/currency_market_tier_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <set>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::repository::currency_market_tier_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_currency_market_tier", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cmt = generate_synthetic_currency_market_tier(ctx);
    cmt.code = cmt.code + "_" + std::string(faker::string::alphanumeric(8));
    cmt.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Currency market tier: " << cmt;

    currency_market_tier_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), cmt));
}

TEST_CASE("read_latest_currency_market_tiers_no_duplicate_codes", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    currency_market_tier_repository repo;

    // Regression: reads must be tenant-filtered. Without the filter the
    // system-tenant seed rows appear alongside the tenant's provisioned
    // copies, duplicating every code in the UI.
    auto read_tiers = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read currency market tiers: " << read_tiers;

    std::set<std::string> codes;
    for (const auto& t : read_tiers)
        codes.insert(t.code);
    CHECK(codes.size() == read_tiers.size());
}

TEST_CASE("read_latest_currency_market_tier_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cmt = generate_synthetic_currency_market_tier(ctx);
    cmt.code = cmt.code + "_" + std::string(faker::string::alphanumeric(8));
    cmt.change_reason_code = "system.test";
    const auto original_name = cmt.name;
    BOOST_LOG_SEV(lg, debug) << "Currency market tier: " << cmt;

    currency_market_tier_repository repo;
    repo.write(h.context(), cmt);

    cmt.name = original_name + " v2";
    repo.write(h.context(), cmt);

    auto read_tiers = repo.read_latest(h.context(), cmt.code);
    BOOST_LOG_SEV(lg, debug) << "Read currency market tiers: " << read_tiers;

    REQUIRE(read_tiers.size() == 1);
    CHECK(read_tiers[0].code == cmt.code);
    CHECK(read_tiers[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_currency_market_tier_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    currency_market_tier_repository repo;

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_tiers = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read currency market tiers: " << read_tiers;

    CHECK(read_tiers.size() == 0);
}
