/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/country.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/country_generator.hpp"
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::country;
using ores::refdata::repository::country_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_country", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto countries = generate_fictional_countries(1, ctx);
    REQUIRE(countries.size() == 1);
    auto cntry = countries[0];
    BOOST_LOG_SEV(lg, debug) << "Country: " << cntry;

    country_repository repo;
    repo.write(h.context(), cntry);

    const auto read_countries = repo.read_latest(h.context(), cntry.alpha2_code);
    REQUIRE(read_countries.size() == 1);
    CHECK(read_countries[0].name == cntry.name);
}

TEST_CASE("write_multiple_countries", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto countries = generate_fictional_countries(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Countries: " << countries;

    country_repository repo;
    repo.write(h.context(), countries);

    const auto read_countries = repo.read_latest(h.context());
    for (const auto& written : countries) {
        const auto it = std::ranges::find_if(
            read_countries, [&](const country& c) { return c.alpha2_code == written.alpha2_code; });
        REQUIRE(it != read_countries.end());
        CHECK(it->name == written.name);
    }
}

TEST_CASE("read_latest_countries", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written_countries = generate_fictional_countries(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Written countries: " << written_countries;

    country_repository repo;
    repo.write(h.context(), written_countries);

    auto read_countries = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read countries: " << read_countries;

    for (const auto& written : written_countries) {
        const auto it = std::ranges::find_if(read_countries, [&written](const country& c) {
            return c.alpha2_code == written.alpha2_code;
        });
        REQUIRE(it != read_countries.end());
        CHECK(it->name == written.name);
    }
}

TEST_CASE("read_latest_country_by_alpha2_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto countries = generate_fictional_countries(1, ctx);
    REQUIRE(!countries.empty());
    auto cntry = countries[0];
    const auto original_name = cntry.name;
    BOOST_LOG_SEV(lg, debug) << "Country: " << cntry;

    country_repository repo;
    repo.write(h.context(), cntry);

    cntry.name = original_name + " v2";
    repo.write(h.context(), cntry);

    auto read_countries = repo.read_latest(h.context(), cntry.alpha2_code);
    BOOST_LOG_SEV(lg, debug) << "Read countries: " << read_countries;

    REQUIRE(read_countries.size() == 1);
    CHECK(read_countries[0].alpha2_code == cntry.alpha2_code);
    CHECK(read_countries[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_alpha2_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    country_repository repo;

    // Write a row the read can answer with, so that an empty answer is the key
    // at work rather than a read that does nothing.
    const auto keeper = generate_synthetic_country(ctx);
    repo.write(h.context(), keeper);

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent alpha2 code: " << nonexistent_code;

    auto read_countries = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read countries: " << read_countries;

    const auto answered_with_another_key = std::ranges::any_of(
        read_countries, [&](const country& c) { return c.alpha2_code == nonexistent_code; });
    CHECK_FALSE(answered_with_another_key);

    // The same read does answer for the key that was written.
    const auto keeper_rows = repo.read_latest(h.context(), keeper.alpha2_code);
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].name == keeper.name);
}

TEST_CASE("read_country_versions_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cntry = generate_synthetic_country(ctx);
    BOOST_LOG_SEV(lg, debug) << "Country: " << cntry;

    country_repository repo;
    repo.write(h.context(), cntry);

    CHECK_FALSE(repo.read_at_version(h.context(), cntry.alpha2_code, 99).has_value());
    const auto versions = repo.read_all(h.context(), cntry.alpha2_code);
    REQUIRE(versions.size() == 1);
    CHECK(versions[0].alpha2_code == cntry.alpha2_code);
}

TEST_CASE("read_country_versions_resolve_each_version", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cntry = generate_synthetic_country(ctx);
    const auto original_name = cntry.name;
    BOOST_LOG_SEV(lg, debug) << "Country v1: " << cntry;

    country_repository repo;
    repo.write(h.context(), cntry);

    cntry.name = original_name + " v2";
    BOOST_LOG_SEV(lg, debug) << "Country v2: " << cntry;
    repo.write(h.context(), cntry);

    const auto versions = repo.read_all(h.context(), cntry.alpha2_code);
    REQUIRE(versions.size() == 2);
    CHECK(versions[0].name == original_name + " v2");
    CHECK(versions[1].name == original_name);

    const auto at_v1 = repo.read_at_version(h.context(), cntry.alpha2_code, 1);
    REQUIRE(at_v1.has_value());
    CHECK(at_v1->name == original_name);

    const auto at_v2 = repo.read_at_version(h.context(), cntry.alpha2_code, 2);
    REQUIRE(at_v2.has_value());
    CHECK(at_v2->name == original_name + " v2");
}

TEST_CASE("read_latest_country_includes_the_written_row", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cntry = generate_synthetic_country(ctx);
    BOOST_LOG_SEV(lg, debug) << "Country: " << cntry;

    country_repository repo;
    repo.write(h.context(), cntry);

    const auto read_countries = repo.read_latest(h.context());
    const auto found = std::ranges::any_of(
        read_countries, [&](const auto& v) { return v.alpha2_code == cntry.alpha2_code; });
    CHECK(found);
}
