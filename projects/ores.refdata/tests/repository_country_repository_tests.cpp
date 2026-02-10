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
#include "ores.refdata/repository/country_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/country.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/country_generator.hpp"

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
    auto countries = generate_fictional_countries(1);
    REQUIRE(!countries.empty());
    auto cntry = countries[0];
    BOOST_LOG_SEV(lg, debug) << "Country: " << cntry;

    country_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), cntry));
}

TEST_CASE("write_multiple_countries", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto countries = generate_fictional_countries(3);
    BOOST_LOG_SEV(lg, debug) << "Countries: " << countries;

    country_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), countries));
}

TEST_CASE("read_latest_countries", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto written_countries = generate_fictional_countries(3);
    BOOST_LOG_SEV(lg, debug) << "Written countries: " << written_countries;

    country_repository repo;
    repo.write(h.context(), written_countries);

    auto read_countries = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read countries: " << read_countries;

    CHECK(read_countries.size() >= written_countries.size());
}

TEST_CASE("read_latest_country_by_alpha2_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto countries = generate_fictional_countries(1);
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
    country_repository repo;

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent alpha2 code: "
                             << nonexistent_code;

    auto read_countries = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read countries: " << read_countries;

    CHECK(read_countries.size() == 0);
}
