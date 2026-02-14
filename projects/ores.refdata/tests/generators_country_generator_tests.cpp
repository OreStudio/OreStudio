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
#include <set>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/country.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/country_generator.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[generators]");

}

using namespace ores::refdata::generators;
using namespace ores::logging;

TEST_CASE("generate_fictional_countries_returns_all_when_no_count", tags) {
    auto lg(make_logger(test_suite));

    auto countries = generate_fictional_countries();
    BOOST_LOG_SEV(lg, debug) << "Generated fictional countries: " << countries;

    CHECK(countries.size() == 50);
}

TEST_CASE("generate_fictional_countries_returns_requested_count", tags) {
    auto lg(make_logger(test_suite));

    auto countries = generate_fictional_countries(5);
    BOOST_LOG_SEV(lg, debug) << "Generated 5 fictional countries: " << countries;

    CHECK(countries.size() == 5);
}

TEST_CASE("generate_fictional_countries_returns_all_when_count_exceeds_available", tags) {
    auto lg(make_logger(test_suite));

    auto countries = generate_fictional_countries(100);
    BOOST_LOG_SEV(lg, debug) << "Generated fictional countries with large count: " << countries;

    CHECK(countries.size() == 50);
}

TEST_CASE("generate_fictional_countries_has_unique_alpha2_codes", tags) {
    auto lg(make_logger(test_suite));

    auto countries = generate_fictional_countries();

    std::set<std::string> codes;
    for (const auto& c : countries)
        codes.insert(c.alpha2_code);

    CHECK(codes.size() == 50);
}

TEST_CASE("generate_fictional_countries_has_expected_first_country", tags) {
    auto lg(make_logger(test_suite));

    auto countries = generate_fictional_countries(1);
    BOOST_LOG_SEV(lg, debug) << "First fictional country: " << countries;

    REQUIRE(countries.size() == 1);
    CHECK(countries[0].alpha2_code == "AL");
    CHECK(countries[0].name == "Aerilon");
    CHECK(!countries[0].modified_by.empty());
    CHECK(countries[0].recorded_at != std::chrono::system_clock::time_point{});
}

TEST_CASE("generate_fictional_countries_contains_known_countries", tags) {
    auto lg(make_logger(test_suite));

    auto countries = generate_fictional_countries();

    std::set<std::string> codes;
    for (const auto& c : countries)
        codes.insert(c.alpha2_code);

    CHECK(codes.count("AL") == 1);  // Aerilon
    CHECK(codes.count("ZE") == 1);  // Zephyria
    CHECK(codes.count("ER") == 1);  // Eriador
    CHECK(codes.count("KR") == 1);  // Krynn
}
