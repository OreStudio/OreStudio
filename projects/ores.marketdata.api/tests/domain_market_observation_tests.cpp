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
#include "ores.marketdata.api/domain/market_observation.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_observation_json_io.hpp" // IWYU pragma: keep.

namespace {

using ores::marketdata::domain::market_observation;

const std::string_view test_suite("ores.marketdata.api.tests");
const std::string tags("[domain]");

market_observation make_curve_observation(
    const std::string& point_id = "1Y",
    const std::string& value = "0.034567") {

    static boost::uuids::random_generator gen;
    market_observation o;
    o.id = gen();
    o.series_id = gen();
    o.observation_date = std::chrono::year{2024} / std::chrono::month{3} /
                         std::chrono::day{20};
    o.point_id = point_id;
    o.value = value;
    o.source = "BLOOMBERG";
    o.recorded_at = std::chrono::system_clock::now();
    return o;
}

}

using namespace ores::logging;

TEST_CASE("create_curve_observation_with_tenor", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_curve_observation("1Y", "0.034567");
    BOOST_LOG_SEV(lg, info) << "Curve observation: " << sut;

    CHECK(!sut.id.is_nil());
    CHECK(!sut.series_id.is_nil());
    CHECK(sut.observation_date == (std::chrono::year{2024} / std::chrono::month{3} /
                                   std::chrono::day{20}));
    CHECK(sut.point_id.has_value());
    CHECK(sut.point_id.value() == "1Y");
    CHECK(sut.value == "0.034567");
    CHECK(sut.source.has_value());
    CHECK(sut.source.value() == "BLOOMBERG");
}

TEST_CASE("create_scalar_observation_without_point_id", tags) {
    auto lg(make_logger(test_suite));

    static boost::uuids::random_generator gen;
    market_observation sut;
    sut.id = gen();
    sut.series_id = gen();
    sut.observation_date = std::chrono::year{2024} / std::chrono::month{6} /
                           std::chrono::day{1};
    sut.value = "1.08450";
    // point_id is null for scalar series (FX spot, equity spot)
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Scalar observation: " << sut;

    CHECK(!sut.point_id.has_value());
    CHECK(sut.value == "1.08450");
    CHECK(!sut.source.has_value());
}

TEST_CASE("create_surface_observation_with_compound_point_id", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_curve_observation("1Y/ATM", "0.1234");
    BOOST_LOG_SEV(lg, info) << "Surface observation: " << sut;

    CHECK(sut.point_id.has_value());
    CHECK(sut.point_id.value() == "1Y/ATM");
}

TEST_CASE("market_observation_json_serialisation", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_curve_observation("5Y", "0.041200");

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();
    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("0.041200") != std::string::npos);
    CHECK(json_output.find("5Y") != std::string::npos);
    CHECK(json_output.find("2024") != std::string::npos);
}

TEST_CASE("create_multiple_observations_for_curve", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> tenors = {"1M", "3M", "6M", "1Y", "2Y", "5Y", "10Y"};
    std::vector<market_observation> observations;
    observations.reserve(tenors.size());

    for (const auto& tenor : tenors) {
        observations.push_back(make_curve_observation(tenor, "0.035000"));
    }
    BOOST_LOG_SEV(lg, info) << "Curve point count: " << observations.size();

    CHECK(observations.size() == tenors.size());
    for (std::size_t i = 0; i < tenors.size(); ++i) {
        REQUIRE(observations[i].point_id.has_value());
        CHECK(observations[i].point_id.value() == tenors[i]);
    }
}
