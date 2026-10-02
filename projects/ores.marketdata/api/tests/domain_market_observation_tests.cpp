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
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_observation_json_io.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cctype>
#include <sstream>

namespace {

using ores::marketdata::domain::market_observation;

const std::string_view test_suite("ores.marketdata.api.tests");
const std::string tags("[domain]");

// The observation's own URI: the series' identity plus this row's coordinate
// keys. The helper spells a USD par-swap series and varies only the maturity.
std::string curve_datum_uri(const std::string& maturity) {
    // The grammar lower-cases a URI's coordinate values, so the helper does too:
    // a caller may spell the maturity either way and get the canonical string.
    std::string lower = maturity;
    for (auto& c : lower)
        c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    return "oresmd://ir/usd?tenor=3m&type=quote&quote=ir_swap&metric=rate&maturity=" + lower;
}

market_observation make_curve_observation(const std::string& maturity = "1y",
                                          const std::string& value = "0.034567") {

    static boost::uuids::random_generator gen;
    market_observation o;
    o.id = gen();
    o.series_id = gen();
    o.observation_datetime = std::chrono::sys_days{std::chrono::year{2024} / std::chrono::month{3} /
                                                   std::chrono::day{20}};
    o.oresmd_uri = curve_datum_uri(maturity);
    o.value = value;
    o.source = "BLOOMBERG";
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
    CHECK(sut.observation_datetime ==
          std::chrono::system_clock::time_point{std::chrono::sys_days{
              std::chrono::year{2024} / std::chrono::month{3} / std::chrono::day{20}}});
    CHECK(!sut.oresmd_uri.empty());
    CHECK(sut.oresmd_uri == curve_datum_uri("1y"));
    CHECK(sut.oresmd_uri.find("maturity=1y") != std::string::npos);
    CHECK(sut.value == "0.034567");
    CHECK(!sut.source.empty());
    CHECK(sut.source == "BLOOMBERG");
}

TEST_CASE("create_observation_without_a_coordinate", tags) {
    auto lg(make_logger(test_suite));

    static boost::uuids::random_generator gen;
    market_observation sut;
    sut.id = gen();
    sut.series_id = gen();
    sut.observation_datetime = std::chrono::sys_days{std::chrono::year{2024} /
                                                     std::chrono::month{6} / std::chrono::day{1}};
    sut.value = "1.08450";
    BOOST_LOG_SEV(lg, info) << "Observation without a coordinate: " << sut;

    CHECK(sut.oresmd_uri.empty());
    CHECK(sut.value == "1.08450");
    CHECK(sut.source.empty());
}

TEST_CASE("create_surface_observation_with_a_compound_coordinate", tags) {
    auto lg(make_logger(test_suite));

    // A surface's coordinate is several keys, and the row keeps them the way the
    // URI spells them: expiry and the value slot, which is a delta here.
    auto sut = make_curve_observation("1y", "0.1234");
    sut.oresmd_uri = "oresmd://ir/eur?tenor=2y&type=vol&quote=swaption&expiry=1y&delta=ATM&"
                     "model=rate_lnvol";
    BOOST_LOG_SEV(lg, info) << "Surface observation: " << sut;

    CHECK(sut.oresmd_uri.find("expiry=1y") != std::string::npos);
    CHECK(sut.oresmd_uri.find("delta=ATM") != std::string::npos);
}

TEST_CASE("market_observation_json_serialisation", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_curve_observation("5y", "0.041200");

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();
    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("0.041200") != std::string::npos);
    CHECK(json_output.find("maturity=5y") != std::string::npos);
    CHECK(json_output.find("2024-03-20 00:00:00Z") != std::string::npos);
}

TEST_CASE("create_multiple_observations_for_curve", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> tenors = {"1m", "3m", "6m", "1y", "2y", "5y", "10y"};
    std::vector<market_observation> observations;
    observations.reserve(tenors.size());

    for (const auto& tenor : tenors) {
        observations.push_back(make_curve_observation(tenor, "0.035000"));
    }
    BOOST_LOG_SEV(lg, info) << "Curve point count: " << observations.size();

    CHECK(observations.size() == tenors.size());
    for (std::size_t i = 0; i < tenors.size(); ++i) {
        REQUIRE(!observations[i].oresmd_uri.empty());
        CHECK(observations[i].oresmd_uri == curve_datum_uri(tenors[i]));
    }
}
