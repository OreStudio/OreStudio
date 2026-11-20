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
#include "ores.ore.core/domain/curve_configuration_projection.hpp"
#include "ores.ore.core/domain/todays_market_projection.hpp"
#include "ores.ore.core/xml/forwarding_curves.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string forwarding_curves_test_suite("ores::ore::core::tests");
const std::string forwarding_curves_tags("[ore][forwarding_curves]");

const std::string market_data = "# published by the party\n"
                                "2026-11-17 DISCOUNT/RATE/GBP/GBP-LIBOR-3M/2Y 0.91\n"
                                "2026-11-17 DISCOUNT/RATE/GBP/GBP-LIBOR-3M/12M 0.96\n"
                                "2026-11-17 DISCOUNT/RATE/GBP/GBP-SONIA/12M 0.95\n"
                                "2026-11-17 DISCOUNT/RATE/SEK/SEK-STIBOR-3M/12M 0.97\n"
                                "2026-11-17 DISCOUNT/RATE/GBP/GBP-SARON/12M 0.99\n"
                                "2026-11-17 DISCOUNT/RATE/GBP/12M 0.94\n"
                                "2026-11-17 FX/RATE/EUR/GBP 0.87\n";

ores::ore::xml::input_files package() {
    ores::ore::xml::input_files files;
    files["ore.xml"] = R"(<ORE>
  <Setup>
    <Parameter name="asofDate">2026-11-17</Parameter>
    <Parameter name="conventionsFile">conventions.xml</Parameter>
    <Parameter name="curveConfigFile">curveconfig.xml</Parameter>
    <Parameter name="marketConfigFile">todaysmarket.xml</Parameter>
  </Setup>
  <Analytics>
    <Analytic type="npv"><Parameter name="active">Y</Parameter></Analytic>
  </Analytics>
</ORE>)";
    files["conventions.xml"] =
        "<Conventions><Zero><Id>GBP-ZERO-TENOR-BASED</Id><TenorBased>Y</TenorBased>"
        "<DayCounter>A365</DayCounter><TenorCalendar>UK</TenorCalendar></Zero></Conventions>";
    files["curveconfig.xml"] = "<CurveConfiguration><YieldCurves/></CurveConfiguration>";
    files["todaysmarket.xml"] = R"(<TodaysMarket>
  <Configuration id="default"><IndexForwardingCurvesId>default</IndexForwardingCurvesId></Configuration>
  <IndexForwardingCurves id="default"><Index name="GBP-SONIA">Yield/GBP/GBP1D</Index></IndexForwardingCurves>
</TodaysMarket>)";
    return files;
}

std::vector<std::string> forwarded(const ores::ore::xml::input_files& files) {
    const auto market =
        ores::ore::domain::todays_market_projection::read(files.at("todaysmarket.xml"));
    std::vector<std::string> out;
    for (const auto& c : market.index_forwarding_curves)
        for (const auto& i : c.index)
            out.push_back(i.name + "=" + i.value);
    return out;
}

std::vector<std::string> curve_ids(const ores::ore::xml::input_files& files) {
    const auto curves =
        ores::ore::domain::curve_configuration_projection::read(files.at("curveconfig.xml"));
    std::vector<std::string> out;
    for (const auto& y : curves.yield_curves.yield_curve)
        out.push_back(y.curve_id);
    return out;
}

}

TEST_CASE("a_market_publishes_each_curve_its_discount_factors_name", forwarding_curves_tags) {
    auto lg(ores::logging::make_logger(forwarding_curves_test_suite));
    const auto curves = ores::ore::xml::published_curves(market_data);
    REQUIRE(curves.size() == 4);
    CHECK(curves[0].name == "GBP-LIBOR-3M");
    CHECK(curves[0].currency == "GBP");
    CHECK(curves[0].quotes == std::vector<std::string>{"DISCOUNT/RATE/GBP/GBP-LIBOR-3M/12M",
                                                       "DISCOUNT/RATE/GBP/GBP-LIBOR-3M/2Y"});
    CHECK(curves[1].name == "GBP-SARON");
    CHECK(curves[2].name == "GBP-SONIA");
    CHECK(curves[3].name == "SEK-STIBOR-3M");
}

TEST_CASE("a_published_curve_the_portfolio_names_is_forwarded_and_built", forwarding_curves_tags) {
    auto lg(ores::logging::make_logger(forwarding_curves_test_suite));
    const auto out = ores::ore::xml::name_forwarding_curves(
        package(), market_data, "<Index>GBP-LIBOR-3M</Index><Index>GBP-SONIA</Index>");

    CHECK(forwarded(out) == std::vector<std::string>{"GBP-SONIA=Yield/GBP/GBP1D",
                                                     "GBP-LIBOR-3M=Yield/GBP/GBP-LIBOR-3M"});
    CHECK(curve_ids(out) == std::vector<std::string>{"GBP-LIBOR-3M"});
    const auto& written = out.at("curveconfig.xml");
    CHECK(written.find("DISCOUNT/RATE/GBP/GBP-LIBOR-3M/2Y") != std::string::npos);
    CHECK(written.find("GBP-ZERO-TENOR-BASED") != std::string::npos);
}

TEST_CASE("a_published_curve_is_left_out_when_nothing_calls_for_it", forwarding_curves_tags) {
    auto lg(ores::logging::make_logger(forwarding_curves_test_suite));
    const auto input = package();

    SECTION("the portfolio does not name it") {
        const auto out = ores::ore::xml::name_forwarding_curves(input, market_data, "<Trade/>");
        CHECK(out == input);
    }
    SECTION("the portfolio names a longer index that starts with it") {
        const auto out = ores::ore::xml::name_forwarding_curves(
            input, market_data, "<Index>GBP-LIBOR-3MX</Index>");
        CHECK(out == input);
    }
    SECTION("the conventions hold no zero convention for its currency") {
        const auto out = ores::ore::xml::name_forwarding_curves(
            input, market_data, "<Index>SEK-STIBOR-3M</Index>");
        CHECK(out == input);
    }
}

TEST_CASE("a_term_rate_is_forwarded_on_its_overnight_index_curve", forwarding_curves_tags) {
    auto lg(ores::logging::make_logger(forwarding_curves_test_suite));
    const auto input = package();

    const auto out = ores::ore::xml::name_forwarding_curves(
        input, "", "<Index>GBP-SONIA-3M</Index><Index>GBP-SONIA-12M</Index>");
    CHECK(forwarded(out) == std::vector<std::string>{"GBP-SONIA=Yield/GBP/GBP1D",
                                                     "GBP-SONIA-12M=Yield/GBP/GBP1D",
                                                     "GBP-SONIA-3M=Yield/GBP/GBP1D"});
    CHECK(out.at("curveconfig.xml") == input.at("curveconfig.xml"));

    SECTION("a name that only starts like a term rate is not one") {
        const auto other = ores::ore::xml::name_forwarding_curves(
            input, "", "<Index>GBP-SONIA-3MX</Index><Index>GBP-SONIA-M</Index>");
        CHECK(other == input);
    }
}

TEST_CASE("a_term_rate_of_a_published_overnight_curve_brings_the_curve_in",
          forwarding_curves_tags) {
    auto lg(ores::logging::make_logger(forwarding_curves_test_suite));
    const auto out = ores::ore::xml::name_forwarding_curves(
        package(), market_data, "<Index>GBP-SARON-3M</Index>");
    CHECK(forwarded(out) == std::vector<std::string>{"GBP-SONIA=Yield/GBP/GBP1D",
                                                     "GBP-SARON=Yield/GBP/GBP-SARON",
                                                     "GBP-SARON-3M=Yield/GBP/GBP-SARON"});
    CHECK(curve_ids(out) == std::vector<std::string>{"GBP-SARON"});
}
